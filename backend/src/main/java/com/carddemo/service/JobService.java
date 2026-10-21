package com.carddemo.service;

import com.carddemo.model.*;
import com.carddemo.repo.*;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Batch jobs ported from the COBOL batch programs:
 *  - posting    → CBTRN01C/CBTRN02C/CBTRN03C
 *  - interest   → CBACT04C
 *  - statements → CBSTM03A/CBSTM03B (COSTM01.CPY record layout)
 */
@Service
public class JobService {

    private static final int REJECT_INVALID_CARD = 100;
    private static final int REJECT_ACCT_NOT_FOUND = 101;
    private static final int REJECT_OVERLIMIT = 102;
    private static final int REJECT_AFTER_EXPIRY = 103;

    private final DailyTransactionRepository dailyTrans;
    private final TransactionRepository transactions;
    private final TranCatBalRepository tcatBals;
    private final DisclosureGroupRepository disclGroups;
    private final CardXrefRepository xrefs;
    private final AccountRepository accounts;
    private final CustomerRepository customers;
    private final CardRepository cards;
    private final TransactionService transactionService;

    public JobService(DailyTransactionRepository dailyTrans,
                      TransactionRepository transactions,
                      TranCatBalRepository tcatBals,
                      DisclosureGroupRepository disclGroups,
                      CardXrefRepository xrefs,
                      AccountRepository accounts,
                      CustomerRepository customers,
                      CardRepository cards,
                      TransactionService transactionService) {
        this.dailyTrans = dailyTrans;
        this.transactions = transactions;
        this.tcatBals = tcatBals;
        this.disclGroups = disclGroups;
        this.xrefs = xrefs;
        this.accounts = accounts;
        this.customers = customers;
        this.cards = cards;
        this.transactionService = transactionService;
    }

    // ------------------------------------------------------------------
    // CBTRN02C — transaction posting
    // ------------------------------------------------------------------

    @Transactional
    public Dto.PostingResult runPostingJob() {
        List<DailyTransaction> daily = dailyTrans.findAll().stream()
                .sorted(Comparator.comparing(DailyTransaction::getTranId))
                .toList();
        List<Dto.RejectedTran> rejects = new ArrayList<>();
        int posted = 0;

        for (DailyTransaction d : daily) {
            String reason = validateDaily(d);
            if (reason == null) {
                post(d);
                posted++;
            } else {
                int code = switch (reason) {
                    case "INVALID CARD NUMBER FOUND" -> REJECT_INVALID_CARD;
                    case "ACCOUNT RECORD NOT FOUND" -> REJECT_ACCT_NOT_FOUND;
                    case "OVERLIMIT TRANSACTION" -> REJECT_OVERLIMIT;
                    default -> REJECT_AFTER_EXPIRY;
                };
                rejects.add(new Dto.RejectedTran(d.getTranId(), code, reason));
            }
            // CBTRN03C removes processed daily records either way.
            dailyTrans.delete(d);
        }
        return new Dto.PostingResult(daily.size(), posted, rejects.size(), rejects);
    }

    /** 1500-VALIDATE-TRAN: xref lookup → account lookup → overlimit → expiry. */
    private String validateDaily(DailyTransaction d) {
        var xref = xrefs.findById(d.getCardNum());
        if (xref.isEmpty()) {
            return "INVALID CARD NUMBER FOUND";
        }
        var acct = accounts.findById(xref.get().getAcctId());
        if (acct.isEmpty()) {
            return "ACCOUNT RECORD NOT FOUND";
        }
        Account a = acct.get();
        BigDecimal tempBal = a.getCurrCycCredit()
                .subtract(a.getCurrCycDebit())
                .add(d.getAmt());
        if (a.getCreditLimit().compareTo(tempBal) < 0) {
            return "OVERLIMIT TRANSACTION";
        }
        String origDate = d.getOrigTs() == null ? "" : d.getOrigTs().substring(0, Math.min(10, d.getOrigTs().length()));
        if (a.getExpirationDate() != null && !a.getExpirationDate().isBlank()
                && a.getExpirationDate().compareTo(origDate) < 0) {
            return "TRANSACTION RECEIVED AFTER ACCT EXPIRATION";
        }
        return null;
    }

    /** 2000-POST-TRANSACTION. */
    private void post(DailyTransaction d) {
        CardXref xref = xrefs.findById(d.getCardNum()).orElseThrow();
        Account acct = accounts.findById(xref.getAcctId()).orElseThrow();

        // 2700-UPDATE-TCATBAL
        TranCatBal.Key key = new TranCatBal.Key(xref.getAcctId(), d.getTypeCd(), d.getCatCd());
        TranCatBal tcb = tcatBals.findById(key).orElseGet(() -> {
            TranCatBal n = new TranCatBal();
            n.setAcctId(xref.getAcctId());
            n.setTypeCd(d.getTypeCd());
            n.setCatCd(d.getCatCd());
            n.setBal(BigDecimal.ZERO.setScale(2));
            return n;
        });
        tcb.setBal(tcb.getBal().add(d.getAmt()));
        tcatBals.save(tcb);

        // 2800-UPDATE-ACCOUNT-REC
        acct.setCurrBal(acct.getCurrBal().add(d.getAmt()));
        if (d.getAmt().compareTo(BigDecimal.ZERO) >= 0) {
            acct.setCurrCycCredit(acct.getCurrCycCredit().add(d.getAmt()));
        } else {
            acct.setCurrCycDebit(acct.getCurrCycDebit().add(d.getAmt()));
        }
        accounts.save(acct);

        // 2900-WRITE-TRANSACTION-FILE
        TransactionRecord t = new TransactionRecord();
        t.setTranId(d.getTranId());
        t.copyFieldsFrom(d);
        t.setProcTs(TransactionService.now());
        transactions.save(t);
    }

    // ------------------------------------------------------------------
    // CBACT04C — interest calculation
    // ------------------------------------------------------------------

    @Transactional
    public Dto.InterestResult runInterestJob() {
        String parmDate = LocalDate.now().format(DateTimeFormatter.ISO_LOCAL_DATE);
        List<TranCatBal> rows = tcatBals.findAllByOrderByAcctIdAscTypeCdAscCatCdAsc();

        List<Dto.AcctInterest> perAccount = new ArrayList<>();
        int txCreated = 0;
        int suffix = 0;
        Long lastAcct = null;
        BigDecimal totalInt = BigDecimal.ZERO.setScale(2);
        Account current = null;
        String cardNum = "";
        BigDecimal grandTotal = BigDecimal.ZERO.setScale(2);

        for (TranCatBal row : rows) {
            if (!row.getAcctId().equals(lastAcct)) {
                if (current != null) {
                    applyInterestToAccount(current, totalInt);
                    perAccount.add(new Dto.AcctInterest(current.getId(), totalInt));
                }
                totalInt = BigDecimal.ZERO.setScale(2);
                lastAcct = row.getAcctId();
                current = accounts.findById(row.getAcctId())
                        .orElseThrow(() -> ApiException.badRequest(
                                "ACCOUNT NOT FOUND: " + row.getAcctId()));
                cardNum = xrefs.findFirstByAcctId(row.getAcctId())
                        .map(CardXref::getCardNum).orElse("");
            }

            DisclosureGroup.Key dkey = new DisclosureGroup.Key(
                    current.getGroupId(), row.getTypeCd(), row.getCatCd());
            var dg = disclGroups.findById(dkey)
                    .or(() -> disclGroups.findById(new DisclosureGroup.Key(
                            "DEFAULT", row.getTypeCd(), row.getCatCd())));
            BigDecimal rate = dg.map(DisclosureGroup::getIntRate).orElse(BigDecimal.ZERO);
            if (rate.compareTo(BigDecimal.ZERO) != 0) {
                // 1300-COMPUTE-INTEREST: (bal * rate) / 1200
                BigDecimal monthlyInt = row.getBal().multiply(rate)
                        .divide(new BigDecimal("1200"), 2, RoundingMode.HALF_UP);
                totalInt = totalInt.add(monthlyInt);
                grandTotal = grandTotal.add(monthlyInt);
                suffix++;
                writeInterestTran(parmDate, suffix, current, cardNum, monthlyInt);
                txCreated++;
            }
        }
        if (current != null) {
            applyInterestToAccount(current, totalInt);
            perAccount.add(new Dto.AcctInterest(current.getId(), totalInt));
        }
        return new Dto.InterestResult(perAccount.size(), txCreated, grandTotal, perAccount);
    }

    /** 1050-UPDATE-ACCOUNT. */
    private void applyInterestToAccount(Account acct, BigDecimal totalInt) {
        acct.setCurrBal(acct.getCurrBal().add(totalInt));
        acct.setCurrCycCredit(BigDecimal.ZERO.setScale(2));
        acct.setCurrCycDebit(BigDecimal.ZERO.setScale(2));
        accounts.save(acct);
    }

    /** 1300-B-WRITE-TX. */
    private void writeInterestTran(String parmDate, int suffix, Account acct,
                                   String cardNum, BigDecimal amount) {
        TransactionRecord t = new TransactionRecord();
        t.setTranId(parmDate + String.format("%06d", suffix));
        t.setTypeCd("01");
        t.setCatCd(5);
        t.setSource("System");
        t.setDescription("Int. for a/c " + acct.getId());
        t.setAmt(amount);
        t.setMerchantId(0L);
        t.setMerchantName("");
        t.setMerchantCity("");
        t.setMerchantZip("");
        t.setCardNum(cardNum);
        String ts = TransactionService.now();
        t.setOrigTs(ts);
        t.setProcTs(ts);
        transactions.save(t);
    }

    // ------------------------------------------------------------------
    // CBSTM03A — statement generation
    // ------------------------------------------------------------------

    @Transactional(readOnly = true)
    public List<Dto.StatementResult> runStatementJob() {
        List<Dto.StatementResult> out = new ArrayList<>();
        for (Account acct : accounts.findAll().stream()
                .sorted(Comparator.comparing(Account::getId)).toList()) {
            out.add(statementFor(acct.getId()));
        }
        return out;
    }

    @Transactional(readOnly = true)
    public Dto.StatementResult statementFor(long acctId) {
        Account acct = accounts.findById(acctId)
                .orElseThrow(() -> ApiException.notFound("Account ID NOT found..."));

        Customer cust = xrefs.findFirstByAcctId(acctId)
                .flatMap(x -> customers.findById(x.getCustId())).orElse(null);

        List<TransactionRecord> txns = xrefs.findAll().stream()
                .filter(x -> x.getAcctId().equals(acctId))
                .flatMap(x -> transactions.findByCardNumOrderByOrigTsAsc(x.getCardNum()).stream())
                .sorted(Comparator.comparing(TransactionRecord::getOrigTs))
                .toList();

        String name = cust == null ? ""
                : (cust.getFirstName() + " " + cust.getLastName()).trim();
        List<Dto.StatementLine> lines = txns.stream()
                .map(t -> new Dto.StatementLine(t.getTranId(),
                        t.getDescription() == null ? "" : t.getDescription().trim(),
                        t.getAmt()))
                .toList();
        BigDecimal total = txns.stream().map(TransactionRecord::getAmt)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        return new Dto.StatementResult(acctId, name, acct.getCurrBal(),
                cust == null ? "" : String.valueOf(cust.getFicoCreditScore()),
                lines, total,
                renderStatementText(acct, cust, txns, total));
    }

    /** COSTM01.CPY ST-LINE0..15 text layout. */
    private String renderStatementText(Account acct, Customer cust,
                                     List<TransactionRecord> txns, BigDecimal total) {
        StringBuilder sb = new StringBuilder();
        sb.append("*".repeat(31)).append("START OF STATEMENT").append("*".repeat(31)).append('\n');
        if (cust != null) {
            sb.append(pad(cust.getFirstName() + " " + cust.getLastName(), 75)).append('\n');
            sb.append(pad(cust.getAddrLine1(), 80)).append('\n');
            sb.append(pad(cust.getAddrLine2(), 80)).append('\n');
            sb.append(pad((cust.getAddrLine3() == null ? "" : cust.getAddrLine3())
                    + " " + nullToEmpty(cust.getAddrStateCd())
                    + " " + nullToEmpty(cust.getAddrCountryCd())
                    + " " + nullToEmpty(cust.getAddrZip()), 80)).append('\n');
        }
        sb.append("-".repeat(80)).append('\n');
        sb.append(" ".repeat(33)).append("Basic Details").append('\n');
        sb.append(pad("Account ID         :" + acct.getId(), 80)).append('\n');
        sb.append(pad("Current Balance    :" + money(acct.getCurrBal()), 80)).append('\n');
        if (cust != null) {
            sb.append(pad("FICO Score         :" + cust.getFicoCreditScore(), 80)).append('\n');
        }
        sb.append("-".repeat(80)).append('\n');
        sb.append(" ".repeat(30)).append("TRANSACTION SUMMARY ").append('\n');
        sb.append("-".repeat(80)).append('\n');
        sb.append("Tran ID         ").append("Tran Details    ")
                .append(" ".repeat(35)).append("  Tran Amount").append('\n');
        for (TransactionRecord t : txns) {
            sb.append(pad(t.getTranId(), 16)).append(' ')
                    .append(pad(t.getDescription() == null ? "" : t.getDescription().trim(), 49))
                    .append('$').append(money(t.getAmt())).append('\n');
        }
        sb.append("Total EXP:").append(" ".repeat(56)).append('$')
                .append(money(total)).append('\n');
        sb.append("*".repeat(32)).append("END OF STATEMENT").append("*".repeat(32)).append('\n');
        return sb.toString();
    }

    private static String pad(String s, int len) {
        if (s == null) s = "";
        return s.length() >= len ? s.substring(0, len) : s + " ".repeat(len - s.length());
    }

    private static String money(BigDecimal v) {
        return String.format("%12s", v.setScale(2).toPlainString());
    }

    private static String nullToEmpty(String s) {
        return s == null ? "" : s;
    }
}
