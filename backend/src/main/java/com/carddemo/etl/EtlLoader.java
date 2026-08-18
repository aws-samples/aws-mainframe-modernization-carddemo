package com.carddemo.etl;

import com.carddemo.etl.records.AccountRecord;
import com.carddemo.etl.records.CardRecord;
import com.carddemo.etl.records.CardXrefRecord;
import com.carddemo.etl.records.CustomerRecord;
import com.carddemo.etl.records.ReferenceRecords.DisclosureGroup;
import com.carddemo.etl.records.ReferenceRecords.SecUser;
import com.carddemo.etl.records.ReferenceRecords.TranCategory;
import com.carddemo.etl.records.ReferenceRecords.TranCategoryBalance;
import com.carddemo.etl.records.ReferenceRecords.TranType;
import com.carddemo.crypto.SsnCrypto;
import com.carddemo.etl.records.TransactionRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Date;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Function;

/**
 * One-time loader for the ASCII sample files + DUSRSECJ.jcl in-stream users
 * (plan §2.3). Load order: trantype → trancatg → discgrp → customers →
 * accounts → cards → xref FK backfill → tcatbal → daily_transactions →
 * transactions (optional flag) → users.
 *
 * Integrity rules implemented here:
 * <ul>
 *   <li>Xref 1:1 assertion (N8): an account mapping to &gt;1 distinct customer
 *       aborts the load loudly.</li>
 *   <li>Blank ACCT-GROUP-ID → NULL (B1).</li>
 *   <li>FK violations are pre-checked and reported, never silently dropped.</li>
 *   <li>Row-count + NUMERIC checksum validation report at the end.</li>
 * </ul>
 */
public class EtlLoader {

    private static final Logger log = LoggerFactory.getLogger(EtlLoader.class);

    private final JdbcTemplate jdbc;
    private final EtlProperties props;
    private final PasswordEncoder passwordEncoder;
    private final SsnCrypto ssnCrypto;

    public EtlLoader(JdbcTemplate jdbc, EtlProperties props, PasswordEncoder passwordEncoder,
                     SsnCrypto ssnCrypto) {
        this.jdbc = jdbc;
        this.props = props;
        this.passwordEncoder = passwordEncoder;
        this.ssnCrypto = ssnCrypto;
    }

    public EtlValidationReport load() {
        EtlValidationReport report = new EtlValidationReport();
        Path dataDir = Path.of(props.dataDir());

        // ---- parse everything up front -------------------------------------
        List<TranType> tranTypes = parse(dataDir.resolve("trantype.txt"), TranType::parse);
        List<TranCategory> tranCategories = parse(dataDir.resolve("trancatg.txt"), TranCategory::parse);
        List<DisclosureGroup> discGroups = parse(dataDir.resolve("discgrp.txt"), DisclosureGroup::parse);
        List<CustomerRecord> customers = parse(dataDir.resolve("custdata.txt"), CustomerRecord::parse);
        List<AccountRecord> accounts = parse(dataDir.resolve("acctdata.txt"), AccountRecord::parse);
        List<CardRecord> cards = parse(dataDir.resolve("carddata.txt"), CardRecord::parse);
        List<CardXrefRecord> xrefs = parse(dataDir.resolve("cardxref.txt"), CardXrefRecord::parse);
        List<TranCategoryBalance> tcatbals = parse(dataDir.resolve("tcatbal.txt"), TranCategoryBalance::parse);
        List<TransactionRecord> dailyTrans = parse(dataDir.resolve("dailytran.txt"), TransactionRecord::parse);
        List<SecUser> users = JclInstreamUsers.parse(Path.of(props.jclFile()));

        // ---- xref 1:1 assertion (N8) — fail loudly BEFORE writing anything --
        assertXrefOneToOne(xrefs);

        // ---- load in plan §2.3 order ----------------------------------------
        loadTranTypes(tranTypes);
        loadTranCategories(tranCategories, tranTypes, report);
        loadDisclosureGroups(discGroups);
        loadCustomers(customers);
        loadAccounts(accounts);
        Set<String> insertedCards = loadCards(cards, accounts, report);
        backfillCustomerIds(xrefs, accounts, cards, customers, report);
        loadTcatbals(tcatbals, accounts, report);
        loadDailyTransactions(dailyTrans);
        if (props.seedTransactions()) {
            loadTransactions(dailyTrans, insertedCards, report);
        } else {
            log.info("--etl.seed-transactions=false: leaving transactions empty (parity fixture mode)");
        }
        loadUsers(users);

        // ---- row-count + checksum validation --------------------------------
        report.addCount("transaction_types", tranTypes.size(), count("transaction_types"));
        report.addCount("transaction_categories", tranCategories.size(), count("transaction_categories"));
        report.addCount("disclosure_groups", discGroups.size(), count("disclosure_groups"));
        report.addCount("customers", customers.size(), count("customers"));
        report.addCount("accounts", accounts.size(), count("accounts"));
        report.addCount("cards", cards.size(), count("cards"));
        report.addCount("accounts.customer_id set",
                xrefs.stream().map(CardXrefRecord::accountId).distinct().count(),
                jdbc.queryForObject("SELECT count(*) FROM accounts WHERE customer_id IS NOT NULL", Long.class));
        report.addCount("transaction_category_balances", tcatbals.size(), count("transaction_category_balances"));
        report.addCount("daily_transactions", dailyTrans.size(), count("daily_transactions"));
        report.addCount("transactions", props.seedTransactions() ? dailyTrans.size() : 0, count("transactions"));
        report.addCount("users", users.size(), count("users"));

        report.addChecksum("accounts", "curr_bal",
                sum(accounts, AccountRecord::currBal), dbSum("accounts", "curr_bal"));
        report.addChecksum("accounts", "credit_limit",
                sum(accounts, AccountRecord::creditLimit), dbSum("accounts", "credit_limit"));
        report.addChecksum("transaction_category_balances", "balance",
                sum(tcatbals, TranCategoryBalance::balance),
                dbSum("transaction_category_balances", "balance"));
        report.addChecksum("daily_transactions", "amount",
                sum(dailyTrans, TransactionRecord::amount), dbSum("daily_transactions", "amount"));
        if (props.seedTransactions()) {
            report.addChecksum("transactions", "amount",
                    sum(dailyTrans, TransactionRecord::amount), dbSum("transactions", "amount"));
        }
        report.addChecksum("disclosure_groups", "interest_rate",
                sum(discGroups, DisclosureGroup::interestRate), dbSum("disclosure_groups", "interest_rate"));
        return report;
    }

    // ------------------------------------------------------------------ steps

    /** Plan §2.3 N8: abort loudly if any account maps to >1 distinct customer. */
    private void assertXrefOneToOne(List<CardXrefRecord> xrefs) {
        Map<Long, Set<Long>> custsByAcct = new TreeMap<>();
        for (CardXrefRecord x : xrefs) {
            custsByAcct.computeIfAbsent(x.accountId(), k -> new TreeSet<>()).add(x.customerId());
        }
        Map<Long, Set<Long>> offenders = new TreeMap<>();
        custsByAcct.forEach((acct, custs) -> {
            if (custs.size() > 1) {
                offenders.put(acct, custs);
            }
        });
        if (!offenders.isEmpty()) {
            StringBuilder sb = new StringBuilder(
                    "XREF 1:1 ASSERTION FAILED — account(s) map to more than one customer:\n");
            offenders.forEach((acct, custs) ->
                    sb.append("  account ").append(acct).append(" -> customers ").append(custs).append('\n'));
            sb.append("Aborting load (plan §2.3 N8: never pick one silently).");
            throw new EtlValidationException(sb.toString());
        }
    }

    private void loadTranTypes(List<TranType> rows) {
        for (TranType r : rows) {
            jdbc.update("INSERT INTO transaction_types (type_cd, description) VALUES (?, ?)",
                    r.typeCd(), r.description());
        }
    }

    private void loadTranCategories(List<TranCategory> rows, List<TranType> types,
                                    EtlValidationReport report) {
        Set<String> typeCds = new HashSet<>();
        types.forEach(t -> typeCds.add(t.typeCd()));
        for (TranCategory r : rows) {
            if (!typeCds.contains(r.typeCd())) {
                report.addViolation("transaction_categories(" + r.typeCd() + "," + r.categoryCd()
                        + "): unknown type_cd " + r.typeCd());
                continue;
            }
            jdbc.update("INSERT INTO transaction_categories (type_cd, category_cd, description) VALUES (?, ?, ?)",
                    r.typeCd(), r.categoryCd(), r.description());
        }
    }

    private void loadDisclosureGroups(List<DisclosureGroup> rows) {
        for (DisclosureGroup r : rows) {
            jdbc.update("INSERT INTO disclosure_groups (group_id, type_cd, category_cd, interest_rate) "
                            + "VALUES (?, ?, ?, ?)",
                    r.groupId(), r.typeCd(), r.categoryCd(), r.interestRate());
        }
    }

    private void loadCustomers(List<CustomerRecord> rows) {
        for (CustomerRecord r : rows) {
            jdbc.update("""
                            INSERT INTO customers (customer_id, first_name, middle_name, last_name,
                              addr_line_1, addr_line_2, addr_line_3, addr_state_cd, addr_country_cd,
                              addr_zip, phone_num_1, phone_num_2, ssn, govt_issued_id, dob,
                              eft_account_id, pri_card_holder, fico_score)
                            VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
                            """,
                    r.customerId(), r.firstName(), r.middleName(), r.lastName(),
                    r.addrLine1(), r.addrLine2(), r.addrLine3(), r.stateCd(), r.countryCd(),
                    r.zip(), r.phone1(), r.phone2(),
                    // encrypted at rest (plan R5) — same converter key as the API
                    ssnCrypto.encrypt(r.ssn()), r.govtIssuedId(), toDate(r.dob()),
                    r.eftAccountId(), r.priCardHolder(), r.ficoScore());
        }
    }

    private void loadAccounts(List<AccountRecord> rows) {
        for (AccountRecord r : rows) {
            // B1: blank ACCT-GROUP-ID already mapped to NULL by the parser (trim → null).
            jdbc.update("""
                            INSERT INTO accounts (account_id, active_status, curr_bal, credit_limit,
                              cash_credit_limit, open_date, expiration_date, reissue_date,
                              curr_cyc_credit, curr_cyc_debit, zip, group_id)
                            VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
                            """,
                    r.accountId(), r.activeStatus(), r.currBal(), r.creditLimit(),
                    r.cashCreditLimit(), toDate(r.openDate()), toDate(r.expirationDate()),
                    toDate(r.reissueDate()), r.currCycCredit(), r.currCycDebit(),
                    r.zip(), r.groupId());
        }
    }

    /** @return card numbers actually inserted (FK-violating rows are skipped + reported). */
    private Set<String> loadCards(List<CardRecord> rows, List<AccountRecord> accounts,
                                  EtlValidationReport report) {
        Set<Long> acctIds = new HashSet<>();
        accounts.forEach(a -> acctIds.add(a.accountId()));
        Set<String> inserted = new HashSet<>();
        for (CardRecord r : rows) {
            if (!acctIds.contains(r.accountId())) {
                report.addViolation("cards(" + r.cardNumber() + "): FK violation — account "
                        + r.accountId() + " not in acctdata");
                continue;
            }
            jdbc.update("""
                            INSERT INTO cards (card_number, account_id, embossed_name,
                              expiration_date, active_status)
                            VALUES (?,?,?,?,?)
                            """,
                    r.cardNumber(), r.accountId(), r.embossedName(),
                    toDate(r.expirationDate()), r.activeStatus());
            inserted.add(r.cardNumber());
        }
        return inserted;
    }

    /**
     * Dissolves CXACAIX: sets accounts.customer_id from the xref, and
     * cross-checks the xref's card→account edge against carddata.
     */
    private void backfillCustomerIds(List<CardXrefRecord> xrefs, List<AccountRecord> accounts,
                                     List<CardRecord> cards, List<CustomerRecord> customers,
                                     EtlValidationReport report) {
        Set<Long> acctIds = new HashSet<>();
        accounts.forEach(a -> acctIds.add(a.accountId()));
        Set<Long> custIds = new HashSet<>();
        customers.forEach(c -> custIds.add(c.customerId()));
        Map<String, Long> cardToAcct = new HashMap<>();
        cards.forEach(c -> cardToAcct.put(c.cardNumber(), c.accountId()));

        Map<Long, Long> custByAcct = new TreeMap<>();
        for (CardXrefRecord x : xrefs) {
            if (!acctIds.contains(x.accountId())) {
                report.addViolation("cardxref(" + x.cardNumber() + "): FK violation — account "
                        + x.accountId() + " not in acctdata");
                continue;
            }
            if (!custIds.contains(x.customerId())) {
                report.addViolation("cardxref(" + x.cardNumber() + "): FK violation — customer "
                        + x.customerId() + " not in custdata");
                continue;
            }
            Long cardAcct = cardToAcct.get(x.cardNumber());
            if (cardAcct == null) {
                report.addViolation("cardxref(" + x.cardNumber() + "): card not in carddata");
            } else if (cardAcct != x.accountId()) {
                report.addViolation("cardxref(" + x.cardNumber() + "): account mismatch — xref says "
                        + x.accountId() + ", carddata says " + cardAcct);
            }
            custByAcct.put(x.accountId(), x.customerId());
        }
        custByAcct.forEach((acct, cust) ->
                jdbc.update("UPDATE accounts SET customer_id = ? WHERE account_id = ?", cust, acct));
    }

    private void loadTcatbals(List<TranCategoryBalance> rows, List<AccountRecord> accounts,
                              EtlValidationReport report) {
        Set<Long> acctIds = new HashSet<>();
        accounts.forEach(a -> acctIds.add(a.accountId()));
        for (TranCategoryBalance r : rows) {
            if (!acctIds.contains(r.accountId())) {
                report.addViolation("transaction_category_balances(" + r.accountId() + "," + r.typeCd()
                        + "," + r.categoryCd() + "): FK violation — account not in acctdata");
                continue;
            }
            jdbc.update("""
                            INSERT INTO transaction_category_balances (account_id, type_cd, category_cd, balance)
                            VALUES (?,?,?,?)
                            """,
                    r.accountId(), r.typeCd(), r.categoryCd(), r.balance());
        }
    }

    private void loadDailyTransactions(List<TransactionRecord> rows) {
        for (TransactionRecord r : rows) {
            jdbc.update("""
                            INSERT INTO daily_transactions (daily_tran_id, type_cd, category_cd, source,
                              description, amount, merchant_id, merchant_name, merchant_city,
                              merchant_zip, card_number, orig_ts, proc_ts)
                            VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
                            """,
                    r.tranId(), r.typeCd(), r.categoryCd(), r.source(), r.description(),
                    r.amount(), r.merchantId(), r.merchantName(), r.merchantCity(),
                    r.merchantZip(), r.cardNumber(), toTs(r.origTs()), toTs(r.procTs()));
        }
    }

    /**
     * Plan §2.3 deliberate divergence: seeds transactions from dailytran.txt so
     * Phase 1 browsing has data (legacy TRANSACT starts near-empty). Guarded by
     * the {@code --etl.seed-transactions} flag; OFF for POSTTRAN parity
     * fixtures (Phase 4 seeding caveat).
     */
    private void loadTransactions(List<TransactionRecord> rows, Set<String> insertedCards,
                                  EtlValidationReport report) {
        for (TransactionRecord r : rows) {
            if (!insertedCards.contains(r.cardNumber())) {
                report.addViolation("transactions(" + r.tranId() + "): FK violation — card "
                        + r.cardNumber() + " not loaded into cards");
                continue;
            }
            jdbc.update("""
                            INSERT INTO transactions (transaction_id, type_cd, category_cd, source,
                              description, amount, merchant_id, merchant_name, merchant_city,
                              merchant_zip, card_number, orig_ts, proc_ts)
                            VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
                            """,
                    r.tranId(), r.typeCd(), r.categoryCd(), r.source(), r.description(),
                    r.amount(), r.merchantId(), r.merchantName(), r.merchantCity(),
                    r.merchantZip(), r.cardNumber(), toTs(r.origTs()), toTs(r.procTs()));
        }
    }

    /** Bcrypt-hashes the 8-char DUSRSECJ passwords (plan §2.4 / R6). */
    private void loadUsers(List<SecUser> rows) {
        for (SecUser r : rows) {
            jdbc.update("""
                            INSERT INTO users (user_id, first_name, last_name, password_hash, user_type)
                            VALUES (?,?,?,?,?)
                            """,
                    r.userId(), r.firstName(), r.lastName(),
                    passwordEncoder.encode(r.password()), r.userType());
        }
    }

    // ---------------------------------------------------------------- helpers

    private <T> List<T> parse(Path file, Function<String, T> parser) {
        try {
            List<String> lines = Files.readAllLines(file);
            return lines.stream()
                    .filter(l -> !l.isBlank())
                    .map(l -> {
                        try {
                            return parser.apply(l);
                        } catch (RuntimeException e) {
                            throw new EtlValidationException(
                                    "parse failure in " + file.getFileName() + ": " + e.getMessage()
                                            + "\n  line: " + l);
                        }
                    })
                    .toList();
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + file, e);
        }
    }

    private long count(String table) {
        Long n = jdbc.queryForObject("SELECT count(*) FROM " + table, Long.class);
        return n == null ? 0 : n;
    }

    private BigDecimal dbSum(String table, String column) {
        BigDecimal v = jdbc.queryForObject(
                "SELECT COALESCE(SUM(" + column + "), 0) FROM " + table, BigDecimal.class);
        return v == null ? BigDecimal.ZERO : v;
    }

    private static <T> BigDecimal sum(List<T> rows, Function<T, BigDecimal> getter) {
        return rows.stream().map(getter).reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    private static Date toDate(LocalDate d) {
        return d == null ? null : Date.valueOf(d);
    }

    private static Timestamp toTs(LocalDateTime t) {
        return t == null ? null : Timestamp.valueOf(t);
    }
}
