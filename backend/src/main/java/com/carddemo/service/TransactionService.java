package com.carddemo.service;

import com.carddemo.model.*;
import com.carddemo.repo.*;
import com.carddemo.util.DateValidator;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/** COTRN00C (list, 10/screen), COTRN01C (detail), COTRN02C (add). */
@Service
public class TransactionService {

    public static final int PAGE_SIZE = 10;
    public static final DateTimeFormatter TS_FMT =
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private final TransactionRepository transactions;
    private final CardXrefRepository xrefs;
    private final AccountRepository accounts;
    private final TransactionTypeRepository types;
    private final TransactionCategoryRepository cats;

    public TransactionService(TransactionRepository transactions, CardXrefRepository xrefs,
                              AccountRepository accounts, TransactionTypeRepository types,
                              TransactionCategoryRepository cats) {
        this.transactions = transactions;
        this.xrefs = xrefs;
        this.accounts = accounts;
        this.types = types;
        this.cats = cats;
    }

    public Dto.PageResult<Dto.TranView> list(int page) {
        var p = transactions.findAllByOrderByTranIdAsc(PageRequest.of(page, PAGE_SIZE));
        return Dto.PageResult.of(p.map(Dto.TranView::of));
    }

    public Dto.TranView view(String id) {
        if (id == null || id.isBlank()) {
            throw ApiException.badRequest("Tran ID can NOT be empty...");
        }
        TransactionRecord t = transactions.findById(id)
                .orElseThrow(() -> ApiException.notFound("Transaction ID NOT found..."));
        return Dto.TranView.of(t);
    }

    /** COTRN02C PROCESS-ENTER-KEY: validate inputs, then add on confirm='Y'. */
    @Transactional
    public Dto.TranView add(Dto.TranAddRequest req) {
        boolean hasAcct = req.accountId() != null;
        boolean hasCard = req.cardNum() != null && !req.cardNum().isBlank();
        if (hasAcct && req.accountId() <= 0) {
            throw ApiException.badRequest("Account ID must be Numeric...");
        }
        if (hasCard && !req.cardNum().matches("\\d+")) {
            throw ApiException.badRequest("Card Number must be Numeric...");
        }
        if (!hasAcct && !hasCard) {
            throw ApiException.badRequest("Account or Card Number must be entered...");
        }

        required(req.typeCd(), "Type CD can NOT be empty...");
        if (req.catCd() == null) {
            throw ApiException.badRequest("Category CD can NOT be empty...");
        }
        required(req.source(), "Source can NOT be empty...");
        required(req.description(), "Description can NOT be empty...");
        required(req.amt(), "Amount can NOT be empty...");
        required(req.origDate(), "Orig Date can NOT be empty...");
        required(req.procDate(), "Proc Date can NOT be empty...");
        required(req.merchantId(), "Merchant ID can NOT be empty...");
        required(req.merchantName(), "Merchant Name can NOT be empty...");
        required(req.merchantCity(), "Merchant City can NOT be empty...");
        required(req.merchantZip(), "Merchant Zip can NOT be empty...");

        if (!req.typeCd().matches("\\d+")) {
            throw ApiException.badRequest("Type CD must be Numeric...");
        }
        BigDecimal amt = parseAmount(req.amt());
        if (!req.origDate().matches("\\d{4}-\\d{2}-\\d{2}")) {
            throw ApiException.badRequest("Orig Date should be in format YYYY-MM-DD");
        }
        if (!req.procDate().matches("\\d{4}-\\d{2}-\\d{2}")) {
            throw ApiException.badRequest("Proc Date should be in format YYYY-MM-DD");
        }
        if (!DateValidator.isValid(req.origDate())) {
            throw ApiException.badRequest("Orig Date - Not a valid date...");
        }
        if (!DateValidator.isValid(req.procDate())) {
            throw ApiException.badRequest("Proc Date - Not a valid date...");
        }
        if (!req.merchantId().matches("\\d+")) {
            throw ApiException.badRequest("Merchant ID must be Numeric...");
        }
        if (!types.existsById(req.typeCd())) {
            throw ApiException.badRequest("Type CD NOT found...");
        }
        if (!cats.existsById(new TransactionCategory.Key(req.typeCd(), req.catCd()))) {
            throw ApiException.badRequest("Category CD NOT found...");
        }

        // Resolve the card through the XREF, like READ-CCXRAIX-FILE.
        String cardNum = hasCard ? req.cardNum().trim() : null;
        if (cardNum == null) {
            cardNum = xrefs.findAll().stream()
                    .filter(x -> x.getAcctId().equals(req.accountId()))
                    .map(CardXref::getCardNum).findFirst()
                    .orElseThrow(() -> ApiException.notFound("Card Number NOT found..."));
        }
        CardXref xref = xrefs.findById(cardNum)
                .orElseThrow(() -> ApiException.notFound("Card Number NOT found..."));
        if (hasAcct && !xref.getAcctId().equals(req.accountId())) {
            throw ApiException.badRequest("Card Number NOT found...");
        }
        accounts.findById(xref.getAcctId())
                .orElseThrow(() -> ApiException.notFound("Account ID NOT found..."));

        String confirm = req.confirm() == null ? "" : req.confirm().trim();
        if (confirm.equalsIgnoreCase("N") || confirm.isEmpty()) {
            throw ApiException.badRequest("Confirm to add this transaction...");
        }
        if (!confirm.equalsIgnoreCase("Y")) {
            throw ApiException.badRequest("Invalid value. Valid values are (Y/N)...");
        }

        TransactionRecord t = new TransactionRecord();
        t.setTranId(nextTranId());
        t.setTypeCd(req.typeCd());
        t.setCatCd(req.catCd());
        t.setSource(req.source());
        t.setDescription(req.description());
        t.setAmt(amt);
        t.setMerchantId(Long.parseLong(req.merchantId().trim()));
        t.setMerchantName(req.merchantName());
        t.setMerchantCity(req.merchantCity());
        t.setMerchantZip(req.merchantZip());
        t.setCardNum(cardNum);
        t.setOrigTs(req.origDate() + " 00:00:00.000000");
        t.setProcTs(req.procDate() + " 00:00:00.000000");
        transactions.save(t);
        return Dto.TranView.of(t);
    }

    /** MOVE HIGH-VALUES / READPREV equivalent: max numeric TRAN-ID + 1. */
    public String nextTranId() {
        long max = transactions.findAll().stream()
                .map(TransactionRecord::getTranId)
                .filter(s -> s.matches("\\d+"))
                .mapToLong(Long::parseLong)
                .max()
                .orElse(0L);
        return String.format("%016d", max + 1);
    }

    static BigDecimal parseAmount(String raw) {
        String s = raw.trim();
        if (!s.matches("-?\\d{1,9}(\\.\\d{1,2})?")) {
            throw ApiException.badRequest("Amount should be in format -99999999.99");
        }
        return new BigDecimal(s).setScale(2);
    }

    static String now() {
        return LocalDateTime.now().format(TS_FMT);
    }

    private static void required(String v, String message) {
        if (v == null || v.isBlank()) {
            throw ApiException.badRequest(message);
        }
    }
}
