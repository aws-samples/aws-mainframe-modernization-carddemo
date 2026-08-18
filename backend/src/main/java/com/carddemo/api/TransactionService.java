package com.carddemo.api;

import com.carddemo.api.dto.AddTransactionRequest;
import com.carddemo.api.dto.ApiDtos.Page;
import com.carddemo.api.dto.ApiDtos.TransactionView;
import com.carddemo.api.ValidationException.FieldError;
import com.carddemo.domain.Card;
import com.carddemo.domain.Transaction;
import com.carddemo.repo.CardRepository;
import com.carddemo.repo.TransactionRepository;
import com.carddemo.validation.DateValidator;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.regex.Pattern;

/**
 * COTRN00C/COTRN01C port (plan §4): keyset paging by transaction_id with the
 * legacy "tran-id seek" ({@code fromId}), plus detail lookup. Card numbers
 * masked in every transaction response (plan §5).
 *
 * <p>Phase 3 adds the COTRN02C port ({@link #addTransaction}): the full
 * VALIDATE-INPUT-KEY-FIELDS / VALIDATE-INPUT-DATA-FIELDS edit suite with the
 * legacy fail-fast semantics (each failed edit PERFORMs SEND-TRNADD-SCREEN,
 * which issues EXEC CICS RETURN — so the FIRST failed edit stops the
 * transaction), then ADD-TRANSACTION with the id drawn from a DB sequence
 * instead of READPREV max+1 (plan R2).</p>
 */
@Service
public class TransactionService {

    /** COTRN02C amount edit (339-351): sign, 8 digits, '.', 2 digits. */
    private static final Pattern LEGACY_AMOUNT = Pattern.compile("[+-]\\d{8}\\.\\d{2}");
    /** COTRN02C date edit (353-381): positional YYYY-MM-DD check. */
    private static final Pattern LEGACY_DATE = Pattern.compile("\\d{4}-\\d{2}-\\d{2}");

    private final TransactionRepository transactions;
    private final CardRepository cards;

    public TransactionService(TransactionRepository transactions, CardRepository cards) {
        this.transactions = transactions;
        this.cards = cards;
    }

    @Transactional(readOnly = true)
    public Page<TransactionView> list(String cursor, Integer limit, String fromId, String cardNumber) {
        int pageSize = Paging.clamp(limit);
        List<Transaction> page = transactions.pageForward(
                Paging.blankToNull(cursor),
                normalizeId(fromId),
                Paging.blankToNull(cardNumber),
                PageRequest.of(0, pageSize + 1));
        return Paging.page(page, pageSize, t -> t.getTransactionId().trim(), this::toView);
    }

    /**
     * COTRN02C port — add a transaction. Validation reproduces the legacy
     * paragraphs with their exact messages and fail-fast order (each failed
     * edit PERFORMs SEND-TRNADD-SCREEN → EXEC CICS RETURN, so only the first
     * failure is reported):
     * <ol>
     *   <li>VALIDATE-INPUT-KEY-FIELDS (195-230): account id OR card number
     *       must be entered; numeric edits; xref resolution — account →
     *       card via CXACAIX (READ-CXACAIX-FILE, 576-604) or card → account
     *       via CCXREF (READ-CCXREF-FILE, 609-637), now cards.account_id.</li>
     *   <li>VALIDATE-INPUT-DATA-FIELDS (235-437): per-field non-empty edits
     *       (252-320), Type/Category CD numeric (322-337), amount format
     *       {@code -99999999.99} (339-351), positional YYYY-MM-DD edits on
     *       orig/proc dates (353-381), CSUTLDTC date validation (389-427),
     *       merchant id numeric (430-436).</li>
     *   <li>ADD-TRANSACTION (442-466): id from a DB sequence rendered as a
     *       16-digit zero-padded string (plan R2 — replaces the race-prone
     *       READPREV max+1); field copy including TRAN-ORIG-TS and
     *       TRAN-PROC-TS both taken from the USER-ENTERED dates (COTRN02C
     *       464-465 moves TORIGDTI/TPROCDTI — it does NOT stamp now()).</li>
     * </ol>
     * The legacy Y/N confirm loop (PROCESS-ENTER-KEY 169-188) is a UI
     * concern: this POST is issued once, after the user confirms.
     */
    @Transactional
    public TransactionView addTransaction(AddTransactionRequest req) {
        // ---- VALIDATE-INPUT-KEY-FIELDS (COTRN02C:195-230) -------------------
        String acctIn = Paging.blankToNull(req.accountId());
        String cardIn = Paging.blankToNull(req.cardNumber());
        Card card;
        if (acctIn != null) {
            if (!isDigits(acctIn)) {
                throw fail("accountId", "Account ID must be Numeric...");
            }
            long acctId = Long.parseLong(acctIn);
            // READ-CXACAIX-FILE: xref by account id → card (first by key order)
            card = cards.findByAccountIdOrderByCardNumberAsc(acctId).stream()
                    .findFirst()
                    .orElseThrow(() -> fail("accountId", "Account ID NOT found..."));
        } else if (cardIn != null) {
            if (!isDigits(cardIn)) {
                throw fail("cardNumber", "Card Number must be Numeric...");
            }
            // READ-CCXREF-FILE: xref by card number → account
            card = cards.findById(pad(cardIn, 16))
                    .orElseThrow(() -> fail("cardNumber", "Card Number NOT found..."));
        } else {
            throw fail("accountId", "Account or Card Number must be entered...");
        }

        // ---- VALIDATE-INPUT-DATA-FIELDS non-empty edits (252-320) -----------
        String typeCd = require(req.typeCd(), "typeCd", "Type CD can NOT be empty...");
        String categoryCd = require(req.categoryCd(), "categoryCd", "Category CD can NOT be empty...");
        String source = require(req.source(), "source", "Source can NOT be empty...");
        String description = require(req.description(), "description", "Description can NOT be empty...");
        String amount = require(req.amount(), "amount", "Amount can NOT be empty...");
        String origDate = require(req.origDate(), "origDate", "Orig Date can NOT be empty...");
        String procDate = require(req.procDate(), "procDate", "Proc Date can NOT be empty...");
        String merchantId = require(req.merchantId(), "merchantId", "Merchant ID can NOT be empty...");
        String merchantName = require(req.merchantName(), "merchantName", "Merchant Name can NOT be empty...");
        String merchantCity = require(req.merchantCity(), "merchantCity", "Merchant City can NOT be empty...");
        String merchantZip = require(req.merchantZip(), "merchantZip", "Merchant Zip can NOT be empty...");

        // ---- numeric edits on Type/Category CD (322-337) ---------------------
        if (!isDigits(typeCd) || typeCd.length() > 2) {
            throw fail("typeCd", "Type CD must be Numeric...");
        }
        if (!isDigits(categoryCd) || categoryCd.length() > 4) {
            throw fail("categoryCd", "Category CD must be Numeric...");
        }

        // ---- amount format edit (339-351): sign + 8 digits + '.' + 2 digits --
        if (!LEGACY_AMOUNT.matcher(amount).matches()) {
            throw fail("amount", "Amount should be in format -99999999.99");
        }
        BigDecimal amountValue = new BigDecimal(amount);

        // ---- positional date-format edits (353-381) --------------------------
        if (!LEGACY_DATE.matcher(origDate).matches()) {
            throw fail("origDate", "Orig Date should be in format YYYY-MM-DD");
        }
        if (!LEGACY_DATE.matcher(procDate).matches()) {
            throw fail("procDate", "Proc Date should be in format YYYY-MM-DD");
        }

        // ---- CSUTLDTC date validation (389-427) -------------------------------
        DateValidator.Result orig = DateValidator.validate("Orig Date", origDate);
        if (orig.error() != null) {
            throw fail("origDate", "Orig Date - Not a valid date...");
        }
        DateValidator.Result proc = DateValidator.validate("Proc Date", procDate);
        if (proc.error() != null) {
            throw fail("procDate", "Proc Date - Not a valid date...");
        }

        // ---- merchant id numeric edit (430-436) --------------------------------
        if (!isDigits(merchantId)) {
            throw fail("merchantId", "Merchant ID must be Numeric...");
        }

        // ---- ADD-TRANSACTION (442-466) -----------------------------------------
        Transaction t = new Transaction();
        t.setTransactionId(formatTransactionId(transactions.nextTransactionId()));
        t.setTypeCd(pad(typeCd, 2));
        t.setCategoryCd(Integer.parseInt(categoryCd));
        t.setSource(source);
        t.setDescription(description);
        t.setAmount(amountValue);
        t.setCardNumber(card.getCardNumber());
        t.setMerchantId(Long.parseLong(merchantId));
        t.setMerchantName(merchantName);
        t.setMerchantCity(merchantCity);
        t.setMerchantZip(merchantZip);
        // 464-465: TRAN-ORIG-TS and TRAN-PROC-TS are both MOVEd from the
        // user-entered dates (X(26) timestamp field, date part only → midnight)
        t.setOrigTs(orig.date().atStartOfDay());
        t.setProcTs(proc.date().atStartOfDay());
        transactions.save(t);
        return TransactionView.of(t, card.getAccountId());
    }

    /** 16-digit zero-padded rendering of the sequence value (plan R2). */
    static String formatTransactionId(long sequenceValue) {
        return String.format("%016d", sequenceValue);
    }

    private static String require(String value, String field, String message) {
        String s = Paging.blankToNull(value);
        if (s == null) {
            throw fail(field, message);
        }
        return s;
    }

    private static ValidationException fail(String field, String message) {
        return new ValidationException(List.of(new FieldError(field, message)));
    }

    private static boolean isDigits(String s) {
        return !s.isEmpty() && s.chars().allMatch(Character::isDigit);
    }

    private static String pad(String s, int width) {
        return s.length() >= width ? s : "0".repeat(width - s.length()) + s;
    }

    @Transactional(readOnly = true)
    public TransactionView get(String transactionId) {
        Transaction t = transactions.findById(normalizeId(transactionId))
                .orElseThrow(() -> new NotFoundException("Transaction " + transactionId + " not found"));
        return toView(t);
    }

    private TransactionView toView(Transaction t) {
        Long accountId = t.getCardNumber() == null
                ? null
                : cards.findById(t.getCardNumber()).map(c -> c.getAccountId()).orElse(null);
        return TransactionView.of(t, accountId);
    }

    /** Legacy ids are 16-char zero-padded; accept shorter numeric input. */
    static String normalizeId(String id) {
        String s = Paging.blankToNull(id);
        if (s == null) {
            return null;
        }
        if (s.length() < 16 && s.chars().allMatch(Character::isDigit)) {
            return "0".repeat(16 - s.length()) + s;
        }
        return s;
    }
}
