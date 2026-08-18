package com.carddemo.api;

import com.carddemo.api.ValidationException.FieldError;
import com.carddemo.api.dto.ApiDtos.TransactionView;
import com.carddemo.domain.Account;
import com.carddemo.domain.Card;
import com.carddemo.domain.Transaction;
import com.carddemo.repo.AccountRepository;
import com.carddemo.repo.CardRepository;
import com.carddemo.repo.TransactionRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;

/**
 * COBIL00C port (plan §4, ported exactly): pay the account balance in full.
 * <ol>
 *   <li>READ-ACCTDAT-FILE with UPDATE (line 351) → {@code SELECT ... FOR
 *       UPDATE} on the account row (plan R3), so concurrent payments on the
 *       same account serialize like CICS record locking.</li>
 *   <li>Balance check (198-205): {@code ACCT-CURR-BAL <= ZEROS} → the legacy
 *       message {@value #NOTHING_TO_PAY}.</li>
 *   <li>READ-CXACAIX-FILE (211): card resolved from the xref by account id —
 *       the AIX returns the first card in key order.</li>
 *   <li>TRAN-RECORD build (218-232): every literal exactly as COBIL00C
 *       225-229 — type '02', category 2, source 'POS TERM', description
 *       'BILL PAYMENT - ONLINE', merchant 999999999 / 'BILL PAYMENT' /
 *       city 'N/A' / zip 'N/A'; amount = full current balance; ORIG-TS and
 *       PROC-TS both = now (GET-CURRENT-TIMESTAMP, seconds precision — the
 *       legacy zeroes the microseconds, WS-TIMESTAMP-TM-MS6, line 266).
 *       Tran id from the DB sequence (plan R2), not READPREV max+1.</li>
 *   <li>Balance update (234-235): {@code curr_bal -= amount} → exactly 0.
 *       COBIL00C touches ONLY ACCT-CURR-BAL — it does NOT modify
 *       ACCT-CURR-CYC-CREDIT/DEBIT (cycle fields are reset by INTCALC,
 *       plan R7) — so this port only writes curr_bal.</li>
 * </ol>
 * All of it in ONE database transaction (WRITE + REWRITE under one CICS
 * syncpoint in the legacy task).
 */
@Service
public class BillPaymentService {

    /** COBIL00C:201 — balance <= 0 message, verbatim. */
    static final String NOTHING_TO_PAY = "You have nothing to pay...";
    /** COBIL00C:187 — confirm flag edit message, verbatim. */
    static final String INVALID_CONFIRM = "Invalid value. Valid values are (Y/N)...";

    private final AccountRepository accounts;
    private final CardRepository cards;
    private final TransactionRepository transactions;
    private final Clock clock;

    public BillPaymentService(AccountRepository accounts, CardRepository cards,
                              TransactionRepository transactions, Clock clock) {
        this.accounts = accounts;
        this.cards = cards;
        this.transactions = transactions;
        this.clock = clock;
    }

    @Transactional
    public TransactionView payBill(long accountId, Boolean confirm) {
        // CONFIRMI edit (COBIL00C:173-191): anything but Y is not a payment.
        // The REST body carries confirm as a boolean; only true proceeds.
        if (confirm == null || !confirm) {
            throw new ValidationException(List.of(new FieldError("confirm", INVALID_CONFIRM)));
        }

        // READ-ACCTDAT-FILE ... UPDATE → row lock (plan R3)
        Account account = accounts.findByIdForUpdate(accountId)
                .orElseThrow(() -> new NotFoundException("Account ID NOT found..."));

        // 198-205: ACCT-CURR-BAL <= ZEROS
        BigDecimal balance = account.getCurrBal();
        if (balance == null || balance.signum() <= 0) {
            throw new ValidationException(List.of(new FieldError("accountId", NOTHING_TO_PAY)));
        }

        // READ-CXACAIX-FILE: xref by account id → card (AIX key order)
        Card card = cards.findByAccountIdOrderByCardNumberAsc(accountId).stream()
                .findFirst()
                .orElseThrow(() -> new NotFoundException("Account ID NOT found..."));

        // 218-232: TRAN-RECORD with the exact legacy literals
        LocalDateTime now = LocalDateTime.now(clock).truncatedTo(ChronoUnit.SECONDS);
        Transaction t = new Transaction();
        t.setTransactionId(TransactionService.formatTransactionId(transactions.nextTransactionId()));
        t.setTypeCd("02");
        t.setCategoryCd(2);
        t.setSource("POS TERM");
        t.setDescription("BILL PAYMENT - ONLINE");
        t.setAmount(balance);
        t.setCardNumber(card.getCardNumber());
        t.setMerchantId(999_999_999L);
        t.setMerchantName("BILL PAYMENT");
        t.setMerchantCity("N/A");
        t.setMerchantZip("N/A");
        t.setOrigTs(now);
        t.setProcTs(now);
        transactions.save(t);

        // 234-235: COMPUTE ACCT-CURR-BAL = ACCT-CURR-BAL - TRAN-AMT (→ 0);
        // curr_cyc_credit/debit untouched — COBIL00C never writes them.
        account.setCurrBal(balance.subtract(balance));

        return TransactionView.of(t, accountId);
    }
}
