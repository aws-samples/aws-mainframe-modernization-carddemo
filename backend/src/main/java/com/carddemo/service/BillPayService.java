package com.carddemo.service;

import com.carddemo.model.*;
import com.carddemo.repo.*;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;

/** COBIL00C.cbl — bill payment. */
@Service
public class BillPayService {

    private final AccountRepository accounts;
    private final CardXrefRepository xrefs;
    private final TransactionRepository transactions;
    private final TransactionService transactionService;

    public BillPayService(AccountRepository accounts, CardXrefRepository xrefs,
                          TransactionRepository transactions,
                          TransactionService transactionService) {
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.transactions = transactions;
        this.transactionService = transactionService;
    }

    @Transactional
    public Dto.BillPayResponse pay(String idText, Dto.BillPayRequest req) {
        if (idText == null || idText.isBlank()) {
            throw ApiException.badRequest("Acct ID can NOT be empty...");
        }
        long acctId = AccountService.parseAccountId(idText);
        Account acct = accounts.findById(acctId)
                .orElseThrow(() -> ApiException.notFound("Account ID NOT found..."));

        if (acct.getCurrBal().compareTo(BigDecimal.ZERO) <= 0) {
            throw ApiException.badRequest("You have nothing to pay...");
        }

        String confirm = req.confirm() == null ? "" : req.confirm().trim();
        if (confirm.equalsIgnoreCase("N") || confirm.isEmpty()) {
            throw ApiException.badRequest("Confirm to make a bill payment...");
        }
        if (!confirm.equalsIgnoreCase("Y")) {
            throw ApiException.badRequest("Invalid value. Valid values are (Y/N)...");
        }

        String cardNum = xrefs.findFirstByAcctId(acctId)
                .map(CardXref::getCardNum).orElse("");

        TransactionRecord t = new TransactionRecord();
        t.setTranId(transactionService.nextTranId());
        t.setTypeCd("02");
        t.setCatCd(2);
        t.setSource("POS TERM");
        t.setDescription("BILL PAYMENT - ONLINE");
        t.setAmt(acct.getCurrBal());
        t.setCardNum(cardNum);
        t.setMerchantId(999999999L);
        t.setMerchantName("BILL PAYMENT");
        t.setMerchantCity("N/A");
        t.setMerchantZip("N/A");
        String ts = TransactionService.now();
        t.setOrigTs(ts);
        t.setProcTs(ts);
        transactions.save(t);

        acct.setCurrBal(acct.getCurrBal().subtract(t.getAmt()));
        accounts.save(acct);

        return new Dto.BillPayResponse(t.getTranId(), t.getAmt(), acct.getCurrBal(),
                "Payment successful. ");
    }
}
