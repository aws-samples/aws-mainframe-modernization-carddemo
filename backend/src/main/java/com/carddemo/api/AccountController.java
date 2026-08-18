package com.carddemo.api;

import com.carddemo.api.dto.AccountUpdateRequest;
import com.carddemo.api.dto.ApiDtos.AccountView;
import com.carddemo.api.dto.ApiDtos.TransactionView;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

/**
 * /api/v1/accounts/{id} — GET: COACTVWC combined account+customer view;
 * PUT: COACTUPC account+customer update; POST /{id}/bill-payment: COBIL00C
 * bill payment (plan §6).
 */
@RestController
@RequestMapping("/api/v1/accounts")
public class AccountController {

    /** Body of POST /accounts/{id}/bill-payment (plan §6): {confirm: true}. */
    public record BillPaymentRequest(Boolean confirm) {
    }

    private final AccountService accountService;
    private final BillPaymentService billPaymentService;

    public AccountController(AccountService accountService, BillPaymentService billPaymentService) {
        this.accountService = accountService;
        this.billPaymentService = billPaymentService;
    }

    @GetMapping("/{id}")
    public AccountView getAccount(@PathVariable("id") long id) {
        return accountService.getAccount(id);
    }

    @PutMapping("/{id}")
    public AccountView updateAccount(@PathVariable("id") long id,
                                     @RequestBody AccountUpdateRequest request) {
        return accountService.updateAccount(id, request);
    }

    /** COBIL00C — pay the full balance; returns the created transaction. */
    @PostMapping("/{id}/bill-payment")
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionView payBill(@PathVariable("id") long id,
                                   @RequestBody BillPaymentRequest request) {
        return billPaymentService.payBill(id, request.confirm());
    }
}
