package com.carddemo.api;

import com.carddemo.api.dto.AddTransactionRequest;
import com.carddemo.api.dto.ApiDtos.Page;
import com.carddemo.api.dto.ApiDtos.TransactionView;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

/**
 * GET /api/v1/transactions + /{id} — COTRN00C/COTRN01C;
 * POST /api/v1/transactions — COTRN02C add transaction (plan §6).
 */
@RestController
@RequestMapping("/api/v1/transactions")
public class TransactionController {

    private final TransactionService transactionService;

    public TransactionController(TransactionService transactionService) {
        this.transactionService = transactionService;
    }

    @GetMapping
    public Page<TransactionView> list(
            @RequestParam(name = "cursor", required = false) String cursor,
            @RequestParam(name = "limit", required = false) Integer limit,
            @RequestParam(name = "fromId", required = false) String fromId,
            @RequestParam(name = "cardNumber", required = false) String cardNumber) {
        return transactionService.list(cursor, limit, fromId, cardNumber);
    }

    @GetMapping("/{id}")
    public TransactionView get(@PathVariable("id") String id) {
        return transactionService.get(id);
    }

    /** COTRN02C add — the confirmed submit (the Y/N confirm loop is UI-side). */
    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionView add(@RequestBody AddTransactionRequest request) {
        return transactionService.addTransaction(request);
    }
}
