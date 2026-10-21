package com.carddemo.web;

import com.carddemo.service.TransactionService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

/** COTRN00C / COTRN01C / COTRN02C endpoints. */
@RestController
@RequestMapping("/api/transactions")
public class TransactionController {

    private final TransactionService service;

    public TransactionController(TransactionService service) {
        this.service = service;
    }

    @GetMapping
    public Dto.PageResult<Dto.TranView> list(@RequestParam(defaultValue = "0") int page) {
        return service.list(page);
    }

    @GetMapping("/{id}")
    public Dto.TranView view(@PathVariable String id) {
        return service.view(id);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public Dto.TranView add(@RequestBody Dto.TranAddRequest req) {
        return service.add(req);
    }
}
