package com.carddemo.web;

import com.carddemo.service.AccountService;
import org.springframework.web.bind.annotation.*;

/** COACTVWC / COACTUPC endpoints. */
@RestController
@RequestMapping("/api/accounts")
public class AccountController {

    private final AccountService service;

    public AccountController(AccountService service) {
        this.service = service;
    }

    @GetMapping("/{id}")
    public Dto.AccountView view(@PathVariable String id) {
        return service.view(id);
    }

    @PutMapping("/{id}")
    public Dto.AccountView update(@PathVariable String id,
                                  @RequestBody Dto.AccountUpdateRequest req) {
        return service.update(id, req);
    }
}
