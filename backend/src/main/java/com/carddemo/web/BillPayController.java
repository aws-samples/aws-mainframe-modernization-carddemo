package com.carddemo.web;

import com.carddemo.service.BillPayService;
import org.springframework.web.bind.annotation.*;

/** COBIL00C endpoint. */
@RestController
@RequestMapping("/api/accounts")
public class BillPayController {

    private final BillPayService service;

    public BillPayController(BillPayService service) {
        this.service = service;
    }

    @PostMapping("/{id}/payment")
    public Dto.BillPayResponse pay(@PathVariable String id,
                                   @RequestBody(required = false) Dto.BillPayRequest req) {
        return service.pay(id, req == null ? new Dto.BillPayRequest(null) : req);
    }
}
