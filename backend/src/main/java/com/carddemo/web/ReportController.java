package com.carddemo.web;

import com.carddemo.service.ReportService;
import org.springframework.web.bind.annotation.*;

/** CORPT00C endpoint. */
@RestController
@RequestMapping("/api/reports")
public class ReportController {

    private final ReportService service;

    public ReportController(ReportService service) {
        this.service = service;
    }

    @GetMapping("/transactions")
    public Dto.ReportResponse transactions(@RequestParam(defaultValue = "custom") String type,
                                           @RequestParam(required = false) String from,
                                           @RequestParam(required = false) String to) {
        return service.report(type, from, to);
    }
}
