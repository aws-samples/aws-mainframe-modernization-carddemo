package com.carddemo.service;

import com.carddemo.repo.TransactionRepository;
import com.carddemo.util.DateValidator;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

/** CORPT00C.cbl — transaction report by date range (Monthly / Yearly / Custom). */
@Service
public class ReportService {

    private static final DateTimeFormatter ISO = DateTimeFormatter.ISO_LOCAL_DATE;

    private final TransactionRepository transactions;

    public ReportService(TransactionRepository transactions) {
        this.transactions = transactions;
    }

    public Dto.ReportResponse report(String type, String from, String to) {
        LocalDate today = LocalDate.now();
        String name;
        String start;
        String end;
        switch (type == null ? "custom" : type.toLowerCase()) {
            case "monthly" -> {
                name = "Monthly";
                start = today.withDayOfMonth(1).format(ISO);
                end = today.format(ISO);
            }
            case "yearly" -> {
                name = "Yearly";
                start = today.withDayOfYear(1).format(ISO);
                end = today.withMonth(12).withDayOfMonth(31).format(ISO);
            }
            case "custom" -> {
                name = "Custom";
                start = requireDate("Start Date", from);
                end = requireDate("End Date", to);
            }
            default -> throw ApiException.badRequest("Select a report type to print report...");
        }

        if (DateValidator.parse(start).isAfter(DateValidator.parse(end))) {
            throw ApiException.badRequest("Start Date cannot be greater than End Date...");
        }

        List<Dto.TranView> items = transactions.findInDateRange(start, end).stream()
                .map(Dto.TranView::of).toList();
        BigDecimal total = items.stream().map(Dto.TranView::amt)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        return new Dto.ReportResponse(name, start, end, items, total, total);
    }

    private static String requireDate(String label, String v) {
        if (v == null || v.isBlank()) {
            throw ApiException.badRequest(label + " can NOT be empty...");
        }
        if (!DateValidator.isValid(v)) {
            throw ApiException.badRequest(label + " - Not a valid date...");
        }
        return v;
    }
}
