package com.carddemo.api.dto;

import java.time.LocalDate;
import java.time.LocalDateTime;

/** Report + statement DTOs (plan §6 — /reports and /statements endpoints). */
public final class ReportDtos {

    private ReportDtos() {
    }

    /**
     * CORPT00C screen input: report type + custom-range date PARTS (the BMS
     * map has separate month/day/year fields and the validation messages are
     * per-part, so the API accepts them the same way). Only used when
     * {@code reportType = "custom"}.
     */
    public record ReportRequest(
            String reportType,
            String startMonth,
            String startDay,
            String startYear,
            String endMonth,
            String endDay,
            String endYear) {
    }

    /** One report_runs row. */
    public record ReportRunView(
            long id,
            String reportType,
            LocalDate fromDate,
            LocalDate toDate,
            String status,
            String requestedBy,
            LocalDateTime createdAt,
            String message) {
    }

    /** One statements row (paths stay server-side; download via /statements/{id}). */
    public record StatementView(
            long id,
            long accountId,
            LocalDate cycleDate,
            LocalDateTime createdAt) {
    }
}
