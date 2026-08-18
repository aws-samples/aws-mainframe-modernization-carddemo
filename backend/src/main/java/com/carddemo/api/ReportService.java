package com.carddemo.api;

import com.carddemo.api.dto.ReportDtos.ReportRequest;
import com.carddemo.api.dto.ReportDtos.ReportRunView;
import com.carddemo.api.ValidationException.FieldError;
import com.carddemo.batch.BatchConfig;
import com.carddemo.config.FilesConfig;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.JobExecutionException;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.util.List;
import java.util.Locale;

/**
 * CORPT00C port (plan §4/§6): request a monthly / yearly / custom-range
 * transaction report. The legacy program derives the date range, then submits
 * TRANREPT JCL through the CICS internal reader (SUBMIT-JOB-TO-INTRDR); here
 * the same derivation feeds a synchronous Spring Batch launch of
 * {@code transactionReportJob} (CBTRN03C), tracked by a {@code report_runs}
 * row. Available to every authenticated user — the legacy menu lists CORPT00
 * with usrtype 'U'.
 *
 * <p>Validation replicates the CORPT00C custom-range edit chain exactly —
 * per-part empty checks, NUMVAL-C normalization, month {@code > '12'} / day
 * {@code > '31'} string checks, then the CSUTLDTC whole-date edit — with the
 * exact legacy messages. Legacy shows one error at a time (each failed edit
 * SENDs the screen and returns), so the API reports the FIRST failure only.
 * The Y/N confirm loop is a UI concern (the SPA confirm dialog), matching how
 * the other confirm screens were ported.</p>
 */
@Service
public class ReportService {

    private final JdbcTemplate jdbc;
    private final JobLauncher jobLauncher;
    private final Job transactionReportJob;
    private final FilesConfig.FilesProperties files;

    public ReportService(JdbcTemplate jdbc, JobLauncher jobLauncher,
                         @Qualifier(BatchConfig.TRANSACTION_REPORT_JOB) Job transactionReportJob,
                         FilesConfig.FilesProperties files) {
        this.jdbc = jdbc;
        this.jobLauncher = jobLauncher;
        this.transactionReportJob = transactionReportJob;
        this.files = files;
    }

    /** The PROCESS-ENTER-KEY outcome: report display name + PARM date range. */
    record Derived(String reportName, LocalDate from, LocalDate to) {
    }

    /**
     * PROCESS-ENTER-KEY date derivation + custom-range validation, extracted
     * with an injectable "today" (FUNCTION CURRENT-DATE) for the test matrix.
     */
    static Derived derive(ReportRequest request, LocalDate today) {
        String type = request.reportType() == null ? "" : request.reportType().toLowerCase(Locale.ROOT);
        return switch (type) {
            case "monthly" -> {
                // start = current year/month + '01'; end = first-of-next-month - 1
                LocalDate from = today.withDayOfMonth(1);
                yield new Derived("Monthly", from, from.plusMonths(1).minusDays(1));
            }
            case "yearly" -> new Derived("Yearly",
                    LocalDate.of(today.getYear(), 1, 1),
                    LocalDate.of(today.getYear(), 12, 31));
            case "custom" -> {
                LocalDate[] range = validateCustomRange(request);
                yield new Derived("Custom", range[0], range[1]);
            }
            default -> throw new ValidationException(List.of(
                    new FieldError("reportType", "Select a report type to print report...")));
        };
    }

    /** POST /reports/transaction — derive range, insert run, launch job. */
    public ReportRunView requestReport(ReportRequest request, String requestedBy) {
        Derived derived = derive(request, LocalDate.now());
        String type = request.reportType().toLowerCase(Locale.ROOT);
        LocalDate from = derived.from();
        LocalDate to = derived.to();

        Long runId = jdbc.queryForObject("""
                        INSERT INTO report_runs (report_type, from_date, to_date, status, requested_by)
                        VALUES (?, ?, ?, 'PENDING', ?) RETURNING id
                        """,
                Long.class, type, from, to, requestedBy);

        JobParameters params = new JobParametersBuilder()
                .addLong("reportRunId", runId)
                .addString("fromDate", from.toString())
                .addString("toDate", to.toString())
                .addLong("requestedAt", System.currentTimeMillis())
                .toJobParameters();
        try {
            // synchronous, like the other admin-triggered jobs (demo scale)
            jobLauncher.run(transactionReportJob, params);
        } catch (JobExecutionException e) {
            jdbc.update("UPDATE report_runs SET status = 'FAILED' WHERE id = ?", runId);
            throw new ConflictException("Could not launch report job: " + e.getMessage());
        }

        ReportRunView run = getRun(runId);
        // legacy success line: '<name> report submitted for printing ...'
        return withMessage(run, derived.reportName() + " report submitted for printing ...");
    }

    /**
     * The CORPT00C custom-range edit chain, first failure wins. Returns
     * {from, to} on success.
     */
    private static LocalDate[] validateCustomRange(ReportRequest r) {
        // per-part empty checks, in legacy EVALUATE order
        requireNotBlank(r.startMonth(), "startMonth", "Start Date - Month can NOT be empty...");
        requireNotBlank(r.startDay(), "startDay", "Start Date - Day can NOT be empty...");
        requireNotBlank(r.startYear(), "startYear", "Start Date - Year can NOT be empty...");
        requireNotBlank(r.endMonth(), "endMonth", "End Date - Month can NOT be empty...");
        requireNotBlank(r.endDay(), "endDay", "End Date - Day can NOT be empty...");
        requireNotBlank(r.endYear(), "endYear", "End Date - Year can NOT be empty...");

        // NUMVAL-C normalization + numeric/range edits, in legacy IF order
        Integer sm = numvalC(r.startMonth());
        if (sm == null || sm > 12) {
            fail("startMonth", "Start Date - Not a valid Month...");
        }
        Integer sd = numvalC(r.startDay());
        if (sd == null || sd > 31) {
            fail("startDay", "Start Date - Not a valid Day...");
        }
        Integer sy = numvalC(r.startYear());
        if (sy == null) {
            fail("startYear", "Start Date - Not a valid Year...");
        }
        Integer em = numvalC(r.endMonth());
        if (em == null || em > 12) {
            fail("endMonth", "End Date - Not a valid Month...");
        }
        Integer ed = numvalC(r.endDay());
        if (ed == null || ed > 31) {
            fail("endDay", "End Date - Not a valid Day...");
        }
        Integer ey = numvalC(r.endYear());
        if (ey == null) {
            fail("endYear", "End Date - Not a valid Year...");
        }

        // CSUTLDTC (CEEDAYS) whole-date edit — strict calendar check
        LocalDate from = toDate(sy, sm, sd, "startMonth", "Start Date - Not a valid date...");
        LocalDate to = toDate(ey, em, ed, "endMonth", "End Date - Not a valid date...");
        return new LocalDate[]{from, to};
    }

    private static void requireNotBlank(String v, String field, String message) {
        if (v == null || v.isBlank()) {
            fail(field, message);
        }
    }

    /**
     * FUNCTION NUMVAL-C tolerates digits with spaces/commas/currency; anything
     * else leaves the field non-numeric (→ the 'Not a valid ...' edit fires).
     * Returns null when not numeric. Month/day/year zero-relations (e.g. '0')
     * pass, exactly like the legacy string compares.
     */
    private static Integer numvalC(String v) {
        String cleaned = v.trim().replace(",", "").replace("$", "");
        if (cleaned.isEmpty() || !cleaned.chars().allMatch(Character::isDigit)) {
            return null;
        }
        try {
            return Integer.parseInt(cleaned);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static LocalDate toDate(int year, int month, int day, String field, String message) {
        try {
            return LocalDate.of(year, month, day);
        } catch (DateTimeException e) {
            fail(field, message);
            throw new IllegalStateException("unreachable");
        }
    }

    private static void fail(String field, String message) {
        throw new ValidationException(List.of(new FieldError(field, message)));
    }

    // ---- listing + download -------------------------------------------------

    private static final RowMapper<ReportRunView> RUN_ROW = (rs, i) -> new ReportRunView(
            rs.getLong("id"),
            rs.getString("report_type"),
            rs.getObject("from_date", LocalDate.class),
            rs.getObject("to_date", LocalDate.class),
            rs.getString("status"),
            rs.getString("requested_by"),
            rs.getObject("created_at", java.time.LocalDateTime.class),
            null);

    /** GET /reports — newest first. */
    public List<ReportRunView> listRuns() {
        return jdbc.query("""
                        SELECT id, report_type, from_date, to_date, status, requested_by, created_at
                        FROM report_runs ORDER BY id DESC
                        """,
                RUN_ROW);
    }

    /** GET /reports/{id}/download — path of the completed run's file. */
    public Path reportFile(long id) {
        List<Object[]> rows = jdbc.query(
                "SELECT status, file_path FROM report_runs WHERE id = ?",
                (rs, i) -> new Object[]{rs.getString(1), rs.getString(2)}, id);
        if (rows.isEmpty()) {
            throw new NotFoundException("Report run not found: " + id);
        }
        String status = (String) rows.get(0)[0];
        String filePath = (String) rows.get(0)[1];
        if (!"COMPLETED".equals(status)) {
            throw new ConflictException("Report run " + id + " is not completed (status " + status + ")");
        }
        Path path = Path.of(filePath);
        // containment guard: the stored path must stay inside the reports dir
        Path base = files.reportsDir().toAbsolutePath().normalize();
        if (!path.toAbsolutePath().normalize().startsWith(base)) {
            throw new NotFoundException("Report file missing for run " + id);
        }
        if (!Files.isRegularFile(path)) {
            throw new NotFoundException("Report file missing for run " + id);
        }
        return path;
    }

    private ReportRunView getRun(long id) {
        return jdbc.queryForObject("""
                        SELECT id, report_type, from_date, to_date, status, requested_by, created_at
                        FROM report_runs WHERE id = ?
                        """,
                RUN_ROW, id);
    }

    private static ReportRunView withMessage(ReportRunView r, String message) {
        return new ReportRunView(r.id(), r.reportType(), r.fromDate(), r.toDate(),
                r.status(), r.requestedBy(), r.createdAt(), message);
    }
}
