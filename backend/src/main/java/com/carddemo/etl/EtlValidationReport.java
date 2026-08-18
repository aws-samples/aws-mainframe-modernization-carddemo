package com.carddemo.etl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Row-count + checksum validation report (plan §2.3). Each entry compares
 * counts (and NUMERIC sums, where applicable) computed from the parsed source
 * against what actually landed in the database. Any mismatch, FK violation, or
 * assertion failure turns the report RED and aborts the load.
 */
public class EtlValidationReport {

    public record Check(String table, String metric, String expected, String actual, boolean ok) {
    }

    private final List<Check> checks = new ArrayList<>();
    private final List<String> violations = new ArrayList<>();

    public void addCount(String table, long expected, long actual) {
        checks.add(new Check(table, "row count", String.valueOf(expected),
                String.valueOf(actual), expected == actual));
    }

    public void addChecksum(String table, String column, BigDecimal expected, BigDecimal actual) {
        checks.add(new Check(table, "SUM(" + column + ")", expected.toPlainString(),
                actual.toPlainString(), expected.compareTo(actual) == 0));
    }

    /** Records an FK/integrity violation — reported, never silently dropped. */
    public void addViolation(String message) {
        violations.add(message);
    }

    public boolean isGreen() {
        return violations.isEmpty() && checks.stream().allMatch(Check::ok);
    }

    public List<Check> checks() {
        return checks;
    }

    public List<String> violations() {
        return violations;
    }

    public String render() {
        StringBuilder sb = new StringBuilder();
        sb.append("\n================ ETL VALIDATION REPORT ================\n");
        sb.append(String.format("%-32s %-22s %18s %18s  %s%n",
                "TABLE", "METRIC", "EXPECTED", "ACTUAL", "OK"));
        for (Check c : checks) {
            sb.append(String.format("%-32s %-22s %18s %18s  %s%n",
                    c.table(), c.metric(), c.expected(), c.actual(), c.ok() ? "PASS" : "FAIL"));
        }
        if (!violations.isEmpty()) {
            sb.append("---- integrity violations (").append(violations.size()).append(") ----\n");
            for (String v : violations) {
                sb.append("  VIOLATION: ").append(v).append('\n');
            }
        }
        sb.append("RESULT: ").append(isGreen() ? "GREEN" : "RED").append('\n');
        sb.append("=======================================================\n");
        return sb.toString();
    }
}
