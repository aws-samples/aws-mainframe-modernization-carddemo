package com.carddemo.batch;

import org.springframework.jdbc.core.JdbcTemplate;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Golden-file helpers for the batch parity tests (plan §4 golden-file
 * strategy). The goldens under src/test/resources/golden/ were derived
 * INDEPENDENTLY of the Java under test by golden/derive_goldens.py (a
 * hand-executed port of the CBTRN02C/CBACT04C rules straight from the COBOL,
 * reading the raw ASCII fixtures itself) — so a bug in the Java cannot leak
 * into the expected values.
 *
 * <p>Format: TSV, header row, NULL rendered as the empty string, money at
 * scale 2, timestamps as {@code yyyy-MM-dd'T'HH:mm:ss}.</p>
 */
final class GoldenFiles {

    private static final DateTimeFormatter TS =
            DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss");

    private GoldenFiles() {
    }

    /** Reads a committed golden TSV from the test classpath (header included). */
    static List<String> golden(String path) {
        try (InputStream in = GoldenFiles.class.getResourceAsStream("/golden/" + path)) {
            if (in == null) {
                throw new IllegalArgumentException("missing golden file: " + path);
            }
            try (BufferedReader r = new BufferedReader(
                    new InputStreamReader(in, StandardCharsets.US_ASCII))) {
                List<String> lines = new ArrayList<>();
                String line;
                while ((line = r.readLine()) != null) {
                    lines.add(line);
                }
                return lines;
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * Runs the query and renders every row as a golden-format TSV line
     * (same NULL/scale/timestamp conventions as derive_goldens.py), prefixed
     * with the given header line.
     */
    static List<String> dump(JdbcTemplate jdbc, String header, String sql) {
        List<String> lines = new ArrayList<>();
        lines.add(header);
        lines.addAll(jdbc.query(sql, (rs, i) -> {
            int cols = rs.getMetaData().getColumnCount();
            StringBuilder sb = new StringBuilder();
            for (int c = 1; c <= cols; c++) {
                if (c > 1) {
                    sb.append('\t');
                }
                sb.append(cell(rs.getObject(c)));
            }
            return sb.toString();
        }));
        return lines;
    }

    private static String cell(Object v) {
        if (v == null) {
            return "";
        }
        if (v instanceof BigDecimal d) {
            return d.setScale(2).toPlainString();
        }
        if (v instanceof Timestamp t) {
            return t.toLocalDateTime().format(TS);
        }
        return v.toString().trim();
    }
}
