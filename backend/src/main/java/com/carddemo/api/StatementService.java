package com.carddemo.api;

import com.carddemo.api.dto.ReportDtos.StatementView;
import com.carddemo.config.FilesConfig;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Statement index/download surface (plan §6): the {@code statements} table
 * rows written by the CBSTM03A/B statementJob, served per account, with the
 * txt or html file streamed on demand. Replaces retrieving STMTFILE/HTMLFILE
 * datasets outside CICS — any authenticated user.
 */
@Service
public class StatementService {

    private final JdbcTemplate jdbc;
    private final FilesConfig.FilesProperties files;

    public StatementService(JdbcTemplate jdbc, FilesConfig.FilesProperties files) {
        this.jdbc = jdbc;
        this.files = files;
    }

    /** GET /accounts/{id}/statements — newest first. 404 for unknown accounts. */
    public List<StatementView> listForAccount(long accountId) {
        Long exists = jdbc.queryForObject(
                "SELECT count(*) FROM accounts WHERE account_id = ?", Long.class, accountId);
        if (exists == null || exists == 0) {
            throw new NotFoundException("Account not found: " + accountId);
        }
        return jdbc.query("""
                        SELECT id, account_id, cycle_date, created_at
                        FROM statements WHERE account_id = ? ORDER BY id DESC
                        """,
                (rs, i) -> new StatementView(
                        rs.getLong("id"),
                        rs.getLong("account_id"),
                        rs.getObject("cycle_date", LocalDate.class),
                        rs.getObject("created_at", LocalDateTime.class)),
                accountId);
    }

    /** GET /statements/{id}?format=txt|html — path of the statement file. */
    public Path statementFile(long id, String format) {
        boolean html = switch (format == null ? "txt" : format) {
            case "txt" -> false;
            case "html" -> true;
            default -> throw new ValidationException(List.of(new ValidationException.FieldError(
                    "format", "format must be txt or html")));
        };
        List<String> paths = jdbc.query(
                "SELECT " + (html ? "html_path" : "txt_path") + " FROM statements WHERE id = ?",
                (rs, i) -> rs.getString(1), id);
        if (paths.isEmpty()) {
            throw new NotFoundException("Statement not found: " + id);
        }
        String p = paths.get(0);
        if (p == null) {
            throw new NotFoundException("Statement " + id + " has no " + format + " rendition");
        }
        Path path = Path.of(p);
        // containment guard: the stored path must stay inside the statements dir
        Path base = files.statementsDir().toAbsolutePath().normalize();
        if (!path.toAbsolutePath().normalize().startsWith(base)) {
            throw new NotFoundException("Statement file missing for id " + id);
        }
        if (!Files.isRegularFile(path)) {
            throw new NotFoundException("Statement file missing for id " + id);
        }
        return path;
    }
}
