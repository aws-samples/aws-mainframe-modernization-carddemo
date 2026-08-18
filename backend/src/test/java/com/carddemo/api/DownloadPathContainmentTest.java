package com.carddemo.api;

import com.carddemo.config.FilesConfig.FilesProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Review should-fix #3: statement/report downloads must refuse a stored path
 * that escapes the configured base directory (defense in depth — the rows are
 * written only by our batch jobs, but a poisoned row must not leak files).
 */
class DownloadPathContainmentTest {

    @SuppressWarnings("unchecked")
    private static JdbcTemplate jdbcReturning(Object row) {
        JdbcTemplate jdbc = mock(JdbcTemplate.class);
        when(jdbc.query(anyString(), any(RowMapper.class), eq(3L))).thenReturn(List.of(row));
        return jdbc;
    }

    @Test
    void statementFileOutsideStatementsDirIs404(@TempDir Path dir) throws Exception {
        Path outside = dir.resolve("secrets.html");
        Files.writeString(outside, "<html></html>");
        FilesProperties files = new FilesProperties(dir.resolve("files").toString());
        StatementService service =
                new StatementService(jdbcReturning(outside.toString()), files);

        assertThatThrownBy(() -> service.statementFile(3L, "html"))
                .isInstanceOf(NotFoundException.class);
    }

    @Test
    void statementFileInsideStatementsDirIsServed(@TempDir Path dir) throws Exception {
        FilesProperties files = new FilesProperties(dir.toString());
        Path inside = files.statementsDir().resolve("stmt-1-50.html");
        Files.createDirectories(files.statementsDir());
        Files.writeString(inside, "<html></html>");
        StatementService service =
                new StatementService(jdbcReturning(inside.toString()), files);

        assertThat(service.statementFile(3L, "html")).isEqualTo(inside);
    }

    @Test
    void reportFileOutsideReportsDirIs404(@TempDir Path dir) throws Exception {
        Path outside = dir.resolve("secrets.txt");
        Files.writeString(outside, "secret");
        FilesProperties files = new FilesProperties(dir.resolve("files").toString());
        ReportService service = new ReportService(
                jdbcReturning(new Object[]{"COMPLETED", outside.toString()}),
                mock(JobLauncher.class), mock(Job.class), files);

        assertThatThrownBy(() -> service.reportFile(3L))
                .isInstanceOf(NotFoundException.class);
    }

    @Test
    void reportFileInsideReportsDirIsServed(@TempDir Path dir) throws Exception {
        FilesProperties files = new FilesProperties(dir.toString());
        Path inside = files.reportsDir().resolve("tranrept-3.txt");
        Files.createDirectories(files.reportsDir());
        Files.writeString(inside, "REPORT BODY");
        ReportService service = new ReportService(
                jdbcReturning(new Object[]{"COMPLETED", inside.toString()}),
                mock(JobLauncher.class), mock(Job.class), files);

        assertThat(service.reportFile(3L)).isEqualTo(inside);
    }
}
