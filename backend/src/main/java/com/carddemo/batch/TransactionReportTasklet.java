package com.carddemo.batch;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.jdbc.core.JdbcTemplate;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * CBTRN03C (TRANREPT) port — the daily transaction detail report, written as
 * 133-character fixed-width lines (CVTRA07Y layouts) to a file under the
 * configured reports directory.
 *
 * <p><b>Input equivalence:</b> legacy TRANFILE is produced by TRANREPT.prc
 * STEP05R — a SORT of TRANSACT with {@code SORT FIELDS=(TRAN-CARD-NUM,A)} and
 * {@code INCLUDE COND=(TRAN-PROC-DT between PARM dates)}. The SQL mirror is
 * {@code proc_ts::date BETWEEN from AND to ORDER BY card_number,
 * transaction_id} (within equal card numbers a DFSORT without EQUALS makes no
 * ordering promise; the KSDS input arrives in TRAN-ID order, so id order is
 * the faithful tiebreak). CBTRN03C then re-checks the date range per record
 * (lines 173–174) — redundant against this query, kept out.</p>
 *
 * <p><b>Line accounting (replicated exactly):</b></p>
 * <ul>
 *   <li>WS-PAGE-SIZE = 20; before EVERY detail after the first headers,
 *       {@code IF MOD(WS-LINE-COUNTER, 20) = 0} → page totals + headers
 *       (1100-WRITE-TRANSACTION-REPORT). The counter counts headers (4),
 *       page-total block (2), account-total block (2) and details (1) —
 *       grand totals are not counted (nothing follows them).</li>
 *   <li>Card break (WS-CURR-CARD-NUM ≠ TRAN-CARD-NUM): write the account
 *       totals for the PREVIOUS card (unless first time) and look up the
 *       xref account for the new card. "Account totals" therefore break per
 *       CARD, not per account.</li>
 *   <li><b>EOF quirk (CBTRN03C.cbl:197–204, replicated):</b> the EOF branch
 *       re-ADDs the LAST record's TRAN-AMT to the page and account totals
 *       (TRAN-RECORD still holds it after the failed READ), then writes page
 *       totals and grand totals. No account-total line is written at EOF —
 *       the last card's account total is silently dropped.</li>
 *   <li>Empty range: first-time headers never trigger, and the EOF branch
 *       still writes a page-total (spaces — zero suppresses the whole
 *       +ZZZ field) and grand-total line, with TRAN-AMT = 0.</li>
 * </ul>
 *
 * <p>Lookups mirror 1500-A/B/C: xref by card, type description (first 15
 * chars printed), category description (first 29 chars printed); a miss
 * abends the legacy program → here the tasklet throws, failing the job.</p>
 */
public class TransactionReportTasklet implements Tasklet {

    private static final Logger log = LoggerFactory.getLogger(TransactionReportTasklet.class);

    private static final int PAGE_SIZE = 20;
    private static final String BLANK_LINE = " ".repeat(133);
    private static final String DASH_LINE = "-".repeat(133);   // TRANSACTION-HEADER-2

    // TRANSACTION-HEADER-1 (CVTRA07Y): fixed captions, 133 wide with the
    // trailing FILLER X(16) VALUE '        Amount' + implicit space padding
    private static final String HEADER_1 =
            CobolPic.padX("Transaction ID", 17)
                    + CobolPic.padX("Account ID", 12)
                    + CobolPic.padX("Transaction Type", 19)
                    + CobolPic.padX("Tran Category", 35)
                    + CobolPic.padX("Tran Source", 14)
                    + " "
                    + CobolPic.padX("        Amount", 16)
                    + " ".repeat(19);

    private final JdbcTemplate jdbc;
    private final Path reportsDir;

    public TransactionReportTasklet(JdbcTemplate jdbc, Path reportsDir) {
        this.jdbc = jdbc;
        this.reportsDir = reportsDir;
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        Map<String, Object> params = chunkContext.getStepContext().getJobParameters();
        long reportRunId = (Long) params.get("reportRunId");
        LocalDate from = LocalDate.parse((String) params.get("fromDate"));
        LocalDate to = LocalDate.parse((String) params.get("toDate"));

        Path file = reportsDir.resolve("tranrept-" + reportRunId + ".txt");
        try {
            Files.createDirectories(reportsDir);
            int details;
            try (Writer out = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
                details = writeReport(out, from, to);
            }
            jdbc.update("UPDATE report_runs SET status = 'COMPLETED', file_path = ? WHERE id = ?",
                    file.toString(), reportRunId);
            log.info("TRANREPT: run {} wrote {} detail lines for {}..{} to {}",
                    reportRunId, details, from, to, file);
            contribution.incrementWriteCount(details);
        } catch (RuntimeException | IOException e) {
            jdbc.update("UPDATE report_runs SET status = 'FAILED' WHERE id = ?", reportRunId);
            if (e instanceof IOException io) {
                throw new UncheckedIOException(io);
            }
            throw (RuntimeException) e;
        }
        return RepeatStatus.FINISHED;
    }

    /** The CBTRN03C main loop; returns the number of detail lines written. */
    private int writeReport(Writer out, LocalDate from, LocalDate to) throws IOException {
        // TRANREPT.prc STEP05R sort+filter equivalent (class javadoc)
        List<ReportTxn> txns = jdbc.query("""
                        SELECT transaction_id, card_number, type_cd, category_cd, source, amount
                        FROM transactions
                        WHERE proc_ts::date BETWEEN ? AND ?
                        ORDER BY card_number, transaction_id
                        """,
                (rs, i) -> new ReportTxn(
                        rs.getString("transaction_id"),
                        rs.getString("card_number"),
                        rs.getString("type_cd"),
                        rs.getInt("category_cd"),
                        rs.getString("source"),
                        rs.getBigDecimal("amount")),
                from, to);

        Map<String, Long> acctByCard = new HashMap<>();
        Map<String, String> typeDesc = new HashMap<>();
        Map<String, String> catDesc = new HashMap<>();

        State s = new State(out, from, to);
        String currCard = null;      // WS-CURR-CARD-NUM
        long currAcct = 0;
        BigDecimal lastAmt = BigDecimal.ZERO;

        for (ReportTxn t : txns) {
            if (!t.cardNumber().equals(currCard)) {
                if (currCard != null) {                    // WS-FIRST-TIME = 'N'
                    s.writeAccountTotals();
                }
                currCard = t.cardNumber();
                currAcct = acctByCard.computeIfAbsent(currCard, this::lookupAccount);  // 1500-A
            }
            String tDesc = typeDesc.computeIfAbsent(t.typeCd(), this::lookupTypeDesc);      // 1500-B
            String cDesc = catDesc.computeIfAbsent(t.typeCd() + '|' + t.categoryCd(),
                    k -> lookupCategoryDesc(t.typeCd(), t.categoryCd()));                    // 1500-C
            s.writeDetail(t, currAcct, tDesc, cDesc);                                        // 1100
            lastAmt = t.amount();
        }

        // EOF branch (CBTRN03C.cbl:197-204) — last amount re-added, then
        // page totals + grand totals; NO account totals.
        s.pageTotal = s.pageTotal.add(lastAmt);
        s.accountTotal = s.accountTotal.add(lastAmt);
        s.writePageTotals();
        s.writeGrandTotals();
        return txns.size();
    }

    /** 1500-A-LOOKUP-XREF — abend (job failure) on a card not in the xref. */
    private long lookupAccount(String cardNumber) {
        List<Long> ids = jdbc.queryForList(
                "SELECT account_id FROM cards WHERE card_number = ?", Long.class, cardNumber);
        if (ids.isEmpty()) {
            throw new IllegalStateException("INVALID CARD NUMBER : " + cardNumber
                    + " — aborting for parity (CBTRN03C 1500-A abend)");
        }
        return ids.get(0);
    }

    /** 1500-B-LOOKUP-TRANTYPE — abend on a missing type. */
    private String lookupTypeDesc(String typeCd) {
        List<String> descs = jdbc.queryForList(
                "SELECT description FROM transaction_types WHERE type_cd = ?", String.class, typeCd);
        if (descs.isEmpty()) {
            throw new IllegalStateException("INVALID TRANSACTION TYPE : " + typeCd
                    + " — aborting for parity (CBTRN03C 1500-B abend)");
        }
        return descs.get(0);
    }

    /** 1500-C-LOOKUP-TRANCATG — abend on a missing (type, category) key. */
    private String lookupCategoryDesc(String typeCd, int categoryCd) {
        List<String> descs = jdbc.queryForList("""
                        SELECT description FROM transaction_categories
                        WHERE type_cd = ? AND category_cd = ?
                        """, String.class, typeCd, categoryCd);
        if (descs.isEmpty()) {
            throw new IllegalStateException("INVALID TRAN CATG KEY : " + typeCd + categoryCd
                    + " — aborting for parity (CBTRN03C 1500-C abend)");
        }
        return descs.get(0);
    }

    /** WS-REPORT-VARS + the 1100/1110/1120 write paragraphs. */
    private static final class State {
        final Writer out;
        final String startDate;
        final String endDate;
        boolean firstTime = true;        // WS-FIRST-TIME
        long lineCounter = 0;            // WS-LINE-COUNTER
        BigDecimal pageTotal = BigDecimal.ZERO;
        BigDecimal accountTotal = BigDecimal.ZERO;
        BigDecimal grandTotal = BigDecimal.ZERO;

        State(Writer out, LocalDate from, LocalDate to) {
            this.out = out;
            this.startDate = from.toString();
            this.endDate = to.toString();
        }

        /** 1100-WRITE-TRANSACTION-REPORT. */
        void writeDetail(ReportTxn t, long accountId, String typeDesc, String catDesc)
                throws IOException {
            if (firstTime) {
                firstTime = false;
                writeHeaders();
            }
            if (lineCounter % PAGE_SIZE == 0) {
                writePageTotals();
                writeHeaders();
            }
            pageTotal = pageTotal.add(t.amount());
            accountTotal = accountTotal.add(t.amount());
            // TRANSACTION-DETAIL-REPORT (CVTRA07Y): 16+1+11+1+2+1+15+1+4+1+29+1+10+4+15+2
            String catCd = String.format("%04d", t.categoryCd());
            write(CobolPic.padX(t.transactionId(), 16) + " "
                    + CobolPic.padX(String.format("%011d", accountId), 11) + " "
                    + CobolPic.padX(t.typeCd(), 2) + "-"
                    + CobolPic.padX(typeDesc, 15) + " "
                    + catCd + "-"
                    + CobolPic.padX(catDesc, 29) + " "
                    + CobolPic.padX(t.source(), 10)
                    + " ".repeat(4)
                    + CobolPic.minusZzz(t.amount())
                    + "  "
                    // MOVE of the 114-byte record into FD PIC X(133) pads right
                    + " ".repeat(19));
            lineCounter++;
        }

        /** 1120-WRITE-HEADERS: name header, blank, header-1, header-2 (4 lines). */
        void writeHeaders() throws IOException {
            write(CobolPic.padX("DALYREPT", 38)
                    + CobolPic.padX("Daily Transaction Report", 41)
                    + "Date Range: " + CobolPic.padX(startDate, 10)
                    + " to " + CobolPic.padX(endDate, 10)
                    + " ".repeat(18));
            lineCounter++;
            write(BLANK_LINE);
            lineCounter++;
            write(HEADER_1);
            lineCounter++;
            write(DASH_LINE);
            lineCounter++;
        }

        /** 1110-WRITE-PAGE-TOTALS: total + dashes; rolls into the grand total. */
        void writePageTotals() throws IOException {
            write(CobolPic.padX("Page Total", 11) + ".".repeat(86)
                    + CobolPic.plusZzz(pageTotal) + " ".repeat(21));
            grandTotal = grandTotal.add(pageTotal);
            pageTotal = BigDecimal.ZERO;
            lineCounter++;
            write(DASH_LINE);
            lineCounter++;
        }

        /** 1120-WRITE-ACCOUNT-TOTALS: card-break total + dashes. */
        void writeAccountTotals() throws IOException {
            write(CobolPic.padX("Account Total", 13) + ".".repeat(84)
                    + CobolPic.plusZzz(accountTotal) + " ".repeat(21));
            accountTotal = BigDecimal.ZERO;
            lineCounter++;
            write(DASH_LINE);
            lineCounter++;
        }

        /** 1110-WRITE-GRAND-TOTALS. */
        void writeGrandTotals() throws IOException {
            write(CobolPic.padX("Grand Total", 11) + ".".repeat(86)
                    + CobolPic.plusZzz(grandTotal) + " ".repeat(21));
        }

        private void write(String line) throws IOException {
            if (line.length() != 133) {
                throw new IllegalStateException(
                        "report line is " + line.length() + " chars, want 133: [" + line + "]");
            }
            out.write(line);
            out.write('\n');
        }
    }

    private record ReportTxn(String transactionId, String cardNumber, String typeCd,
                             int categoryCd, String source, BigDecimal amount) {
    }
}
