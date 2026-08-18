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
import java.util.List;
import java.util.Map;

/**
 * CBSTM03A/CBSTM03B (CREASTMT) port — account statements in both plain text
 * (80-char lines, STMTFILE) and HTML (100-char lines, HTMLFILE), one statement
 * per card-xref record in card-number order (the KSDS key order the legacy
 * sequential XREFFILE read produces).
 *
 * <p><b>Deliberate divergence (stated in the phase report):</b> legacy
 * concatenates every statement into ONE STMTFILE and ONE HTMLFILE (the HTML
 * file is many complete {@code <!DOCTYPE html>} documents back to back). Here
 * each statement is its own pair of files under the statements dir, indexed by
 * a {@code statements} row (plan §1/§2.2) so the API can serve them per
 * account. Line layouts, write order, and formatting inside each statement
 * follow the COBOL exactly.</p>
 *
 * <p>Fidelity notes (all from CBSTM03A):</p>
 * <ul>
 *   <li>Transactions come from TRNXFILE = TRANSACT sorted by (card, tran-id)
 *       (CREASTMT.JCL SORT FIELDS=(263,16,CH,A,1,16,CH,A)) — mirrored as
 *       {@code WHERE card_number = ? ORDER BY transaction_id}. The legacy 2-D
 *       table caps at 10 transactions per card (OCCURS 10, would overflow
 *       beyond); we impose no cap — divergence only reachable with >10 txns
 *       per card (fixture max is 6).</li>
 *   <li>ST-LINE1 name / ST-LINE4 address-3 are STRING ... DELIMITED BY ' '
 *       concatenations: each source field contributes only up to its first
 *       space, joined by single spaces (so a two-word city yields only its
 *       first word — legacy quirk, replicated).</li>
 *   <li>The HTML name/address paragraphs STRING the text DELIMITED BY
 *       {@code '  '} (two spaces) and append {@code '  </p>'}; the
 *       basic-details and transaction cells embed the FULL fixed-width field
 *       (padding included) because they are delimited by {@code '*'}.</li>
 *   <li>Total EXP is the sum of the card's transaction amounts
 *       (ADD TRNX-AMT TO WS-TOTAL-AMT), reset per xref record.</li>
 *   <li>Amounts: ST-CURR-BAL is PIC 9(9).99- (zero-padded, trailing sign);
 *       ST-TRANAMT / ST-TOTAL-TRAMT are PIC Z(9).99- (zero-suppressed).</li>
 * </ul>
 */
public class StatementTasklet implements Tasklet {

    private static final Logger log = LoggerFactory.getLogger(StatementTasklet.class);

    private static final String STAR_LINE0 = "*".repeat(31) + "START OF STATEMENT" + "*".repeat(31);
    private static final String STAR_LINE15 = "*".repeat(32) + "END OF STATEMENT" + "*".repeat(32);
    private static final String DASH_80 = "-".repeat(80);
    private static final String LINE6 = " ".repeat(33) + CobolPic.padX("Basic Details", 14) + " ".repeat(33);
    private static final String LINE11 = " ".repeat(30) + CobolPic.padX("TRANSACTION SUMMARY ", 20) + " ".repeat(30);
    private static final String LINE13 = CobolPic.padX("Tran ID         ", 16)
            + CobolPic.padX("Tran Details    ", 51) + CobolPic.padX("  Tran Amount", 13);

    // HTML-LINES 88-level fixed lines (CBSTM03A), written as 100-char records
    private static final String H_L01 = "<!DOCTYPE html>";
    private static final String H_L02 = "<html lang=\"en\">";
    private static final String H_L03 = "<head>";
    private static final String H_L04 = "<meta charset=\"utf-8\">";
    private static final String H_L05 = "<title>HTML Table Layout</title>";
    private static final String H_L06 = "</head>";
    private static final String H_L07 = "<body style=\"margin:0px;\">";
    private static final String H_L08 = "<table  align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">";
    private static final String H_TRS = "<tr>";
    private static final String H_TRE = "</tr>";
    private static final String H_TDE = "</td>";
    private static final String H_L10 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">";
    private static final String H_L15 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">";
    private static final String H_L16 = "<p style=\"font-size:16px\">Bank of XYZ</p>";
    private static final String H_L17 = "<p>410 Terry Ave N</p>";
    private static final String H_L18 = "<p>Seattle WA 99999</p>";
    private static final String H_L22_35 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">";
    private static final String H_L30_42 = "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">";
    private static final String H_L31 = "<p style=\"font-size:16px\">Basic Details</p>";
    private static final String H_L43 = "<p style=\"font-size:16px\">Transaction Summary</p>";
    private static final String H_L47 = "<td style=\"width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">";
    private static final String H_L48 = "<p style=\"font-size:16px\">Tran ID</p>";
    private static final String H_L50 = "<td style=\"width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">";
    private static final String H_L51 = "<p style=\"font-size:16px\">Tran Details</p>";
    private static final String H_L53 = "<td style=\"width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;\">";
    private static final String H_L54 = "<p style=\"font-size:16px\">Amount</p>";
    private static final String H_L58 = "<td style=\"width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">";
    private static final String H_L61 = "<td style=\"width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">";
    private static final String H_L64 = "<td style=\"width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;\">";
    private static final String H_L75 = "<h3>End of Statement</h3>";
    private static final String H_L78 = "</table>";
    private static final String H_L79 = "</body>";
    private static final String H_L80 = "</html>";

    private final JdbcTemplate jdbc;
    private final Path statementsDir;

    public StatementTasklet(JdbcTemplate jdbc, Path statementsDir) {
        this.jdbc = jdbc;
        this.statementsDir = statementsDir;
    }

    @Override
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) {
        Map<String, Object> params = chunkContext.getStepContext().getJobParameters();
        LocalDate cycleDate = params.containsKey("cycleDate")
                ? LocalDate.parse((String) params.get("cycleDate"))
                : LocalDate.now();
        long runId = chunkContext.getStepContext().getStepExecution().getJobExecutionId();

        // 1000-XREFFILE-GET-NEXT: xref records in card-number (KSDS key) order,
        // with the 2000-CUSTFILE-GET / 3000-ACCTFILE-GET reads joined in.
        // A missing customer/account row abends the legacy program; inner joins
        // plus the ETL's xref FK backfill give the same guarantee (a dangling
        // reference would have failed the load).
        List<XrefRow> xrefs = jdbc.query("""
                        SELECT cd.card_number, a.account_id, a.curr_bal,
                               c.first_name, c.middle_name, c.last_name,
                               c.addr_line_1, c.addr_line_2, c.addr_line_3,
                               c.addr_state_cd, c.addr_country_cd, c.addr_zip,
                               c.fico_score
                        FROM cards cd
                        JOIN accounts a ON a.account_id = cd.account_id
                        JOIN customers c ON c.customer_id = a.customer_id
                        ORDER BY cd.card_number
                        """,
                (rs, i) -> new XrefRow(
                        rs.getString("card_number"),
                        rs.getLong("account_id"),
                        rs.getBigDecimal("curr_bal"),
                        rs.getString("first_name"),
                        rs.getString("middle_name"),
                        rs.getString("last_name"),
                        rs.getString("addr_line_1"),
                        rs.getString("addr_line_2"),
                        rs.getString("addr_line_3"),
                        rs.getString("addr_state_cd"),
                        rs.getString("addr_country_cd"),
                        rs.getString("addr_zip"),
                        rs.getObject("fico_score", Integer.class)));

        try {
            Files.createDirectories(statementsDir);
            int written = 0;
            for (XrefRow x : xrefs) {
                writeStatement(x, cycleDate, runId);
                written++;
            }
            contribution.incrementWriteCount(written);
            log.info("CREASTMT: run {} wrote {} statements (cycle {}) under {}",
                    runId, written, cycleDate, statementsDir);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        return RepeatStatus.FINISHED;
    }

    private void writeStatement(XrefRow x, LocalDate cycleDate, long runId) throws IOException {
        // 4000-TRNXFILE-GET input: TRANSACT sorted by (card, tran-id)
        List<StmtTxn> txns = jdbc.query("""
                        SELECT transaction_id, description, amount
                        FROM transactions
                        WHERE card_number = ?
                        ORDER BY transaction_id
                        """,
                (rs, i) -> new StmtTxn(
                        rs.getString("transaction_id"),
                        rs.getString("description"),
                        rs.getBigDecimal("amount")),
                x.cardNumber());

        Path txt = statementsDir.resolve("stmt-" + runId + "-" + x.accountId() + ".txt");
        Path html = statementsDir.resolve("stmt-" + runId + "-" + x.accountId() + ".html");

        String acctId20 = CobolPic.padX(String.format("%011d", x.accountId()), 20);
        String currBal13 = CobolPic.nineNineV99TrailingSign(x.currBal());
        String fico20 = CobolPic.padX(x.ficoScore() == null
                ? "" : String.format("%03d", x.ficoScore()), 20);

        // ST-LINE1: STRING first/middle/last DELIMITED BY ' ' joined by spaces
        String name = firstToken(x.firstName()) + " " + firstToken(x.middleName())
                + " " + firstToken(x.lastName()) + " ";
        // ST-LINE4: addr-3 / state / country / zip, same DELIMITED BY ' ' quirk
        String addr3 = firstToken(x.addrLine3()) + " " + firstToken(x.stateCd())
                + " " + firstToken(x.countryCd()) + " " + firstToken(x.zip()) + " ";

        BigDecimal total = BigDecimal.ZERO;

        try (Writer st = Files.newBufferedWriter(txt, StandardCharsets.UTF_8);
             Writer ht = Files.newBufferedWriter(html, StandardCharsets.UTF_8)) {
            TxtOut t = new TxtOut(st);
            HtmlOut h = new HtmlOut(ht);

            // ---- 5000-CREATE-STATEMENT ------------------------------------
            t.line(STAR_LINE0);
            // 5100-WRITE-HTML-HEADER
            h.line(H_L01); h.line(H_L02); h.line(H_L03); h.line(H_L04); h.line(H_L05);
            h.line(H_L06); h.line(H_L07); h.line(H_L08);
            h.line(H_TRS); h.line(H_L10);
            h.line("<h3>Statement for Account Number: " + acctId20 + "</h3>");
            h.line(H_TDE); h.line(H_TRE);
            h.line(H_TRS); h.line(H_L15); h.line(H_L16); h.line(H_L17); h.line(H_L18);
            h.line(H_TDE); h.line(H_TRE);
            h.line(H_TRS); h.line(H_L22_35);
            // 5200-WRITE-HTML-NMADBS (name/address <p>s, DELIMITED BY '  ')
            h.line("<p style=\"font-size:16px\">" + upToDoubleSpace(CobolPic.padX(name, 50)) + "  </p>");
            h.line("<p>" + upToDoubleSpace(CobolPic.padX(x.addrLine1(), 50)) + "  </p>");
            h.line("<p>" + upToDoubleSpace(CobolPic.padX(x.addrLine2(), 50)) + "  </p>");
            h.line("<p>" + upToDoubleSpace(CobolPic.padX(addr3, 80)) + "  </p>");
            h.line(H_TDE); h.line(H_TRE);
            h.line(H_TRS); h.line(H_L30_42); h.line(H_L31); h.line(H_TDE); h.line(H_TRE);
            h.line(H_TRS); h.line(H_L22_35);
            // basic-details cells embed the FULL fixed-width fields (delim '*')
            h.line("<p>Account ID         : " + acctId20 + "</p>");
            h.line("<p>Current Balance    : " + currBal13 + "</p>");
            h.line("<p>FICO Score         : " + fico20 + "</p>");
            h.line(H_TDE); h.line(H_TRE);
            h.line(H_TRS); h.line(H_L30_42); h.line(H_L43); h.line(H_TDE); h.line(H_TRE);
            h.line(H_TRS);
            h.line(H_L47); h.line(H_L48); h.line(H_TDE);
            h.line(H_L50); h.line(H_L51); h.line(H_TDE);
            h.line(H_L53); h.line(H_L54); h.line(H_TDE);
            h.line(H_TRE);

            t.line(CobolPic.padX(name, 75) + " ".repeat(5));                 // ST-LINE1
            t.line(CobolPic.padX(x.addrLine1(), 50) + " ".repeat(30));       // ST-LINE2
            t.line(CobolPic.padX(x.addrLine2(), 50) + " ".repeat(30));       // ST-LINE3
            t.line(CobolPic.padX(addr3, 80));                                // ST-LINE4
            t.line(DASH_80);                                                 // ST-LINE5
            t.line(LINE6);
            t.line(DASH_80);                                                 // ST-LINE5
            t.line("Account ID         :" + acctId20 + " ".repeat(40));      // ST-LINE7
            t.line("Current Balance    :" + currBal13 + " ".repeat(7) + " ".repeat(40)); // ST-LINE8
            t.line("FICO Score         :" + fico20 + " ".repeat(40));        // ST-LINE9
            t.line(DASH_80);                                                 // ST-LINE10
            t.line(LINE11);
            t.line(DASH_80);                                                 // ST-LINE12
            t.line(LINE13);
            t.line(DASH_80);                                                 // ST-LINE12

            // ---- 4000-TRNXFILE-GET / 6000-WRITE-TRANS ----------------------
            for (StmtTxn txn : txns) {
                String id16 = CobolPic.padX(txn.transactionId(), 16);
                String desc49 = CobolPic.padX(txn.description(), 49);
                String amt13 = CobolPic.zNineV99TrailingSign(txn.amount());
                t.line(id16 + " " + desc49 + "$" + amt13);                    // ST-LINE14
                h.line(H_TRS);
                h.line(H_L58);
                h.line("<p>" + id16 + "</p>");
                h.line(H_TDE);
                h.line(H_L61);
                h.line("<p>" + desc49 + "</p>");
                h.line(H_TDE);
                h.line(H_L64);
                h.line("<p>" + amt13 + "</p>");
                h.line(H_TDE);
                h.line(H_TRE);
                total = total.add(txn.amount());                              // ADD TRNX-AMT
            }

            // ---- statement trailer (end of 4000) ---------------------------
            t.line(DASH_80);                                                 // ST-LINE12
            t.line("Total EXP:" + " ".repeat(56) + "$"
                    + CobolPic.zNineV99TrailingSign(total));                 // ST-LINE14A
            t.line(STAR_LINE15);                                             // ST-LINE15
            h.line(H_TRS); h.line(H_L10); h.line(H_L75); h.line(H_TDE); h.line(H_TRE);
            h.line(H_L78); h.line(H_L79); h.line(H_L80);
        }

        jdbc.update("""
                        INSERT INTO statements (account_id, cycle_date, txt_path, html_path)
                        VALUES (?, ?, ?, ?)
                        """,
                x.accountId(), cycleDate, txt.toString(), html.toString());
    }

    /** STRING ... DELIMITED BY ' ': the field contributes up to its first space. */
    private static String firstToken(String v) {
        if (v == null) {
            return "";
        }
        int sp = v.indexOf(' ');
        return sp < 0 ? v : v.substring(0, sp);
    }

    /** STRING ... DELIMITED BY '  ' (two spaces): stop at the first double space. */
    private static String upToDoubleSpace(String v) {
        int i = v.indexOf("  ");
        return i < 0 ? v : v.substring(0, i);
    }

    /** FD-STMTFILE-REC PIC X(80). */
    private record TxtOut(Writer out) {
        void line(String s) throws IOException {
            String padded = CobolPic.padX(s, 80);
            if (s.length() > 80) {
                throw new IllegalStateException("statement line > 80 chars: [" + s + "]");
            }
            out.write(padded);
            out.write('\n');
        }
    }

    /** FD-HTMLFILE-REC PIC X(100). */
    private record HtmlOut(Writer out) {
        void line(String s) throws IOException {
            String padded = CobolPic.padX(s, 100);
            if (s.length() > 100) {
                throw new IllegalStateException("HTML line > 100 chars: [" + s + "]");
            }
            out.write(padded);
            out.write('\n');
        }
    }

    private record XrefRow(String cardNumber, long accountId, BigDecimal currBal,
                           String firstName, String middleName, String lastName,
                           String addrLine1, String addrLine2, String addrLine3,
                           String stateCd, String countryCd, String zip,
                           Integer ficoScore) {
    }

    private record StmtTxn(String transactionId, String description, BigDecimal amount) {
    }
}
