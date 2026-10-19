package com.client.runtime;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.http.ResponseEntity;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Validation API for the modernized logic and real-time database inspection.
 *   GET  /api/v1/programs                               programs and translation coverage
 *   GET  /api/v1/rules                                  every Agent 04 rule, translated or not, and its fields
 *   POST /api/v1/programs/{program}/rules/{rule}/execute   run one rule with the given field values
 *   POST /api/v1/programs/{program}/execute             run the whole program
 *   POST /api/v1/jobs/{program}/execute                 batch step entry used by Cloud Workflows
 *   GET  /api/v1/database/status                        live database connectivity & metadata
 *   GET  /api/v1/database/tables                        all migrated tables with columns & row counts
 *   GET  /api/v1/database/tables/{table}/data           live data rows from the selected table
 *   POST /api/v1/database/query                         read-only SELECT queries for real-time validation
 *   GET  /api/v1/screens/{mapset}                       a CICS screen's fields and handling program
 *   POST /api/v1/screens/{mapset}/submit                run the CICS program for a screen
 */
@RestController
@RequestMapping("/api/v1")
public class RuleCatalogController {

    private final ProgramRegistry registry;

    public RuleCatalogController(ProgramRegistry registry) {
        this.registry = registry;
    }

    @GetMapping("/programs")
    public List<Map<String, Object>> programs() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (ProgramService p : registry.all()) {
            Map<String, Object> m = new LinkedHashMap<>();
            long translated = p.rules().stream().filter(RuleInfo::translated).count();
            m.put("program", p.programId());
            m.put("source", p.sourcePath());
            m.put("programTranslated", p.programTranslated());
            m.put("untranslated", p.untranslatedReasons());
            m.put("rules", p.rules().size());
            m.put("rulesTranslated", translated);
            out.add(m);
        }
        return out;
    }

    @GetMapping("/rules")
    public List<Map<String, Object>> rules() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (ProgramService p : registry.all()) {
            for (RuleInfo r : p.rules()) {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("program", p.programId());
                m.put("rule", r.id());
                m.put("type", r.type());
                m.put("statement", r.statement());
                m.put("citation", r.citation());
                m.put("translated", r.translated());
                m.put("reason", r.reason());
                m.put("fields", r.fields());
                m.put("notes", r.notes());
                m.put("execute", "/api/v1/programs/" + p.programId() + "/rules/" + r.id() + "/execute");
                out.add(m);
            }
        }
        return out;
    }

    @PostMapping("/programs/{program}/rules/{rule}/execute")
    public ResponseEntity<Map<String, Object>> executeRule(@PathVariable("program") String program,
                                                           @PathVariable("rule") String rule,
                                                           @RequestBody(required = false) Map<String, Object> input) {
        ProgramService p = registry.find(program);
        if (p == null) return error(404, "Unknown program " + program);
        RuleInfo info = p.rules().stream().filter(r -> r.id().equals(rule)).findFirst().orElse(null);
        if (info == null) return error(404, "Program " + program + " has no rule " + rule);

        Map<String, Object> reqInput = input != null ? input : Map.of();
        if (info.translated()) {
            return run(p, reqInput, st -> p.executeRule(rule, st), info.fields(), rule);
        }
        Map<String, Object> resp = new LinkedHashMap<>();
        resp.put("status", "UNTRANSLATED");
        resp.put("rule", rule);
        resp.put("program", program);
        resp.put("type", info.type());
        resp.put("statement", info.statement());
        resp.put("citation", info.citation());
        resp.put("reason", info.reason() != null ? info.reason() : "Rule requires modernization review");
        return ResponseEntity.status(422).body(resp);
    }

    @PostMapping("/programs/{program}/execute")
    public ResponseEntity<Map<String, Object>> executeProgram(@PathVariable("program") String program,
                                                              @RequestBody(required = false) Map<String, Object> input) {
        ProgramService p = registry.find(program);
        if (p == null) return error(404, "Unknown program " + program);
        Map<String, Object> reqInput = input != null ? input : Map.of();
        if (p.programTranslated()) {
            return run(p, reqInput, p::execute, List.of(), null);
        }
        Map<String, Object> resp = new LinkedHashMap<>();
        resp.put("status", "UNTRANSLATED");
        resp.put("program", program);
        resp.put("reason", "Program contains untranslated paragraphs requiring modernizer review");
        resp.put("problems", p.problems());
        return ResponseEntity.status(422).body(resp);
    }

    @PostMapping("/jobs/{program}/execute")
    public ResponseEntity<Map<String, Object>> executeJobStep(@PathVariable("program") String program,
                                                              @RequestBody(required = false) Map<String, Object> step) {
        ResponseEntity<Map<String, Object>> r = executeProgram(program, null);
        if (step != null) r.getBody().put("step", step);
        return r;
    }

    @GetMapping("/database/status")
    public ResponseEntity<Map<String, Object>> databaseStatus() {
        Map<String, Object> out = new LinkedHashMap<>();
        JdbcTemplate jt = registry.jdbc();
        if (jt == null) {
            out.put("connected", false);
            out.put("error", "No DataSource configured or available in runtime context");
            return ResponseEntity.ok(out);
        }
        try {
            Map<String, Object> meta = jt.queryForMap(
                "SELECT current_database() AS db, current_user AS usr, version() AS ver"
            );
            out.put("connected", true);
            out.put("database", meta.get("db"));
            out.put("user", meta.get("usr"));
            out.put("version", meta.get("ver"));
            Integer tblCount = jt.queryForObject(
                "SELECT count(*) FROM information_schema.tables WHERE table_schema = 'public'", Integer.class
            );
            out.put("tableCount", tblCount != null ? tblCount : 0);
        } catch (Exception e) {
            out.put("connected", false);
            out.put("error", e.getMessage());
        }
        return ResponseEntity.ok(out);
    }

    @GetMapping("/database/tables")
    public ResponseEntity<List<Map<String, Object>>> databaseTables() {
        JdbcTemplate jt = registry.jdbc();
        if (jt == null) return ResponseEntity.status(503).body(List.of());
        try {
            String dbProduct = "";
            try {
                if (jt.getDataSource() != null) {
                    try (var conn = jt.getDataSource().getConnection()) {
                        dbProduct = conn.getMetaData().getDatabaseProductName().toLowerCase();
                    }
                }
            } catch (Exception ignored) {}

            List<String> tableNames = new ArrayList<>();
            if (dbProduct.contains("sqlite")) {
                tableNames = jt.queryForList(
                    "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY name",
                    String.class
                );
            } else {
                tableNames = jt.queryForList(
                    "SELECT CASE WHEN table_schema = 'public' THEN table_name ELSE table_schema || '.' || table_name END " +
                    "FROM information_schema.tables " +
                    "WHERE table_schema NOT IN ('pg_catalog', 'information_schema') AND table_type = 'BASE TABLE' " +
                    "ORDER BY table_schema, table_name",
                    String.class
                );
            }
            List<Map<String, Object>> result = new ArrayList<>();
            for (String tbl : tableNames) {
                Map<String, Object> tInfo = new LinkedHashMap<>();
                tInfo.put("tableName", tbl);
                String quoted = tbl.contains(".")
                    ? "\"" + tbl.replace(".", "\".\"") + "\""
                    : "\"" + tbl + "\"";
                try {
                    Integer count = jt.queryForObject("SELECT count(*) FROM " + quoted, Integer.class);
                    tInfo.put("rowCount", count != null ? count : 0);
                } catch (Exception ignored) {
                    tInfo.put("rowCount", 0);
                }
                List<Map<String, Object>> cols = new ArrayList<>();
                if (dbProduct.contains("sqlite")) {
                    try {
                        List<Map<String, Object>> pragmaCols = jt.queryForList("PRAGMA table_info(" + tbl + ")");
                        for (Map<String, Object> p : pragmaCols) {
                            Map<String, Object> cm = new LinkedHashMap<>();
                            cm.put("column_name", p.get("name"));
                            cm.put("data_type", p.get("type"));
                            cm.put("is_nullable", ((Number) p.getOrDefault("notnull", 0)).intValue() == 0 ? "YES" : "NO");
                            cols.add(cm);
                        }
                    } catch (Exception ignored) {}
                } else {
                    String schema = tbl.contains(".") ? tbl.split("\\.")[0] : "public";
                    String pureTbl = tbl.contains(".") ? tbl.split("\\.")[1] : tbl;
                    cols = jt.queryForList(
                        "SELECT column_name, data_type, is_nullable, character_maximum_length, numeric_precision, numeric_scale " +
                        "FROM information_schema.columns WHERE table_schema = ? AND table_name = ? ORDER BY ordinal_position",
                        schema, pureTbl
                    );
                }
                tInfo.put("columns", cols);
                result.add(tInfo);
            }
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            return ResponseEntity.status(500).body(List.of(Map.of("error", e.getMessage())));
        }
    }

    @GetMapping("/database/tables/{table}/data")
    public ResponseEntity<Map<String, Object>> tableData(@PathVariable("table") String table,
                                                         @RequestParam(value = "limit", defaultValue = "50") int limit,
                                                         @RequestParam(value = "offset", defaultValue = "0") int offset) {
        JdbcTemplate jt = registry.jdbc();
        if (jt == null) return error(503, "Database is not connected");
        if (!table.matches("^[a-zA-Z0-9_.]+$")) {
            return error(400, "Invalid table name: " + table);
        }
        int safeLimit = Math.min(Math.max(limit, 1), 500);
        int safeOffset = Math.max(offset, 0);
        try {
            String quoted = table.contains(".")
                ? "\"" + table.replace(".", "\".\"") + "\""
                : "\"" + table + "\"";
            String sql = "SELECT * FROM " + quoted + " LIMIT " + safeLimit + " OFFSET " + safeOffset;
            List<Map<String, Object>> rows = jt.queryForList(sql);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("table", table);
            out.put("limit", safeLimit);
            out.put("offset", safeOffset);
            out.put("rowCount", rows.size());
            out.put("rows", rows);
            return ResponseEntity.ok(out);
        } catch (Exception e) {
            return error(500, "Failed to query table " + table + ": " + e.getMessage());
        }
    }

    @PostMapping("/database/query")
    public ResponseEntity<Map<String, Object>> executeQuery(@RequestBody(required = false) Map<String, Object> body) {
        JdbcTemplate jt = registry.jdbc();
        if (jt == null) return error(503, "Database is not connected");
        if (body == null || !body.containsKey("sql")) {
            return error(400, "Missing 'sql' in request body");
        }
        String sql = String.valueOf(body.get("sql")).trim();
        String upper = sql.toUpperCase();
        if (!upper.startsWith("SELECT") && !upper.startsWith("WITH")) {
            return error(400, "Only read-only SELECT or WITH queries are permitted in data inspection mode");
        }
        try {
            List<Map<String, Object>> rows = jt.queryForList(sql);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("query", sql);
            out.put("rowCount", rows.size());
            out.put("rows", rows);
            return ResponseEntity.ok(out);
        } catch (Exception e) {
            return error(400, "Query failed: " + e.getMessage());
        }
    }

    @GetMapping("/screens")
    public ResponseEntity<List<Map<String, Object>>> listScreens() {
        List<Map<String, Object>> out = new ArrayList<>();
        for (String mapset : Screen.listAll()) {
            Screen s = Screen.load(mapset);
            if (s != null) {
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("mapset", s.mapset);
                m.put("maps", s.maps);
                m.put("programs", s.programs);
                m.put("submit", "/api/v1/screens/" + s.mapset + "/submit");
                out.add(m);
            }
        }
        return ResponseEntity.ok(out);
    }

    @GetMapping("/screens/{mapset}")
    public ResponseEntity<Map<String, Object>> screen(@PathVariable("mapset") String mapset) {
        Screen s = Screen.load(mapset);
        if (s == null) return error(404, "Unknown screen " + mapset);
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("mapset", s.mapset);
        m.put("maps", s.maps);
        m.put("programs", s.programs);
        m.put("submit", "/api/v1/screens/" + s.mapset + "/submit");
        m.put("request", Map.of("aid", "ENTER | CLEAR | PF1..PF24 | PA1..PA3", "fields", "{FIELD: value}",
                "commarea", "{FIELD: value} returned by the previous response; omit on the first request"));
        return ResponseEntity.ok(m);
    }

    @PostMapping("/screens/{mapset}/submit")
    @SuppressWarnings("unchecked")
    public ResponseEntity<Map<String, Object>> submit(@PathVariable("mapset") String mapset,
                                                      @RequestBody(required = false) Map<String, Object> body) {
        Screen s = Screen.load(mapset);
        if (s == null) return error(404, "Unknown screen " + mapset);
        Map<String, Object> req = body == null ? Map.of() : body;
        String aid = String.valueOf(req.getOrDefault("aid", "ENTER")).toUpperCase();
        Map<String, Object> fields = req.get("fields") instanceof Map ? (Map<String, Object>) req.get("fields") : new LinkedHashMap<>();
        Map<String, Object> commarea = req.get("commarea") instanceof Map ? (Map<String, Object>) req.get("commarea") : new LinkedHashMap<>();

        if (s.programs.isEmpty()) {
            return error(422, "Screen " + mapset + " is not bound to an online program");
        }
        String pgmId = s.programs.get(0);
        ProgramService p = registry.find(pgmId);
        if (p == null) {
            return error(404, "Bound program " + pgmId + " for screen " + mapset + " is not registered");
        }
        if (!p.programTranslated()) {
            Map<String, Object> err = new LinkedHashMap<>();
            err.put("status", "UNTRANSLATED");
            err.put("screen", mapset);
            err.put("program", p.programId());
            err.put("reason", "Bound program contains untranslated paragraphs requiring modernizer review");
            err.put("problems", p.problems());
            return ResponseEntity.status(422).body(err);
        }

        try {
            CobolState st = p.newState();
            for (Map.Entry<String, Object> e : fields.entrySet()) {
                String in = e.getKey().toUpperCase() + "I", len = e.getKey().toUpperCase() + "L";
                if (st.has(in)) st.setInput(in, e.getValue());
                if (st.has(len)) st.setInput(len, BigDecimal.valueOf(String.valueOf(e.getValue()).length()));
                if (st.has(e.getKey().toUpperCase())) st.setInput(e.getKey().toUpperCase(), e.getValue());
            }
            for (Map.Entry<String, Object> e : commarea.entrySet()) {
                if (st.has(e.getKey())) st.setInput(e.getKey(), e.getValue());
            }
            st.move("EIBAID", aid);
            if (!commarea.isEmpty()) st.setNum("EIBCALEN", BigDecimal.ONE, false);
            ResponseEntity<Map<String, Object>> r = execute(p, st, p::execute, List.of(), null);
            if (r.getStatusCode().isError()) {
                return r;
            }
            Map<String, Object> out = r.getBody();
            if (out == null) out = new LinkedHashMap<>();
            out.put("status", "SUCCESS");
            out.put("screen", st.screenName() != null ? st.screenName() : mapset);
            out.put("screenFields", st.screenOutput() != null && !st.screenOutput().isEmpty() ? st.screenOutput() : fields);
            if (st.returnCommarea() != null) {
                out.put("commarea", st.snapshot(st.elementaries(st.returnCommarea())));
            } else {
                out.put("commarea", commarea);
            }
            out.put("message", "Screen transaction " + mapset + " executed via " + p.programId());
            return ResponseEntity.ok(out);
        } catch (CobolDataException | NotTranslatedException ex) {
            Map<String, Object> err = new LinkedHashMap<>();
            err.put("status", "ERROR");
            err.put("screen", mapset);
            err.put("program", p.programId());
            err.put("error", ex.getMessage());
            return ResponseEntity.status(422).body(err);
        } catch (Exception ex) {
            Map<String, Object> err = new LinkedHashMap<>();
            err.put("status", "FAILED");
            err.put("screen", mapset);
            err.put("program", p.programId());
            err.put("error", ex.getMessage() != null ? ex.getMessage() : ex.getClass().getSimpleName());
            return ResponseEntity.status(500).body(err);
        }
    }

    private interface Step { void run(CobolState st); }

    private ResponseEntity<Map<String, Object>> run(ProgramService p, Map<String, Object> input, Step step,
                                                    List<String> ruleFields, String rule) {
        CobolState st = p.newState();
        Map<String, Object> in = input == null ? Map.of() : input;
        List<String> unknown = st.unknown(in.keySet());
        if (!unknown.isEmpty()) return error(400, "Not declared in " + p.programId() + ": " + unknown);
        try {
            for (Map.Entry<String, Object> e : in.entrySet()) st.setInput(e.getKey(), e.getValue());
        } catch (CobolDataException e) {
            return error(400, e.getMessage());
        }
        ResponseEntity<Map<String, Object>> r = execute(p, st, step, ruleFields, rule);
        r.getBody().put("inputs", in);
        return r;
    }

    private ResponseEntity<Map<String, Object>> execute(ProgramService p, CobolState st, Step step,
                                                        List<String> ruleFields, String rule) {
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("program", p.programId());
        if (rule != null) out.put("rule", rule);
        int code = 200;
        try {
            step.run(st);
            out.put("status", "EXECUTED");
        } catch (ProgramExit e) {
            out.put("status", "EXECUTED");
        } catch (NotTranslatedException e) {
            out.put("status", "NOT_TRANSLATED");
            out.put("error", e.getMessage());
            code = 422;
        } catch (CobolDataException | ArithmeticException e) {
            out.put("status", "DATA_ERROR");
            out.put("error", e.getMessage());
            code = 422;
        }
        out.put("changed", st.changed());
        if (!ruleFields.isEmpty()) out.put("fields", st.snapshot(ruleFields));
        out.put("display", st.displayLines());
        if (!st.sqlErrors().isEmpty()) out.put("sqlErrors", st.sqlErrors());
        return ResponseEntity.status(code).body(out);
    }

    private static ResponseEntity<Map<String, Object>> error(int code, String message) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("status", "ERROR");
        m.put("error", message);
        return ResponseEntity.status(code).body(m);
    }

    /** Screen definition generated from the BMS mapset (resource cobol/screens/MAPSET.screen). */
    static final class Screen {
        String mapset;
        final Map<String, List<Map<String, Object>>> maps = new LinkedHashMap<>();
        final List<String> programs = new ArrayList<>();

        static List<String> listAll() {
            List<String> list = new ArrayList<>();
            try (InputStream in = Screen.class.getResourceAsStream("/cobol/screens/screens.idx")) {
                if (in != null) {
                    BufferedReader r = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8));
                    for (String line; (line = r.readLine()) != null; ) {
                        line = line.trim();
                        if (!line.isEmpty() && !list.contains(line)) list.add(line);
                    }
                }
            } catch (Exception ignored) {}
            return list;
        }

        static Screen load(String mapset) {
            String name = mapset.toUpperCase();
            if (!name.matches("[A-Z0-9#@$_-]+")) return null;
            try (InputStream in = Screen.class.getResourceAsStream("/cobol/screens/" + name + ".screen")) {
                if (in == null) return null;
                Screen s = new Screen();
                s.mapset = name;
                BufferedReader r = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8));
                for (String line; (line = r.readLine()) != null; ) {
                    String[] p = line.split("\t", -1);
                    if (p[0].equals("P") && p.length > 1) s.programs.add(p[1]);
                    if (p[0].equals("F") && p.length > 3) {
                        Map<String, Object> f = new LinkedHashMap<>();
                        f.put("field", p[2]);
                        f.put("length", Integer.parseInt(p[3]));
                        s.maps.computeIfAbsent(p[1], k -> new ArrayList<>()).add(f);
                    }
                }
                return s;
            } catch (java.io.IOException e) {
                return null;
            }
        }
    }
}
