package com.client.runtime;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

/** All generated program services, CALL / EXEC CICS LINK between them, and the database. */
@Component
public class ProgramRegistry {

    private final Map<String, ProgramService> byId = new LinkedHashMap<>();
    private final ObjectProvider<JdbcTemplate> jdbc;

    public ProgramRegistry(List<ProgramService> services, ObjectProvider<JdbcTemplate> jdbc) {
        this.jdbc = jdbc;
        for (ProgramService s : services) {
            byId.putIfAbsent(s.programId().toUpperCase(), s);
            for (String a : s.aliases()) byId.putIfAbsent(a.toUpperCase(), s);
        }
    }

    public ProgramService find(String id) {
        return id == null ? null : byId.get(id.trim().toUpperCase());
    }

    public Collection<ProgramService> all() {
        return new java.util.LinkedHashSet<>(byId.values());
    }

    public JdbcTemplate jdbc() {
        return jdbc.getIfAvailable();
    }

    /** CALL 'PGM' USING a b: the callee gets its own storage; parameters are copied in and, BY REFERENCE, back. */
    public void call(String id, CobolState caller, String[] args, boolean[] byReference) {
        ProgramService p = find(id);
        if (p == null) {
            caller.handleExternalCall(id, args);
            return;
        }
        CobolState callee = p.newState();
        List<String> params = callee.usingParameters();
        if (params.size() != args.length) {
            throw new CobolDataException("CALL " + id + " passes " + args.length + " parameter(s); the program expects "
                    + params.size());
        }
        for (int i = 0; i < args.length; i++) copy(caller, args[i], callee, params.get(i));
        runCalled(p, callee);
        for (int i = 0; i < args.length; i++) if (byReference[i]) copy(callee, params.get(i), caller, args[i]);
    }

    /** EXEC CICS LINK PROGRAM(id) [COMMAREA(area)]: the commarea maps to the callee's DFHCOMMAREA. */
    public void link(String id, CobolState caller, String commarea) {
        ProgramService p = require(id);
        CobolState callee = p.newState();
        String target = callee.has("DFHCOMMAREA") || callee.isGroup("DFHCOMMAREA") ? "DFHCOMMAREA"
                : callee.usingParameters().isEmpty() ? null : callee.usingParameters().get(0);
        if (commarea != null) {
            if (target == null) throw new CobolDataException(id + " declares no DFHCOMMAREA to receive the COMMAREA");
            copy(caller, commarea, callee, target);
            callee.setNum("EIBCALEN", BigDecimal.ONE, false);
        }
        runCalled(p, callee);
        if (commarea != null) copy(callee, target, caller, commarea);
    }

    private ProgramService require(String id) {
        ProgramService p = find(id);
        if (p == null) throw new NotTranslatedException("Program " + id + " is not part of this application");
        return p;
    }

    private static void runCalled(ProgramService p, CobolState st) {
        try {
            p.execute(st);
        } catch (ProgramExit e) {
            // GOBACK in the called program returns to the caller
        }
    }

    /** Copies an elementary item or a group (element by element, same layout required). */
    static void copy(CobolState from, String a, CobolState to, String b) {
        List<String> src = from.elementaries(a), dst = to.elementaries(b);
        if (src.size() != dst.size()) {
            throw new CobolDataException("Parameter layouts differ: " + a + " has " + src.size()
                    + " item(s), " + b + " has " + dst.size());
        }
        for (int i = 0; i < src.size(); i++) to.move(dst.get(i), from.val(src.get(i)));
    }
}
