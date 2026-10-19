package com.client.runtime;

import java.util.List;

/** Implemented by every service generated from a COBOL program. */
public interface ProgramService {
    String programId();
    List<String> aliases();
    String sourcePath();
    boolean programTranslated();
    List<String> untranslatedReasons();
    default List<String> problems() { return untranslatedReasons(); }
    List<RuleInfo> rules();
    CobolState newState();
    void executeRule(String ruleId, CobolState st);
    void execute(CobolState st);
}
