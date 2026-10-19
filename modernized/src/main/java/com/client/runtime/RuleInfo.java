package com.client.runtime;

import java.util.List;

/** One business rule from Agent 04 and whether its cited COBOL was translated. */
public record RuleInfo(String id, String type, String statement, String citation, boolean translated,
                       String reason, List<String> fields, String notes) {
}
