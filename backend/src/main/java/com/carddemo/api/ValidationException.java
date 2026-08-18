package com.carddemo.api;

import java.util.List;

/**
 * 400 Bad Request carrying the full field-level edit results, so the UI can
 * render inline errors — the REST replacement for the BMS map's per-field
 * error highlighting + WS-RETURN-MSG line (COACTUPC 1200-EDIT-MAP-INPUTS).
 */
public class ValidationException extends RuntimeException {

    /** One failed edit: {@code field} is the JSON path in the request body. */
    public record FieldError(String field, String message) {
    }

    private final transient List<FieldError> fieldErrors;

    public ValidationException(List<FieldError> fieldErrors) {
        // legacy shows the FIRST error message on the screen (WS-RETURN-MSG-OFF guard)
        super(fieldErrors.isEmpty() ? "validation failed" : fieldErrors.get(0).message());
        this.fieldErrors = List.copyOf(fieldErrors);
    }

    public List<FieldError> getFieldErrors() {
        return fieldErrors;
    }
}
