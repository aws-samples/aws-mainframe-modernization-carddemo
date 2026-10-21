package com.client.runtime;

/** Raised when execution reaches COBOL logic that Agent 06 did not translate. */
public final class NotTranslatedException extends RuntimeException {
    public NotTranslatedException(String message) { super(message); }
}
