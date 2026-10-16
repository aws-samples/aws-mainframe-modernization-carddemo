package com.client.runtime;

/** A data condition the COBOL program would treat as an error (invalid numeric data, divide by zero). */
public final class CobolDataException extends RuntimeException {
    public CobolDataException(String message) { super(message); }
}
