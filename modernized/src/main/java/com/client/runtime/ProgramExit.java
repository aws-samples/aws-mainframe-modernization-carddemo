package com.client.runtime;

/** GOBACK / STOP RUN / EXEC CICS RETURN: ends the current program. */
public final class ProgramExit extends RuntimeException {
    public ProgramExit() { super(null, null, false, false); }
}
