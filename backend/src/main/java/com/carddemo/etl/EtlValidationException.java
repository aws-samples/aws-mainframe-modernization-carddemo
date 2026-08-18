package com.carddemo.etl;

/** Aborts the ETL loudly on a data-integrity violation (plan §2.3). */
public class EtlValidationException extends RuntimeException {

    public EtlValidationException(String message) {
        super(message);
    }
}
