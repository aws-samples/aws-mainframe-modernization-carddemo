package com.client.security;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ProblemDetail;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import java.net.URI;
import java.time.Instant;
import java.util.UUID;

/**
 * Enterprise RFC 7807 Problem Details Global Exception Handler.
 * Eliminates internal stack trace leakage while providing structured error responses
 * with unique correlation trace IDs for auditability.
 */
@RestControllerAdvice
public class GlobalExceptionHandler {
    private static final Logger log = LoggerFactory.getLogger(GlobalExceptionHandler.class);

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ProblemDetail handleValidationException(MethodArgumentNotValidException ex) {
        String traceId = UUID.randomUUID().toString();
        log.warn("[Trace {}] Validation failure on payload: {}", traceId, ex.getMessage());
        ProblemDetail problem = ProblemDetail.forStatusAndDetail(HttpStatus.BAD_REQUEST, "Validation failed for request payload");
        problem.setTitle("Bad Request");
        problem.setType(URI.create("https://api.enterprise.bank/errors/validation-failed"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("traceId", traceId);
        problem.setProperty("fieldErrors", ex.getBindingResult().getFieldErrors().stream()
            .map(err -> err.getField() + ": " + err.getDefaultMessage()).toList());
        return problem;
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public ProblemDetail handleIllegalArgument(IllegalArgumentException ex) {
        String traceId = UUID.randomUUID().toString();
        log.warn("[Trace {}] Invalid argument encountered: {}", traceId, ex.getMessage());
        ProblemDetail problem = ProblemDetail.forStatusAndDetail(HttpStatus.BAD_REQUEST, ex.getMessage());
        problem.setTitle("Invalid Argument");
        problem.setType(URI.create("https://api.enterprise.bank/errors/invalid-argument"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("traceId", traceId);
        return problem;
    }

    @ExceptionHandler(Exception.class)
    public ProblemDetail handleGeneralException(Exception ex) {
        String traceId = UUID.randomUUID().toString();
        log.error("[Trace {}] Unhandled system error caught by GlobalExceptionHandler", traceId, ex);
        ProblemDetail problem = ProblemDetail.forStatusAndDetail(HttpStatus.INTERNAL_SERVER_ERROR, "An internal processing error occurred. Reference correlation traceId: " + traceId);
        problem.setTitle("Internal Server Error");
        problem.setType(URI.create("https://api.enterprise.bank/errors/internal-error"));
        problem.setProperty("timestamp", Instant.now());
        problem.setProperty("traceId", traceId);
        return problem;
    }
}
