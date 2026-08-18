package com.carddemo.api;

/**
 * 409 Conflict: optimistic-lock loss ("Record changed by some one else.
 * Please review" — COACTUPC 9700 / COCRDUPC 9300) or duplicate user id
 * ("User ID already exist..." — COUSR01C DUPREC).
 */
public class ConflictException extends RuntimeException {

    public ConflictException(String message) {
        super(message);
    }
}
