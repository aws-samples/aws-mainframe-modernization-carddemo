package com.carddemo.api.dto;

/**
 * PUT /api/v1/cards/{cardNumber} payload — the COCRDUPC editable fields:
 * embossed name, expiry month/year, active status (plan §4; CVV deliberately
 * dropped per R5 — legacy rewrites it but never edits it). {@code version}
 * is the fetched @Version for the 9300-CHECK-CHANGE-IN-REC 409 (plan R3).
 */
public record CardUpdateRequest(
        Long version,
        String embossedName,
        String expiryMonth,
        String expiryYear,
        String activeStatus) {
}
