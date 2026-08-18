package com.carddemo.api.dto;

/**
 * POST /api/v1/transactions — COTRN02C add-transaction input (plan §6).
 * All fields are raw strings; the server re-runs the legacy edit suite
 * (VALIDATE-INPUT-KEY-FIELDS / VALIDATE-INPUT-DATA-FIELDS) on them.
 *
 * <p>Key fields: exactly like the legacy screen, either {@code accountId} or
 * {@code cardNumber} may be entered; when both are present the account wins
 * and the card is resolved from the xref (COTRN02C EVALUATE order, 195-230).
 * The legacy Y/N confirm step is a UI concern — this POST is the confirmed
 * add (plan §3 row 11).</p>
 *
 * <p>{@code amount} must be in the exact legacy format {@code -99999999.99}
 * (sign + 8 digits + '.' + 2 digits, COTRN02C 339-351); {@code origDate} and
 * {@code procDate} are YYYY-MM-DD (353-427).</p>
 */
public record AddTransactionRequest(
        String accountId,
        String cardNumber,
        String typeCd,
        String categoryCd,
        String source,
        String description,
        String amount,
        String origDate,
        String procDate,
        String merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip) {
}
