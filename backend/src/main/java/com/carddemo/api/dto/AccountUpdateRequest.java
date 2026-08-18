package com.carddemo.api.dto;

/**
 * PUT /api/v1/accounts/{id} payload — the COACTUPC screen fields (plan §6:
 * one combined account+customer update, customers have no endpoint of their
 * own). Amounts/dates/numbers arrive as strings so the server can apply the
 * exact legacy edit suite (1200-EDIT-MAP-INPUTS) instead of Jackson coercion.
 *
 * <p>{@code accountVersion}/{@code customerVersion} carry the versions the
 * client fetched; a mismatch on save is the legacy "Record changed by some
 * one else. Please review" (9700-CHECK-CHANGE-IN-REC → HTTP 409, plan R3).</p>
 */
public record AccountUpdateRequest(
        Long accountVersion,
        Long customerVersion,
        // account (CVACT01Y)
        String activeStatus,
        String creditLimit,
        String cashCreditLimit,
        String currBal,
        String openDate,
        String expirationDate,
        String reissueDate,
        String currCycCredit,
        String currCycDebit,
        String groupId,
        // customer (CVCUS01Y)
        String firstName,
        String middleName,
        String lastName,
        String ssn,
        String dob,
        String ficoScore,
        String addrLine1,
        String addrLine2,
        String city,
        String stateCd,
        String zip,
        String countryCd,
        String phone1,
        String phone2,
        String govtIssuedId,
        String eftAccountId,
        String priCardHolder) {
}
