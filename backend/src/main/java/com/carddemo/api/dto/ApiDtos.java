package com.carddemo.api.dto;

import com.carddemo.domain.Account;
import com.carddemo.domain.Card;
import com.carddemo.domain.Customer;
import com.carddemo.domain.Transaction;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/** Read-only response shapes for Phase 1 (plan §6). */
public final class ApiDtos {

    private ApiDtos() {
    }

    /** Card row for list views — number MASKED (plan §5). */
    public record CardSummary(
            String cardNumber,
            String maskedCardNumber,
            Long accountId,
            String embossedName,
            LocalDate expirationDate,
            String activeStatus) {

        public static CardSummary masked(Card c) {
            // full number deliberately null in list responses
            return new CardSummary(null, c.maskedNumber(), c.getAccountId(),
                    trim(c.getEmbossedName()), c.getExpirationDate(), trim(c.getActiveStatus()));
        }
    }

    /**
     * Card detail — full number allowed (COCRDSLC view, plan §6).
     * {@code version} is the optimistic-lock token the edit form echoes back
     * on PUT (COCRDUPC 9300-CHECK-CHANGE-IN-REC, plan R3).
     */
    public record CardDetail(
            String cardNumber,
            String maskedCardNumber,
            Long accountId,
            String embossedName,
            LocalDate expirationDate,
            String activeStatus,
            long version) {

        public static CardDetail of(Card c) {
            return new CardDetail(c.getCardNumber() == null ? null : c.getCardNumber().trim(),
                    c.maskedNumber(), c.getAccountId(), trim(c.getEmbossedName()),
                    c.getExpirationDate(), trim(c.getActiveStatus()), c.getVersion());
        }
    }

    /** Combined account + customer view — COACTVWC (plan §3 row 4). */
    public record AccountView(
            Long accountId,
            String activeStatus,
            BigDecimal currBal,
            BigDecimal creditLimit,
            BigDecimal cashCreditLimit,
            LocalDate openDate,
            LocalDate expirationDate,
            LocalDate reissueDate,
            BigDecimal currCycCredit,
            BigDecimal currCycDebit,
            String zip,
            String groupId,
            long version,
            CustomerView customer,
            List<CardSummary> cards) {

        public static AccountView of(Account a, Customer c, List<Card> cards) {
            return new AccountView(
                    a.getAccountId(), trim(a.getActiveStatus()), a.getCurrBal(),
                    a.getCreditLimit(), a.getCashCreditLimit(), a.getOpenDate(),
                    a.getExpirationDate(), a.getReissueDate(), a.getCurrCycCredit(),
                    a.getCurrCycDebit(), trim(a.getZip()), trim(a.getGroupId()),
                    a.getVersion(),
                    c == null ? null : CustomerView.of(c),
                    cards.stream().map(CardSummary::masked).toList());
        }
    }

    /** Customer block of the account view — SSN last-4 only (plan §5). */
    public record CustomerView(
            Long customerId,
            String firstName,
            String middleName,
            String lastName,
            String addrLine1,
            String addrLine2,
            String addrLine3,
            String addrStateCd,
            String addrCountryCd,
            String addrZip,
            String phoneNum1,
            String phoneNum2,
            String ssnLast4,
            String govtIssuedId,
            LocalDate dob,
            String eftAccountId,
            String priCardHolder,
            Integer ficoScore,
            long version) {

        public static CustomerView of(Customer c) {
            return new CustomerView(
                    c.getCustomerId(), trim(c.getFirstName()), trim(c.getMiddleName()),
                    trim(c.getLastName()), trim(c.getAddrLine1()), trim(c.getAddrLine2()),
                    trim(c.getAddrLine3()), trim(c.getAddrStateCd()), trim(c.getAddrCountryCd()),
                    trim(c.getAddrZip()), trim(c.getPhoneNum1()), trim(c.getPhoneNum2()),
                    c.ssnLast4(), trim(c.getGovtIssuedId()), c.getDob(),
                    trim(c.getEftAccountId()), trim(c.getPriCardHolder()), c.getFicoScore(),
                    c.getVersion());
        }
    }

    /** Transaction row/detail — card number masked everywhere but card detail. */
    public record TransactionView(
            String transactionId,
            String typeCd,
            Integer categoryCd,
            String source,
            String description,
            BigDecimal amount,
            Long merchantId,
            String merchantName,
            String merchantCity,
            String merchantZip,
            String maskedCardNumber,
            Long accountId,
            LocalDateTime origTs,
            LocalDateTime procTs) {

        public static TransactionView of(Transaction t, Long accountId) {
            return new TransactionView(
                    trim(t.getTransactionId()), trim(t.getTypeCd()), t.getCategoryCd(),
                    trim(t.getSource()), trim(t.getDescription()), t.getAmount(),
                    t.getMerchantId(), trim(t.getMerchantName()), trim(t.getMerchantCity()),
                    trim(t.getMerchantZip()), t.maskedCardNumber(), accountId,
                    t.getOrigTs(), t.getProcTs());
        }
    }

    /** Keyset page envelope: {@code nextCursor} null on the last page. */
    public record Page<T>(List<T> items, String nextCursor) {
    }

    private static String trim(String s) {
        return s == null ? null : s.trim();
    }
}
