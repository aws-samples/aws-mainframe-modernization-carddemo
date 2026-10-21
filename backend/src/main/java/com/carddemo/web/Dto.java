package com.carddemo.web;

import com.carddemo.model.*;

import java.math.BigDecimal;
import java.util.List;

/** Request/response payloads for the REST API. */
public final class Dto {

    private Dto() {}

    // ---------- auth ----------
    public record LoginRequest(String userId, String password) {}

    public record LoginResponse(String token, String userId, String userType,
                                String firstName, String lastName) {}

    public record MenuOption(int num, String name, String program, String path) {}

    public record MenuResponse(String role, List<MenuOption> options) {}

    // ---------- pagination ----------
    public record PageResult<T>(List<T> content, int page, int size,
                                long totalElements, int totalPages,
                                boolean hasNext, boolean hasPrevious) {
        public static <T> PageResult<T> of(org.springframework.data.domain.Page<T> p) {
            return new PageResult<>(p.getContent(), p.getNumber(), p.getSize(),
                    p.getTotalElements(), p.getTotalPages(), p.hasNext(), p.hasPrevious());
        }
    }

    // ---------- account ----------
    public record AccountView(Long acctId, String activeStatus, BigDecimal currBal,
                              BigDecimal creditLimit, BigDecimal cashCreditLimit,
                              String openDate, String expirationDate, String reissueDate,
                              BigDecimal currCycCredit, BigDecimal currCycDebit,
                              String addrZip, String groupId, CustomerView customer) {
        public static AccountView of(Account a, Customer c) {
            return new AccountView(a.getId(), a.getActiveStatus(), a.getCurrBal(),
                    a.getCreditLimit(), a.getCashCreditLimit(), a.getOpenDate(),
                    a.getExpirationDate(), a.getReissueDate(), a.getCurrCycCredit(),
                    a.getCurrCycDebit(), a.getAddrZip(), a.getGroupId(),
                    c == null ? null : CustomerView.of(c));
        }
    }

    public record CustomerView(Long custId, String firstName, String middleName,
                               String lastName, String addrLine1, String addrLine2,
                               String addrLine3, String addrStateCd, String addrCountryCd,
                               String addrZip, String phoneNum1, String phoneNum2,
                               Long ssn, String govtIssuedId, String dob,
                               String eftAccountId, String priCardHolderInd,
                               Integer ficoCreditScore) {
        public static CustomerView of(Customer c) {
            return new CustomerView(c.getId(), c.getFirstName(), c.getMiddleName(),
                    c.getLastName(), c.getAddrLine1(), c.getAddrLine2(), c.getAddrLine3(),
                    c.getAddrStateCd(), c.getAddrCountryCd(), c.getAddrZip(),
                    c.getPhoneNum1(), c.getPhoneNum2(), c.getSsn(), c.getGovtIssuedId(),
                    c.getDob(), c.getEftAccountId(), c.getPriCardHolderInd(),
                    c.getFicoCreditScore());
        }
    }

    /** PUT /api/accounts/{id} — editable account fields; null = keep current. */
    public record AccountUpdateRequest(String activeStatus, BigDecimal creditLimit,
                                       BigDecimal cashCreditLimit, String openDate,
                                       String expirationDate, String reissueDate,
                                       BigDecimal currCycCredit, BigDecimal currCycDebit,
                                       String addrZip, String groupId,
                                       CustomerUpdate customer) {}

    public record CustomerUpdate(String firstName, String middleName, String lastName,
                                 String addrLine1, String addrLine2, String addrLine3,
                                 String addrStateCd, String addrCountryCd, String addrZip,
                                 String phoneNum1, String phoneNum2, Long ssn,
                                 String govtIssuedId, String dob, String eftAccountId,
                                 String priCardHolderInd, Integer ficoCreditScore) {}

    // ---------- card ----------
    public record CardView(String cardNum, Long acctId, Integer cvvCd,
                           String embossedName, String expirationDate,
                           String activeStatus) {
        public static CardView of(Card c) {
            return new CardView(c.getCardNum(), c.getAcctId(), c.getCvvCd(),
                    c.getEmbossedName(), c.getExpirationDate(), c.getActiveStatus());
        }
    }

    /** PUT /api/cards/{num} — COCRDUPC editable fields. */
    public record CardUpdateRequest(String embossedName, String activeStatus,
                                    String expirationDate, String expiryMonth,
                                    String expiryYear) {}

    // ---------- transaction ----------
    public record TranView(String tranId, String typeCd, Integer catCd, String source,
                           String description, BigDecimal amt, Long merchantId,
                           String merchantName, String merchantCity, String merchantZip,
                           String cardNum, String origTs, String procTs) {
        public static TranView of(TranFields t, String id) {
            return new TranView(id, t.getTypeCd(), t.getCatCd(), t.getSource(),
                    t.getDescription(), t.getAmt(), t.getMerchantId(), t.getMerchantName(),
                    t.getMerchantCity(), t.getMerchantZip(), t.getCardNum(),
                    t.getOrigTs(), t.getProcTs());
        }
        public static TranView of(TransactionRecord t) { return of(t, t.getTranId()); }
    }

    /** POST /api/transactions — COTRN02C fields. */
    public record TranAddRequest(Long accountId, String cardNum, String typeCd,
                                 Integer catCd, String source, String description,
                                 String amt, String merchantId, String merchantName,
                                 String merchantCity, String merchantZip,
                                 String origDate, String procDate, String confirm) {}

    // ---------- report ----------
    public record ReportResponse(String reportName, String startDate, String endDate,
                                 List<TranView> transactions, BigDecimal pageTotal,
                                 BigDecimal grandTotal) {}

    // ---------- bill pay ----------
    public record BillPayRequest(String confirm) {}

    public record BillPayResponse(String tranId, BigDecimal paymentAmount,
                                  BigDecimal newBalance, String message) {}

    // ---------- users ----------
    public record UserView(String userId, String firstName, String lastName,
                           String password, String userType) {
        public static UserView of(UserSecurity u) {
            return new UserView(u.getUsrId(), u.getFirstName(), u.getLastName(),
                    u.getPassword(), u.getUsrType());
        }
    }

    public record UserRequest(String userId, String firstName, String lastName,
                              String password, String userType) {}

    // ---------- jobs ----------
    public record PostingResult(int processed, int posted, int rejected,
                                List<RejectedTran> rejects) {}

    public record RejectedTran(String tranId, int reason, String description) {}

    public record InterestResult(int accountsProcessed, int transactionsCreated,
                                 BigDecimal totalInterest, List<AcctInterest> perAccount) {}

    public record AcctInterest(Long acctId, BigDecimal interest) {}

    public record StatementResult(Long acctId, String customerName, BigDecimal currentBalance,
                                  String ficoScore, List<StatementLine> lines,
                                  BigDecimal totalAmount, String text) {}

    public record StatementLine(String tranId, String details, BigDecimal amount) {}
}
