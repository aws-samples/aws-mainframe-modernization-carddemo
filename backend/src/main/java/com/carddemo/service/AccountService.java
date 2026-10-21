package com.carddemo.service;

import com.carddemo.model.*;
import com.carddemo.repo.*;
import com.carddemo.util.DateValidator;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COACTVWC.cbl (view) + COACTUPC.cbl (update) logic. */
@Service
public class AccountService {

    private final AccountRepository accounts;
    private final CardXrefRepository xrefs;
    private final CustomerRepository customers;

    public AccountService(AccountRepository accounts, CardXrefRepository xrefs,
                          CustomerRepository customers) {
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.customers = customers;
    }

    public Dto.AccountView view(String idText) {
        long id = parseAccountId(idText);
        Account a = accounts.findById(id)
                .orElseThrow(() -> ApiException.notFound("Account ID NOT found..."));
        Customer c = xrefs.findFirstByAcctId(id)
                .flatMap(x -> customers.findById(x.getCustId())).orElse(null);
        return Dto.AccountView.of(a, c);
    }

    static long parseAccountId(String idText) {
        if (idText == null || idText.isBlank()) {
            throw ApiException.badRequest("Acct ID can NOT be empty...");
        }
        // The CICS field is PIC 9(11): entry right-justifies and zero-fills,
        // so "1" and "00000000001" are the same account.
        String t = idText.trim();
        if (!t.matches("\\d{1,11}") || t.matches("0+")) {
            throw ApiException.badRequest(
                    "Account number must be a non zero 11 digit number");
        }
        return Long.parseLong(t);
    }

    @Transactional
    public Dto.AccountView update(String idText, Dto.AccountUpdateRequest req) {
        long id = parseAccountId(idText);
        Account a = accounts.findById(id)
                .orElseThrow(() -> ApiException.notFound("Account ID NOT found..."));

        // Field-level validations mirroring COACTUPC.cbl EDIT-* paragraphs.
        if (req.activeStatus() != null && !req.activeStatus().equalsIgnoreCase("Y")
                && !req.activeStatus().equalsIgnoreCase("N")) {
            throw ApiException.badRequest("Account Active Status must be Y or N");
        }
        if (req.creditLimit() == null) {
            throw ApiException.badRequest("Credit Limit must be supplied");
        }
        validateDate("Open Date", req.openDate());
        validateDate("Expiry Date", req.expirationDate());
        validateDate("Reissue Date", req.reissueDate());

        a.setActiveStatus(req.activeStatus() != null ? req.activeStatus().toUpperCase()
                : a.getActiveStatus());
        a.setCreditLimit(req.creditLimit());
        if (req.cashCreditLimit() != null) a.setCashCreditLimit(req.cashCreditLimit());
        if (req.openDate() != null) a.setOpenDate(req.openDate());
        if (req.expirationDate() != null) a.setExpirationDate(req.expirationDate());
        if (req.reissueDate() != null) a.setReissueDate(req.reissueDate());
        if (req.currCycCredit() != null) a.setCurrCycCredit(req.currCycCredit());
        if (req.currCycDebit() != null) a.setCurrCycDebit(req.currCycDebit());
        if (req.addrZip() != null) a.setAddrZip(req.addrZip());
        if (req.groupId() != null) a.setGroupId(req.groupId());
        accounts.save(a);

        Customer customer = xrefs.findFirstByAcctId(id)
                .flatMap(x -> customers.findById(x.getCustId())).orElse(null);
        if (req.customer() != null && customer != null) {
            applyCustomerUpdate(customer, req.customer());
            customers.save(customer);
        }
        return Dto.AccountView.of(a, customer);
    }

    private void applyCustomerUpdate(Customer c, Dto.CustomerUpdate u) {
        if (u.firstName() != null) {
            require(u.firstName(), "First Name");
            c.setFirstName(u.firstName());
        }
        if (u.lastName() != null) {
            require(u.lastName(), "Last Name");
            c.setLastName(u.lastName());
        }
        if (u.middleName() != null) c.setMiddleName(u.middleName());
        if (u.addrLine1() != null) c.setAddrLine1(u.addrLine1());
        if (u.addrLine2() != null) c.setAddrLine2(u.addrLine2());
        if (u.addrLine3() != null) c.setAddrLine3(u.addrLine3());
        if (u.addrStateCd() != null) c.setAddrStateCd(u.addrStateCd());
        if (u.addrCountryCd() != null) c.setAddrCountryCd(u.addrCountryCd());
        if (u.addrZip() != null) c.setAddrZip(u.addrZip());
        if (u.phoneNum1() != null) {
            validatePhone("Phone Number 1", u.phoneNum1());
            c.setPhoneNum1(u.phoneNum1());
        }
        if (u.phoneNum2() != null) {
            validatePhone("Phone Number 2", u.phoneNum2());
            c.setPhoneNum2(u.phoneNum2());
        }
        if (u.ssn() != null) c.setSsn(u.ssn());
        if (u.govtIssuedId() != null) c.setGovtIssuedId(u.govtIssuedId());
        if (u.dob() != null) {
            validateDate("Date of Birth", u.dob());
            c.setDob(u.dob());
        }
        if (u.eftAccountId() != null) c.setEftAccountId(u.eftAccountId());
        if (u.priCardHolderInd() != null) c.setPriCardHolderInd(u.priCardHolderInd());
        if (u.ficoCreditScore() != null) c.setFicoCreditScore(u.ficoCreditScore());
    }

    private static void require(String v, String name) {
        if (v.isBlank()) {
            throw ApiException.badRequest(name + " must be supplied.");
        }
    }

    private static void validateDate(String name, String v) {
        if (v != null && !DateValidator.isValid(v)) {
            throw ApiException.badRequest(name + ": Not a valid date...");
        }
    }

    /** Phone fields must carry a 3-digit area code, 3-digit prefix and
     *  4-digit line number (COACTUPC EDIT-PHONE-* paragraphs). */
    private static void validatePhone(String name, String v) {
        if (v.isBlank()) {
            throw ApiException.badRequest(name + ": Area code must be supplied.");
        }
        String digits = v.replaceAll("[^0-9]", "");
        if (digits.length() < 3) {
            throw ApiException.badRequest(name + ": Area code must be A 3 digit number.");
        }
        if (digits.length() < 6) {
            throw ApiException.badRequest(name + ": Prefix code must be A 3 digit number.");
        }
        if (digits.length() != 10) {
            throw ApiException.badRequest(name + ": Line number code must be A 4 digit number.");
        }
    }
}
