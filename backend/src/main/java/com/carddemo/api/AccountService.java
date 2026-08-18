package com.carddemo.api;

import com.carddemo.api.dto.AccountUpdateRequest;
import com.carddemo.api.dto.ApiDtos.AccountView;
import com.carddemo.domain.Account;
import com.carddemo.domain.Card;
import com.carddemo.domain.Customer;
import com.carddemo.repo.AccountRepository;
import com.carddemo.repo.CardRepository;
import com.carddemo.repo.CustomerRepository;
import com.carddemo.validation.AccountUpdateValidator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.Clock;
import java.util.List;
import java.util.Objects;

/**
 * COACTVWC port (plan §4): resolve account → customer (via the dissolved
 * CXACAIX xref, now accounts.customer_id) → cards, into one combined view.
 *
 * <p>Phase 2 adds the COACTUPC port ({@link #updateAccount}): full edit suite,
 * change-detection no-op, optimistic "changed by someone else" 409, and a
 * single transaction rewriting BOTH the account and the customer (legacy
 * 9600-WRITE-PROCESSING rewrites ACCTDAT + CUSTDAT under one syncpoint).</p>
 */
@Service
public class AccountService {

    /** COACTUPC 9700-CHECK-CHANGE-IN-REC / DATA-WAS-CHANGED-BEFORE-UPDATE. */
    static final String CHANGED_BY_SOMEONE_ELSE = "Record changed by some one else. Please review";

    private final AccountRepository accounts;
    private final CustomerRepository customers;
    private final CardRepository cards;
    private final AccountUpdateValidator validator;

    public AccountService(AccountRepository accounts, CustomerRepository customers, CardRepository cards,
                          Clock clock) {
        this.accounts = accounts;
        this.customers = customers;
        this.cards = cards;
        this.validator = new AccountUpdateValidator(clock);
    }

    @Transactional(readOnly = true)
    public AccountView getAccount(long accountId) {
        Account account = accounts.findById(accountId)
                .orElseThrow(() -> new NotFoundException("Account " + accountId + " not found"));
        Customer customer = account.getCustomerId() == null
                ? null
                : customers.findById(account.getCustomerId()).orElse(null);
        List<Card> accountCards = cards.findByAccountIdOrderByCardNumberAsc(accountId);
        return AccountView.of(account, customer, accountCards);
    }

    /**
     * COACTUPC port: (1) 1200-EDIT-MAP-INPUTS edit suite; (2) 1205-COMPARE-
     * OLD-NEW change detection — a no-op update returns the current view
     * without writing; (3) 9700-CHECK-CHANGE-IN-REC — the client's fetched
     * versions must match the stored @Version values or the save is rejected
     * with the legacy "changed by some one else" message (409, plan R3);
     * (4) 9600-WRITE-PROCESSING — account + customer rewritten in ONE
     * transaction (legacy does SYNCPOINT ROLLBACK if the customer write fails).
     */
    @Transactional
    public AccountView updateAccount(long accountId, AccountUpdateRequest req) {
        AccountUpdateValidator.Parsed in = validator.validate(req);

        Account account = accounts.findById(accountId)
                .orElseThrow(() -> new NotFoundException("Account " + accountId + " not found"));
        Customer customer = account.getCustomerId() == null
                ? null
                : customers.findById(account.getCustomerId()).orElse(null);
        if (customer == null) {
            throw new NotFoundException("Customer for account " + accountId + " not found");
        }

        // 9700-CHECK-CHANGE-IN-REC: reject if the record moved since the fetch
        if ((req.accountVersion() != null && req.accountVersion() != account.getVersion())
                || (req.customerVersion() != null && req.customerVersion() != customer.getVersion())) {
            throw new ConflictException(CHANGED_BY_SOMEONE_ELSE);
        }

        // 1205-COMPARE-OLD-NEW: success-without-write when nothing changed
        if (noChanges(account, customer, in)) {
            return AccountView.of(account, customer,
                    cards.findByAccountIdOrderByCardNumberAsc(accountId));
        }

        // 9600-WRITE-PROCESSING (both entities, one transaction)
        account.setActiveStatus(in.activeStatus());
        account.setCreditLimit(in.creditLimit());
        account.setCashCreditLimit(in.cashCreditLimit());
        account.setCurrBal(in.currBal());
        account.setOpenDate(in.openDate());
        account.setExpirationDate(in.expirationDate());
        account.setReissueDate(in.reissueDate());
        account.setCurrCycCredit(in.currCycCredit());
        account.setCurrCycDebit(in.currCycDebit());
        account.setGroupId(in.groupId());

        customer.setFirstName(in.firstName());
        customer.setMiddleName(in.middleName());
        customer.setLastName(in.lastName());
        if (in.ssn() != null) {
            customer.setSsn(in.ssn());
        }
        customer.setDob(in.dob());
        customer.setFicoScore(in.ficoScore());
        customer.setAddrLine1(in.addrLine1());
        customer.setAddrLine2(in.addrLine2());
        customer.setAddrLine3(in.city());
        customer.setAddrStateCd(in.stateCd());
        customer.setAddrZip(in.zip());
        customer.setAddrCountryCd(in.countryCd());
        customer.setPhoneNum1(in.phone1());
        customer.setPhoneNum2(in.phone2());
        customer.setGovtIssuedId(in.govtIssuedId());
        customer.setEftAccountId(in.eftAccountId());
        customer.setPriCardHolder(in.priCardHolder());

        accounts.saveAndFlush(account);
        customers.saveAndFlush(customer);
        return AccountView.of(account, customer,
                cards.findByAccountIdOrderByCardNumberAsc(accountId));
    }

    /** 1205-COMPARE-OLD-NEW: field-by-field compare of new vs stored values. */
    private static boolean noChanges(Account a, Customer c, AccountUpdateValidator.Parsed in) {
        return eq(trim(a.getActiveStatus()), in.activeStatus())
                && eqAmt(a.getCreditLimit(), in.creditLimit())
                && eqAmt(a.getCashCreditLimit(), in.cashCreditLimit())
                && eqAmt(a.getCurrBal(), in.currBal())
                && Objects.equals(a.getOpenDate(), in.openDate())
                && Objects.equals(a.getExpirationDate(), in.expirationDate())
                && Objects.equals(a.getReissueDate(), in.reissueDate())
                && eqAmt(a.getCurrCycCredit(), in.currCycCredit())
                && eqAmt(a.getCurrCycDebit(), in.currCycDebit())
                && eq(trim(a.getGroupId()), in.groupId())
                && eq(trim(c.getFirstName()), in.firstName())
                && eq(trim(c.getMiddleName()), in.middleName())
                && eq(trim(c.getLastName()), in.lastName())
                && (in.ssn() == null || eq(trim(c.getSsn()), in.ssn()))
                && Objects.equals(c.getDob(), in.dob())
                && Objects.equals(c.getFicoScore(), in.ficoScore())
                && eq(trim(c.getAddrLine1()), in.addrLine1())
                && eq(trim(c.getAddrLine2()), in.addrLine2())
                && eq(trim(c.getAddrLine3()), in.city())
                && eq(trim(c.getAddrStateCd()), in.stateCd())
                && eq(trim(c.getAddrZip()), in.zip())
                && eq(trim(c.getAddrCountryCd()), in.countryCd())
                && eq(trim(c.getPhoneNum1()), in.phone1())
                && eq(trim(c.getPhoneNum2()), in.phone2())
                && eq(trim(c.getGovtIssuedId()), in.govtIssuedId())
                && eq(trim(c.getEftAccountId()), in.eftAccountId())
                && eq(trim(c.getPriCardHolder()), in.priCardHolder());
    }

    /** Legacy compares upper-cased trimmed text (1205-COMPARE-OLD-NEW). */
    private static boolean eq(String stored, String incoming) {
        String s = stored == null ? "" : stored.toUpperCase();
        String i = incoming == null ? "" : incoming.toUpperCase();
        return s.equals(i);
    }

    private static boolean eqAmt(BigDecimal stored, BigDecimal incoming) {
        if (stored == null || incoming == null) {
            return stored == null && incoming == null;
        }
        return stored.compareTo(incoming) == 0;
    }

    private static String trim(String s) {
        return s == null ? null : s.trim();
    }
}
