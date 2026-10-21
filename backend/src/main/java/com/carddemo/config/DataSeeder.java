package com.carddemo.config;

import com.carddemo.model.*;
import com.carddemo.repo.*;
import com.carddemo.util.CobolFiles;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * REQ-DATA-02: loads every table from the fixed-width ASCII files that mirror
 * the mainframe datasets (app/data/ASCII + usrsec.txt from DUSRSECJ.jcl).
 * Runs once, only when the account table is empty.
 */
@Component
public class DataSeeder implements ApplicationRunner {

    private static final Logger log = LoggerFactory.getLogger(DataSeeder.class);

    private final ResourceLoader resources;
    private final String seedDir;
    private final AccountRepository accounts;
    private final CardRepository cards;
    private final CardXrefRepository xrefs;
    private final CustomerRepository customers;
    private final DailyTransactionRepository dailyTrans;
    private final DisclosureGroupRepository disclGroups;
    private final TranCatBalRepository tcatBals;
    private final TransactionCategoryRepository tranCats;
    private final TransactionTypeRepository tranTypes;
    private final UserSecurityRepository users;
    private final TransactionRepository transactions;

    public DataSeeder(ResourceLoader resources,
                      @Value("${carddemo.seed-dir}") String seedDir,
                      AccountRepository accounts,
                      CardRepository cards,
                      CardXrefRepository xrefs,
                      CustomerRepository customers,
                      DailyTransactionRepository dailyTrans,
                      DisclosureGroupRepository disclGroups,
                      TranCatBalRepository tcatBals,
                      TransactionCategoryRepository tranCats,
                      TransactionTypeRepository tranTypes,
                      UserSecurityRepository users,
                      TransactionRepository transactions) {
        this.resources = resources;
        this.seedDir = seedDir;
        this.accounts = accounts;
        this.cards = cards;
        this.xrefs = xrefs;
        this.customers = customers;
        this.dailyTrans = dailyTrans;
        this.disclGroups = disclGroups;
        this.tcatBals = tcatBals;
        this.tranCats = tranCats;
        this.tranTypes = tranTypes;
        this.users = users;
        this.transactions = transactions;
    }

    @Override
    @Transactional
    public void run(ApplicationArguments args) throws IOException {
        if (accounts.count() > 0) {
            log.info("Seed data already present ({} accounts) — skipping", accounts.count());
            return;
        }
        seedAccounts();
        seedCards();
        seedXrefs();
        seedCustomers();
        seedDailyTrans();
        seedDisclosureGroups();
        seedTranCatBals();
        seedTranCats();
        seedTranTypes();
        seedUsers();
        log.info("Seed complete: {} accounts, {} cards, {} xrefs, {} customers, "
                        + "{} daily transactions, {} disclosure groups, {} cat balances, "
                        + "{} tran categories, {} tran types, {} users",
                accounts.count(), cards.count(), xrefs.count(), customers.count(),
                dailyTrans.count(), disclGroups.count(), tcatBals.count(),
                tranCats.count(), tranTypes.count(), users.count());
    }

    private List<String> lines(String file) throws IOException {
        // Prefer an on-disk directory (config points at app/data/ASCII when
        // running inside the repo); fall back to the bundled classpath copies.
        Path p = Path.of(seedDir, file);
        if (Files.isRegularFile(p)) {
            return Files.readAllLines(p, StandardCharsets.UTF_8);
        }
        Resource r = resources.getResource("classpath:/seed/" + file);
        if (!r.exists()) {
            throw new IOException("Seed file not found: " + file);
        }
        try (BufferedReader br = new BufferedReader(
                new InputStreamReader(r.getInputStream(), StandardCharsets.UTF_8))) {
            List<String> out = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.isBlank()) {
                    out.add(line);
                }
            }
            return out;
        }
    }

    /** CVACT01Y: id(11) status(1) bal(12) climit(12) cashlim(12) open(10)
     *  exp(10) reissue(10) cyccred(12) cycdeb(12) zip(10) group(10). */
    private void seedAccounts() throws IOException {
        for (String l : lines("acctdata.txt")) {
            Account a = new Account();
            a.setId(CobolFiles.num(l, 1, 11));
            a.setActiveStatus(CobolFiles.str(l, 12, 1));
            a.setCurrBal(CobolFiles.zoned(l, 13, 12, 2));
            a.setCreditLimit(CobolFiles.zoned(l, 25, 12, 2));
            a.setCashCreditLimit(CobolFiles.zoned(l, 37, 12, 2));
            a.setOpenDate(CobolFiles.str(l, 49, 10));
            a.setExpirationDate(CobolFiles.str(l, 59, 10));
            a.setReissueDate(CobolFiles.str(l, 69, 10));
            a.setCurrCycCredit(CobolFiles.zoned(l, 79, 12, 2));
            a.setCurrCycDebit(CobolFiles.zoned(l, 91, 12, 2));
            String zip = CobolFiles.str(l, 103, 10);
            String group = CobolFiles.str(l, 113, 10);
            // The ASCII dataset stores the group id in the 103-112 slot and
            // leaves 113-122 blank; honor either position.
            if (group.isEmpty() && zip.startsWith("A")) {
                a.setGroupId(zip);
                a.setAddrZip("");
            } else {
                a.setAddrZip(zip);
                a.setGroupId(group);
            }
            accounts.save(a);
        }
    }

    /** CVACT02Y: num(16) acct(11) cvv(3) name(50) exp(10) status(1). */
    private void seedCards() throws IOException {
        for (String l : lines("carddata.txt")) {
            Card c = new Card();
            c.setCardNum(CobolFiles.str(l, 1, 16));
            c.setAcctId(CobolFiles.num(l, 17, 11));
            c.setCvvCd((int) CobolFiles.num(l, 28, 3));
            c.setEmbossedName(CobolFiles.str(l, 31, 50));
            c.setExpirationDate(CobolFiles.str(l, 81, 10));
            c.setActiveStatus(CobolFiles.str(l, 91, 1));
            cards.save(c);
        }
    }

    /** CVACT03Y: cardnum(16) cust(9) acct(11). */
    private void seedXrefs() throws IOException {
        for (String l : lines("cardxref.txt")) {
            CardXref x = new CardXref();
            x.setCardNum(CobolFiles.str(l, 1, 16));
            x.setCustId(CobolFiles.num(l, 17, 9));
            x.setAcctId(CobolFiles.num(l, 26, 11));
            xrefs.save(x);
        }
    }

    /** CVCUS01Y layout (RECLN 500). */
    private void seedCustomers() throws IOException {
        for (String l : lines("custdata.txt")) {
            Customer c = new Customer();
            c.setId(CobolFiles.num(l, 1, 9));
            c.setFirstName(CobolFiles.str(l, 10, 25));
            c.setMiddleName(CobolFiles.str(l, 35, 25));
            c.setLastName(CobolFiles.str(l, 60, 25));
            c.setAddrLine1(CobolFiles.str(l, 85, 50));
            c.setAddrLine2(CobolFiles.str(l, 135, 50));
            c.setAddrLine3(CobolFiles.str(l, 185, 50));
            c.setAddrStateCd(CobolFiles.str(l, 235, 2));
            c.setAddrCountryCd(CobolFiles.str(l, 237, 3));
            c.setAddrZip(CobolFiles.str(l, 240, 10));
            c.setPhoneNum1(CobolFiles.str(l, 250, 15));
            c.setPhoneNum2(CobolFiles.str(l, 265, 15));
            c.setSsn(CobolFiles.num(l, 280, 9));
            c.setGovtIssuedId(CobolFiles.str(l, 289, 20));
            c.setDob(CobolFiles.str(l, 309, 10));
            c.setEftAccountId(CobolFiles.str(l, 319, 10));
            c.setPriCardHolderInd(CobolFiles.str(l, 329, 1));
            c.setFicoCreditScore((int) CobolFiles.num(l, 330, 3));
            customers.save(c);
        }
    }

    /** CVTRA06Y layout (RECLN 350). */
    private void seedDailyTrans() throws IOException {
        for (String l : lines("dailytran.txt")) {
            DailyTransaction t = new DailyTransaction();
            t.setTranId(CobolFiles.str(l, 1, 16));
            fillTran(l, t);
            dailyTrans.save(t);
        }
    }

    private void fillTran(String l, TranFields t) {
        t.setTypeCd(CobolFiles.str(l, 17, 2));
        t.setCatCd((int) CobolFiles.num(l, 19, 4));
        t.setSource(CobolFiles.str(l, 23, 10));
        t.setDescription(CobolFiles.str(l, 33, 100));
        t.setAmt(CobolFiles.zoned(l, 133, 11, 2));
        t.setMerchantId(CobolFiles.num(l, 144, 9));
        t.setMerchantName(CobolFiles.str(l, 153, 50));
        t.setMerchantCity(CobolFiles.str(l, 203, 50));
        t.setMerchantZip(CobolFiles.str(l, 253, 10));
        t.setCardNum(CobolFiles.str(l, 263, 16));
        t.setOrigTs(CobolFiles.str(l, 279, 26));
        t.setProcTs(CobolFiles.str(l, 305, 26));
    }

    /** CVTRA02Y: group(10) type(2) cat(4) rate(6 = S9(4)V99). */
    private void seedDisclosureGroups() throws IOException {
        for (String l : lines("discgrp.txt")) {
            DisclosureGroup d = new DisclosureGroup();
            d.setAcctGroupId(CobolFiles.str(l, 1, 10));
            d.setTranTypeCd(CobolFiles.str(l, 11, 2));
            d.setTranCatCd((int) CobolFiles.num(l, 13, 4));
            d.setIntRate(CobolFiles.zoned(l, 17, 6, 2));
            disclGroups.save(d);
        }
    }

    /** CVTRA01Y: acct(11) type(2) cat(4) bal(11 = S9(9)V99). */
    private void seedTranCatBals() throws IOException {
        for (String l : lines("tcatbal.txt")) {
            TranCatBal t = new TranCatBal();
            t.setAcctId(CobolFiles.num(l, 1, 11));
            t.setTypeCd(CobolFiles.str(l, 12, 2));
            t.setCatCd((int) CobolFiles.num(l, 14, 4));
            t.setBal(CobolFiles.zoned(l, 18, 11, 2));
            tcatBals.save(t);
        }
    }

    /** CVTRA04Y: type(2) cat(4) desc(50). */
    private void seedTranCats() throws IOException {
        for (String l : lines("trancatg.txt")) {
            TransactionCategory t = new TransactionCategory();
            t.setTypeCd(CobolFiles.str(l, 1, 2));
            t.setCatCd((int) CobolFiles.num(l, 3, 4));
            t.setCatTypeDesc(CobolFiles.str(l, 7, 50));
            tranCats.save(t);
        }
    }

    /** CVTRA03Y: type(2) desc(50). */
    private void seedTranTypes() throws IOException {
        for (String l : lines("trantype.txt")) {
            TransactionType t = new TransactionType();
            t.setTypeCd(CobolFiles.str(l, 1, 2));
            t.setTypeDesc(CobolFiles.str(l, 3, 50));
            tranTypes.save(t);
        }
    }

    /** CSUSR01Y: id(8) fname(20) lname(20) pwd(8) type(1). */
    private void seedUsers() throws IOException {
        for (String l : lines("usrsec.txt")) {
            UserSecurity u = new UserSecurity();
            u.setUsrId(CobolFiles.str(l, 1, 8));
            u.setFirstName(CobolFiles.str(l, 9, 20));
            u.setLastName(CobolFiles.str(l, 29, 20));
            u.setPassword(CobolFiles.str(l, 49, 8));
            u.setUsrType(CobolFiles.str(l, 57, 1));
            users.save(u);
        }
    }
}
