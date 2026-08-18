package com.carddemo.etl;

import com.carddemo.etl.records.AccountRecord;
import com.carddemo.etl.records.CardRecord;
import com.carddemo.etl.records.CardXrefRecord;
import com.carddemo.etl.records.CustomerRecord;
import com.carddemo.etl.records.ReferenceRecords.DisclosureGroup;
import com.carddemo.etl.records.ReferenceRecords.SecUser;
import com.carddemo.etl.records.ReferenceRecords.TranCategory;
import com.carddemo.etl.records.ReferenceRecords.TranCategoryBalance;
import com.carddemo.etl.records.ReferenceRecords.TranType;
import com.carddemo.etl.records.TransactionRecord;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Copybook-offset parser tests against real first lines of the committed
 * ASCII fixtures (etl/fixtures/ascii), i.e. copybook fixtures per plan §8.
 */
class CopybookParserTest {

    static final Path ASCII = Path.of("../etl/fixtures/ascii");

    private static String firstLine(String file) throws Exception {
        try (var lines = Files.lines(ASCII.resolve(file))) {
            return lines.findFirst().orElseThrow();
        }
    }

    @Test
    void parsesAccountRecordPerCvact01y() throws Exception {
        AccountRecord r = AccountRecord.parse(firstLine("acctdata.txt"));
        assertThat(r.accountId()).isEqualTo(1L);
        assertThat(r.activeStatus()).isEqualTo("Y");
        // '00000001940{' PIC S9(10)V99 -> +194.00; '00000020200{' -> +2020.00; '00000010200{' -> +1020.00
        assertThat(r.currBal()).isEqualByComparingTo(new BigDecimal("194.00"));
        assertThat(r.creditLimit()).isEqualByComparingTo(new BigDecimal("2020.00"));
        assertThat(r.cashCreditLimit()).isEqualByComparingTo(new BigDecimal("1020.00"));
        assertThat(r.openDate()).isEqualTo(LocalDate.of(2014, 11, 20));
        assertThat(r.expirationDate()).isEqualTo(LocalDate.of(2025, 5, 20));
        assertThat(r.reissueDate()).isEqualTo(LocalDate.of(2025, 5, 20));
        assertThat(r.currCycCredit()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(r.currCycDebit()).isEqualByComparingTo(BigDecimal.ZERO);
        assertThat(r.zip()).isEqualTo("A000000000");
        // B1: all 50 sample accounts have blank ACCT-GROUP-ID -> NULL
        assertThat(r.groupId()).isNull();
    }

    @Test
    void allFixtureAccountsHaveNullGroupId() throws Exception {
        List<String> lines = Files.readAllLines(ASCII.resolve("acctdata.txt"));
        assertThat(lines).hasSize(50);
        assertThat(lines.stream().map(AccountRecord::parse))
                .allSatisfy(a -> assertThat(a.groupId()).isNull());
    }

    @Test
    void parsesCustomerRecordPerCvcus01y() throws Exception {
        CustomerRecord r = CustomerRecord.parse(firstLine("custdata.txt"));
        assertThat(r.customerId()).isEqualTo(1L);
        assertThat(r.firstName()).isEqualTo("Immanuel");
        assertThat(r.middleName()).isEqualTo("Madeline");
        assertThat(r.lastName()).isEqualTo("Kessler");
        assertThat(r.addrLine1()).isEqualTo("618 Deshaun Route");
        assertThat(r.countryCd()).isEqualTo("USA");
        assertThat(r.ssn()).hasSize(9).containsOnlyDigits();
        assertThat(r.dob()).isNotNull();
        assertThat(r.priCardHolder()).isEqualTo("Y");
        assertThat(r.ficoScore()).isBetween(1, 999);
    }

    @Test
    void parsesCardRecordPerCvact02y() throws Exception {
        CardRecord r = CardRecord.parse(firstLine("carddata.txt"));
        assertThat(r.cardNumber()).isEqualTo("0500024453765740");
        assertThat(r.accountId()).isEqualTo(50L);
        assertThat(r.embossedName()).isEqualTo("Aniya Von");
        assertThat(r.expirationDate()).isEqualTo(LocalDate.of(2023, 3, 9));
        assertThat(r.activeStatus()).isEqualTo("Y");
    }

    @Test
    void parsesCardXrefPerCvact03y() throws Exception {
        CardXrefRecord r = CardXrefRecord.parse(firstLine("cardxref.txt"));
        assertThat(r.cardNumber()).isEqualTo("0500024453765740");
        assertThat(r.customerId()).isEqualTo(50L);
        assertThat(r.accountId()).isEqualTo(50L);
    }

    @Test
    void parsesDailyTransactionPerCvtra06y() throws Exception {
        TransactionRecord r = TransactionRecord.parse(firstLine("dailytran.txt"));
        assertThat(r.tranId()).isEqualTo("0000000000683580");
        assertThat(r.typeCd()).isEqualTo("01");
        assertThat(r.categoryCd()).isEqualTo(1);
        assertThat(r.source()).isEqualTo("POS TERM");
        assertThat(r.description()).isEqualTo("Purchase at Abshire-Lowe");
        // '0000005047G' -> +504.77 (G overpunch = +7)
        assertThat(r.amount()).isEqualByComparingTo(new BigDecimal("504.77"));
        assertThat(r.merchantId()).isEqualTo(800000000L);
        assertThat(r.merchantName()).isEqualTo("Abshire-Lowe");
        assertThat(r.merchantCity()).isEqualTo("North Enoshaven");
        assertThat(r.merchantZip()).isEqualTo("72112");
        assertThat(r.cardNumber()).isEqualTo("4859452612877065");
        assertThat(r.origTs()).isEqualTo(LocalDateTime.of(2022, 6, 10, 19, 27, 53));
        // DALYTRAN-PROC-TS is blank in the daily file
        assertThat(r.procTs()).isNull();
    }

    @Test
    void parsesNegativeDailyTransactionAmount() throws Exception {
        List<String> lines = Files.readAllLines(ASCII.resolve("dailytran.txt"));
        // line 2: Return item, amount '0000009190}' -> -919.00
        TransactionRecord r = TransactionRecord.parse(lines.get(1));
        assertThat(r.typeCd()).isEqualTo("03");
        assertThat(r.amount()).isEqualByComparingTo(new BigDecimal("-919.00"));
    }

    @Test
    void parsesReferenceRecords() throws Exception {
        TranType tt = TranType.parse(firstLine("trantype.txt"));
        assertThat(tt.typeCd()).isEqualTo("01");
        assertThat(tt.description()).isEqualTo("Purchase");

        TranCategory tc = TranCategory.parse(firstLine("trancatg.txt"));
        assertThat(tc.typeCd()).isEqualTo("01");
        assertThat(tc.categoryCd()).isEqualTo(1);
        assertThat(tc.description()).isEqualTo("Regular Sales Draft");

        DisclosureGroup dg = DisclosureGroup.parse(firstLine("discgrp.txt"));
        assertThat(dg.groupId()).isEqualTo("A000000000");
        assertThat(dg.typeCd()).isEqualTo("01");
        assertThat(dg.categoryCd()).isEqualTo(1);
        assertThat(dg.interestRate()).isEqualByComparingTo(new BigDecimal("15.00"));

        TranCategoryBalance tb = TranCategoryBalance.parse(firstLine("tcatbal.txt"));
        assertThat(tb.accountId()).isEqualTo(1L);
        assertThat(tb.typeCd()).isEqualTo("01");
        assertThat(tb.categoryCd()).isEqualTo(1);
        assertThat(tb.balance()).isEqualByComparingTo(BigDecimal.ZERO);
    }

    @Test
    void parsesDusrsecjInstreamUsersPerCsusr01y() {
        List<SecUser> users = JclInstreamUsers.parse(Path.of("../etl/fixtures/jcl/DUSRSECJ.jcl"));
        assertThat(users).hasSize(10);
        SecUser admin = users.get(0);
        assertThat(admin.userId()).isEqualTo("ADMIN001");
        assertThat(admin.firstName()).isEqualTo("MARGARET");
        assertThat(admin.lastName()).isEqualTo("GOLD");
        assertThat(admin.password()).isEqualTo("PASSWORD");
        assertThat(admin.userType()).isEqualTo("A");
        SecUser user = users.get(5);
        assertThat(user.userId()).isEqualTo("USER0001");
        assertThat(user.userType()).isEqualTo("U");
        assertThat(users).filteredOn(u -> "A".equals(u.userType())).hasSize(5);
        assertThat(users).filteredOn(u -> "U".equals(u.userType())).hasSize(5);
    }

    @Test
    void dateParserRejectsGarbage() {
        assertThatThrownBy(() -> FixedWidth.date("20XX-99-99", 0, 10))
                .isInstanceOf(DateTimeParseException.class);
    }

    @Test
    void dateParserBlankIsNull() {
        assertThat(FixedWidth.date("          ", 0, 10)).isNull();
    }

    @Test
    void shortLineIsSpacePaddedToRecln() {
        // trantype.txt line 7 has trailing spaces stripped (60 vs 61 chars)
        assertThat(FixedWidth.text("07Adjustment", 2, 50)).isEqualTo("Adjustment");
        assertThat(FixedWidth.text("07Adjustment", 52, 8)).isNull();
    }
}
