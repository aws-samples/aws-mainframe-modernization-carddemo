package com.carddemo.etl.records;

import com.carddemo.etl.FixedWidth;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * ACCTDAT record per CVACT01Y (RECLN 300).
 *
 * <pre>
 * offset len field
 *      0  11 ACCT-ID                PIC 9(11)
 *     11   1 ACCT-ACTIVE-STATUS     PIC X(01)
 *     12  12 ACCT-CURR-BAL          PIC S9(10)V99
 *     24  12 ACCT-CREDIT-LIMIT      PIC S9(10)V99
 *     36  12 ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
 *     48  10 ACCT-OPEN-DATE         PIC X(10)
 *     58  10 ACCT-EXPIRAION-DATE    PIC X(10)   (legacy typo; -> expiration_date)
 *     68  10 ACCT-REISSUE-DATE      PIC X(10)
 *     78  12 ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
 *     90  12 ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
 *    102  10 ACCT-ADDR-ZIP          PIC X(10)
 *    112  10 ACCT-GROUP-ID          PIC X(10)   (blank -> NULL, plan B1)
 * </pre>
 */
public record AccountRecord(
        long accountId,
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
        String groupId) {

    public static AccountRecord parse(String line) {
        return new AccountRecord(
                FixedWidth.unsignedLong(line, 0, 11),
                FixedWidth.text(line, 11, 1),
                FixedWidth.zoned(line, 12, 12, 2),
                FixedWidth.zoned(line, 24, 12, 2),
                FixedWidth.zoned(line, 36, 12, 2),
                FixedWidth.date(line, 48, 10),
                FixedWidth.date(line, 58, 10),
                FixedWidth.date(line, 68, 10),
                FixedWidth.zoned(line, 78, 12, 2),
                FixedWidth.zoned(line, 90, 12, 2),
                FixedWidth.text(line, 102, 10),
                FixedWidth.text(line, 112, 10));
    }
}
