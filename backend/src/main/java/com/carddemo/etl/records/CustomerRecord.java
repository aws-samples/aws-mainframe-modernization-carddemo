package com.carddemo.etl.records;

import com.carddemo.etl.FixedWidth;

import java.time.LocalDate;

/**
 * CUSTDAT record per CVCUS01Y (RECLN 500).
 *
 * <pre>
 * offset len field
 *      0   9 CUST-ID                  PIC 9(09)
 *      9  25 CUST-FIRST-NAME          PIC X(25)
 *     34  25 CUST-MIDDLE-NAME         PIC X(25)
 *     59  25 CUST-LAST-NAME           PIC X(25)
 *     84  50 CUST-ADDR-LINE-1         PIC X(50)
 *    134  50 CUST-ADDR-LINE-2         PIC X(50)
 *    184  50 CUST-ADDR-LINE-3         PIC X(50)
 *    234   2 CUST-ADDR-STATE-CD       PIC X(02)
 *    236   3 CUST-ADDR-COUNTRY-CD     PIC X(03)
 *    239  10 CUST-ADDR-ZIP            PIC X(10)
 *    249  15 CUST-PHONE-NUM-1         PIC X(15)
 *    264  15 CUST-PHONE-NUM-2         PIC X(15)
 *    279   9 CUST-SSN                 PIC 9(09)
 *    288  20 CUST-GOVT-ISSUED-ID      PIC X(20)
 *    308  10 CUST-DOB-YYYY-MM-DD      PIC X(10)
 *    318  10 CUST-EFT-ACCOUNT-ID      PIC X(10)
 *    328   1 CUST-PRI-CARD-HOLDER-IND PIC X(01)
 *    329   3 CUST-FICO-CREDIT-SCORE   PIC 9(03)
 * </pre>
 */
public record CustomerRecord(
        long customerId,
        String firstName,
        String middleName,
        String lastName,
        String addrLine1,
        String addrLine2,
        String addrLine3,
        String stateCd,
        String countryCd,
        String zip,
        String phone1,
        String phone2,
        String ssn,
        String govtIssuedId,
        LocalDate dob,
        String eftAccountId,
        String priCardHolder,
        int ficoScore) {

    public static CustomerRecord parse(String line) {
        return new CustomerRecord(
                FixedWidth.unsignedLong(line, 0, 9),
                FixedWidth.text(line, 9, 25),
                FixedWidth.text(line, 34, 25),
                FixedWidth.text(line, 59, 25),
                FixedWidth.text(line, 84, 50),
                FixedWidth.text(line, 134, 50),
                FixedWidth.text(line, 184, 50),
                FixedWidth.text(line, 234, 2),
                FixedWidth.text(line, 236, 3),
                FixedWidth.text(line, 239, 10),
                FixedWidth.text(line, 249, 15),
                FixedWidth.text(line, 264, 15),
                FixedWidth.raw(line, 279, 9),
                FixedWidth.text(line, 288, 20),
                FixedWidth.date(line, 308, 10),
                FixedWidth.text(line, 318, 10),
                FixedWidth.text(line, 328, 1),
                FixedWidth.unsignedInt(line, 329, 3));
    }
}
