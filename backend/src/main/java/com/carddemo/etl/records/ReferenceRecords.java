package com.carddemo.etl.records;

import com.carddemo.etl.FixedWidth;

import java.math.BigDecimal;

/** Small reference-data record layouts (CVTRA01Y-04Y). */
public final class ReferenceRecords {

    private ReferenceRecords() {
    }

    /**
     * TRANTYPE per CVTRA03Y (RECLN 60):
     * 0/2 TRAN-TYPE PIC X(02); 2/50 TRAN-TYPE-DESC PIC X(50).
     */
    public record TranType(String typeCd, String description) {
        public static TranType parse(String line) {
            return new TranType(FixedWidth.raw(line, 0, 2), FixedWidth.text(line, 2, 50));
        }
    }

    /**
     * TRANCATG per CVTRA04Y (RECLN 60):
     * 0/2 TRAN-TYPE-CD PIC X(02); 2/4 TRAN-CAT-CD PIC 9(04);
     * 6/50 TRAN-CAT-TYPE-DESC PIC X(50).
     */
    public record TranCategory(String typeCd, int categoryCd, String description) {
        public static TranCategory parse(String line) {
            return new TranCategory(
                    FixedWidth.raw(line, 0, 2),
                    FixedWidth.unsignedInt(line, 2, 4),
                    FixedWidth.text(line, 6, 50));
        }
    }

    /**
     * DISCGRP per CVTRA02Y (RECLN 50):
     * 0/10 DIS-ACCT-GROUP-ID PIC X(10); 10/2 DIS-TRAN-TYPE-CD PIC X(02);
     * 12/4 DIS-TRAN-CAT-CD PIC 9(04); 16/6 DIS-INT-RATE PIC S9(04)V99.
     */
    public record DisclosureGroup(String groupId, String typeCd, int categoryCd, BigDecimal interestRate) {
        public static DisclosureGroup parse(String line) {
            return new DisclosureGroup(
                    FixedWidth.text(line, 0, 10),
                    FixedWidth.raw(line, 10, 2),
                    FixedWidth.unsignedInt(line, 12, 4),
                    FixedWidth.zoned(line, 16, 6, 2));
        }
    }

    /**
     * TCATBALF per CVTRA01Y (RECLN 50):
     * 0/11 TRANCAT-ACCT-ID PIC 9(11); 11/2 TRANCAT-TYPE-CD PIC X(02);
     * 13/4 TRANCAT-CD PIC 9(04); 17/11 TRAN-CAT-BAL PIC S9(09)V99.
     */
    public record TranCategoryBalance(long accountId, String typeCd, int categoryCd, BigDecimal balance) {
        public static TranCategoryBalance parse(String line) {
            return new TranCategoryBalance(
                    FixedWidth.unsignedLong(line, 0, 11),
                    FixedWidth.raw(line, 11, 2),
                    FixedWidth.unsignedInt(line, 13, 4),
                    FixedWidth.zoned(line, 17, 11, 2));
        }
    }

    /**
     * USRSEC per CSUSR01Y (RECLN 80), from DUSRSECJ.jcl in-stream SYSUT1 data:
     * 0/8 SEC-USR-ID PIC X(08); 8/20 SEC-USR-FNAME PIC X(20);
     * 28/20 SEC-USR-LNAME PIC X(20); 48/8 SEC-USR-PWD PIC X(08);
     * 56/1 SEC-USR-TYPE PIC X(01).
     */
    public record SecUser(String userId, String firstName, String lastName, String password, String userType) {
        public static SecUser parse(String line) {
            return new SecUser(
                    FixedWidth.text(line, 0, 8),
                    FixedWidth.text(line, 8, 20),
                    FixedWidth.text(line, 28, 20),
                    FixedWidth.raw(line, 48, 8).trim(),
                    FixedWidth.text(line, 56, 1));
        }
    }
}
