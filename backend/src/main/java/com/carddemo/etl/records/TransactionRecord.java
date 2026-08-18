package com.carddemo.etl.records;

import com.carddemo.etl.FixedWidth;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * TRAN/DALYTRAN record per CVTRA05Y / CVTRA06Y (identical layout, RECLN 350).
 *
 * <pre>
 * offset len field
 *      0  16 TRAN-ID            PIC X(16)
 *     16   2 TRAN-TYPE-CD       PIC X(02)
 *     18   4 TRAN-CAT-CD        PIC 9(04)
 *     22  10 TRAN-SOURCE        PIC X(10)
 *     32 100 TRAN-DESC          PIC X(100)
 *    132  11 TRAN-AMT           PIC S9(09)V99
 *    143   9 TRAN-MERCHANT-ID   PIC 9(09)
 *    152  50 TRAN-MERCHANT-NAME PIC X(50)
 *    202  50 TRAN-MERCHANT-CITY PIC X(50)
 *    252  10 TRAN-MERCHANT-ZIP  PIC X(10)
 *    262  16 TRAN-CARD-NUM      PIC X(16)
 *    278  26 TRAN-ORIG-TS       PIC X(26)
 *    304  26 TRAN-PROC-TS       PIC X(26)
 * </pre>
 */
public record TransactionRecord(
        String tranId,
        String typeCd,
        int categoryCd,
        String source,
        String description,
        BigDecimal amount,
        long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNumber,
        LocalDateTime origTs,
        LocalDateTime procTs) {

    public static TransactionRecord parse(String line) {
        return new TransactionRecord(
                FixedWidth.raw(line, 0, 16),
                FixedWidth.text(line, 16, 2),
                FixedWidth.unsignedInt(line, 18, 4),
                FixedWidth.text(line, 22, 10),
                FixedWidth.text(line, 32, 100),
                FixedWidth.zoned(line, 132, 11, 2),
                FixedWidth.unsignedLong(line, 143, 9),
                FixedWidth.text(line, 152, 50),
                FixedWidth.text(line, 202, 50),
                FixedWidth.text(line, 252, 10),
                FixedWidth.raw(line, 262, 16),
                FixedWidth.timestamp(line, 278, 26),
                FixedWidth.timestamp(line, 304, 26));
    }
}
