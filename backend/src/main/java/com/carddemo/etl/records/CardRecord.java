package com.carddemo.etl.records;

import com.carddemo.etl.FixedWidth;

import java.time.LocalDate;

/**
 * CARDDAT record per CVACT02Y (RECLN 150).
 *
 * <pre>
 * offset len field
 *      0  16 CARD-NUM            PIC X(16)
 *     16  11 CARD-ACCT-ID        PIC 9(11)
 *     27   3 CARD-CVV-CD         PIC 9(03)   (parsed but NOT persisted — plan R5)
 *     30  50 CARD-EMBOSSED-NAME  PIC X(50)
 *     80  10 CARD-EXPIRAION-DATE PIC X(10)   (legacy typo; -> expiration_date)
 *     90   1 CARD-ACTIVE-STATUS  PIC X(01)
 * </pre>
 */
public record CardRecord(
        String cardNumber,
        long accountId,
        String embossedName,
        LocalDate expirationDate,
        String activeStatus) {

    public static CardRecord parse(String line) {
        return new CardRecord(
                FixedWidth.raw(line, 0, 16),
                FixedWidth.unsignedLong(line, 16, 11),
                FixedWidth.text(line, 30, 50),
                FixedWidth.date(line, 80, 10),
                FixedWidth.text(line, 90, 1));
    }
}
