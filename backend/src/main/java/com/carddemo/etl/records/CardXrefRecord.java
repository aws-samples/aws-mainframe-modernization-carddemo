package com.carddemo.etl.records;

import com.carddemo.etl.FixedWidth;

/**
 * CXACAIX xref record per CVACT03Y (RECLN 50). The table is dissolved
 * (plan §2.2): it backfills accounts.customer_id and re-checks cards.account_id.
 *
 * <pre>
 * offset len field
 *      0  16 XREF-CARD-NUM PIC X(16)
 *     16   9 XREF-CUST-ID  PIC 9(09)
 *     25  11 XREF-ACCT-ID  PIC 9(11)
 * </pre>
 */
public record CardXrefRecord(String cardNumber, long customerId, long accountId) {

    public static CardXrefRecord parse(String line) {
        return new CardXrefRecord(
                FixedWidth.raw(line, 0, 16),
                FixedWidth.unsignedLong(line, 16, 9),
                FixedWidth.unsignedLong(line, 25, 11));
    }
}
