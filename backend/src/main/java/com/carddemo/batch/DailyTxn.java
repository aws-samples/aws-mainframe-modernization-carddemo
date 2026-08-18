package com.carddemo.batch;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/** One daily_transactions row (CVTRA06Y) as read by the POSTTRAN job. */
public record DailyTxn(
        String dailyTranId,
        String typeCd,
        Integer categoryCd,
        String source,
        String description,
        BigDecimal amount,
        Long merchantId,
        String merchantName,
        String merchantCity,
        String merchantZip,
        String cardNumber,
        LocalDateTime origTs,
        LocalDateTime procTs) {
}
