package com.carddemo.service;

import com.carddemo.model.Card;
import com.carddemo.repo.CardRepository;
import com.carddemo.web.ApiException;
import com.carddemo.web.Dto;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** COCRDLIC.cbl (list, 7 rows/screen), COCRDSLC.cbl (detail), COCRDUPC.cbl (update). */
@Service
public class CardService {

    /** WS-MAX-SCREEN-LINES in COCRDLIC. */
    public static final int PAGE_SIZE = 7;

    private final CardRepository cards;

    public CardService(CardRepository cards) {
        this.cards = cards;
    }

    public Dto.PageResult<Dto.CardView> list(int page) {
        var p = cards.findAllByOrderByCardNumAsc(PageRequest.of(page, PAGE_SIZE));
        return Dto.PageResult.of(p.map(Dto.CardView::of));
    }

    public Dto.CardView view(String cardNum) {
        Card c = cards.findById(normalize(cardNum))
                .orElseThrow(() -> ApiException.notFound("Card Number NOT found..."));
        return Dto.CardView.of(c);
    }

    @Transactional
    public Dto.CardView update(String cardNum, Dto.CardUpdateRequest req) {
        Card c = cards.findById(normalize(cardNum))
                .orElseThrow(() -> ApiException.notFound("Card Number NOT found..."));

        // COCRDUPC validation messages.
        if (req.activeStatus() != null && !req.activeStatus().equalsIgnoreCase("Y")
                && !req.activeStatus().equalsIgnoreCase("N")) {
            throw ApiException.badRequest("Card Active Status must be Y or N");
        }
        String exp = req.expirationDate();
        if (exp == null && (req.expiryMonth() != null || req.expiryYear() != null)) {
            // Month/year supplied separately (BMS has distinct fields).
            int month = parseMonth(req.expiryMonth());
            int year = parseYear(req.expiryYear());
            String day = c.getExpirationDate() != null && c.getExpirationDate().length() == 10
                    ? c.getExpirationDate().substring(8) : "01";
            exp = String.format("%04d-%02d-%s", year, month, day);
        }
        if (exp != null) {
            validateExpiry(exp);
        }

        if (req.embossedName() != null) c.setEmbossedName(req.embossedName());
        if (req.activeStatus() != null) c.setActiveStatus(req.activeStatus().toUpperCase());
        if (exp != null) c.setExpirationDate(exp);
        cards.save(c);
        return Dto.CardView.of(c);
    }

    private static void validateExpiry(String exp) {
        if (!exp.matches("\\d{4}-\\d{2}-\\d{2}")) {
            throw ApiException.badRequest("Invalid card expiry year");
        }
        int month = Integer.parseInt(exp.substring(5, 7));
        if (month < 1 || month > 12) {
            throw ApiException.badRequest("Card expiry month must be between 1 and 12");
        }
        if (!com.carddemo.util.DateValidator.isValid(exp)) {
            throw ApiException.badRequest("Invalid card expiry year");
        }
    }

    private static int parseMonth(String m) {
        try {
            int v = Integer.parseInt(m.trim());
            if (v < 1 || v > 12) throw new NumberFormatException();
            return v;
        } catch (Exception e) {
            throw ApiException.badRequest("Card expiry month must be between 1 and 12");
        }
    }

    private static int parseYear(String y) {
        try {
            return Integer.parseInt(y.trim());
        } catch (Exception e) {
            throw ApiException.badRequest("Invalid card expiry year");
        }
    }

    private static String normalize(String cardNum) {
        if (cardNum == null || cardNum.isBlank()) {
            throw ApiException.badRequest("Account or Card Number must be entered...");
        }
        if (!cardNum.matches("\\d{16}")) {
            throw ApiException.badRequest("Card number if supplied must be a 16 digit number");
        }
        return cardNum;
    }
}
