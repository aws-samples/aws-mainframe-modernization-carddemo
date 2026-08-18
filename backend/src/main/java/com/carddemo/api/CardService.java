package com.carddemo.api;

import com.carddemo.api.ValidationException.FieldError;
import com.carddemo.api.dto.ApiDtos.CardDetail;
import com.carddemo.api.dto.ApiDtos.CardSummary;
import com.carddemo.api.dto.ApiDtos.Page;
import com.carddemo.api.dto.CardUpdateRequest;
import com.carddemo.domain.Card;
import com.carddemo.repo.CardRepository;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * COCRDLIC/COCRDSLC port (plan §4): account filter + keyset paging on the
 * card_number natural PK. Per open decision R9(b) there is NO per-user
 * scoping — authenticated USERs browse all cards with filters.
 */
@Service
public class CardService {

    private final CardRepository cards;

    public CardService(CardRepository cards) {
        this.cards = cards;
    }

    /** Card numbers MASKED in list responses (plan §5). */
    @Transactional(readOnly = true)
    public Page<CardSummary> listCards(Long accountId, String cursor, Integer limit) {
        int pageSize = Paging.clamp(limit);
        // fetch one extra row to know whether a next page exists
        List<Card> page = cards.pageForward(accountId, Paging.blankToNull(cursor),
                PageRequest.of(0, pageSize + 1));
        return Paging.page(page, pageSize, c -> c.getCardNumber().trim(), CardSummary::masked);
    }

    /** Full card number allowed on the detail view only (plan §5). */
    @Transactional(readOnly = true)
    public CardDetail getCard(String cardNumber) {
        Card card = cards.findById(cardNumber)
                .orElseThrow(() -> new NotFoundException("Card not found"));
        return CardDetail.of(card);
    }

    /**
     * COCRDUPC port (plan §4): name alpha edit (1230-EDIT-NAME), status Y/N
     * (1240-EDIT-CARDSTATUS), expiry month 1–12 (1250-EDIT-EXPIRY-MON) and
     * year 1950–2099 (1260-EDIT-EXPIRY-YEAR); change-detection no-op
     * (NO-CHANGES-DETECTED); fetched-version mismatch → 409 with the legacy
     * "changed by some one else" message (9300-CHECK-CHANGE-IN-REC, plan R3).
     * The confirm-before-commit flow (F5) lives in the UI confirm step.
     */
    @Transactional
    public CardDetail updateCard(String cardNumber, CardUpdateRequest req) {
        List<FieldError> errors = new ArrayList<>();

        // 1230-EDIT-NAME: mandatory, alphabets and spaces only
        String name = req.embossedName() == null ? "" : req.embossedName().trim();
        if (name.isEmpty()) {
            errors.add(new FieldError("embossedName", "Card name not provided"));
        } else if (!name.chars().allMatch(ch -> Character.isLetter(ch) || ch == ' ')) {
            errors.add(new FieldError("embossedName",
                    "Card name can only contain alphabets and spaces"));
        }

        // 1240-EDIT-CARDSTATUS: mandatory Y/N
        String status = req.activeStatus() == null ? "" : req.activeStatus().trim().toUpperCase();
        if (!status.equals("Y") && !status.equals("N")) {
            errors.add(new FieldError("activeStatus", "Card Active Status must be Y or N"));
        }

        // 1250-EDIT-EXPIRY-MON: mandatory, VALID-MONTH 1 THRU 12
        Integer month = parseInt(req.expiryMonth());
        if (month == null || month < 1 || month > 12) {
            errors.add(new FieldError("expiryMonth", "Card expiry month must be between 1 and 12"));
        }

        // 1260-EDIT-EXPIRY-YEAR: mandatory, VALID-YEAR 1950 THRU 2099
        Integer year = parseInt(req.expiryYear());
        if (year == null || year < 1950 || year > 2099) {
            errors.add(new FieldError("expiryYear", "Invalid card expiry year"));
        }

        if (!errors.isEmpty()) {
            throw new ValidationException(errors);
        }

        Card card = cards.findById(cardNumber)
                .orElseThrow(() -> new NotFoundException("Card not found"));

        // 9300-CHECK-CHANGE-IN-REC → DATA-WAS-CHANGED-BEFORE-UPDATE
        if (req.version() != null && req.version() != card.getVersion()) {
            throw new ConflictException(AccountService.CHANGED_BY_SOMEONE_ELSE);
        }

        // legacy keeps the expiry DAY from the stored record (only M/Y are edited)
        LocalDate stored = card.getExpirationDate();
        int day = stored == null ? 1 : Math.min(stored.getDayOfMonth(),
                LocalDate.of(year, month, 1).lengthOfMonth());
        LocalDate newExpiry = LocalDate.of(year, month, day);

        // NO-CHANGES-DETECTED: upper-cased compare of new vs old → no-op write
        boolean unchanged = name.equalsIgnoreCase(safeTrim(card.getEmbossedName()))
                && status.equalsIgnoreCase(safeTrim(card.getActiveStatus()))
                && Objects.equals(stored, newExpiry);
        if (unchanged) {
            return CardDetail.of(card);
        }

        card.setEmbossedName(name);
        card.setActiveStatus(status);
        card.setExpirationDate(newExpiry);
        cards.saveAndFlush(card);
        return CardDetail.of(card);
    }

    private static Integer parseInt(String s) {
        if (s == null || s.isBlank()) {
            return null;
        }
        try {
            return Integer.valueOf(s.trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    private static String safeTrim(String s) {
        return s == null ? "" : s.trim();
    }
}
