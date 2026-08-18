package com.carddemo.api;

import com.carddemo.api.dto.ApiDtos.CardDetail;
import com.carddemo.api.dto.ApiDtos.CardSummary;
import com.carddemo.api.dto.ApiDtos.Page;
import com.carddemo.api.dto.CardUpdateRequest;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/** /api/v1/cards + /{cardNumber} — COCRDLIC/COCRDSLC list/detail, COCRDUPC update (plan §6). */
@RestController
@RequestMapping("/api/v1/cards")
public class CardController {

    private final CardService cardService;

    public CardController(CardService cardService) {
        this.cardService = cardService;
    }

    @GetMapping
    public Page<CardSummary> listCards(
            @RequestParam(name = "accountId", required = false) Long accountId,
            @RequestParam(name = "cursor", required = false) String cursor,
            @RequestParam(name = "limit", required = false) Integer limit) {
        return cardService.listCards(accountId, cursor, limit);
    }

    @GetMapping("/{cardNumber}")
    public CardDetail getCard(@PathVariable("cardNumber") String cardNumber) {
        return cardService.getCard(cardNumber);
    }

    @PutMapping("/{cardNumber}")
    public CardDetail updateCard(@PathVariable("cardNumber") String cardNumber,
                                 @RequestBody CardUpdateRequest request) {
        return cardService.updateCard(cardNumber, request);
    }
}
