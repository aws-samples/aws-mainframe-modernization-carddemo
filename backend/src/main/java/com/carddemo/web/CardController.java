package com.carddemo.web;

import com.carddemo.service.CardService;
import org.springframework.web.bind.annotation.*;

/** COCRDLIC / COCRDSLC / COCRDUPC endpoints. */
@RestController
@RequestMapping("/api/cards")
public class CardController {

    private final CardService service;

    public CardController(CardService service) {
        this.service = service;
    }

    @GetMapping
    public Dto.PageResult<Dto.CardView> list(@RequestParam(defaultValue = "0") int page) {
        return service.list(page);
    }

    @GetMapping("/{cardNum}")
    public Dto.CardView view(@PathVariable String cardNum) {
        return service.view(cardNum);
    }

    @PutMapping("/{cardNum}")
    public Dto.CardView update(@PathVariable String cardNum,
                               @RequestBody Dto.CardUpdateRequest req) {
        return service.update(cardNum, req);
    }
}
