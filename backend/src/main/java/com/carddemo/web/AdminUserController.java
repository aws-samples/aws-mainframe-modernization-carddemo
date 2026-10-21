package com.carddemo.web;

import com.carddemo.service.UserAdminService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

/** COUSR00C-COUSR03C endpoints (ADMIN role only — enforced by SecurityConfig). */
@RestController
@RequestMapping("/api/admin/users")
public class AdminUserController {

    private final UserAdminService service;

    public AdminUserController(UserAdminService service) {
        this.service = service;
    }

    @GetMapping
    public Dto.PageResult<Dto.UserView> list(@RequestParam(defaultValue = "0") int page) {
        return service.list(page);
    }

    @GetMapping("/{userId}")
    public Dto.UserView get(@PathVariable String userId) {
        return service.get(userId);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public Dto.UserView add(@RequestBody Dto.UserRequest req) {
        return service.add(req);
    }

    @PutMapping("/{userId}")
    public Dto.UserView update(@PathVariable String userId,
                               @RequestBody Dto.UserRequest req) {
        return service.update(userId, req);
    }

    @DeleteMapping("/{userId}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void delete(@PathVariable String userId) {
        service.delete(userId);
    }
}
