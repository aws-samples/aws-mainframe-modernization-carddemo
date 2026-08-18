package com.carddemo.api;

import com.carddemo.api.dto.ApiDtos.Page;
import com.carddemo.api.dto.UserDtos.UserCreateRequest;
import com.carddemo.api.dto.UserDtos.UserUpdateRequest;
import com.carddemo.api.dto.UserDtos.UserView;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

/**
 * /api/v1/users — admin user CRUD, COUSR00C-03C (plan §6). ADMIN-only via
 * the SecurityConfig {@code /api/v1/users/**} rule (plan §5).
 */
@RestController
@RequestMapping("/api/v1/users")
public class UserController {

    private final UserAdminService userAdminService;

    public UserController(UserAdminService userAdminService) {
        this.userAdminService = userAdminService;
    }

    @GetMapping
    public Page<UserView> listUsers(
            @RequestParam(name = "cursor", required = false) String cursor,
            @RequestParam(name = "limit", required = false) Integer limit) {
        return userAdminService.listUsers(cursor, limit);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public UserView createUser(@RequestBody UserCreateRequest request) {
        return userAdminService.createUser(request);
    }

    @GetMapping("/{id}")
    public UserView getUser(@PathVariable("id") String id) {
        return userAdminService.getUser(id);
    }

    @PutMapping("/{id}")
    public UserView updateUser(@PathVariable("id") String id,
                               @RequestBody UserUpdateRequest request) {
        return userAdminService.updateUser(id, request);
    }

    @DeleteMapping("/{id}")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void deleteUser(@PathVariable("id") String id) {
        userAdminService.deleteUser(id);
    }
}
