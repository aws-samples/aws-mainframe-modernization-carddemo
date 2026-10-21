package com.carddemo.web;

import com.carddemo.config.TokenAuthFilter;
import com.carddemo.config.TokenService;
import com.carddemo.model.UserSecurity;
import com.carddemo.repo.UserSecurityRepository;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

/** COSGN00C.cbl — signon screen. */
@RestController
@RequestMapping("/api/auth")
public class AuthController {

    private final UserSecurityRepository users;
    private final TokenService tokens;

    public AuthController(UserSecurityRepository users, TokenService tokens) {
        this.users = users;
        this.tokens = tokens;
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody Dto.LoginRequest req) {
        if (req.userId() == null || req.userId().isBlank()) {
            return error("Please enter User ID ...");
        }
        if (req.password() == null || req.password().isBlank()) {
            return error("Please enter Password ...");
        }
        var user = users.findById(req.userId().trim().toUpperCase());
        if (user.isEmpty()) {
            return error("User not found. Try again ...");
        }
        if (!user.get().getPassword().equals(req.password().trim())) {
            return error("Wrong Password. Try again ...");
        }
        UserSecurity u = user.get();
        return ResponseEntity.ok(new Dto.LoginResponse(
                tokens.issue(u.getUsrId()), u.getUsrId(), u.getUsrType(),
                u.getFirstName(), u.getLastName()));
    }

    @GetMapping("/me")
    public Map<String, Object> me() {
        UserSecurity u = TokenAuthFilter.currentUser();
        if (u == null) {
            throw ApiException.forbidden("Unauthorized");
        }
        return Map.of("userId", u.getUsrId(), "userType", u.getUsrType(),
                "firstName", u.getFirstName(), "lastName", u.getLastName(),
                "admin", u.isAdmin());
    }

    private static ResponseEntity<Map<String, String>> error(String msg) {
        return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body(Map.of("message", msg));
    }
}
