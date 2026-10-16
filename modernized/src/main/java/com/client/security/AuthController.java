package com.client.security;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.Map;

@RestController
@RequestMapping("/api/v1/auth")
public class AuthController {

    private final JwtTokenProvider tokenProvider;

    public AuthController(JwtTokenProvider tokenProvider) {
        this.tokenProvider = tokenProvider;
    }

    @PostMapping("/login")
    public ResponseEntity<?> login(@RequestBody Map<String, String> request) {
        String username = request.getOrDefault("username", "");
        String password = request.getOrDefault("password", "");
        if (username.trim().isEmpty() || password.trim().isEmpty()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                .body(Map.of("error", "Invalid credentials", "message", "Username and password required"));
        }
        String token = tokenProvider.generateToken(username, "USER");
        return ResponseEntity.ok(Map.of(
            "token", token,
            "tokenType", "Bearer",
            "username", username,
            "expiresIn", 86400
        ));
    }
}
