package com.carddemo.api;

import com.carddemo.auth.AuthService;
import com.carddemo.auth.AuthService.LoginResult;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** POST /api/v1/auth/login — COSGN00C (plan §6). */
@RestController
@RequestMapping("/api/v1/auth")
@Validated
public class AuthController {

    private final AuthService authService;

    public AuthController(AuthService authService) {
        this.authService = authService;
    }

    public record LoginRequest(
            @NotBlank @Size(max = 8) String userId,
            @NotBlank @Size(max = 8) String password) {
    }

    public record LoginResponse(
            String token,
            long expiresInSeconds,
            String userId,
            String firstName,
            String lastName,
            String role) {
    }

    @PostMapping("/login")
    public LoginResponse login(@RequestBody @jakarta.validation.Valid LoginRequest request) {
        LoginResult result = authService.login(request.userId(), request.password());
        return new LoginResponse(
                result.token(),
                result.expiresInSeconds(),
                result.user().getUserId(),
                result.user().getFirstName(),
                result.user().getLastName(),
                result.user().role());
    }
}
