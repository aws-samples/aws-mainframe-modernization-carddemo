package com.carddemo.auth;

import com.carddemo.config.JwtConfig.JwtProperties;
import com.carddemo.domain.UserAccount;
import com.carddemo.repo.UserAccountRepository;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwsHeader;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.jwt.JwtEncoder;
import org.springframework.security.oauth2.jwt.JwtEncoderParameters;
import org.springframework.stereotype.Service;

import java.time.Instant;

/**
 * COSGN00C replacement (plan §4): user lookup + bcrypt verify + type A/U →
 * JWT with a {@code role} claim. Legacy compared the plaintext password
 * (COSGN00C.cbl:223); we verify against the bcrypt hash instead.
 */
@Service
public class AuthService {

    private final UserAccountRepository users;
    private final PasswordEncoder passwordEncoder;
    private final JwtEncoder jwtEncoder;
    private final JwtProperties jwtProperties;

    public AuthService(UserAccountRepository users,
                       PasswordEncoder passwordEncoder,
                       JwtEncoder jwtEncoder,
                       JwtProperties jwtProperties) {
        this.users = users;
        this.passwordEncoder = passwordEncoder;
        this.jwtEncoder = jwtEncoder;
        this.jwtProperties = jwtProperties;
    }

    public record LoginResult(String token, long expiresInSeconds, UserAccount user) {
    }

    /**
     * @throws BadCredentialsException on unknown user or wrong password —
     *         a single indistinct failure, unlike COSGN00C's separate
     *         "User not found" / "Wrong Password" messages (deliberate:
     *         don't leak which half failed).
     */
    public LoginResult login(String userId, String password) {
        UserAccount user = users.findById(userId == null ? "" : userId.trim().toUpperCase())
                .orElseThrow(() -> new BadCredentialsException("Invalid credentials"));
        if (!passwordEncoder.matches(password, user.getPasswordHash())) {
            throw new BadCredentialsException("Invalid credentials");
        }
        Instant now = Instant.now();
        Instant expiresAt = now.plus(jwtProperties.ttl());
        JwtClaimsSet claims = JwtClaimsSet.builder()
                .subject(user.getUserId())
                .issuedAt(now)
                .expiresAt(expiresAt)
                .claim("role", user.role())
                .claim("firstName", user.getFirstName())
                .claim("lastName", user.getLastName())
                .build();
        String token = jwtEncoder
                .encode(JwtEncoderParameters.from(JwsHeader.with(MacAlgorithm.HS256).build(), claims))
                .getTokenValue();
        return new LoginResult(token, jwtProperties.ttl().toSeconds(), user);
    }
}
