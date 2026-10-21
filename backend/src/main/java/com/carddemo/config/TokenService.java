package com.carddemo.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Base64;
import java.util.Optional;

/**
 * Minimal signed-token session service. Tokens are
 * base64url(userId).base64url(expiryEpoch).base64url(HMAC-SHA256).
 * Mirrors the CICS pseudo-conversational session: the signed-in user is
 * carried in the "commarea" of every request.
 */
@Service
public class TokenService {

    private final byte[] secret;
    private final long ttlMinutes;

    public TokenService(@Value("${carddemo.auth.secret}") String secret,
                        @Value("${carddemo.auth.token-ttl-minutes}") long ttlMinutes) {
        this.secret = secret.getBytes(StandardCharsets.UTF_8);
        this.ttlMinutes = ttlMinutes;
    }

    public String issue(String userId) {
        long expiry = Instant.now().plusSeconds(ttlMinutes * 60).getEpochSecond();
        String payload = userId + ":" + expiry;
        return b64(userId) + "." + b64(Long.toString(expiry)) + "." + b64(sign(payload));
    }

    /** Returns the user id when the token is valid and unexpired. */
    public Optional<String> verify(String token) {
        try {
            String[] parts = token.split("\\.");
            if (parts.length != 3) {
                return Optional.empty();
            }
            String userId = unb64(parts[0]);
            long expiry = Long.parseLong(unb64(parts[1]));
            String expected = b64(sign(userId + ":" + expiry));
            if (!constantTimeEquals(expected, parts[2])) {
                return Optional.empty();
            }
            if (Instant.now().getEpochSecond() > expiry) {
                return Optional.empty();
            }
            return Optional.of(userId);
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    private byte[] sign(String payload) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(secret, "HmacSHA256"));
            return mac.doFinal(payload.getBytes(StandardCharsets.UTF_8));
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    private static String b64(String s) {
        return Base64.getUrlEncoder().withoutPadding()
                .encodeToString(s.getBytes(StandardCharsets.UTF_8));
    }

    private static String b64(byte[] b) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(b);
    }

    private static String unb64(String s) {
        return new String(Base64.getUrlDecoder().decode(s), StandardCharsets.UTF_8);
    }

    private static boolean constantTimeEquals(String a, String b) {
        if (a.length() != b.length()) {
            return false;
        }
        int r = 0;
        for (int i = 0; i < a.length(); i++) {
            r |= a.charAt(i) ^ b.charAt(i);
        }
        return r == 0;
    }
}
