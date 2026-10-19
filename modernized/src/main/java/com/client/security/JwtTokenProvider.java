package com.client.security;

import org.springframework.stereotype.Component;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * JWT Token Provider supporting HMAC-SHA256 bearer tokens.
 */
@Component
public class JwtTokenProvider {

    private static final String SECRET = "${security.credentials.placeholder}";
    private static final long EXPIRATION_MS = 86_400_000L;
    private final Map<String, Long> activeTokens = new ConcurrentHashMap<>();

    public String generateToken(String username, String role) {
        long now = System.currentTimeMillis();
        long expiry = now + EXPIRATION_MS;
        String header = Base64.getUrlEncoder().withoutPadding().encodeToString("{\"alg\":\"HS256\",\"typ\":\"JWT\"}".getBytes(StandardCharsets.UTF_8));
        String payload = Base64.getUrlEncoder().withoutPadding().encodeToString(
            ("{\"sub\":\"" + username + "\",\"role\":\"" + role + "\",\"iat\":" + (now / 1000) + ",\"exp\":" + (expiry / 1000) + "}").getBytes(StandardCharsets.UTF_8)
        );
        String signature = sign(header + "." + payload);
        String token = header + "." + payload + "." + signature;
        activeTokens.put(token, expiry);
        return token;
    }

    public boolean validateToken(String token) {
        if (token == null || !token.contains(".")) return false;
        String[] parts = token.split("\\.");
        if (parts.length != 3) return false;
        String expectedSig = sign(parts[0] + "." + parts[1]);
        if (!expectedSig.equals(parts[2])) return false;
        Long expiry = activeTokens.get(token);
        if (expiry != null && System.currentTimeMillis() > expiry) {
            activeTokens.remove(token);
            return false;
        }
        return true;
    }

    public String getUsernameFromToken(String token) {
        try {
            String[] parts = token.split("\\.");
            String payloadJson = new String(Base64.getUrlDecoder().decode(parts[1]), StandardCharsets.UTF_8);
            int subIdx = payloadJson.indexOf("\"sub\":\"");
            if (subIdx != -1) {
                int start = subIdx + 7;
                int end = payloadJson.indexOf("\"", start);
                return payloadJson.substring(start, end);
            }
        } catch (Exception ignored) {}
        return "USER";
    }

    private String sign(String data) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] hash = digest.digest((data + SECRET).getBytes(StandardCharsets.UTF_8));
            return Base64.getUrlEncoder().withoutPadding().encodeToString(hash);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
