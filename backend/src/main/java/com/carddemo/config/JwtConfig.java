package com.carddemo.config;

import com.nimbusds.jose.jwk.source.ImmutableSecret;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.oauth2.jose.jws.MacAlgorithm;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.jwt.JwtEncoder;
import org.springframework.security.oauth2.jwt.NimbusJwtDecoder;
import org.springframework.security.oauth2.jwt.NimbusJwtEncoder;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.time.Duration;

/**
 * Symmetric-key JWT (HS256) wiring — plan §5: short-lived access token, no
 * refresh rotation (demo scope). The same secret signs (login) and verifies
 * (resource-server filter).
 */
@Configuration
@EnableConfigurationProperties(JwtConfig.JwtProperties.class)
public class JwtConfig {

    /** {@code carddemo.jwt.secret} must be ≥ 32 bytes for HS256. */
    @ConfigurationProperties("carddemo.jwt")
    public record JwtProperties(String secret, Duration ttl) {
        public SecretKey secretKey() {
            byte[] bytes = secret.getBytes(StandardCharsets.UTF_8);
            if (bytes.length < 32) {
                throw new IllegalStateException("carddemo.jwt.secret must be at least 32 bytes for HS256");
            }
            return new SecretKeySpec(bytes, "HmacSHA256");
        }
    }

    @Bean
    public JwtEncoder jwtEncoder(JwtProperties props) {
        return new NimbusJwtEncoder(new ImmutableSecret<>(props.secretKey()));
    }

    @Bean
    public JwtDecoder jwtDecoder(JwtProperties props) {
        return NimbusJwtDecoder.withSecretKey(props.secretKey())
                .macAlgorithm(MacAlgorithm.HS256)
                .build();
    }
}
