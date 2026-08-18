package com.carddemo.crypto;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * SSN at-rest encryption wiring (plan R5): key material from
 * {@code carddemo.ssn.key} ({@code CARDDEMO_SSN_KEY}), consumed by the JPA
 * {@link SsnAttributeConverter} and the ETL loader.
 */
@Configuration
@EnableConfigurationProperties(SsnConfig.SsnProperties.class)
public class SsnConfig {

    /** {@code carddemo.ssn.key} — any non-blank string; SHA-256-derived AES key. */
    @ConfigurationProperties("carddemo.ssn")
    public record SsnProperties(String key) {
    }

    @Bean
    public SsnCrypto ssnCrypto(SsnProperties props) {
        return new SsnCrypto(props.key());
    }
}
