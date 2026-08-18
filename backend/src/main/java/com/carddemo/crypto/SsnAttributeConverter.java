package com.carddemo.crypto;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;
import org.springframework.stereotype.Component;

/**
 * Transparent AES-GCM encryption of {@code customers.ssn} (plan R5): the
 * entity field stays the plaintext SSN (so {@code Customer#ssnLast4()} masking
 * is unchanged) while the column stores ciphertext. Resolved as a Spring bean
 * through Hibernate's {@code SpringBeanContainer} so the key is injectable.
 */
@Component
@Converter
public class SsnAttributeConverter implements AttributeConverter<String, String> {

    private final SsnCrypto crypto;

    public SsnAttributeConverter(SsnCrypto crypto) {
        this.crypto = crypto;
    }

    @Override
    public String convertToDatabaseColumn(String plaintext) {
        return crypto.encrypt(plaintext);
    }

    @Override
    public String convertToEntityAttribute(String ciphertext) {
        return crypto.decrypt(ciphertext);
    }
}
