package com.carddemo.crypto;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * SSN at-rest encryption (plan R5, review should-fix #2): AES-GCM round-trip
 * through {@link SsnCrypto} and the JPA {@link SsnAttributeConverter}.
 */
class SsnCryptoTest {

    private final SsnCrypto crypto = new SsnCrypto("test-only-ssn-key");

    @Test
    void roundTripsAndNeverStoresPlaintext() {
        String ct = crypto.encrypt("587518382");
        assertThat(ct).isNotEqualTo("587518382").doesNotContain("587518382");
        assertThat(crypto.decrypt(ct)).isEqualTo("587518382");
        // fresh IV per encryption — same plaintext, different ciphertext
        assertThat(crypto.encrypt("587518382")).isNotEqualTo(ct);
    }

    @Test
    void nullPassesThroughBothWays() {
        assertThat(crypto.encrypt(null)).isNull();
        assertThat(crypto.decrypt(null)).isNull();
    }

    @Test
    void converterRoundTrips() {
        SsnAttributeConverter converter = new SsnAttributeConverter(crypto);
        String column = converter.convertToDatabaseColumn("899518382");
        assertThat(column).isNotEqualTo("899518382");
        assertThat(converter.convertToEntityAttribute(column)).isEqualTo("899518382");
        assertThat(converter.convertToDatabaseColumn(null)).isNull();
        assertThat(converter.convertToEntityAttribute(null)).isNull();
    }

    @Test
    void wrongKeyOrPlaintextRowFailsLoudly() {
        String ct = crypto.encrypt("587518382");
        assertThatThrownBy(() -> new SsnCrypto("a-different-key").decrypt(ct))
                .isInstanceOf(IllegalStateException.class);
        // a pre-V4 plaintext row (never valid base64 IV||ct) fails, not garbles
        assertThatThrownBy(() -> crypto.decrypt("587518382"))
                .isInstanceOf(IllegalStateException.class);
    }
}
