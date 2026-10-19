package com.client.security;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import java.util.regex.Pattern;

/**
 * PCI-DSS compliant credit card PAN masking utility.
 * Retains the first 6 digits (BIN/IIN) and last 4 digits, masking intermediate digits.
 * Example: '4111222233334444' -> '411122******4444'
 */
public class PanMask {
    private static final Pattern CARD_PATTERN = Pattern.compile("\\b(\\d{6})\\d{4,9}(\\d{4})\\b");

    public static String mask(String pan) {
        if (pan == null || pan.length() < 12) {
            return pan;
        }
        String clean = pan.replaceAll("\\s+", "");
        if (clean.length() == 16) {
            return clean.substring(0, 6) + "******" + clean.substring(12);
        }
        return CARD_PATTERN.matcher(clean).replaceAll("$1******$2");
    }

    public static class Serializer extends JsonSerializer<String> {
        @Override
        public void serialize(String value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
            gen.writeString(mask(value));
        }
    }
}
