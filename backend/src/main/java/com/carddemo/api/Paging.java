package com.carddemo.api;

import com.carddemo.api.dto.ApiDtos.Page;

import java.util.List;
import java.util.function.Function;

/**
 * Shared keyset-pagination helpers (plan §3 pagination pattern) for the
 * STARTBR/READNEXT-replacement list endpoints (cards, transactions, users).
 */
final class Paging {

    static final int MAX_LIMIT = 100;
    static final int DEFAULT_LIMIT = 20;

    private Paging() {
    }

    static int clamp(Integer limit) {
        if (limit == null || limit < 1) {
            return DEFAULT_LIMIT;
        }
        return Math.min(limit, MAX_LIMIT);
    }

    static String blankToNull(String s) {
        return s == null || s.isBlank() ? null : s.trim();
    }

    /**
     * Builds a page from a query that fetched {@code pageSize + 1} rows: the
     * extra row only signals that a next page exists; the last kept row's key
     * (via {@code cursorOf}) becomes the next cursor.
     */
    static <E, T> Page<T> page(List<E> fetched, int pageSize,
                               Function<E, String> cursorOf, Function<E, T> mapper) {
        boolean hasNext = fetched.size() > pageSize;
        List<E> items = hasNext ? fetched.subList(0, pageSize) : fetched;
        String nextCursor = hasNext ? cursorOf.apply(items.get(items.size() - 1)) : null;
        return new Page<>(items.stream().map(mapper).toList(), nextCursor);
    }
}
