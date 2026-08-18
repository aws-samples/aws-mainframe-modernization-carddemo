package com.carddemo.api;

import com.carddemo.api.dto.ReportDtos.ReportRequest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * CORPT00C PROCESS-ENTER-KEY matrix (plan §4/§8): monthly/yearly derivation
 * incl. month-end boundaries, and the custom-range edit chain with the exact
 * legacy messages, first failure wins. Runs against the static
 * {@link ReportService#derive} with an injected "today" (FUNCTION
 * CURRENT-DATE stand-in).
 */
class ReportServiceDeriveTest {

    private static final LocalDate TODAY = LocalDate.of(2022, 6, 15);

    private static ReportRequest custom(String sm, String sd, String sy,
                                        String em, String ed, String ey) {
        return new ReportRequest("custom", sm, sd, sy, em, ed, ey);
    }

    // ---- monthly / yearly derivation ------------------------------------

    @Test
    void monthlyIsFirstToLastDayOfCurrentMonth() {
        var d = ReportService.derive(new ReportRequest("monthly", null, null, null, null, null, null), TODAY);
        assertThat(d.reportName()).isEqualTo("Monthly");
        assertThat(d.from()).isEqualTo(LocalDate.of(2022, 6, 1));
        assertThat(d.to()).isEqualTo(LocalDate.of(2022, 6, 30));
    }

    @Test
    void monthlyHandlesMonthEndRollovers() {
        // 31-day month, February (non-leap + leap), and December→year end
        var jan = ReportService.derive(new ReportRequest("monthly", null, null, null, null, null, null),
                LocalDate.of(2023, 1, 31));
        assertThat(jan.to()).isEqualTo(LocalDate.of(2023, 1, 31));
        var feb = ReportService.derive(new ReportRequest("monthly", null, null, null, null, null, null),
                LocalDate.of(2023, 2, 14));
        assertThat(feb.to()).isEqualTo(LocalDate.of(2023, 2, 28));
        var leap = ReportService.derive(new ReportRequest("monthly", null, null, null, null, null, null),
                LocalDate.of(2024, 2, 29));
        assertThat(leap.from()).isEqualTo(LocalDate.of(2024, 2, 1));
        assertThat(leap.to()).isEqualTo(LocalDate.of(2024, 2, 29));
        var dec = ReportService.derive(new ReportRequest("monthly", null, null, null, null, null, null),
                LocalDate.of(2022, 12, 31));
        assertThat(dec.to()).isEqualTo(LocalDate.of(2022, 12, 31));
    }

    @Test
    void yearlyIsJanFirstToDecThirtyFirst() {
        var d = ReportService.derive(new ReportRequest("yearly", null, null, null, null, null, null), TODAY);
        assertThat(d.reportName()).isEqualTo("Yearly");
        assertThat(d.from()).isEqualTo(LocalDate.of(2022, 1, 1));
        assertThat(d.to()).isEqualTo(LocalDate.of(2022, 12, 31));
    }

    @Test
    void missingReportTypeFailsWithLegacyMessage() {
        assertThatThrownBy(() -> ReportService.derive(
                new ReportRequest(null, null, null, null, null, null, null), TODAY))
                .isInstanceOf(ValidationException.class)
                .hasMessage("Select a report type to print report...");
        assertThatThrownBy(() -> ReportService.derive(
                new ReportRequest("weekly", null, null, null, null, null, null), TODAY))
                .isInstanceOf(ValidationException.class)
                .hasMessage("Select a report type to print report...");
    }

    // ---- custom range: happy path + NUMVAL-C tolerance -------------------

    @Test
    void customRangeParsesValidDates() {
        var d = ReportService.derive(custom("1", "5", "2022", "12", "31", "2022"), TODAY);
        assertThat(d.reportName()).isEqualTo("Custom");
        assertThat(d.from()).isEqualTo(LocalDate.of(2022, 1, 5));
        assertThat(d.to()).isEqualTo(LocalDate.of(2022, 12, 31));
    }

    @Test
    void numvalCToleratesSpacesCommasAndCurrency() {
        // FUNCTION NUMVAL-C strips spaces / commas / currency signs
        var d = ReportService.derive(custom(" 2 ", "10", " 2,022", "3", "15", "$2022"), TODAY);
        assertThat(d.from()).isEqualTo(LocalDate.of(2022, 2, 10));
        assertThat(d.to()).isEqualTo(LocalDate.of(2022, 3, 15));
    }

    // ---- custom range: the edit chain, first failure wins ----------------

    @ParameterizedTest(name = "[{index}] {6}")
    @CsvSource(delimiter = ';', value = {
            // empty checks, in legacy EVALUATE order
            "'';5;2022;12;31;2022;Start Date - Month can NOT be empty...",
            "1;;2022;12;31;2022;Start Date - Day can NOT be empty...",
            "1;5;'';12;31;2022;Start Date - Year can NOT be empty...",
            "1;5;2022;'';31;2022;End Date - Month can NOT be empty...",
            "1;5;2022;12;;2022;End Date - Day can NOT be empty...",
            "1;5;2022;12;31;'';End Date - Year can NOT be empty...",
            // numeric / range edits, in legacy IF order
            "AB;5;2022;12;31;2022;Start Date - Not a valid Month...",
            "13;5;2022;12;31;2022;Start Date - Not a valid Month...",
            "1;32;2022;12;31;2022;Start Date - Not a valid Day...",
            "1;xx;2022;12;31;2022;Start Date - Not a valid Day...",
            "1;5;20AA;12;31;2022;Start Date - Not a valid Year...",
            "1;5;2022;13;31;2022;End Date - Not a valid Month...",
            "1;5;2022;12;99;2022;End Date - Not a valid Day...",
            "1;5;2022;12;31;Y2K2;End Date - Not a valid Year...",
            // whole-date edit (CSUTLDTC): passes the string edits, bad calendar
            "2;30;2022;12;31;2022;Start Date - Not a valid date...",
            "2;29;2023;12;31;2023;Start Date - Not a valid date...",
            "1;5;2022;4;31;2022;End Date - Not a valid date...",
            // first failure wins: BOTH month fields bad → start message
            "13;32;20AA;13;32;20AA;Start Date - Not a valid Month...",
            // month/day zero pass the string edits but fail the date edit
            "0;5;2022;12;31;2022;Start Date - Not a valid date...",
            "1;0;2022;12;31;2022;Start Date - Not a valid date...",
    })
    void customRangeEditChainReportsFirstLegacyMessage(
            String sm, String sd, String sy, String em, String ed, String ey, String message) {
        assertThatThrownBy(() -> ReportService.derive(custom(sm, sd, sy, em, ed, ey), TODAY))
                .isInstanceOf(ValidationException.class)
                .hasMessage(message);
    }

    @Test
    void leapDayIsAcceptedInLeapYears() {
        var d = ReportService.derive(custom("2", "29", "2024", "3", "1", "2024"), TODAY);
        assertThat(d.from()).isEqualTo(LocalDate.of(2024, 2, 29));
    }
}
