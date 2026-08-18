package com.carddemo.api;

import com.carddemo.auth.AuthService;
import com.carddemo.config.JwtConfig;
import com.carddemo.config.SecurityConfig;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.context.annotation.Import;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.jwt;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Authz matrix (plan §8, §5): 401 when unauthenticated; /api/v1/admin/** and
 * /api/v1/users/** blocked (403) for USER; everything else reachable for both
 * roles; /auth/login public.
 */
@WebMvcTest(controllers = {AuthController.class, AccountController.class,
        CardController.class, TransactionController.class, BatchJobController.class,
        ReportController.class, StatementController.class})
@Import({SecurityConfig.class, JwtConfig.class, ApiExceptionHandler.class})
@TestPropertySource(properties = {
        "carddemo.jwt.secret=test-only-jwt-signing-secret-32-bytes-min!",
        "carddemo.jwt.ttl=15m"
})
class SecurityAuthzTest {

    @Autowired
    MockMvc mvc;

    @MockitoBean
    AuthService authService;
    @MockitoBean
    AccountService accountService;
    @MockitoBean
    CardService cardService;
    @MockitoBean
    TransactionService transactionService;
    @MockitoBean
    BillPaymentService billPaymentService;
    @MockitoBean
    BatchJobService batchJobService;
    @MockitoBean
    ReportService reportService;
    @MockitoBean
    StatementService statementService;

    // ---- unauthenticated → 401 ----------------------------------------

    @Test
    void unauthenticatedRequestsAreRejectedWith401() throws Exception {
        mvc.perform(get("/api/v1/accounts/1")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/cards")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/cards/1234567890123456")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/transactions")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/transactions/0000000000000001")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/admin/jobs/runs")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/users")).andExpect(status().isUnauthorized());
    }

    // ---- login is public -----------------------------------------------

    @Test
    void loginIsPublicAndBadCredentialsReturn401Body() throws Exception {
        when(authService.login(any(), any())).thenThrow(new BadCredentialsException("Invalid credentials"));
        mvc.perform(post("/api/v1/auth/login")
                        .contentType("application/json")
                        .content("{\"userId\":\"USER0001\",\"password\":\"WRONGPWD\"}"))
                .andExpect(status().isUnauthorized())
                .andExpect(jsonPath("$.error").value("invalid_credentials"));
    }

    // ---- USER blocked from admin surfaces -------------------------------

    @Test
    void userRoleIsForbiddenFromAdminAndUsersEndpoints() throws Exception {
        mvc.perform(get("/api/v1/admin/jobs/runs").with(user()))
                .andExpect(status().isForbidden());
        mvc.perform(get("/api/v1/users").with(user()))
                .andExpect(status().isForbidden());
        mvc.perform(get("/api/v1/users/USER0001").with(user()))
                .andExpect(status().isForbidden());
    }

    @Test
    void adminRolePassesAdminAuthz() throws Exception {
        // Phase 4: BatchJobController now serves /admin/jobs — ADMIN gets 200
        when(batchJobService.listRuns()).thenReturn(java.util.List.of());
        mvc.perform(get("/api/v1/admin/jobs/runs").with(admin()))
                .andExpect(status().isOk());
    }

    @Test
    void userRoleIsForbiddenFromRunningJobs() throws Exception {
        mvc.perform(post("/api/v1/admin/jobs/postTransactions/run").with(user()))
                .andExpect(status().isForbidden());
        mvc.perform(post("/api/v1/admin/jobs/interestCalc/run").with(user()))
                .andExpect(status().isForbidden());
        // Phase 5: the statements job is admin-triggered like the others
        mvc.perform(post("/api/v1/admin/jobs/statements/run").with(user()))
                .andExpect(status().isForbidden());
    }

    // ---- Phase 5: reports + statements (any authenticated user) ---------

    @Test
    void reportAndStatementEndpointsRequireAuthentication() throws Exception {
        mvc.perform(get("/api/v1/reports")).andExpect(status().isUnauthorized());
        mvc.perform(post("/api/v1/reports/transaction")
                        .contentType("application/json").content("{}"))
                .andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/reports/1/download")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/accounts/1/statements")).andExpect(status().isUnauthorized());
        mvc.perform(get("/api/v1/statements/1")).andExpect(status().isUnauthorized());
    }

    @Test
    void reportsAreOpenToBothRolesLikeTheLegacyMenu() throws Exception {
        // legacy CORPT00 is a usrtype 'U' menu entry — USER may request/list
        when(reportService.listRuns()).thenReturn(java.util.List.of());
        mvc.perform(get("/api/v1/reports").with(user())).andExpect(status().isOk());
        mvc.perform(get("/api/v1/reports").with(admin())).andExpect(status().isOk());

        when(reportService.requestReport(any(), any())).thenReturn(
                new com.carddemo.api.dto.ReportDtos.ReportRunView(
                        1L, "monthly", java.time.LocalDate.of(2022, 6, 1),
                        java.time.LocalDate.of(2022, 6, 30), "COMPLETED", "USER0001",
                        java.time.LocalDateTime.now(),
                        "Monthly report submitted for printing ..."));
        mvc.perform(post("/api/v1/reports/transaction").with(user())
                        .contentType("application/json")
                        .content("{\"reportType\":\"monthly\"}"))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.message")
                        .value("Monthly report submitted for printing ..."));
    }

    @Test
    void reportDownloadStreamsTextAttachmentForUsers(@org.junit.jupiter.api.io.TempDir
                                                     java.nio.file.Path dir) throws Exception {
        java.nio.file.Path file = dir.resolve("tranrept-7.txt");
        java.nio.file.Files.writeString(file, "REPORT BODY\n");
        when(reportService.reportFile(7L)).thenReturn(file);
        mvc.perform(get("/api/v1/reports/7/download").with(user()))
                .andExpect(status().isOk())
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .header().string("Content-Disposition",
                                "attachment; filename=\"tranrept-7.txt\""))
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .content().contentTypeCompatibleWith("text/plain"))
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .content().string("REPORT BODY\n"));
    }

    @Test
    void statementListAndDownloadAreOpenToUsers(@org.junit.jupiter.api.io.TempDir
                                                java.nio.file.Path dir) throws Exception {
        when(statementService.listForAccount(50L)).thenReturn(java.util.List.of());
        mvc.perform(get("/api/v1/accounts/50/statements").with(user()))
                .andExpect(status().isOk());

        java.nio.file.Path txt = dir.resolve("stmt-1-50.txt");
        java.nio.file.Files.writeString(txt, "STATEMENT\n");
        when(statementService.statementFile(3L, "txt")).thenReturn(txt);
        mvc.perform(get("/api/v1/statements/3").with(user()))
                .andExpect(status().isOk())
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .header().string("Content-Disposition",
                                "attachment; filename=\"stmt-1-50.txt\""))
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .content().contentTypeCompatibleWith("text/plain"));

        java.nio.file.Path html = dir.resolve("stmt-1-50.html");
        java.nio.file.Files.writeString(html, "<!DOCTYPE html>\n");
        when(statementService.statementFile(3L, "html")).thenReturn(html);
        mvc.perform(get("/api/v1/statements/3?format=html").with(user()))
                .andExpect(status().isOk())
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .header().string("Content-Disposition",
                                "inline; filename=\"stmt-1-50.html\""))
                // stored-XSS mitigation: legacy statement HTML is served sandboxed
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .header().string("Content-Security-Policy", "sandbox"))
                .andExpect(org.springframework.test.web.servlet.result.MockMvcResultMatchers
                        .content().contentTypeCompatibleWith("text/html"));
    }

    // ---- read endpoints reachable by both roles -------------------------

    @Test
    void userAndAdminCanReachReadOnlyEndpoints() throws Exception {
        mvc.perform(get("/api/v1/cards").with(user())).andExpect(status().isOk());
        mvc.perform(get("/api/v1/cards").with(admin())).andExpect(status().isOk());
        mvc.perform(get("/api/v1/transactions").with(user())).andExpect(status().isOk());
        mvc.perform(get("/api/v1/accounts/1").with(user())).andExpect(status().isOk());
    }

    private static org.springframework.test.web.servlet.request.RequestPostProcessor user() {
        return jwt().jwt(j -> j.subject("USER0001").claim("role", "USER"))
                .authorities(new org.springframework.security.core.authority.SimpleGrantedAuthority("ROLE_USER"));
    }

    private static org.springframework.test.web.servlet.request.RequestPostProcessor admin() {
        return jwt().jwt(j -> j.subject("ADMIN001").claim("role", "ADMIN"))
                .authorities(new org.springframework.security.core.authority.SimpleGrantedAuthority("ROLE_ADMIN"));
    }
}
