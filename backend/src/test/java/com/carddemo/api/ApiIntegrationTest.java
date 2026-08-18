package com.carddemo.api;

import com.carddemo.etl.EtlLoader;
import com.carddemo.etl.EtlProperties;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Full-stack Phase 1 integration test against a real PostgreSQL seeded by the
 * ETL: real bcrypt login for a DUSRSECJ seed user + admin, JWT round-trip,
 * masking assertions (card list masked / detail unmasked, SSN last-4 only),
 * and keyset pagination (stable ordering, cursor page-forward, no overlap).
 */
@Testcontainers
@SpringBootTest
@AutoConfigureMockMvc
@TestPropertySource(properties = {
        "carddemo.jwt.secret=test-only-jwt-signing-secret-32-bytes-min!",
        "carddemo.jwt.ttl=15m"
})
class ApiIntegrationTest {

    @Container
    @ServiceConnection
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>("postgres:16-alpine");

    private static final AtomicBoolean SEEDED = new AtomicBoolean(false);

    @Autowired
    MockMvc mvc;
    @Autowired
    ObjectMapper json;
    @Autowired
    JdbcTemplate jdbc;
    @Autowired
    com.carddemo.crypto.SsnCrypto ssnCrypto;

    /** ETL seeding happens once, lazily, from the first test (needs the context's JdbcTemplate). */
    void seedOnce() {
        if (SEEDED.compareAndSet(false, true)) {
            EtlProperties props = new EtlProperties(
                    "../etl/fixtures/ascii", "../etl/fixtures/jcl/DUSRSECJ.jcl", true);
            // low-strength bcrypt keeps the test fast; login uses matches() which honors the cost in the hash
            new EtlLoader(jdbc, props, new BCryptPasswordEncoder(4), ssnCrypto).load();
        }
    }

    private String login(String userId, String password) throws Exception {
        MvcResult res = mvc.perform(post("/api/v1/auth/login")
                        .contentType("application/json")
                        .content("{\"userId\":\"" + userId + "\",\"password\":\"" + password + "\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.token").isNotEmpty())
                .andReturn();
        return json.readTree(res.getResponse().getContentAsString()).get("token").asText();
    }

    // ---- auth ------------------------------------------------------------

    @Test
    void loginWithSeededUserAndAdminReturnsRoleClaims() throws Exception {
        seedOnce();
        MvcResult user = mvc.perform(post("/api/v1/auth/login")
                        .contentType("application/json")
                        .content("{\"userId\":\"USER0001\",\"password\":\"PASSWORD\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.role").value("USER"))
                .andExpect(jsonPath("$.userId").value("USER0001"))
                .andReturn();
        assertThat(user.getResponse().getContentAsString()).doesNotContain("password");

        mvc.perform(post("/api/v1/auth/login")
                        .contentType("application/json")
                        .content("{\"userId\":\"ADMIN001\",\"password\":\"PASSWORD\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.role").value("ADMIN"));
    }

    @Test
    void loginWithWrongPasswordIs401() throws Exception {
        seedOnce();
        mvc.perform(post("/api/v1/auth/login")
                        .contentType("application/json")
                        .content("{\"userId\":\"USER0001\",\"password\":\"WRONGPWD\"}"))
                .andExpect(status().isUnauthorized());
    }

    @Test
    void userTokenIsBlockedFromAdminSurfaceButAdminTokenPasses() throws Exception {
        seedOnce();
        String userToken = login("USER0001", "PASSWORD");
        String adminToken = login("ADMIN001", "PASSWORD");
        mvc.perform(get("/api/v1/users").header("Authorization", "Bearer " + userToken))
                .andExpect(status().isForbidden());
        // admin passes authz and reaches the Phase 2 user-list handler
        mvc.perform(get("/api/v1/users").header("Authorization", "Bearer " + adminToken))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items").isArray());
    }

    // ---- masking -----------------------------------------------------------

    @Test
    void cardListMasksNumbersAndDetailReturnsFullNumber() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        MvcResult list = mvc.perform(get("/api/v1/cards?limit=5")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andReturn();
        JsonNode items = json.readTree(list.getResponse().getContentAsString()).get("items");
        assertThat(items).hasSize(5);
        for (JsonNode item : items) {
            assertThat(item.get("cardNumber").isNull()).as("full number absent in list").isTrue();
            assertThat(item.get("maskedCardNumber").asText()).matches("\\*{4}-\\*{4}-\\*{4}-\\d{4}");
        }

        String firstCard = jdbc.queryForObject(
                "SELECT card_number FROM cards ORDER BY card_number LIMIT 1", String.class);
        mvc.perform(get("/api/v1/cards/" + firstCard.trim())
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.cardNumber").value(firstCard.trim()));
    }

    @Test
    void accountViewExposesSsnLast4OnlyAndGovtIdAndEftAccount() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        Long accountId = jdbc.queryForObject(
                "SELECT account_id FROM accounts ORDER BY account_id LIMIT 1", Long.class);
        String storedSsn = jdbc.queryForObject("""
                SELECT c.ssn FROM customers c
                JOIN accounts a ON a.customer_id = c.customer_id
                WHERE a.account_id = ?""", String.class, accountId);
        // at rest the column holds AES-GCM ciphertext, not the 9-digit SSN
        assertThat(storedSsn).doesNotMatch("\\d{9}");
        String fullSsn = ssnCrypto.decrypt(storedSsn);
        assertThat(fullSsn).hasSize(9).containsOnlyDigits();

        MvcResult res = mvc.perform(get("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.customer.ssnLast4").value(fullSsn.substring(fullSsn.length() - 4)))
                .andExpect(jsonPath("$.customer.govtIssuedId").isNotEmpty())
                .andExpect(jsonPath("$.customer.eftAccountId").isNotEmpty())
                .andReturn();

        String body = res.getResponse().getContentAsString();
        assertThat(body).as("full SSN must never appear").doesNotContain(fullSsn);
        // cards embedded in the account view are masked too
        JsonNode cards = json.readTree(body).get("cards");
        for (JsonNode card : cards) {
            assertThat(card.get("cardNumber").isNull()).isTrue();
            assertThat(card.get("maskedCardNumber").asText()).startsWith("****-");
        }
    }

    @Test
    void transactionResponsesMaskCardNumbers() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        MvcResult res = mvc.perform(get("/api/v1/transactions?limit=5")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andReturn();
        JsonNode items = json.readTree(res.getResponse().getContentAsString()).get("items");
        assertThat(items).isNotEmpty();
        for (JsonNode item : items) {
            assertThat(item.get("maskedCardNumber").asText()).matches("\\*{4}-\\*{4}-\\*{4}-\\d{4}");
            assertThat(item.has("cardNumber")).as("no unmasked field in transaction views").isFalse();
        }
    }

    // ---- keyset pagination ---------------------------------------------------

    @Test
    void cardKeysetPaginationPagesForwardWithStableOrderingAndNoOverlap() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");

        MvcResult p1 = mvc.perform(get("/api/v1/cards?limit=20")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode page1 = json.readTree(p1.getResponse().getContentAsString());
        assertThat(page1.get("items")).hasSize(20);
        String cursor = page1.get("nextCursor").asText();
        assertThat(cursor).isNotBlank();

        MvcResult p2 = mvc.perform(get("/api/v1/cards?limit=20&cursor=" + cursor)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode page2 = json.readTree(p2.getResponse().getContentAsString());
        assertThat(page2.get("items")).hasSize(20);

        MvcResult p3 = mvc.perform(get("/api/v1/cards?limit=20&cursor=" + page2.get("nextCursor").asText())
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode page3 = json.readTree(p3.getResponse().getContentAsString());
        // 50 cards total → 20 + 20 + 10, last page has no cursor
        assertThat(page3.get("items")).hasSize(10);
        assertThat(page3.get("nextCursor").isNull()).isTrue();

        // stable ordering + no overlap across pages (masked last-4 + account id as identity)
        java.util.List<String> seen = new java.util.ArrayList<>();
        for (JsonNode page : new JsonNode[]{page1, page2, page3}) {
            for (JsonNode item : page.get("items")) {
                seen.add(item.get("maskedCardNumber").asText() + "@" + item.get("accountId").asText());
            }
        }
        assertThat(seen).hasSize(50).doesNotHaveDuplicates();
    }

    @Test
    void cardListFiltersByAccountId() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        Long accountId = jdbc.queryForObject(
                "SELECT account_id FROM cards GROUP BY account_id ORDER BY account_id LIMIT 1", Long.class);
        Long expected = jdbc.queryForObject(
                "SELECT count(*) FROM cards WHERE account_id = ?", Long.class, accountId);
        MvcResult res = mvc.perform(get("/api/v1/cards?accountId=" + accountId + "&limit=50")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode items = json.readTree(res.getResponse().getContentAsString()).get("items");
        assertThat((long) items.size()).isEqualTo(expected);
        for (JsonNode item : items) {
            assertThat(item.get("accountId").asLong()).isEqualTo(accountId);
        }
    }

    @Test
    void transactionKeysetPaginationAndFromIdSeek() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");

        MvcResult p1 = mvc.perform(get("/api/v1/transactions?limit=100")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode page1 = json.readTree(p1.getResponse().getContentAsString());
        assertThat(page1.get("items")).hasSize(100);
        String cursor = page1.get("nextCursor").asText();

        MvcResult p2 = mvc.perform(get("/api/v1/transactions?limit=100&cursor=" + cursor)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode page2 = json.readTree(p2.getResponse().getContentAsString());
        String firstOfPage2 = page2.get("items").get(0).get("transactionId").asText();
        assertThat(firstOfPage2.compareTo(cursor)).as("strictly after cursor").isGreaterThan(0);

        // fromId seek (COTRN00C tran-id seek): inclusive start
        MvcResult seek = mvc.perform(get("/api/v1/transactions?limit=5&fromId=" + firstOfPage2)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        JsonNode seekPage = json.readTree(seek.getResponse().getContentAsString());
        assertThat(seekPage.get("items").get(0).get("transactionId").asText()).isEqualTo(firstOfPage2);
    }

    @Test
    void transactionDetailAndNotFound() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        String tranId = jdbc.queryForObject(
                "SELECT transaction_id FROM transactions ORDER BY transaction_id LIMIT 1", String.class);
        mvc.perform(get("/api/v1/transactions/" + tranId.trim())
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.transactionId").value(tranId.trim()))
                .andExpect(jsonPath("$.accountId").isNumber());
        mvc.perform(get("/api/v1/transactions/9999999999999999")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isNotFound());
        mvc.perform(get("/api/v1/accounts/99999999999")
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isNotFound());
    }

    // ======================================================================
    // Phase 2 — updates/CRUD (COACTUPC, COCRDUPC, COUSR00C-03C)
    // ======================================================================

    /** Valid COACTUPC payload for the given fetched account view. */
    private com.fasterxml.jackson.databind.node.ObjectNode accountPayload(JsonNode view) {
        com.fasterxml.jackson.databind.node.ObjectNode p = json.createObjectNode();
        p.put("accountVersion", view.get("version").asLong());
        p.put("customerVersion", view.get("customer").get("version").asLong());
        p.put("activeStatus", "Y");
        p.put("creditLimit", "6130.00");
        p.put("cashCreditLimit", "1580.00");
        p.put("currBal", "5448.00");
        p.put("openDate", "2013-06-19");
        p.put("expirationDate", "2027-08-11");
        p.put("reissueDate", "2024-08-11");
        p.put("currCycCredit", "1912.40");
        p.put("currCycDebit", "482.15");
        p.put("groupId", "A000000000");
        p.put("firstName", "Enrico");
        p.put("middleName", "April");
        p.put("lastName", "Rosenbaum");
        p.putNull("ssn"); // keep stored
        p.put("dob", "1961-10-08");
        p.put("ficoScore", "711");
        p.put("addrLine1", "4917 Myrna Flats");
        p.put("addrLine2", "Apt. 453");
        p.put("city", "West Bernita");
        p.put("stateCd", "TX");
        p.put("zip", "75001");
        p.put("countryCd", "USA");
        p.put("phone1", "(214) 706-9510");
        p.put("phone2", "");
        p.put("govtIssuedId", "5875183820");
        p.put("eftAccountId", "0000005062");
        p.put("priCardHolder", "Y");
        return p;
    }

    private JsonNode fetchAccount(String token, long accountId) throws Exception {
        MvcResult res = mvc.perform(get("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        return json.readTree(res.getResponse().getContentAsString());
    }

    // ---- PUT /accounts/{id} — COACTUPC port -------------------------------

    @Test
    void accountUpdateWritesAccountAndCustomerInOneTransaction() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        Long accountId = jdbc.queryForObject(
                "SELECT account_id FROM accounts ORDER BY account_id DESC LIMIT 1", Long.class);
        JsonNode view = fetchAccount(token, accountId);
        String storedSsnLast4 = view.get("customer").get("ssnLast4").asText();

        mvc.perform(put("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(accountPayload(view).toString()))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.creditLimit").value(6130.00))
                .andExpect(jsonPath("$.customer.firstName").value("Enrico"))
                .andExpect(jsonPath("$.customer.addrStateCd").value("TX"))
                // phone normalized to the legacy (AAA)BBB-CCCC layout
                .andExpect(jsonPath("$.customer.phoneNum1").value("(214)706-9510"))
                // null ssn in payload keeps the stored value
                .andExpect(jsonPath("$.customer.ssnLast4").value(storedSsnLast4));

        // both rows written
        assertThat(jdbc.queryForObject(
                "SELECT credit_limit FROM accounts WHERE account_id = ?",
                java.math.BigDecimal.class, accountId))
                .isEqualByComparingTo("6130.00");
        assertThat(jdbc.queryForObject("""
                SELECT c.first_name FROM customers c
                JOIN accounts a ON a.customer_id = c.customer_id
                WHERE a.account_id = ?""", String.class, accountId).trim())
                .isEqualTo("Enrico");
    }

    @Test
    void accountUpdateRejectsInvalidFieldsWithFieldErrors() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        Long accountId = jdbc.queryForObject(
                "SELECT account_id FROM accounts ORDER BY account_id DESC LIMIT 1", Long.class);
        JsonNode view = fetchAccount(token, accountId);
        com.fasterxml.jackson.databind.node.ObjectNode bad = accountPayload(view);
        bad.put("ficoScore", "299");        // 1275-EDIT-FICO-SCORE fail
        bad.put("stateCd", "XX");           // 1270-EDIT-US-STATE-CD fail

        MvcResult res = mvc.perform(put("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(bad.toString()))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.error").value("validation"))
                .andReturn();
        JsonNode errors = json.readTree(res.getResponse().getContentAsString()).get("fieldErrors");
        java.util.List<String> messages = new java.util.ArrayList<>();
        errors.forEach(e -> messages.add(e.get("message").asText()));
        assertThat(messages).contains(
                "FICO Score: should be between 300 and 850",
                "State: is not a valid state code");
    }

    @Test
    void accountUpdateWithStaleVersionIs409ChangedBySomeoneElse() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        Long accountId = jdbc.queryForObject(
                "SELECT account_id FROM accounts ORDER BY account_id DESC LIMIT 1", Long.class);
        JsonNode view = fetchAccount(token, accountId);
        com.fasterxml.jackson.databind.node.ObjectNode stale = accountPayload(view);
        stale.put("accountVersion", view.get("version").asLong() + 41); // someone else saved since
        stale.put("groupId", "CHANGED01");

        mvc.perform(put("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(stale.toString()))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.error").value("conflict"))
                .andExpect(jsonPath("$.message").value(
                        "Record changed by some one else. Please review"));
    }

    @Test
    void accountUpdateNoChangeIsSuccessWithoutWrite() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        Long accountId = jdbc.queryForObject(
                "SELECT account_id FROM accounts ORDER BY account_id DESC LIMIT 1", Long.class);
        // first save establishes known values
        JsonNode view = fetchAccount(token, accountId);
        mvc.perform(put("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(accountPayload(view).toString()))
                .andExpect(status().isOk());
        long versionAfterWrite = fetchAccount(token, accountId).get("version").asLong();

        // identical resubmit → 1205-COMPARE-OLD-NEW no-op: 200, version unchanged
        JsonNode fresh = fetchAccount(token, accountId);
        mvc.perform(put("/api/v1/accounts/" + accountId)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(accountPayload(fresh).toString()))
                .andExpect(status().isOk());
        assertThat(fetchAccount(token, accountId).get("version").asLong())
                .as("no-op update must not write (NO-CHANGES-DETECTED)")
                .isEqualTo(versionAfterWrite);
    }

    // ---- PUT /cards/{cardNumber} — COCRDUPC port ---------------------------

    @Test
    void cardUpdateValidatesAndWrites() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        String cardNumber = jdbc.queryForObject(
                "SELECT card_number FROM cards ORDER BY card_number DESC LIMIT 1", String.class).trim();
        MvcResult res = mvc.perform(get("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk()).andReturn();
        long version = json.readTree(res.getResponse().getContentAsString()).get("version").asLong();

        // 1230-EDIT-NAME fail
        mvc.perform(put("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"embossedName\":\"JOHN D03\","
                                + "\"expiryMonth\":\"12\",\"expiryYear\":\"2028\",\"activeStatus\":\"Y\"}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("Card name can only contain alphabets and spaces"));

        // 1250-EDIT-EXPIRY-MON fail
        mvc.perform(put("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"embossedName\":\"JOHN DOE\","
                                + "\"expiryMonth\":\"13\",\"expiryYear\":\"2028\",\"activeStatus\":\"Y\"}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("Card expiry month must be between 1 and 12"));

        // 1260-EDIT-EXPIRY-YEAR fail (VALID-YEAR 1950 THRU 2099)
        mvc.perform(put("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"embossedName\":\"JOHN DOE\","
                                + "\"expiryMonth\":\"12\",\"expiryYear\":\"1949\",\"activeStatus\":\"Y\"}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message").value("Invalid card expiry year"));

        // 1240-EDIT-CARDSTATUS fail
        mvc.perform(put("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"embossedName\":\"JOHN DOE\","
                                + "\"expiryMonth\":\"12\",\"expiryYear\":\"2028\",\"activeStatus\":\"X\"}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[0].message")
                        .value("Card Active Status must be Y or N"));

        // valid write
        mvc.perform(put("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"embossedName\":\"JOHN DOE\","
                                + "\"expiryMonth\":\"12\",\"expiryYear\":\"2028\",\"activeStatus\":\"N\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.embossedName").value("JOHN DOE"))
                .andExpect(jsonPath("$.activeStatus").value("N"));
        assertThat(jdbc.queryForObject(
                "SELECT embossed_name FROM cards WHERE card_number = ?", String.class, cardNumber).trim())
                .isEqualTo("JOHN DOE");

        // stale version → 409 with the legacy message (9300-CHECK-CHANGE-IN-REC)
        mvc.perform(put("/api/v1/cards/" + cardNumber)
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"embossedName\":\"JANE DOE\","
                                + "\"expiryMonth\":\"11\",\"expiryYear\":\"2029\",\"activeStatus\":\"Y\"}"))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value(
                        "Record changed by some one else. Please review"));
    }

    // ---- /users — COUSR00C-03C admin CRUD e2e -------------------------------

    @Test
    void adminUserCrudEndToEnd() throws Exception {
        seedOnce();
        String admin = login("ADMIN001", "PASSWORD");

        // list (COUSR00C): seeded users present, keyset page shape
        mvc.perform(get("/api/v1/users?limit=5").header("Authorization", "Bearer " + admin))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.items.length()").value(5))
                .andExpect(jsonPath("$.nextCursor").isNotEmpty())
                // password material never returned (plan §5)
                .andExpect(jsonPath("$.items[0].passwordHash").doesNotExist());

        // create (COUSR01C)
        mvc.perform(post("/api/v1/users").header("Authorization", "Bearer " + admin)
                        .contentType("application/json")
                        .content("{\"userId\":\"TESTUSR1\",\"firstName\":\"Test\","
                                + "\"lastName\":\"User\",\"password\":\"SECRET01\",\"userType\":\"U\"}"))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.userId").value("TESTUSR1"))
                .andExpect(jsonPath("$.userType").value("U"));

        // duplicate id → 409 "User ID already exist..." (COUSR01C DUPREC)
        mvc.perform(post("/api/v1/users").header("Authorization", "Bearer " + admin)
                        .contentType("application/json")
                        .content("{\"userId\":\"TESTUSR1\",\"firstName\":\"Dup\","
                                + "\"lastName\":\"User\",\"password\":\"SECRET01\",\"userType\":\"U\"}"))
                .andExpect(status().isConflict())
                .andExpect(jsonPath("$.message").value("User ID already exist..."));

        // mandatory-field edits (COUSR01C "... can NOT be empty...")
        mvc.perform(post("/api/v1/users").header("Authorization", "Bearer " + admin)
                        .contentType("application/json")
                        .content("{\"userId\":\"\",\"firstName\":\"\",\"lastName\":\"\","
                                + "\"password\":\"\",\"userType\":\"\"}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.fieldErrors[?(@.field=='userId')].message")
                        .value("User ID can NOT be empty..."))
                .andExpect(jsonPath("$.fieldErrors[?(@.field=='userType')].message")
                        .value("User Type can NOT be empty..."));

        // new user can sign in with the bcrypt-hashed initial password
        login("TESTUSR1", "SECRET01");

        // get (COUSR02C read)
        mvc.perform(get("/api/v1/users/TESTUSR1").header("Authorization", "Bearer " + admin))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("Test"));

        // update incl. password + promote to admin (COUSR02C)
        MvcResult got = mvc.perform(get("/api/v1/users/TESTUSR1")
                        .header("Authorization", "Bearer " + admin))
                .andExpect(status().isOk()).andReturn();
        long version = json.readTree(got.getResponse().getContentAsString()).get("version").asLong();
        mvc.perform(put("/api/v1/users/TESTUSR1").header("Authorization", "Bearer " + admin)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"firstName\":\"Renamed\","
                                + "\"lastName\":\"User\",\"password\":\"NEWPASS1\",\"userType\":\"A\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.firstName").value("Renamed"))
                .andExpect(jsonPath("$.userType").value("A"));
        // old password rejected, new one works and carries the ADMIN role
        mvc.perform(post("/api/v1/auth/login").contentType("application/json")
                        .content("{\"userId\":\"TESTUSR1\",\"password\":\"SECRET01\"}"))
                .andExpect(status().isUnauthorized());
        mvc.perform(post("/api/v1/auth/login").contentType("application/json")
                        .content("{\"userId\":\"TESTUSR1\",\"password\":\"NEWPASS1\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.role").value("ADMIN"));

        // stale version → 409
        mvc.perform(put("/api/v1/users/TESTUSR1").header("Authorization", "Bearer " + admin)
                        .contentType("application/json")
                        .content("{\"version\":" + version + ",\"firstName\":\"Race\","
                                + "\"lastName\":\"Loser\",\"userType\":\"U\"}"))
                .andExpect(status().isConflict());

        // delete (COUSR03C) then 404
        mvc.perform(delete("/api/v1/users/TESTUSR1").header("Authorization", "Bearer " + admin))
                .andExpect(status().isNoContent());
        mvc.perform(get("/api/v1/users/TESTUSR1").header("Authorization", "Bearer " + admin))
                .andExpect(status().isNotFound());
        mvc.perform(delete("/api/v1/users/TESTUSR1").header("Authorization", "Bearer " + admin))
                .andExpect(status().isNotFound());
    }

    @Test
    void userEndpointsBlockedForUserRole() throws Exception {
        seedOnce();
        String userToken = login("USER0001", "PASSWORD");
        mvc.perform(post("/api/v1/users").header("Authorization", "Bearer " + userToken)
                        .contentType("application/json")
                        .content("{\"userId\":\"HACKER01\",\"firstName\":\"X\",\"lastName\":\"Y\","
                                + "\"password\":\"PWD\",\"userType\":\"A\"}"))
                .andExpect(status().isForbidden());
        mvc.perform(delete("/api/v1/users/ADMIN001").header("Authorization", "Bearer " + userToken))
                .andExpect(status().isForbidden());
    }

    // ======================================================================
    // Phase 3 — add transaction + bill payment (COTRN02C, COBIL00C)
    // ======================================================================

    /** An account that has a card and a positive balance (not the account the
     *  COACTUPC update test mutates, to stay order-independent). */
    private long billableAccountId() {
        return jdbc.queryForObject("""
                SELECT a.account_id FROM accounts a
                JOIN cards c ON c.account_id = a.account_id
                WHERE a.curr_bal > 0
                  AND a.account_id <> (SELECT max(account_id) FROM accounts)
                ORDER BY a.account_id LIMIT 1""", Long.class);
    }

    /** Valid COTRN02C payload keyed by account id. */
    private com.fasterxml.jackson.databind.node.ObjectNode txnPayload(long accountId) {
        com.fasterxml.jackson.databind.node.ObjectNode p = json.createObjectNode();
        p.put("accountId", String.valueOf(accountId));
        p.put("typeCd", "01");
        p.put("categoryCd", "0001");
        p.put("source", "POS TERM");
        p.put("description", "PARITY TEST PURCHASE");
        p.put("amount", "+00000075.00");
        p.put("origDate", "2025-04-15");
        p.put("procDate", "2025-04-16");
        p.put("merchantId", "123456789");
        p.put("merchantName", "TEST MERCHANT");
        p.put("merchantCity", "DALLAS");
        p.put("merchantZip", "75001");
        return p;
    }

    // ---- POST /accounts/{id}/bill-payment — COBIL00C parity ---------------

    @Test
    void billPaymentZeroesBalanceAndWritesExactLegacyShapedTransaction() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        long accountId = billableAccountId();
        java.math.BigDecimal priorBalance = jdbc.queryForObject(
                "SELECT curr_bal FROM accounts WHERE account_id = ?",
                java.math.BigDecimal.class, accountId);
        assertThat(priorBalance).isPositive();

        MvcResult res = mvc.perform(post("/api/v1/accounts/" + accountId + "/bill-payment")
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"confirm\":true}"))
                .andExpect(status().isCreated())
                // exact COBIL00C 225-229 literals
                .andExpect(jsonPath("$.typeCd").value("02"))
                .andExpect(jsonPath("$.categoryCd").value(2))
                .andExpect(jsonPath("$.source").value("POS TERM"))
                .andExpect(jsonPath("$.description").value("BILL PAYMENT - ONLINE"))
                .andExpect(jsonPath("$.merchantId").value(999999999))
                .andExpect(jsonPath("$.merchantName").value("BILL PAYMENT"))
                .andExpect(jsonPath("$.merchantCity").value("N/A"))
                .andExpect(jsonPath("$.merchantZip").value("N/A"))
                .andExpect(jsonPath("$.accountId").value(accountId))
                .andReturn();
        JsonNode view = json.readTree(res.getResponse().getContentAsString());
        // amount = the full prior balance (COMPUTE TRAN-AMT = ACCT-CURR-BAL)
        assertThat(new java.math.BigDecimal(view.get("amount").asText()))
                .isEqualByComparingTo(priorBalance);
        // tran id: 16-digit zero-padded sequence rendering (plan R2)
        String tranId = view.get("transactionId").asText();
        assertThat(tranId).matches("\\d{16}");
        assertThat(view.get("maskedCardNumber").asText()).matches("\\*{4}-\\*{4}-\\*{4}-\\d{4}");

        // balance zeroed (COBIL00C 234-235), cycle fields untouched by design
        assertThat(jdbc.queryForObject(
                "SELECT curr_bal FROM accounts WHERE account_id = ?",
                java.math.BigDecimal.class, accountId))
                .isEqualByComparingTo("0.00");

        // persisted row is legacy-shaped incl. city/zip 'N/A' (§7 exit criteria)
        java.util.Map<String, Object> row = jdbc.queryForMap(
                "SELECT * FROM transactions WHERE transaction_id = ?", tranId);
        assertThat(row.get("type_cd").toString().trim()).isEqualTo("02");
        assertThat(((Number) row.get("category_cd")).intValue()).isEqualTo(2);
        assertThat(row.get("source").toString().trim()).isEqualTo("POS TERM");
        assertThat(row.get("description").toString().trim()).isEqualTo("BILL PAYMENT - ONLINE");
        assertThat((java.math.BigDecimal) row.get("amount")).isEqualByComparingTo(priorBalance);
        assertThat(((Number) row.get("merchant_id")).longValue()).isEqualTo(999_999_999L);
        assertThat(row.get("merchant_name").toString().trim()).isEqualTo("BILL PAYMENT");
        assertThat(row.get("merchant_city").toString().trim()).isEqualTo("N/A");
        assertThat(row.get("merchant_zip").toString().trim()).isEqualTo("N/A");
        // orig_ts == proc_ts (GET-CURRENT-TIMESTAMP used for both)
        assertThat(row.get("orig_ts")).isEqualTo(row.get("proc_ts"));

        // paying again on the now-zero balance → the legacy message (COBIL00C:201)
        mvc.perform(post("/api/v1/accounts/" + accountId + "/bill-payment")
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"confirm\":true}"))
                .andExpect(status().isBadRequest())
                .andExpect(jsonPath("$.error").value("validation"))
                .andExpect(jsonPath("$.fieldErrors[0].message").value("You have nothing to pay..."));
    }

    @Test
    void billPaymentRequiresConfirmAndExistingAccount() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        long accountId = billableAccountId();
        // confirm missing/false → COBIL00C:187 message, nothing written
        java.math.BigDecimal before = jdbc.queryForObject(
                "SELECT curr_bal FROM accounts WHERE account_id = ?",
                java.math.BigDecimal.class, accountId);
        for (String body : new String[]{"{}", "{\"confirm\":false}"}) {
            mvc.perform(post("/api/v1/accounts/" + accountId + "/bill-payment")
                            .header("Authorization", "Bearer " + token)
                            .contentType("application/json")
                            .content(body))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.fieldErrors[0].field").value("confirm"))
                    .andExpect(jsonPath("$.fieldErrors[0].message")
                            .value("Invalid value. Valid values are (Y/N)..."));
        }
        assertThat(jdbc.queryForObject(
                "SELECT curr_bal FROM accounts WHERE account_id = ?",
                java.math.BigDecimal.class, accountId))
                .isEqualByComparingTo(before);
        // unknown account → 404 with the legacy message
        mvc.perform(post("/api/v1/accounts/99999999999/bill-payment")
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content("{\"confirm\":true}"))
                .andExpect(status().isNotFound());
    }

    // ---- POST /transactions — COTRN02C parity ------------------------------

    @Test
    void addTransactionPersistsWithSequentialZeroPaddedIdsAndUserEnteredTimestamps() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        long accountId = billableAccountId();

        MvcResult first = mvc.perform(post("/api/v1/transactions")
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(txnPayload(accountId).toString()))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.typeCd").value("01"))
                .andExpect(jsonPath("$.categoryCd").value(1))
                .andExpect(jsonPath("$.source").value("POS TERM"))
                .andExpect(jsonPath("$.amount").value(75.00))
                .andExpect(jsonPath("$.accountId").value(accountId))
                // 464-465: ORIG-TS/PROC-TS from the USER-ENTERED dates, not now()
                .andExpect(jsonPath("$.origTs").value("2025-04-15T00:00:00"))
                .andExpect(jsonPath("$.procTs").value("2025-04-16T00:00:00"))
                .andReturn();
        String id1 = json.readTree(first.getResponse().getContentAsString())
                .get("transactionId").asText();
        assertThat(id1).matches("\\d{16}");

        MvcResult second = mvc.perform(post("/api/v1/transactions")
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(txnPayload(accountId).toString()))
                .andExpect(status().isCreated()).andReturn();
        String id2 = json.readTree(second.getResponse().getContentAsString())
                .get("transactionId").asText();
        // sequence-backed ids: strictly increasing, consecutive here (plan R2)
        assertThat(Long.parseLong(id2)).isEqualTo(Long.parseLong(id1) + 1);

        // detail round-trip (COTRN01C view of the new row)
        mvc.perform(get("/api/v1/transactions/" + id1)
                        .header("Authorization", "Bearer " + token))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.description").value("PARITY TEST PURCHASE"));
    }

    @Test
    void addTransactionResolvesAccountFromCardNumber() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        long accountId = billableAccountId();
        String cardNumber = jdbc.queryForObject(
                "SELECT card_number FROM cards WHERE account_id = ? ORDER BY card_number LIMIT 1",
                String.class, accountId).trim();
        com.fasterxml.jackson.databind.node.ObjectNode p = txnPayload(accountId);
        p.remove("accountId");
        p.put("cardNumber", cardNumber);
        mvc.perform(post("/api/v1/transactions")
                        .header("Authorization", "Bearer " + token)
                        .contentType("application/json")
                        .content(p.toString()))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.accountId").value(accountId));
    }

    @Test
    void addTransactionValidationMatrixFailsFastWithLegacyMessages() throws Exception {
        seedOnce();
        String token = login("USER0001", "PASSWORD");
        long accountId = billableAccountId();

        record Case(java.util.function.Consumer<com.fasterxml.jackson.databind.node.ObjectNode> mutate,
                    String field, String message) {
        }
        java.util.List<Case> cases = java.util.List.of(
                // VALIDATE-INPUT-KEY-FIELDS (195-230)
                new Case(p -> p.remove("accountId"),
                        "accountId", "Account or Card Number must be entered..."),
                new Case(p -> p.put("accountId", "ABC"),
                        "accountId", "Account ID must be Numeric..."),
                new Case(p -> p.put("accountId", "99999999999"),
                        "accountId", "Account ID NOT found..."),
                new Case(p -> { p.remove("accountId"); p.put("cardNumber", "12AB"); },
                        "cardNumber", "Card Number must be Numeric..."),
                new Case(p -> { p.remove("accountId"); p.put("cardNumber", "9999999999999999"); },
                        "cardNumber", "Card Number NOT found..."),
                // non-empty edits (252-320)
                new Case(p -> p.put("typeCd", ""), "typeCd", "Type CD can NOT be empty..."),
                new Case(p -> p.put("categoryCd", " "), "categoryCd", "Category CD can NOT be empty..."),
                new Case(p -> p.remove("source"), "source", "Source can NOT be empty..."),
                new Case(p -> p.remove("description"), "description", "Description can NOT be empty..."),
                new Case(p -> p.remove("amount"), "amount", "Amount can NOT be empty..."),
                new Case(p -> p.remove("origDate"), "origDate", "Orig Date can NOT be empty..."),
                new Case(p -> p.remove("procDate"), "procDate", "Proc Date can NOT be empty..."),
                new Case(p -> p.remove("merchantId"), "merchantId", "Merchant ID can NOT be empty..."),
                new Case(p -> p.remove("merchantName"), "merchantName", "Merchant Name can NOT be empty..."),
                new Case(p -> p.remove("merchantCity"), "merchantCity", "Merchant City can NOT be empty..."),
                new Case(p -> p.remove("merchantZip"), "merchantZip", "Merchant Zip can NOT be empty..."),
                // numeric edits (322-337)
                new Case(p -> p.put("typeCd", "XX"), "typeCd", "Type CD must be Numeric..."),
                new Case(p -> p.put("categoryCd", "12A4"), "categoryCd", "Category CD must be Numeric..."),
                // amount format (339-351)
                new Case(p -> p.put("amount", "75.00"),
                        "amount", "Amount should be in format -99999999.99"),
                new Case(p -> p.put("amount", "+75.000"),
                        "amount", "Amount should be in format -99999999.99"),
                // positional date edits (353-381)
                new Case(p -> p.put("origDate", "04/15/2025"),
                        "origDate", "Orig Date should be in format YYYY-MM-DD"),
                new Case(p -> p.put("procDate", "20250416"),
                        "procDate", "Proc Date should be in format YYYY-MM-DD"),
                // CSUTLDTC edits (389-427)
                new Case(p -> p.put("origDate", "2025-02-30"),
                        "origDate", "Orig Date - Not a valid date..."),
                new Case(p -> p.put("procDate", "2025-13-01"),
                        "procDate", "Proc Date - Not a valid date..."),
                // merchant id numeric edit (430-436)
                new Case(p -> p.put("merchantId", "12AB56789"),
                        "merchantId", "Merchant ID must be Numeric..."));

        long before = jdbc.queryForObject("SELECT count(*) FROM transactions", Long.class);
        for (Case c : cases) {
            com.fasterxml.jackson.databind.node.ObjectNode p = txnPayload(accountId);
            c.mutate().accept(p);
            mvc.perform(post("/api/v1/transactions")
                            .header("Authorization", "Bearer " + token)
                            .contentType("application/json")
                            .content(p.toString()))
                    .andExpect(status().isBadRequest())
                    .andExpect(jsonPath("$.error").value("validation"))
                    // legacy fail-fast: exactly ONE field error, the first failed edit
                    .andExpect(jsonPath("$.fieldErrors.length()").value(1))
                    .andExpect(jsonPath("$.fieldErrors[0].field").value(c.field()))
                    .andExpect(jsonPath("$.fieldErrors[0].message").value(c.message()));
        }
        // nothing persisted by any rejected payload
        assertThat(jdbc.queryForObject("SELECT count(*) FROM transactions", Long.class))
                .isEqualTo(before);
    }
}
