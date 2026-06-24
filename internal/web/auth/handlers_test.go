package auth

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/cookiejar"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/audit"
	authpkg "github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/auth"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/repo"
)

type testServer struct {
	srv      *httptest.Server
	client   *http.Client
	store    *authpkg.MemorySessionStore
	users    *repo.InMemoryUserSec
	sink     *audit.MemorySink
	handlers *Handlers
	svc      *authpkg.Service
}

func newTestServer(t *testing.T) *testServer {
	t.Helper()
	users := repo.NewInMemoryUserSec()
	if err := authpkg.SeedDefaultUsers(context.Background(), users, 4); err != nil {
		t.Fatalf("seed: %v", err)
	}
	sink := audit.NewMemorySink()
	store := authpkg.NewMemorySessionStore()
	svc := authpkg.NewService(users, store, sink)

	h := NewHandlers(svc)
	h.CookieOptions.Secure = false // httptest is plain HTTP

	mux := http.NewServeMux()
	mux.HandleFunc("GET /login", h.GetLogin)
	mux.HandleFunc("POST /login", h.PostLogin)
	mux.HandleFunc("POST /logout", h.PostLogout)

	adminMux := http.NewServeMux()
	adminMux.HandleFunc("GET /admin/users", h.ListUsers)
	adminMux.HandleFunc("POST /admin/users", h.CreateUser)
	adminMux.HandleFunc("GET /admin/users/{id}", h.GetUser)
	adminMux.HandleFunc("POST /admin/users/{id}", h.UpdateUser)
	adminMux.HandleFunc("POST /admin/users/{id}/delete", h.DeleteUser)

	mux.Handle("/admin/", authpkg.RequireAdmin(sink, h.LoginPath)(adminMux))

	loadSession := authpkg.LoadSession(store)
	csrf := authpkg.CSRFProtect()

	srv := httptest.NewServer(loadSession(csrf(mux)))
	t.Cleanup(srv.Close)

	jar, _ := cookiejar.New(nil)
	return &testServer{
		srv:      srv,
		client:   &http.Client{Jar: jar, CheckRedirect: func(req *http.Request, via []*http.Request) error { return http.ErrUseLastResponse }},
		store:    store,
		users:    users,
		sink:     sink,
		handlers: h,
		svc:      svc,
	}
}

func (ts *testServer) get(t *testing.T, path string) *http.Response {
	t.Helper()
	req, _ := http.NewRequest(http.MethodGet, ts.srv.URL+path, nil)
	req.Header.Set("Accept", "text/html")
	resp, err := ts.client.Do(req)
	if err != nil {
		t.Fatalf("GET %s: %v", path, err)
	}
	return resp
}

func (ts *testServer) getJSON(t *testing.T, path string) *http.Response {
	t.Helper()
	req, _ := http.NewRequest(http.MethodGet, ts.srv.URL+path, nil)
	req.Header.Set("Accept", "application/json")
	resp, err := ts.client.Do(req)
	if err != nil {
		t.Fatalf("GET %s: %v", path, err)
	}
	return resp
}

// loginCSRFToken fetches GET /login so the pre-login CSRF cookie is set on the
// jar, then returns the token value for use in the form post.
func (ts *testServer) loginCSRFToken(t *testing.T) string {
	t.Helper()
	resp := ts.get(t, "/login")
	defer func() { _ = resp.Body.Close() }()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("GET /login: %d", resp.StatusCode)
	}
	u, _ := url.Parse(ts.srv.URL)
	for _, c := range ts.client.Jar.Cookies(u) {
		if c.Name == authpkg.CSRFCookieName {
			return c.Value
		}
	}
	t.Fatal("pre-login csrf cookie not set")
	return ""
}

// sessionCSRFToken returns the CSRF token tied to the current session cookie.
func (ts *testServer) sessionCSRFToken(t *testing.T) string {
	t.Helper()
	u, _ := url.Parse(ts.srv.URL)
	for _, c := range ts.client.Jar.Cookies(u) {
		if c.Name == authpkg.CSRFCookieName {
			return c.Value
		}
	}
	t.Fatal("session csrf cookie not set")
	return ""
}

func (ts *testServer) postForm(t *testing.T, path string, form url.Values) *http.Response {
	t.Helper()
	req, _ := http.NewRequest(http.MethodPost, ts.srv.URL+path, strings.NewReader(form.Encode()))
	req.Header.Set("Content-Type", "application/x-www-form-urlencoded")
	resp, err := ts.client.Do(req)
	if err != nil {
		t.Fatalf("POST %s: %v", path, err)
	}
	return resp
}

func (ts *testServer) login(t *testing.T, userID, password string) *http.Response {
	t.Helper()
	tok := ts.loginCSRFToken(t)
	form := url.Values{}
	form.Set("user_id", userID)
	form.Set("password", password)
	form.Set(authpkg.CSRFFormField, tok)
	return ts.postForm(t, "/login", form)
}

// ---- Tests ------------------------------------------------------------------

func TestEndToEndAcceptanceFlow(t *testing.T) {
	// Issue acceptance: seed → POST /login as ADMIN001 → GET /admin/users (200)
	//                   → as USER0001 → GET /admin/users (403).
	ts := newTestServer(t)

	// Admin login
	resp := ts.login(t, "ADMIN001", "PASSWORD")
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusSeeOther {
		t.Fatalf("admin login: want 303, got %d", resp.StatusCode)
	}
	if loc := resp.Header.Get("Location"); loc != "/admin/users" {
		t.Fatalf("admin login redirect: got %q", loc)
	}

	resp = ts.get(t, "/admin/users")
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("admin GET /admin/users: want 200, got %d", resp.StatusCode)
	}

	// Sign off
	logoutForm := url.Values{}
	logoutForm.Set(authpkg.CSRFFormField, ts.sessionCSRFToken(t))
	resp = ts.postForm(t, "/logout", logoutForm)
	_ = resp.Body.Close()

	// Regular user login
	resp = ts.login(t, "USER0001", "PASSWORD")
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusSeeOther {
		t.Fatalf("user login: want 303, got %d", resp.StatusCode)
	}
	if loc := resp.Header.Get("Location"); loc != "/" {
		t.Fatalf("user login redirect: got %q (admins go to /admin/users, users go home)", loc)
	}

	// Regular user is forbidden from admin pages
	resp = ts.get(t, "/admin/users")
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("user GET /admin/users: want 403, got %d", resp.StatusCode)
	}
	// Audit must record the access denial.
	if ts.sink.CountByAction(audit.ActionAccessDenied) != 1 {
		t.Fatalf("expected 1 access.denied audit, got %d", ts.sink.CountByAction(audit.ActionAccessDenied))
	}
}

func TestLoginRejectsMissingCSRF(t *testing.T) {
	ts := newTestServer(t)
	// Skip the GET /login step that would set the pre-login CSRF cookie.
	form := url.Values{}
	form.Set("user_id", "ADMIN001")
	form.Set("password", "PASSWORD")
	resp := ts.postForm(t, "/login", form)
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 without CSRF, got %d", resp.StatusCode)
	}
}

func TestLoginWithBadCSRFRejected(t *testing.T) {
	ts := newTestServer(t)
	_ = ts.loginCSRFToken(t) // pre-login cookie set, but use the wrong value
	form := url.Values{}
	form.Set("user_id", "ADMIN001")
	form.Set("password", "PASSWORD")
	form.Set(authpkg.CSRFFormField, "tampered")
	resp := ts.postForm(t, "/login", form)
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusForbidden {
		t.Fatalf("expected 403 with bad CSRF, got %d", resp.StatusCode)
	}
}

func TestUnauthenticatedAdminRedirects(t *testing.T) {
	ts := newTestServer(t)
	resp := ts.get(t, "/admin/users")
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusSeeOther {
		t.Fatalf("anon GET /admin/users: want 303, got %d", resp.StatusCode)
	}
	if loc := resp.Header.Get("Location"); loc != "/login" {
		t.Fatalf("expected redirect to /login, got %q", loc)
	}
}

func TestSessionCookieFlags(t *testing.T) {
	ts := newTestServer(t)
	resp := ts.login(t, "ADMIN001", "PASSWORD")
	_ = resp.Body.Close()
	var sessCookie *http.Cookie
	for _, c := range resp.Cookies() {
		if c.Name == authpkg.SessionCookieName {
			sessCookie = c
		}
	}
	if sessCookie == nil {
		t.Fatal("session cookie not set")
	}
	if !sessCookie.HttpOnly {
		t.Fatal("session cookie must be HttpOnly")
	}
	if sessCookie.SameSite != http.SameSiteLaxMode {
		t.Fatalf("session cookie SameSite=Lax required, got %v", sessCookie.SameSite)
	}
	// Secure flag is toggled off in tests because httptest is plain HTTP, but
	// cookies set in production via DefaultCookieOptions() should be Secure.
	if defOpts := authpkg.DefaultCookieOptions(); !defOpts.Secure {
		t.Fatal("DefaultCookieOptions must produce Secure cookies in production")
	}
}

func TestBruteForceLockoutOverHTTP(t *testing.T) {
	// End-to-end variant of the rate-limit test, hitting the HTTP edge so the
	// CSRF + session machinery is exercised at the same time.
	ts := newTestServer(t)
	// Tighten the per-user limit so we don't have to send 5 attempts per test.
	ts.svc.UserLimiter = authpkg.NewRateLimiter(2, 15*60*1_000_000_000) // 15min in ns

	for i := 0; i < 2; i++ {
		resp := ts.login(t, "ADMIN001", "wrong")
		_ = resp.Body.Close()
		if resp.StatusCode != http.StatusUnauthorized {
			t.Fatalf("attempt %d: want 401, got %d", i, resp.StatusCode)
		}
	}
	// Next attempt — even with the right password — must be 429.
	resp := ts.login(t, "ADMIN001", "PASSWORD")
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusTooManyRequests {
		t.Fatalf("expected 429 after lockout, got %d", resp.StatusCode)
	}
}

func TestAdminCreateUpdateDeleteUserOverHTTP(t *testing.T) {
	ts := newTestServer(t)
	resp := ts.login(t, "ADMIN001", "PASSWORD")
	_ = resp.Body.Close()
	tok := ts.sessionCSRFToken(t)

	// Create
	form := url.Values{}
	form.Set(authpkg.CSRFFormField, tok)
	form.Set("user_id", "OP01")
	form.Set("first_name", "Op")
	form.Set("last_name", "Erator")
	form.Set("password", "secret")
	form.Set("user_type", "U")
	resp = ts.postForm(t, "/admin/users", form)
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusSeeOther {
		t.Fatalf("create: want 303, got %d", resp.StatusCode)
	}
	got, err := ts.users.Get(context.Background(), "OP01")
	if err != nil {
		t.Fatalf("user not created: %v", err)
	}
	if got.PwdHash == "secret" {
		t.Fatal("stored password must be hashed")
	}

	// JSON GET (and verify hash never leaks)
	resp = ts.getJSON(t, "/admin/users/OP01")
	body := map[string]any{}
	if err := json.NewDecoder(resp.Body).Decode(&body); err != nil {
		t.Fatalf("decode json: %v", err)
	}
	_ = resp.Body.Close()
	if _, leaked := body["pwd_hash"]; leaked {
		t.Fatal("password hash leaked over JSON")
	}

	// Update
	form = url.Values{}
	form.Set(authpkg.CSRFFormField, tok)
	form.Set("first_name", "Operator")
	form.Set("last_name", "Smith")
	form.Set("user_type", "A")
	resp = ts.postForm(t, "/admin/users/OP01", form)
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusSeeOther {
		t.Fatalf("update: want 303, got %d", resp.StatusCode)
	}
	got, _ = ts.users.Get(context.Background(), "OP01")
	if got.FirstName != "Operator" || got.LastName != "Smith" || got.Type != domain.UserTypeAdmin {
		t.Fatalf("update did not stick: %+v", got)
	}

	// Delete
	form = url.Values{}
	form.Set(authpkg.CSRFFormField, tok)
	resp = ts.postForm(t, "/admin/users/OP01/delete", form)
	_ = resp.Body.Close()
	if resp.StatusCode != http.StatusSeeOther {
		t.Fatalf("delete: want 303, got %d", resp.StatusCode)
	}
	if _, err := ts.users.Get(context.Background(), "OP01"); err == nil {
		t.Fatal("user not deleted")
	}

	// Audit covers the lifecycle.
	for _, a := range []audit.Action{audit.ActionUserCreate, audit.ActionUserUpdate, audit.ActionUserDelete} {
		if ts.sink.CountByAction(a) != 1 {
			t.Fatalf("expected 1 %s audit, got %d", a, ts.sink.CountByAction(a))
		}
	}
}
