// Package auth (web/auth) is the HTTP edge for sign-on and admin user CRUD.
// It is the Go replacement for the COBOL programs:
//
//	COSGN00C  -> POST /login, GET /login, POST /logout
//	COUSR00C  -> GET  /admin/users
//	COUSR01C  -> POST /admin/users          (add)
//	COUSR02C  -> POST /admin/users/{id}     (update)
//	COUSR03C  -> POST /admin/users/{id}/delete  (delete; HTML form, no DELETE verb)
//
// Handlers render minimal HTML for browsers and JSON for Accept: application/json.
package auth

import (
	"encoding/json"
	"errors"
	"html/template"
	"net/http"
	"strings"

	authpkg "github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/auth"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/repo"
)

// Handlers exposes the HTTP entry points. Wire it with Routes().
type Handlers struct {
	Service       *authpkg.Service
	CookieOptions authpkg.CookieOptions
	LoginPath     string // typically "/login"
	HomePath      string // where to send a USER after successful login
	AdminPath     string // where to send an ADMIN after successful login
}

func NewHandlers(svc *authpkg.Service) *Handlers {
	return &Handlers{
		Service:       svc,
		CookieOptions: authpkg.DefaultCookieOptions(),
		LoginPath:     "/login",
		HomePath:      "/",
		AdminPath:     "/admin/users",
	}
}

// Routes registers all handlers on the given mux. Caller is responsible for
// wrapping the admin subtree with LoadSession + RequireAdmin + CSRFProtect;
// the login routes need only LoadSession + CSRFProtect.
func (h *Handlers) Routes(mux *http.ServeMux) {
	mux.HandleFunc("GET /login", h.GetLogin)
	mux.HandleFunc("POST /login", h.PostLogin)
	mux.HandleFunc("POST /logout", h.PostLogout)

	mux.HandleFunc("GET /admin/users", h.ListUsers)
	mux.HandleFunc("POST /admin/users", h.CreateUser)
	mux.HandleFunc("GET /admin/users/{id}", h.GetUser)
	mux.HandleFunc("POST /admin/users/{id}", h.UpdateUser)
	mux.HandleFunc("POST /admin/users/{id}/delete", h.DeleteUser)
}

// ---- Login / logout ---------------------------------------------------------

func (h *Handlers) GetLogin(w http.ResponseWriter, r *http.Request) {
	if _, ok := authpkg.SessionFromContext(r.Context()); ok {
		http.Redirect(w, r, h.HomePath, http.StatusSeeOther)
		return
	}
	// We render a fresh CSRF token so even an anonymous form submit can be
	// double-submitted. We bind it to a short-lived "pre-session" cookie.
	preCSRF, err := authpkg.NewCSRFToken()
	if err != nil {
		http.Error(w, "internal error", http.StatusInternalServerError)
		return
	}
	authpkg.SetPreLoginCSRFCookie(w, preCSRF, h.CookieOptions)
	renderLogin(w, loginView{CSRFToken: preCSRF})
}

type loginRequest struct {
	UserID   string `json:"user_id"`
	Password string `json:"password"`
}

func (h *Handlers) PostLogin(w http.ResponseWriter, r *http.Request) {
	if _, ok := authpkg.SessionFromContext(r.Context()); ok {
		http.Redirect(w, r, h.HomePath, http.StatusSeeOther)
		return
	}
	// CSRFProtect middleware has already validated the double-submit token
	// against the carddemo_csrf cookie set on GET /login.

	userID, password, err := readLoginInput(r)
	if err != nil {
		http.Error(w, "bad request", http.StatusBadRequest)
		return
	}

	sess, err := h.Service.Login(r.Context(), userID, password, authpkg.ClientIP(r))
	if err != nil {
		switch {
		case errors.Is(err, authpkg.ErrRateLimited):
			respondLoginError(w, r, "Too many attempts. Try again later.", http.StatusTooManyRequests)
		default:
			respondLoginError(w, r, "Invalid user id or password.", http.StatusUnauthorized)
		}
		return
	}

	authpkg.SetSessionCookie(w, sess, h.CookieOptions)
	dest := h.HomePath
	if sess.IsAdmin() {
		dest = h.AdminPath
	}
	if wantsJSON(r) {
		writeJSON(w, http.StatusOK, map[string]string{
			"user_id":   sess.UserID,
			"user_type": string(sess.UserType),
			"redirect":  dest,
		})
		return
	}
	http.Redirect(w, r, dest, http.StatusSeeOther)
}

func (h *Handlers) PostLogout(w http.ResponseWriter, r *http.Request) {
	if sess, ok := authpkg.SessionFromContext(r.Context()); ok {
		_ = h.Service.Logout(r.Context(), sess, authpkg.ClientIP(r))
	}
	authpkg.ClearSessionCookies(w, h.CookieOptions)
	if wantsJSON(r) {
		w.WriteHeader(http.StatusNoContent)
		return
	}
	http.Redirect(w, r, h.LoginPath, http.StatusSeeOther)
}

// ---- Admin user CRUD --------------------------------------------------------

func (h *Handlers) ListUsers(w http.ResponseWriter, r *http.Request) {
	users, err := h.Service.ListUsers(r.Context())
	if err != nil {
		http.Error(w, "internal error", http.StatusInternalServerError)
		return
	}
	sess, _ := authpkg.SessionFromContext(r.Context())
	if wantsJSON(r) {
		writeJSON(w, http.StatusOK, map[string]any{"users": stripHashes(users)})
		return
	}
	renderAdminList(w, adminListView{
		Users:     stripHashes(users),
		CSRFToken: sess.CSRFToken,
	})
}

func (h *Handlers) GetUser(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	u, err := h.Service.GetUser(r.Context(), id)
	if err != nil {
		http.Error(w, "not found", http.StatusNotFound)
		return
	}
	if wantsJSON(r) {
		writeJSON(w, http.StatusOK, stripHash(u))
		return
	}
	sess, _ := authpkg.SessionFromContext(r.Context())
	renderAdminEdit(w, adminEditView{User: stripHash(u), CSRFToken: sess.CSRFToken})
}

type userMutationRequest struct {
	UserID    string `json:"user_id"`
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	Password  string `json:"password"`
	UserType  string `json:"user_type"`
}

func (h *Handlers) CreateUser(w http.ResponseWriter, r *http.Request) {
	req, err := readMutationInput(r)
	if err != nil {
		http.Error(w, "bad request", http.StatusBadRequest)
		return
	}
	t, err := parseUserType(req.UserType)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	sess, _ := authpkg.SessionFromContext(r.Context())
	err = h.Service.CreateUser(r.Context(), sess.UserID, req.UserID, req.FirstName, req.LastName, req.Password, t, authpkg.ClientIP(r))
	if err != nil {
		writeMutationError(w, r, err)
		return
	}
	if wantsJSON(r) {
		w.WriteHeader(http.StatusCreated)
		return
	}
	http.Redirect(w, r, h.AdminPath, http.StatusSeeOther)
}

func (h *Handlers) UpdateUser(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	req, err := readMutationInput(r)
	if err != nil {
		http.Error(w, "bad request", http.StatusBadRequest)
		return
	}
	t, err := parseUserType(req.UserType)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	sess, _ := authpkg.SessionFromContext(r.Context())
	err = h.Service.UpdateUser(r.Context(), sess.UserID, id, req.FirstName, req.LastName, req.Password, t, authpkg.ClientIP(r))
	if err != nil {
		writeMutationError(w, r, err)
		return
	}
	if wantsJSON(r) {
		w.WriteHeader(http.StatusOK)
		return
	}
	http.Redirect(w, r, h.AdminPath, http.StatusSeeOther)
}

func (h *Handlers) DeleteUser(w http.ResponseWriter, r *http.Request) {
	id := r.PathValue("id")
	sess, _ := authpkg.SessionFromContext(r.Context())
	if err := h.Service.DeleteUser(r.Context(), sess.UserID, id, authpkg.ClientIP(r)); err != nil {
		writeMutationError(w, r, err)
		return
	}
	if wantsJSON(r) {
		w.WriteHeader(http.StatusNoContent)
		return
	}
	http.Redirect(w, r, h.AdminPath, http.StatusSeeOther)
}

// ---- helpers ---------------------------------------------------------------

func readLoginInput(r *http.Request) (string, string, error) {
	if strings.HasPrefix(r.Header.Get("Content-Type"), "application/json") {
		var lr loginRequest
		if err := json.NewDecoder(r.Body).Decode(&lr); err != nil {
			return "", "", err
		}
		return lr.UserID, lr.Password, nil
	}
	if err := r.ParseForm(); err != nil {
		return "", "", err
	}
	return r.PostFormValue("user_id"), r.PostFormValue("password"), nil
}

func readMutationInput(r *http.Request) (userMutationRequest, error) {
	if strings.HasPrefix(r.Header.Get("Content-Type"), "application/json") {
		var req userMutationRequest
		if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
			return req, err
		}
		return req, nil
	}
	if err := r.ParseForm(); err != nil {
		return userMutationRequest{}, err
	}
	return userMutationRequest{
		UserID:    r.PostFormValue("user_id"),
		FirstName: r.PostFormValue("first_name"),
		LastName:  r.PostFormValue("last_name"),
		Password:  r.PostFormValue("password"),
		UserType:  r.PostFormValue("user_type"),
	}, nil
}

func parseUserType(s string) (domain.UserType, error) {
	s = strings.ToUpper(strings.TrimSpace(s))
	switch s {
	case "A", "ADMIN":
		return domain.UserTypeAdmin, nil
	case "U", "USER":
		return domain.UserTypeUser, nil
	}
	return 0, domain.ErrInvalidUserType
}

func writeMutationError(w http.ResponseWriter, r *http.Request, err error) {
	switch {
	case errors.Is(err, repo.ErrConflict):
		http.Error(w, "user already exists", http.StatusConflict)
	case errors.Is(err, repo.ErrNotFound):
		http.Error(w, "user not found", http.StatusNotFound)
	case errors.Is(err, domain.ErrUserIDEmpty),
		errors.Is(err, domain.ErrUserIDTooLong),
		errors.Is(err, domain.ErrFirstNameEmpty),
		errors.Is(err, domain.ErrLastNameEmpty),
		errors.Is(err, domain.ErrPasswordEmpty),
		errors.Is(err, domain.ErrPasswordTooLong),
		errors.Is(err, domain.ErrInvalidUserType):
		http.Error(w, err.Error(), http.StatusBadRequest)
	default:
		http.Error(w, "internal error", http.StatusInternalServerError)
	}
	_ = r // r kept for future logging hooks
}

func wantsJSON(r *http.Request) bool {
	a := r.Header.Get("Accept")
	return strings.Contains(strings.ToLower(a), "application/json") ||
		strings.HasPrefix(r.Header.Get("Content-Type"), "application/json")
}

func writeJSON(w http.ResponseWriter, status int, body any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(body)
}

// publicUser is what we serialize back to clients — the bcrypt hash never leaves the server.
type publicUser struct {
	UserID    string `json:"user_id"`
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	UserType  string `json:"user_type"`
}

func stripHash(u domain.UserSec) publicUser {
	return publicUser{
		UserID:    u.UserID,
		FirstName: u.FirstName,
		LastName:  u.LastName,
		UserType:  string(u.Type),
	}
}

func stripHashes(us []domain.UserSec) []publicUser {
	out := make([]publicUser, len(us))
	for i, u := range us {
		out[i] = stripHash(u)
	}
	return out
}

func respondLoginError(w http.ResponseWriter, r *http.Request, msg string, status int) {
	if wantsJSON(r) {
		writeJSON(w, status, map[string]string{"error": msg})
		return
	}
	w.WriteHeader(status)
	renderLogin(w, loginView{Error: msg, CSRFToken: preLoginCSRFFromRequest(r)})
}

func preLoginCSRFFromRequest(r *http.Request) string {
	c, err := r.Cookie(authpkg.CSRFCookieName)
	if err != nil {
		return ""
	}
	return c.Value
}

// ---- views (tiny inline templates) -----------------------------------------

type loginView struct {
	Error     string
	CSRFToken string
}

type adminListView struct {
	Users     []publicUser
	CSRFToken string
}

type adminEditView struct {
	User      publicUser
	CSRFToken string
}

var (
	loginTpl = template.Must(template.New("login").Parse(`<!doctype html>
<html><head><title>CardDemo Sign-on</title></head><body>
<h1>CardDemo Sign-on</h1>
{{if .Error}}<p style="color:red">{{.Error}}</p>{{end}}
<form method="post" action="/login">
  <input type="hidden" name="csrf_token" value="{{.CSRFToken}}">
  <label>User ID <input name="user_id" maxlength="8" required></label>
  <label>Password <input type="password" name="password" maxlength="8" required></label>
  <button type="submit">Sign on</button>
</form>
</body></html>`))

	adminListTpl = template.Must(template.New("admin_list").Parse(`<!doctype html>
<html><head><title>Admin · Users</title></head><body>
<h1>Users</h1>
<form method="post" action="/logout">
  <input type="hidden" name="csrf_token" value="{{.CSRFToken}}">
  <button type="submit">Sign off</button>
</form>
<table border="1">
<tr><th>ID</th><th>First</th><th>Last</th><th>Type</th><th></th></tr>
{{range .Users}}
<tr>
  <td><a href="/admin/users/{{.UserID}}">{{.UserID}}</a></td>
  <td>{{.FirstName}}</td><td>{{.LastName}}</td><td>{{.UserType}}</td>
  <td>
    <form method="post" action="/admin/users/{{.UserID}}/delete" style="display:inline">
      <input type="hidden" name="csrf_token" value="{{$.CSRFToken}}">
      <button type="submit">Delete</button>
    </form>
  </td>
</tr>
{{end}}
</table>
<h2>Add user</h2>
<form method="post" action="/admin/users">
  <input type="hidden" name="csrf_token" value="{{.CSRFToken}}">
  <label>User ID <input name="user_id" maxlength="8" required></label>
  <label>First <input name="first_name" maxlength="20" required></label>
  <label>Last <input name="last_name" maxlength="20" required></label>
  <label>Password <input type="password" name="password" maxlength="8" required></label>
  <label>Type
    <select name="user_type">
      <option value="U">User</option>
      <option value="A">Admin</option>
    </select>
  </label>
  <button type="submit">Add</button>
</form>
</body></html>`))

	adminEditTpl = template.Must(template.New("admin_edit").Parse(`<!doctype html>
<html><head><title>Edit {{.User.UserID}}</title></head><body>
<h1>Edit {{.User.UserID}}</h1>
<form method="post" action="/admin/users/{{.User.UserID}}">
  <input type="hidden" name="csrf_token" value="{{.CSRFToken}}">
  <input type="hidden" name="user_id" value="{{.User.UserID}}">
  <label>First <input name="first_name" value="{{.User.FirstName}}" maxlength="20" required></label>
  <label>Last  <input name="last_name"  value="{{.User.LastName}}"  maxlength="20" required></label>
  <label>New Password (leave blank to keep) <input type="password" name="password" maxlength="8"></label>
  <label>Type
    <select name="user_type">
      <option value="U" {{if eq .User.UserType "U"}}selected{{end}}>User</option>
      <option value="A" {{if eq .User.UserType "A"}}selected{{end}}>Admin</option>
    </select>
  </label>
  <button type="submit">Save</button>
</form>
<form method="post" action="/admin/users/{{.User.UserID}}/delete">
  <input type="hidden" name="csrf_token" value="{{.CSRFToken}}">
  <button type="submit">Delete</button>
</form>
<a href="/admin/users">Back</a>
</body></html>`))
)

func renderLogin(w http.ResponseWriter, v loginView) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_ = loginTpl.Execute(w, v)
}

func renderAdminList(w http.ResponseWriter, v adminListView) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_ = adminListTpl.Execute(w, v)
}

func renderAdminEdit(w http.ResponseWriter, v adminEditView) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_ = adminEditTpl.Execute(w, v)
}
