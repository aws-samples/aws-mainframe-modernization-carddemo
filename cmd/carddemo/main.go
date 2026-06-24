// Command carddemo is a thin wiring binary that boots the auth + admin user
// HTTP server. RAU-43 (web layer) will replace this with the full app entrypoint.
package main

import (
	"context"
	"log"
	"net/http"
	"os"
	"time"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/audit"
	authpkg "github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/auth"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/repo"
	webauth "github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/web/auth"
	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/web/middleware"
)

func main() {
	addr := os.Getenv("CARDDEMO_ADDR")
	if addr == "" {
		addr = ":8080"
	}

	users := repo.NewInMemoryUserSec()
	if err := authpkg.SeedDefaultUsers(context.Background(), users, 0); err != nil {
		log.Fatalf("seed: %v", err)
	}
	sink := audit.NewMemorySink()
	store := authpkg.NewMemorySessionStore()
	svc := authpkg.NewService(users, store, sink)

	h := webauth.NewHandlers(svc)
	if os.Getenv("CARDDEMO_INSECURE_COOKIES") == "1" {
		h.CookieOptions.Secure = false // dev/local only — never set this in prod
	}

	mux := http.NewServeMux()
	loadSession := authpkg.LoadSession(store)
	requireAdmin := authpkg.RequireAdmin(sink, h.LoginPath)
	csrf := authpkg.CSRFProtect()

	publicMux := http.NewServeMux()
	publicMux.HandleFunc("GET /login", h.GetLogin)
	publicMux.HandleFunc("POST /login", h.PostLogin)
	publicMux.HandleFunc("POST /logout", h.PostLogout)

	adminMux := http.NewServeMux()
	adminMux.HandleFunc("GET /admin/users", h.ListUsers)
	adminMux.HandleFunc("POST /admin/users", h.CreateUser)
	adminMux.HandleFunc("GET /admin/users/{id}", h.GetUser)
	adminMux.HandleFunc("POST /admin/users/{id}", h.UpdateUser)
	adminMux.HandleFunc("POST /admin/users/{id}/delete", h.DeleteUser)

	mux.Handle("/login", publicMux)
	mux.Handle("/logout", publicMux)
	mux.Handle("/admin/", requireAdmin(adminMux))

	srv := &http.Server{
		Addr:              addr,
		Handler:           middleware.SecurityHeaders(loadSession(csrf(mux))),
		ReadHeaderTimeout: 10 * time.Second,
		ReadTimeout:       30 * time.Second,
		WriteTimeout:      30 * time.Second,
		IdleTimeout:       120 * time.Second,
	}
	log.Printf("carddemo listening on %s", addr) //nolint:gosec // G706: addr is operator-supplied at startup, not user input
	if err := srv.ListenAndServe(); err != nil {
		log.Fatal(err)
	}
}
