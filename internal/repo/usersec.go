// Package repo defines persistence interfaces and an in-memory implementation
// for the user-security store (USRSEC VSAM file in the legacy system).
//
// The interface is the contract RAU-38 will eventually back with a real KV/SQL
// store; the in-memory impl exists so the auth + admin flows can run and be
// tested today.
package repo

import (
	"context"
	"errors"
	"sort"
	"sync"

	"github.com/aws-samples/aws-mainframe-modernization-carddemo/internal/domain"
)

var (
	ErrNotFound = errors.New("user not found")
	ErrConflict = errors.New("user already exists")
)

// UserSecRepo mirrors the VSAM/KSDS operations the COBOL programs perform on
// the USRSEC dataset (READ / WRITE / REWRITE / DELETE / browse).
type UserSecRepo interface {
	Get(ctx context.Context, userID string) (domain.UserSec, error)
	Create(ctx context.Context, u domain.UserSec) error
	Update(ctx context.Context, u domain.UserSec) error
	Delete(ctx context.Context, userID string) error
	List(ctx context.Context) ([]domain.UserSec, error)
}

// InMemoryUserSec is a thread-safe map-backed UserSecRepo. Production code will
// swap this for the persistent implementation from RAU-38.
type InMemoryUserSec struct {
	mu    sync.RWMutex
	users map[string]domain.UserSec
}

func NewInMemoryUserSec() *InMemoryUserSec {
	return &InMemoryUserSec{users: make(map[string]domain.UserSec)}
}

func (r *InMemoryUserSec) Get(_ context.Context, userID string) (domain.UserSec, error) {
	id := domain.NormalizeUserID(userID)
	r.mu.RLock()
	defer r.mu.RUnlock()
	u, ok := r.users[id]
	if !ok {
		return domain.UserSec{}, ErrNotFound
	}
	return u, nil
}

func (r *InMemoryUserSec) Create(_ context.Context, u domain.UserSec) error {
	id := domain.NormalizeUserID(u.UserID)
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, exists := r.users[id]; exists {
		return ErrConflict
	}
	u.UserID = id
	r.users[id] = u
	return nil
}

func (r *InMemoryUserSec) Update(_ context.Context, u domain.UserSec) error {
	id := domain.NormalizeUserID(u.UserID)
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, exists := r.users[id]; !exists {
		return ErrNotFound
	}
	u.UserID = id
	r.users[id] = u
	return nil
}

func (r *InMemoryUserSec) Delete(_ context.Context, userID string) error {
	id := domain.NormalizeUserID(userID)
	r.mu.Lock()
	defer r.mu.Unlock()
	if _, exists := r.users[id]; !exists {
		return ErrNotFound
	}
	delete(r.users, id)
	return nil
}

func (r *InMemoryUserSec) List(_ context.Context) ([]domain.UserSec, error) {
	r.mu.RLock()
	out := make([]domain.UserSec, 0, len(r.users))
	for _, u := range r.users {
		out = append(out, u)
	}
	r.mu.RUnlock()
	sort.Slice(out, func(i, j int) bool { return out[i].UserID < out[j].UserID })
	return out, nil
}
