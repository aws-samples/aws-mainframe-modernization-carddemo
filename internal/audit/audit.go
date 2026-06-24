// Package audit provides a tiny structured audit log used by the auth and
// admin-user flows. Every login attempt and every admin user mutation must
// produce one Event so security review (RAU-46) can replay activity.
package audit

import (
	"sync"
	"time"
)

type Action string

const (
	ActionLoginSuccess Action = "login.success"
	ActionLoginFailure Action = "login.failure"
	ActionLogout       Action = "logout"
	ActionUserCreate   Action = "user.create"
	ActionUserUpdate   Action = "user.update"
	ActionUserDelete   Action = "user.delete"
	ActionAccessDenied Action = "access.denied"
)

// Event is intentionally narrow: never carry password material, even hashed.
type Event struct {
	At     time.Time
	Action Action
	Actor  string // authenticated user, "" if pre-auth
	Target string // user id being acted on, may equal Actor for self
	Remote string // remote IP / forwarded address
	Reason string // freeform short reason, e.g. "wrong password"
}

// Sink receives audit events. Implementations must be safe for concurrent use.
type Sink interface {
	Record(Event)
}

// MemorySink keeps events in memory; suitable for tests and dev. Production
// will swap in a Sink that writes to durable storage.
type MemorySink struct {
	mu     sync.Mutex
	events []Event
	now    func() time.Time
}

func NewMemorySink() *MemorySink {
	return &MemorySink{now: time.Now}
}

func (s *MemorySink) Record(e Event) {
	if e.At.IsZero() {
		e.At = s.now()
	}
	s.mu.Lock()
	s.events = append(s.events, e)
	s.mu.Unlock()
}

func (s *MemorySink) Events() []Event {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]Event, len(s.events))
	copy(out, s.events)
	return out
}

// CountByAction is handy for tests that assert "exactly N login.failure events".
func (s *MemorySink) CountByAction(a Action) int {
	s.mu.Lock()
	defer s.mu.Unlock()
	n := 0
	for _, e := range s.events {
		if e.Action == a {
			n++
		}
	}
	return n
}
