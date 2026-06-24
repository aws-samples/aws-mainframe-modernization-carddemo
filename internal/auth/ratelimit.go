package auth

import (
	"sync"
	"time"
)

// RateLimiter is a fixed-window counter keyed by (key) — typically "ip:1.2.3.4"
// or "user:ADMIN001". A failed login records both an IP key and a username key
// so attackers cannot hide behind either rotation alone.
//
// Two cohorts are tracked separately because they need different policies:
// per-IP is broad (catches credential stuffing), per-user is tight (catches
// targeted brute force). The handler increments both on failure.
//
// On Allow: if the key is currently over budget (>= max attempts inside the
// window), returns false and a Retry-After hint. Successful logins should call
// Reset(key) to clear the user counter so a legitimate user is not locked out
// after their own typo.
type RateLimiter struct {
	mu      sync.Mutex
	buckets map[string]*bucket
	max     int
	window  time.Duration
	now     func() time.Time
}

type bucket struct {
	count       int
	windowStart time.Time
}

func NewRateLimiter(max int, window time.Duration) *RateLimiter {
	return &RateLimiter{
		buckets: make(map[string]*bucket),
		max:     max,
		window:  window,
		now:     time.Now,
	}
}

// Allow records a hit and returns false if the key is over budget. The second
// return is the time remaining in the current window (only meaningful on a
// false return).
func (r *RateLimiter) Allow(key string) (bool, time.Duration) {
	r.mu.Lock()
	defer r.mu.Unlock()
	now := r.now()
	b, ok := r.buckets[key]
	if !ok || now.Sub(b.windowStart) >= r.window {
		r.buckets[key] = &bucket{count: 1, windowStart: now}
		return true, 0
	}
	if b.count >= r.max {
		return false, r.window - now.Sub(b.windowStart)
	}
	b.count++
	return true, 0
}

// Peek reports whether the key is currently over budget without recording a
// hit. Used to short-circuit a request before doing any work (and before the
// password compare, so we don't burn bcrypt cycles on a locked-out attacker).
func (r *RateLimiter) Peek(key string) (bool, time.Duration) {
	r.mu.Lock()
	defer r.mu.Unlock()
	b, ok := r.buckets[key]
	if !ok {
		return true, 0
	}
	now := r.now()
	if now.Sub(b.windowStart) >= r.window {
		return true, 0
	}
	if b.count >= r.max {
		return false, r.window - now.Sub(b.windowStart)
	}
	return true, 0
}

// Reset clears the counter for a key, e.g. after a successful login.
func (r *RateLimiter) Reset(key string) {
	r.mu.Lock()
	delete(r.buckets, key)
	r.mu.Unlock()
}
