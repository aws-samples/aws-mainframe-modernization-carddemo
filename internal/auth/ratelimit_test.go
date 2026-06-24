package auth

import (
	"testing"
	"time"
)

func TestRateLimiterAllowsUpToMax(t *testing.T) {
	rl := NewRateLimiter(3, time.Minute)
	for i := 0; i < 3; i++ {
		ok, _ := rl.Allow("k")
		if !ok {
			t.Fatalf("attempt %d: expected allow", i)
		}
	}
	ok, retry := rl.Allow("k")
	if ok {
		t.Fatal("4th attempt should be denied")
	}
	if retry <= 0 {
		t.Fatalf("expected positive retry-after, got %v", retry)
	}
}

func TestRateLimiterPeekDoesNotConsume(t *testing.T) {
	rl := NewRateLimiter(2, time.Minute)
	rl.Allow("k")
	for i := 0; i < 5; i++ {
		ok, _ := rl.Peek("k")
		if !ok {
			t.Fatal("peek should not exhaust the budget")
		}
	}
	ok, _ := rl.Allow("k")
	if !ok {
		t.Fatal("second Allow after Peeks should still be allowed")
	}
	ok, _ = rl.Allow("k")
	if ok {
		t.Fatal("third Allow should be denied")
	}
}

func TestRateLimiterWindowExpiry(t *testing.T) {
	rl := NewRateLimiter(2, time.Minute)
	now := time.Unix(0, 0)
	rl.now = func() time.Time { return now }

	rl.Allow("k")
	rl.Allow("k")
	if ok, _ := rl.Allow("k"); ok {
		t.Fatal("expected denial inside window")
	}
	now = now.Add(2 * time.Minute)
	if ok, _ := rl.Allow("k"); !ok {
		t.Fatal("expected allow after window expiry")
	}
}

func TestRateLimiterReset(t *testing.T) {
	rl := NewRateLimiter(1, time.Minute)
	rl.Allow("k")
	if ok, _ := rl.Allow("k"); ok {
		t.Fatal("expected denial before reset")
	}
	rl.Reset("k")
	if ok, _ := rl.Allow("k"); !ok {
		t.Fatal("expected allow after reset")
	}
}
