import { useState } from 'react'
import type { FormEvent } from 'react'
import { Navigate, useLocation, useNavigate } from 'react-router-dom'
import { ApiError } from '../api/client'
import { useAuth } from '../auth/AuthContext'
import { IconLogo } from '../components/icons'

/** Sign-in screen — login-centered / login-centered-error mockups (COSGN00C). */
export default function LoginPage() {
  const { user, login } = useAuth()
  const navigate = useNavigate()
  const location = useLocation()
  const [userId, setUserId] = useState('')
  const [password, setPassword] = useState('')
  const [error, setError] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)

  if (user) {
    return <Navigate to="/" replace />
  }

  const from = (location.state as { from?: string } | null)?.from ?? '/'

  const onSubmit = async (e: FormEvent) => {
    e.preventDefault()
    if (!userId.trim() || !password) {
      setError('Enter both your user ID and password.')
      return
    }
    setBusy(true)
    setError(null)
    try {
      await login(userId.trim(), password)
      navigate(from, { replace: true })
    } catch (err) {
      if (err instanceof ApiError && err.status === 401) {
        setError('Sign-in failed. Check your user ID and password and try again.')
      } else {
        setError('Unable to sign in right now. Please try again.')
      }
    } finally {
      setBusy(false)
    }
  }

  return (
    <div className="auth-body">
      <div className="auth-center">
        <div className="auth-card">
          <div className="auth-brand">
            <div className="logo">
              <IconLogo />
            </div>
            <div>
              <div className="name">CardDemo</div>
              <div className="sub">Card Management</div>
            </div>
          </div>
          <h1>Sign in</h1>
          <p className="lead">Enter your user ID and password to continue.</p>
          {error && (
            <div className="alert error" role="alert" data-component-id="login-error">
              <div>
                <b>Sign-in failed.</b> {error.replace('Sign-in failed. ', '')}
              </div>
            </div>
          )}
          <form onSubmit={onSubmit}>
            <div className="field">
              <label htmlFor="userid">User ID</label>
              <input
                id="userid"
                className={`input mono${error ? ' invalid' : ''}`}
                data-component-id="login-user-id"
                type="text"
                maxLength={8}
                autoComplete="username"
                placeholder="8 characters"
                value={userId}
                onChange={(e) => setUserId(e.target.value)}
              />
              <div className="hint">Your 8-character user ID, e.g. USER0001</div>
            </div>
            <div className="field">
              <label htmlFor="password">Password</label>
              <input
                id="password"
                className={`input${error ? ' invalid' : ''}`}
                data-component-id="login-password"
                type="password"
                maxLength={8}
                autoComplete="current-password"
                placeholder="8 characters"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
              />
            </div>
            <button
              className="btn btn-primary btn-lg"
              style={{ width: '100%', marginTop: 6 }}
              data-component-id="login-submit"
              type="submit"
              disabled={busy}
            >
              {busy ? 'Signing in…' : 'Sign in'}
            </button>
          </form>
          <div className="auth-foot">
            Authorized users only. Sessions expire after 15 minutes of inactivity.
          </div>
        </div>
      </div>
    </div>
  )
}
