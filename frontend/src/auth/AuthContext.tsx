import { createContext, useCallback, useContext, useEffect, useMemo, useState } from 'react'
import type { ReactNode } from 'react'
import {
  api,
  clearSession,
  getStoredToken,
  getStoredUser,
  setUnauthorizedHandler,
  storeSession,
} from '../api/client'
import type { AuthUser, LoginResponse } from '../api/types'

interface AuthContextValue {
  user: AuthUser | null
  login: (userId: string, password: string) => Promise<AuthUser>
  logout: () => void
}

const AuthContext = createContext<AuthContextValue | null>(null)

export function AuthProvider({ children }: { children: ReactNode }) {
  // refresh-on-load: restore user if a token is still stored
  const [user, setUser] = useState<AuthUser | null>(() =>
    getStoredToken() ? getStoredUser() : null,
  )

  const logout = useCallback(() => {
    clearSession()
    setUser(null)
  }, [])

  useEffect(() => {
    setUnauthorizedHandler(logout)
    return () => setUnauthorizedHandler(null)
  }, [logout])

  const login = useCallback(async (userId: string, password: string) => {
    const res = await api.post<LoginResponse>('/api/v1/auth/login', { userId, password })
    const u = storeSession(res)
    setUser(u)
    return u
  }, [])

  const value = useMemo(() => ({ user, login, logout }), [user, login, logout])
  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>
}

export function useAuth(): AuthContextValue {
  const ctx = useContext(AuthContext)
  if (!ctx) throw new Error('useAuth must be used within AuthProvider')
  return ctx
}
