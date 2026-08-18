/** Minimal fetch wrapper: Bearer token, JSON, 401 -> logout callback. */

const TOKEN_KEY = 'carddemo.token'
const USER_KEY = 'carddemo.user'

import type { AuthUser, LoginResponse } from './types'

let onUnauthorized: (() => void) | null = null

export function setUnauthorizedHandler(fn: (() => void) | null) {
  onUnauthorized = fn
}

export function getStoredToken(): string | null {
  return localStorage.getItem(TOKEN_KEY)
}

export function getStoredUser(): AuthUser | null {
  const raw = localStorage.getItem(USER_KEY)
  if (!raw) return null
  try {
    return JSON.parse(raw) as AuthUser
  } catch {
    return null
  }
}

export function storeSession(login: LoginResponse) {
  localStorage.setItem(TOKEN_KEY, login.token)
  const user: AuthUser = {
    userId: login.userId,
    firstName: login.firstName,
    lastName: login.lastName,
    role: login.role,
  }
  localStorage.setItem(USER_KEY, JSON.stringify(user))
  return user
}

export function clearSession() {
  localStorage.removeItem(TOKEN_KEY)
  localStorage.removeItem(USER_KEY)
}

/** One failed server-side edit (COACTUPC edit suite); field = request body key. */
export interface ApiFieldError {
  field: string
  message: string
}

export class ApiError extends Error {
  readonly status: number
  readonly errorCode: string | null
  readonly fieldErrors: ApiFieldError[]

  constructor(
    status: number,
    errorCode: string | null,
    message: string,
    fieldErrors: ApiFieldError[] = [],
  ) {
    super(message)
    this.status = status
    this.errorCode = errorCode
    this.fieldErrors = fieldErrors
  }
}

async function request<T>(path: string, init?: RequestInit): Promise<T> {
  const headers = new Headers(init?.headers)
  headers.set('Accept', 'application/json')
  if (init?.body) headers.set('Content-Type', 'application/json')
  const token = getStoredToken()
  if (token) {
    headers.set('Authorization', `Bearer ${token}`)
    // Some proxies (e.g. sandbox preview edges) overwrite the Authorization
    // header; the Vite dev proxy restores it from this one (see vite.config.ts).
    headers.set('X-Auth-Token', token)
  }

  const res = await fetch(path, { ...init, headers })
  if (res.status === 401 && !path.endsWith('/auth/login')) {
    onUnauthorized?.()
  }
  if (!res.ok) {
    let errorCode: string | null = null
    let message = `Request failed (${res.status})`
    let fieldErrors: ApiFieldError[] = []
    try {
      const body = (await res.json()) as {
        error?: string
        message?: string
        fieldErrors?: ApiFieldError[]
      }
      errorCode = body.error ?? null
      if (body.message) message = body.message
      if (Array.isArray(body.fieldErrors)) fieldErrors = body.fieldErrors
    } catch {
      // non-JSON error body
    }
    throw new ApiError(res.status, errorCode, message, fieldErrors)
  }
  if (res.status === 204) return undefined as T
  return (await res.json()) as T
}

/**
 * Authenticated file fetch (report/statement downloads): plain <a href> links
 * can't carry the Authorization header, so fetch the body as a Blob with the
 * same auth headers the JSON wrapper uses.
 */
export async function fetchBlob(path: string): Promise<Blob> {
  const headers = new Headers()
  const token = getStoredToken()
  if (token) {
    headers.set('Authorization', `Bearer ${token}`)
    headers.set('X-Auth-Token', token)
  }
  const res = await fetch(path, { headers })
  if (res.status === 401) onUnauthorized?.()
  if (!res.ok) {
    throw new ApiError(res.status, null, `Download failed (${res.status})`)
  }
  return res.blob()
}

export const api = {
  get: <T>(path: string) => request<T>(path),
  post: <T>(path: string, body: unknown) =>
    request<T>(path, { method: 'POST', body: JSON.stringify(body) }),
  put: <T>(path: string, body: unknown) =>
    request<T>(path, { method: 'PUT', body: JSON.stringify(body) }),
  delete: <T = void>(path: string) => request<T>(path, { method: 'DELETE' }),
}

/** Shared react-query retry: don't retry 404s; otherwise up to 2 retries. */
export function retryUnlessNotFound(failureCount: number, error: unknown): boolean {
  return !(error instanceof ApiError && error.status === 404) && failureCount < 2
}
