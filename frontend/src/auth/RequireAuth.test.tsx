import { describe, expect, it } from 'vitest'
import { render, screen } from '@testing-library/react'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { AuthProvider } from './AuthContext'
import RequireAuth from './RequireAuth'

function renderAt(path: string) {
  return render(
    <MemoryRouter initialEntries={[path]}>
      <AuthProvider>
        <Routes>
          <Route path="/login" element={<h1>Login screen</h1>} />
          <Route element={<RequireAuth />}>
            <Route path="/" element={<h1>Protected home</h1>} />
          </Route>
        </Routes>
      </AuthProvider>
    </MemoryRouter>,
  )
}

describe('RequireAuth', () => {
  it('redirects unauthenticated users to /login', () => {
    renderAt('/')
    expect(screen.getByText('Login screen')).toBeInTheDocument()
    expect(screen.queryByText('Protected home')).not.toBeInTheDocument()
  })

  it('renders the protected route when a session is stored', () => {
    localStorage.setItem('carddemo.token', 'jwt-token')
    localStorage.setItem(
      'carddemo.user',
      JSON.stringify({ userId: 'USER0001', firstName: 'L', lastName: 'T', role: 'USER' }),
    )
    renderAt('/')
    expect(screen.getByText('Protected home')).toBeInTheDocument()
  })
})
