import { describe, expect, it, vi } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { AuthProvider } from '../auth/AuthContext'
import LoginPage from './LoginPage'

function renderLogin() {
  return render(
    <MemoryRouter initialEntries={['/login']}>
      <AuthProvider>
        <Routes>
          <Route path="/login" element={<LoginPage />} />
          <Route path="/" element={<h1>Dashboard home</h1>} />
        </Routes>
      </AuthProvider>
    </MemoryRouter>,
  )
}

function mockFetchOnce(status: number, body: unknown) {
  const fn = vi.fn().mockResolvedValue(
    new Response(JSON.stringify(body), {
      status,
      headers: { 'Content-Type': 'application/json' },
    }),
  )
  vi.stubGlobal('fetch', fn)
  return fn
}

describe('LoginPage', () => {
  it('signs in and redirects on valid credentials', async () => {
    const fetchMock = mockFetchOnce(200, {
      token: 'jwt-token',
      expiresInSeconds: 900,
      userId: 'USER0001',
      firstName: 'LAWRENCE',
      lastName: 'THOMAS',
      role: 'USER',
    })
    renderLogin()

    await userEvent.type(screen.getByLabelText('User ID'), 'USER0001')
    await userEvent.type(screen.getByLabelText('Password'), 'PASSWORD')
    await userEvent.click(screen.getByRole('button', { name: 'Sign in' }))

    await waitFor(() => expect(screen.getByText('Dashboard home')).toBeInTheDocument())
    expect(fetchMock).toHaveBeenCalledWith(
      '/api/v1/auth/login',
      expect.objectContaining({ method: 'POST' }),
    )
    expect(localStorage.getItem('carddemo.token')).toBe('jwt-token')
  })

  it('shows the error alert on 401 and marks inputs invalid', async () => {
    mockFetchOnce(401, { error: 'invalid_credentials' })
    renderLogin()

    await userEvent.type(screen.getByLabelText('User ID'), 'USER0001')
    await userEvent.type(screen.getByLabelText('Password'), 'WRONGPWD')
    await userEvent.click(screen.getByRole('button', { name: 'Sign in' }))

    const alert = await screen.findByRole('alert')
    expect(alert).toHaveTextContent('Sign-in failed')
    expect(screen.getByLabelText('User ID')).toHaveClass('invalid')
    expect(screen.getByLabelText('Password')).toHaveClass('invalid')
    expect(localStorage.getItem('carddemo.token')).toBeNull()
    // still on the login page
    expect(screen.getByRole('button', { name: 'Sign in' })).toBeInTheDocument()
  })

  it('validates that both fields are filled before submitting', async () => {
    const fetchMock = mockFetchOnce(200, {})
    renderLogin()

    await userEvent.click(screen.getByRole('button', { name: 'Sign in' }))

    expect(await screen.findByRole('alert')).toHaveTextContent(
      'Enter both your user ID and password.',
    )
    expect(fetchMock).not.toHaveBeenCalled()
  })
})
