import { describe, expect, it, vi } from 'vitest'
import { render, screen, waitFor, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import AdminUsersPage from './AdminUsersPage'
import type { Page, UserView } from '../api/types'
import { jsonResponse } from '../test/helpers'

const users: Page<UserView> = {
  items: [
    { userId: 'ADMIN001', firstName: 'Alice', lastName: 'Barker', userType: 'A', version: 0 },
    { userId: 'USER0001', firstName: 'Ward', lastName: 'Jones', userType: 'U', version: 0 },
  ],
  nextCursor: null,
}

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/admin/users']}>
        <Routes>
          <Route path="/admin/users" element={<AdminUsersPage />} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('AdminUsersPage', () => {
  it('lists users with type badges and filters by search text', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(jsonResponse(200, users)))
    renderPage()

    expect(await screen.findByText('ADMIN001')).toBeInTheDocument()
    expect(screen.getByText('USER0001')).toBeInTheDocument()
    const table = screen.getByRole('table')
    expect(within(table).getByText('Admin')).toBeInTheDocument()
    expect(within(table).getByText('User')).toBeInTheDocument()

    await userEvent.type(screen.getByLabelText('Search'), 'ward')
    expect(screen.queryByText('ADMIN001')).not.toBeInTheDocument()
    expect(screen.getByText('USER0001')).toBeInTheDocument()
  })

  it('validates the add-user drawer and blocks empty submits (COUSR01C edits)', async () => {
    const fetchMock = vi.fn().mockResolvedValue(jsonResponse(200, users))
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await screen.findByText('ADMIN001')
    await userEvent.click(screen.getByRole('button', { name: 'Add user' }))
    const drawer = screen.getByRole('dialog', { name: 'Add user' })
    await userEvent.click(within(drawer).getByRole('button', { name: 'Create user' }))

    expect(await within(drawer).findByText('User ID can NOT be empty...')).toBeInTheDocument()
    expect(within(drawer).getByText('First Name can NOT be empty...')).toBeInTheDocument()
    expect(within(drawer).getByText('Last Name can NOT be empty...')).toBeInTheDocument()
    expect(within(drawer).getByText('Password can NOT be empty...')).toBeInTheDocument()
    // only the list GET went out
    expect(fetchMock.mock.calls.every(([, init]) => init?.method !== 'POST')).toBe(true)
  })

  it('creates a user via the drawer and surfaces duplicate-ID 409 inline', async () => {
    const fetchMock = vi
      .fn()
      .mockImplementation((_url: string, init?: RequestInit) => {
        if (init?.method === 'POST') {
          return Promise.resolve(
            jsonResponse(409, { error: 'conflict', message: 'User ID already exist...' }),
          )
        }
        return Promise.resolve(jsonResponse(200, users))
      })
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await screen.findByText('ADMIN001')
    await userEvent.click(screen.getByRole('button', { name: 'Add user' }))
    const drawer = screen.getByRole('dialog', { name: 'Add user' })
    await userEvent.type(within(drawer).getByLabelText(/User ID/), 'ADMIN001')
    await userEvent.type(within(drawer).getByLabelText(/First name/), 'Dup')
    await userEvent.type(within(drawer).getByLabelText(/Last name/), 'User')
    await userEvent.type(within(drawer).getByLabelText(/Initial password/), 'PASSWORD')
    await userEvent.click(within(drawer).getByRole('button', { name: 'Create user' }))

    expect(await within(drawer).findByText('User ID already exist...')).toBeInTheDocument()
  })

  it('deletes a user after the confirm modal', async () => {
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'DELETE') {
        return Promise.resolve(new Response(null, { status: 204 }))
      }
      return Promise.resolve(jsonResponse(200, users))
    })
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await screen.findByText('USER0001')
    await userEvent.click(screen.getByRole('button', { name: 'Delete user USER0001' }))
    const modal = screen.getByRole('dialog', { name: 'Delete user' })
    expect(within(modal).getByText(/cannot be undone/)).toBeInTheDocument()
    await userEvent.click(within(modal).getByRole('button', { name: 'Delete user' }))

    await waitFor(() =>
      expect(
        fetchMock.mock.calls.some(
          ([url, init]) => url === '/api/v1/users/USER0001' && init?.method === 'DELETE',
        ),
      ).toBe(true),
    )
    await waitFor(() =>
      expect(screen.queryByRole('dialog', { name: 'Delete user' })).not.toBeInTheDocument(),
    )
  })
})
