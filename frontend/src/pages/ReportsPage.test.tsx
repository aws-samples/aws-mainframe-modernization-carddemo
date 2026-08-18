import { describe, expect, it, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import ReportsPage from './ReportsPage'
import type { ReportRunView } from '../api/types'
import { jsonResponse } from '../test/helpers'

const completedRun: ReportRunView = {
  id: 12,
  reportType: 'custom',
  fromDate: '2022-01-01',
  toDate: '2022-12-31',
  status: 'COMPLETED',
  requestedBy: 'USER0001',
  createdAt: '2026-08-17T10:00:00',
  message: null,
}

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/reports']}>
        <ReportsPage />
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('ReportsPage', () => {
  it('lists past runs with a download action for completed ones', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(jsonResponse(200, [completedRun])))
    renderPage()

    expect(await screen.findByText('custom')).toBeInTheDocument()
    expect(screen.getByText('2022-01-01 → 2022-12-31')).toBeInTheDocument()
    expect(screen.getByText('COMPLETED')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Download' })).toBeInTheDocument()
  })

  it('submits a custom-range report with the CORPT00C per-part date fields', async () => {
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'POST') {
        return Promise.resolve(
          jsonResponse(201, {
            ...completedRun,
            message: 'Custom report submitted for printing ...',
          }),
        )
      }
      return Promise.resolve(jsonResponse(200, []))
    })
    vi.stubGlobal('fetch', fetchMock)
    vi.stubGlobal('confirm', vi.fn().mockReturnValue(true))
    renderPage()

    await screen.findByText('No report runs yet. Request one above.')
    await userEvent.type(screen.getByLabelText(/Start date/), '2022-01-05')
    await userEvent.type(screen.getByLabelText(/End date/), '2022-12-31')
    await userEvent.click(screen.getByRole('button', { name: 'Run report' }))

    expect(
      await screen.findByText('Custom report submitted for printing ...'),
    ).toBeInTheDocument()
    const postCall = fetchMock.mock.calls.find(
      (c) => (c[1] as RequestInit | undefined)?.method === 'POST',
    )!
    expect(postCall[0]).toBe('/api/v1/reports/transaction')
    expect(JSON.parse((postCall[1] as RequestInit).body as string)).toEqual({
      reportType: 'custom',
      startMonth: '01',
      startDay: '05',
      startYear: '2022',
      endMonth: '12',
      endDay: '31',
      endYear: '2022',
    })
  })

  it('does not submit when the CORPT00C confirm dialog is declined', async () => {
    const fetchMock = vi.fn().mockResolvedValue(jsonResponse(200, []))
    vi.stubGlobal('fetch', fetchMock)
    vi.stubGlobal('confirm', vi.fn().mockReturnValue(false))
    renderPage()

    await screen.findByText('No report runs yet. Request one above.')
    await userEvent.click(screen.getByRole('button', { name: 'Run report' }))
    expect(
      fetchMock.mock.calls.filter((c) => (c[1] as RequestInit | undefined)?.method === 'POST'),
    ).toHaveLength(0)
  })

  it('surfaces the first legacy edit message from a 400', async () => {
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'POST') {
        return Promise.resolve(
          jsonResponse(400, {
            error: 'validation_failed',
            message: 'Start Date - Month can NOT be empty...',
            fieldErrors: [
              { field: 'startMonth', message: 'Start Date - Month can NOT be empty...' },
            ],
          }),
        )
      }
      return Promise.resolve(jsonResponse(200, []))
    })
    vi.stubGlobal('fetch', fetchMock)
    vi.stubGlobal('confirm', vi.fn().mockReturnValue(true))
    renderPage()

    await screen.findByText('No report runs yet. Request one above.')
    await userEvent.click(screen.getByRole('button', { name: 'Run report' }))
    expect(
      await screen.findByText('Start Date - Month can NOT be empty...'),
    ).toBeInTheDocument()
  })

  it('switches monthly/yearly and hides the custom date fields', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(jsonResponse(200, [])))
    renderPage()

    await screen.findByText('No report runs yet. Request one above.')
    expect(screen.getByLabelText(/Start date/)).toBeInTheDocument()
    await userEvent.click(screen.getByLabelText(/Monthly \(current month\)/))
    expect(screen.queryByLabelText(/Start date/)).not.toBeInTheDocument()
  })
})
