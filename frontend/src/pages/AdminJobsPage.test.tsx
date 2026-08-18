import { describe, expect, it, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import AdminJobsPage from './AdminJobsPage'
import type { JobRunView } from '../api/types'
import { jsonResponse } from '../test/helpers'

const postTranRun: JobRunView = {
  runId: 7,
  jobName: 'postTransactionsJob',
  status: 'COMPLETED',
  exitCode: 'COMPLETED_WITH_REJECTS',
  startTime: '2026-08-17T10:00:00',
  endTime: '2026-08-17T10:00:02',
  readCount: 300,
  writeCount: 300,
  posted: 262,
  rejected: 38,
}

const failedRun: JobRunView = {
  runId: 8,
  jobName: 'interestCalcJob',
  status: 'FAILED',
  exitCode: 'FAILED',
  startTime: '2026-08-17T11:00:00',
  endTime: '2026-08-17T11:00:01',
  readCount: 0,
  writeCount: 0,
  posted: null,
  rejected: null,
}

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/admin/jobs']}>
        <AdminJobsPage />
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('AdminJobsPage', () => {
  it('lists runs with status badges and POSTTRAN posted/rejected counts', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn().mockResolvedValue(jsonResponse(200, [postTranRun, failedRun])),
    )
    renderPage()

    expect(await screen.findByText('postTransactionsJob')).toBeInTheDocument()
    expect(screen.getByText('COMPLETED')).toBeInTheDocument()
    expect(screen.getByText('COMPLETED_WITH_REJECTS')).toBeInTheDocument()
    expect(screen.getByText('262 / 38')).toBeInTheDocument()
    // INTCALC has no posted/rejected counters
    expect(screen.getByText('FAILED', { selector: '.badge' })).toBeInTheDocument()
    expect(screen.getByText('—')).toBeInTheDocument()
  })

  it('runs POSTTRAN from its card and shows the outcome banner', async () => {
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'POST') return Promise.resolve(jsonResponse(201, postTranRun))
      return Promise.resolve(jsonResponse(200, []))
    })
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    expect(await screen.findByText('No runs yet. Trigger a job above to see it here.'))
      .toBeInTheDocument()

    // POSTTRAN, INTCALC and (Phase 5) CREASTMT cards
    const buttons = screen.getAllByRole('button', { name: 'Run now' })
    expect(buttons).toHaveLength(3)
    await userEvent.click(buttons[0])

    expect(
      await screen.findByText('postTransactionsJob finished: COMPLETED'),
    ).toBeInTheDocument()
    expect(
      screen.getByText('(exit COMPLETED_WITH_REJECTS, posted 262, rejected 38)'),
    ).toBeInTheDocument()
    const postCall = fetchMock.mock.calls.find(
      ([, init]) => (init as RequestInit | undefined)?.method === 'POST',
    ) as [string, RequestInit]
    expect(postCall[0]).toBe('/api/v1/admin/jobs/postTransactions/run')
  })

  it('surfaces a launch failure as an error banner', async () => {
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'POST') {
        return Promise.resolve(
          jsonResponse(409, { error: 'conflict', message: 'Job already running' }),
        )
      }
      return Promise.resolve(jsonResponse(200, []))
    })
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    const buttons = await screen.findAllByRole('button', { name: 'Run now' })
    await userEvent.click(buttons[1])

    expect(await screen.findByText('Job failed to start.')).toBeInTheDocument()
    expect(screen.getByText('Job already running')).toBeInTheDocument()
  })
})
