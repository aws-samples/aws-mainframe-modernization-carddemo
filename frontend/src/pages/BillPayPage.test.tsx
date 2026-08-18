import { describe, expect, it, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import BillPayPage from './BillPayPage'
import type { AccountView } from '../api/types'
import { jsonResponse } from '../test/helpers'

const baseAccount: AccountView = {
  accountId: 2,
  activeStatus: 'Y',
  currBal: 5448,
  creditLimit: 6130,
  cashCreditLimit: 1580,
  openDate: '2013-06-19',
  expirationDate: '2027-08-11',
  reissueDate: '2024-08-11',
  currCycCredit: 1912.4,
  currCycDebit: 482.15,
  zip: '75001',
  groupId: 'A000000000',
  version: 3,
  customer: {
    customerId: 2,
    firstName: 'Enrico',
    middleName: null,
    lastName: 'Rosenbaum',
    addrLine1: null,
    addrLine2: null,
    addrLine3: null,
    addrStateCd: null,
    addrCountryCd: null,
    addrZip: null,
    phoneNum1: null,
    phoneNum2: null,
    ssnLast4: '8382',
    govtIssuedId: null,
    dob: null,
    eftAccountId: null,
    priCardHolder: 'Y',
    ficoScore: 711,
    version: 5,
  },
  cards: [
    {
      cardNumber: null,
      maskedCardNumber: '****-****-****-4733',
      accountId: 2,
      embossedName: 'ENRICO ROSENBAUM',
      expirationDate: '2027-08-11',
      activeStatus: 'Y',
    },
  ],
}

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/accounts/2/bill-pay']}>
        <Routes>
          <Route path="/accounts/:id/bill-pay" element={<BillPayPage />} />
          <Route path="/accounts/:id" element={<h1>Account detail</h1>} />
          <Route path="/transactions/:id" element={<h1>Transaction detail</h1>} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('BillPayPage', () => {
  it('gates the pay button behind the confirm checkbox and posts {confirm:true}', async () => {
    const created = { transactionId: '0000001000000099' }
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'POST') return Promise.resolve(jsonResponse(201, created))
      return Promise.resolve(jsonResponse(200, baseAccount))
    })
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    // COBIL00C confirm flow: pay disabled until the checkbox is ticked
    const payButton = await screen.findByRole('button', { name: 'Pay $5,448.00' })
    expect(payButton).toBeDisabled()
    expect(screen.getByText('****-****-****-4733')).toBeInTheDocument()
    expect(screen.getByText('02 Payment — BILL PAYMENT - ONLINE')).toBeInTheDocument()
    expect(screen.getByText('$5,448.00 (full balance)')).toBeInTheDocument()

    await userEvent.click(screen.getByRole('checkbox'))
    expect(payButton).toBeEnabled()
    await userEvent.click(payButton)

    // navigated to the payment transaction's detail
    expect(await screen.findByText('Transaction detail')).toBeInTheDocument()
    const postCall = fetchMock.mock.calls.find(
      ([, init]) => (init as RequestInit | undefined)?.method === 'POST',
    ) as [string, RequestInit]
    expect(postCall[0]).toBe('/api/v1/accounts/2/bill-payment')
    expect(JSON.parse(postCall[1].body as string)).toEqual({ confirm: true })
  })

  it('shows the zero-balance state with no pay button (COBIL00C nothing-to-pay)', async () => {
    vi.stubGlobal(
      'fetch',
      vi.fn().mockResolvedValue(jsonResponse(200, { ...baseAccount, currBal: 0 })),
    )
    renderPage()

    expect(await screen.findByText('You have nothing to pay.')).toBeInTheDocument()
    expect(screen.queryByRole('checkbox')).not.toBeInTheDocument()
    expect(screen.queryByRole('button', { name: /^Pay / })).not.toBeInTheDocument()
    // both the head icon and the footer button link back to the account
    expect(screen.getAllByRole('link', { name: 'Back to account' })).toHaveLength(2)
  })

  it('surfaces the legacy server message when the balance was already paid', async () => {
    const fetchMock = vi.fn().mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === 'POST') {
        return Promise.resolve(
          jsonResponse(400, {
            error: 'validation',
            fieldErrors: [{ field: 'accountId', message: 'You have nothing to pay...' }],
          }),
        )
      }
      return Promise.resolve(jsonResponse(200, baseAccount))
    })
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await userEvent.click(await screen.findByRole('checkbox'))
    await userEvent.click(screen.getByRole('button', { name: 'Pay $5,448.00' }))

    expect(await screen.findByText('You have nothing to pay...')).toBeInTheDocument()
  })
})
