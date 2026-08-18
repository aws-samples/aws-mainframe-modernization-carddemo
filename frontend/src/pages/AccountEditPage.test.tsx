import { describe, expect, it, vi } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import AccountEditPage from './AccountEditPage'
import type { AccountView } from '../api/types'
import { jsonResponse } from '../test/helpers'

const account: AccountView = {
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
    middleName: 'April',
    lastName: 'Rosenbaum',
    addrLine1: '4917 Myrna Flats',
    addrLine2: 'Apt 453',
    addrLine3: 'West Bernita',
    addrStateCd: 'TX',
    addrCountryCd: 'USA',
    addrZip: '75001',
    phoneNum1: '(214)706-9510',
    phoneNum2: null,
    ssnLast4: '8382',
    govtIssuedId: null,
    dob: '1961-10-08',
    eftAccountId: '0000005062',
    priCardHolder: 'Y',
    ficoScore: 711,
    version: 5,
  },
  cards: [],
}

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/accounts/2/edit']}>
        <Routes>
          <Route path="/accounts/:id/edit" element={<AccountEditPage />} />
          <Route path="/accounts/:id" element={<h1>Account detail</h1>} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

describe('AccountEditPage', () => {
  it('loads the account into the form and blocks invalid FICO/state on submit', async () => {
    vi.stubGlobal('fetch', vi.fn().mockResolvedValue(jsonResponse(200, account)))
    renderPage()

    const fico = await screen.findByLabelText(/FICO score/)
    expect(fico).toHaveValue('711')
    expect(screen.getByLabelText(/First name/)).toHaveValue('Enrico')
    // SSN shown masked
    expect(screen.getByLabelText(/SSN/)).toHaveValue('***-**-8382')

    await userEvent.clear(fico)
    await userEvent.type(fico, '299')
    const state = screen.getByLabelText(/State/)
    await userEvent.clear(state)
    await userEvent.type(state, 'X')
    await userEvent.click(screen.getByRole('button', { name: 'Save changes' }))

    expect(
      await screen.findByText('FICO Score: should be between 300 and 850'),
    ).toBeInTheDocument()
    expect(screen.getByText('State must be a 2 letter code.')).toBeInTheDocument()
    expect(screen.getByText(/fields need attention/)).toBeInTheDocument()
    // no PUT was sent — the only fetch call is the initial GET
    const calls = (fetch as ReturnType<typeof vi.fn>).mock.calls
    expect(calls.every(([, init]) => !init || init.method === undefined)).toBe(true)
  })

  it('PUTs with versions and null ssn when unchanged, then navigates back', async () => {
    const fetchMock = vi
      .fn()
      .mockImplementation(() => Promise.resolve(jsonResponse(200, account)))
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    const first = await screen.findByLabelText(/First name/)
    await userEvent.clear(first)
    await userEvent.type(first, 'Enrique')
    await userEvent.click(screen.getByRole('button', { name: 'Save changes' }))

    await waitFor(() => expect(screen.getByText('Account detail')).toBeInTheDocument())
    const putCall = fetchMock.mock.calls.find(([, init]) => init?.method === 'PUT')
    expect(putCall?.[0]).toBe('/api/v1/accounts/2')
    const body = JSON.parse(putCall?.[1]?.body as string)
    expect(body.accountVersion).toBe(3)
    expect(body.customerVersion).toBe(5)
    expect(body.ssn).toBeNull() // masked value untouched -> keep stored SSN
    expect(body.firstName).toBe('Enrique')
  })

  it('shows the conflict banner on a 409 stale-version save', async () => {
    const fetchMock = vi
      .fn()
      .mockResolvedValueOnce(jsonResponse(200, account))
      .mockResolvedValueOnce(
        jsonResponse(409, {
          error: 'conflict',
          message: 'Record changed by some one else. Please review',
        }),
      )
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await screen.findByLabelText(/First name/)
    await userEvent.click(screen.getByRole('button', { name: 'Save changes' }))

    expect(
      await screen.findByText(/Record changed by some one else. Please review/),
    ).toBeInTheDocument()
  })

  it('maps server fieldErrors onto the form on 400', async () => {
    const fetchMock = vi
      .fn()
      .mockResolvedValueOnce(jsonResponse(200, account))
      .mockResolvedValueOnce(
        jsonResponse(400, {
          error: 'validation',
          message: 'State: is not a valid state code',
          fieldErrors: [{ field: 'stateCd', message: 'State: is not a valid state code' }],
        }),
      )
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    const state = await screen.findByLabelText(/State/)
    await userEvent.clear(state)
    await userEvent.type(state, 'XX')
    await userEvent.click(screen.getByRole('button', { name: 'Save changes' }))

    expect(await screen.findByText('State: is not a valid state code')).toBeInTheDocument()
  })
})
