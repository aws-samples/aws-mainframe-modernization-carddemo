import { describe, expect, it, vi } from 'vitest'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter, Route, Routes } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import TransactionAddPage from './TransactionAddPage'
import { toLegacyAmount } from '../format'
import { jsonResponse } from '../test/helpers'

function renderPage() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return render(
    <QueryClientProvider client={queryClient}>
      <MemoryRouter initialEntries={['/transactions/new']}>
        <Routes>
          <Route path="/transactions/new" element={<TransactionAddPage />} />
          <Route path="/transactions/:id" element={<h1>Transaction detail</h1>} />
        </Routes>
      </MemoryRouter>
    </QueryClientProvider>,
  )
}

async function fillValidForm() {
  await userEvent.type(screen.getByLabelText(/Account ID/), '2')
  await userEvent.type(screen.getByLabelText(/Amount/), '75.00')
  await userEvent.type(screen.getByLabelText(/Description/), 'Weekly groceries')
  await userEvent.type(screen.getByLabelText(/Merchant ID/), '800000000')
  await userEvent.type(screen.getByLabelText(/Merchant name/), 'Abshire-Lowe')
  await userEvent.type(screen.getByLabelText(/Merchant city/), 'Altenwerthshire')
  await userEvent.type(screen.getByLabelText(/Merchant ZIP/), '12546')
}

describe('toLegacyAmount', () => {
  it('normalizes user amounts to the legacy sign+8.2 format (NUMVAL-C re-format)', () => {
    expect(toLegacyAmount('75.00')).toBe('+00000075.00')
    expect(toLegacyAmount('75')).toBe('+00000075.00')
    expect(toLegacyAmount('75.5')).toBe('+00000075.50')
    expect(toLegacyAmount('-25.99')).toBe('-00000025.99')
    expect(toLegacyAmount('+00000075.00')).toBe('+00000075.00')
    expect(toLegacyAmount('abc')).toBeNull()
    expect(toLegacyAmount('1.234')).toBeNull()
    expect(toLegacyAmount('123456789')).toBeNull()
  })
})

describe('TransactionAddPage', () => {
  it('requires account or card and blocks bad amounts client-side', async () => {
    renderPage()
    await userEvent.type(screen.getByLabelText(/Amount/), 'abc')
    await userEvent.click(screen.getByRole('button', { name: 'Continue to confirm' }))

    expect(
      await screen.findByText('Account or Card Number must be entered...'),
    ).toBeInTheDocument()
    expect(
      screen.getByText('Amount should be in format -99999999.99'),
    ).toBeInTheDocument()
    // still on step 1 — no confirm screen
    expect(screen.queryByText('Review & confirm')).not.toBeInTheDocument()
  })

  it('shows the confirm step and posts the normalized payload once confirmed', async () => {
    const created = { transactionId: '0000001000000042' }
    const fetchMock = vi.fn().mockResolvedValue(jsonResponse(201, created))
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await fillValidForm()
    await userEvent.click(screen.getByRole('button', { name: 'Continue to confirm' }))

    // step 2: review shows the legacy-formatted amount, nothing posted yet
    expect(await screen.findByText('Review & confirm')).toBeInTheDocument()
    expect(screen.getByText('+00000075.00')).toBeInTheDocument()
    expect(fetchMock).not.toHaveBeenCalled()

    await userEvent.click(
      screen.getByRole('button', { name: 'Confirm & post transaction' }),
    )

    // navigated to the new transaction's detail page
    expect(await screen.findByText('Transaction detail')).toBeInTheDocument()
    expect(fetchMock).toHaveBeenCalledTimes(1)
    const [url, init] = fetchMock.mock.calls[0] as [string, RequestInit]
    expect(url).toBe('/api/v1/transactions')
    expect(init.method).toBe('POST')
    const body = JSON.parse(init.body as string)
    expect(body).toMatchObject({
      accountId: '2',
      cardNumber: null,
      typeCd: '01',
      categoryCd: '0001',
      source: 'POS TERM',
      description: 'Weekly groceries',
      amount: '+00000075.00',
      merchantId: '800000000',
      merchantName: 'Abshire-Lowe',
      merchantCity: 'Altenwerthshire',
      merchantZip: '12546',
    })
  })

  it('maps a server field error back onto the form (legacy fail-fast message)', async () => {
    const fetchMock = vi.fn().mockResolvedValue(
      jsonResponse(400, {
        error: 'validation',
        fieldErrors: [{ field: 'accountId', message: 'Account ID NOT found...' }],
      }),
    )
    vi.stubGlobal('fetch', fetchMock)
    renderPage()

    await fillValidForm()
    await userEvent.click(screen.getByRole('button', { name: 'Continue to confirm' }))
    await userEvent.click(
      await screen.findByRole('button', { name: 'Confirm & post transaction' }),
    )

    // back on step 1 with the server message attached to the field
    expect(await screen.findByText('Account ID NOT found...')).toBeInTheDocument()
    expect(screen.queryByText('Review & confirm')).not.toBeInTheDocument()
  })
})
