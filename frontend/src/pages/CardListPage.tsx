import { useState } from 'react'
import type { FormEvent } from 'react'
import { Link, useNavigate, useSearchParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api/client'
import type { CardSummary, Page } from '../api/types'
import Badge from '../components/Badge'
import { cardStatus, expMonth } from '../format'

const PAGE_SIZE = 20

/**
 * /cards — card-list-comfortable mockup (COCRDLIC).
 * Keyset pagination is forward-only on the server; Previous is a client-side
 * cursor stack. Full card numbers never appear in list responses (plan §5), so
 * detail is reached by entering a full 16-digit number in the toolbar.
 */
export default function CardListPage() {
  const [searchParams, setSearchParams] = useSearchParams()
  const navigate = useNavigate()

  const accountIdParam = searchParams.get('accountId') ?? ''
  const cursor = searchParams.get('cursor') ?? ''
  // client-side Previous: stack of prior cursors ('' = first page)
  const [cursorStack, setCursorStack] = useState<string[]>([])

  const [accountFilter, setAccountFilter] = useState(accountIdParam)
  const [cardNumber, setCardNumber] = useState('')
  const [formError, setFormError] = useState<string | null>(null)

  const query = useQuery({
    queryKey: ['cards', accountIdParam, cursor],
    queryFn: () => {
      const qs = new URLSearchParams()
      qs.set('limit', String(PAGE_SIZE))
      if (accountIdParam) qs.set('accountId', accountIdParam)
      if (cursor) qs.set('cursor', cursor)
      return api.get<Page<CardSummary>>(`/api/v1/cards?${qs}`)
    },
  })

  const applyFilters = (e: FormEvent) => {
    e.preventDefault()
    const card = cardNumber.trim()
    if (card) {
      if (!/^\d{16}$/.test(card)) {
        setFormError('Card number must be the full 16 digits.')
        return
      }
      setFormError(null)
      navigate(`/cards/${card}`)
      return
    }
    const acct = accountFilter.trim()
    if (acct && !/^\d{1,11}$/.test(acct)) {
      setFormError('Account ID must be numeric (up to 11 digits).')
      return
    }
    setFormError(null)
    setCursorStack([])
    const next = new URLSearchParams()
    if (acct) next.set('accountId', String(Number(acct)))
    setSearchParams(next)
  }

  const clearFilters = () => {
    setAccountFilter('')
    setCardNumber('')
    setFormError(null)
    setCursorStack([])
    setSearchParams(new URLSearchParams())
  }

  const goNext = () => {
    const nextCursor = query.data?.nextCursor
    if (!nextCursor) return
    setCursorStack((s) => [...s, cursor])
    const next = new URLSearchParams(searchParams)
    next.set('cursor', nextCursor)
    setSearchParams(next)
  }

  const goPrev = () => {
    if (cursorStack.length === 0) return
    const prev = cursorStack[cursorStack.length - 1]
    setCursorStack((s) => s.slice(0, -1))
    const next = new URLSearchParams(searchParams)
    if (prev) next.set('cursor', prev)
    else next.delete('cursor')
    setSearchParams(next)
  }

  const items = query.data?.items ?? []

  return (
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">Cards</h1>
          <p className="page-sub">
            Browse cards, filter by account. Card numbers are masked in lists — enter a full card
            number to open its detail.
          </p>
        </div>
      </div>
      <div className="card">
        <form className="toolbar" onSubmit={applyFilters}>
          <div className="field">
            <label htmlFor="f-acct">Account ID</label>
            <input
              id="f-acct"
              className="input mono"
              placeholder="11 digits, e.g. 00000000002"
              data-component-id="cards-filter-account"
              value={accountFilter}
              onChange={(e) => setAccountFilter(e.target.value)}
            />
          </div>
          <div className="field">
            <label htmlFor="f-card">Card number</label>
            <input
              id="f-card"
              className="input mono"
              placeholder="Full 16 digits"
              data-component-id="cards-filter-card"
              value={cardNumber}
              onChange={(e) => setCardNumber(e.target.value)}
            />
          </div>
          <button className="btn btn-primary" type="submit" data-component-id="cards-filter-apply">
            Search
          </button>
          <div className="spacer" />
          <button
            className="btn btn-secondary"
            type="button"
            onClick={clearFilters}
            data-component-id="cards-filter-clear"
          >
            Clear filters
          </button>
          {formError && <div className="error-text" style={{ width: '100%' }}>{formError}</div>}
        </form>
        {query.isPending && <div style={{ padding: 20 }}>Loading cards…</div>}
        {query.isError && (
          <div style={{ padding: 20 }} className="error-text">
            Could not load cards.
          </div>
        )}
        {query.data && (
          <>
            <table className="data">
              <thead>
                <tr>
                  <th>Card number</th>
                  <th>Account ID</th>
                  <th>Embossed name</th>
                  <th>Expires</th>
                  <th>Status</th>
                  <th style={{ textAlign: 'right' }}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {items.length === 0 && (
                  <tr>
                    <td colSpan={6} style={{ color: 'var(--muted)' }}>
                      No cards match the current filters.
                    </td>
                  </tr>
                )}
                {items.map((c) => {
                  const st = cardStatus(c.activeStatus)
                  return (
                    <tr key={c.maskedCardNumber + c.accountId}>
                      <td className="mono">{c.maskedCardNumber}</td>
                      <td className="mono">
                        <Link to={`/accounts/${c.accountId}`}>{c.accountId}</Link>
                      </td>
                      <td>{c.embossedName ?? '—'}</td>
                      <td className="mono">{expMonth(c.expirationDate)}</td>
                      <td>
                        <Badge color={st.color} dot>
                          {st.label}
                        </Badge>
                      </td>
                      <td>
                        <div className="row-actions">
                          <Link className="btn btn-ghost btn-sm" to={`/accounts/${c.accountId}`}>
                            Account
                          </Link>
                        </div>
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
            <div className="pagebar">
              <div>
                Showing <b>{items.length}</b> card{items.length === 1 ? '' : 's'}
                {accountIdParam && (
                  <>
                    {' '}
                    for account <b className="mono">{accountIdParam}</b>
                  </>
                )}
              </div>
              <div className="pager">
                <button
                  className="pgbtn"
                  type="button"
                  disabled={cursorStack.length === 0}
                  onClick={goPrev}
                  data-component-id="cards-page-prev"
                >
                  Previous
                </button>
                <button
                  className="pgbtn"
                  type="button"
                  disabled={!query.data.nextCursor}
                  onClick={goNext}
                  data-component-id="cards-page-next"
                >
                  Next
                </button>
              </div>
            </div>
          </>
        )}
      </div>
    </>
  )
}
