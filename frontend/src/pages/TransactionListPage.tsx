import { useState } from 'react'
import type { FormEvent } from 'react'
import { Link, useSearchParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api/client'
import type { Page, TransactionView } from '../api/types'
import Badge from '../components/Badge'
import { dateTime, money, txnBadgeColor, txnTypeLabel } from '../format'

const PAGE_SIZE = 20

/**
 * /transactions — transaction-list-table mockup (COTRN00C).
 * Forward-only keyset pagination with a client-side Previous cursor stack;
 * the "Transaction ID" toolbar field is the COTRN00C seek (fromId, inclusive).
 */
export default function TransactionListPage() {
  const [searchParams, setSearchParams] = useSearchParams()
  const cursor = searchParams.get('cursor') ?? ''
  const fromId = searchParams.get('fromId') ?? ''
  const [cursorStack, setCursorStack] = useState<string[]>([])
  const [seekId, setSeekId] = useState(fromId)
  const [formError, setFormError] = useState<string | null>(null)

  const query = useQuery({
    queryKey: ['transactions', fromId, cursor],
    queryFn: () => {
      const qs = new URLSearchParams()
      qs.set('limit', String(PAGE_SIZE))
      if (fromId) qs.set('fromId', fromId)
      if (cursor) qs.set('cursor', cursor)
      return api.get<Page<TransactionView>>(`/api/v1/transactions?${qs}`)
    },
  })

  const applySeek = (e: FormEvent) => {
    e.preventDefault()
    const id = seekId.trim()
    if (id && !/^\d{1,16}$/.test(id)) {
      setFormError('Transaction ID must be numeric (up to 16 digits).')
      return
    }
    setFormError(null)
    setCursorStack([])
    const next = new URLSearchParams()
    if (id) next.set('fromId', id)
    setSearchParams(next)
  }

  const clearSeek = () => {
    setSeekId('')
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
          <h1 className="page-title">Transactions</h1>
          <p className="page-sub">Posted transactions across all accounts, in ID order.</p>
        </div>
        <div className="head-actions">
          <Link
            className="btn btn-primary"
            to="/transactions/new"
            data-component-id="txn-list-add"
          >
            Add transaction
          </Link>
        </div>
      </div>
      <div className="card">
        <form className="toolbar" onSubmit={applySeek}>
          <div className="field">
            <label htmlFor="t-id">Transaction ID</label>
            <input
              id="t-id"
              className="input mono"
              placeholder="Jump to 16-digit ID"
              data-component-id="txn-filter-id"
              value={seekId}
              onChange={(e) => setSeekId(e.target.value)}
            />
          </div>
          <button className="btn btn-primary" type="submit" data-component-id="txn-filter-apply">
            Search
          </button>
          <div className="spacer" />
          <button
            className="btn btn-secondary"
            type="button"
            onClick={clearSeek}
            data-component-id="txn-filter-clear"
          >
            Clear
          </button>
          {formError && <div className="error-text" style={{ width: '100%' }}>{formError}</div>}
        </form>
        {query.isPending && <div style={{ padding: 20 }}>Loading transactions…</div>}
        {query.isError && (
          <div style={{ padding: 20 }} className="error-text">
            Could not load transactions.
          </div>
        )}
        {query.data && (
          <>
            <table className="data">
              <thead>
                <tr>
                  <th>Transaction ID</th>
                  <th>Date &amp; time</th>
                  <th>Type</th>
                  <th>Merchant</th>
                  <th>Card</th>
                  <th className="num">Amount</th>
                  <th style={{ textAlign: 'right' }}>Actions</th>
                </tr>
              </thead>
              <tbody>
                {items.length === 0 && (
                  <tr>
                    <td colSpan={7} style={{ color: 'var(--muted)' }}>
                      No transactions found.
                    </td>
                  </tr>
                )}
                {items.map((t) => (
                  <tr key={t.transactionId}>
                    <td className="mono">
                      <Link to={`/transactions/${t.transactionId}`}>{t.transactionId}</Link>
                    </td>
                    <td style={{ whiteSpace: 'nowrap' }}>{dateTime(t.origTs)}</td>
                    <td>
                      <Badge color={txnBadgeColor(t.typeCd)}>{txnTypeLabel(t.typeCd)}</Badge>
                    </td>
                    <td>{t.merchantName ?? '—'}</td>
                    <td className="mono">{t.maskedCardNumber}</td>
                    <td className={`num${t.amount < 0 ? ' amt-neg' : ''}`}>{money(t.amount)}</td>
                    <td>
                      <div className="row-actions">
                        <Link
                          className="btn btn-ghost btn-sm"
                          to={`/transactions/${t.transactionId}`}
                          aria-label={`View transaction ${t.transactionId}`}
                        >
                          View
                        </Link>
                      </div>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
            <div className="pagebar">
              <div>
                Showing <b>{items.length}</b> transaction{items.length === 1 ? '' : 's'}
                {fromId && (
                  <>
                    {' '}
                    from ID <b className="mono">{fromId}</b>
                  </>
                )}
              </div>
              <div className="pager">
                <button
                  className="pgbtn"
                  type="button"
                  disabled={cursorStack.length === 0}
                  onClick={goPrev}
                  data-component-id="txn-page-prev"
                >
                  Previous
                </button>
                <button
                  className="pgbtn"
                  type="button"
                  disabled={!query.data.nextCursor}
                  onClick={goNext}
                  data-component-id="txn-page-next"
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
