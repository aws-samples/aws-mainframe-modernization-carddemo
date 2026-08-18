import { useState } from 'react'
import type { FormEvent } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api } from '../api/client'
import type { Page, TransactionView } from '../api/types'
import { useAuth } from '../auth/AuthContext'
import Badge from '../components/Badge'
import { IconAccounts, IconCard, IconList } from '../components/icons'
import { dateOnly, money, txnBadgeColor, txnTypeLabel } from '../format'

/** Account-ID search box — the entry point to /accounts/:id (COACTVW menu path). */
function AccountLookup() {
  const [accountId, setAccountId] = useState('')
  const [error, setError] = useState<string | null>(null)
  const navigate = useNavigate()

  const onSubmit = (e: FormEvent) => {
    e.preventDefault()
    const id = accountId.trim()
    if (!/^\d{1,11}$/.test(id)) {
      setError('Enter a numeric account ID (up to 11 digits).')
      return
    }
    setError(null)
    navigate(`/accounts/${Number(id)}`)
  }

  return (
    <form className="toolbar" style={{ borderBottom: 0 }} onSubmit={onSubmit}>
      <div className="field" style={{ minWidth: 260 }}>
        <label htmlFor="dash-acct">Account ID</label>
        <input
          id="dash-acct"
          className={`input mono${error ? ' invalid' : ''}`}
          placeholder="11 digits, e.g. 00000000002"
          data-component-id="dash-account-lookup"
          value={accountId}
          onChange={(e) => setAccountId(e.target.value)}
        />
        {error && <div className="error-text">{error}</div>}
      </div>
      <button className="btn btn-primary" type="submit" data-component-id="dash-account-open">
        Open account
      </button>
    </form>
  )
}

/** Dashboard — dashboard-overview (ADMIN) / dashboard-tiles-user (USER) mockups. */
export default function DashboardPage() {
  const { user } = useAuth()
  const recent = useQuery({
    queryKey: ['transactions', 'recent'],
    queryFn: () => api.get<Page<TransactionView>>('/api/v1/transactions?limit=5'),
  })

  const isAdmin = user?.role === 'ADMIN'
  const firstName = user?.firstName?.trim() || user?.userId

  return (
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">{isAdmin ? 'Dashboard' : 'Main menu'}</h1>
          <p className="page-sub">
            {isAdmin
              ? `Welcome back, ${firstName}. Here is the latest portfolio activity.`
              : `Signed in as ${user?.userId} · standard user. Administration items are hidden for this role.`}
          </p>
        </div>
      </div>

      <div className="card">
        <div className="card-head">
          <h2>Account view</h2>
        </div>
        <AccountLookup />
      </div>

      <div className="card">
        <div className="card-head">
          <h2>Quick actions</h2>
        </div>
        <div className="card-body">
          <div className="tile-grid">
            <Link className="quick" to="/accounts" data-component-id="quick-account-view">
              <div className="qi">
                <IconAccounts size={18} />
              </div>
              <div className="qt">Account view</div>
              <div className="qd">Look up account &amp; customer details</div>
            </Link>
            <Link className="quick" to="/cards" data-component-id="quick-card-list">
              <div className="qi">
                <IconCard size={18} />
              </div>
              <div className="qt">Card list</div>
              <div className="qd">Browse cards, filter by account</div>
            </Link>
            <Link className="quick" to="/transactions" data-component-id="quick-transaction-list">
              <div className="qi">
                <IconList size={18} />
              </div>
              <div className="qt">Transaction list</div>
              <div className="qd">Paginated transaction history</div>
            </Link>
          </div>
        </div>
      </div>

      <div className="card">
        <div className="card-head">
          <h2>Recent transactions</h2>
          <Link className="link-plain" to="/transactions">
            View all
          </Link>
        </div>
        <div className="card-body tight">
          {recent.isPending && <div style={{ padding: 20 }}>Loading…</div>}
          {recent.isError && (
            <div style={{ padding: 20 }} className="error-text">
              Could not load transactions.
            </div>
          )}
          {recent.data && (
            <table className="data">
              <thead>
                <tr>
                  <th>Transaction ID</th>
                  <th>Date</th>
                  <th>Type</th>
                  <th>Merchant</th>
                  <th className="num">Amount</th>
                </tr>
              </thead>
              <tbody>
                {recent.data.items.map((t) => (
                  <tr key={t.transactionId}>
                    <td className="mono">
                      <Link to={`/transactions/${t.transactionId}`}>{t.transactionId}</Link>
                    </td>
                    <td>{dateOnly(t.origTs)}</td>
                    <td>
                      <Badge color={txnBadgeColor(t.typeCd)}>{txnTypeLabel(t.typeCd)}</Badge>
                    </td>
                    <td>{t.merchantName ?? '—'}</td>
                    <td className={`num${t.amount < 0 ? ' amt-neg' : ''}`}>{money(t.amount)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          )}
        </div>
      </div>
    </>
  )
}
