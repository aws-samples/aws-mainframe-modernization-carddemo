import { useState } from 'react'
import type { FormEvent } from 'react'
import { useNavigate } from 'react-router-dom'

/** /accounts — account-ID lookup entry (legacy COACTVW is fetch-by-ID only). */
export default function AccountsPage() {
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
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">Accounts</h1>
          <p className="page-sub">
            Look up an account by ID to view balances, customer details and cards.
          </p>
        </div>
      </div>
      <div className="card">
        <form className="toolbar" style={{ borderBottom: 0 }} onSubmit={onSubmit}>
          <div className="field" style={{ minWidth: 280 }}>
            <label htmlFor="acct-id">Account ID</label>
            <input
              id="acct-id"
              className={`input mono${error ? ' invalid' : ''}`}
              placeholder="11 digits, e.g. 00000000002"
              data-component-id="accounts-lookup-id"
              value={accountId}
              onChange={(e) => setAccountId(e.target.value)}
            />
            {error && <div className="error-text">{error}</div>}
            <div className="hint">Seeded accounts are 1–50.</div>
          </div>
          <button className="btn btn-primary" type="submit" data-component-id="accounts-lookup-open">
            Open account
          </button>
        </form>
      </div>
    </>
  )
}
