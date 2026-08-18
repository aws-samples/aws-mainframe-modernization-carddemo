import { useState } from 'react'
import { Link, useNavigate, useParams } from 'react-router-dom'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { api, ApiError, retryUnlessNotFound } from '../api/client'
import type { AccountView, TransactionView } from '../api/types'
import { IconBack } from '../components/icons'
import { dateOnly, fullName, money } from '../format'

/**
 * /accounts/:id/bill-pay — bill-pay-confirm / bill-pay-zero-balance mockups
 * (COBIL00C). Full-balance payment only; the checkbox is the legacy Y/N
 * confirmation, and the POST body {confirm: true} is issued once, on Pay.
 * A zero (or negative) balance shows the legacy "You have nothing to pay."
 * state instead of the pay form.
 */
export default function BillPayPage() {
  const { id } = useParams<{ id: string }>()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const [checked, setChecked] = useState(false)
  const [paying, setPaying] = useState(false)
  const [serverError, setServerError] = useState<string | null>(null)

  const query = useQuery({
    queryKey: ['account', id],
    queryFn: () => api.get<AccountView>(`/api/v1/accounts/${id}`),
    retry: retryUnlessNotFound,
  })

  if (query.isPending) {
    return <div className="card"><div className="card-body">Loading account…</div></div>
  }
  if (query.isError) {
    const notFound = query.error instanceof ApiError && query.error.status === 404
    return (
      <div className="alert error" role="alert">
        <div>
          <b>{notFound ? 'Account not found.' : 'Could not load account.'}</b>{' '}
          <Link to="/accounts">Back to accounts</Link>
        </div>
      </div>
    )
  }

  const a = query.data
  const holder = a.customer ? fullName(a.customer.firstName, a.customer.lastName, '—') : '—'
  const card = a.cards[0]
  const hasBalance = a.currBal > 0

  const pay = async () => {
    setServerError(null)
    setPaying(true)
    try {
      const created = await api.post<TransactionView>(
        `/api/v1/accounts/${a.accountId}/bill-payment`,
        { confirm: true },
      )
      await queryClient.invalidateQueries({ queryKey: ['account', id] })
      navigate(`/transactions/${created.transactionId}`)
    } catch (err) {
      if (err instanceof ApiError && err.fieldErrors.length > 0) {
        setServerError(err.fieldErrors[0].message)
      } else if (err instanceof ApiError) {
        setServerError(err.message)
      } else {
        setServerError('Payment failed. Please try again.')
      }
      await queryClient.invalidateQueries({ queryKey: ['account', id] })
    } finally {
      setPaying(false)
    }
  }

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <Link
              className="icon-btn"
              to={`/accounts/${a.accountId}`}
              aria-label="Back to account"
              data-component-id="billpay-back"
            >
              <IconBack size={15} />
            </Link>
            <h1 className="page-title">Bill payment</h1>
          </div>
          <p className="page-sub">
            Account <span className="mono">{a.accountId}</span> &middot; {holder}
          </p>
        </div>
      </div>
      <div className="card" style={{ maxWidth: 640 }}>
        <div className="card-head">
          <h2>Pay full balance</h2>
        </div>
        <div className="card-body">
          {!hasBalance ? (
            <>
              <div className="alert info" style={{ marginBottom: 0 }} role="status">
                <div>
                  <b>You have nothing to pay.</b> The current balance on this account is{' '}
                  {money(a.currBal)}, so no payment can be made.
                </div>
              </div>
              <div className="divider" />
              <dl className="kv">
                <dt>Current balance</dt>
                <dd className="mono">{money(a.currBal)}</dd>
                <dt>Credit limit</dt>
                <dd className="mono">{money(a.creditLimit)}</dd>
              </dl>
            </>
          ) : (
            <>
              {serverError && (
                <div className="alert error" role="alert">
                  <div>
                    <b>Could not pay.</b> {serverError}
                  </div>
                </div>
              )}
              <div
                className="stat"
                style={{
                  border: '1px solid var(--primary-border)',
                  background: 'var(--primary-soft)',
                }}
              >
                <div className="lbl" style={{ color: '#1e3a8a' }}>
                  Current balance
                </div>
                <div className="val" style={{ fontSize: 30 }}>
                  {money(a.currBal)}
                </div>
                <div className="delta" style={{ color: '#3653a0' }}>
                  As of {dateOnly(new Date().toISOString())} &middot; credit limit{' '}
                  {money(a.creditLimit)}
                </div>
              </div>
              <div className="divider" />
              <dl className="kv">
                <dt>Payment amount</dt>
                <dd className="mono" style={{ fontWeight: 700 }}>
                  {money(a.currBal)} (full balance)
                </dd>
                <dt>Payment method</dt>
                <dd>Online bill payment</dd>
                <dt>Applied to card</dt>
                <dd className="mono">{card ? card.maskedCardNumber : '—'}</dd>
                <dt>Posted as</dt>
                <dd>02 Payment — BILL PAYMENT - ONLINE</dd>
              </dl>
              <div className="divider" />
              <label className="check">
                <input
                  type="checkbox"
                  checked={checked}
                  onChange={(e) => setChecked(e.target.checked)}
                  data-component-id="billpay-confirm-check"
                />
                <span>
                  I confirm payment of the full current balance of <b>{money(a.currBal)}</b>
                </span>
              </label>
            </>
          )}
        </div>
        <div className="form-foot">
          <div className="note">
            {hasBalance ? 'Partial payments are not supported on this screen.' : ''}
          </div>
          {hasBalance ? (
            <div style={{ display: 'flex', gap: 10 }}>
              <Link
                className="btn btn-secondary"
                to={`/accounts/${a.accountId}`}
                data-component-id="billpay-cancel"
              >
                Cancel
              </Link>
              <button
                className="btn btn-primary"
                type="button"
                disabled={!checked || paying}
                onClick={pay}
                data-component-id="billpay-pay"
              >
                {paying ? 'Paying…' : `Pay ${money(a.currBal)}`}
              </button>
            </div>
          ) : (
            <Link
              className="btn btn-secondary"
              to={`/accounts/${a.accountId}`}
              data-component-id="billpay-back-btn"
            >
              Back to account
            </Link>
          )}
        </div>
      </div>
    </>
  )
}
