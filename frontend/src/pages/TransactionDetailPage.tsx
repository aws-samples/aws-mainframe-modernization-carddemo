import { Link, useNavigate, useParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api, ApiError, retryUnlessNotFound } from '../api/client'
import type { TransactionView } from '../api/types'
import Badge from '../components/Badge'
import { IconBack } from '../components/icons'
import { dateOnly, dateTime, money, txnBadgeColor, txnTypeLabel } from '../format'

/** /transactions/:id — transaction-detail-view mockup (COTRN01C). */
export default function TransactionDetailPage() {
  const { id } = useParams<{ id: string }>()
  const navigate = useNavigate()
  const query = useQuery({
    queryKey: ['transaction', id],
    queryFn: () => api.get<TransactionView>(`/api/v1/transactions/${id}`),
    retry: retryUnlessNotFound,
  })

  if (query.isPending) {
    return <div className="card"><div className="card-body">Loading transaction…</div></div>
  }
  if (query.isError) {
    const notFound = query.error instanceof ApiError && query.error.status === 404
    return (
      <div className="alert error" role="alert">
        <div>
          <b>{notFound ? 'Transaction not found.' : 'Could not load transaction.'}</b>{' '}
          <Link to="/transactions">Back to transactions</Link>
        </div>
      </div>
    )
  }

  const t = query.data

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <button
              type="button"
              className="icon-btn"
              onClick={() => navigate(-1)}
              aria-label="Back to transactions"
              data-component-id="txn-detail-back"
            >
              <IconBack />
            </button>
            <h1 className="page-title">
              Transaction <span className="mono" style={{ fontSize: 19 }}>{t.transactionId}</span>
            </h1>
            <Badge color="green" dot>
              Posted
            </Badge>
          </div>
          <p className="page-sub">
            {txnTypeLabel(t.typeCd)} &middot; posted {dateOnly(t.procTs)}
          </p>
        </div>
      </div>
      <div className="card">
        <div className="card-body" style={{ padding: '24px 24px 20px' }}>
          <div className="balance-hero">
            <div className="amt">{money(t.amount)}</div>
            <Badge color={txnBadgeColor(t.typeCd)}>{txnTypeLabel(t.typeCd)}</Badge>
          </div>
          <div className="divider" />
          <div className="grid-2">
            <dl className="kv">
              <dt>Transaction ID</dt>
              <dd className="mono">{t.transactionId}</dd>
              <dt>Type</dt>
              <dd>{txnTypeLabel(t.typeCd)}</dd>
              <dt>Category</dt>
              <dd>{t.categoryCd ?? '—'}</dd>
              <dt>Source</dt>
              <dd className="mono">{t.source ?? '—'}</dd>
              <dt>Description</dt>
              <dd>{t.description ?? '—'}</dd>
              <dt>Amount</dt>
              <dd className="mono">{money(t.amount)}</dd>
              <dt>Origination</dt>
              <dd className="mono">{dateTime(t.origTs)}</dd>
              <dt>Processed</dt>
              <dd className="mono">{dateTime(t.procTs)}</dd>
            </dl>
            <dl className="kv">
              <dt>Card number</dt>
              <dd className="mono">{t.maskedCardNumber}</dd>
              <dt>Account ID</dt>
              <dd className="mono">
                {t.accountId != null ? (
                  <Link to={`/accounts/${t.accountId}`}>{t.accountId}</Link>
                ) : (
                  '—'
                )}
              </dd>
              <dt>Merchant ID</dt>
              <dd className="mono">{t.merchantId ?? '—'}</dd>
              <dt>Merchant name</dt>
              <dd>{t.merchantName ?? '—'}</dd>
              <dt>Merchant city</dt>
              <dd>{t.merchantCity ?? '—'}</dd>
              <dt>Merchant ZIP</dt>
              <dd className="mono">{t.merchantZip ?? '—'}</dd>
            </dl>
          </div>
        </div>
      </div>
    </>
  )
}
