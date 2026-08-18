import { Link, useNavigate, useParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api, ApiError, retryUnlessNotFound } from '../api/client'
import type { CardDetail } from '../api/types'
import Badge from '../components/Badge'
import { IconBack } from '../components/icons'
import { cardStatus, dateOnly } from '../format'

/** /cards/:cardNumber — card detail (COCRDSLC): full number allowed here only. */
export default function CardDetailPage() {
  const { cardNumber } = useParams<{ cardNumber: string }>()
  const navigate = useNavigate()
  const query = useQuery({
    queryKey: ['card', cardNumber],
    queryFn: () => api.get<CardDetail>(`/api/v1/cards/${cardNumber}`),
    retry: retryUnlessNotFound,
  })

  if (query.isPending) {
    return <div className="card"><div className="card-body">Loading card…</div></div>
  }
  if (query.isError) {
    const notFound = query.error instanceof ApiError && query.error.status === 404
    return (
      <div className="alert error" role="alert">
        <div>
          <b>{notFound ? 'Card not found.' : 'Could not load card.'}</b>{' '}
          <Link to="/cards">Back to card list</Link>
        </div>
      </div>
    )
  }

  const c = query.data
  const st = cardStatus(c.activeStatus)

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <button
              type="button"
              className="icon-btn"
              onClick={() => navigate(-1)}
              aria-label="Back"
              data-component-id="card-detail-back"
            >
              <IconBack />
            </button>
            <h1 className="page-title">
              Card <span className="mono" style={{ fontSize: 19 }}>{c.maskedCardNumber}</span>
            </h1>
            <Badge color={st.color} dot>
              {st.label}
            </Badge>
          </div>
          <p className="page-sub">
            Account <span className="mono">{c.accountId}</span> &middot; expires{' '}
            {dateOnly(c.expirationDate)}
          </p>
        </div>
        <div className="head-actions">
          <Link
            className="btn btn-primary"
            to={`/cards/${c.cardNumber}/edit`}
            data-component-id="card-detail-edit"
          >
            Edit card
          </Link>
        </div>
      </div>
      <div className="card">
        <div className="card-head">
          <h2>Card information</h2>
        </div>
        <div className="card-body">
          <dl className="kv">
            <dt>Card number</dt>
            <dd className="mono">{c.cardNumber}</dd>
            <dt>Account ID</dt>
            <dd className="mono">
              <Link to={`/accounts/${c.accountId}`}>{c.accountId}</Link>
            </dd>
            <dt>Embossed name</dt>
            <dd>{c.embossedName ?? '—'}</dd>
            <dt>Expiration date</dt>
            <dd>{dateOnly(c.expirationDate)}</dd>
            <dt>Status</dt>
            <dd>
              <Badge color={st.color} dot>
                {st.label}
              </Badge>
            </dd>
          </dl>
        </div>
      </div>
    </>
  )
}
