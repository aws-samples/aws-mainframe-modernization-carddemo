import { useState } from 'react'
import { Link, useParams } from 'react-router-dom'
import { useQuery } from '@tanstack/react-query'
import { api, ApiError, fetchBlob, retryUnlessNotFound } from '../api/client'
import type { AccountView, StatementView } from '../api/types'
import Badge from '../components/Badge'
import { cardStatus, dateOnly, expMonth, fullName, money } from '../format'

/**
 * Statements card (Phase 5): the CBSTM03A statement runs indexed for this
 * account — plain-text download plus HTML view, fetched as authed blobs
 * (plain links can't carry the Authorization header). The HTML statement
 * embeds unescaped legacy text (golden-parity bytes), so it is rendered ONLY
 * inside a sandboxed iframe (no allow-scripts / allow-same-origin) instead of
 * a same-origin tab — stored-XSS mitigation.
 */
function StatementsCard({ accountId }: { accountId: number }) {
  const [error, setError] = useState<string | null>(null)
  const [viewer, setViewer] = useState<{ statementId: number; url: string } | null>(null)
  const query = useQuery({
    queryKey: ['statements', accountId],
    queryFn: () => api.get<StatementView[]>(`/api/v1/accounts/${accountId}/statements`),
  })

  function closeViewer() {
    setViewer((v) => {
      if (v) URL.revokeObjectURL(v.url)
      return null
    })
  }

  async function open(statement: StatementView, format: 'txt' | 'html') {
    setError(null)
    try {
      const blob = await fetchBlob(`/api/v1/statements/${statement.id}?format=${format}`)
      const url = URL.createObjectURL(blob)
      if (format === 'html') {
        closeViewer()
        setViewer({ statementId: statement.id, url })
      } else {
        const a = document.createElement('a')
        a.href = url
        a.download = `stmt-${statement.id}-${accountId}.txt`
        a.click()
        setTimeout(() => URL.revokeObjectURL(url), 30_000)
      }
    } catch (err) {
      setError(err instanceof ApiError ? err.message : 'Could not fetch the statement.')
    }
  }

  const statements = query.data ?? []
  return (
    <div className="card">
      <div className="card-head">
        <h2>Statements</h2>
      </div>
      <div className="card-body tight">
        {error && (
          <div className="alert error" role="alert" style={{ margin: 12 }}>
            <div>{error}</div>
          </div>
        )}
        {query.isLoading ? (
          <p style={{ padding: 16 }}>Loading statements…</p>
        ) : query.isError ? (
          <p style={{ padding: 16 }}>Could not load statements.</p>
        ) : statements.length === 0 ? (
          <p style={{ padding: 16, color: 'var(--muted)' }}>
            No statements yet. An administrator can generate them from the batch jobs page.
          </p>
        ) : (
          <table className="data compact">
            <thead>
              <tr>
                <th>Statement</th>
                <th>Cycle date</th>
                <th>Created</th>
                <th></th>
              </tr>
            </thead>
            <tbody>
              {statements.map((s) => (
                <tr key={s.id}>
                  <td className="mono">{s.id}</td>
                  <td>{dateOnly(s.cycleDate)}</td>
                  <td>{s.createdAt ? s.createdAt.slice(0, 19).replace('T', ' ') : '—'}</td>
                  <td>
                    <div className="row-actions">
                      <button
                        className="btn btn-ghost btn-sm"
                        onClick={() => open(s, 'html')}
                        data-component-id={`stmt-view-${s.id}`}
                      >
                        View HTML
                      </button>
                      <button
                        className="btn btn-ghost btn-sm"
                        onClick={() => open(s, 'txt')}
                        data-component-id={`stmt-download-${s.id}`}
                      >
                        Download
                      </button>
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
      {viewer && (
        <div className="overlay" role="dialog" aria-modal="true" aria-label="Statement viewer">
          <div className="modal" style={{ maxWidth: 860 }}>
            <div className="m-head">
              <h2>Statement {viewer.statementId}</h2>
            </div>
            <div className="m-body">
              {/* sandbox with NO tokens: scripts and same-origin access disabled */}
              <iframe
                sandbox=""
                src={viewer.url}
                title={`Statement ${viewer.statementId}`}
                style={{ width: '100%', height: '60vh', border: '1px solid var(--border)' }}
                data-component-id="stmt-viewer-frame"
              />
            </div>
            <div className="m-foot">
              <button
                type="button"
                className="btn btn-secondary"
                onClick={closeViewer}
                data-component-id="stmt-viewer-close"
              >
                Close
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}

/** /accounts/:id — account-detail-two-column mockup (COACTVWC). */
export default function AccountDetailPage() {
  const { id } = useParams<{ id: string }>()
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
          {notFound ? (
            <>
              No account with ID <span className="mono">{id}</span>.{' '}
              <Link to="/accounts">Try another ID</Link>.
            </>
          ) : (
            'Please try again.'
          )}
        </div>
      </div>
    )
  }

  const a = query.data
  const c = a.customer
  const active = a.activeStatus === 'Y'
  const holder = c ? fullName(c.firstName, c.lastName, '—') : '—'

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <h1 className="page-title">
              Account <span className="mono" style={{ fontSize: 19 }}>{a.accountId}</span>
            </h1>
            <Badge color={active ? 'green' : 'gray'} dot>
              {active ? 'Active' : 'Inactive'}
            </Badge>
          </div>
          <p className="page-sub">
            Opened {dateOnly(a.openDate)} &middot; Group{' '}
            <span className="mono">{a.groupId || '—'}</span> &middot; Primary holder: {holder}
          </p>
        </div>
        <div className="head-actions">
          <Link
            className="btn btn-secondary"
            to={`/accounts/${a.accountId}/bill-pay`}
            data-component-id="acct-detail-billpay"
          >
            Bill payment
          </Link>
          <Link
            className="btn btn-primary"
            to={`/accounts/${a.accountId}/edit`}
            data-component-id="acct-detail-edit"
          >
            Edit account
          </Link>
        </div>
      </div>

      <div className="stat-grid">
        <div className="stat">
          <div className="lbl">Current balance</div>
          <div className="val">{money(a.currBal)}</div>
          <div className="delta">of {money(a.creditLimit)} credit limit</div>
        </div>
        <div className="stat">
          <div className="lbl">Credit limit</div>
          <div className="val">{money(a.creditLimit)}</div>
          <div className="delta">Cash limit {money(a.cashCreditLimit)}</div>
        </div>
        <div className="stat">
          <div className="lbl">Cycle credit</div>
          <div className="val">{money(a.currCycCredit)}</div>
          <div className="delta">Current cycle</div>
        </div>
        <div className="stat">
          <div className="lbl">Cycle debit</div>
          <div className="val">{money(a.currCycDebit)}</div>
          <div className="delta">Current cycle</div>
        </div>
      </div>

      <div className="grid-2">
        <div>
          <div className="card">
            <div className="card-head">
              <h2>Account information</h2>
            </div>
            <div className="card-body">
              <dl className="kv">
                <dt>Account ID</dt>
                <dd className="mono">{a.accountId}</dd>
                <dt>Status</dt>
                <dd>
                  <Badge color={active ? 'green' : 'gray'} dot>
                    {active ? 'Active' : 'Inactive'}
                  </Badge>
                </dd>
                <dt>Open date</dt>
                <dd>{dateOnly(a.openDate)}</dd>
                <dt>Expiration date</dt>
                <dd>{dateOnly(a.expirationDate)}</dd>
                <dt>Reissue date</dt>
                <dd>{dateOnly(a.reissueDate)}</dd>
                <dt>Credit limit</dt>
                <dd className="mono">{money(a.creditLimit)}</dd>
                <dt>Cash credit limit</dt>
                <dd className="mono">{money(a.cashCreditLimit)}</dd>
                <dt>Current balance</dt>
                <dd className="mono">{money(a.currBal)}</dd>
                <dt>Current cycle credit</dt>
                <dd className="mono">{money(a.currCycCredit)}</dd>
                <dt>Current cycle debit</dt>
                <dd className="mono">{money(a.currCycDebit)}</dd>
                <dt>Disclosure group</dt>
                <dd className="mono">{a.groupId || '—'}</dd>
              </dl>
            </div>
          </div>
          <div className="card">
            <div className="card-head">
              <h2>Cards on this account</h2>
              <Link className="link-plain" to={`/cards?accountId=${a.accountId}`}>
                Open card list
              </Link>
            </div>
            <div className="card-body tight">
              <table className="data">
                <thead>
                  <tr>
                    <th>Card number</th>
                    <th>Embossed name</th>
                    <th>Expires</th>
                    <th>Status</th>
                    <th></th>
                  </tr>
                </thead>
                <tbody>
                  {a.cards.length === 0 && (
                    <tr>
                      <td colSpan={5} style={{ color: 'var(--muted)' }}>
                        No cards on this account.
                      </td>
                    </tr>
                  )}
                  {a.cards.map((card) => {
                    const st = cardStatus(card.activeStatus)
                    return (
                      <tr key={card.maskedCardNumber}>
                        <td className="mono">{card.maskedCardNumber}</td>
                        <td>{card.embossedName ?? '—'}</td>
                        <td>{expMonth(card.expirationDate)}</td>
                        <td>
                          <Badge color={st.color} dot>
                            {st.label}
                          </Badge>
                        </td>
                        <td>
                          <div className="row-actions">
                            <Link
                              className="btn btn-ghost btn-sm"
                              to={`/cards?accountId=${a.accountId}`}
                            >
                              View
                            </Link>
                          </div>
                        </td>
                      </tr>
                    )
                  })}
                </tbody>
              </table>
            </div>
          </div>
        </div>
        <div>
          <div className="card">
            <div className="card-head">
              <h2>Customer information</h2>
            </div>
            <div className="card-body">
              {!c && <p style={{ color: 'var(--muted)' }}>No customer linked to this account.</p>}
              {c && (
                <dl className="kv">
                  <dt>Customer ID</dt>
                  <dd className="mono">{c.customerId}</dd>
                  <dt>Name</dt>
                  <dd>{[c.firstName, c.middleName, c.lastName].filter(Boolean).join(' ') || '—'}</dd>
                  <dt>SSN</dt>
                  <dd className="mono">{c.ssnLast4 ? `***-**-${c.ssnLast4}` : '—'}</dd>
                  <dt>Date of birth</dt>
                  <dd>{dateOnly(c.dob)}</dd>
                  <dt>FICO score</dt>
                  <dd>{c.ficoScore ?? '—'}</dd>
                  <dt>Address</dt>
                  <dd>
                    {[c.addrLine1, c.addrLine2, c.addrLine3].filter(Boolean).map((l) => (
                      <span key={l}>
                        {l}
                        <br />
                      </span>
                    ))}
                    {[c.addrStateCd, c.addrZip].filter(Boolean).join(' ')}
                    {c.addrCountryCd ? <br /> : null}
                    {c.addrCountryCd}
                  </dd>
                  <dt>Phone 1</dt>
                  <dd className="mono">{c.phoneNum1 ?? '—'}</dd>
                  <dt>Phone 2</dt>
                  <dd className="mono">{c.phoneNum2 ?? '—'}</dd>
                  <dt>Government ID</dt>
                  <dd className="mono">{c.govtIssuedId ?? '—'}</dd>
                  <dt>EFT account</dt>
                  <dd className="mono">{c.eftAccountId ?? '—'}</dd>
                  <dt>Primary card holder</dt>
                  <dd>{c.priCardHolder === 'Y' ? 'Yes' : 'No'}</dd>
                </dl>
              )}
            </div>
          </div>
          <StatementsCard accountId={a.accountId} />
        </div>
      </div>
    </>
  )
}
