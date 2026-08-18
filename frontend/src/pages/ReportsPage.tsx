import { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { api, ApiError, fetchBlob } from '../api/client'
import type { ReportRequest, ReportRunView } from '../api/types'
import Badge from '../components/Badge'
import { dateTime } from '../format'

/**
 * /reports — CORPT00C port (reports-stacked mockup): request a monthly /
 * yearly / custom-range transaction report (TRANREPT / CBTRN03C) and download
 * completed runs. The legacy Y/N confirm loop is the browser confirm dialog;
 * the server re-runs the whole CORPT00C edit chain and reports the first
 * failing edit with the exact legacy message.
 */

type Period = 'monthly' | 'yearly' | 'custom'

/** Split a user-typed YYYY-MM-DD into the per-part CORPT00C fields. */
function dateParts(v: string): { month: string; day: string; year: string } {
  const [year = '', month = '', day = ''] = v.trim().split('-')
  return { month, day, year }
}

function statusColor(status: string): 'green' | 'red' | 'amber' | 'blue' | 'gray' {
  if (status === 'COMPLETED') return 'green'
  if (status === 'FAILED') return 'red'
  if (status === 'PENDING') return 'amber'
  return 'gray'
}

export default function ReportsPage() {
  const queryClient = useQueryClient()
  const [period, setPeriod] = useState<Period>('custom')
  const [startDate, setStartDate] = useState('')
  const [endDate, setEndDate] = useState('')
  const [error, setError] = useState<string | null>(null)
  const [notice, setNotice] = useState<string | null>(null)

  const runsQuery = useQuery({
    queryKey: ['report-runs'],
    queryFn: () => api.get<ReportRunView[]>('/api/v1/reports'),
  })

  const submitMutation = useMutation({
    mutationFn: (body: ReportRequest) =>
      api.post<ReportRunView>('/api/v1/reports/transaction', body),
    onMutate: () => {
      setError(null)
      setNotice(null)
    },
    onSuccess: async (run) => {
      setNotice(run.message ?? 'Report submitted for printing ...')
      await queryClient.invalidateQueries({ queryKey: ['report-runs'] })
    },
    onError: (err) => {
      setError(
        err instanceof ApiError ? err.message : 'Could not submit the report. Please try again.',
      )
    },
  })

  function submit() {
    // CORPT00C confirm loop ('Please confirm to print the ... report...')
    const label = period === 'monthly' ? 'Monthly' : period === 'yearly' ? 'Yearly' : 'Custom'
    if (!window.confirm(`Please confirm to print the ${label} report...`)) return
    const body: ReportRequest = { reportType: period }
    if (period === 'custom') {
      const s = dateParts(startDate)
      const e = dateParts(endDate)
      body.startMonth = s.month
      body.startDay = s.day
      body.startYear = s.year
      body.endMonth = e.month
      body.endDay = e.day
      body.endYear = e.year
    }
    submitMutation.mutate(body)
  }

  async function download(run: ReportRunView) {
    setError(null)
    try {
      const blob = await fetchBlob(`/api/v1/reports/${run.id}/download`)
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `tranrept-${run.id}.txt`
      a.click()
      URL.revokeObjectURL(url)
    } catch (err) {
      setError(err instanceof ApiError ? err.message : 'Could not download the report.')
    }
  }

  const runs = runsQuery.data ?? []

  return (
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">Reports</h1>
          <p className="page-sub">Request transaction reports and download completed runs.</p>
        </div>
      </div>

      {error && (
        <div className="alert error" role="alert">
          <div>{error}</div>
        </div>
      )}
      {notice && (
        <div className="alert success" role="status">
          <div>{notice}</div>
        </div>
      )}

      <div className="card">
        <div className="card-head">
          <h2>Request a transaction report</h2>
        </div>
        <div className="card-body">
          <div className="field" style={{ marginBottom: 16 }}>
            <label>
              Report period <span className="req">*</span>
            </label>
            <div className="radio-row" role="radiogroup" aria-label="Report period">
              <label className="radio">
                <input
                  type="radio"
                  name="period"
                  checked={period === 'monthly'}
                  onChange={() => setPeriod('monthly')}
                  data-component-id="report-period-monthly"
                />{' '}
                Monthly (current month)
              </label>
              <label className="radio">
                <input
                  type="radio"
                  name="period"
                  checked={period === 'yearly'}
                  onChange={() => setPeriod('yearly')}
                  data-component-id="report-period-yearly"
                />{' '}
                Yearly (current year)
              </label>
              <label className="radio">
                <input
                  type="radio"
                  name="period"
                  checked={period === 'custom'}
                  onChange={() => setPeriod('custom')}
                  data-component-id="report-period-custom"
                />{' '}
                Custom date range
              </label>
            </div>
          </div>
          {period === 'custom' && (
            <div className="form-grid" style={{ maxWidth: 520 }}>
              <div className="field">
                <label htmlFor="r-start">
                  Start date <span className="req">*</span>
                </label>
                <input
                  id="r-start"
                  className="input mono"
                  value={startDate}
                  onChange={(e) => setStartDate(e.target.value)}
                  placeholder="YYYY-MM-DD"
                  data-component-id="report-start-date"
                />
                <div className="hint">YYYY-MM-DD</div>
              </div>
              <div className="field">
                <label htmlFor="r-end">
                  End date <span className="req">*</span>
                </label>
                <input
                  id="r-end"
                  className="input mono"
                  value={endDate}
                  onChange={(e) => setEndDate(e.target.value)}
                  placeholder="YYYY-MM-DD"
                  data-component-id="report-end-date"
                />
              </div>
            </div>
          )}
          <div className="hint" style={{ marginTop: 4 }}>
            The report runs as a background job. It appears below and can be downloaded when it
            completes.
          </div>
        </div>
        <div className="form-foot">
          <div className="note">Reports include page totals and grand totals per account.</div>
          <button
            className="btn btn-primary"
            onClick={submit}
            disabled={submitMutation.isPending}
            data-component-id="report-submit"
          >
            {submitMutation.isPending ? 'Submitting…' : 'Run report'}
          </button>
        </div>
      </div>

      <div className="card">
        <div className="card-head">
          <h2>Past report runs</h2>
          <button
            className="icon-btn"
            aria-label="Refresh list"
            onClick={() => runsQuery.refetch()}
            data-component-id="report-refresh"
          >
            ⟳
          </button>
        </div>
        <div className="card-body tight">
          {runsQuery.isLoading ? (
            <p style={{ padding: 16 }}>Loading report runs…</p>
          ) : runsQuery.isError ? (
            <p style={{ padding: 16 }}>Could not load the report runs.</p>
          ) : runs.length === 0 ? (
            <p style={{ padding: 16, color: 'var(--muted)' }}>
              No report runs yet. Request one above.
            </p>
          ) : (
            <table className="data">
              <thead>
                <tr>
                  <th>Run ID</th>
                  <th>Type</th>
                  <th>Date range</th>
                  <th>Requested</th>
                  <th>Status</th>
                  <th></th>
                </tr>
              </thead>
              <tbody>
                {runs.map((run) => (
                  <tr key={run.id}>
                    <td className="mono">{run.id}</td>
                    <td>{run.reportType}</td>
                    <td className="mono">
                      {run.fromDate} → {run.toDate}
                    </td>
                    <td>
                      {run.requestedBy} · {dateTime(run.createdAt)}
                    </td>
                    <td>
                      <Badge color={statusColor(run.status)}>{run.status}</Badge>
                    </td>
                    <td>
                      <div className="row-actions">
                        {run.status === 'COMPLETED' && (
                          <button
                            className="btn btn-ghost btn-sm"
                            onClick={() => download(run)}
                            data-component-id={`report-download-${run.id}`}
                          >
                            Download
                          </button>
                        )}
                      </div>
                    </td>
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
