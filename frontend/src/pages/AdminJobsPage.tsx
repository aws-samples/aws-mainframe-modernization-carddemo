import { useState } from 'react'
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'
import { api, ApiError } from '../api/client'
import type { JobRunView } from '../api/types'
import Badge from '../components/Badge'
import { dateTime } from '../format'

/**
 * /admin/jobs — Phase 4 batch admin (plan §6): trigger the POSTTRAN
 * (CBTRN02C) and INTCALC (CBACT04C) Spring Batch jobs and browse the run
 * history from Spring Batch's own metadata tables (the GDG replacement).
 */

const JOBS = [
  {
    pathName: 'postTransactions',
    title: 'Post daily transactions',
    legacy: 'POSTTRAN — CBTRN02C',
    description:
      'Validates each staged daily transaction and posts it: category balance upsert, ' +
      'account balance/cycle update, transactions insert. Failures land in the rejects table ' +
      '(100 bad card, 101 missing account, 102 over limit, 103 expired — last wins).',
  },
  {
    pathName: 'interestCalc',
    title: 'Calculate interest',
    legacy: 'INTCALC — CBACT04C',
    description:
      'Sweeps category balances in account order, writes one interest transaction per ' +
      'category row (bal × rate ÷ 1200, DEFAULT disclosure-group fallback) and applies ' +
      'each account total with a cycle reset.',
  },
  {
    pathName: 'statements',
    title: 'Generate statements',
    legacy: 'CREASTMT — CBSTM03A/B',
    description:
      'Writes a plain-text and an HTML statement per card (name/address, current balance, ' +
      'FICO score, transaction summary with Total EXP). Statements appear on each ' +
      'account detail page once the job completes.',
  },
] as const

function statusColor(status: string): 'green' | 'red' | 'amber' | 'blue' | 'gray' {
  if (status === 'COMPLETED') return 'green'
  if (status === 'FAILED') return 'red'
  if (status === 'STARTED' || status === 'STARTING') return 'blue'
  return 'gray'
}

export default function AdminJobsPage() {
  const queryClient = useQueryClient()
  const [error, setError] = useState<string | null>(null)
  const [lastRun, setLastRun] = useState<JobRunView | null>(null)

  const runsQuery = useQuery({
    queryKey: ['job-runs'],
    queryFn: () => api.get<JobRunView[]>('/api/v1/admin/jobs/runs'),
  })

  const runMutation = useMutation({
    mutationFn: (pathName: string) =>
      api.post<JobRunView>(`/api/v1/admin/jobs/${pathName}/run`, {}),
    onMutate: () => {
      setError(null)
      setLastRun(null)
    },
    onSuccess: async (run) => {
      setLastRun(run)
      await queryClient.invalidateQueries({ queryKey: ['job-runs'] })
    },
    onError: (err) => {
      setError(err instanceof ApiError ? err.message : 'Could not run the job. Please try again.')
    },
  })

  const runs = runsQuery.data ?? []

  return (
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">Batch jobs</h1>
          <p className="page-sub">
            Run the nightly batch on demand — POSTTRAN and INTCALC, ported from the legacy JCL.
          </p>
        </div>
      </div>

      {error && (
        <div className="alert error" role="alert">
          <div>
            <b>Job failed to start.</b> {error}
          </div>
        </div>
      )}
      {lastRun && (
        <div
          className={`alert ${lastRun.status === 'FAILED' ? 'error' : 'success'}`}
          role="status"
        >
          <div>
            <b>
              {lastRun.jobName} finished: {lastRun.status}
            </b>{' '}
            (exit {lastRun.exitCode}
            {lastRun.posted != null ? `, posted ${lastRun.posted}` : ''}
            {lastRun.rejected != null ? `, rejected ${lastRun.rejected}` : ''})
          </div>
        </div>
      )}

      <div className="grid two" style={{ display: 'grid', gap: 16, gridTemplateColumns: '1fr 1fr' }}>
        {JOBS.map((job) => (
          <div className="card" key={job.pathName} data-component-id={`job-card-${job.pathName}`}>
            <div className="page-head" style={{ marginBottom: 8 }}>
              <div>
                <h2 className="page-title" style={{ fontSize: 18 }}>
                  {job.title}
                </h2>
                <p className="page-sub">{job.legacy}</p>
              </div>
            </div>
            <p style={{ marginTop: 0 }}>{job.description}</p>
            <button
              className="btn btn-primary"
              disabled={runMutation.isPending}
              onClick={() => runMutation.mutate(job.pathName)}
              data-component-id={`run-${job.pathName}`}
            >
              {runMutation.isPending && runMutation.variables === job.pathName
                ? 'Running…'
                : 'Run now'}
            </button>
          </div>
        ))}
      </div>

      <div className="card" style={{ marginTop: 16 }}>
        <h2 className="page-title" style={{ fontSize: 18 }}>
          Run history
        </h2>
        {runsQuery.isLoading ? (
          <p>Loading runs…</p>
        ) : runsQuery.isError ? (
          <div className="alert error" role="alert">
            <div>Could not load the run history.</div>
          </div>
        ) : runs.length === 0 ? (
          <p>No runs yet. Trigger a job above to see it here.</p>
        ) : (
          <table className="data">
            <thead>
              <tr>
                <th>Run</th>
                <th>Job</th>
                <th>Status</th>
                <th>Exit code</th>
                <th>Started</th>
                <th>Ended</th>
                <th>Read</th>
                <th>Written</th>
                <th>Posted / rejected</th>
              </tr>
            </thead>
            <tbody>
              {runs.map((run) => (
                <tr key={run.runId}>
                  <td>{run.runId}</td>
                  <td>{run.jobName}</td>
                  <td>
                    <Badge color={statusColor(run.status)}>{run.status}</Badge>
                  </td>
                  <td>{run.exitCode}</td>
                  <td>{dateTime(run.startTime)}</td>
                  <td>{dateTime(run.endTime)}</td>
                  <td>{run.readCount}</td>
                  <td>{run.writeCount}</td>
                  <td>{run.posted != null ? `${run.posted} / ${run.rejected ?? 0}` : '—'}</td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </div>
    </>
  )
}
