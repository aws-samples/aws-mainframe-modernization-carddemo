import React, { useState } from 'react';
import { useParams, Link } from 'react-router-dom';
import { api } from '../api.js';
import { Screen, Err } from '../App.jsx';

const money = (v) => (v == null ? '' : Number(v).toLocaleString('en-US', { minimumFractionDigits: 2 }));

export function JobsPage() {
  const [posting, setPosting] = useState(null);
  const [interest, setInterest] = useState(null);
  const [statements, setStatements] = useState(null);
  const [err, setErr] = useState(null);
  const [busy, setBusy] = useState('');

  async function run(name, fn, setter) {
    setErr(null); setBusy(name);
    try { setter(await fn()); }
    catch (e) { setErr(e); }
    finally { setBusy(''); }
  }

  return (
    <Screen title="Batch Jobs" tran="BATCH">
      <Err error={err} />
      <div className="field-row">
        <button disabled={!!busy} onClick={() => run('posting',
          () => api('/api/jobs/posting', { method: 'POST' }), setPosting)}>
          Run Transaction Posting (CBTRN02C)
        </button>
        <button disabled={!!busy} onClick={() => run('interest',
          () => api('/api/jobs/interest', { method: 'POST' }), setInterest)}>
          Run Interest Calculation (CBACT04C)
        </button>
        <button disabled={!!busy} onClick={() => run('statements',
          () => api('/api/jobs/statements', { method: 'POST' }), setStatements)}>
          Run Statement Print (CBSTM03A)
        </button>
      </div>
      {posting && (
        <div>
          <h1>Posting Result</h1>
          <p>Processed {posting.processed}: {posting.posted} posted, {posting.rejected} rejected.</p>
          {posting.rejects.length > 0 && (
            <table>
              <thead><tr><th>Tran ID</th><th>Reason</th><th>Description</th></tr></thead>
              <tbody>
                {posting.rejects.map((r) => (
                  <tr key={r.tranId}><td>{r.tranId}</td><td>{r.reason}</td><td>{r.description}</td></tr>
                ))}
              </tbody>
            </table>
          )}
        </div>
      )}
      {interest && (
        <div>
          <h1>Interest Result</h1>
          <p>{interest.accountsProcessed} accounts, {interest.transactionsCreated} interest transactions,
            total interest {money(interest.totalInterest)}.</p>
          <table>
            <thead><tr><th>Account</th><th className="num">Interest</th></tr></thead>
            <tbody>
              {interest.perAccount.map((a) => (
                <tr key={a.acctId}><td>{String(a.acctId).padStart(11, '0')}</td><td className="num">{money(a.interest)}</td></tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
      {statements && (
        <div>
          <h1>Statements ({statements.length})</h1>
          <table>
            <thead><tr><th>Account</th><th>Customer</th><th className="num">Balance</th><th /></tr></thead>
            <tbody>
              {statements.map((s) => (
                <tr key={s.acctId}>
                  <td>{String(s.acctId).padStart(11, '0')}</td>
                  <td>{s.customerName}</td>
                  <td className="num">{money(s.currentBalance)}</td>
                  <td><Link to={`/admin/jobs/statements/${s.acctId}`}>View</Link></td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </Screen>
  );
}

export function StatementPage() {
  const { acctId } = useParams();
  const [stmt, setStmt] = useState(null);
  const [err, setErr] = useState(null);

  useState(() => {
    api(`/api/jobs/statements/${acctId}`).then(setStmt).catch(setErr);
  });

  return (
    <Screen title="Account Statement" tran="CBSTM03A">
      <Err error={err} />
      {stmt && <pre className="stmt">{stmt.text}</pre>}
    </Screen>
  );
}
