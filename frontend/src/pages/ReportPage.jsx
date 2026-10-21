import React, { useState } from 'react';
import { api } from '../api.js';
import { Screen, Err, usePfKeys } from '../App.jsx';

const money = (v) => (v == null ? '' : Number(v).toLocaleString('en-US', { minimumFractionDigits: 2 }));

export default function ReportPage() {
  const [type, setType] = useState('monthly');
  const [from, setFrom] = useState('');
  const [to, setTo] = useState('');
  const [report, setReport] = useState(null);
  const [err, setErr] = useState(null);

  usePfKeys({ F5: () => document.getElementById('rptBtn')?.click() });

  async function run(e) {
    e?.preventDefault();
    setErr(null); setReport(null);
    const params = new URLSearchParams({ type });
    if (type === 'custom') {
      if (from) params.set('from', from);
      if (to) params.set('to', to);
    }
    try { setReport(await api(`/api/reports/transactions?${params}`)); }
    catch (e2) { setErr(e2); }
  }

  return (
    <Screen title="Transaction Reports" tran="CORPT00C" pf={['F3=Back', 'F5=Run', 'F12=Logout']}>
      <div className="field-row">
        <label>Report Type</label>
        <label><input type="radio" name="rt" checked={type === 'monthly'} onChange={() => setType('monthly')} style={{ minWidth: 20 }} /> Monthly (current month)</label>
        <label><input type="radio" name="rt" checked={type === 'yearly'} onChange={() => setType('yearly')} style={{ minWidth: 20 }} /> Yearly (current year)</label>
        <label><input type="radio" name="rt" checked={type === 'custom'} onChange={() => setType('custom')} style={{ minWidth: 20 }} /> Custom date range</label>
      </div>
      {type === 'custom' && (
        <form onSubmit={run}>
          <div className="field-row">
            <label>Start Date (YYYY-MM-DD)</label>
            <input value={from} onChange={(e) => setFrom(e.target.value)} maxLength={10} style={{ minWidth: 120 }} />
          </div>
          <div className="field-row">
            <label>End Date (YYYY-MM-DD)</label>
            <input value={to} onChange={(e) => setTo(e.target.value)} maxLength={10} style={{ minWidth: 120 }} />
          </div>
        </form>
      )}
      <div className="field-row"><button id="rptBtn" onClick={run}>Submit Report (F5)</button></div>
      <Err error={err} />
      {report && (
        <div>
          <h1>{report.reportName}</h1>
          <p>Date Range: {report.startDate} to {report.endDate}</p>
          <table>
            <thead><tr><th>Tran ID</th><th>Type</th><th>Category</th><th>Source</th><th>Description</th><th className="num">Amount</th></tr></thead>
            <tbody>
              {report.transactions.map((t) => (
                <tr key={t.tranId}>
                  <td>{t.tranId}</td><td>{t.typeCd}</td><td>{t.catCd}</td>
                  <td>{t.source}</td><td>{t.description}</td>
                  <td className="num">{money(t.amt)}</td>
                </tr>
              ))}
            </tbody>
          </table>
          <div className="field-row"><label>Page Total</label><input readOnly value={money(report.pageTotal)} /></div>
          <div className="field-row"><label>Grand Total</label><input readOnly value={money(report.grandTotal)} /></div>
        </div>
      )}
    </Screen>
  );
}
