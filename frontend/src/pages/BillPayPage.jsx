import React, { useState } from 'react';
import { api } from '../api.js';
import { Screen, Err } from '../App.jsx';

const money = (v) => (v == null ? '' : Number(v).toLocaleString('en-US', { minimumFractionDigits: 2 }));

export default function BillPayPage() {
  const [acctId, setAcctId] = useState('');
  const [acct, setAcct] = useState(null);
  const [confirm, setConfirm] = useState('');
  const [result, setResult] = useState(null);
  const [err, setErr] = useState(null);

  async function load(e) {
    e?.preventDefault();
    setErr(null); setResult(null);
    try { setAcct(await api(`/api/accounts/${acctId.trim()}`)); }
    catch (e2) { setErr(e2); }
  }

  async function pay(e) {
    e.preventDefault();
    setErr(null); setResult(null);
    try {
      const r = await api(`/api/accounts/${acctId.trim()}/payment`,
        { method: 'POST', body: { confirm } });
      setResult(r);
    } catch (e2) { setErr(e2); }
  }

  return (
    <Screen title="Bill Payment" tran="COBIL00C">
      <form onSubmit={load} className="field-row">
        <label>Account Number</label>
        <input value={acctId} onChange={(e) => { setAcctId(e.target.value); setAcct(null); setResult(null); }}
          maxLength={11} autoFocus />
        <button type="submit">ENTER</button>
      </form>
      <Err error={err} />
      {acct && !result && (
        <div>
          <div className="field-row"><label>Current Balance</label><input readOnly value={money(acct.currBal)} /></div>
          {Number(acct.currBal) <= 0
            ? <div className="msg ok">You have nothing to pay...</div>
            : (
              <form onSubmit={pay}>
                <div className="field-row">
                  <label>Confirm. Pay full balance? (Y/N)</label>
                  <input value={confirm} onChange={(e) => setConfirm(e.target.value)} maxLength={1} style={{ minWidth: 40 }} />
                </div>
                <div className="field-row"><label /><button type="submit">Make Payment</button></div>
              </form>
            )}
        </div>
      )}
      {result && (
        <div>
          <div className="msg ok">{result.message || 'Payment successful'}</div>
          <div className="field-row"><label>Transaction ID</label><input readOnly value={result.tranId} /></div>
          <div className="field-row"><label>Payment Amount</label><input readOnly value={money(result.paymentAmount)} /></div>
          <div className="field-row"><label>New Balance</label><input readOnly value={money(result.newBalance)} /></div>
        </div>
      )}
    </Screen>
  );
}
