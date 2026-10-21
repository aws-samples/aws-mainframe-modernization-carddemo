import React, { useEffect, useState } from 'react';
import { useNavigate, useParams } from 'react-router-dom';
import { api } from '../api.js';
import { Screen, Err, usePfKeys } from '../App.jsx';

const money = (v) => (v == null ? '' : Number(v).toLocaleString('en-US', { minimumFractionDigits: 2 }));
function Row({ label, value }) {
  return <div className="field-row"><label>{label}</label><input readOnly value={value ?? ''} /></div>;
}

export function TranListPage() {
  const [data, setData] = useState(null);
  const [page, setPage] = useState(0);
  const [err, setErr] = useState(null);
  const nav = useNavigate();

  useEffect(() => {
    setErr(null);
    api(`/api/transactions?page=${page}`).then(setData).catch(setErr);
  }, [page]);

  usePfKeys({
    F7: () => { if (data?.hasPrevious) setPage((p) => p - 1); },
    F8: () => { if (data?.hasNext) setPage((p) => p + 1); },
  });

  return (
    <Screen title="Transaction List" tran="COTRN00C"
      pf={['F3=Back', 'F7=Backward', 'F8=Forward', 'F12=Logout']}>
      <Err error={err} />
      <table>
        <thead>
          <tr><th>Tran ID</th><th>Date</th><th>Description</th><th className="num">Amount</th></tr>
        </thead>
        <tbody>
          {data?.content.map((t) => (
            <tr key={t.tranId} className="sel" onClick={() => nav(`/transactions/${t.tranId}`)}>
              <td>{t.tranId}</td>
              <td>{(t.origTs || '').slice(0, 10)}</td>
              <td>{t.description}</td>
              <td className="num">{money(t.amt)}</td>
            </tr>
          ))}
        </tbody>
      </table>
      <div className="pager">
        <button disabled={!data?.hasPrevious} onClick={() => setPage((p) => p - 1)}>F7 Prev</button>
        <span>Page {(data?.page ?? 0) + 1} of {data?.totalPages ?? 0} (10 per page)</span>
        <button disabled={!data?.hasNext} onClick={() => setPage((p) => p + 1)}>F8 Next</button>
      </div>
    </Screen>
  );
}

export function TranViewPage() {
  const { tranId } = useParams();
  const [id, setId] = useState('');
  const [tran, setTran] = useState(null);
  const [err, setErr] = useState(null);
  const nav = useNavigate();

  useEffect(() => {
    if (tranId) {
      api(`/api/transactions/${tranId}`).then(setTran).catch(setErr);
    }
  }, [tranId]);

  return (
    <Screen title="Transaction View" tran="COTRN01C">
      {!tran && (
        <form onSubmit={(e) => { e.preventDefault(); nav(`/transactions/${id.trim()}`); }}
          className="field-row">
          <label>Transaction ID</label>
          <input value={id} onChange={(e) => setId(e.target.value)} maxLength={16} autoFocus />
          <button type="submit">ENTER</button>
        </form>
      )}
      <Err error={err} />
      {tran && (
        <div className="grid2">
          <div>
            <Row label="Transaction ID" value={tran.tranId} />
            <Row label="Type Code" value={tran.typeCd} />
            <Row label="Category Code" value={tran.catCd} />
            <Row label="Source" value={tran.source} />
            <Row label="Description" value={tran.description} />
            <Row label="Amount" value={money(tran.amt)} />
          </div>
          <div>
            <Row label="Card Number" value={tran.cardNum} />
            <Row label="Merchant ID" value={tran.merchantId} />
            <Row label="Merchant Name" value={tran.merchantName} />
            <Row label="Merchant City" value={tran.merchantCity} />
            <Row label="Merchant Zip" value={tran.merchantZip} />
            <Row label="Orig Timestamp" value={tran.origTs} />
            <Row label="Proc Timestamp" value={tran.procTs} />
          </div>
        </div>
      )}
    </Screen>
  );
}

export function TranAddPage() {
  const [form, setForm] = useState({
    accountId: '', cardNum: '', typeCd: '', catCd: '', source: 'POS TERM',
    description: '', amt: '', merchantId: '', merchantName: '', merchantCity: '',
    merchantZip: '', origDate: '', procDate: '', confirm: '',
  });
  const [err, setErr] = useState(null);
  const [ok, setOk] = useState(null);
  const set = (k, v) => setForm((f) => ({ ...f, [k]: v }));

  usePfKeys({ F4: () => document.getElementById('tranClrBtn')?.click() });

  async function submit(e) {
    e.preventDefault();
    setErr(null); setOk(null);
    try {
      const body = {
        accountId: form.accountId ? Number(form.accountId) : null,
        cardNum: form.cardNum || null, typeCd: form.typeCd || null,
        catCd: form.catCd ? Number(form.catCd) : null, source: form.source,
        description: form.description, amt: form.amt,
        merchantId: form.merchantId || null, merchantName: form.merchantName,
        merchantCity: form.merchantCity, merchantZip: form.merchantZip,
        origDate: form.origDate || null, procDate: form.procDate || null,
        confirm: form.confirm || null,
      };
      const t = await api('/api/transactions', { method: 'POST', body });
      setOk(`Transaction ${t.tranId} added successfully`);
    } catch (e2) { setErr(e2); }
  }

  const In = ({ label, k, max, w }) => (
    <div className="field-row">
      <label>{label}</label>
      <input value={form[k]} onChange={(e) => set(k, e.target.value)} maxLength={max}
        style={w ? { minWidth: w } : undefined} />
    </div>
  );

  return (
    <Screen title="Add Transaction" tran="COTRN02C" pf={['F3=Back', 'F4=Clear', 'F12=Logout']}>
      <Err error={err} />
      {ok && <div className="msg ok">{ok}</div>}
      <form onSubmit={submit} className="grid2">
        <div>
          {In({ label: "Account ID (or Card #)", k: "accountId", max: 11 })}
          {In({ label: "Card Number", k: "cardNum", max: 16 })}
          {In({ label: "Type CD", k: "typeCd", max: 2, w: 50 })}
          {In({ label: "Cat CD", k: "catCd", max: 4, w: 60 })}
          {In({ label: "Source", k: "source", max: 10 })}
          {In({ label: "Description", k: "description", max: 100 })}
          {In({ label: "Amount", k: "amt", max: 12, w: 120 })}
        </div>
        <div>
          {In({ label: "Merchant ID", k: "merchantId", max: 9, w: 120 })}
          {In({ label: "Merchant Name", k: "merchantName", max: 50 })}
          {In({ label: "Merchant City", k: "merchantCity", max: 50 })}
          {In({ label: "Merchant Zip", k: "merchantZip", max: 10, w: 110 })}
          {In({ label: "Orig Date (YYYY-MM-DD)", k: "origDate", max: 10, w: 110 })}
          {In({ label: "Proc Date (YYYY-MM-DD)", k: "procDate", max: 10, w: 110 })}
          {In({ label: "Confirm (Y/N)", k: "confirm", max: 1, w: 40 })}
        </div>
        <div className="field-row">
          <button type="submit">Add Transaction</button>
          <button id="tranClrBtn" type="button" onClick={() => setForm({
            accountId: '', cardNum: '', typeCd: '', catCd: '', source: 'POS TERM',
            description: '', amt: '', merchantId: '', merchantName: '', merchantCity: '',
            merchantZip: '', origDate: '', procDate: '', confirm: '',
          })}>Clear (F4)</button>
        </div>
      </form>
    </Screen>
  );
}
