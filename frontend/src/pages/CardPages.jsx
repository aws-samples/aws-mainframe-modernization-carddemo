import React, { useEffect, useState } from 'react';
import { useNavigate, useParams, useLocation } from 'react-router-dom';
import { api } from '../api.js';
import { Screen, Err, usePfKeys } from '../App.jsx';

export function CardListPage() {
  const [data, setData] = useState(null);
  const [page, setPage] = useState(0);
  const [err, setErr] = useState(null);
  const nav = useNavigate();

  useEffect(() => {
    setErr(null);
    api(`/api/cards?page=${page}`).then(setData).catch(setErr);
  }, [page]);

  usePfKeys({
    F7: () => { if (data?.hasPrevious) setPage((p) => p - 1); },
    F8: () => { if (data?.hasNext) setPage((p) => p + 1); },
  });

  return (
    <Screen title="Credit Card List" tran="COCRDLIC"
      pf={['F3=Back', 'F7=Backward', 'F8=Forward', 'F12=Logout']}>
      <Err error={err} />
      <table>
        <thead>
          <tr><th>Card Number</th><th>Acct</th><th>Name</th><th>Status</th><th /></tr>
        </thead>
        <tbody>
          {data?.content.map((c) => (
            <tr key={c.cardNum} className="sel" onClick={() => nav(`/cards/${c.cardNum}`)}>
              <td>{c.cardNum}</td>
              <td>{String(c.acctId).padStart(11, '0')}</td>
              <td>{c.embossedName}</td>
              <td>{c.activeStatus}</td>
              <td><button className="link">S</button></td>
            </tr>
          ))}
        </tbody>
      </table>
      <div className="pager">
        <button disabled={!data?.hasPrevious} onClick={() => setPage((p) => p - 1)}>F7 Prev</button>
        <span>Page {(data?.page ?? 0) + 1} of {data?.totalPages ?? 0} (7 per page)</span>
        <button disabled={!data?.hasNext} onClick={() => setPage((p) => p + 1)}>F8 Next</button>
      </div>
    </Screen>
  );
}

export function CardDetailPage({ edit = false }) {
  const { cardNum } = useParams();
  const loc = useLocation();
  const [num, setNum] = useState('');
  const [card, setCard] = useState(null);
  const [form, setForm] = useState(null);
  const [err, setErr] = useState(null);
  const [ok, setOk] = useState(null);
  const nav = useNavigate();
  const isEdit = edit || loc.pathname.endsWith('/edit');

  useEffect(() => {
    if (cardNum) load(cardNum);
    // eslint-disable-next-line
  }, [cardNum]);

  usePfKeys({ F5: () => document.getElementById('cardSaveBtn')?.click() });

  async function load(id) {
    setErr(null); setOk(null);
    try {
      const c = await api(`/api/cards/${id}`);
      setCard(c);
      setForm({ embossedName: c.embossedName, activeStatus: c.activeStatus,
        expiryMonth: c.expirationDate?.slice(5, 7) ?? '', expiryYear: c.expirationDate?.slice(0, 4) ?? '' });
    } catch (e) { setErr(e); setCard(null); }
  }

  async function save(e) {
    e.preventDefault();
    setErr(null); setOk(null);
    try {
      const updated = await api(`/api/cards/${card.cardNum}`, { method: 'PUT', body: form });
      setCard(updated);
      setOk('Card updated successfully');
    } catch (e2) { setErr(e2); }
  }

  return (
    <Screen title={isEdit ? 'Credit Card Update' : 'Credit Card View'}
      tran={isEdit ? 'COCRDUPC' : 'COCRDSLC'}
      pf={isEdit ? ['F3=Back', 'F5=Save', 'F12=Logout'] : ['F3=Back', 'F12=Logout']}>
      {!card && (
        <form onSubmit={(e) => { e.preventDefault(); nav(`/${isEdit ? `cards/${num.trim()}/edit` : `cards/${num.trim()}`}`); }}
          className="field-row">
          <label>Card Number</label>
          <input value={num} onChange={(e) => setNum(e.target.value)} maxLength={16} autoFocus />
          <button type="submit">ENTER</button>
        </form>
      )}
      <Err error={err} />
      {ok && <div className="msg ok">{ok}</div>}
      {card && !isEdit && (
        <div>
          <div className="field-row"><label>Card Number</label><input readOnly value={card.cardNum} /></div>
          <div className="field-row"><label>Account Id</label><input readOnly value={String(card.acctId).padStart(11, '0')} /></div>
          <div className="field-row"><label>Name on Card</label><input readOnly value={card.embossedName} /></div>
          <div className="field-row"><label>Status</label><input readOnly value={card.activeStatus} /></div>
          <div className="field-row"><label>Expiry Date</label><input readOnly value={card.expirationDate} /></div>
          <div className="field-row"><label>CVV</label><input readOnly value={card.cvvCd} /></div>
          <button onClick={() => nav(`/cards/${card.cardNum}/edit`)}>Update (F4)</button>
        </div>
      )}
      {card && isEdit && form && (
        <form onSubmit={save}>
          <div className="field-row"><label>Card Number</label><input readOnly value={card.cardNum} /></div>
          <div className="field-row"><label>Name on Card</label>
            <input value={form.embossedName} onChange={(e) => setForm({ ...form, embossedName: e.target.value })} maxLength={50} /></div>
          <div className="field-row"><label>Card Status (Y/N)</label>
            <input value={form.activeStatus} onChange={(e) => setForm({ ...form, activeStatus: e.target.value })} maxLength={1} style={{ minWidth: 40 }} /></div>
          <div className="field-row"><label>Expiry Month (MM)</label>
            <input value={form.expiryMonth} onChange={(e) => setForm({ ...form, expiryMonth: e.target.value })} maxLength={2} style={{ minWidth: 50 }} /></div>
          <div className="field-row"><label>Expiry Year (YYYY)</label>
            <input value={form.expiryYear} onChange={(e) => setForm({ ...form, expiryYear: e.target.value })} maxLength={4} style={{ minWidth: 70 }} /></div>
          <div className="field-row"><button id="cardSaveBtn" type="submit">Save (F5)</button></div>
        </form>
      )}
    </Screen>
  );
}
