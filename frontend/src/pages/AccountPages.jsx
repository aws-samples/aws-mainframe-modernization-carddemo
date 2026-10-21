import React, { useState } from 'react';
import { api } from '../api.js';
import { Screen, Err, usePfKeys } from '../App.jsx';

const money = (v) => (v == null ? '' : Number(v).toLocaleString('en-US', { minimumFractionDigits: 2 }));

function Row({ label, value }) {
  return <div className="field-row"><label>{label}</label><input readOnly value={value ?? ''} /></div>;
}

export function AccountViewPage() {
  const [acctId, setAcctId] = useState('');
  const [acct, setAcct] = useState(null);
  const [err, setErr] = useState(null);

  async function load(e) {
    e?.preventDefault();
    setErr(null); setAcct(null);
    try { setAcct(await api(`/api/accounts/${acctId.trim()}`)); }
    catch (e2) { setErr(e2); }
  }

  const c = acct?.customer;
  return (
    <Screen title="Account View" tran="COACTVWC">
      <form onSubmit={load} className="field-row">
        <label>Account Number</label>
        <input value={acctId} onChange={(e) => setAcctId(e.target.value)} maxLength={11} autoFocus />
        <button type="submit">ENTER</button>
      </form>
      <Err error={err} />
      {acct && (
        <div className="grid2">
          <div>
            <Row label="Account Number" value={String(acct.acctId).padStart(11, '0')} />
            <Row label="Active Status" value={acct.activeStatus} />
            <Row label="Current Balance" value={money(acct.currBal)} />
            <Row label="Credit Limit" value={money(acct.creditLimit)} />
            <Row label="Cash Credit Limit" value={money(acct.cashCreditLimit)} />
            <Row label="Open Date" value={acct.openDate} />
            <Row label="Expiry Date" value={acct.expirationDate} />
            <Row label="Reissue Date" value={acct.reissueDate} />
            <Row label="Curr Cyc Credit" value={money(acct.currCycCredit)} />
            <Row label="Curr Cyc Debit" value={money(acct.currCycDebit)} />
            <Row label="Account Group" value={acct.groupId} />
          </div>
          {c && (
            <div>
              <Row label="Customer Id" value={String(c.custId).padStart(9, '0')} />
              <Row label="Name" value={`${c.firstName} ${c.middleName} ${c.lastName}`} />
              <Row label="Address" value={c.addrLine1} />
              <Row label="" value={c.addrLine2} />
              <Row label="" value={`${c.addrLine3 ?? ''} ${c.addrStateCd} ${c.addrZip}`} />
              <Row label="Country" value={c.addrCountryCd} />
              <Row label="Phone 1" value={c.phoneNum1} />
              <Row label="Phone 2" value={c.phoneNum2} />
              <Row label="SSN" value={c.ssn} />
              <Row label="DOB" value={c.dob} />
              <Row label="FICO Score" value={c.ficoCreditScore} />
            </div>
          )}
        </div>
      )}
    </Screen>
  );
}

export function AccountUpdatePage() {
  const [acctId, setAcctId] = useState('');
  const [form, setForm] = useState(null);
  const [err, setErr] = useState(null);
  const [ok, setOk] = useState(null);

  usePfKeys({ F5: () => document.getElementById('acctSaveBtn')?.click() });

  async function load(e) {
    e?.preventDefault();
    setErr(null); setOk(null);
    try {
      const a = await api(`/api/accounts/${acctId.trim()}`);
      setForm({ ...a, customer: a.customer ? { ...a.customer } : {} });
    } catch (e2) { setErr(e2); }
  }

  function set(k, v) { setForm((f) => ({ ...f, [k]: v })); }
  function setC(k, v) { setForm((f) => ({ ...f, customer: { ...f.customer, [k]: v } })); }

  async function save(e) {
    e.preventDefault();
    setErr(null); setOk(null);
    const body = {
      activeStatus: form.activeStatus,
      creditLimit: form.creditLimit === '' ? null : Number(form.creditLimit),
      cashCreditLimit: form.cashCreditLimit === '' ? null : Number(form.cashCreditLimit),
      openDate: form.openDate, expirationDate: form.expirationDate, reissueDate: form.reissueDate,
      currCycCredit: form.currCycCredit === '' ? null : Number(form.currCycCredit),
      currCycDebit: form.currCycDebit === '' ? null : Number(form.currCycDebit),
      addrZip: form.addrZip, groupId: form.groupId,
      customer: form.customer ? {
        firstName: form.customer.firstName, middleName: form.customer.middleName,
        lastName: form.customer.lastName, addrLine1: form.customer.addrLine1,
        addrLine2: form.customer.addrLine2, addrLine3: form.customer.addrLine3,
        addrStateCd: form.customer.addrStateCd, addrCountryCd: form.customer.addrCountryCd,
        addrZip: form.customer.addrZip, phoneNum1: form.customer.phoneNum1,
        phoneNum2: form.customer.phoneNum2,
        ssn: form.customer.ssn === '' || form.customer.ssn == null ? null : Number(form.customer.ssn),
        govtIssuedId: form.customer.govtIssuedId, dob: form.customer.dob,
        eftAccountId: form.customer.eftAccountId, priCardHolderInd: form.customer.priCardHolderInd,
        ficoCreditScore: form.customer.ficoCreditScore === '' || form.customer.ficoCreditScore == null
          ? null : Number(form.customer.ficoCreditScore),
      } : null,
    };
    try {
      const updated = await api(`/api/accounts/${acctId.trim()}`, { method: 'PUT', body });
      setForm({ ...updated, customer: updated.customer ? { ...updated.customer } : {} });
      setOk('Account updated successfully');
    } catch (e2) { setErr(e2); }
  }

  const In = ({ label, k, max }) => (
    <div className="field-row">
      <label>{label}</label>
      <input value={form?.[k] ?? ''} onChange={(e) => set(k, e.target.value)} maxLength={max} />
    </div>
  );
  const CIn = ({ label, k, max }) => (
    <div className="field-row">
      <label>{label}</label>
      <input value={form?.customer?.[k] ?? ''} onChange={(e) => setC(k, e.target.value)} maxLength={max} />
    </div>
  );

  return (
    <Screen title="Account Update" tran="COACTUPC" pf={['F3=Back', 'F5=Save', 'F12=Logout']}>
      <form onSubmit={load} className="field-row">
        <label>Account Number</label>
        <input value={acctId} onChange={(e) => setAcctId(e.target.value)} maxLength={11} autoFocus />
        <button type="submit">ENTER</button>
      </form>
      <Err error={err} />
      {ok && <div className="msg ok">{ok}</div>}
      {form && (
        <form onSubmit={save} className="grid2">
          <div>
            {In({ label: "Active Status (Y/N)", k: "activeStatus", max: 1 })}
            {In({ label: "Current Balance", k: "currBal" })}
            {In({ label: "Credit Limit", k: "creditLimit" })}
            {In({ label: "Cash Credit Limit", k: "cashCreditLimit" })}
            {In({ label: "Open Date (YYYY-MM-DD)", k: "openDate", max: 10 })}
            {In({ label: "Expiry Date (YYYY-MM-DD)", k: "expirationDate", max: 10 })}
            {In({ label: "Reissue Date (YYYY-MM-DD)", k: "reissueDate", max: 10 })}
            {In({ label: "Curr Cyc Credit", k: "currCycCredit" })}
            {In({ label: "Curr Cyc Debit", k: "currCycDebit" })}
            {In({ label: "Account Group", k: "groupId", max: 10 })}
          </div>
          <div>
            {CIn({ label: "First Name", k: "firstName", max: 25 })}
            {CIn({ label: "Middle Name", k: "middleName", max: 25 })}
            {CIn({ label: "Last Name", k: "lastName", max: 25 })}
            {CIn({ label: "Address Line 1", k: "addrLine1", max: 50 })}
            {CIn({ label: "Address Line 2", k: "addrLine2", max: 50 })}
            {CIn({ label: "Address Line 3", k: "addrLine3", max: 50 })}
            {CIn({ label: "State", k: "addrStateCd", max: 2 })}
            {CIn({ label: "Country", k: "addrCountryCd", max: 3 })}
            {CIn({ label: "Zip", k: "addrZip", max: 10 })}
            {CIn({ label: "Phone 1", k: "phoneNum1", max: 20 })}
            {CIn({ label: "Phone 2", k: "phoneNum2", max: 20 })}
            {CIn({ label: "SSN", k: "ssn", max: 9 })}
            {CIn({ label: "DOB (YYYY-MM-DD)", k: "dob", max: 10 })}
            {CIn({ label: "EFT Account Id", k: "eftAccountId", max: 10 })}
            {CIn({ label: "Primary Card Holder (Y/N)", k: "priCardHolderInd", max: 1 })}
            {CIn({ label: "FICO Score", k: "ficoCreditScore", max: 3 })}
          </div>
          <div className="field-row"><button id="acctSaveBtn" type="submit">Save (F5)</button></div>
        </form>
      )}
    </Screen>
  );
}
