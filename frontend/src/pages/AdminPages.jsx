import React, { useEffect, useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { api } from '../api.js';
import { Screen, Err, usePfKeys } from '../App.jsx';

export function UserListPage() {
  const [data, setData] = useState(null);
  const [page, setPage] = useState(0);
  const [err, setErr] = useState(null);
  const nav = useNavigate();

  useEffect(() => {
    setErr(null);
    api(`/api/admin/users?page=${page}`).then(setData).catch(setErr);
  }, [page]);

  usePfKeys({
    F7: () => { if (data?.hasPrevious) setPage((p) => p - 1); },
    F8: () => { if (data?.hasNext) setPage((p) => p + 1); },
  });

  return (
    <Screen title="User List (Security)" tran="COUSR00C"
      pf={['F3=Back', 'F7=Backward', 'F8=Forward', 'F12=Logout']}>
      <Err error={err} />
      <table>
        <thead><tr><th>User ID</th><th>First Name</th><th>Last Name</th><th>Type</th><th /></tr></thead>
        <tbody>
          {data?.content.map((u) => (
            <tr key={u.userId}>
              <td>{u.userId}</td><td>{u.firstName}</td><td>{u.lastName}</td><td>{u.userType}</td>
              <td>
                <button className="link" onClick={() => nav(`/admin/users/update?id=${u.userId}`)}>U</button>
                <button className="link" onClick={() => nav(`/admin/users/delete?id=${u.userId}`)}>D</button>
              </td>
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

function UserForm({ title, tran, submitLabel, onSubmit }) {
  const [form, setForm] = useState({ userId: '', firstName: '', lastName: '', password: '', userType: '' });
  const [err, setErr] = useState(null);
  const [ok, setOk] = useState(null);
  const set = (k, v) => setForm((f) => ({ ...f, [k]: v }));

  async function submit(e) {
    e.preventDefault();
    setErr(null); setOk(null);
    try {
      await onSubmit(form);
      setOk(`${title} successful`);
    } catch (e2) { setErr(e2); }
  }

  const In = ({ label, k, max, type }) => (
    <div className="field-row">
      <label>{label}</label>
      <input type={type || 'text'} value={form[k]}
        onChange={(e) => set(k, e.target.value)} maxLength={max} />
    </div>
  );

  return (
    <Screen title={title} tran={tran}>
      <Err error={err} />
      {ok && <div className="msg ok">{ok}</div>}
      <form onSubmit={submit}>
        {In({ label: "User ID", k: "userId", max: 8 })}
        {In({ label: "First Name", k: "firstName", max: 20 })}
        {In({ label: "Last Name", k: "lastName", max: 20 })}
        {In({ label: "Password", k: "password", max: 8, type: "password" })}
        {In({ label: "User Type (A=Admin, U=User)", k: "userType", max: 1 })}
        <div className="field-row"><label /><button type="submit">{submitLabel}</button></div>
      </form>
    </Screen>
  );
}

export function UserAddPage() {
  return (
    <UserForm title="Add User (Security)" tran="COUSR01C" submitLabel="Add User"
      onSubmit={(f) => api('/api/admin/users', { method: 'POST', body: f })} />
  );
}

export function UserUpdatePage() {
  const [search] = useState(() => new URLSearchParams(window.location.search));
  const [userId, setUserId] = useState(search.get('id') || '');
  const [loaded, setLoaded] = useState(false);
  const [form, setForm] = useState({ userId: '', firstName: '', lastName: '', password: '', userType: '' });
  const [err, setErr] = useState(null);
  const [ok, setOk] = useState(null);

  useEffect(() => { if (userId) load(); /* eslint-disable-next-line */ }, []);

  async function load(e) {
    e?.preventDefault();
    setErr(null); setOk(null); setLoaded(false);
    try {
      const u = await api(`/api/admin/users/${userId.trim()}`);
      setForm(u); setLoaded(true);
    } catch (e2) { setErr(e2); }
  }

  async function save(e) {
    e.preventDefault();
    setErr(null); setOk(null);
    try {
      await api(`/api/admin/users/${userId.trim()}`, { method: 'PUT', body: form });
      setOk('User updated successfully');
    } catch (e2) { setErr(e2); }
  }

  const In = ({ label, k, max, type }) => (
    <div className="field-row">
      <label>{label}</label>
      <input type={type || 'text'} value={form[k]} onChange={(e) => setForm((f) => ({ ...f, [k]: e.target.value }))} maxLength={max} />
    </div>
  );

  return (
    <Screen title="Update User (Security)" tran="COUSR02C">
      <form onSubmit={load} className="field-row">
        <label>User ID</label>
        <input value={userId} onChange={(e) => setUserId(e.target.value)} maxLength={8} autoFocus />
        <button type="submit">ENTER</button>
      </form>
      <Err error={err} />
      {ok && <div className="msg ok">{ok}</div>}
      {loaded && (
        <form onSubmit={save}>
          {In({ label: "First Name", k: "firstName", max: 20 })}
          {In({ label: "Last Name", k: "lastName", max: 20 })}
          {In({ label: "Password", k: "password", max: 8, type: "password" })}
          {In({ label: "User Type (A=Admin, U=User)", k: "userType", max: 1 })}
          <div className="field-row"><button type="submit">Update User</button></div>
        </form>
      )}
    </Screen>
  );
}

export function UserDeletePage() {
  const [search] = useState(() => new URLSearchParams(window.location.search));
  const [userId, setUserId] = useState(search.get('id') || '');
  const [loaded, setLoaded] = useState(false);
  const [user, setUser] = useState(null);
  const [err, setErr] = useState(null);
  const [ok, setOk] = useState(null);

  useEffect(() => { if (userId) load(); /* eslint-disable-next-line */ }, []);

  async function load(e) {
    e?.preventDefault();
    setErr(null); setOk(null); setLoaded(false);
    try { setUser(await api(`/api/admin/users/${userId.trim()}`)); setLoaded(true); }
    catch (e2) { setErr(e2); }
  }

  async function del() {
    setErr(null); setOk(null);
    try {
      await api(`/api/admin/users/${userId.trim()}`, { method: 'DELETE' });
      setOk(`User ${userId.trim()} deleted`);
      setLoaded(false); setUser(null);
    } catch (e2) { setErr(e2); }
  }

  return (
    <Screen title="Delete User (Security)" tran="COUSR03C">
      <form onSubmit={load} className="field-row">
        <label>User ID</label>
        <input value={userId} onChange={(e) => setUserId(e.target.value)} maxLength={8} autoFocus />
        <button type="submit">ENTER</button>
      </form>
      <Err error={err} />
      {ok && <div className="msg ok">{ok}</div>}
      {loaded && user && (
        <div>
          <div className="field-row"><label>User ID</label><input readOnly value={user.userId} /></div>
          <div className="field-row"><label>Name</label><input readOnly value={`${user.firstName} ${user.lastName}`} /></div>
          <div className="field-row"><label>Type</label><input readOnly value={user.userType} /></div>
          <div className="field-row">
            <label />
            <button className="danger" onClick={del}>Delete User</button>
          </div>
        </div>
      )}
    </Screen>
  );
}
