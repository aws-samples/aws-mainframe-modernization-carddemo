import React, { useState } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import { api } from '../api.js';
import { useAuth, Err } from '../App.jsx';

export default function LoginPage() {
  const [userId, setUserId] = useState('');
  const [password, setPassword] = useState('');
  const [err, setErr] = useState(null);
  const { login } = useAuth();
  const nav = useNavigate();
  const loc = useLocation();

  async function submit(e) {
    e.preventDefault();
    setErr(null);
    if (!userId.trim()) return setErr(new Error('Please enter your User ID'));
    if (!password) return setErr(new Error('Please enter your Password'));
    try {
      const data = await api('/api/auth/login', { method: 'POST', body: { userId: userId.trim(), password } });
      login(data);
      nav(loc.state?.from?.pathname || '/', { replace: true });
    } catch (e2) { setErr(e2); }
  }

  return (
    <div className="screen">
      <div className="screen-head">
        <span className="screen-title">CardDemo — Sign On</span>
        <span className="screen-tran">Tran: CC00</span>
      </div>
      <p>Type your User ID and Password, then press ENTER.</p>
      <Err error={err} />
      <form onSubmit={submit}>
        <div className="field-row">
          <label>User ID</label>
          <input value={userId} onChange={(e) => setUserId(e.target.value)} maxLength={8} autoFocus />
        </div>
        <div className="field-row">
          <label>Password</label>
          <input type="password" value={password} onChange={(e) => setPassword(e.target.value)} maxLength={8} />
        </div>
        <div className="field-row"><label /><button type="submit">ENTER</button></div>
      </form>
      <div className="pfbar"><span><b>ENTER</b> Sign on</span></div>
    </div>
  );
}
