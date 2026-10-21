import React, { createContext, useCallback, useContext, useEffect, useState } from 'react';
import { Routes, Route, Navigate, useNavigate, useLocation } from 'react-router-dom';
import { api, getUser, setSession, clearSession } from './api.js';
import LoginPage from './pages/LoginPage.jsx';
import MenuPage from './pages/MenuPage.jsx';
import { AccountViewPage, AccountUpdatePage } from './pages/AccountPages.jsx';
import { CardListPage, CardDetailPage } from './pages/CardPages.jsx';
import { TranListPage, TranViewPage, TranAddPage } from './pages/TransactionPages.jsx';
import ReportPage from './pages/ReportPage.jsx';
import BillPayPage from './pages/BillPayPage.jsx';
import { UserListPage, UserAddPage, UserUpdatePage, UserDeletePage } from './pages/AdminPages.jsx';
import { JobsPage, StatementPage } from './pages/JobsPage.jsx';

const AuthCtx = createContext(null);
export function useAuth() { return useContext(AuthCtx); }

/* PF-key support, mirroring BMS maps: F3 back, F7 prev page, F8 next page, F12 logout. */
const PfCtx = createContext({});
export function usePfKeys(handlers) {
  const reg = useContext(PfCtx);
  useEffect(() => {
    for (const [k, fn] of Object.entries(handlers)) reg.current[k] = fn;
    return () => { for (const k of Object.keys(handlers)) delete reg.current[k]; };
  });
}

export function Screen({ title, tran, children, pf = ['F3=Back', 'F12=Logout'] }) {
  const nav = useNavigate();
  const { user, logout } = useAuth();
  const reg = useContext(PfCtx);
  usePfKeys({ F3: () => nav(-1), F12: logout });
  return (
    <div className="screen">
      <div className="screen-head">
        <span className="screen-title">{title}</span>
        <span className="screen-tran">
          {tran ? `Tran: ${tran} ` : ''}{user ? `${user.userId} (${user.userType})` : ''}
        </span>
      </div>
      {children}
      <div className="pfbar">
        {pf.map((p) => {
          const [k, label] = p.split('=');
          return <span key={k}><b>{k}</b> {label}</span>;
        })}
      </div>
    </div>
  );
}

export function Err({ error }) {
  if (!error) return null;
  return <div className="msg err">{error.message || String(error)}</div>;
}

function RequireAuth({ children }) {
  const { user } = useAuth();
  const loc = useLocation();
  if (!user) return <Navigate to="/login" state={{ from: loc }} replace />;
  return children;
}

export default function App() {
  const [user, setUser] = useState(getUser());
  const handlers = React.useRef({});
  const nav = useNavigate();

  const logout = useCallback(() => { clearSession(); setUser(null); nav('/login'); }, [nav]);

  useEffect(() => {
    const onKey = (e) => {
      const map = { F3: 'F3', F7: 'F7', F8: 'F8', F12: 'F12', F4: 'F4', F5: 'F5' };
      const name = map[e.key];
      if (name && handlers.current[name]) {
        e.preventDefault();
        handlers.current[name]();
      }
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, []);

  const login = useCallback((data) => {
    setSession(data.token, data);
    setUser(data);
  }, []);

  const ctx = { user, login, logout, api };

  return (
    <PfCtx.Provider value={handlers}>
      <AuthCtx.Provider value={ctx}>
        <Routes>
          <Route path="/login" element={<LoginPage />} />
          <Route path="/" element={<RequireAuth><MenuPage /></RequireAuth>} />
          <Route path="/account" element={<RequireAuth><AccountViewPage /></RequireAuth>} />
          <Route path="/accounts/view" element={<RequireAuth><AccountViewPage /></RequireAuth>} />
          <Route path="/account/update" element={<RequireAuth><AccountUpdatePage /></RequireAuth>} />
          <Route path="/accounts/update" element={<RequireAuth><AccountUpdatePage /></RequireAuth>} />
          <Route path="/cards" element={<RequireAuth><CardListPage /></RequireAuth>} />
          <Route path="/cards/view" element={<RequireAuth><CardDetailPage /></RequireAuth>} />
          <Route path="/cards/update" element={<RequireAuth><CardDetailPage edit /></RequireAuth>} />
          <Route path="/cards/:cardNum" element={<RequireAuth><CardDetailPage /></RequireAuth>} />
          <Route path="/cards/:cardNum/edit" element={<RequireAuth><CardDetailPage edit /></RequireAuth>} />
          <Route path="/transactions" element={<RequireAuth><TranListPage /></RequireAuth>} />
          <Route path="/transactions/add" element={<RequireAuth><TranAddPage /></RequireAuth>} />
          <Route path="/transactions/view" element={<RequireAuth><TranViewPage /></RequireAuth>} />
          <Route path="/transactions/:tranId" element={<RequireAuth><TranViewPage /></RequireAuth>} />
          <Route path="/reports" element={<RequireAuth><ReportPage /></RequireAuth>} />
          <Route path="/billpay" element={<RequireAuth><BillPayPage /></RequireAuth>} />
          <Route path="/admin/users" element={<RequireAuth><UserListPage /></RequireAuth>} />
          <Route path="/admin/users/add" element={<RequireAuth><UserAddPage /></RequireAuth>} />
          <Route path="/admin/users/update" element={<RequireAuth><UserUpdatePage /></RequireAuth>} />
          <Route path="/admin/users/delete" element={<RequireAuth><UserDeletePage /></RequireAuth>} />
          <Route path="/admin/jobs" element={<RequireAuth><JobsPage /></RequireAuth>} />
          <Route path="/admin/jobs/statements/:acctId" element={<RequireAuth><StatementPage /></RequireAuth>} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
      </AuthCtx.Provider>
    </PfCtx.Provider>
  );
}
