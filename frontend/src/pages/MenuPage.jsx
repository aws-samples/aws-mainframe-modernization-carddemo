import React, { useEffect, useState } from 'react';
import { Link } from 'react-router-dom';
import { api } from '../api.js';
import { Screen, Err } from '../App.jsx';

export default function MenuPage() {
  const [menu, setMenu] = useState(null);
  const [err, setErr] = useState(null);

  useEffect(() => {
    api('/api/menu').then(setMenu).catch(setErr);
  }, []);

  const title = menu?.role === 'A' ? 'Admin Main Menu' : 'Main Menu';
  const tran = menu?.role === 'A' ? 'CA00' : 'CM00';

  return (
    <Screen title={`CardDemo — ${title}`} tran={tran} pf={['F3=Exit', 'F12=Logout']}>
      <Err error={err} />
      <div className="menu-list">
        {menu?.options.map((o) => (
          <div className="menu-item" key={o.num}>
            <span className="num">{String(o.num).padStart(2, '0')}.</span>
            <Link to={o.path}>{o.name}</Link>
            <span className="screen-tran"> {o.program}</span>
          </div>
        ))}
      </div>
    </Screen>
  );
}
