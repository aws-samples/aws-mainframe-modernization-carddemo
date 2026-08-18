import { NavLink, Outlet, useNavigate } from 'react-router-dom'
import { useAuth } from '../auth/AuthContext'
import { fullName, initials } from '../format'
import {
  IconAccounts,
  IconBolt,
  IconCard,
  IconDashboard,
  IconList,
  IconLogo,
  IconReport,
  IconSignOut,
  IconUsers,
} from './icons'

/** Navy sidebar + topbar shell (dashboard-overview / dashboard-tiles-user mockups). */
const sideLink = ({ isActive }: { isActive: boolean }) =>
  isActive ? 'side-link active' : 'side-link'

export default function AppShell() {
  const { user, logout } = useAuth()
  const navigate = useNavigate()

  if (!user) return null

  const signOut = () => {
    logout()
    navigate('/login', { replace: true })
  }

  const isAdmin = user.role === 'ADMIN'
  const name = fullName(user.firstName, user.lastName, user.userId)

  return (
    <div className="app">
      <aside className="sidebar" aria-label="Primary navigation">
        <div className="side-brand">
          <div className="logo">
            <IconLogo />
          </div>
          <div>
            <div className="name">CardDemo</div>
            <div className="sub">Card Management</div>
          </div>
        </div>
        <nav className="side-nav">
          <div className="side-label">Main</div>
          <NavLink className={sideLink} to="/" end data-component-id="nav-dashboard">
            <IconDashboard />
            Dashboard
          </NavLink>
          <NavLink className={sideLink} to="/accounts" data-component-id="nav-accounts">
            <IconAccounts />
            Accounts
          </NavLink>
          <NavLink className={sideLink} to="/cards" data-component-id="nav-cards">
            <IconCard />
            Cards
          </NavLink>
          <NavLink className={sideLink} to="/transactions" data-component-id="nav-transactions">
            <IconList />
            Transactions
          </NavLink>
          <NavLink className={sideLink} to="/reports" data-component-id="nav-reports">
            <IconReport />
            Reports
          </NavLink>
          {isAdmin && (
            <>
              <div className="side-label">Administration</div>
              <NavLink className={sideLink} to="/admin/users" data-component-id="nav-users">
                <IconUsers />
                User management
              </NavLink>
              <NavLink className={sideLink} to="/admin/jobs" data-component-id="nav-jobs">
                <IconBolt />
                Batch jobs
              </NavLink>
            </>
          )}
        </nav>
        <div className="side-foot">
          <button
            type="button"
            className="side-link"
            onClick={signOut}
            data-component-id="nav-signout"
            aria-label="Sign out"
          >
            <IconSignOut />
            Sign out
          </button>
        </div>
      </aside>
      <div className="main">
        <header className="topbar">
          <div className="crumbs" aria-label="Breadcrumb">
            <span className="here">CardDemo</span>
          </div>
          <div className="user-chip">
            <div className="who">
              <div className="nm">{name}</div>
              <div className="id">
                {user.userId} &middot; {isAdmin ? 'Administrator' : 'User'}
              </div>
            </div>
            <div className="avatar" aria-hidden>
              {initials(user.firstName, user.lastName, user.userId)}
            </div>
            <button
              type="button"
              className="icon-btn"
              onClick={signOut}
              data-component-id="header-signout"
              aria-label="Sign out"
            >
              <IconSignOut size={15} />
            </button>
          </div>
        </header>
        <main className="content">
          <Outlet />
        </main>
      </div>
    </div>
  )
}
