import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { BrowserRouter, Route, Routes } from 'react-router-dom'
import { AuthProvider } from './auth/AuthContext'
import RequireAuth from './auth/RequireAuth'
import AppShell from './components/AppShell'
import AccountDetailPage from './pages/AccountDetailPage'
import AccountEditPage from './pages/AccountEditPage'
import AccountsPage from './pages/AccountsPage'
import AdminJobsPage from './pages/AdminJobsPage'
import AdminUserEditPage from './pages/AdminUserEditPage'
import AdminUsersPage from './pages/AdminUsersPage'
import BillPayPage from './pages/BillPayPage'
import CardDetailPage from './pages/CardDetailPage'
import CardEditPage from './pages/CardEditPage'
import CardListPage from './pages/CardListPage'
import DashboardPage from './pages/DashboardPage'
import LoginPage from './pages/LoginPage'
import ReportsPage from './pages/ReportsPage'
import TransactionAddPage from './pages/TransactionAddPage'
import TransactionDetailPage from './pages/TransactionDetailPage'
import TransactionListPage from './pages/TransactionListPage'

const queryClient = new QueryClient({
  defaultOptions: { queries: { refetchOnWindowFocus: false } },
})

export default function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <AuthProvider>
          <Routes>
            <Route path="/login" element={<LoginPage />} />
            <Route element={<RequireAuth />}>
              <Route element={<AppShell />}>
                <Route path="/" element={<DashboardPage />} />
                <Route path="/accounts" element={<AccountsPage />} />
                <Route path="/accounts/:id" element={<AccountDetailPage />} />
                <Route path="/accounts/:id/edit" element={<AccountEditPage />} />
                <Route path="/accounts/:id/bill-pay" element={<BillPayPage />} />
                <Route path="/cards" element={<CardListPage />} />
                <Route path="/cards/:cardNumber" element={<CardDetailPage />} />
                <Route path="/cards/:cardNumber/edit" element={<CardEditPage />} />
                <Route path="/transactions" element={<TransactionListPage />} />
                <Route path="/transactions/new" element={<TransactionAddPage />} />
                <Route path="/transactions/:id" element={<TransactionDetailPage />} />
                <Route path="/reports" element={<ReportsPage />} />
                <Route path="/admin/users" element={<AdminUsersPage />} />
                <Route path="/admin/users/:userId/edit" element={<AdminUserEditPage />} />
                <Route path="/admin/jobs" element={<AdminJobsPage />} />
              </Route>
            </Route>
          </Routes>
        </AuthProvider>
      </BrowserRouter>
    </QueryClientProvider>
  )
}
