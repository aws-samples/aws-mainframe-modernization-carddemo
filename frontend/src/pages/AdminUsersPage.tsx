import { useState } from 'react'
import { Link } from 'react-router-dom'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { z } from 'zod'
import { api, ApiError } from '../api/client'
import type { Page, UserCreateRequest, UserView } from '../api/types'
import Badge from '../components/Badge'

/**
 * /admin/users — COUSR00C user list with COUSR01C add-user drawer and
 * COUSR03C delete confirm (admin-users-list-drawer / -delete-confirm mockups).
 * Server pages by user-ID keyset cursor; search/type filters apply to the
 * loaded page client-side.
 */

const PAGE_SIZE = 10

const createSchema = z.object({
  userId: z
    .string()
    .trim()
    .min(1, 'User ID can NOT be empty...')
    .max(8, 'User ID must be 8 characters or less...'),
  firstName: z
    .string()
    .trim()
    .min(1, 'First Name can NOT be empty...')
    .max(20, 'First Name must be 20 characters or less...'),
  lastName: z
    .string()
    .trim()
    .min(1, 'Last Name can NOT be empty...')
    .max(20, 'Last Name must be 20 characters or less...'),
  password: z
    .string()
    .min(1, 'Password can NOT be empty...')
    .max(8, 'Password must be 8 characters or less...'),
  userType: z.enum(['A', 'U'], { message: 'User Type must be A or U' }),
})

type CreateValues = z.infer<typeof createSchema>

export default function AdminUsersPage() {
  const queryClient = useQueryClient()
  const [cursor, setCursor] = useState<string | null>(null)
  const [prevCursors, setPrevCursors] = useState<(string | null)[]>([])
  const [search, setSearch] = useState('')
  const [typeFilter, setTypeFilter] = useState<'ALL' | 'A' | 'U'>('ALL')
  const [drawerOpen, setDrawerOpen] = useState(false)
  const [deleting, setDeleting] = useState<UserView | null>(null)
  const [deleteError, setDeleteError] = useState<string | null>(null)
  const [deleteBusy, setDeleteBusy] = useState(false)
  const [createError, setCreateError] = useState<string | null>(null)

  const query = useQuery({
    queryKey: ['users', cursor],
    queryFn: () =>
      api.get<Page<UserView>>(
        `/api/v1/users?limit=${PAGE_SIZE}${cursor ? `&cursor=${encodeURIComponent(cursor)}` : ''}`,
      ),
  })

  const {
    register,
    handleSubmit,
    reset,
    setError,
    formState: { errors, isSubmitting },
  } = useForm<CreateValues>({
    resolver: zodResolver(createSchema),
    defaultValues: { userType: 'U' },
  })

  const openDrawer = () => {
    reset({ userId: '', firstName: '', lastName: '', password: '', userType: 'U' })
    setCreateError(null)
    setDrawerOpen(true)
  }

  const onCreate = async (values: CreateValues) => {
    setCreateError(null)
    const payload: UserCreateRequest = { ...values }
    try {
      await api.post<UserView>('/api/v1/users', payload)
      setDrawerOpen(false)
      await queryClient.invalidateQueries({ queryKey: ['users'] })
    } catch (err) {
      if (err instanceof ApiError && err.status === 409) {
        setError('userId', { type: 'server', message: err.message })
      } else if (err instanceof ApiError) {
        setCreateError(err.message)
      } else {
        setCreateError('Could not create the user. Please try again.')
      }
    }
  }

  const onDelete = async () => {
    if (!deleting) return
    setDeleteBusy(true)
    setDeleteError(null)
    try {
      await api.delete(`/api/v1/users/${deleting.userId}`)
      setDeleting(null)
      await queryClient.invalidateQueries({ queryKey: ['users'] })
    } catch (err) {
      setDeleteError(err instanceof ApiError ? err.message : 'Delete failed. Please try again.')
    } finally {
      setDeleteBusy(false)
    }
  }

  const items = (query.data?.items ?? []).filter((u) => {
    if (typeFilter !== 'ALL' && u.userType !== typeFilter) return false
    const q = search.trim().toUpperCase()
    if (!q) return true
    return (
      u.userId.toUpperCase().includes(q) ||
      `${u.firstName ?? ''} ${u.lastName ?? ''}`.toUpperCase().includes(q)
    )
  })

  return (
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">User management</h1>
          <p className="page-sub">
            User IDs are 8 characters and cannot be changed after creation.
          </p>
        </div>
        <div className="head-actions">
          <button
            type="button"
            className="btn btn-primary"
            onClick={openDrawer}
            data-component-id="user-add"
          >
            Add user
          </button>
        </div>
      </div>

      <div className="card">
        <div className="toolbar">
          <div className="field">
            <label htmlFor="u-search">Search</label>
            <input
              id="u-search"
              className="input"
              placeholder="User ID or name"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              data-component-id="users-search"
            />
          </div>
          <div className="field" style={{ minWidth: 150 }}>
            <label htmlFor="u-type">Type</label>
            <select
              id="u-type"
              className="select"
              value={typeFilter}
              onChange={(e) => setTypeFilter(e.target.value as 'ALL' | 'A' | 'U')}
              data-component-id="users-filter-type"
            >
              <option value="ALL">All</option>
              <option value="A">Admin</option>
              <option value="U">User</option>
            </select>
          </div>
        </div>

        <table className="data">
          <thead>
            <tr>
              <th>User ID</th>
              <th>Name</th>
              <th>Type</th>
              <th style={{ textAlign: 'right' }}>Actions</th>
            </tr>
          </thead>
          <tbody>
            {query.isPending && (
              <tr>
                <td colSpan={4} style={{ color: 'var(--muted)' }}>Loading users…</td>
              </tr>
            )}
            {query.isError && (
              <tr>
                <td colSpan={4} style={{ color: 'var(--red)' }}>Could not load users.</td>
              </tr>
            )}
            {!query.isPending && !query.isError && items.length === 0 && (
              <tr>
                <td colSpan={4} style={{ color: 'var(--muted)' }}>No users match.</td>
              </tr>
            )}
            {items.map((u) => (
              <tr key={u.userId}>
                <td className="mono">{u.userId}</td>
                <td>{[u.firstName, u.lastName].filter(Boolean).join(' ') || '—'}</td>
                <td>
                  {u.userType === 'A' ? (
                    <Badge color="blue">Admin</Badge>
                  ) : (
                    <Badge color="gray">User</Badge>
                  )}
                </td>
                <td>
                  <div className="row-actions">
                    <Link
                      className="btn btn-ghost btn-sm"
                      to={`/admin/users/${u.userId}/edit`}
                      aria-label={`Edit user ${u.userId}`}
                      data-component-id={`user-edit-${u.userId}`}
                    >
                      Edit
                    </Link>
                    <button
                      type="button"
                      className="btn btn-ghost btn-sm"
                      style={{ color: 'var(--red)' }}
                      aria-label={`Delete user ${u.userId}`}
                      onClick={() => {
                        setDeleteError(null)
                        setDeleting(u)
                      }}
                      data-component-id={`user-delete-${u.userId}`}
                    >
                      Delete
                    </button>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>

        <div className="pagebar">
          <div>
            Showing <b>{items.length}</b> user{items.length === 1 ? '' : 's'}
          </div>
          <div className="pager">
            <button
              type="button"
              className="pgbtn"
              disabled={prevCursors.length === 0}
              onClick={() => {
                const prev = [...prevCursors]
                const back = prev.pop() ?? null
                setPrevCursors(prev)
                setCursor(back)
              }}
              data-component-id="users-page-prev"
            >
              Previous
            </button>
            <button
              type="button"
              className="pgbtn"
              disabled={!query.data?.nextCursor}
              onClick={() => {
                setPrevCursors((p) => [...p, cursor])
                setCursor(query.data?.nextCursor ?? null)
              }}
              data-component-id="users-page-next"
            >
              Next
            </button>
          </div>
        </div>
      </div>

      {drawerOpen && (
        <>
          <div className="scrim" onClick={() => setDrawerOpen(false)} />
          <div className="drawer" role="dialog" aria-modal="true" aria-label="Add user">
            <div className="d-head">
              <h2>Add user</h2>
              <button
                type="button"
                className="icon-btn"
                aria-label="Close"
                onClick={() => setDrawerOpen(false)}
                data-component-id="user-drawer-close"
              >
                ×
              </button>
            </div>
            <form onSubmit={handleSubmit(onCreate)} noValidate style={{ display: 'contents' }}>
              <div className="d-body">
                {createError && (
                  <div className="alert error" role="alert">
                    <div>{createError}</div>
                  </div>
                )}
                <div className="field">
                  <label htmlFor="nu-id">
                    User ID <span className="req">*</span>
                  </label>
                  <input
                    id="nu-id"
                    className={`input mono${errors.userId ? ' invalid' : ''}`}
                    maxLength={8}
                    placeholder="8 characters, e.g. USER0005"
                    data-component-id="user-form-id"
                    {...register('userId')}
                  />
                  {errors.userId ? (
                    <div className="error-text">{errors.userId.message}</div>
                  ) : (
                    <div className="hint">Must be unique; cannot be changed later</div>
                  )}
                </div>
                <div className="form-grid" style={{ marginTop: 16 }}>
                  <div className="field">
                    <label htmlFor="nu-first">
                      First name <span className="req">*</span>
                    </label>
                    <input
                      id="nu-first"
                      className={`input${errors.firstName ? ' invalid' : ''}`}
                      data-component-id="user-form-first-name"
                      {...register('firstName')}
                    />
                    {errors.firstName && (
                      <div className="error-text">{errors.firstName.message}</div>
                    )}
                  </div>
                  <div className="field">
                    <label htmlFor="nu-last">
                      Last name <span className="req">*</span>
                    </label>
                    <input
                      id="nu-last"
                      className={`input${errors.lastName ? ' invalid' : ''}`}
                      data-component-id="user-form-last-name"
                      {...register('lastName')}
                    />
                    {errors.lastName && (
                      <div className="error-text">{errors.lastName.message}</div>
                    )}
                  </div>
                </div>
                <div className="field" style={{ marginTop: 16 }}>
                  <label htmlFor="nu-pass">
                    Initial password <span className="req">*</span>
                  </label>
                  <input
                    id="nu-pass"
                    className={`input${errors.password ? ' invalid' : ''}`}
                    type="password"
                    maxLength={8}
                    data-component-id="user-form-password"
                    {...register('password')}
                  />
                  {errors.password ? (
                    <div className="error-text">{errors.password.message}</div>
                  ) : (
                    <div className="hint">
                      8 characters max; the user should change it after first sign-in
                    </div>
                  )}
                </div>
                <div className="field" style={{ marginTop: 16 }}>
                  <label>
                    User type <span className="req">*</span>
                  </label>
                  <div className="radio-row" role="radiogroup" aria-label="User type">
                    <label className="radio">
                      <input
                        type="radio"
                        value="U"
                        data-component-id="user-form-type-user"
                        {...register('userType')}
                      />{' '}
                      U — Regular user
                    </label>
                    <label className="radio">
                      <input
                        type="radio"
                        value="A"
                        data-component-id="user-form-type-admin"
                        {...register('userType')}
                      />{' '}
                      A — Administrator
                    </label>
                  </div>
                </div>
              </div>
              <div className="d-foot">
                <button
                  type="button"
                  className="btn btn-secondary"
                  onClick={() => setDrawerOpen(false)}
                  data-component-id="user-drawer-cancel"
                >
                  Cancel
                </button>
                <button
                  type="submit"
                  className="btn btn-primary"
                  disabled={isSubmitting}
                  data-component-id="user-drawer-save"
                >
                  {isSubmitting ? 'Creating…' : 'Create user'}
                </button>
              </div>
            </form>
          </div>
        </>
      )}

      {deleting && (
        <div className="overlay" role="dialog" aria-modal="true" aria-label="Delete user">
          <div className="modal">
            <div className="m-head">
              <h2>Delete user {deleting.userId}?</h2>
            </div>
            <div className="m-body">
              {deleteError && (
                <div className="alert error" role="alert">
                  <div>{deleteError}</div>
                </div>
              )}
              <p>
                This permanently removes{' '}
                <b>{[deleting.firstName, deleting.lastName].filter(Boolean).join(' ')}</b>{' '}
                (<span className="mono">{deleting.userId}</span>) and cannot be undone.
              </p>
            </div>
            <div className="m-foot">
              <button
                type="button"
                className="btn btn-secondary"
                onClick={() => setDeleting(null)}
                data-component-id="user-delete-cancel"
              >
                Cancel
              </button>
              <button
                type="button"
                className="btn btn-danger"
                disabled={deleteBusy}
                onClick={onDelete}
                data-component-id="user-delete-confirm"
              >
                {deleteBusy ? 'Deleting…' : 'Delete user'}
              </button>
            </div>
          </div>
        </div>
      )}
    </>
  )
}
