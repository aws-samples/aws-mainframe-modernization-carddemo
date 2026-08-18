import { useEffect, useState } from 'react'
import { Link, useNavigate, useParams } from 'react-router-dom'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { z } from 'zod'
import { api, ApiError, retryUnlessNotFound } from '../api/client'
import type { UserUpdateRequest, UserView } from '../api/types'
import { IconBack } from '../components/icons'

/**
 * /admin/users/:userId/edit — COUSR02C user update + COUSR03C delete
 * (admin-users-edit-page / -delete-confirm mockups). Blank password keeps
 * the current one; this screen is the only password-change path (plan §6).
 */

const schema = z.object({
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
  password: z.string().max(8, 'Password must be 8 characters or less...'),
  userType: z.enum(['A', 'U'], { message: 'User Type must be A or U' }),
})

type FormValues = z.infer<typeof schema>

export default function AdminUserEditPage() {
  const { userId } = useParams<{ userId: string }>()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const [conflict, setConflict] = useState<string | null>(null)
  const [serverError, setServerError] = useState<string | null>(null)
  const [confirmDelete, setConfirmDelete] = useState(false)
  const [deleteBusy, setDeleteBusy] = useState(false)

  const query = useQuery({
    queryKey: ['user', userId],
    queryFn: () => api.get<UserView>(`/api/v1/users/${userId}`),
    retry: retryUnlessNotFound,
  })

  const {
    register,
    handleSubmit,
    reset,
    formState: { errors, isSubmitting },
  } = useForm<FormValues>({ resolver: zodResolver(schema) })

  useEffect(() => {
    if (query.data) {
      reset({
        firstName: query.data.firstName ?? '',
        lastName: query.data.lastName ?? '',
        password: '',
        userType: query.data.userType === 'A' ? 'A' : 'U',
      })
    }
  }, [query.data, reset])

  if (query.isPending) {
    return <div className="card"><div className="card-body">Loading user…</div></div>
  }
  if (query.isError) {
    const notFound = query.error instanceof ApiError && query.error.status === 404
    return (
      <div className="alert error" role="alert">
        <div>
          <b>{notFound ? 'User not found.' : 'Could not load user.'}</b>{' '}
          <Link to="/admin/users">Back to users</Link>
        </div>
      </div>
    )
  }

  const u = query.data

  const onSubmit = async (values: FormValues) => {
    setConflict(null)
    setServerError(null)
    const payload: UserUpdateRequest = {
      version: u.version,
      firstName: values.firstName,
      lastName: values.lastName,
      password: values.password === '' ? null : values.password,
      userType: values.userType,
    }
    try {
      await api.put<UserView>(`/api/v1/users/${userId}`, payload)
      await queryClient.invalidateQueries({ queryKey: ['users'] })
      await queryClient.invalidateQueries({ queryKey: ['user', userId] })
      navigate('/admin/users')
    } catch (err) {
      if (err instanceof ApiError && err.status === 409) {
        setConflict(err.message)
      } else if (err instanceof ApiError) {
        setServerError(err.message)
      } else {
        setServerError('Save failed. Please try again.')
      }
    }
  }

  const onDelete = async () => {
    setDeleteBusy(true)
    setServerError(null)
    try {
      await api.delete(`/api/v1/users/${userId}`)
      await queryClient.invalidateQueries({ queryKey: ['users'] })
      navigate('/admin/users')
    } catch (err) {
      setConfirmDelete(false)
      setServerError(err instanceof ApiError ? err.message : 'Delete failed. Please try again.')
    } finally {
      setDeleteBusy(false)
    }
  }

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <Link
              className="icon-btn"
              to="/admin/users"
              aria-label="Back to users"
              data-component-id="user-edit-back"
            >
              <IconBack />
            </Link>
            <h1 className="page-title">
              Edit user <span className="mono" style={{ fontSize: 19 }}>{u.userId}</span>
            </h1>
          </div>
          <p className="page-sub">{[u.firstName, u.lastName].filter(Boolean).join(' ')}</p>
        </div>
      </div>

      {conflict && (
        <div className="alert error" role="alert">
          <div>
            <b>Save conflict.</b> {conflict}{' '}
            <button
              type="button"
              className="btn btn-ghost btn-sm"
              onClick={() => {
                setConflict(null)
                query.refetch()
              }}
              data-component-id="user-edit-reload"
            >
              Reload latest values
            </button>
          </div>
        </div>
      )}
      {serverError && (
        <div className="alert error" role="alert">
          <div><b>Request failed.</b> {serverError}</div>
        </div>
      )}

      <form onSubmit={handleSubmit(onSubmit)} noValidate>
        <div className="card" style={{ maxWidth: 560 }}>
          <div className="card-body">
            <div className="field">
              <label htmlFor="eu-id">User ID</label>
              <input
                id="eu-id"
                className="input mono"
                value={u.userId}
                disabled
                data-component-id="user-form-id"
              />
              <div className="hint">User ID cannot be changed</div>
            </div>
            <div className="form-grid" style={{ marginTop: 16 }}>
              <div className="field">
                <label htmlFor="eu-first">
                  First name <span className="req">*</span>
                </label>
                <input
                  id="eu-first"
                  className={`input${errors.firstName ? ' invalid' : ''}`}
                  data-component-id="user-form-first-name"
                  {...register('firstName')}
                />
                {errors.firstName && (
                  <div className="error-text">{errors.firstName.message}</div>
                )}
              </div>
              <div className="field">
                <label htmlFor="eu-last">
                  Last name <span className="req">*</span>
                </label>
                <input
                  id="eu-last"
                  className={`input${errors.lastName ? ' invalid' : ''}`}
                  data-component-id="user-form-last-name"
                  {...register('lastName')}
                />
                {errors.lastName && <div className="error-text">{errors.lastName.message}</div>}
              </div>
            </div>
            <div className="field" style={{ marginTop: 16 }}>
              <label htmlFor="eu-pass">
                New password <span className="opt">(leave blank to keep current)</span>
              </label>
              <input
                id="eu-pass"
                className={`input${errors.password ? ' invalid' : ''}`}
                type="password"
                maxLength={8}
                data-component-id="user-form-password"
                {...register('password')}
              />
              {errors.password && <div className="error-text">{errors.password.message}</div>}
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
              {errors.userType && <div className="error-text">{errors.userType.message}</div>}
            </div>
          </div>
          <div className="form-foot">
            <button
              type="button"
              className="btn btn-ghost btn-sm"
              style={{ color: 'var(--red)' }}
              onClick={() => setConfirmDelete(true)}
              data-component-id="user-edit-delete"
            >
              Delete user
            </button>
            <div style={{ display: 'flex', gap: 10 }}>
              <Link
                className="btn btn-secondary"
                to="/admin/users"
                data-component-id="user-edit-cancel"
              >
                Cancel
              </Link>
              <button
                type="submit"
                className="btn btn-primary"
                disabled={isSubmitting}
                data-component-id="user-edit-save"
              >
                {isSubmitting ? 'Saving…' : 'Save changes'}
              </button>
            </div>
          </div>
        </div>
      </form>

      {confirmDelete && (
        <div className="overlay" role="dialog" aria-modal="true" aria-label="Delete user">
          <div className="modal">
            <div className="m-head">
              <h2>Delete user {u.userId}?</h2>
            </div>
            <div className="m-body">
              <p>
                This permanently removes{' '}
                <b>{[u.firstName, u.lastName].filter(Boolean).join(' ')}</b>{' '}
                (<span className="mono">{u.userId}</span>) and cannot be undone.
              </p>
            </div>
            <div className="m-foot">
              <button
                type="button"
                className="btn btn-secondary"
                onClick={() => setConfirmDelete(false)}
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
