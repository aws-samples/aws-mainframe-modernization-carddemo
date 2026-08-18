import { useEffect, useState } from 'react'
import { Link, useNavigate, useParams } from 'react-router-dom'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { z } from 'zod'
import { api, ApiError, retryUnlessNotFound } from '../api/client'
import type { CardDetail, CardUpdateRequest } from '../api/types'
import { IconBack } from '../components/icons'

/**
 * /cards/:cardNumber/edit — COCRDUPC card update. Mirrors the legacy F5
 * confirm flow: edit → review the changes → confirm before the PUT.
 * Client-side zod mirrors the 1230-1260 edits; the server re-runs them.
 */

const schema = z.object({
  embossedName: z
    .string()
    .trim()
    .min(1, 'Card name not provided')
    .regex(/^[A-Za-z ]+$/, 'Card name can only contain alphabets and spaces'),
  expiryMonth: z
    .string()
    .regex(/^\d{1,2}$/, 'Card expiry month must be between 1 and 12')
    .refine((v) => Number(v) >= 1 && Number(v) <= 12, 'Card expiry month must be between 1 and 12'),
  expiryYear: z
    .string()
    .regex(/^\d{4}$/, 'Invalid card expiry year')
    .refine((v) => Number(v) >= 1950 && Number(v) <= 2099, 'Invalid card expiry year'),
  activeStatus: z.enum(['Y', 'N'], { message: 'Card Active Status must be Y or N' }),
})

type FormValues = z.infer<typeof schema>

function toFormValues(c: CardDetail): FormValues {
  const exp = c.expirationDate ?? ''
  return {
    embossedName: c.embossedName ?? '',
    expiryMonth: exp ? String(Number(exp.slice(5, 7))) : '',
    expiryYear: exp ? exp.slice(0, 4) : '',
    activeStatus: c.activeStatus === 'N' ? 'N' : 'Y',
  }
}

export default function CardEditPage() {
  const { cardNumber } = useParams<{ cardNumber: string }>()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const [confirming, setConfirming] = useState<FormValues | null>(null)
  const [conflict, setConflict] = useState<string | null>(null)
  const [serverError, setServerError] = useState<string | null>(null)
  const [saving, setSaving] = useState(false)

  const query = useQuery({
    queryKey: ['card', cardNumber],
    queryFn: () => api.get<CardDetail>(`/api/v1/cards/${cardNumber}`),
    retry: retryUnlessNotFound,
  })

  const {
    register,
    handleSubmit,
    reset,
    setError,
    formState: { errors },
  } = useForm<FormValues>({ resolver: zodResolver(schema), mode: 'onBlur' })

  useEffect(() => {
    if (query.data) reset(toFormValues(query.data))
  }, [query.data, reset])

  if (query.isPending) {
    return <div className="card"><div className="card-body">Loading card…</div></div>
  }
  if (query.isError) {
    return (
      <div className="alert error" role="alert">
        <div>
          <b>Could not load card.</b> <Link to="/cards">Back to card list</Link>
        </div>
      </div>
    )
  }

  const c = query.data

  const save = async (values: FormValues) => {
    setConflict(null)
    setServerError(null)
    setSaving(true)
    const payload: CardUpdateRequest = {
      version: c.version,
      embossedName: values.embossedName,
      expiryMonth: values.expiryMonth,
      expiryYear: values.expiryYear,
      activeStatus: values.activeStatus,
    }
    try {
      await api.put<CardDetail>(`/api/v1/cards/${cardNumber}`, payload)
      await queryClient.invalidateQueries({ queryKey: ['card', cardNumber] })
      navigate(`/cards/${cardNumber}`)
    } catch (err) {
      setConfirming(null)
      if (err instanceof ApiError && err.status === 409) {
        setConflict(err.message)
      } else if (err instanceof ApiError && err.fieldErrors.length > 0) {
        for (const fe of err.fieldErrors) {
          setError(fe.field as keyof FormValues, { type: 'server', message: fe.message })
        }
      } else if (err instanceof ApiError) {
        setServerError(err.message)
      } else {
        setServerError('Save failed. Please try again.')
      }
    } finally {
      setSaving(false)
    }
  }

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <Link
              className="icon-btn"
              to={`/cards/${cardNumber}`}
              aria-label="Back to card"
              data-component-id="card-edit-back"
            >
              <IconBack />
            </Link>
            <h1 className="page-title">
              Update card <span className="mono" style={{ fontSize: 19 }}>{c.maskedCardNumber}</span>
            </h1>
          </div>
          <p className="page-sub">
            Account <span className="mono">{c.accountId}</span> &middot; review and confirm
            before saving.
          </p>
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
              data-component-id="card-edit-reload"
            >
              Reload latest values
            </button>
          </div>
        </div>
      )}
      {serverError && (
        <div className="alert error" role="alert">
          <div><b>Save failed.</b> {serverError}</div>
        </div>
      )}

      <form onSubmit={handleSubmit((values) => setConfirming(values))} noValidate>
        <div className="card" style={{ maxWidth: 560 }}>
          <div className="card-body">
            <div className="field">
              <label htmlFor="ce-cardNumber">Card number</label>
              <input
                id="ce-cardNumber"
                className="input mono"
                value={c.cardNumber}
                disabled
                data-component-id="card-edit-number"
              />
              <div className="hint">Card number cannot be changed</div>
            </div>
            <div className="field" style={{ marginTop: 16 }}>
              <label htmlFor="ce-embossedName">
                Embossed name <span className="req">*</span>
              </label>
              <input
                id="ce-embossedName"
                className={`input${errors.embossedName ? ' invalid' : ''}`}
                aria-invalid={errors.embossedName ? true : undefined}
                data-component-id="card-edit-embossed-name"
                {...register('embossedName')}
              />
              {errors.embossedName && (
                <div className="error-text">{errors.embossedName.message}</div>
              )}
            </div>
            <div className="form-grid" style={{ marginTop: 16 }}>
              <div className="field">
                <label htmlFor="ce-expiryMonth">
                  Expiry month <span className="req">*</span>
                </label>
                <input
                  id="ce-expiryMonth"
                  className={`input mono${errors.expiryMonth ? ' invalid' : ''}`}
                  aria-invalid={errors.expiryMonth ? true : undefined}
                  data-component-id="card-edit-expiry-month"
                  {...register('expiryMonth')}
                />
                {errors.expiryMonth ? (
                  <div className="error-text">{errors.expiryMonth.message}</div>
                ) : (
                  <div className="hint">1–12</div>
                )}
              </div>
              <div className="field">
                <label htmlFor="ce-expiryYear">
                  Expiry year <span className="req">*</span>
                </label>
                <input
                  id="ce-expiryYear"
                  className={`input mono${errors.expiryYear ? ' invalid' : ''}`}
                  aria-invalid={errors.expiryYear ? true : undefined}
                  data-component-id="card-edit-expiry-year"
                  {...register('expiryYear')}
                />
                {errors.expiryYear ? (
                  <div className="error-text">{errors.expiryYear.message}</div>
                ) : (
                  <div className="hint">4 digits, 1950–2099</div>
                )}
              </div>
            </div>
            <div className="field" style={{ marginTop: 16 }}>
              <label>
                Active status <span className="req">*</span>
              </label>
              <div className="radio-row" role="radiogroup" aria-label="Active status">
                <label className="radio">
                  <input
                    type="radio"
                    value="Y"
                    data-component-id="card-edit-status-active"
                    {...register('activeStatus')}
                  />{' '}
                  Y — Active
                </label>
                <label className="radio">
                  <input
                    type="radio"
                    value="N"
                    data-component-id="card-edit-status-inactive"
                    {...register('activeStatus')}
                  />{' '}
                  N — Inactive
                </label>
              </div>
              {errors.activeStatus && (
                <div className="error-text">{errors.activeStatus.message}</div>
              )}
            </div>
          </div>
          <div className="form-foot">
            <div className="note">Concurrent edits are detected on save.</div>
            <div style={{ display: 'flex', gap: 10 }}>
              <Link
                className="btn btn-secondary"
                to={`/cards/${cardNumber}`}
                data-component-id="card-edit-cancel"
              >
                Cancel
              </Link>
              <button
                type="submit"
                className="btn btn-primary"
                disabled={saving}
                data-component-id="card-edit-review"
              >
                Review changes
              </button>
            </div>
          </div>
        </div>
      </form>

      {confirming && (
        <div className="overlay" role="dialog" aria-modal="true" aria-label="Confirm card update">
          <div className="modal">
            <div className="m-head">
              <h2>Confirm card update</h2>
            </div>
            <div className="m-body">
              <dl className="kv">
                <dt>Card</dt>
                <dd className="mono">{c.maskedCardNumber}</dd>
                <dt>Embossed name</dt>
                <dd>{confirming.embossedName}</dd>
                <dt>Expiry</dt>
                <dd className="mono">
                  {confirming.expiryYear}-{confirming.expiryMonth.padStart(2, '0')}
                </dd>
                <dt>Status</dt>
                <dd>{confirming.activeStatus === 'Y' ? 'Y — Active' : 'N — Inactive'}</dd>
              </dl>
            </div>
            <div className="m-foot">
              <button
                type="button"
                className="btn btn-secondary"
                onClick={() => setConfirming(null)}
                data-component-id="card-edit-confirm-cancel"
              >
                Go back
              </button>
              <button
                type="button"
                className="btn btn-primary"
                disabled={saving}
                onClick={() => save(confirming)}
                data-component-id="card-edit-confirm-save"
              >
                {saving ? 'Saving…' : 'Confirm and save'}
              </button>
            </div>
          </div>
        </div>
      )}
    </>
  )
}
