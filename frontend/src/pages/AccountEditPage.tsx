import { useEffect, useState } from 'react'
import { Link, useNavigate, useParams } from 'react-router-dom'
import { useQuery, useQueryClient } from '@tanstack/react-query'
import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { z } from 'zod'
import { api, ApiError, retryUnlessNotFound } from '../api/client'
import type { AccountUpdateRequest, AccountView } from '../api/types'
import { IconBack } from '../components/icons'

/**
 * /accounts/:id/edit — COACTUPC account+customer update
 * (account-edit-sectioned mockup). Client-side zod mirrors the key
 * 1200-EDIT-MAP-INPUTS rules; the server re-runs the full legacy edit suite
 * and 400 responses carry fieldErrors that are mapped back onto the form.
 */

const SIGNED_AMOUNT = /^[+-]?\$?(\d{1,3}(,\d{3})*|\d+)(\.\d{1,2})?$/
const DATE_YMD = /^\d{4}-\d{2}-\d{2}$/
const ALPHA = /^[A-Za-z ]+$/

const schema = z.object({
  activeStatus: z.enum(['Y', 'N'], { message: 'Account Status must be Y or N.' }),
  creditLimit: z.string().regex(SIGNED_AMOUNT, 'Credit Limit is not valid'),
  cashCreditLimit: z.string().regex(SIGNED_AMOUNT, 'Cash Credit Limit is not valid'),
  currBal: z.string().regex(SIGNED_AMOUNT, 'Current Balance is not valid'),
  openDate: z.string().regex(DATE_YMD, 'Open Date must be YYYY-MM-DD.'),
  expirationDate: z.string().regex(DATE_YMD, 'Expiry Date must be YYYY-MM-DD.'),
  reissueDate: z.string().regex(DATE_YMD, 'Reissue Date must be YYYY-MM-DD.'),
  currCycCredit: z.string().regex(SIGNED_AMOUNT, 'Current Cycle Credit Limit is not valid'),
  currCycDebit: z.string().regex(SIGNED_AMOUNT, 'Current Cycle Debit Limit is not valid'),
  groupId: z.string(),
  firstName: z.string().regex(ALPHA, 'First Name can have alphabets only.'),
  middleName: z
    .string()
    .refine((v) => v.trim() === '' || ALPHA.test(v), 'Middle Name can have alphabets only.'),
  lastName: z.string().regex(ALPHA, 'Last Name can have alphabets only.'),
  ssn: z.string(),
  dob: z.string().regex(DATE_YMD, 'Date of Birth must be YYYY-MM-DD.'),
  ficoScore: z
    .string()
    .regex(/^\d{3}$/, 'FICO Score must be all numeric.')
    .refine((v) => Number(v) >= 300 && Number(v) <= 850, 'FICO Score: should be between 300 and 850'),
  addrLine1: z.string().trim().min(1, 'Address Line 1 must be supplied.'),
  addrLine2: z.string(),
  city: z.string().regex(ALPHA, 'City can have alphabets only.'),
  stateCd: z.string().regex(/^[A-Za-z]{2}$/, 'State must be a 2 letter code.'),
  zip: z.string().regex(/^\d{5}$/, 'Zip must be all numeric.'),
  countryCd: z.string().regex(ALPHA, 'Country can have alphabets only.'),
  phone1: z.string(),
  phone2: z.string(),
  govtIssuedId: z.string(),
  eftAccountId: z.string().regex(/^\d{10}$/, 'EFT Account Id must be all numeric.'),
  priCardHolder: z.enum(['Y', 'N'], { message: 'Primary Card Holder must be Y or N.' }),
})

type FormValues = z.infer<typeof schema>

function amount(v: number): string {
  return v.toFixed(2)
}

function toFormValues(a: AccountView): FormValues {
  const c = a.customer
  return {
    activeStatus: a.activeStatus === 'N' ? 'N' : 'Y',
    creditLimit: amount(a.creditLimit),
    cashCreditLimit: amount(a.cashCreditLimit),
    currBal: amount(a.currBal),
    openDate: a.openDate ?? '',
    expirationDate: a.expirationDate ?? '',
    reissueDate: a.reissueDate ?? '',
    currCycCredit: amount(a.currCycCredit),
    currCycDebit: amount(a.currCycDebit),
    groupId: a.groupId ?? '',
    firstName: c?.firstName ?? '',
    middleName: c?.middleName ?? '',
    lastName: c?.lastName ?? '',
    ssn: c?.ssnLast4 ? `***-**-${c.ssnLast4}` : '',
    dob: c?.dob ?? '',
    ficoScore: c?.ficoScore != null ? String(c.ficoScore) : '',
    addrLine1: c?.addrLine1 ?? '',
    addrLine2: c?.addrLine2 ?? '',
    city: c?.addrLine3 ?? '',
    stateCd: c?.addrStateCd ?? '',
    zip: c?.addrZip ?? '',
    countryCd: c?.addrCountryCd ?? '',
    phone1: c?.phoneNum1 ?? '',
    phone2: c?.phoneNum2 ?? '',
    govtIssuedId: c?.govtIssuedId ?? '',
    eftAccountId: c?.eftAccountId ?? '',
    priCardHolder: c?.priCardHolder === 'N' ? 'N' : 'Y',
  }
}

export default function AccountEditPage() {
  const { id } = useParams<{ id: string }>()
  const navigate = useNavigate()
  const queryClient = useQueryClient()
  const [conflict, setConflict] = useState<string | null>(null)
  const [serverError, setServerError] = useState<string | null>(null)
  const [saving, setSaving] = useState(false)

  const query = useQuery({
    queryKey: ['account', id],
    queryFn: () => api.get<AccountView>(`/api/v1/accounts/${id}`),
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
    return <div className="card"><div className="card-body">Loading account…</div></div>
  }
  if (query.isError || !query.data.customer) {
    return (
      <div className="alert error" role="alert">
        <div>
          <b>Could not load account for editing.</b>{' '}
          <Link to="/accounts">Back to accounts</Link>
        </div>
      </div>
    )
  }

  const a = query.data
  const c = a.customer!
  const maskedSsn = c.ssnLast4 ? `***-**-${c.ssnLast4}` : ''
  const errorCount = Object.keys(errors).length

  const onSubmit = async (values: FormValues) => {
    setConflict(null)
    setServerError(null)
    setSaving(true)
    const ssnUnchanged = values.ssn.trim() === '' || values.ssn.trim() === maskedSsn
    const payload: AccountUpdateRequest = {
      accountVersion: a.version,
      customerVersion: c.version,
      activeStatus: values.activeStatus,
      creditLimit: values.creditLimit,
      cashCreditLimit: values.cashCreditLimit,
      currBal: values.currBal,
      openDate: values.openDate,
      expirationDate: values.expirationDate,
      reissueDate: values.reissueDate,
      currCycCredit: values.currCycCredit,
      currCycDebit: values.currCycDebit,
      groupId: values.groupId.trim() || null,
      firstName: values.firstName,
      middleName: values.middleName.trim() || null,
      lastName: values.lastName,
      ssn: ssnUnchanged ? null : values.ssn,
      dob: values.dob,
      ficoScore: values.ficoScore,
      addrLine1: values.addrLine1,
      addrLine2: values.addrLine2.trim() || null,
      city: values.city,
      stateCd: values.stateCd,
      zip: values.zip,
      countryCd: values.countryCd,
      phone1: values.phone1.trim() || null,
      phone2: values.phone2.trim() || null,
      govtIssuedId: values.govtIssuedId.trim() || null,
      eftAccountId: values.eftAccountId,
      priCardHolder: values.priCardHolder,
    }
    try {
      await api.put<AccountView>(`/api/v1/accounts/${id}`, payload)
      await queryClient.invalidateQueries({ queryKey: ['account', id] })
      navigate(`/accounts/${id}`)
    } catch (err) {
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

  const field = (
    name: keyof FormValues,
    label: string,
    opts: {
      required?: boolean
      optional?: boolean
      hint?: string
      mono?: boolean
      span2?: boolean
      maxWidth?: number
    } = {},
  ) => {
    const err = errors[name]
    return (
      <div className={`field${opts.span2 ? ' span2' : ''}`}>
        <label htmlFor={`ae-${name}`}>
          {label}{' '}
          {opts.required && <span className="req">*</span>}
          {opts.optional && <span className="opt">(optional)</span>}
        </label>
        <input
          id={`ae-${name}`}
          className={`input${opts.mono ? ' mono' : ''}${err ? ' invalid' : ''}`}
          style={opts.maxWidth ? { maxWidth: opts.maxWidth } : undefined}
          aria-invalid={err ? true : undefined}
          data-component-id={`acct-edit-${name}`}
          {...register(name)}
        />
        {err ? (
          <div className="error-text">{err.message}</div>
        ) : opts.hint ? (
          <div className="hint">{opts.hint}</div>
        ) : null}
      </div>
    )
  }

  return (
    <>
      <div className="page-head">
        <div>
          <div style={{ display: 'flex', alignItems: 'center', gap: 10 }}>
            <Link
              className="icon-btn"
              to={`/accounts/${id}`}
              aria-label="Back to account"
              data-component-id="acct-edit-back"
            >
              <IconBack />
            </Link>
            <h1 className="page-title">
              Update account <span className="mono" style={{ fontSize: 19 }}>{a.accountId}</span>
            </h1>
          </div>
          <p className="page-sub">
            {[c.firstName, c.lastName].filter(Boolean).join(' ')} &middot; Changes are validated
            field by field and saved together.
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
              data-component-id="acct-edit-reload"
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
      {errorCount > 0 && (
        <div className="alert error" role="alert">
          <div>
            <b>
              {errorCount} {errorCount === 1 ? 'field needs' : 'fields need'} attention.
            </b>{' '}
            Fix the highlighted fields below, then save again. Nothing has been saved yet.
          </div>
        </div>
      )}

      <form onSubmit={handleSubmit(onSubmit)} noValidate>
        <div className="card">
          <div className="card-body">
            <fieldset className="section">
              <legend className="section-title">Account</legend>
              <div className="form-grid">
                <div className="field">
                  <label htmlFor="ae-accountId">Account ID</label>
                  <input
                    id="ae-accountId"
                    className="input mono"
                    value={a.accountId}
                    disabled
                    data-component-id="acct-edit-acct-id"
                  />
                  <div className="hint">Account ID cannot be changed</div>
                </div>
                <div className="field">
                  <label htmlFor="ae-activeStatus">
                    Active status <span className="req">*</span>
                  </label>
                  <select
                    id="ae-activeStatus"
                    className={`select${errors.activeStatus ? ' invalid' : ''}`}
                    data-component-id="acct-edit-status"
                    {...register('activeStatus')}
                  >
                    <option value="Y">Y — Active</option>
                    <option value="N">N — Inactive</option>
                  </select>
                  {errors.activeStatus && (
                    <div className="error-text">{errors.activeStatus.message}</div>
                  )}
                </div>
                {field('creditLimit', 'Credit limit', {
                  required: true,
                  mono: true,
                  hint: 'Signed amount, 2 decimals',
                })}
                {field('cashCreditLimit', 'Cash credit limit', { required: true, mono: true })}
                {field('currBal', 'Current balance', { required: true, mono: true })}
                {field('openDate', 'Open date', { required: true, mono: true, hint: 'YYYY-MM-DD' })}
                {field('expirationDate', 'Expiration date', {
                  required: true,
                  mono: true,
                  hint: 'YYYY-MM-DD',
                })}
                {field('reissueDate', 'Reissue date', { required: true, mono: true })}
                {field('currCycCredit', 'Current cycle credit', { required: true, mono: true })}
                {field('currCycDebit', 'Current cycle debit', { required: true, mono: true })}
                {field('groupId', 'Account group ID', { optional: true, mono: true })}
              </div>
            </fieldset>

            <fieldset className="section">
              <legend className="section-title">Customer</legend>
              <div className="form-grid">
                {field('firstName', 'First name', { required: true })}
                {field('middleName', 'Middle name', { optional: true })}
                {field('lastName', 'Last name', { required: true })}
                {field('ssn', 'SSN', {
                  required: true,
                  mono: true,
                  hint: 'Shown masked; type a full SSN only to change it',
                })}
                {field('dob', 'Date of birth', { required: true, mono: true, hint: 'YYYY-MM-DD' })}
                {field('ficoScore', 'FICO score', { required: true, mono: true, hint: '300–850' })}
                {field('addrLine1', 'Address line 1', { required: true, span2: true })}
                {field('addrLine2', 'Address line 2', { optional: true, span2: true })}
                {field('city', 'City', { required: true })}
                {field('stateCd', 'State', { required: true, mono: true, maxWidth: 120 })}
                {field('zip', 'ZIP code', { required: true, mono: true, maxWidth: 160 })}
                {field('countryCd', 'Country code', { required: true, mono: true })}
                {field('phone1', 'Phone 1', {
                  optional: true,
                  mono: true,
                  hint: 'US format (AAA)NNN-NNNN',
                })}
                {field('phone2', 'Phone 2', { optional: true, mono: true })}
                {field('govtIssuedId', 'Government issued ID', { optional: true, mono: true })}
                {field('eftAccountId', 'EFT account ID', {
                  required: true,
                  mono: true,
                  hint: '10 digits',
                })}
                <div className="field">
                  <label>
                    Primary card holder <span className="req">*</span>
                  </label>
                  <div className="radio-row" role="radiogroup" aria-label="Primary card holder">
                    <label className="radio">
                      <input
                        type="radio"
                        value="Y"
                        data-component-id="acct-edit-pri-yes"
                        {...register('priCardHolder')}
                      />{' '}
                      Y — Yes
                    </label>
                    <label className="radio">
                      <input
                        type="radio"
                        value="N"
                        data-component-id="acct-edit-pri-no"
                        {...register('priCardHolder')}
                      />{' '}
                      N — No
                    </label>
                  </div>
                  {errors.priCardHolder && (
                    <div className="error-text">{errors.priCardHolder.message}</div>
                  )}
                </div>
              </div>
            </fieldset>
          </div>
          <div className="form-foot">
            <div className="note">Concurrent edits are detected on save.</div>
            <div style={{ display: 'flex', gap: 10 }}>
              <Link
                className="btn btn-secondary"
                to={`/accounts/${id}`}
                data-component-id="acct-edit-cancel"
              >
                Cancel
              </Link>
              <button
                type="submit"
                className="btn btn-primary"
                disabled={saving}
                data-component-id="acct-edit-save"
              >
                {saving ? 'Saving…' : 'Save changes'}
              </button>
            </div>
          </div>
        </div>
      </form>
    </>
  )
}
