import { useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { useForm } from 'react-hook-form'
import { zodResolver } from '@hookform/resolvers/zod'
import { z } from 'zod'
import { api, ApiError } from '../api/client'
import type { AddTransactionRequest, TransactionView } from '../api/types'
import { toLegacyAmount, TXN_TYPE_LABELS } from '../format'

/**
 * /transactions/new — transaction-add-form + transaction-add-confirm mockups
 * (COTRN02C). Two steps: Details → Confirm; the POST is issued once, after
 * the user confirms (the legacy Y/N confirm loop is this UI step).
 *
 * The API demands the exact legacy amount format (sign + 8 digits + '.' + 2
 * decimals, COTRN02C 339-351); user input like "75.00" is normalized to
 * "+00000075.00" before the POST, mirroring the legacy NUMVAL-C re-format.
 */

/** Transaction categories from the TRANCATG reference file, keyed by type. */
const CATEGORY_OPTIONS: Record<string, { code: string; label: string }[]> = {
  '01': [
    { code: '0001', label: 'Regular Sales Draft' },
    { code: '0002', label: 'Regular Cash Advance' },
    { code: '0003', label: 'Convenience Check Debit' },
    { code: '0004', label: 'ATM Cash Advance' },
    { code: '0005', label: 'Interest Amount' },
  ],
  '02': [
    { code: '0001', label: 'Cash payment' },
    { code: '0002', label: 'Electronic payment' },
    { code: '0003', label: 'Check payment' },
  ],
  '03': [
    { code: '0001', label: 'Credit to Account' },
    { code: '0002', label: 'Credit to Purchase balance' },
    { code: '0003', label: 'Credit to Cash balance' },
  ],
  '04': [
    { code: '0001', label: 'Zero dollar authorization' },
    { code: '0002', label: 'Online purchase authorization' },
    { code: '0003', label: 'Travel booking authorization' },
  ],
  '05': [{ code: '0001', label: 'Refund credit' }],
  '06': [
    { code: '0001', label: 'Fraud reversal' },
    { code: '0002', label: 'Non-fraud reversal' },
  ],
  '07': [{ code: '0001', label: 'Sales draft credit adjustment' }],
}

const DATE_RE = /^\d{4}-\d{2}-\d{2}$/

const schema = z
  .object({
    accountId: z.string().trim().regex(/^\d*$/, 'Account ID must be Numeric...'),
    cardNumber: z.string().trim().regex(/^\d*$/, 'Card Number must be Numeric...'),
    typeCd: z.string().trim().min(1, 'Type CD can NOT be empty...'),
    categoryCd: z.string().trim().min(1, 'Category CD can NOT be empty...'),
    source: z.string().trim().min(1, 'Source can NOT be empty...'),
    description: z.string().trim().min(1, 'Description can NOT be empty...'),
    amount: z
      .string()
      .trim()
      .min(1, 'Amount can NOT be empty...')
      .refine((v) => toLegacyAmount(v) !== null, 'Amount should be in format -99999999.99'),
    origDate: z
      .string()
      .trim()
      .regex(DATE_RE, 'Orig Date should be in format YYYY-MM-DD'),
    procDate: z
      .string()
      .trim()
      .regex(DATE_RE, 'Proc Date should be in format YYYY-MM-DD'),
    merchantId: z
      .string()
      .trim()
      .min(1, 'Merchant ID can NOT be empty...')
      .regex(/^\d+$/, 'Merchant ID must be Numeric...'),
    merchantName: z.string().trim().min(1, 'Merchant Name can NOT be empty...'),
    merchantCity: z.string().trim().min(1, 'Merchant City can NOT be empty...'),
    merchantZip: z.string().trim().min(1, 'Merchant Zip can NOT be empty...'),
  })
  .refine((v) => v.accountId !== '' || v.cardNumber !== '', {
    message: 'Account or Card Number must be entered...',
    path: ['accountId'],
  })

type FormValues = z.infer<typeof schema>

const today = () => new Date().toISOString().slice(0, 10)

export default function TransactionAddPage() {
  const navigate = useNavigate()
  const [confirming, setConfirming] = useState<FormValues | null>(null)
  const [serverError, setServerError] = useState<string | null>(null)
  const [posting, setPosting] = useState(false)

  const {
    register,
    handleSubmit,
    watch,
    setError,
    formState: { errors },
  } = useForm<FormValues>({
    resolver: zodResolver(schema),
    mode: 'onBlur',
    defaultValues: {
      accountId: '',
      cardNumber: '',
      typeCd: '01',
      categoryCd: '0001',
      source: 'POS TERM',
      description: '',
      amount: '',
      origDate: today(),
      procDate: today(),
      merchantId: '',
      merchantName: '',
      merchantCity: '',
      merchantZip: '',
    },
  })

  const typeCd = watch('typeCd')
  const categories = CATEGORY_OPTIONS[typeCd] ?? CATEGORY_OPTIONS['01']

  const toConfirm = (values: FormValues) => {
    setServerError(null)
    setConfirming(values)
  }

  const post = async () => {
    if (!confirming) return
    setServerError(null)
    setPosting(true)
    const payload: AddTransactionRequest = {
      accountId: confirming.accountId || null,
      cardNumber: confirming.cardNumber || null,
      typeCd: confirming.typeCd,
      categoryCd: confirming.categoryCd,
      source: confirming.source,
      description: confirming.description,
      amount: toLegacyAmount(confirming.amount) ?? confirming.amount,
      origDate: confirming.origDate,
      procDate: confirming.procDate,
      merchantId: confirming.merchantId,
      merchantName: confirming.merchantName,
      merchantCity: confirming.merchantCity,
      merchantZip: confirming.merchantZip,
    }
    try {
      const created = await api.post<TransactionView>('/api/v1/transactions', payload)
      navigate(`/transactions/${created.transactionId}`)
    } catch (err) {
      setConfirming(null)
      if (err instanceof ApiError && err.fieldErrors.length > 0) {
        for (const fe of err.fieldErrors) {
          setError(fe.field as keyof FormValues, { type: 'server', message: fe.message })
        }
      } else if (err instanceof ApiError) {
        setServerError(err.message)
      } else {
        setServerError('Post failed. Please try again.')
      }
    } finally {
      setPosting(false)
    }
  }

  const fieldError = (name: keyof FormValues) =>
    errors[name] && <div className="error-text">{errors[name]?.message}</div>

  if (confirming) {
    const catLabel =
      categories.find((c) => c.code === confirming.categoryCd)?.label ?? ''
    return (
      <>
        <div className="page-head">
          <div>
            <h1 className="page-title">Add transaction</h1>
            <p className="page-sub">
              Review the details below. The transaction is posted only after you confirm.
            </p>
          </div>
        </div>
        <div className="steps" aria-label="Progress">
          <div className="step done">
            <span className="n">&#10003;</span>Details
          </div>
          <div className="bar" />
          <div className="step active">
            <span className="n">2</span>Confirm
          </div>
        </div>
        <div className="card" style={{ maxWidth: 860 }}>
          <div className="card-head">
            <h2>Review &amp; confirm</h2>
            <span className="badge amber">
              <span className="dot" />
              Not yet posted
            </span>
          </div>
          <div className="card-body">
            <div className="alert info">
              <div>
                <b>Please verify every field.</b> Posting updates the account and cannot be
                undone from this screen.
              </div>
            </div>
            <div className="grid-2">
              <dl className="kv">
                <dt>Account ID</dt>
                <dd className="mono">{confirming.accountId || '—'}</dd>
                <dt>Card number</dt>
                <dd className="mono">{confirming.cardNumber || 'from account'}</dd>
                <dt>Type</dt>
                <dd>
                  {confirming.typeCd} — {TXN_TYPE_LABELS[confirming.typeCd] ?? ''}
                </dd>
                <dt>Category</dt>
                <dd>
                  {confirming.categoryCd}
                  {catLabel ? ` — ${catLabel}` : ''}
                </dd>
                <dt>Source</dt>
                <dd className="mono">{confirming.source}</dd>
                <dt>Amount</dt>
                <dd className="mono" style={{ fontWeight: 700 }}>
                  {toLegacyAmount(confirming.amount)}
                </dd>
              </dl>
              <dl className="kv">
                <dt>Description</dt>
                <dd>{confirming.description}</dd>
                <dt>Origination date</dt>
                <dd className="mono">{confirming.origDate}</dd>
                <dt>Processing date</dt>
                <dd className="mono">{confirming.procDate}</dd>
                <dt>Merchant</dt>
                <dd>
                  {confirming.merchantName} (ID {confirming.merchantId})
                </dd>
                <dt>Merchant location</dt>
                <dd>
                  {confirming.merchantCity}, {confirming.merchantZip}
                </dd>
              </dl>
            </div>
            {serverError && (
              <div className="alert error" role="alert" style={{ marginTop: 18 }}>
                <div>
                  <b>Could not post.</b> {serverError}
                </div>
              </div>
            )}
          </div>
          <div className="form-foot">
            <div className="note">Step 2 of 2</div>
            <div style={{ display: 'flex', gap: 10 }}>
              <button
                className="btn btn-secondary"
                type="button"
                onClick={() => setConfirming(null)}
                data-component-id="txn-confirm-back"
              >
                Back to edit
              </button>
              <button
                className="btn btn-primary"
                type="button"
                onClick={post}
                disabled={posting}
                data-component-id="txn-confirm-post"
              >
                {posting ? 'Posting…' : 'Confirm & post transaction'}
              </button>
            </div>
          </div>
        </div>
      </>
    )
  }

  return (
    <>
      <div className="page-head">
        <div>
          <h1 className="page-title">Add transaction</h1>
          <p className="page-sub">
            Enter the transaction details. You will review everything before it is posted.
          </p>
        </div>
      </div>
      <div className="steps" aria-label="Progress">
        <div className="step active">
          <span className="n">1</span>Details
        </div>
        <div className="bar" />
        <div className="step">
          <span className="n">2</span>Confirm
        </div>
      </div>
      <form onSubmit={handleSubmit(toConfirm)} noValidate>
        <div className="card" style={{ maxWidth: 860 }}>
          <div className="card-body">
            {serverError && (
              <div className="alert error" role="alert">
                <div>
                  <b>Could not post.</b> {serverError}
                </div>
              </div>
            )}
            <fieldset className="section">
              <legend className="section-title">Card &amp; account</legend>
              <div className="form-grid">
                <div className="field">
                  <label htmlFor="n-acct">Account ID</label>
                  <input
                    id="n-acct"
                    className={`input mono${errors.accountId ? ' invalid' : ''}`}
                    data-component-id="txn-new-account"
                    {...register('accountId')}
                  />
                  <div className="hint">Enter the account ID, the card number, or both</div>
                  {fieldError('accountId')}
                </div>
                <div className="field">
                  <label htmlFor="n-card">Card number</label>
                  <input
                    id="n-card"
                    className={`input mono${errors.cardNumber ? ' invalid' : ''}`}
                    data-component-id="txn-new-card"
                    {...register('cardNumber')}
                  />
                  {fieldError('cardNumber')}
                </div>
              </div>
            </fieldset>
            <fieldset className="section">
              <legend className="section-title">Transaction</legend>
              <div className="form-grid">
                <div className="field">
                  <label htmlFor="n-type">
                    Type <span className="req">*</span>
                  </label>
                  <select
                    id="n-type"
                    className="select"
                    data-component-id="txn-new-type"
                    {...register('typeCd')}
                  >
                    {Object.entries(TXN_TYPE_LABELS).map(([code, label]) => (
                      <option key={code} value={code}>
                        {code} — {label}
                      </option>
                    ))}
                  </select>
                  {fieldError('typeCd')}
                </div>
                <div className="field">
                  <label htmlFor="n-cat">
                    Category <span className="req">*</span>
                  </label>
                  <select
                    id="n-cat"
                    className="select"
                    data-component-id="txn-new-category"
                    {...register('categoryCd')}
                  >
                    {categories.map((c) => (
                      <option key={c.code} value={c.code}>
                        {c.code} — {c.label}
                      </option>
                    ))}
                  </select>
                  {fieldError('categoryCd')}
                </div>
                <div className="field">
                  <label htmlFor="n-src">
                    Source <span className="req">*</span>
                  </label>
                  <input
                    id="n-src"
                    className={`input mono${errors.source ? ' invalid' : ''}`}
                    maxLength={10}
                    data-component-id="txn-new-source"
                    {...register('source')}
                  />
                  {fieldError('source')}
                </div>
                <div className="field">
                  <label htmlFor="n-amt">
                    Amount <span className="req">*</span>
                  </label>
                  <input
                    id="n-amt"
                    className={`input mono${errors.amount ? ' invalid' : ''}`}
                    placeholder="75.00"
                    data-component-id="txn-new-amount"
                    {...register('amount')}
                  />
                  <div className="hint">Signed, 2 decimals, e.g. -25.99 for credits</div>
                  {fieldError('amount')}
                </div>
                <div className="field span2">
                  <label htmlFor="n-desc">
                    Description <span className="req">*</span>
                  </label>
                  <input
                    id="n-desc"
                    className={`input${errors.description ? ' invalid' : ''}`}
                    maxLength={100}
                    data-component-id="txn-new-description"
                    {...register('description')}
                  />
                  {fieldError('description')}
                </div>
                <div className="field">
                  <label htmlFor="n-orig">
                    Origination date <span className="req">*</span>
                  </label>
                  <input
                    id="n-orig"
                    className={`input mono${errors.origDate ? ' invalid' : ''}`}
                    data-component-id="txn-new-orig-date"
                    {...register('origDate')}
                  />
                  <div className="hint">YYYY-MM-DD</div>
                  {fieldError('origDate')}
                </div>
                <div className="field">
                  <label htmlFor="n-proc">
                    Processing date <span className="req">*</span>
                  </label>
                  <input
                    id="n-proc"
                    className={`input mono${errors.procDate ? ' invalid' : ''}`}
                    data-component-id="txn-new-proc-date"
                    {...register('procDate')}
                  />
                  {fieldError('procDate')}
                </div>
              </div>
            </fieldset>
            <fieldset className="section">
              <legend className="section-title">Merchant</legend>
              <div className="form-grid">
                <div className="field">
                  <label htmlFor="n-mid">
                    Merchant ID <span className="req">*</span>
                  </label>
                  <input
                    id="n-mid"
                    className={`input mono${errors.merchantId ? ' invalid' : ''}`}
                    data-component-id="txn-new-merchant-id"
                    {...register('merchantId')}
                  />
                  {fieldError('merchantId')}
                </div>
                <div className="field">
                  <label htmlFor="n-mname">
                    Merchant name <span className="req">*</span>
                  </label>
                  <input
                    id="n-mname"
                    className={`input${errors.merchantName ? ' invalid' : ''}`}
                    data-component-id="txn-new-merchant-name"
                    {...register('merchantName')}
                  />
                  {fieldError('merchantName')}
                </div>
                <div className="field">
                  <label htmlFor="n-mcity">
                    Merchant city <span className="req">*</span>
                  </label>
                  <input
                    id="n-mcity"
                    className={`input${errors.merchantCity ? ' invalid' : ''}`}
                    data-component-id="txn-new-merchant-city"
                    {...register('merchantCity')}
                  />
                  {fieldError('merchantCity')}
                </div>
                <div className="field">
                  <label htmlFor="n-mzip">
                    Merchant ZIP <span className="req">*</span>
                  </label>
                  <input
                    id="n-mzip"
                    className={`input mono${errors.merchantZip ? ' invalid' : ''}`}
                    style={{ maxWidth: 180 }}
                    data-component-id="txn-new-merchant-zip"
                    {...register('merchantZip')}
                  />
                  {fieldError('merchantZip')}
                </div>
              </div>
            </fieldset>
          </div>
          <div className="form-foot">
            <div className="note">
              The transaction ID is assigned automatically when the transaction is posted.
            </div>
            <div style={{ display: 'flex', gap: 10 }}>
              <Link
                className="btn btn-secondary"
                to="/transactions"
                data-component-id="txn-new-cancel"
              >
                Cancel
              </Link>
              <button
                className="btn btn-primary"
                type="submit"
                data-component-id="txn-new-continue"
              >
                Continue to confirm
              </button>
            </div>
          </div>
        </div>
      </form>
    </>
  )
}
