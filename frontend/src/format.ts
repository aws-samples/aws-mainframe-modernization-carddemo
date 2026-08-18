const usd = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' })

export function money(v: number | null | undefined): string {
  if (v == null) return '—'
  if (v < 0) return `-${usd.format(-v)}`
  return usd.format(v)
}

export function dateOnly(v: string | null | undefined): string {
  if (!v) return '—'
  return v.slice(0, 10)
}

export function dateTime(v: string | null | undefined): string {
  if (!v) return '—'
  return v.slice(0, 19).replace('T', ' ')
}

/** Card expiration as YYYY-MM. */
export function expMonth(v: string | null | undefined): string {
  if (!v) return '—'
  return v.slice(0, 7)
}

/** 'Y'/'A' style status char -> label + badge color. */
export function cardStatus(status: string | null | undefined): {
  label: string
  color: 'green' | 'gray'
} {
  return status === 'Y'
    ? { label: 'Active', color: 'green' }
    : { label: 'Inactive', color: 'gray' }
}

/**
 * "75", "75.5", "-25.99", "+00000075.00" → legacy "+00000075.00" (or null).
 * The COTRN02C amount edit demands sign + 8 digits + '.' + 2 decimals; this
 * mirrors the legacy NUMVAL-C re-format so users can type natural amounts.
 */
export function toLegacyAmount(input: string): string | null {
  const m = /^([+-]?)(\d{1,8})(?:\.(\d{1,2}))?$/.exec(input.trim())
  if (!m) return null
  const sign = m[1] === '-' ? '-' : '+'
  const whole = m[2].padStart(8, '0')
  const cents = (m[3] ?? '').padEnd(2, '0')
  return `${sign}${whole}.${cents}`
}

export const TXN_TYPE_LABELS: Record<string, string> = {
  '01': 'Purchase',
  '02': 'Payment',
  '03': 'Credit',
  '04': 'Authorization',
  '05': 'Refund',
  '06': 'Reversal',
  '07': 'Adjustment',
}

export function txnTypeLabel(typeCd: string | null | undefined): string {
  if (!typeCd) return '—'
  const label = TXN_TYPE_LABELS[typeCd]
  return label ? `${typeCd} ${label}` : typeCd
}

export function txnBadgeColor(typeCd: string | null | undefined): 'blue' | 'green' | 'gray' {
  if (typeCd === '02' || typeCd === '03') return 'green'
  if (typeCd === '01') return 'blue'
  return 'gray'
}

export function initials(first: string | null, last: string | null, fallback: string): string {
  const a = first?.trim()?.[0] ?? ''
  const b = last?.trim()?.[0] ?? ''
  const s = `${a}${b}`.toUpperCase()
  return s || fallback.slice(0, 2).toUpperCase()
}

export function fullName(first: string | null, last: string | null, fallback: string): string {
  const s = [first?.trim(), last?.trim()].filter(Boolean).join(' ')
  return s || fallback
}
