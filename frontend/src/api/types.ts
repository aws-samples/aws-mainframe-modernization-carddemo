/** Response shapes mirrored from backend ApiDtos / AuthController (plan §6). */

export interface LoginResponse {
  token: string
  expiresInSeconds: number
  userId: string
  firstName: string | null
  lastName: string | null
  role: 'ADMIN' | 'USER'
}

export interface AuthUser {
  userId: string
  firstName: string | null
  lastName: string | null
  role: 'ADMIN' | 'USER'
}

export interface CardSummary {
  cardNumber: string | null
  maskedCardNumber: string
  accountId: number
  embossedName: string | null
  expirationDate: string | null
  activeStatus: string | null
}

export interface CardDetail {
  cardNumber: string
  maskedCardNumber: string
  accountId: number
  embossedName: string | null
  expirationDate: string | null
  activeStatus: string | null
  version: number
}

export interface CustomerView {
  customerId: number
  firstName: string | null
  middleName: string | null
  lastName: string | null
  addrLine1: string | null
  addrLine2: string | null
  addrLine3: string | null
  addrStateCd: string | null
  addrCountryCd: string | null
  addrZip: string | null
  phoneNum1: string | null
  phoneNum2: string | null
  ssnLast4: string | null
  govtIssuedId: string | null
  dob: string | null
  eftAccountId: string | null
  priCardHolder: string | null
  ficoScore: number | null
  version: number
}

export interface AccountView {
  accountId: number
  activeStatus: string | null
  currBal: number
  creditLimit: number
  cashCreditLimit: number
  openDate: string | null
  expirationDate: string | null
  reissueDate: string | null
  currCycCredit: number
  currCycDebit: number
  zip: string | null
  groupId: string | null
  version: number
  customer: CustomerView | null
  cards: CardSummary[]
}

/** PUT /api/v1/accounts/{id} — COACTUPC fields; all strings, server re-edits. */
export interface AccountUpdateRequest {
  accountVersion: number
  customerVersion: number
  activeStatus: string
  creditLimit: string
  cashCreditLimit: string
  currBal: string
  openDate: string
  expirationDate: string
  reissueDate: string
  currCycCredit: string
  currCycDebit: string
  groupId: string | null
  firstName: string
  middleName: string | null
  lastName: string
  ssn: string | null // null = keep stored value
  dob: string
  ficoScore: string
  addrLine1: string
  addrLine2: string | null
  city: string
  stateCd: string
  zip: string
  countryCd: string
  phone1: string | null
  phone2: string | null
  govtIssuedId: string | null
  eftAccountId: string
  priCardHolder: string
}

/** PUT /api/v1/cards/{cardNumber} — COCRDUPC editable fields. */
export interface CardUpdateRequest {
  version: number
  embossedName: string
  expiryMonth: string
  expiryYear: string
  activeStatus: string
}

/** Admin user CRUD (COUSR00C-03C). Password never returned. */
export interface UserView {
  userId: string
  firstName: string | null
  lastName: string | null
  userType: 'A' | 'U' | string
  version: number
}

export interface UserCreateRequest {
  userId: string
  firstName: string
  lastName: string
  password: string
  userType: string
}

export interface UserUpdateRequest {
  version: number
  firstName: string
  lastName: string
  password: string | null // blank/null = keep current
  userType: string
}

/**
 * POST /api/v1/transactions — COTRN02C add-transaction input. All strings;
 * the server re-runs the full legacy edit suite. Either accountId or
 * cardNumber may be entered (account wins when both are present).
 */
export interface AddTransactionRequest {
  accountId: string | null
  cardNumber: string | null
  typeCd: string
  categoryCd: string
  source: string
  description: string
  amount: string // exact legacy format: sign + 8 digits + '.' + 2 decimals
  origDate: string
  procDate: string
  merchantId: string
  merchantName: string
  merchantCity: string
  merchantZip: string
}

export interface TransactionView {
  transactionId: string
  typeCd: string | null
  categoryCd: number | null
  source: string | null
  description: string | null
  amount: number
  merchantId: number | null
  merchantName: string | null
  merchantCity: string | null
  merchantZip: string | null
  maskedCardNumber: string
  accountId: number | null
  origTs: string | null
  procTs: string | null
}

export interface Page<T> {
  items: T[]
  nextCursor: string | null
}

/**
 * One batch job run (Spring Batch execution) from GET /admin/jobs/runs.
 * posted/rejected are POSTTRAN-only counters; null for INTCALC.
 */
export interface JobRunView {
  runId: number
  jobName: string
  status: string
  exitCode: string
  startTime: string | null
  endTime: string | null
  readCount: number
  writeCount: number
  posted: number | null
  rejected: number | null
}

/** One report_runs row (POST /reports/transaction, GET /reports). */
export interface ReportRunView {
  id: number
  reportType: string
  fromDate: string
  toDate: string
  status: string
  requestedBy: string
  createdAt: string | null
  message: string | null
}

/** POST /api/v1/reports/transaction — CORPT00C per-part date fields. */
export interface ReportRequest {
  reportType: 'monthly' | 'yearly' | 'custom'
  startMonth?: string
  startDay?: string
  startYear?: string
  endMonth?: string
  endDay?: string
  endYear?: string
}

/** One statements row (GET /accounts/{id}/statements). */
export interface StatementView {
  id: number
  accountId: number
  cycleDate: string
  createdAt: string | null
}
