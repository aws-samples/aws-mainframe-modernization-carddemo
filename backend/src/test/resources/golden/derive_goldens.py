#!/usr/bin/env python3
"""Golden-file derivation for CardDemo Phase 4 batch parity (POSTTRAN / INTCALC).

This script hand-executes the CBTRN02C and CBACT04C rules DIRECTLY against the
committed ASCII fixture files (etl/fixtures/ascii). It is deliberately and
completely independent of the Java implementation under test: it parses the
fixed-width records itself (copybook offsets), applies the COBOL paragraphs in
source order, and emits the golden TSVs consumed by BatchParityTest.

Rules implemented (cited to the COBOL):

CBTRN02C (POSTTRAN), per daily record in file order (== daily_tran_id order,
the fixture file is sorted):
  1500-A-LOOKUP-XREF   card not in xref -> reason 100 (short-circuits 1500-B)
  1500-B-LOOKUP-ACCT   account missing  -> reason 101; else BOTH checks run,
                       LAST WINS (CBTRN02C.cbl:403-421):
                         102 if credit_limit < cyc_credit - cyc_debit + amt
                         103 if expiration_date < orig_ts(1:10)  (overwrites 102)
  2700-UPDATE-TCATBAL  +amt, create-if-missing (2700-A INITIALIZEs then ADDs)
  2800-UPDATE-ACCOUNT  curr_bal += amt; amt >= 0 -> cyc_credit += amt
                       else cyc_debit += amt (negative amounts make the debit
                       bucket negative — legacy behavior, preserved)
  2900-WRITE           TRAN-ID = DALYTRAN-ID, orig_ts copied, proc_ts = now
                       (proc_ts therefore excluded from the goldens)
  The account is RE-READ per record (1500-B READs ACCOUNT-FILE), so posted
  amounts affect the overlimit check of LATER records in the same run.

CBACT04C (INTCALC), per tcatbal row in key order (acct, type, cat):
  account break        1050-UPDATE-ACCOUNT for the PREVIOUS account:
                       curr_bal += total_int; cyc credit/debit reset to 0
  ***LEGACY QUIRK***   the main loop is PERFORM UNTIL END-OF-FILE='Y'
                       (test-before); its ELSE PERFORM 1050-UPDATE-ACCOUNT is
                       unreachable dead code, so the LAST account is NEVER
                       flushed: its interest transactions are written but its
                       curr_bal / cycle fields are left untouched. Replicated.
  1200-GET-INTEREST-RATE  disclosure_groups(acct.group, type, cat); NULL group
                       or miss -> 'DEFAULT' group; missing DEFAULT -> ABEND
                       (job fails). rate == 0 -> skip row (no txn, no total).
  1300-COMPUTE-INTEREST  monthly = bal * rate / 1200, COMPUTE without ROUNDED
                       -> truncated toward zero at 2 decimals (ROUND_DOWN).
                       A zero balance with nonzero rate still writes a 0.00
                       transaction (1300-B is unconditional inside 1300).
  1300-B-WRITE-TX      one txn PER tcatbal row: type '01', cat 0005,
                       source 'System', desc 'Int. for a/c <11-digit acct>',
                       merchant_id 0, merchant name/city/zip spaces (-> NULL),
                       card from xref by account, orig=proc=now, id from
                       PARM-DATE+suffix -> replaced by a sequence (plan R2),
                       so ids/timestamps are excluded from the golden and the
                       parity test compares rows by content.

Output: TSV (tab-separated, no quoting — descriptions contain commas), NULL as
empty string, money at fixed scale 2.

Run:  python3 backend/src/test/resources/golden/derive_goldens.py
"""
import sys
from collections import Counter
from decimal import Decimal
from pathlib import Path

HERE = Path(__file__).resolve().parent
ASCII = HERE.parents[4] / 'etl' / 'fixtures' / 'ascii'


# ---------------------------------------------------------------------------
# fixed-width parsing (copybook offsets; zoned-decimal overpunch)
# ---------------------------------------------------------------------------

def overpunch(s: str) -> int:
    """Trailing zoned-decimal overpunch: {=+0, A-I=+1..9, }=-0, J-R=-1..-9."""
    body, last = s[:-1], s[-1]
    if last.isdigit():
        return int(body + last)
    if last == '{':
        return int(body + '0')
    if 'A' <= last <= 'I':
        return int(body + str(ord(last) - ord('A') + 1))
    if last == '}':
        return -int(body + '0')
    if 'J' <= last <= 'R':
        return -int(body + str(ord(last) - ord('J') + 1))
    raise ValueError(f'bad overpunch char {last!r} in {s!r}')


def money(s: str) -> Decimal:
    return Decimal(overpunch(s)) / 100


def lines(name: str):
    return [l.rstrip('\r\n') for l in open(ASCII / name, encoding='ascii')]


# CVACT01Y offsets: id 0/11, status 11/1, curr_bal 12/12, credit_limit 24/12,
# cash_credit_limit 36/12, open 48/10, expiration 58/10, reissue 68/10,
# cyc_credit 78/12, cyc_debit 90/12, zip 102/10, group 112/10.
accounts = {}
for l in lines('acctdata.txt'):
    accounts[int(l[0:11])] = dict(
        curr_bal=money(l[12:24]), credit_limit=money(l[24:36]),
        expiration=l[58:68],
        cyc_credit=money(l[78:90]), cyc_debit=money(l[90:102]),
        group=l[112:122].strip() or None)

# CVACT03Y: card 0/16, cust 16/9, acct 25/11
xref = {l[0:16]: int(l[25:36]) for l in lines('cardxref.txt')}

# CVTRA01Y: acct 0/11, type 11/2, cat 13/4, bal 17/11
tcatbal = {(int(l[0:11]), l[11:13], int(l[13:17])): money(l[17:28])
           for l in lines('tcatbal.txt')}

# CVTRA02Y: group 0/10, type 10/2, cat 12/4, rate 16/6
discgrp = {(l[0:10].strip(), l[10:12], int(l[12:16])): money(l[16:22])
           for l in lines('discgrp.txt')}

# CVTRA06Y: id 0/16, type 16/2, cat 18/4, source 22/10, desc 32/100,
# amt 132/11, merch_id 143/9, merch_name 152/50, merch_city 202/50,
# merch_zip 252/10, card 262/16, orig_ts 278/26, proc_ts 304/26
daily = []
for l in lines('dailytran.txt'):
    daily.append(dict(
        id=l[0:16], type=l[16:18], cat=int(l[18:22]),
        source=l[22:32].strip(), desc=l[32:132].strip(),
        amt=money(l[132:143]), merch_id=int(l[143:152]),
        merch_name=l[152:202].strip(), merch_city=l[202:252].strip(),
        merch_zip=l[252:262].strip(), card=l[262:278],
        orig_ts=l[278:304].strip(), proc_ts=l[304:330].strip()))
# DALYTRAN is read sequentially; the fixture happens to be sorted by id, and
# the Java reader orders by daily_tran_id — assert the equivalence holds.
assert [d['id'] for d in daily] == sorted(d['id'] for d in daily), \
    'dailytran.txt is not id-sorted; file order != daily_tran_id order'


# ---------------------------------------------------------------------------
# POSTTRAN (CBTRN02C)
# ---------------------------------------------------------------------------
posted, rejects = [], []
for t in daily:
    reason, desc = 0, ''
    aid = None
    if t['card'] not in xref:                                   # 1500-A
        reason, desc = 100, 'INVALID CARD NUMBER FOUND'
    else:
        aid = xref[t['card']]
        if aid not in accounts:                                 # 1500-B
            reason, desc = 101, 'ACCOUNT RECORD NOT FOUND'
        else:
            acct = accounts[aid]
            temp = acct['cyc_credit'] - acct['cyc_debit'] + t['amt']
            if acct['credit_limit'] < temp:                     # 102
                reason, desc = 102, 'OVERLIMIT TRANSACTION'
            if acct['expiration'] < t['orig_ts'][0:10]:         # 103 last-wins
                reason, desc = 103, 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION'
    if reason == 0:
        key = (aid, t['type'], t['cat'])                        # 2700
        tcatbal[key] = tcatbal.get(key, Decimal(0)) + t['amt']
        acct['curr_bal'] += t['amt']                            # 2800
        if t['amt'] >= 0:
            acct['cyc_credit'] += t['amt']
        else:
            acct['cyc_debit'] += t['amt']
        posted.append(t)                                        # 2900
    else:
        rejects.append((t, reason, desc))

reasons = Counter(r for _, r, _ in rejects)
print(f'POSTTRAN: posted={len(posted)} rejected={len(rejects)} reasons={dict(reasons)}')

# ---------------------------------------------------------------------------
# write POSTTRAN goldens
# ---------------------------------------------------------------------------

def iso(ts: str) -> str:
    """DB2 'YYYY-MM-DD HH.MM.SS' / 'YYYY-MM-DD HH:MM:SS.ffffff' -> ISO seconds."""
    if not ts:
        return ''
    return ts[0:10] + 'T' + ts[11:19].replace('.', ':')


def tsv(path: Path, header: list, rows: list):
    with open(path, 'w', encoding='ascii', newline='\n') as f:
        f.write('\t'.join(header) + '\n')
        for r in rows:
            cells = ['' if c is None else str(c) for c in r]
            assert not any('\t' in c for c in cells), f'tab in data: {r}'
            f.write('\t'.join(cells) + '\n')


def m2(v: Decimal) -> str:
    return f'{v:.2f}'


PT = HERE / 'posttran'
IC = HERE / 'intcalc'
PT.mkdir(exist_ok=True)
IC.mkdir(exist_ok=True)

tsv(PT / 'transactions.tsv',
    ['transaction_id', 'type_cd', 'category_cd', 'source', 'description',
     'amount', 'merchant_id', 'merchant_name', 'merchant_city',
     'merchant_zip', 'card_number', 'orig_ts'],
    [[t['id'], t['type'], t['cat'], t['source'], t['desc'], m2(t['amt']),
      t['merch_id'], t['merch_name'], t['merch_city'], t['merch_zip'],
      t['card'], iso(t['orig_ts'])] for t in posted])

tsv(PT / 'transaction_rejects.tsv',
    ['daily_tran_id', 'type_cd', 'category_cd', 'source', 'description',
     'amount', 'merchant_id', 'merchant_name', 'merchant_city',
     'merchant_zip', 'card_number', 'orig_ts', 'fail_reason',
     'fail_reason_desc'],
    [[t['id'], t['type'], t['cat'], t['source'], t['desc'], m2(t['amt']),
      t['merch_id'], t['merch_name'], t['merch_city'], t['merch_zip'],
      t['card'], iso(t['orig_ts']), reason, desc]
     for t, reason, desc in rejects])

tsv(PT / 'accounts.tsv',
    ['account_id', 'curr_bal', 'curr_cyc_credit', 'curr_cyc_debit'],
    [[a, m2(v['curr_bal']), m2(v['cyc_credit']), m2(v['cyc_debit'])]
     for a, v in sorted(accounts.items())])

tsv(PT / 'transaction_category_balances.tsv',
    ['account_id', 'type_cd', 'category_cd', 'balance'],
    [[a, ty, cat, m2(bal)] for (a, ty, cat), bal in sorted(tcatbal.items())])

# ---------------------------------------------------------------------------
# INTCALC (CBACT04C) — runs against the post-POSTTRAN state
# ---------------------------------------------------------------------------

def rate_for(group, ty, cat):
    if group is not None and (group, ty, cat) in discgrp:
        return discgrp[(group, ty, cat)]
    if ('DEFAULT', ty, cat) not in discgrp:
        sys.exit(f'ABEND (1200-A): missing DEFAULT disclosure group ({ty},{cat})')
    return discgrp[('DEFAULT', ty, cat)]


card_by_acct = {}
for card, acct in sorted(xref.items()):
    card_by_acct.setdefault(acct, card)   # lowest card number per account

interest = []
last_acct, total, first = None, Decimal(0), True
for (aid, ty, cat), bal in sorted(tcatbal.items()):
    if aid != last_acct:
        if not first:                     # 1050 for the PREVIOUS account
            a = accounts[last_acct]
            a['curr_bal'] += total
            a['cyc_credit'] = Decimal(0)
            a['cyc_debit'] = Decimal(0)
        first = False
        total = Decimal(0)
        last_acct = aid
    r = rate_for(accounts[aid]['group'], ty, cat)
    if r != 0:
        monthly = (bal * r / Decimal(1200)).quantize(
            Decimal('0.01'), rounding='ROUND_DOWN')   # COMPUTE truncates
        total += monthly
        interest.append([aid, '01', 5, 'System', f'Int. for a/c {aid:011d}',
                         m2(monthly), 0, '', '', '', card_by_acct[aid]])
# LEGACY QUIRK (see module docstring): NO flush for the last account.
print(f'INTCALC: interest txns={len(interest)} '
      f'(zero-amount={sum(1 for i in interest if i[5] == "0.00")}); '
      f'last account NOT flushed: {last_acct}')

tsv(IC / 'interest_transactions.tsv',
    ['type_cd', 'category_cd', 'source', 'description', 'amount',
     'merchant_id', 'merchant_name', 'merchant_city', 'merchant_zip',
     'card_number'],
    [row[1:] for row in sorted(interest, key=lambda r: (r[4], r[1], r[2]))])

tsv(IC / 'accounts.tsv',
    ['account_id', 'curr_bal', 'curr_cyc_credit', 'curr_cyc_debit'],
    [[a, m2(v['curr_bal']), m2(v['cyc_credit']), m2(v['cyc_debit'])]
     for a, v in sorted(accounts.items())])

print('goldens written to', PT, 'and', IC)
