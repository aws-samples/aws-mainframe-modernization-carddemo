#!/usr/bin/env python3
"""Golden-file derivation for CardDemo Phase 5 (TRANREPT / CREASTMT).

Sibling of derive_goldens.py, same philosophy: hand-execute the COBOL rules
(CBTRN03C + CBSTM03A) DIRECTLY against the committed ASCII fixtures,
completely independently of the Java implementation under test.

Pipeline being modelled (matches ReportStatementParityTest):
  1. POSTTRAN (CBTRN02C) posts the daily file — re-simulated here exactly as
     in derive_goldens.py (posted set + account balances). INTCALC is NOT run.
  2. TRANREPT (CBTRN03C) over PARM range 2020-01-01..2099-12-31: every posted
     transaction qualifies (proc_ts = run time), sorted by (card, tran-id) —
     TRANREPT.prc STEP05R SORT FIELDS=(TRAN-CARD-NUM,A) with the id-ordered
     KSDS input as the tiebreak.
  3. CREASTMT (CBSTM03A) statements; goldens for TWO accounts (the first two
     cards in card-number order: 0500024453765740 -> acct 50,
     0683586198171516 -> acct 27).

CBTRN03C rules (all cited to the COBOL):
  - 133-byte report lines, WS-PAGE-SIZE=20; WS-LINE-COUNTER counts headers
    (4 lines), page-total block (2), account-total block (2), details (1).
  - card break -> 1120-WRITE-ACCOUNT-TOTALS for the PREVIOUS card ("account"
    totals break per CARD).
  - page break BEFORE a detail when MOD(counter,20)=0 -> 1110 page totals
    (rolls into grand) + 1120-WRITE-HEADERS.
  - EOF quirk (CBTRN03C.cbl:197-204): TRAN-RECORD still holds the last record
    after the failed READ, so its TRAN-AMT is re-ADDed to page+account totals,
    then page totals + grand totals are written; the last card's account
    total is silently dropped.
  - PIC -ZZZ,ZZZ,ZZZ.ZZ / +ZZZ,ZZZ,ZZZ.ZZ editing: full zero suppression
    (zero -> all spaces), commas inside the suppressed region become spaces.

CBSTM03A rules:
  - 80-byte statement lines, layouts ST-LINE0..15 (see the COBOL 88-levels).
  - ST-LINE1 name / ST-LINE4 addr are STRING ... DELIMITED BY ' ': each field
    contributes only up to its first space, joined by single spaces.
  - ST-CURR-BAL PIC 9(9).99- (zero-padded); ST-TRANAMT / ST-TOTAL-TRAMT
    PIC Z(9).99- (zero-suppressed, decimals always shown, trailing sign).
  - transactions per card in tran-id order (CREASTMT.JCL SORT 263,16 / 1,16),
    Total EXP = sum of the card's transaction amounts.

Run:  python3 backend/src/test/resources/golden/derive_report_statement_goldens.py
"""
from decimal import Decimal
from pathlib import Path

HERE = Path(__file__).resolve().parent
ASCII = HERE.parents[4] / 'etl' / 'fixtures' / 'ascii'

REPORT_FROM = '2020-01-01'
REPORT_TO = '2099-12-31'
STATEMENT_ACCOUNTS = (50, 27)   # first two cards in card-number order


def overpunch(s: str) -> int:
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


# ---------------------------------------------------------------------------
# fixture parsing (same copybook offsets as derive_goldens.py)
# ---------------------------------------------------------------------------

accounts = {}
for l in lines('acctdata.txt'):
    accounts[int(l[0:11])] = dict(
        curr_bal=money(l[12:24]), credit_limit=money(l[24:36]),
        expiration=l[58:68],
        cyc_credit=money(l[78:90]), cyc_debit=money(l[90:102]))

# CVACT03Y: card 0/16, cust 16/9, acct 25/11
xref_acct = {l[0:16]: int(l[25:36]) for l in lines('cardxref.txt')}
xref_cust = {l[0:16]: int(l[16:25]) for l in lines('cardxref.txt')}

# CVTRA03Y trantype: type 0/2, desc 2/50 (trimmed on load)
trantype = {l[0:2]: l[2:52].strip() for l in lines('trantype.txt')}
# CVTRA04Y trancatg: type 0/2, cat 2/4, desc 6/50
trancatg = {(l[0:2], int(l[2:6])): l[6:56].strip() for l in lines('trancatg.txt')}

# CUSTREC offsets
customers = {}
for l in lines('custdata.txt'):
    customers[int(l[0:9])] = dict(
        first=l[9:34].strip(), mid=l[34:59].strip(), last=l[59:84].strip(),
        a1=l[84:134].strip(), a2=l[134:184].strip(), a3=l[184:234].strip(),
        state=l[234:236].strip(), country=l[236:239].strip(),
        zip=l[239:249].strip(), fico=int(l[329:332]))

# CVTRA06Y dailytran
daily = []
for l in lines('dailytran.txt'):
    daily.append(dict(
        id=l[0:16], type=l[16:18], cat=int(l[18:22]),
        source=l[22:32].strip(), desc=l[32:132].strip(),
        amt=money(l[132:143]), card=l[262:278], orig_ts=l[278:304].strip()))

# ---------------------------------------------------------------------------
# POSTTRAN (CBTRN02C) re-simulation — identical rules to derive_goldens.py
# ---------------------------------------------------------------------------
posted = []
for t in daily:
    if t['card'] not in xref_acct:
        continue                                              # 100
    aid = xref_acct[t['card']]
    if aid not in accounts:
        continue                                              # 101
    acct = accounts[aid]
    reject = acct['credit_limit'] < acct['cyc_credit'] - acct['cyc_debit'] + t['amt']  # 102
    if acct['expiration'] < t['orig_ts'][0:10]:               # 103 last-wins
        reject = True
    if reject:
        continue
    acct['curr_bal'] += t['amt']                              # 2800
    if t['amt'] >= 0:
        acct['cyc_credit'] += t['amt']
    else:
        acct['cyc_debit'] += t['amt']
    posted.append(dict(t, acct=aid))                          # 2900

print(f'POSTTRAN re-simulation: posted={len(posted)}')
assert len(posted) == 262, 'expected 262 posted from the shipped fixtures'


# ---------------------------------------------------------------------------
# COBOL PIC editing (independent Python implementation)
# ---------------------------------------------------------------------------

def _suppress(s: str) -> str:
    out = list(s)
    for i, c in enumerate(out):
        if c in ('0', ','):
            out[i] = ' '
        else:
            break
    return ''.join(out)


def _signed_zzz(v: Decimal, positive_sign: str) -> str:
    """PIC -ZZZ,ZZZ,ZZZ.ZZ / +ZZZ,ZZZ,ZZZ.ZZ (width 15); zero -> all spaces."""
    u = int((abs(v) * 100).quantize(Decimal(1)))
    if v == 0:
        return ' ' * 15
    sign = '-' if v < 0 else positive_sign
    ip = f'{u // 100:09d}'
    grouped = ip[0:3] + ',' + ip[3:6] + ',' + ip[6:9]
    return sign + _suppress(grouped) + '.' + f'{u % 100:02d}'


def minus_zzz(v: Decimal) -> str:
    return _signed_zzz(v, ' ')


def plus_zzz(v: Decimal) -> str:
    return _signed_zzz(v, '+')


def z9_v99_trailing(v: Decimal) -> str:
    """PIC Z(9).99- (width 13): decimals always shown, trailing sign."""
    u = int((abs(v) * 100).quantize(Decimal(1)))
    return _suppress(f'{u // 100:09d}') + '.' + f'{u % 100:02d}' + ('-' if v < 0 else ' ')


def n9_v99_trailing(v: Decimal) -> str:
    """PIC 9(9).99- (width 13): zero-padded, trailing sign."""
    u = int((abs(v) * 100).quantize(Decimal(1)))
    d = f'{u:011d}'
    return d[0:9] + '.' + d[9:11] + ('-' if v < 0 else ' ')


def pad_x(s: str, w: int) -> str:
    return s[:w] if len(s) >= w else s + ' ' * (w - len(s))


# ---------------------------------------------------------------------------
# TRANREPT (CBTRN03C)
# ---------------------------------------------------------------------------
report_txns = sorted(posted, key=lambda t: (t['card'], t['id']))

DASH_133 = '-' * 133
BLANK_133 = ' ' * 133
HEADER_1 = (pad_x('Transaction ID', 17) + pad_x('Account ID', 12)
            + pad_x('Transaction Type', 19) + pad_x('Tran Category', 35)
            + pad_x('Tran Source', 14) + ' ' + pad_x('        Amount', 16)
            + ' ' * 19)

out = []
counter = 0
page_total = Decimal(0)
account_total = Decimal(0)
grand_total = Decimal(0)


def w(line: str):
    assert len(line) == 133, (len(line), line)
    out.append(line)


def write_headers():
    global counter
    w(pad_x('DALYREPT', 38) + pad_x('Daily Transaction Report', 41)
      + 'Date Range: ' + pad_x(REPORT_FROM, 10) + ' to ' + pad_x(REPORT_TO, 10)
      + ' ' * 18)
    w(BLANK_133)
    w(HEADER_1)
    w(DASH_133)
    counter += 4


def write_page_totals():
    global counter, page_total, grand_total
    w(pad_x('Page Total', 11) + '.' * 86 + plus_zzz(page_total) + ' ' * 21)
    grand_total += page_total
    page_total = Decimal(0)
    w(DASH_133)
    counter += 2


def write_account_totals():
    global counter, account_total
    w(pad_x('Account Total', 13) + '.' * 84 + plus_zzz(account_total) + ' ' * 21)
    account_total = Decimal(0)
    w(DASH_133)
    counter += 2


first = True
curr_card = None
last_amt = Decimal(0)
for t in report_txns:
    if t['card'] != curr_card:
        if curr_card is not None:
            write_account_totals()                            # 1120
        curr_card = t['card']
    if first:
        first = False
        write_headers()
    if counter % 20 == 0:
        write_page_totals()                                   # 1110
        write_headers()
    page_total += t['amt']
    account_total += t['amt']
    w(pad_x(t['id'], 16) + ' '
      + pad_x(f"{t['acct']:011d}", 11) + ' '
      + pad_x(t['type'], 2) + '-'
      + pad_x(trantype[t['type']], 15) + ' '
      + f"{t['cat']:04d}" + '-'
      + pad_x(trancatg[(t['type'], t['cat'])], 29) + ' '
      + pad_x(t['source'], 10)
      + ' ' * 4
      + minus_zzz(t['amt'])
      + '  '
      + ' ' * 19)                                             # X(114) -> X(133)
    counter += 1
    last_amt = t['amt']

# EOF quirk: last amount re-added, page+grand totals, NO account totals
page_total += last_amt
account_total += last_amt
write_page_totals()
w(pad_x('Grand Total', 11) + '.' * 86 + plus_zzz(grand_total) + ' ' * 21)

REPORT_DIR = HERE / 'report'
REPORT_DIR.mkdir(exist_ok=True)
with open(REPORT_DIR / 'tranrept.txt', 'w', encoding='ascii', newline='\n') as f:
    for line in out:
        f.write(line + '\n')
print(f'TRANREPT golden: {len(out)} lines, {len(report_txns)} details, '
      f'grand total {grand_total}')


# ---------------------------------------------------------------------------
# CREASTMT (CBSTM03A) — statement text for the two golden accounts
# ---------------------------------------------------------------------------

def first_token(v: str) -> str:
    """STRING ... DELIMITED BY ' ': contributes up to the first space."""
    sp = v.find(' ')
    return v if sp < 0 else v[:sp]


txns_by_card = {}
for t in sorted(posted, key=lambda t: t['id']):               # tran-id order
    txns_by_card.setdefault(t['card'], []).append(t)

STMT_DIR = HERE / 'statements'
STMT_DIR.mkdir(exist_ok=True)

for card, aid in sorted(xref_acct.items()):
    if aid not in STATEMENT_ACCOUNTS:
        continue
    c = customers[xref_cust[card]]
    a = accounts[aid]
    name = (first_token(c['first']) + ' ' + first_token(c['mid'])
            + ' ' + first_token(c['last']) + ' ')
    addr3 = (first_token(c['a3']) + ' ' + first_token(c['state'])
             + ' ' + first_token(c['country']) + ' ' + first_token(c['zip']) + ' ')
    acct20 = pad_x(f'{aid:011d}', 20)
    fico20 = pad_x(f"{c['fico']:03d}", 20)

    st = []
    st.append('*' * 31 + 'START OF STATEMENT' + '*' * 31)     # ST-LINE0
    st.append(pad_x(name, 75) + ' ' * 5)                      # ST-LINE1
    st.append(pad_x(c['a1'], 50) + ' ' * 30)                  # ST-LINE2
    st.append(pad_x(c['a2'], 50) + ' ' * 30)                  # ST-LINE3
    st.append(pad_x(addr3, 80))                               # ST-LINE4
    st.append('-' * 80)                                       # ST-LINE5
    st.append(' ' * 33 + pad_x('Basic Details', 14) + ' ' * 33)  # ST-LINE6
    st.append('-' * 80)                                       # ST-LINE5
    st.append('Account ID         :' + acct20 + ' ' * 40)     # ST-LINE7
    st.append('Current Balance    :' + n9_v99_trailing(a['curr_bal'])
              + ' ' * 7 + ' ' * 40)                           # ST-LINE8
    st.append('FICO Score         :' + fico20 + ' ' * 40)     # ST-LINE9
    st.append('-' * 80)                                       # ST-LINE10
    st.append(' ' * 30 + pad_x('TRANSACTION SUMMARY ', 20) + ' ' * 30)  # ST-LINE11
    st.append('-' * 80)                                       # ST-LINE12
    st.append(pad_x('Tran ID         ', 16) + pad_x('Tran Details    ', 51)
              + pad_x('  Tran Amount', 13))                   # ST-LINE13
    st.append('-' * 80)                                       # ST-LINE12
    total = Decimal(0)
    for t in txns_by_card.get(card, []):
        st.append(pad_x(t['id'], 16) + ' ' + pad_x(t['desc'], 49)
                  + '$' + z9_v99_trailing(t['amt']))          # ST-LINE14
        total += t['amt']
    st.append('-' * 80)                                       # ST-LINE12
    st.append('Total EXP:' + ' ' * 56 + '$' + z9_v99_trailing(total))  # ST-LINE14A
    st.append('*' * 32 + 'END OF STATEMENT' + '*' * 32)       # ST-LINE15

    with open(STMT_DIR / f'stmt-{aid}.txt', 'w', encoding='ascii', newline='\n') as f:
        for line in st:
            assert len(line) == 80, (len(line), line)
            f.write(line + '\n')
    print(f'CREASTMT golden: acct {aid} card {card} '
          f'{len(txns_by_card.get(card, []))} txns, Total EXP {total}')

print('goldens written to', REPORT_DIR, 'and', STMT_DIR)
