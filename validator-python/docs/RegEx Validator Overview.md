## **Original Validator: Keyword-Based Pattern Matching**

The first pass validator uses **regex patterns** to look for specific keywords and patterns:

### **1. Component Mentions (Regex Patterns)**

- **COBOL Programs**: `r'\b([A-Z]{2,8}C)\b'` - Looks for 2-8 uppercase letters ending with 'C'
  - Examples: `COACTUPC`, `COSGN00C`, `COMEN01C`
- **CICS Transactions**: `r'\b([A-Z0-9]{4})\b'` - Looks for exactly 4 uppercase letters/numbers
  - Examples: `CC00`, `CAUP`, `CACT`, `CUSR`
- **VSAM Files**: `r'\b([A-Z]+(?:DAT|SEC|XREF))\b'` - Looks for uppercase words ending with DAT, SEC, or XREF
  - Examples: `ACCTDATA`, `CARDSEC`, `TRANXREF`

### **2. Architectural Pattern Keywords**

- **CICS**: Looks for "CICS" + checks for `XCTL` or `EXEC CICS` commands
- **BMS**: Looks for "BMS" + checks for `MAP` or `SCREEN` keywords
- **VSAM**: Looks for "VSAM" + checks for `READ` or `WRITE` keywords

### **3. Data Model Consistency**

- Parses COBOL copybooks using regex: `r'(\d{2})\s+(\w+)\s+PIC\s+([^.]*)'`
- Extracts field definitions like `05 ACCTNO PIC X(16)`

### **4. Validation Logic**

- **PASS**: No errors found
- **PARTIAL**: 1-2 errors
- **FAIL**: 3+ errors

## **Limitations of Keyword Matching**

1. **Rigid patterns** - Misses variations in naming conventions
2. **No context understanding** - Can't distinguish between similar terms
3. **False positives** - May flag correct requirements as wrong
4. **Limited semantic understanding** - Can't understand intent or meaning

I have stareted a **DeepEval validator** - to provide **semantic understanding** that goes beyond simple keyword matching!

## **Specific Example: How the Original Validator Parses a Requirement**

### **The Requirement Text:**

```
"1. **Architecture Considerations**: The system must integrate IMS DB, DB2, and MQ for real-time processing of credit card authorization requests. It should ensure transactional consistency using two-phase commit transactions across IMS DB and DB2.

2. **Involved Modules/Classes**:
   - COPAUA0C: Handles authorization request processing triggered by MQ messages.
   - COPAUS0C: Displays authorization summary.
   - COPAUS1C: Displays authorization details.
   - COPAUS2C: Marks transactions as fraudulent and updates DB2.
   - CBPAUP0C: Purges expired authorizations.

3. **Relevant Interfaces or Methods**:
   - MQ queues for request and response: AWS.M2.CARDDEMO.PAUTH.REQUEST and AWS.M2.CARDDEMO.PAUTH.REPLY.
   - COBOL BMS screens for user interaction: Authorization Request Screen.
   - CICS transactions: CP00, CPVS, CPVD.

4. **Database Schema Changes**:
   - AUTHFRDS table in DB2 to store authorization and fraud-related data.
   - PA_AUTHORIZATION_DETAILS segment in IMS DB for hierarchical storage of authorization details."
```

### **How the Original Validator Would Parse This:**

#### **1. Component Mentions Check:**

```python
# Regex pattern: r'\b([A-Z]{2,8}C)\b'
mentioned_programs = re.findall(program_pattern, requirement_text)
# Finds: ['COPAUA0C', 'COPAUS0C', 'COPAUS1C', 'COPAUS2C', 'CBPAUP0C']

# For each program found:
for program in mentioned_programs:
    if program in self.known_components['programs']:
        evidence.append(f"PASS: Program {program} exists in codebase")
    else:
        errors.append(f"FAIL: Program {program} not found in codebase")
```

#### **2. Transaction Mentions Check:**

```python
# Regex pattern: r'\b([A-Z0-9]{4})\b'
mentioned_transactions = re.findall(transaction_pattern, requirement_text)
# Finds: ['CP00', 'CPVS', 'CPVD']

# For each transaction found:
for trans in mentioned_transactions:
    if trans in self.known_components['transactions']:
        evidence.append(f"PASS: Transaction {trans} exists in CSD")
    else:
        errors.append(f"FAIL: Transaction {trans} not found in CSD")
```

#### **3. Architectural Pattern Checks:**

```python
# Check for CICS patterns
if 'CICS' in requirement_text.upper():
    if 'XCTL' in requirement_text or 'EXEC CICS' in requirement_text:
        evidence.append("PASS: CICS patterns correctly identified")
    else:
        errors.append("FAIL: CICS mentioned but no CICS commands referenced")

# Check for BMS patterns
if 'BMS' in requirement_text.upper():
    if 'MAP' in requirement_text.upper() or 'SCREEN' in requirement_text.upper():
        evidence.append("PASS: BMS screen mapping correctly identified")
    else:
        errors.append("FAIL: BMS mentioned but no screen mapping details")

# Check for VSAM patterns
if 'VSAM' in requirement_text.upper():
    if 'READ' in requirement_text.upper() or 'WRITE' in requirement_text.upper():
        evidence.append("PASS: VSAM file operations correctly identified")
    else:
        errors.append("FAIL: VSAM mentioned but no file operations specified")
```

### **What the Validator Would Find:**

#### **✅ PASSES:**

- **IMS DB** mentioned (architectural pattern)
- **DB2** mentioned (architectural pattern)
- **MQ** mentioned (architectural pattern)
- **BMS** mentioned + "screen" keyword found
- **CICS** mentioned (but no specific commands found - might flag this)

#### **❌ FAILS:**

- **COPAUA0C, COPAUS0C, COPAUS1C, COPAUS2C, CBPAUP0C** - These programs don't exist in the actual CardDemo codebase
- **CP00, CPVS, CPVD** - These transactions don't exist in the actual CSD file
- **AUTHFRDS** - This table doesn't exist in the actual database schema

#### **⚠️ PARTIAL:**

- **CICS** mentioned but no `XCTL` or `EXEC CICS` commands found
- **VSAM** mentioned but no `READ` or `WRITE` operations specified

### **Final Result:**

The validator would likely mark this as **"FAIL"** because:

1. Multiple non-existent programs mentioned (5 programs)
2. Multiple non-existent transactions mentioned (3 transactions)
3. Non-existent database table mentioned
4. Missing specific CICS commands
5. Missing specific VSAM operations

**Total Errors:** ~10+ errors → **FAIL status**

There are limitations to the **keyword-based approach** as it catches factual inaccuracies but might miss the **semantic correctness** of the overall requirement. It was intended as a first pass through to see what it uncovers ... especially in missing rquirements.
