# CBTRN02C COBOL Program Documentation

## Metadata
- **File Name**: CBTRN02C.cbl
- **Application**: CardDemo
- **Type**: Batch COBOL Program
- **Function**: Processes records from a daily transaction file, validates them, posts valid transactions, and writes rejected transactions to a separate file.

---

## Overview

The `CBTRN02C` program is a batch COBOL application designed to process daily transaction records. It reads input files, validates transaction data, updates account and transaction balances, and handles rejected transactions. The program ensures data integrity by performing validations and error handling during file operations.

---

## File Definitions

### Input Files
| **File Name**       | **Organization** | **Access Mode** | **Record Key**           | **File Status**       |
|----------------------|------------------|------------------|--------------------------|------------------------|
| `DALYTRAN-FILE`      | Sequential       | Sequential       | N/A                      | `DALYTRAN-STATUS`      |
| `XREF-FILE`          | Indexed          | Random           | `FD-XREF-CARD-NUM`       | `XREFFILE-STATUS`      |

### Output Files
| **File Name**       | **Organization** | **Access Mode** | **Record Key**           | **File Status**       |
|----------------------|------------------|------------------|--------------------------|------------------------|
| `TRANSACT-FILE`      | Indexed          | Random           | `FD-TRANS-ID`            | `TRANFILE-STATUS`      |
| `DALYREJS-FILE`      | Sequential       | Sequential       | N/A                      | `DALYREJS-STATUS`      |
| `ACCOUNT-FILE`       | Indexed          | Random           | `FD-ACCT-ID`             | `ACCTFILE-STATUS`      |
| `TCATBAL-FILE`       | Indexed          | Random           | `FD-TRAN-CAT-KEY`        | `TCATBALF-STATUS`      |

---

## Data Structures

### File Records
| **File Name**       | **Record Structure**                                                                 |
|----------------------|--------------------------------------------------------------------------------------|
| `DALYTRAN-FILE`      | `FD-TRAN-RECORD` contains `FD-TRAN-ID` (16 bytes) and `FD-CUST-DATA` (334 bytes).    |
| `TRANSACT-FILE`      | `FD-TRANFILE-REC` contains `FD-TRANS-ID` (16 bytes) and `FD-ACCT-DATA` (334 bytes).  |
| `XREF-FILE`          | `FD-XREFFILE-REC` contains `FD-XREF-CARD-NUM` (16 bytes) and `FD-XREF-DATA` (34 bytes). |
| `DALYREJS-FILE`      | `FD-REJS-RECORD` contains `FD-REJECT-RECORD` (350 bytes) and `FD-VALIDATION-TRAILER` (80 bytes). |
| `ACCOUNT-FILE`       | `FD-ACCTFILE-REC` contains `FD-ACCT-ID` (11 digits) and `FD-ACCT-DATA` (289 bytes).  |
| `TCATBAL-FILE`       | `FD-TRAN-CAT-BAL-RECORD` contains `FD-TRAN-CAT-KEY` (structured fields) and `FD-TRAN-CAT-DATA` (33 bytes). |

### Working Storage
- **Status Variables**: File status codes for all files.
- **Counters**: `WS-TRANSACTION-COUNT`, `WS-REJECT-COUNT`.
- **Flags**: `WS-CREATE-TRANCAT-REC`.
- **Validation Trailer**: `WS-VALIDATION-FAIL-REASON` and `WS-VALIDATION-FAIL-REASON-DESC`.
- **Timestamp Structures**: `COBOL-TS` and `DB2-FORMAT-TS`.

---

## Program Logic

### Execution Flow
1. **Initialization**:
   - Open all required files (`DALYTRAN-FILE`, `TRANSACT-FILE`, `XREF-FILE`, `DALYREJS-FILE`, `ACCOUNT-FILE`, `TCATBAL-FILE`).
   - Handle file open errors.

2. **Processing Transactions**:
   - Read records from `DALYTRAN-FILE`.
   - Validate transactions:
     - Lookup card number in `XREF-FILE`.
     - Lookup account in `ACCOUNT-FILE`.
     - Perform additional validations (e.g., credit limit, expiration date).
   - Post valid transactions:
     - Update balances in `TCATBAL-FILE`.
     - Update account record in `ACCOUNT-FILE`.
     - Write transaction to `TRANSACT-FILE`.
   - Write rejected transactions to `DALYREJS-FILE`.

3. **Finalization**:
   - Close all files.
   - Display transaction counts (processed and rejected).
   - Set return code based on rejection count.

4. **Error Handling**:
   - Display error messages for file operations.
   - Perform program abend if critical errors occur.

---

## Key Procedures

### File Operations
| **Procedure Name**       | **Description**                                   |
|---------------------------|--------------------------------------------------|
| `0000-DALYTRAN-OPEN`      | Opens `DALYTRAN-FILE` for input.                 |
| `0100-TRANFILE-OPEN`      | Opens `TRANSACT-FILE` for output.                |
| `0200-XREFFILE-OPEN`      | Opens `XREF-FILE` for input.                     |
| `0300-DALYREJS-OPEN`      | Opens `DALYREJS-FILE` for output.                |
| `0400-ACCTFILE-OPEN`      | Opens `ACCOUNT-FILE` for I-O.                    |
| `0500-TCATBALF-OPEN`      | Opens `TCATBAL-FILE` for I-O.                    |
| `9000-DALYTRAN-CLOSE`     | Closes `DALYTRAN-FILE`.                          |
| `9100-TRANFILE-CLOSE`     | Closes `TRANSACT-FILE`.                          |
| `9200-XREFFILE-CLOSE`     | Closes `XREF-FILE`.                              |
| `9300-DALYREJS-CLOSE`     | Closes `DALYREJS-FILE`.                          |
| `9400-ACCTFILE-CLOSE`     | Closes `ACCOUNT-FILE`.                           |
| `9500-TCATBALF-CLOSE`     | Closes `TCATBAL-FILE`.                           |

### Transaction Processing
| **Procedure Name**       | **Description**                                   |
|---------------------------|--------------------------------------------------|
| `1000-DALYTRAN-GET-NEXT`  | Reads the next record from `DALYTRAN-FILE`.       |
| `1500-VALIDATE-TRAN`      | Validates transaction data.                      |
| `2000-POST-TRANSACTION`   | Posts valid transactions.                        |
| `2500-WRITE-REJECT-REC`   | Writes rejected transactions to `DALYREJS-FILE`. |
| `2700-UPDATE-TCATBAL`     | Updates transaction category balances.           |
| `2800-UPDATE-ACCOUNT-REC` | Updates account balances.                        |
| `2900-WRITE-TRANSACTION-FILE` | Writes transaction to `TRANSACT-FILE`.       |

---

## Insights

1. **Validation Logic**:
   - The program performs multiple validations, including card number lookup, account lookup, credit limit checks, and expiration date checks. This ensures data integrity and prevents invalid transactions from being posted.

2. **Error Handling**:
   - Comprehensive error handling is implemented for file operations. Errors are displayed, and the program abends if critical issues occur.

3. **File Status Codes**:
   - File status codes are used extensively to determine the success or failure of file operations. This is critical for ensuring proper program flow.

4. **Transaction Categorization**:
   - The program updates transaction category balances (`TCATBAL-FILE`) dynamically, creating new records if necessary.

5. **Batch Processing**:
   - The program is designed for batch processing, handling multiple transactions in a single execution cycle.

6. **Scalability**:
   - The modular design of procedures allows for easy addition of new validations or file operations.

7. **Timestamp Handling**:
   - The program converts COBOL timestamps to DB2 format for database compatibility.

8. **Return Code**:
   - The return code is set based on the number of rejected transactions, providing a summary of processing results.

---
