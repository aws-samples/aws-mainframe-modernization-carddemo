# Documentation for COBOL Program: CBACT04C

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| CBACT04C.cbl  | CardDemo        | Batch COBOL Program | Interest Calculator Program |

## Overview
This COBOL program calculates interest for accounts based on transaction categories and updates account balances accordingly. It processes multiple indexed and sequential files, performs calculations, and writes transaction records.

---

## Sections

### Identification Division
- **Program ID**: `CBACT04C`
- **Author**: AWS

### Environment Division
Defines the file control and input-output operations for the program.

#### File Control
| **File Name**       | **Assignment** | **Organization** | **Access Mode** | **Record Key**               | **File Status**         |
|----------------------|----------------|-------------------|-----------------|------------------------------|-------------------------|
| TCATBAL-FILE         | TCATBALF       | Indexed           | Sequential      | FD-TRAN-CAT-KEY              | TCATBALF-STATUS         |
| XREF-FILE            | XREFFILE       | Indexed           | Random          | FD-XREF-CARD-NUM, FD-XREF-ACCT-ID | XREFFILE-STATUS         |
| ACCOUNT-FILE         | ACCTFILE       | Indexed           | Random          | FD-ACCT-ID                   | ACCTFILE-STATUS         |
| DISCGRP-FILE         | DISCGRP        | Indexed           | Random          | FD-DISCGRP-KEY               | DISCGRP-STATUS          |
| TRANSACT-FILE        | TRANSACT       | Sequential        | Sequential      | N/A                          | TRANFILE-STATUS         |

---

### Data Division

#### File Section
Defines the structure of records for each file.

| **File Name**       | **Record Structure**                                                                 |
|----------------------|--------------------------------------------------------------------------------------|
| TCATBAL-FILE         | Contains transaction category balance records with keys and data fields.            |
| XREF-FILE            | Contains cross-reference records for card numbers, customer numbers, and account IDs. |
| DISCGRP-FILE         | Contains disclosure group records with keys and data fields.                        |
| ACCOUNT-FILE         | Contains account records with account IDs and associated data.                      |
| TRANSACT-FILE        | Contains transaction records with transaction IDs and account data.                 |

#### Working-Storage Section
Defines variables used for processing, including:
- **File Status Variables**: Status codes for file operations.
- **Timestamp Variables**: COBOL and DB2 timestamp formats.
- **Miscellaneous Variables**: Counters, flags, and intermediate calculation results.

#### Linkage Section
Defines external parameters passed to the program:
- **PARM-LENGTH**: Length of the parameter.
- **PARM-DATE**: Date parameter.

---

### Procedure Division

#### Main Logic
1. **Initialization**:
   - Opens all required files.
   - Displays the start of execution.

2. **Processing Loop**:
   - Reads transaction category balance records sequentially.
   - Updates account balances and calculates interest for each account.
   - Writes transaction records for interest calculations.

3. **Finalization**:
   - Closes all files.
   - Displays the end of execution.

#### Key Subroutines
| **Subroutine**            | **Description**                                                                 |
|----------------------------|---------------------------------------------------------------------------------|
| `0000-TCATBALF-OPEN`       | Opens the transaction category balance file.                                    |
| `0100-XREFFILE-OPEN`       | Opens the cross-reference file.                                                |
| `0200-DISCGRP-OPEN`        | Opens the disclosure group file.                                               |
| `0300-ACCTFILE-OPEN`       | Opens the account file.                                                        |
| `0400-TRANFILE-OPEN`       | Opens the transaction file.                                                    |
| `1000-TCATBALF-GET-NEXT`   | Reads the next record from the transaction category balance file.               |
| `1050-UPDATE-ACCOUNT`      | Updates account balances based on processed transactions.                      |
| `1100-GET-ACCT-DATA`       | Retrieves account data from the account file.                                  |
| `1110-GET-XREF-DATA`       | Retrieves cross-reference data for an account.                                 |
| `1200-GET-INTEREST-RATE`   | Retrieves interest rate from the disclosure group file.                        |
| `1300-COMPUTE-INTEREST`    | Calculates monthly interest for an account.                                    |
| `1300-B-WRITE-TX`          | Writes transaction records for interest calculations.                          |
| `1400-COMPUTE-FEES`        | Placeholder for fee computation logic.                                         |
| `9000-TCATBALF-CLOSE`      | Closes the transaction category balance file.                                  |
| `9100-XREFFILE-CLOSE`      | Closes the cross-reference file.                                               |
| `9200-DISCGRP-CLOSE`       | Closes the disclosure group file.                                              |
| `9300-ACCTFILE-CLOSE`      | Closes the account file.                                                       |
| `9400-TRANFILE-CLOSE`      | Closes the transaction file.                                                   |
| `Z-GET-DB2-FORMAT-TIMESTAMP` | Converts COBOL timestamp to DB2 format.                                       |
| `9999-ABEND-PROGRAM`       | Abends the program in case of critical errors.                                 |
| `9910-DISPLAY-IO-STATUS`   | Displays file I/O status for debugging purposes.                               |

---

## Insights

### File Operations
- The program uses indexed files for random access and sequential files for sequential processing.
- File status codes are checked after every operation to ensure data integrity.

### Interest Calculation
- Interest is calculated using the formula: `(TRAN-CAT-BAL * DIS-INT-RATE) / 1200`.
- Monthly interest is accumulated and written as transaction records.

### Error Handling
- Comprehensive error handling is implemented for file operations.
- Critical errors result in program abend with detailed status messages.

### Timestamp Management
- Timestamp conversion ensures compatibility with DB2 format for transaction records.

### Modular Design
- The program is structured into modular subroutines for better maintainability and readability.

### Placeholder Logic
- Fee computation logic (`1400-COMPUTE-FEES`) is yet to be implemented.

---

## Notes
- The program adheres to the Apache License, Version 2.0.
- Version information: `CardDemo_v1.0-15-g27d6c6f-68`, Date: `2022-07-19 23:12:31 CDT`.
