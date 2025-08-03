# CBTRN03C Program Documentation

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| CBTRN03C.cbl  | CardDemo        | Batch COBOL Program | Print the transaction detail report |

## Overview
The `CBTRN03C` program is a batch COBOL application designed to generate a transaction detail report. It processes transaction data, cross-references card information, and categorizes transactions based on type and category. The program reads input files, performs validations, and writes formatted reports to an output file.

## Key Features
- Reads transaction data from multiple input files.
- Validates and categorizes transactions based on type and category.
- Generates detailed transaction reports with totals and headers.
- Handles file I/O operations with error handling and status checks.
- Supports sequential and indexed file access.

---

## File Definitions

### Input Files
| **File Name**       | **Organization** | **Access Mode** | **Record Key** | **File Status** |
|---------------------|------------------|-----------------|----------------|-----------------|
| TRANSACT-FILE       | Sequential       | Sequential      | N/A            | TRANFILE-STATUS |
| XREF-FILE           | Indexed          | Random          | FD-XREF-CARD-NUM | CARDXREF-STATUS |
| TRANTYPE-FILE       | Indexed          | Random          | FD-TRAN-TYPE   | TRANTYPE-STATUS |
| TRANCATG-FILE       | Indexed          | Random          | FD-TRAN-CAT-KEY | TRANCATG-STATUS |
| DATE-PARMS-FILE     | Sequential       | Sequential      | N/A            | DATEPARM-STATUS |

### Output Files
| **File Name**       | **Organization** | **File Status** |
|---------------------|------------------|-----------------|
| REPORT-FILE         | Sequential       | TRANREPT-STATUS |

---

## Data Structures

### File Section
| **File Name**       | **Record Structure** |
|---------------------|-----------------------|
| TRANSACT-FILE       | FD-TRANS-DATA (304 bytes), FD-TRAN-PROC-TS (26 bytes), FD-FILLER (20 bytes) |
| XREF-FILE           | FD-XREF-CARD-NUM (16 bytes), FD-XREF-DATA (34 bytes) |
| TRANTYPE-FILE       | FD-TRAN-TYPE (2 bytes), FD-TRAN-DATA (58 bytes) |
| TRANCATG-FILE       | FD-TRAN-CAT-KEY (FD-TRAN-TYPE-CD: 2 bytes, FD-TRAN-CAT-CD: 4 bytes), FD-TRAN-CAT-DATA (54 bytes) |
| REPORT-FILE         | FD-REPTFILE-REC (133 bytes) |
| DATE-PARMS-FILE     | FD-DATEPARM-REC (80 bytes) |

### Working-Storage Section
- **Date Parameters**: `WS-START-DATE`, `WS-END-DATE`
- **Report Variables**: `WS-FIRST-TIME`, `WS-LINE-COUNTER`, `WS-PAGE-SIZE`, `WS-PAGE-TOTAL`, `WS-ACCOUNT-TOTAL`, `WS-GRAND-TOTAL`, `WS-CURR-CARD-NUM`
- **Status Variables**: `TRANFILE-STATUS`, `CARDXREF-STATUS`, `TRANTYPE-STATUS`, `TRANCATG-STATUS`, `TRANREPT-STATUS`, `DATEPARM-STATUS`
- **Error Handling**: `IO-STATUS`, `APPL-RESULT`, `END-OF-FILE`

---

## Program Logic

### Initialization
1. Display start message.
2. Open all required files (`TRANSACT-FILE`, `REPORT-FILE`, `XREF-FILE`, `TRANTYPE-FILE`, `TRANCATG-FILE`, `DATE-PARMS-FILE`).
3. Read date parameters from `DATE-PARMS-FILE`.

### Main Processing
1. Loop through transactions in `TRANSACT-FILE` until `END-OF-FILE`.
2. Validate transaction date range (`WS-START-DATE` to `WS-END-DATE`).
3. Perform cross-referencing and lookups:
   - Card information (`XREF-FILE`).
   - Transaction type (`TRANTYPE-FILE`).
   - Transaction category (`TRANCATG-FILE`).
4. Write transaction details to `REPORT-FILE`.
5. Calculate and write totals:
   - Page totals.
   - Account totals.
   - Grand totals.

### File Closure
1. Close all files (`TRANSACT-FILE`, `REPORT-FILE`, `XREF-FILE`, `TRANTYPE-FILE`, `TRANCATG-FILE`, `DATE-PARMS-FILE`).
2. Display end message.

---

## Error Handling
- **File Status Checks**: Each file operation checks the file status and handles errors appropriately.
- **Invalid Keys**: Displays error messages for invalid keys during lookups.
- **Abend Routine**: Calls `CEE3ABD` to terminate the program in case of critical errors.

---

## Insights
- **Modular Design**: The program is structured into modular routines for file operations, data processing, and report generation.
- **Error Resilience**: Comprehensive error handling ensures robust file operations and data integrity.
- **Performance Optimization**: Use of indexed files and random access improves lookup efficiency.
- **Scalability**: The program can be extended to handle additional transaction types or categories with minimal changes.
- **Report Formatting**: Headers, totals, and detailed transaction data are well-organized for readability.
