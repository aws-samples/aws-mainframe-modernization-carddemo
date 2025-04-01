# CBTRN01C Documentation

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| CBTRN01C.cbl  | CardDemo        | Batch COBOL Program | Post records from the daily transaction file |

## Overview
The `CBTRN01C` program is a batch COBOL application designed to process records from a daily transaction file (`DALYTRAN-FILE`) and perform operations such as validating card numbers, retrieving account information, and handling errors. It interacts with multiple indexed files, including customer, cross-reference, card, account, and transaction files.

## File Definitions
The program uses several files with specific attributes and purposes:

| **File Name**       | **Organization** | **Access Mode** | **Record Key**         | **Purpose** |
|----------------------|------------------|-----------------|-------------------------|-------------|
| `DALYTRAN-FILE`      | Sequential       | Sequential      | N/A                     | Stores daily transaction records. |
| `CUSTOMER-FILE`      | Indexed          | Random          | `FD-CUST-ID`            | Stores customer data. |
| `XREF-FILE`          | Indexed          | Random          | `FD-XREF-CARD-NUM`      | Maps card numbers to account and customer IDs. |
| `CARD-FILE`          | Indexed          | Random          | `FD-CARD-NUM`           | Stores card-related data. |
| `ACCOUNT-FILE`       | Indexed          | Random          | `FD-ACCT-ID`            | Stores account-related data. |
| `TRANSACT-FILE`      | Indexed          | Random          | `FD-TRANS-ID`           | Stores transaction-related data. |

## Data Structures
### File Section
The program defines the following file structures:

| **File**            | **Record Structure** |
|---------------------|-----------------------|
| `DALYTRAN-FILE`     | `FD-TRAN-RECORD` with fields `FD-TRAN-ID` (16 chars) and `FD-CUST-DATA` (334 chars). |
| `CUSTOMER-FILE`     | `FD-CUSTFILE-REC` with fields `FD-CUST-ID` (9 digits) and `FD-CUST-DATA` (491 chars). |
| `XREF-FILE`         | `FD-XREFFILE-REC` with fields `FD-XREF-CARD-NUM` (16 chars) and `FD-XREF-DATA` (34 chars). |
| `CARD-FILE`         | `FD-CARDFILE-REC` with fields `FD-CARD-NUM` (16 chars) and `FD-CARD-DATA` (134 chars). |
| `ACCOUNT-FILE`      | `FD-ACCTFILE-REC` with fields `FD-ACCT-ID` (11 digits) and `FD-ACCT-DATA` (289 chars). |
| `TRANSACT-FILE`     | `FD-TRANFILE-REC` with fields `FD-TRANS-ID` (16 chars) and `FD-ACCT-DATA` (334 chars). |

### Working-Storage Section
The program defines various working-storage variables for file statuses, binary conversions, and control flags:

| **Variable**               | **Purpose** |
|----------------------------|-------------|
| `DALYTRAN-STATUS`          | Tracks the status of `DALYTRAN-FILE`. |
| `CUSTFILE-STATUS`          | Tracks the status of `CUSTOMER-FILE`. |
| `XREFFILE-STATUS`          | Tracks the status of `XREF-FILE`. |
| `CARDFILE-STATUS`          | Tracks the status of `CARD-FILE`. |
| `ACCTFILE-STATUS`          | Tracks the status of `ACCOUNT-FILE`. |
| `TRANFILE-STATUS`          | Tracks the status of `TRANSACT-FILE`. |
| `END-OF-DAILY-TRANS-FILE`  | Indicates the end of the daily transaction file. |
| `WS-XREF-READ-STATUS`      | Tracks the status of cross-reference file reads. |
| `WS-ACCT-READ-STATUS`      | Tracks the status of account file reads. |

## Logic
### Main Flow
1. **Initialization**:
   - Opens all required files (`DALYTRAN-FILE`, `CUSTOMER-FILE`, `XREF-FILE`, `CARD-FILE`, `ACCOUNT-FILE`, `TRANSACT-FILE`).
   - Displays a start message.

2. **Processing Records**:
   - Reads records from `DALYTRAN-FILE`.
   - Validates card numbers using `XREF-FILE`.
   - Retrieves account information from `ACCOUNT-FILE`.
   - Handles errors for invalid card numbers or missing accounts.

3. **Termination**:
   - Closes all files.
   - Displays an end message.

### Key Procedures
| **Procedure**          | **Description** |
|-------------------------|-----------------|
| `0000-DALYTRAN-OPEN`   | Opens the daily transaction file. |
| `0100-CUSTFILE-OPEN`   | Opens the customer file. |
| `0200-XREFFILE-OPEN`   | Opens the cross-reference file. |
| `0300-CARDFILE-OPEN`   | Opens the card file. |
| `0400-ACCTFILE-OPEN`   | Opens the account file. |
| `0500-TRANFILE-OPEN`   | Opens the transaction file. |
| `1000-DALYTRAN-GET-NEXT` | Reads the next record from the daily transaction file. |
| `2000-LOOKUP-XREF`     | Validates card numbers using the cross-reference file. |
| `3000-READ-ACCOUNT`    | Retrieves account information from the account file. |
| `9000-DALYTRAN-CLOSE`  | Closes the daily transaction file. |
| `9100-CUSTFILE-CLOSE`  | Closes the customer file. |
| `9200-XREFFILE-CLOSE`  | Closes the cross-reference file. |
| `9300-CARDFILE-CLOSE`  | Closes the card file. |
| `9400-ACCTFILE-CLOSE`  | Closes the account file. |
| `9500-TRANFILE-CLOSE`  | Closes the transaction file. |
| `Z-DISPLAY-IO-STATUS`  | Displays file I/O status. |
| `Z-ABEND-PROGRAM`      | Abends the program in case of critical errors. |

### Error Handling
- Invalid file statuses are displayed and logged.
- Critical errors trigger program abend using `Z-ABEND-PROGRAM`.

## Insights
- **File Status Codes**: The program uses two-byte file status codes (`STAT1` and `STAT2`) to determine success or failure of file operations.
- **Sequential and Indexed Access**: The program demonstrates the use of both sequential and indexed file access modes, showcasing COBOL's versatility in handling different file types.
- **Error Recovery**: The program includes robust error handling mechanisms, ensuring that invalid records or file access issues are properly logged and managed.
- **Batch Processing**: The program is designed for batch execution, processing multiple records in a loop until the end of the file is reached.
- **Modular Design**: Procedures are modular, making the program easier to maintain and extend.
