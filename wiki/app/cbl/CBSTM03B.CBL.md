# CBSTM03B Documentation

## Metadata
| **File Name** | **CBSTM03B.CBL** |
|---------------|------------------|
| **Author**    | AWS              |
| **Application** | CardDemo       |
| **Type**      | Batch COBOL Subroutine |
| **Function**  | File processing related to Transact Report |

---

## Overview

CBSTM03B is a batch COBOL subroutine designed to handle file operations for the `CardDemo` application. It processes multiple indexed files (`TRNX-FILE`, `XREF-FILE`, `CUST-FILE`, and `ACCT-FILE`) and performs operations such as opening, reading, and closing files. The program is invoked by a higher-level statement creation program and uses a linkage section to interact with external data.

---

## File Definitions

### File Control
The program defines four indexed files with specific access modes and record keys:

| **File Name** | **Assignment** | **Organization** | **Access Mode** | **Record Key**       | **File Status**       |
|---------------|----------------|-------------------|-----------------|----------------------|-----------------------|
| `TRNX-FILE`   | `TRNXFILE`     | Indexed           | Sequential      | `FD-TRNXS-ID`        | `TRNXFILE-STATUS`     |
| `XREF-FILE`   | `XREFFILE`     | Indexed           | Sequential      | `FD-XREF-CARD-NUM`   | `XREFFILE-STATUS`     |
| `CUST-FILE`   | `CUSTFILE`     | Indexed           | Random          | `FD-CUST-ID`         | `CUSTFILE-STATUS`     |
| `ACCT-FILE`   | `ACCTFILE`     | Indexed           | Random          | `FD-ACCT-ID`         | `ACCTFILE-STATUS`     |

---

### File Section
Each file has a defined structure for its records:

| **File Name** | **Record Structure**                                                                 |
|---------------|--------------------------------------------------------------------------------------|
| `TRNX-FILE`   | `FD-TRNXFILE-REC` contains `FD-TRNXS-ID` (Card and ID fields) and `FD-ACCT-DATA`.    |
| `XREF-FILE`   | `FD-XREFFILE-REC` contains `FD-XREF-CARD-NUM` and `FD-XREF-DATA`.                   |
| `CUST-FILE`   | `FD-CUSTFILE-REC` contains `FD-CUST-ID` and `FD-CUST-DATA`.                         |
| `ACCT-FILE`   | `FD-ACCTFILE-REC` contains `FD-ACCT-ID` and `FD-ACCT-DATA`.                         |

---

## Working-Storage Section

The program defines file status fields for each file to capture the status of file operations:

| **File Name** | **Status Fields**         |
|---------------|---------------------------|
| `TRNX-FILE`   | `TRNXFILE-STAT1`, `TRNXFILE-STAT2` |
| `XREF-FILE`   | `XREFFILE-STAT1`, `XREFFILE-STAT2` |
| `CUST-FILE`   | `CUSTFILE-STAT1`, `CUSTFILE-STAT2` |
| `ACCT-FILE`   | `ACCTFILE-STAT1`, `ACCTFILE-STAT2` |

---

## Linkage Section

The linkage section defines the interface for external interaction:

| **Field Name**       | **Description**                                                                 |
|-----------------------|---------------------------------------------------------------------------------|
| `LK-M03B-DD`          | Specifies the file to be processed (`TRNXFILE`, `XREFFILE`, `CUSTFILE`, `ACCTFILE`). |
| `LK-M03B-OPER`        | Specifies the operation (`OPEN`, `CLOSE`, `READ`, `READ-K`, `WRITE`, `REWRITE`). |
| `LK-M03B-RC`          | Stores the return code of the operation.                                       |
| `LK-M03B-KEY`         | Key used for indexed file operations.                                          |
| `LK-M03B-KEY-LN`      | Length of the key.                                                             |
| `LK-M03B-FLDT`        | Buffer for file data.                                                          |

---

## Procedure Division

### Main Logic
The program uses the `EVALUATE` statement to determine the file to process based on `LK-M03B-DD`. It then performs the corresponding file operations by invoking specific procedures.

### File Processing Procedures
Each file has its own processing procedure:

| **Procedure Name**       | **Operations**                                                                 |
|---------------------------|-------------------------------------------------------------------------------|
| `1000-TRNXFILE-PROC`      | Handles `TRNX-FILE` operations: `OPEN`, `READ`, `CLOSE`.                     |
| `2000-XREFFILE-PROC`      | Handles `XREF-FILE` operations: `OPEN`, `READ`, `CLOSE`.                     |
| `3000-CUSTFILE-PROC`      | Handles `CUST-FILE` operations: `OPEN`, `READ-K` (key-based read), `CLOSE`.  |
| `4000-ACCTFILE-PROC`      | Handles `ACCT-FILE` operations: `OPEN`, `READ-K` (key-based read), `CLOSE`.  |

### Exit Points
Each procedure has an exit point to move the file status to the return code (`LK-M03B-RC`) and terminate the procedure.

---

## Insights

1. **Modular Design**: The program is structured into modular procedures for each file, making it easy to maintain and extend.
2. **File Status Handling**: File status codes are consistently captured and returned, ensuring robust error handling.
3. **Key-Based Reads**: The program supports key-based reads for `CUST-FILE` and `ACCT-FILE`, enabling efficient access to specific records.
4. **Linkage Section**: The linkage section provides a flexible interface for external programs to interact with this subroutine.
5. **Batch Processing**: Designed for batch operations, the program is optimized for sequential and random file access.

---
