# Documentation for COBIL00C.CBL

## Metadata
| **File Name** | **COBIL00C.cbl** |
|---------------|------------------|
| **Application** | CardDemo |
| **Type** | CICS COBOL Program |
| **Function** | Bill Payment - Pay account balance in full and process transactions for online bill payment. |

---

## Overview

The COBIL00C program is a CICS COBOL application designed to handle online bill payments. It facilitates the payment of account balances in full and manages transaction records. The program interacts with multiple datasets and screens, ensuring proper validation, error handling, and transaction processing.

---

## Sections

### **Identification Division**
- **Program ID**: `COBIL00C`
- **Author**: AWS

### **Environment Division**
- **Configuration Section**: Defines the environment setup for the program.

### **Data Division**

#### **Working-Storage Section**
This section contains variables used throughout the program for processing and control. Key variables include:

| **Variable Name** | **Description** | **Picture Clause** | **Initial Value** |
|--------------------|-----------------|---------------------|-------------------|
| `WS-PGMNAME` | Program name | `PIC X(08)` | `'COBIL00C'` |
| `WS-TRANID` | Transaction ID | `PIC X(04)` | `'CB00'` |
| `WS-MESSAGE` | Message buffer | `PIC X(80)` | Spaces |
| `WS-TRANSACT-FILE` | Transaction file name | `PIC X(08)` | `'TRANSACT'` |
| `WS-ACCTDAT-FILE` | Account data file name | `PIC X(08)` | `'ACCTDAT '` |
| `WS-CXACAIX-FILE` | Cross-reference file name | `PIC X(08)` | `'CXACAIX '` |
| `WS-ERR-FLG` | Error flag | `PIC X(01)` | `'N'` |
| `WS-TRAN-AMT` | Transaction amount | `PIC +99999999.99` | None |
| `WS-CURR-BAL` | Current balance | `PIC +9999999999.99` | None |
| `WS-TRAN-ID-NUM` | Transaction ID number | `PIC 9(16)` | Zeros |
| `WS-TRAN-DATE` | Transaction date | `PIC X(08)` | `'00/00/00'` |

#### **Linkage Section**
- **DFHCOMMAREA**: Used for passing data between programs. It is dynamically sized based on `EIBCALEN`.

---

### **Procedure Division**

#### **Main Logic**
The `MAIN-PARA` section initializes flags, processes user input, and handles screen navigation. Key operations include:
- Setting error and modification flags.
- Handling initial program entry and re-entry.
- Processing user actions based on screen input (`EIBAID`).
- Returning control to the previous screen or program.

#### **Key Subroutines**
| **Subroutine Name** | **Description** |
|----------------------|-----------------|
| `PROCESS-ENTER-KEY` | Validates user input and processes the "Enter" key action. Handles account ID validation, confirmation flags, and updates account balance. |
| `GET-CURRENT-TIMESTAMP` | Retrieves the current timestamp using CICS commands (`ASKTIME` and `FORMATTIME`). |
| `RETURN-TO-PREV-SCREEN` | Navigates back to the previous screen or program. |
| `SEND-BILLPAY-SCREEN` | Sends the bill payment screen to the user. |
| `RECEIVE-BILLPAY-SCREEN` | Receives user input from the bill payment screen. |
| `POPULATE-HEADER-INFO` | Populates header information for the screen, including current date and time. |
| `READ-ACCTDAT-FILE` | Reads account data from the `ACCTDAT` file. Handles errors such as "Account ID not found." |
| `UPDATE-ACCTDAT-FILE` | Updates account data in the `ACCTDAT` file. |
| `READ-CXACAIX-FILE` | Reads cross-reference data from the `CXACAIX` file. |
| `STARTBR-TRANSACT-FILE` | Starts browsing the transaction file. |
| `READPREV-TRANSACT-FILE` | Reads the previous record in the transaction file. |
| `ENDBR-TRANSACT-FILE` | Ends browsing the transaction file. |
| `WRITE-TRANSACT-FILE` | Writes a new transaction record to the transaction file. Handles duplicate keys and other errors. |
| `CLEAR-CURRENT-SCREEN` | Clears the current screen and reinitializes fields. |
| `INITIALIZE-ALL-FIELDS` | Resets all fields to their default values. |

---

## Insights

### **Error Handling**
The program uses flags (`WS-ERR-FLG`) and messages (`WS-MESSAGE`) to manage errors. It ensures user-friendly error reporting and prevents invalid operations.

### **CICS Integration**
The program heavily relies on CICS commands (`READ`, `WRITE`, `STARTBR`, `ENDBR`, `ASKTIME`, `FORMATTIME`) for file and screen operations. This integration allows seamless interaction with datasets and user interfaces.

### **Transaction Management**
The program ensures proper transaction handling by:
- Validating account IDs and balances.
- Generating unique transaction IDs.
- Writing transaction records to the file.
- Updating account balances post-payment.

### **Screen Navigation**
The program supports dynamic screen navigation based on user actions (`DFHENTER`, `DFHPF3`, `DFHPF4`). It ensures smooth transitions between screens and programs.

### **Data Validation**
The program validates critical inputs such as account IDs, confirmation flags, and transaction amounts. Invalid inputs trigger error messages and prevent further processing.

### **Modular Design**
The program is structured into modular subroutines, making it easier to maintain and extend functionality.

---

## External Dependencies
The program includes several copybooks for shared definitions and structures:
- `COCOM01Y`
- `COBIL00`
- `COTTL01Y`
- `CSDAT01Y`
- `CSMSG01Y`
- `CVACT01Y`
- `CVACT03Y`
- `CVTRA05Y`
- `DFHAID`
- `DFHBMSCA`

These copybooks provide definitions for screen maps, account structures, transaction records, and other shared resources.

---

## Version Information
| **Version** | **Date** | **Time** |
|-------------|----------|----------|
| CardDemo_v1.0-15-g27d6c6f-68 | 2022-07-19 | 23:12:32 CDT |
