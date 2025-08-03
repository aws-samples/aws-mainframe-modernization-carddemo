# Documentation for `COACTUPC.cbl`

## Overview

The COBOL program `COACTUPC.cbl` is designed to handle account updates in a business logic layer. It interacts with CICS (Customer Information Control System) to process and validate user inputs, fetch account and customer data, and update records in the respective files. The program includes extensive validation logic, error handling, and user interface management.

---

## Metadata

| **Attribute**       | **Value**                     |
|----------------------|-------------------------------|
| **File Name**        | `COACTUPC.cbl`               |
| **Layer**            | Business Logic               |
| **Function**         | Accept and process account updates |
| **Copyright**        | Amazon.com, Inc. or its affiliates |
| **License**          | Apache License, Version 2.0  |
| **Date Written**     | July 2022                    |
| **Date Compiled**    | Today                        |

---

## Program Structure

### Divisions

1. **Identification Division**  
   Defines the program name and metadata.

2. **Environment Division**  
   Specifies the input-output section for CICS interaction.

3. **Data Division**  
   Contains working storage, linkage section, and data structures for account and customer entities.

4. **Procedure Division**  
   Implements the program logic, including initialization, input validation, data fetching, record updates, and error handling.

---

## Key Features

### Data Structures

#### Account Update Record
| **Field Name**               | **Type**         | **Description**                     |
|-------------------------------|------------------|-------------------------------------|
| `ACCT-UPDATE-ID`             | `PIC 9(11)`      | Account ID                          |
| `ACCT-UPDATE-ACTIVE-STATUS`  | `PIC X(01)`      | Active status (Y/N)                 |
| `ACCT-UPDATE-CURR-BAL`       | `PIC S9(10)V99`  | Current balance                     |
| `ACCT-UPDATE-CREDIT-LIMIT`   | `PIC S9(10)V99`  | Credit limit                        |
| `ACCT-UPDATE-CASH-CREDIT-LIMIT` | `PIC S9(10)V99` | Cash credit limit                   |
| `ACCT-UPDATE-OPEN-DATE`      | `PIC X(10)`      | Open date                           |
| `ACCT-UPDATE-EXPIRAION-DATE` | `PIC X(10)`      | Expiration date                     |
| `ACCT-UPDATE-REISSUE-DATE`   | `PIC X(10)`      | Reissue date                        |
| `ACCT-UPDATE-CURR-CYC-CREDIT` | `PIC S9(10)V99` | Current cycle credit                |
| `ACCT-UPDATE-CURR-CYC-DEBIT` | `PIC S9(10)V99`  | Current cycle debit                 |
| `ACCT-UPDATE-GROUP-ID`       | `PIC X(10)`      | Group ID                            |

#### Customer Update Record
| **Field Name**               | **Type**         | **Description**                     |
|-------------------------------|------------------|-------------------------------------|
| `CUST-UPDATE-ID`             | `PIC 9(09)`      | Customer ID                         |
| `CUST-UPDATE-FIRST-NAME`     | `PIC X(25)`      | First name                          |
| `CUST-UPDATE-MIDDLE-NAME`    | `PIC X(25)`      | Middle name                         |
| `CUST-UPDATE-LAST-NAME`      | `PIC X(25)`      | Last name                           |
| `CUST-UPDATE-ADDR-LINE-1`    | `PIC X(50)`      | Address line 1                      |
| `CUST-UPDATE-ADDR-LINE-2`    | `PIC X(50)`      | Address line 2                      |
| `CUST-UPDATE-ADDR-LINE-3`    | `PIC X(50)`      | Address line 3                      |
| `CUST-UPDATE-ADDR-STATE-CD`  | `PIC X(02)`      | State code                          |
| `CUST-UPDATE-ADDR-COUNTRY-CD` | `PIC X(03)`     | Country code                        |
| `CUST-UPDATE-ADDR-ZIP`       | `PIC X(10)`      | ZIP code                            |
| `CUST-UPDATE-PHONE-NUM-1`    | `PIC X(15)`      | Phone number 1                      |
| `CUST-UPDATE-PHONE-NUM-2`    | `PIC X(15)`      | Phone number 2                      |
| `CUST-UPDATE-SSN`            | `PIC 9(09)`      | Social Security Number              |
| `CUST-UPDATE-GOVT-ISSUED-ID` | `PIC X(20)`      | Government-issued ID                |
| `CUST-UPDATE-DOB-YYYY-MM-DD` | `PIC X(10)`      | Date of birth                       |
| `CUST-UPDATE-EFT-ACCOUNT-ID` | `PIC X(10)`      | EFT account ID                      |
| `CUST-UPDATE-PRI-CARD-IND`   | `PIC X(01)`      | Primary cardholder indicator        |
| `CUST-UPDATE-FICO-CREDIT-SCORE` | `PIC 9(03)`   | FICO credit score                   |

---

### Logic

#### Initialization
- Clears working storage and commarea.
- Sets up transaction and program context.

#### Input Validation
- Validates user inputs for account and customer fields.
- Includes checks for numeric values, mandatory fields, and format-specific validations (e.g., SSN, phone numbers).

#### Data Fetching
- Reads account and customer data from respective files using CICS commands.
- Handles errors such as record not found or file access issues.

#### Record Updates
- Updates account and customer records in their respective files.
- Ensures data integrity by checking for changes before updates.
- Handles locking and rollback scenarios.

#### User Interface Management
- Sends and receives maps for user interaction.
- Sets up screen attributes, including field protection and cursor positioning.
- Displays error and informational messages.

---

## Insights

### Strengths
1. **Comprehensive Validation**: The program includes detailed validation logic for various input types, ensuring data integrity.
2. **Error Handling**: Robust error handling mechanisms are in place for file operations and user inputs.
3. **CICS Integration**: Efficient use of CICS commands for file access and transaction management.
4. **Modular Design**: The program is divided into logical sections, making it easier to maintain and extend.

### Potential Improvements
1. **Code Reusability**: Some validation routines could be refactored into reusable modules to reduce redundancy.
2. **Internationalization**: The program assumes U.S.-specific formats (e.g., phone numbers, ZIP codes). Enhancements could be made for global compatibility.
3. **Performance Optimization**: The program could benefit from caching frequently accessed data to reduce file I/O operations.

### Dependencies
- **Copybooks**: The program relies on several copybooks for common routines, screen layouts, and dataset definitions.
- **CICS**: The program is tightly coupled with CICS for transaction and file management.

---

## Error Messages

| **Message Code**              | **Description**                                      |
|--------------------------------|----------------------------------------------------|
| `WS-PROMPT-FOR-ACCT`          | Account number not provided                        |
| `WS-NAME-MUST-BE-ALPHA`       | Name can only contain alphabets and spaces         |
| `SEARCHED-ACCT-NOT-NUMERIC`   | Account number must be a non-zero 11-digit number  |
| `DID-NOT-FIND-ACCT-IN-CARDXREF` | Account not found in cross-reference file         |
| `COULD-NOT-LOCK-ACCT-FOR-UPDATE` | Could not lock account record for update         |
| `DATA-WAS-CHANGED-BEFORE-UPDATE` | Record changed by someone else                   |

---

## Key Procedures

| **Procedure Name**            | **Description**                                      |
|--------------------------------|----------------------------------------------------|
| `0000-MAIN`                   | Main entry point for the program                   |
| `1000-PROCESS-INPUTS`         | Processes user inputs                              |
| `2000-DECIDE-ACTION`          | Determines the next action based on user inputs    |
| `3000-SEND-MAP`               | Sends the user interface map                       |
| `9000-READ-ACCT`              | Reads account data                                 |
| `9600-WRITE-PROCESSING`       | Handles record updates                             |
| `9700-CHECK-CHANGE-IN-REC`    | Checks for changes in records before updates       |

---

## External Files

| **File Name**                 | **Purpose**                                         |
|--------------------------------|----------------------------------------------------|
| `ACCTDAT`                     | Account master file                                |
| `CUSTDAT`                     | Customer master file                               |
| `CARDDAT`                     | Card data file                                     |
| `CARDAIX`                     | Card alternate index file                          |
| `CXACAIX`                     | Cross-reference file for account and card          |

---

## Transaction Flow

1. **User Input**: User provides account and customer details via the interface.
2. **Validation**: Inputs are validated for correctness and completeness.
3. **Data Fetching**: Account and customer data are fetched from respective files.
4. **Record Updates**: Validated data is written back to the files.
5. **Confirmation**: Success or failure messages are displayed to the user.

---
