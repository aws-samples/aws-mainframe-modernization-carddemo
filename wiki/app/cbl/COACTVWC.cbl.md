# Documentation for `COACTVWC.cbl`

## Metadata
- **File Name**: `COACTVWC.cbl`
- **Layer**: Business Logic
- **Function**: Accept and process Account View requests

---

## Overview

The COBOL program `COACTVWC` is designed to handle account view requests in a CICS environment. It processes user inputs, validates them, retrieves account and customer data, and displays the results. The program includes error handling, screen setup, and interaction with multiple datasets.

---

## Sections

### Identification Division
- **Program ID**: `COACTVWC`
- **Date Written**: May 2022
- **Date Compiled**: Today

### Environment Division
- **Input-Output Section**: Defines the interaction with external files and datasets.

### Data Division
#### Working-Storage Section
This section contains variables used for:
1. **CICS Processing**:
   - `WS-RESP-CD` and `WS-REAS-CD`: Response codes for CICS operations.
   - `WS-TRANID`: Transaction ID.

2. **Input Flags**:
   - Flags for input validation (`INPUT-OK`, `INPUT-ERROR`, etc.).
   - Flags for account and customer filters.

3. **File Handling**:
   - `WS-XREF-RID`: Stores account and customer IDs for cross-referencing.
   - `WS-FILE-READ-FLAGS`: Flags indicating whether account or customer data was found in master files.
   - `WS-FILE-ERROR-MESSAGE`: Constructs error messages for file operations.

4. **Output Messages**:
   - `WS-LONG-MSG`, `WS-INFO-MSG`, and `WS-RETURN-MSG`: Used for constructing and displaying messages to the user.

5. **Literals and Constants**:
   - Predefined values for program names, transaction IDs, mapsets, and filenames.

#### Linkage Section
- **DFHCOMMAREA**: Used for passing data between programs in the CICS environment.

---

## Procedure Division

### Main Logic (`0000-MAIN`)
1. **Initialization**:
   - Handles abends using `EXEC CICS HANDLE ABEND`.
   - Initializes working storage and commarea variables.
   - Clears error messages.

2. **Input Handling**:
   - Processes passed data and remaps PF keys.
   - Validates user inputs and sets appropriate flags.

3. **Decision Making**:
   - Uses `EVALUATE TRUE` to determine actions based on user inputs.
   - Handles different scenarios such as exiting, re-entering, or processing inputs.

4. **Error Handling**:
   - Constructs error messages for invalid inputs or unexpected scenarios.

5. **CICS Return**:
   - Returns control to the calling program or transaction.

---

### Subroutines

#### `1000-SEND-MAP`
- Initializes screen variables and attributes.
- Sends the screen map to the user.

#### `2000-PROCESS-INPUTS`
- Receives user inputs and validates them.
- Edits individual fields and cross-field dependencies.

#### `9000-READ-ACCT`
- Reads account data from the master file.
- Retrieves associated customer data.

#### `9200-GETCARDXREF-BYACCT`
- Reads the card cross-reference file using the account ID.
- Handles errors for missing or invalid data.

#### `9300-GETACCTDATA-BYACCT`
- Reads the account master file using the account ID.
- Handles errors for missing or invalid data.

#### `9400-GETCUSTDATA-BYCUST`
- Reads the customer master file using the customer ID.
- Handles errors for missing or invalid data.

#### `SEND-PLAIN-TEXT` and `SEND-LONG-TEXT`
- Debugging routines for displaying plain or long text messages.

#### `ABEND-ROUTINE`
- Handles unexpected abends and sends error messages.

---

## Insights

### Key Features
1. **CICS Integration**:
   - The program heavily relies on CICS commands (`EXEC CICS`) for file operations, screen handling, and transaction management.

2. **Error Handling**:
   - Comprehensive error handling for file operations, input validation, and unexpected scenarios.

3. **Screen Management**:
   - Dynamically sets up screen attributes and variables based on user inputs and program context.

4. **Data Validation**:
   - Validates account and customer IDs for format, length, and numeric constraints.

5. **Modular Design**:
   - Organized into subroutines for better readability and maintainability.

### Dependencies
- **Copybooks**:
  - Includes several copybooks for common variables, screen titles, messages, and record layouts.
  - Examples: `CVCRD01Y`, `COCOM01Y`, `CVACT01Y`, `CVACT02Y`, `CVACT03Y`.

- **Datasets**:
  - Interacts with datasets such as `ACCTDAT`, `CARDDAT`, and `CUSTDAT`.

### Potential Enhancements
1. **Error Logging**:
   - Implement detailed logging for errors to aid debugging and monitoring.
   
2. **Dynamic Screen Updates**:
   - Enhance screen attributes to support dynamic updates based on user actions.

3. **Performance Optimization**:
   - Optimize file read operations to reduce response times.

4. **Security**:
   - Validate inputs against potential injection attacks or unauthorized access.

---

## Tables

### Flags and Their Values

| **Flag Name**               | **88-Level Condition**         | **Value**       |
|-----------------------------|--------------------------------|-----------------|
| `WS-INPUT-FLAG`             | `INPUT-OK`                    | `'0'`           |
|                             | `INPUT-ERROR`                 | `'1'`           |
|                             | `INPUT-PENDING`               | `LOW-VALUES`    |
| `WS-PFK-FLAG`               | `PFK-VALID`                   | `'0'`           |
|                             | `PFK-INVALID`                 | `'1'`           |
| `WS-EDIT-ACCT-FLAG`         | `FLG-ACCTFILTER-NOT-OK`       | `'0'`           |
|                             | `FLG-ACCTFILTER-ISVALID`      | `'1'`           |
|                             | `FLG-ACCTFILTER-BLANK`        | `' '`           |
| `WS-EDIT-CUST-FLAG`         | `FLG-CUSTFILTER-NOT-OK`       | `'0'`           |
|                             | `FLG-CUSTFILTER-ISVALID`      | `'1'`           |
|                             | `FLG-CUSTFILTER-BLANK`        | `' '`           |

### Literals and Constants

| **Literal Name**            | **Value**                     |
|-----------------------------|-------------------------------|
| `LIT-THISPGM`               | `'COACTVWC'`                  |
| `LIT-THISTRANID`            | `'CAVW'`                      |
| `LIT-THISMAPSET`            | `'COACTVW '`                  |
| `LIT-THISMAP`               | `'CACTVWA'`                  |
| `LIT-ACCTFILENAME`          | `'ACCTDAT '`                  |
| `LIT-CARDFILENAME`          | `'CARDDAT '`                  |
| `LIT-CUSTFILENAME`          | `'CUSTDAT '`                  |

---

## Error Messages

| **Error Scenario**                          | **Message**                                                                 |
|---------------------------------------------|-----------------------------------------------------------------------------|
| Account not found in cross-reference file   | `'Account: <ID> not found in Cross ref file. Resp: <RESP> Reas: <RESP2>'`  |
| Account not found in master file            | `'Account: <ID> not found in Acct Master file. Resp: <RESP> Reas: <RESP2>'`|
| Customer not found in master file           | `'CustId: <ID> not found in customer master. Resp: <RESP> Reas: <RESP2>'`  |
| Unexpected data scenario                    | `'UNEXPECTED DATA SCENARIO'`                                               |
| Input error                                 | `'Account Filter must be a non-zero 11 digit number'`                      |

---
