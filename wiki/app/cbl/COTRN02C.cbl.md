# Documentation for `COTRN02C.CBL`

## Overview

The `COTRN02C.CBL` program is a **CICS COBOL application** designed to add a new transaction to the `TRANSACT` file. It is part of the **CardDemo** application and interacts with multiple datasets and screens to validate, process, and store transaction data. The program ensures data integrity through extensive validation and error handling mechanisms.

---

## Metadata

| **Attribute**       | **Value**                     |
|----------------------|-------------------------------|
| **Program Name**     | `COTRN02C`                   |
| **Application**      | `CardDemo`                   |
| **Type**             | CICS COBOL Program           |
| **Function**         | Add a new transaction to the `TRANSACT` file |
| **Author**           | AWS                          |
| **License**          | Apache License 2.0           |

---

## Key Components

### 1. **Working-Storage Section**
This section defines variables and flags used throughout the program. Key variables include:

| **Variable**         | **Description**                              | **Picture Clause**       | **Default Value** |
|-----------------------|----------------------------------------------|---------------------------|-------------------|
| `WS-PGMNAME`         | Program name                                 | `PIC X(08)`              | `'COTRN02C'`      |
| `WS-TRANID`          | Transaction ID                               | `PIC X(04)`              | `'CT02'`          |
| `WS-MESSAGE`         | Error or informational message               | `PIC X(80)`              | Spaces            |
| `WS-TRANSACT-FILE`   | Dataset name for transactions                | `PIC X(08)`              | `'TRANSACT'`      |
| `WS-ERR-FLG`         | Error flag                                   | `PIC X(01)`              | `'N'`             |
| `WS-TRAN-AMT`        | Transaction amount                           | `PIC +99999999.99`       | -                 |
| `WS-TRAN-DATE`       | Transaction date                             | `PIC X(08)`              | `'00/00/00'`      |
| `WS-ACCT-ID-N`       | Numeric account ID                           | `PIC 9(11)`              | `0`               |
| `WS-CARD-NUM-N`      | Numeric card number                          | `PIC 9(16)`              | `0`               |

#### Flags
- **Error Flags**:
  - `ERR-FLG-ON`: Indicates an error (`'Y'`).
  - `ERR-FLG-OFF`: Indicates no error (`'N'`).
- **User Modification Flags**:
  - `USR-MODIFIED-YES`: User has modified data (`'Y'`).
  - `USR-MODIFIED-NO`: User has not modified data (`'N'`).

---

### 2. **Linkage Section**
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

| **Variable**         | **Description**                              | **Picture Clause**       |
|-----------------------|----------------------------------------------|---------------------------|
| `DFHCOMMAREA`        | Communication area for inter-program data    | `PIC X(01)` (variable length) |

---

### 3. **Procedure Division**
The main logic of the program is implemented here. It is divided into multiple paragraphs for modularity and clarity.

#### Main Logic (`MAIN-PARA`)
- Initializes flags and variables.
- Handles the flow based on the presence of `DFHCOMMAREA`.
- Processes user input and navigates between screens.

#### Key Subroutines

| **Subroutine**               | **Description**                                                                 |
|-------------------------------|---------------------------------------------------------------------------------|
| `PROCESS-ENTER-KEY`          | Validates input fields and processes the transaction addition.                  |
| `VALIDATE-INPUT-KEY-FIELDS`  | Validates key fields like account ID and card number.                           |
| `VALIDATE-INPUT-DATA-FIELDS` | Validates data fields like transaction amount, dates, and merchant information. |
| `ADD-TRANSACTION`            | Adds a new transaction to the `TRANSACT` file.                                  |
| `COPY-LAST-TRAN-DATA`        | Copies data from the last transaction for reuse.                                |
| `RETURN-TO-PREV-SCREEN`      | Navigates back to the previous screen.                                          |
| `SEND-TRNADD-SCREEN`         | Sends the transaction addition screen to the user.                              |
| `RECEIVE-TRNADD-SCREEN`      | Receives user input from the transaction addition screen.                       |
| `POPULATE-HEADER-INFO`       | Populates header information for the screen.                                    |

---

## Key Functionalities

### 1. **Transaction Validation**
The program performs extensive validation on both key and data fields:
- **Key Fields**:
  - Account ID and Card Number must be numeric.
  - Either Account ID or Card Number must be provided.
- **Data Fields**:
  - Fields like `Type CD`, `Category CD`, `Source`, `Amount`, `Orig Date`, and `Proc Date` cannot be empty.
  - Dates must follow the `YYYY-MM-DD` format.
  - Amount must follow the `-99999999.99` format.

### 2. **Transaction Addition**
- Generates a new transaction ID by reading the last transaction and incrementing its ID.
- Writes the transaction to the `TRANSACT` file.
- Displays success or error messages based on the outcome.

### 3. **Error Handling**
- Uses the `WS-ERR-FLG` to track errors.
- Displays appropriate error messages for invalid inputs or system errors.
- Handles CICS responses (`RESP` and `RESP2`) for file operations.

### 4. **Screen Navigation**
- Sends and receives data from the `COTRN2A` map.
- Allows navigation between screens using function keys (e.g., `PF3`, `PF4`, `PF5`).

---

## Insights

1. **CICS Integration**:
   - The program heavily relies on CICS commands (`EXEC CICS`) for file operations, screen handling, and program control.

2. **Data Validation**:
   - The program ensures data integrity by validating all inputs before processing. This reduces the risk of invalid data being stored in the system.

3. **Reusability**:
   - Modular design with reusable subroutines like `VALIDATE-INPUT-KEY-FIELDS` and `VALIDATE-INPUT-DATA-FIELDS`.

4. **Error Messaging**:
   - User-friendly error messages guide users to correct their inputs.

5. **Scalability**:
   - The program can be extended to handle additional transaction types or datasets with minimal changes.

6. **Dependencies**:
   - The program uses several copybooks (`COPY` statements) for shared structures and constants, ensuring consistency across the application.

---

## External Dependencies

| **Copybook**         | **Purpose**                                      |
|-----------------------|--------------------------------------------------|
| `COCOM01Y`           | Common definitions for the application.          |
| `COTRN02`            | Transaction-specific definitions.                |
| `COTTL01Y`           | Title and label definitions.                     |
| `CSDAT01Y`           | Date-related utilities.                          |
| `CSMSG01Y`           | Message definitions.                             |
| `CVTRA05Y`           | Transaction-related structures.                  |
| `CVACT01Y`           | Account-related structures.                      |
| `CVACT03Y`           | Additional account-related structures.           |
| `DFHAID`             | CICS attention identifier definitions.           |
| `DFHBMSCA`           | CICS basic mapping support definitions.          |

---

## Error Handling Scenarios

| **Scenario**                          | **Error Message**                                |
|---------------------------------------|-------------------------------------------------|
| Account ID not numeric                | `Account ID must be Numeric...`                 |
| Card Number not numeric               | `Card Number must be Numeric...`                |
| Missing Account or Card Number        | `Account or Card Number must be entered...`     |
| Invalid Orig Date format              | `Orig Date should be in format YYYY-MM-DD`      |
| Invalid Proc Date format              | `Proc Date should be in format YYYY-MM-DD`      |
| Duplicate Transaction ID              | `Tran ID already exist...`                      |
| Unable to add transaction             | `Unable to Add Transaction...`                  |

---

## Screen Interaction

| **Screen**            | **Description**                              |
|------------------------|----------------------------------------------|
| `COTRN2A`             | Transaction addition screen.                 |
| `COSGN00C`            | Previous screen for navigation.              |
| `COMEN01C`            | Menu screen for navigation.                  |

---

## License

This program is licensed under the **Apache License, Version 2.0**. For more details, visit [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0).
