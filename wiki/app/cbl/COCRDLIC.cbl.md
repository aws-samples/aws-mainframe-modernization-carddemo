# Documentation for `COCRDLIC.cbl`

## Metadata
- **File Name**: `COCRDLIC.cbl`
- **Layer**: Business Logic
- **Function**: List Credit Cards
  - **Behavior**:
    - Lists all credit cards if no context is passed and the user is an admin.
    - Lists only credit cards associated with the account in the COMMAREA if the user is not an admin.

---

## Overview

The program `COCRDLIC.cbl` is designed to handle the listing of credit cards based on user context and permissions. It interacts with CICS (Customer Information Control System) for transaction management and screen handling. The program supports navigation through paginated credit card records and provides functionality for filtering, viewing, and updating credit card details.

---

## Sections

### 1. **Data Division**
The `Data Division` contains declarations for working storage, constants, and linkage section variables. These are used for managing program state, screen attributes, and CICS communication.

#### **Working-Storage Section**
- **General CICS Variables**:
  - `WS-RESP-CD`: Response code for CICS operations.
  - `WS-REAS-CD`: Reason code for CICS operations.
  - `WS-TRANID`: Transaction ID.

- **Input Flags**:
  - `WS-INPUT-FLAG`: Indicates input validity.
    - `INPUT-OK`: Valid input.
    - `INPUT-ERROR`: Invalid input.
  - `WS-EDIT-ACCT-FLAG`: Account filter validation.
  - `WS-EDIT-CARD-FLAG`: Card filter validation.

- **Output Variables**:
  - `CARD-ACCT-ID-X`: Account ID (alphanumeric).
  - `CARD-CVV-CD-X`: Card CVV code (alphanumeric).
  - `WS-LONG-MSG`: Long message for debugging.
  - `WS-INFO-MSG`: Informational message for user guidance.

- **File Handling Variables**:
  - `WS-CARD-RID`: Record ID for credit card file.
  - `WS-FILTER-RECORD-FLAG`: Flags for record filtering.

#### **Constants**
- Predefined literals for program, transaction, and map identifiers:
  - `LIT-THISPGM`: Program name (`COCRDLIC`).
  - `LIT-THISTRANID`: Transaction ID (`CCLI`).
  - `LIT-THISMAPSET`: Mapset name (`COCRDLI`).
  - `LIT-THISMAP`: Map name (`CCRDLIA`).

#### **Linkage Section**
- `DFHCOMMAREA`: Communication area for passing data between programs.

---

### 2. **Procedure Division**
The `Procedure Division` contains the main logic for the program, including initialization, input validation, navigation, and interaction with CICS.

#### **Main Logic**
- **Initialization**:
  - Clears error messages and initializes program context.
  - Handles first-run scenarios by setting default values.

- **Input Handling**:
  - Validates account and card filters.
  - Ensures numeric and length constraints for account and card IDs.

- **Navigation**:
  - Supports page navigation:
    - **PF3**: Exit to main menu.
    - **PF7**: Page up.
    - **PF8**: Page down.
    - **Enter**: View or update selected card details.

- **Error Handling**:
  - Displays appropriate error messages for invalid inputs or navigation issues.

#### **CICS Operations**
- **SEND MAP**:
  - Sends screen data to the user.
- **RECEIVE MAP**:
  - Receives user input from the screen.
- **STARTBR/READNEXT/READPREV**:
  - Handles browsing and reading records from the credit card file.
- **XCTL**:
  - Transfers control to other programs for detailed view or update.

#### **Pagination**
- Implements logic for paginated display of credit card records:
  - Tracks the first and last card keys for navigation.
  - Filters records based on user context and input criteria.

#### **Filtering**
- Filters records based on account and card filters provided by the user.

---

## Insights

### 1. **Program Behavior**
- The program is designed to handle both admin and non-admin users, with different levels of access to credit card data.
- It ensures robust input validation to prevent errors and unauthorized access.

### 2. **CICS Integration**
- The program heavily relies on CICS for transaction management, screen handling, and file operations.
- It uses CICS commands like `SEND MAP`, `RECEIVE MAP`, `STARTBR`, and `READNEXT` for seamless user interaction and data retrieval.

### 3. **Error Handling**
- Comprehensive error handling is implemented to guide users in case of invalid inputs or navigation issues.
- Error messages are dynamically updated based on the context.

### 4. **Modular Design**
- The program is structured into modular paragraphs for initialization, input validation, navigation, and record handling.
- This design improves readability and maintainability.

### 5. **Scalability**
- The program supports paginated display, making it scalable for large datasets.
- Filters ensure efficient data retrieval based on user context.

---

## Key Variables and Their Purpose

| **Variable Name**         | **Purpose**                                                                 |
|----------------------------|-----------------------------------------------------------------------------|
| `WS-RESP-CD`              | Stores response code for CICS operations.                                  |
| `WS-TRANID`               | Stores transaction ID for the program.                                     |
| `WS-EDIT-ACCT-FLAG`       | Validates account filter input.                                             |
| `WS-EDIT-CARD-FLAG`       | Validates card filter input.                                                |
| `CARD-ACCT-ID-X`          | Stores account ID for output.                                               |
| `CARD-CVV-CD-X`           | Stores card CVV code for output.                                            |
| `WS-LONG-MSG`             | Stores long messages for debugging purposes.                               |
| `WS-CA-FIRST-CARD-NUM`    | Tracks the first card number for pagination.                                |
| `WS-CA-LAST-CARD-NUM`     | Tracks the last card number for pagination.                                 |
| `WS-SCRN-COUNTER`         | Counts the number of records displayed on the screen.                      |

---

## Key Procedures

| **Procedure Name**         | **Purpose**                                                                 |
|-----------------------------|-----------------------------------------------------------------------------|
| `0000-MAIN`                | Main entry point for the program.                                           |
| `1000-SEND-MAP`            | Sends screen data to the user.                                              |
| `2000-RECEIVE-MAP`         | Receives user input from the screen.                                        |
| `9000-READ-FORWARD`        | Reads records in forward direction for pagination.                         |
| `9100-READ-BACKWARDS`      | Reads records in backward direction for pagination.                        |
| `9500-FILTER-RECORDS`      | Filters records based on user-provided criteria.                           |

---

## Error Messages

| **Message**                                | **Condition**                                                                 |
|--------------------------------------------|-------------------------------------------------------------------------------|
| `NO RECORDS FOUND FOR THIS SEARCH CONDITION.` | No records match the search criteria.                                         |
| `INVALID ACTION CODE`                      | User input contains an invalid action code.                                   |
| `ACCOUNT FILTER, IF SUPPLIED MUST BE A 11 DIGIT NUMBER` | Account filter input is invalid.                                              |
| `CARD ID FILTER, IF SUPPLIED MUST BE A 16 DIGIT NUMBER` | Card filter input is invalid.                                                 |
| `NO MORE RECORDS TO SHOW`                  | No additional records are available for pagination.                          |

---

## Dependencies

- **CICS Copybooks**:
  - `DFHBMSCA`: Basic mapping support.
  - `DFHAID`: Aid key definitions.
- **Application Copybooks**:
  - `COCOM01Y`: Application-specific COMMAREA definitions.
  - `CVACT02Y`: Credit card record layout.
  - `COCRDLI`: Credit card list screen layout.

---

## Notes

- The program is licensed under the Apache License, Version 2.0.
- Debugging utilities like `SEND-PLAIN-TEXT` and `SEND-LONG-TEXT` are included but should not be used in production.
