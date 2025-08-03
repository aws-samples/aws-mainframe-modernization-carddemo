# Documentation for COBOL Program: COCRDUPC.cbl

## Metadata
- **File Name**: COCRDUPC.cbl
- **Layer**: Business Logic
- **Function**: Accept and process credit card detail requests
- **Copyright**: Amazon.com, Inc. or its affiliates
- **License**: Apache License, Version 2.0

---

## Overview

This COBOL program is designed to handle credit card detail requests, including validation, updates, and user interaction through CICS screens. It processes user inputs, validates data, fetches associated card details, and updates records in the database. The program is structured into multiple sections, including initialization, input validation, decision-making, and data handling.

---

## Sections

### Identification Division
- **Program ID**: COCRDUPC
- **Date Written**: April 2022
- **Date Compiled**: Today

### Environment Division
- **Input-Output Section**: Defines the environment for file handling and CICS operations.

### Data Division
#### Working-Storage Section
This section contains variables for:
- **CICS Processing Variables**: Response codes, transaction IDs, and flags.
- **Input Flags**: Used for validation of user inputs (e.g., account number, card number, card name, card status, expiration date).
- **Output Variables**: Used for constructing messages and screen attributes.
- **File Error Messages**: Predefined error messages for file handling issues.
- **Screen Messages**: Predefined messages for user interaction.

#### Literals and Constants
Defines fixed values used throughout the program, such as program names, transaction IDs, and map names.

#### Application Commarea Copybooks
Includes common copybooks for shared data structures and screen layouts.

#### Linkage Section
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

---

## Procedure Division

### Main Logic (`0000-MAIN`)
- **Initialization**: Handles program startup, clears error messages, and initializes working storage.
- **Input Handling**: Processes user inputs and validates them.
- **Decision Making**: Determines the next action based on user inputs and program state.
- **Screen Interaction**: Sends screens to the user for input or confirmation.

### Input Processing (`1000-PROCESS-INPUTS`)
- **Receive Map**: Retrieves user inputs from the screen.
- **Edit Inputs**: Validates user inputs, including account number, card number, card name, card status, and expiration date.

### Decision Action (`2000-DECIDE-ACTION`)
- Evaluates program state and user inputs to decide the next steps:
  - Fetch card details.
  - Validate changes.
  - Save updates.
  - Handle errors.

### Screen Handling (`3000-SEND-MAP`)
- **Screen Initialization**: Sets up screen variables and attributes.
- **Information Messages**: Displays appropriate messages based on program state.
- **Screen Attributes**: Protects or unprotects fields based on context.
- **Send Screen**: Sends the screen to the user.

### Data Handling
#### Read Data (`9000-READ-DATA`)
- Fetches card details based on account and card number.
- Handles errors if the record is not found or cannot be read.

#### Write Processing (`9200-WRITE-PROCESSING`)
- Updates card details in the database.
- Validates record locking and checks for changes before updating.

#### Check Record Changes (`9300-CHECK-CHANGE-IN-REC`)
- Compares old and new data to detect changes before updating the record.

### Error Handling (`ABEND-ROUTINE`)
- Handles unexpected errors and sends an error message to the user.

---

## Insights

### Key Features
- **CICS Integration**: The program uses CICS commands for screen interaction, file handling, and transaction management.
- **Validation Logic**: Comprehensive validation for user inputs, including numeric checks, length checks, and format checks.
- **Error Handling**: Predefined error messages and routines for handling file errors and unexpected scenarios.
- **Screen Management**: Dynamic screen setup based on program state and user inputs.

### Data Structures
- **Flags**: Used extensively for tracking validation states and program flow.
- **Commarea**: Facilitates data sharing between programs and screens.
- **File Records**: Structures for reading and updating card details in the database.

### Program Flow
1. **Initialization**: Sets up the program context and clears previous states.
2. **Input Validation**: Ensures user inputs are valid before proceeding.
3. **Decision Making**: Determines the next action based on inputs and program state.
4. **Data Handling**: Reads and updates card details in the database.
5. **Screen Interaction**: Provides feedback and prompts to the user.

### Error Scenarios
- **File Not Found**: Handles cases where the card record is not found.
- **Record Locking Issues**: Detects and handles scenarios where the record cannot be locked for updates.
- **Data Changes**: Ensures data integrity by checking for changes before updating.

---

## Tables

### Flags and Their Values

| **Flag Name**               | **Description**                          | **Values**                  |
|-----------------------------|------------------------------------------|-----------------------------|
| `INPUT-OK`                 | Input validation status                  | `'0'` (Valid), `'1'` (Error), LOW-VALUES (Pending) |
| `FLG-ACCTFILTER`           | Account filter validation                | `'0'` (Not OK), `'1'` (Valid), `' '` (Blank) |
| `FLG-CARDFILTER`           | Card filter validation                   | `'0'` (Not OK), `'1'` (Valid), `' '` (Blank) |
| `FLG-CARDNAME`             | Card name validation                     | `'0'` (Not OK), `'1'` (Valid), `' '` (Blank) |
| `FLG-CARDSTATUS`           | Card status validation                   | `'0'` (Not OK), `'1'` (Valid), `' '` (Blank) |
| `FLG-CARDEXPMON`           | Expiry month validation                  | `'0'` (Not OK), `'1'` (Valid), `' '` (Blank) |
| `FLG-CARDEXPYEAR`          | Expiry year validation                   | `'0'` (Not OK), `'1'` (Valid), `' '` (Blank) |

### Predefined Messages

| **Message Name**            | **Description**                          | **Value**                  |
|-----------------------------|------------------------------------------|-----------------------------|
| `FOUND-CARDS-FOR-ACCOUNT`   | Card details found                      | `'Details of selected card shown above'` |
| `PROMPT-FOR-SEARCH-KEYS`    | Prompt for search criteria              | `'Please enter Account and Card Number'` |
| `PROMPT-FOR-CONFIRMATION`   | Prompt for confirmation                 | `'Changes validated. Press F5 to save'` |
| `CONFIRM-UPDATE-SUCCESS`    | Update successful                       | `'Changes committed to database'` |
| `INFORM-FAILURE`            | Update failed                           | `'Changes unsuccessful. Please try again'` |

---

## Dependencies
- **CICS Copybooks**: Includes standard CICS copybooks for screen handling and transaction management.
- **Application Copybooks**: Shared data structures for account, card, and customer records.

---
