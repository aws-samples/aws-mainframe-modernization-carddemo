# Documentation for `COCRDSLC.cbl`

## Metadata
- **File Name**: `COCRDSLC.cbl`
- **Layer**: Business Logic
- **Function**: Accept and process credit card detail requests
- **Copyright**: Amazon.com, Inc. or its affiliates
- **License**: Apache License, Version 2.0

---

## Overview

The COBOL program `COCRDSLC.cbl` is designed to handle credit card detail requests. It includes functionality for input validation, data retrieval, and user interface management. The program interacts with CICS (Customer Information Control System) for transaction handling and screen management.

---

## Sections

### Identification Division
- **Program ID**: `COCRDSLC`
- **Date Written**: April 2022
- **Date Compiled**: Today

### Environment Division
- **Input-Output Section**: Specifies the environment for file handling and CICS operations.

### Data Division
#### Working-Storage Section
This section contains variables used for processing, input validation, output formatting, and error handling.

##### General CICS Related Variables
| Variable Name       | Data Type       | Initial Value | Description                          |
|---------------------|-----------------|---------------|--------------------------------------|
| `WS-RESP-CD`        | `PIC S9(09) COMP` | ZEROS        | Response code for CICS operations.  |
| `WS-REAS-CD`        | `PIC S9(09) COMP` | ZEROS        | Reason code for CICS operations.    |
| `WS-TRANID`         | `PIC X(4)`       | SPACES       | Transaction ID.                     |

##### Input Flags
| Flag Name                  | Data Type | Description                                   |
|----------------------------|-----------|-----------------------------------------------|
| `WS-INPUT-FLAG`            | `PIC X(1)` | Indicates the status of input validation.     |
| `WS-EDIT-ACCT-FLAG`        | `PIC X(1)` | Flags for account number validation.          |
| `WS-EDIT-CARD-FLAG`        | `PIC X(1)` | Flags for card number validation.             |
| `WS-RETURN-FLAG`           | `PIC X(1)` | Indicates whether to return a message.        |
| `WS-PFK-FLAG`              | `PIC X(1)` | Flags for PF key validation.                  |

##### Output Variables
| Variable Name              | Data Type | Description                                   |
|----------------------------|-----------|-----------------------------------------------|
| `CARD-ACCT-ID-X`           | `PIC X(11)` | Account ID in alphanumeric format.            |
| `CARD-CVV-CD-X`            | `PIC X(03)` | CVV code in alphanumeric format.              |
| `CARD-CARD-NUM-X`          | `PIC X(16)` | Card number in alphanumeric format.           |
| `CARD-NAME-EMBOSSED-X`     | `PIC X(50)` | Embossed name on the card.                    |
| `CARD-STATUS-X`            | `PIC X`     | Card status.                                  |
| `CARD-EXPIRAION-DATE-X`    | `PIC X(10)` | Card expiration date in alphanumeric format.  |

##### File and Data Handling
| Variable Name              | Data Type | Description                                   |
|----------------------------|-----------|-----------------------------------------------|
| `WS-CARD-RID`              | Group     | Contains card record identifiers.             |
| `WS-FILE-ERROR-MESSAGE`    | Group     | Constructs error messages for file operations.|

##### Literals and Constants
| Literal Name               | Data Type | Value       | Description                               |
|----------------------------|-----------|-------------|-------------------------------------------|
| `LIT-THISPGM`              | `PIC X(8)` | `COCRDSLC` | Program name.                             |
| `LIT-THISTRANID`           | `PIC X(4)` | `CCDL`     | Transaction ID.                           |
| `LIT-CARDFILENAME`         | `PIC X(8)` | `CARDDAT ` | Card data file name.                      |

#### Linkage Section
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

---

## Procedure Division

### Main Logic (`0000-MAIN`)
1. **Initialization**:
   - Handles CICS abends.
   - Initializes working storage and commarea variables.
   - Clears error messages.

2. **Input Handling**:
   - Validates input data and stores context.
   - Maps PF keys and checks AID (Attention Identifier).

3. **Decision Making**:
   - Uses `EVALUATE TRUE` to determine actions based on input and context.
   - Handles transitions between screens and programs.

4. **Error Handling**:
   - Displays error messages and returns to the caller.

### Subroutines
#### `1000-SEND-MAP`
- Initializes screen variables and attributes.
- Sends the screen map to the user.

#### `2000-PROCESS-INPUTS`
- Receives user input from the screen.
- Validates account and card numbers.
- Handles cross-field edits.

#### `9000-READ-DATA`
- Reads card data from files using primary and alternate keys.

#### `SEND-LONG-TEXT` and `SEND-PLAIN-TEXT`
- Debugging routines for displaying messages.

#### `ABEND-ROUTINE`
- Handles unexpected program termination.

---

## Insights

### Key Features
- **CICS Integration**: The program leverages CICS for transaction management, screen handling, and file operations.
- **Input Validation**: Comprehensive validation for account and card numbers, including numeric checks and length constraints.
- **Dynamic Screen Management**: Adjusts screen attributes, colors, and cursor positions based on user input and context.
- **Error Handling**: Constructs detailed error messages for debugging and user feedback.

### Potential Enhancements
- **Modularization**: Some sections could be refactored into smaller, reusable modules for better maintainability.
- **Logging**: Implement detailed logging for debugging and monitoring purposes.
- **Performance Optimization**: Optimize file read operations, especially for large datasets.

### Dependencies
- **Copybooks**: The program relies on several copybooks for common variables, screen layouts, and dataset structures.
- **CICS Commands**: Extensive use of CICS commands for transaction handling and screen management.

### Known Limitations
- Debugging routines (`SEND-LONG-TEXT` and `SEND-PLAIN-TEXT`) are not suitable for production use.
- Error handling could be expanded to include more granular responses for specific scenarios.

---

## Tables

### Input Flags
| Flag Name                  | Values       | Description                                   |
|----------------------------|--------------|-----------------------------------------------|
| `INPUT-OK`                 | `'0'`        | Input is valid.                               |
| `INPUT-ERROR`              | `'1'`        | Input contains errors.                        |
| `INPUT-PENDING`            | `LOW-VALUES` | Input is pending validation.                  |

### Output Variables
| Variable Name              | Redefines               | Description                                   |
|----------------------------|-------------------------|-----------------------------------------------|
| `CARD-ACCT-ID-N`           | `CARD-ACCT-ID-X`        | Account ID in numeric format.                |
| `CARD-CVV-CD-N`            | `CARD-CVV-CD-X`         | CVV code in numeric format.                  |
| `CARD-CARD-NUM-N`          | `CARD-CARD-NUM-X`       | Card number in numeric format.               |
| `CARD-EXPIRAION-DATE-N`    | `CARD-EXPIRAION-DATE-X` | Expiration date in numeric format.           |

---

## Error Messages
| Error Type                 | Message                                      |
|----------------------------|----------------------------------------------|
| File Error                 | `'File Error: '` followed by operation details. |
| Account Validation Error   | `'Account number must be a non zero 11 digit number'` |
| Card Validation Error      | `'Card number if supplied must be a 16 digit number'` |
| Unexpected Data Scenario   | `'UNEXPECTED DATA SCENARIO'`                |

---

## Debugging Routines
- **SEND-LONG-TEXT**: Displays long debugging messages.
- **SEND-PLAIN-TEXT**: Displays plain text messages for debugging purposes.

---

## License
This program is licensed under the Apache License, Version 2.0. For more details, visit [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
