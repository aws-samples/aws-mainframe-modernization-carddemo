# Documentation for COTRN00C.CBL

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| COTRN00C.CBL  | CardDemo        | CICS COBOL Program | List Transactions from TRANSACT file |

## Overview
The program `COTRN00C` is a CICS COBOL application designed to list transactions from the `TRANSACT` file. It interacts with CICS services to manage transaction data, navigate through pages of transaction records, and handle user inputs via screens. The program supports operations such as navigating forward and backward through transaction pages, selecting transactions, and handling errors.

## Sections

### Identification Division
- **Program ID**: `COTRN00C`
- **Author**: AWS

### Environment Division
Defines the configuration for the program's runtime environment.

### Data Division
#### Working-Storage Section
Contains variables used for program execution, including flags, counters, and temporary storage.

| **Variable Name**       | **Type**       | **Purpose** |
|--------------------------|----------------|-------------|
| `WS-PGMNAME`             | PIC X(08)     | Program name identifier. |
| `WS-TRANID`              | PIC X(04)     | Transaction ID. |
| `WS-MESSAGE`             | PIC X(80)     | Message buffer for error or status messages. |
| `WS-TRANSACT-FILE`       | PIC X(08)     | Name of the transaction file. |
| `WS-ERR-FLG`             | PIC X(01)     | Error flag. |
| `WS-TRANSACT-EOF`        | PIC X(01)     | End-of-file flag for the transaction file. |
| `WS-SEND-ERASE-FLG`      | PIC X(01)     | Flag to determine whether to erase the screen before sending data. |
| `WS-RESP-CD`             | PIC S9(09) COMP | Response code for CICS operations. |
| `WS-REAS-CD`             | PIC S9(09) COMP | Reason code for CICS operations. |
| `WS-REC-COUNT`           | PIC S9(04) COMP | Record counter. |
| `WS-IDX`                 | PIC S9(04) COMP | Index for transaction records. |
| `WS-PAGE-NUM`            | PIC S9(04) COMP | Current page number. |
| `WS-TRAN-AMT`            | PIC +99999999.99 | Transaction amount. |
| `WS-TRAN-DATE`           | PIC X(08)     | Transaction date. |

#### Linkage Section
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

### Procedure Division
The main logic of the program is implemented in the Procedure Division. It includes multiple paragraphs for handling specific operations.

#### Main-PARA
- Initializes flags and variables.
- Handles the initial screen setup and user input processing.
- Navigates between screens based on user actions.

#### Process-Enter-Key
- Processes user input for selecting transactions.
- Validates transaction selection and navigates to the appropriate program.

#### Process-PF7-Key
- Handles navigation to the previous page of transactions.
- Displays an error message if the user is already at the top of the page.

#### Process-PF8-Key
- Handles navigation to the next page of transactions.
- Displays an error message if the user is already at the bottom of the page.

#### Process-Page-Forward
- Reads the next set of transaction records from the file.
- Populates transaction data for display on the screen.

#### Process-Page-Backward
- Reads the previous set of transaction records from the file.
- Populates transaction data for display on the screen.

#### Populate-TRAN-Data
- Maps transaction data from the file to screen fields based on the current index.

#### Initialize-TRAN-Data
- Clears transaction data fields for the current page.

#### Return-to-Prev-Screen
- Navigates back to the previous screen.

#### Send-TRNLST-Screen
- Sends the transaction list screen to the user.
- Handles screen erasure based on the `SEND-ERASE-FLG`.

#### Receive-TRNLST-Screen
- Receives user input from the transaction list screen.

#### Populate-Header-Info
- Populates header information for the screen, including the current date and time.

#### StartBR-TRANSACT-File
- Starts a browse operation on the `TRANSACT` file.

#### ReadNEXT-TRANSACT-File
- Reads the next record from the `TRANSACT` file.

#### ReadPREV-TRANSACT-File
- Reads the previous record from the `TRANSACT` file.

#### ENDBR-TRANSACT-File
- Ends the browse operation on the `TRANSACT` file.

## Insights
- **CICS Integration**: The program heavily relies on CICS commands (`STARTBR`, `READNEXT`, `READPREV`, `SEND`, `RECEIVE`, `XCTL`) for file operations and screen management.
- **Pagination**: Implements logic for navigating through transaction records in a paginated manner, supporting both forward and backward navigation.
- **Error Handling**: Includes robust error handling mechanisms for invalid inputs, end-of-file conditions, and CICS operation failures.
- **Screen Management**: Dynamically updates screen fields based on user actions and transaction data.
- **Reusability**: The program uses modular paragraphs for specific tasks, enhancing maintainability and readability.

## Dependencies
The program includes several copybooks:
- `COCOM01Y`
- `COTRN00`
- `COTTL01Y`
- `CSDAT01Y`
- `CSMSG01Y`
- `CVTRA05Y`
- `DFHAID`
- `DFHBMSCA`

These copybooks likely define data structures, constants, and mappings used throughout the program.
