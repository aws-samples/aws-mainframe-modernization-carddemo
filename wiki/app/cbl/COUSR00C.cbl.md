# Documentation for `COUSR00C.CBL`

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| `COUSR00C.CBL` | CardDemo | CICS COBOL Program | List all users from `USRSEC` file |

---

## Overview

The `COUSR00C.CBL` program is a CICS COBOL application designed to list users from the `USRSEC` file. It provides functionality for navigating through user records, handling user selections, and managing pagination. The program interacts with CICS services for file operations and screen management.

---

## Sections

### Identification Division
- **Program ID**: `COUSR00C`
- **Author**: AWS

### Environment Division
- **Configuration Section**: Defines the environment setup for the program.

### Data Division

#### Working-Storage Section
This section contains variables used for program execution, including flags, counters, and user data structures.

| **Variable Name** | **Description** | **Type** | **Initial Value** |
|--------------------|-----------------|----------|-------------------|
| `WS-PGMNAME` | Program name | PIC X(08) | `'COUSR00C'` |
| `WS-TRANID` | Transaction ID | PIC X(04) | `'CU00'` |
| `WS-MESSAGE` | Message buffer | PIC X(80) | Spaces |
| `WS-USRSEC-FILE` | File name for user security | PIC X(08) | `'USRSEC  '` |
| `WS-ERR-FLG` | Error flag | PIC X(01) | `'N'` |
| `WS-USER-DATA` | User data structure | OCCURS 10 TIMES | Contains user selection, ID, name, and type |

#### Linkage Section
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

---

### Procedure Division

#### Main Logic (`MAIN-PARA`)
- Initializes flags and variables.
- Handles program re-entry and navigation between screens.
- Processes user input and evaluates actions based on keys pressed (`ENTER`, `PF3`, `PF7`, `PF8`).
- Returns control to CICS with transaction ID and communication area.

#### Key Processing
- **`PROCESS-ENTER-KEY`**: Handles user selection and navigates to appropriate programs based on the selection (`U` for update, `D` for delete).
- **`PROCESS-PF7-KEY`**: Handles backward pagination.
- **`PROCESS-PF8-KEY`**: Handles forward pagination.

#### Pagination Logic
- **`PROCESS-PAGE-FORWARD`**: Reads the next set of user records from the `USRSEC` file.
- **`PROCESS-PAGE-BACKWARD`**: Reads the previous set of user records from the `USRSEC` file.

#### File Operations
- **`STARTBR-USER-SEC-FILE`**: Starts browsing the `USRSEC` file.
- **`READNEXT-USER-SEC-FILE`**: Reads the next record from the file.
- **`READPREV-USER-SEC-FILE`**: Reads the previous record from the file.
- **`ENDBR-USER-SEC-FILE`**: Ends browsing the file.

#### Screen Management
- **`SEND-USRLST-SCREEN`**: Sends the user list screen to the terminal.
- **`RECEIVE-USRLST-SCREEN`**: Receives user input from the screen.

#### Data Initialization
- **`INITIALIZE-USER-DATA`**: Clears user data fields.
- **`POPULATE-USER-DATA`**: Populates user data fields with values from the file.

---

## Insights

1. **CICS Integration**: The program heavily relies on CICS commands (`STARTBR`, `READNEXT`, `READPREV`, `SEND`, `RECEIVE`, `XCTL`) for file operations and screen management.
2. **Pagination Support**: Implements forward and backward pagination for navigating user records.
3. **Error Handling**: Uses flags (`ERR-FLG`, `USER-SEC-EOF`) to manage errors and end-of-file conditions.
4. **Dynamic Screen Updates**: Supports dynamic updates to the user list screen based on user actions.
5. **User Selection Logic**: Allows users to select records for update (`U`) or delete (`D`) operations.
6. **Modular Design**: The program is divided into logical modules for better readability and maintainability.

---

## Key Features

| **Feature** | **Description** |
|-------------|-----------------|
| **User Listing** | Displays a paginated list of users from the `USRSEC` file. |
| **Navigation** | Supports navigation through user records using `PF7` (backward) and `PF8` (forward) keys. |
| **Selection Handling** | Processes user selections for update or delete operations. |
| **Error Messaging** | Displays appropriate error messages for invalid actions or file errors. |
| **Screen Management** | Dynamically updates the screen based on user actions and file data. |

---

## Dependencies

The program includes several copybooks:
- `COCOM01Y`
- `COUSR00`
- `COTTL01Y`
- `CSDAT01Y`
- `CSMSG01Y`
- `CSUSR01Y`
- `DFHAID`
- `DFHBMSCA`

These copybooks provide definitions for data structures, constants, and CICS mappings used in the program.

---

## Version Information

| **Version** | **Date** | **Time** | **Timezone** |
|-------------|----------|----------|--------------|
| `CardDemo_v1.0-15-g27d6c6f-68` | 2022-07-19 | 23:12:34 | CDT |
