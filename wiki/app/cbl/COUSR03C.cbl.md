# Documentation for `COUSR03C.CBL`

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| `COUSR03C.CBL` | CardDemo | CICS COBOL Program | Delete a user from `USRSEC` file |

---

## Overview

This COBOL program is designed to interact with a CICS environment to delete a user from the `USRSEC` file. It provides functionality for user validation, screen navigation, and file operations such as reading and deleting user records. The program uses CICS commands for transaction management and screen handling.

---

## Sections

### **Identification Division**
- **Program ID**: `COUSR03C`
- **Author**: AWS

### **Environment Division**
- Specifies the configuration for the program.

### **Data Division**
#### **Working-Storage Section**
Defines variables used throughout the program:
| **Variable Name**       | **Type**       | **Purpose**                                                                 |
|--------------------------|----------------|-----------------------------------------------------------------------------|
| `WS-PGMNAME`            | `PIC X(08)`    | Stores the program name (`COUSR03C`).                                       |
| `WS-TRANID`             | `PIC X(04)`    | Stores the transaction ID (`CU03`).                                         |
| `WS-MESSAGE`            | `PIC X(80)`    | Holds error or informational messages.                                      |
| `WS-USRSEC-FILE`        | `PIC X(08)`    | Name of the user security file (`USRSEC`).                                  |
| `WS-ERR-FLG`            | `PIC X(01)`    | Error flag (`Y` for error, `N` for no error).                               |
| `WS-RESP-CD`            | `PIC S9(09) COMP` | Stores response codes from CICS commands.                                   |
| `WS-REAS-CD`            | `PIC S9(09) COMP` | Stores reason codes from CICS commands.                                     |
| `WS-USR-MODIFIED`       | `PIC X(01)`    | Indicates if the user has been modified (`Y` for yes, `N` for no).          |

#### **Linkage Section**
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

---

## Procedure Division

### **Main Logic**
The main logic handles the following:
1. **Initialization**:
   - Resets error flags and clears messages.
2. **Transaction Handling**:
   - Checks if `DFHCOMMAREA` is empty and navigates to the previous screen if necessary.
   - Processes user input and navigates between screens based on function keys (`PF3`, `PF5`, etc.).
3. **CICS Return**:
   - Returns control to CICS with the transaction ID and communication area.

### **Key Subroutines**
#### **PROCESS-ENTER-KEY**
- Validates the user ID input.
- Reads user information from the `USRSEC` file if the input is valid.

#### **DELETE-USER-INFO**
- Validates the user ID input.
- Deletes the user record from the `USRSEC` file if the input is valid.

#### **RETURN-TO-PREV-SCREEN**
- Navigates back to the previous screen or program.

#### **SEND-USRDEL-SCREEN**
- Sends the user deletion screen to the terminal.

#### **RECEIVE-USRDEL-SCREEN**
- Receives user input from the deletion screen.

#### **POPULATE-HEADER-INFO**
- Populates header information such as current date, time, program name, and transaction ID.

#### **READ-USER-SEC-FILE**
- Reads user information from the `USRSEC` file using the user ID as the key.

#### **DELETE-USER-SEC-FILE**
- Deletes the user record from the `USRSEC` file.

#### **CLEAR-CURRENT-SCREEN**
- Clears the current screen and reinitializes fields.

#### **INITIALIZE-ALL-FIELDS**
- Resets all fields to their default values.

---

## Insights

### **Key Features**
- **CICS Integration**: The program uses CICS commands (`READ`, `DELETE`, `SEND`, `RECEIVE`, `RETURN`) for transaction and file management.
- **Error Handling**: Implements robust error handling with flags and messages to guide user actions.
- **Screen Navigation**: Supports navigation between screens using function keys (`PF3`, `PF5`, etc.).
- **Dynamic Data Handling**: Uses `DFHCOMMAREA` for passing data dynamically between programs.

### **Dependencies**
- **Copybooks**:
  - `COCOM01Y`, `COUSR03`, `COTTL01Y`, `CSDAT01Y`, `CSMSG01Y`, `CSUSR01Y`
  - `DFHAID`, `DFHBMSCA`
- **USRSEC File**: The program interacts with the `USRSEC` file for user data management.

### **Potential Enhancements**
- **Validation Improvements**: Enhance user ID validation to include additional checks (e.g., format validation).
- **Logging**: Add detailed logging for debugging and auditing purposes.
- **Error Messages**: Provide more descriptive error messages for better user experience.

---

## License Information
This program is licensed under the Apache License, Version 2.0. For details, visit [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
