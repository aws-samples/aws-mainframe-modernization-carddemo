# Documentation for `COUSR01C.cbl`

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| `COUSR01C.cbl` | CardDemo | CICS COBOL Program | Add a new Regular/Admin user to `USRSEC` file |

---

## Overview

This COBOL program is designed to add a new user (Regular or Admin) to the `USRSEC` file in a CICS environment. It interacts with the user interface to collect user details, validates the input, and writes the user information to the security file. The program handles various scenarios, including empty fields, duplicate user IDs, and other errors.

---

## Sections

### **1. Identification Division**
- **Program ID**: `COUSR01C`
- **Author**: AWS

### **2. Environment Division**
- Specifies the configuration for the program.

### **3. Data Division**

#### **Working-Storage Section**
This section defines variables used throughout the program. Key variables include:
| **Variable Name** | **Type** | **Purpose** |
|--------------------|----------|-------------|
| `WS-PGMNAME`       | PIC X(08) | Program name (`COUSR01C`) |
| `WS-TRANID`        | PIC X(04) | Transaction ID (`CU01`) |
| `WS-MESSAGE`       | PIC X(80) | Message buffer for user feedback |
| `WS-USRSEC-FILE`   | PIC X(08) | File name (`USRSEC`) |
| `WS-ERR-FLG`       | PIC X(01) | Error flag (`Y` for error, `N` for no error) |
| `WS-RESP-CD`       | PIC S9(09) COMP | Response code from CICS operations |
| `WS-REAS-CD`       | PIC S9(09) COMP | Reason code from CICS operations |

#### **Linkage Section**
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

---

### **4. Procedure Division**

#### **Main Logic**
The main logic (`MAIN-PARA`) handles the following:
1. **Initialization**:
   - Sets error flag to `OFF`.
   - Clears message buffers.
2. **Input Handling**:
   - If no communication area (`EIBCALEN = 0`), returns to the previous screen.
   - Otherwise, processes user input and determines the next action based on the key pressed (`EIBAID`).
3. **Key Handling**:
   - Handles `ENTER`, `PF3`, `PF4`, and other keys.
   - Validates user input and displays appropriate messages.

#### **Key Subroutines**
| **Subroutine** | **Purpose** |
|-----------------|-------------|
| `PROCESS-ENTER-KEY` | Validates user input fields (e.g., First Name, Last Name, User ID, Password, User Type). Writes user data to the `USRSEC` file if validation passes. |
| `RETURN-TO-PREV-SCREEN` | Returns control to the previous screen or program. |
| `SEND-USRADD-SCREEN` | Sends the user addition screen (`COUSR1A`) to the terminal. |
| `RECEIVE-USRADD-SCREEN` | Receives user input from the addition screen. |
| `POPULATE-HEADER-INFO` | Populates header information (e.g., current date, time, program name). |
| `WRITE-USER-SEC-FILE` | Writes user data to the `USRSEC` file. Handles duplicate keys and other errors. |
| `CLEAR-CURRENT-SCREEN` | Clears the current screen and reinitializes fields. |
| `INITIALIZE-ALL-FIELDS` | Resets all fields to their default values (e.g., spaces, `-1`).

---

## Insights

### **Error Handling**
- The program uses the `WS-ERR-FLG` flag to track errors during validation and file operations.
- Specific error messages are displayed for empty fields, duplicate user IDs, and other issues.

### **CICS Integration**
- The program heavily relies on CICS commands (`EXEC CICS`) for screen handling (`SEND`, `RECEIVE`) and file operations (`WRITE`).

### **User Input Validation**
- Each user input field (First Name, Last Name, User ID, Password, User Type) is validated to ensure it is not empty or invalid.
- Invalid inputs trigger error messages and prevent further processing.

### **Dynamic Screen Navigation**
- The program supports dynamic navigation between screens based on user actions (e.g., pressing `PF3` or `PF4`).

### **File Operations**
- The `WRITE-USER-SEC-FILE` subroutine writes user data to the `USRSEC` file and handles duplicate keys (`DUPKEY`) and other errors.

### **Reusability**
- The program uses modular subroutines for common tasks (e.g., screen handling, field initialization), improving maintainability and reusability.

---

## External Dependencies
The program includes several copybooks:
| **Copybook** | **Purpose** |
|--------------|-------------|
| `COCOM01Y`   | Common definitions |
| `COUSR01`    | User-related definitions |
| `COTTL01Y`   | Title-related definitions |
| `CSDAT01Y`   | Date-related definitions |
| `CSMSG01Y`   | Message-related definitions |
| `CSUSR01Y`   | User-related definitions |
| `DFHAID`     | CICS attention identifier |
| `DFHBMSCA`   | CICS basic mapping support |

---

## License
This program is licensed under the Apache License, Version 2.0. For details, visit [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
