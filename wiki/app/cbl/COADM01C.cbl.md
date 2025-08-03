# Documentation for `COADM01C.cbl`

## Metadata
- **File Name**: `COADM01C.cbl`
- **Application**: CardDemo
- **Type**: CICS COBOL Program
- **Function**: Admin Menu for Admin users
- **Version**: CardDemo_v1.0-15-g27d6c6f-68
- **Date**: 2022-07-19 23:12:32 CDT

---

## Overview

The `COADM01C.cbl` program is a CICS COBOL application designed to provide an administrative menu for admin users within the CardDemo application. It handles user interactions, menu navigation, and transitions between screens. The program is structured into multiple sections, including data declarations, logic for screen handling, and menu processing.

---

## Sections

### **Working-Storage Section**
This section defines variables used throughout the program. Key variables include:

| **Variable Name**       | **Type**       | **Purpose**                                                                 |
|--------------------------|----------------|-----------------------------------------------------------------------------|
| `WS-PGMNAME`            | PIC X(08)      | Stores the program name (`COADM01C`).                                       |
| `WS-TRANID`             | PIC X(04)      | Stores the transaction ID (`CA00`).                                         |
| `WS-MESSAGE`            | PIC X(80)      | Holds error or informational messages.                                      |
| `WS-USRSEC-FILE`        | PIC X(08)      | Stores the user security file name (`USRSEC`).                              |
| `WS-ERR-FLG`            | PIC X(01)      | Error flag (`Y` for error, `N` for no error).                               |
| `WS-RESP-CD`            | PIC S9(09) COMP| Stores the response code from CICS operations.                              |
| `WS-OPTION`             | PIC 9(02)      | Stores the selected menu option.                                            |
| `WS-ADMIN-OPT-TXT`      | PIC X(40)      | Holds the text for menu options.                                            |

### **Linkage Section**
Defines the communication area (`DFHCOMMAREA`) used for passing data between programs. It supports dynamic sizing based on `EIBCALEN`.

| **Variable Name** | **Type** | **Purpose** |
|--------------------|----------|-------------|
| `DFHCOMMAREA`     | PIC X(01) OCCURS 1 TO 32767 | Used for inter-program communication. |

---

## Procedure Division

### **Main Logic (`MAIN-PARA`)**
The main entry point of the program. It initializes variables, checks the communication area (`EIBCALEN`), and handles screen navigation based on user input.

#### Key Operations:
1. **Initialization**:
   - Sets error flag to off (`ERR-FLG-OFF`).
   - Clears messages.

2. **Communication Area Check**:
   - If `EIBCALEN = 0`, redirects to the sign-on screen (`COSGN00C`).
   - Otherwise, processes the communication area and determines if the program is re-entered.

3. **Screen Handling**:
   - Sends the menu screen (`SEND-MENU-SCREEN`).
   - Receives user input (`RECEIVE-MENU-SCREEN`).
   - Evaluates user actions (`EIBAID`) and processes accordingly:
     - `DFHENTER`: Processes the selected menu option.
     - `DFHPF3`: Returns to the sign-on screen.
     - Other keys: Displays an error message.

4. **CICS Return**:
   - Returns control to CICS with the transaction ID and communication area.

---

### **Subroutines**

#### **Process-Enter-Key**
Handles the logic for processing the `ENTER` key. It validates the selected menu option and executes the corresponding program.

| **Operation** | **Description** |
|---------------|-----------------|
| **Validation** | Ensures the selected option is numeric, within range, and not zero. |
| **Execution** | Transfers control to the program associated with the selected option using `XCTL`. |

#### **Return-to-Signon-Screen**
Redirects the user to the sign-on screen (`COSGN00C`) if necessary.

#### **Send-Menu-Screen**
Sends the menu screen to the user. It populates header information and builds menu options dynamically.

#### **Receive-Menu-Screen**
Receives user input from the menu screen and stores it in the input area (`COADM1AI`).

#### **Populate-Header-Info**
Populates header information such as current date, time, program name, and transaction ID.

#### **Build-Menu-Options**
Dynamically constructs menu options based on the available admin options. Each option is assigned to a specific output field (`OPTN001O` to `OPTN010O`).

---

## Insights

1. **Dynamic Menu Handling**:
   - The program dynamically builds menu options based on the number of available admin options (`CDEMO-ADMIN-OPT-COUNT`).

2. **Error Handling**:
   - Comprehensive error handling is implemented to validate user input and display appropriate messages.

3. **CICS Integration**:
   - The program leverages CICS commands (`SEND`, `RECEIVE`, `XCTL`, `RETURN`) for screen handling and program transitions.

4. **Reusability**:
   - The use of COPY statements (`COCOM01Y`, `COADM02Y`, etc.) indicates modular design and reusability of common structures.

5. **Scalability**:
   - The program supports up to 10 menu options and can dynamically adjust based on the number of available options.

6. **User Experience**:
   - Provides clear error messages and feedback to guide the user in selecting valid menu options.

---

## External Dependencies

The program relies on several external COPY files for data structures and constants:
- `COCOM01Y`
- `COADM02Y`
- `COADM01`
- `COTTL01Y`
- `CSDAT01Y`
- `CSMSG01Y`
- `CSUSR01Y`
- `DFHAID`
- `DFHBMSCA`

These files likely contain definitions for screen maps, constants, and other shared resources.
