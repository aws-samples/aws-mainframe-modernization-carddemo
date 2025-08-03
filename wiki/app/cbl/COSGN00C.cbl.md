# Documentation for COSGN00C.CBL

## Metadata
| **File Name** | **COSGN00C.cbl** |
|---------------|------------------|
| **Application** | CardDemo |
| **Type** | CICS COBOL Program |
| **Function** | Signon Screen for the CardDemo Application |

---

## Overview

The `COSGN00C.CBL` program is a CICS COBOL application designed to handle the sign-on screen functionality for the CardDemo application. It facilitates user authentication by interacting with a user security file and provides appropriate responses based on user input and system conditions.

---

## Sections

### **1. Identification Division**
- **Program ID**: `COSGN00C`
- **Author**: AWS

### **2. Environment Division**
- Configuration settings are defined here, but no specific configurations are detailed in the provided code.

### **3. Data Division**

#### **Working-Storage Section**
This section defines variables used throughout the program. Key variables include:
| **Variable Name** | **Description** | **Data Type** | **Initial Value** |
|-------------------|-----------------|---------------|-------------------|
| `WS-PGMNAME` | Program name | PIC X(08) | `'COSGN00C'` |
| `WS-TRANID` | Transaction ID | PIC X(04) | `'CC00'` |
| `WS-MESSAGE` | Message buffer | PIC X(80) | Spaces |
| `WS-USRSEC-FILE` | User security file name | PIC X(08) | `'USRSEC  '` |
| `WS-ERR-FLG` | Error flag | PIC X(01) | `'N'` |
| `ERR-FLG-ON` | Error flag condition | 88 Level | `'Y'` |
| `ERR-FLG-OFF` | Error flag condition | 88 Level | `'N'` |
| `WS-RESP-CD` | Response code | PIC S9(09) COMP | Zeros |
| `WS-REAS-CD` | Reason code | PIC S9(09) COMP | Zeros |
| `WS-USER-ID` | User ID | PIC X(08) | Undefined |
| `WS-USER-PWD` | User password | PIC X(08) | Undefined |

#### **Linkage Section**
Defines the communication area (`DFHCOMMAREA`) for passing data between programs. It uses dynamic sizing based on `EIBCALEN`.

---

### **4. Procedure Division**

#### **Main Logic**
The main logic handles the sign-on process and user input validation. It includes:
1. **Initialization**:
   - Sets error flag to off.
   - Clears message buffers.
2. **Input Handling**:
   - Checks if `EIBCALEN` is zero (indicating no communication area).
   - Evaluates user input based on `EIBAID` (CICS attention identifier).
3. **CICS Return**:
   - Returns control to the transaction specified in `WS-TRANID`.

#### **Key Subroutines**

| **Subroutine** | **Description** |
|----------------|-----------------|
| `PROCESS-ENTER-KEY` | Handles user input validation for User ID and Password. If valid, reads the user security file. |
| `SEND-SIGNON-SCREEN` | Sends the sign-on screen to the user with appropriate error messages. |
| `SEND-PLAIN-TEXT` | Sends plain text messages to the user. |
| `POPULATE-HEADER-INFO` | Populates header information such as current date, time, and system/application identifiers. |
| `READ-USER-SEC-FILE` | Reads the user security file to validate credentials and determine user type. |

---

## Insights

### **Error Handling**
- The program uses the `WS-ERR-FLG` variable to track errors. Specific error messages are displayed based on conditions such as missing User ID, missing Password, or invalid credentials.

### **CICS Integration**
- The program heavily relies on CICS commands (`EXEC CICS`) for operations such as screen handling (`SEND`), data retrieval (`READ`), and program control (`XCTL`).

### **Dynamic User Interaction**
- The program dynamically evaluates user input and provides tailored responses, ensuring a user-friendly experience.

### **Security**
- User credentials are validated against a security file (`USRSEC`). Passwords are compared directly, and user types are determined for further program control.

### **Modular Design**
- The program is structured into modular subroutines, making it easier to maintain and extend functionality.

### **Dependencies**
- The program includes several copybooks (`COCOM01Y`, `COSGN00`, etc.) that likely define additional structures and constants used throughout the program.

---

## Key Features

| **Feature** | **Description** |
|-------------|-----------------|
| **Sign-On Screen** | Displays a sign-on screen for user authentication. |
| **User Validation** | Validates User ID and Password against a security file. |
| **Error Messaging** | Provides detailed error messages for invalid inputs or system issues. |
| **Dynamic Navigation** | Redirects users to different programs based on their user type (e.g., Admin or Regular User). |
| **Header Information** | Populates and displays system and application details dynamically. |

---

## License Information
This program is licensed under the Apache License, Version 2.0. For more details, visit [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
