# Documentation for `COMEN01C.CBL`

## Metadata
| **File Name** | `COMEN01C.cbl` |
|---------------|----------------|
| **Application** | CardDemo |
| **Type** | CICS COBOL Program |
| **Function** | Main Menu for Regular Users |

## Overview
The `COMEN01C` program is a CICS COBOL application designed to provide a main menu interface for regular users of the CardDemo system. It handles user interactions, menu navigation, and program transitions based on user input. The program includes error handling, dynamic menu generation, and communication with other programs via CICS commands.

## Sections

### Identification Division
- **Program ID**: `COMEN01C`
- **Author**: AWS

### Environment Division
- **Configuration Section**: No specific configurations are defined in this section.

### Data Division

#### Working-Storage Section
This section defines variables used throughout the program. Key variables include:

| **Variable Name** | **Type** | **Purpose** |
|--------------------|----------|-------------|
| `WS-PGMNAME` | PIC X(08) | Stores the program name (`COMEN01C`). |
| `WS-TRANID` | PIC X(04) | Transaction ID (`CM00`). |
| `WS-MESSAGE` | PIC X(80) | Holds error or informational messages. |
| `WS-USRSEC-FILE` | PIC X(08) | User security file name (`USRSEC`). |
| `WS-ERR-FLG` | PIC X(01) | Error flag (`Y` for error, `N` for no error). |
| `WS-RESP-CD` | PIC S9(09) COMP | Response code for CICS commands. |
| `WS-OPTION` | PIC 9(02) | Stores the user's selected menu option. |
| `WS-MENU-OPT-TXT` | PIC X(40) | Text for menu options. |

#### Linkage Section
Defines the `DFHCOMMAREA` structure for inter-program communication. The size of the communication area depends on the `EIBCALEN` value.

### Procedure Division

#### Main Logic (`MAIN-PARA`)
The main logic handles:
1. Initialization of error flags and messages.
2. Determining whether the program is being re-entered or newly invoked.
3. Sending and receiving menu screens.
4. Processing user input and navigating to the appropriate program or handling errors.

#### Key Subroutines
1. **`PROCESS-ENTER-KEY`**:
   - Validates user input for menu options.
   - Handles access restrictions for admin-only options.
   - Executes the selected program if valid.

2. **`RETURN-TO-SIGNON-SCREEN`**:
   - Redirects the user to the sign-on screen (`COSGN00C`).

3. **`SEND-MENU-SCREEN`**:
   - Sends the dynamically built menu screen to the user.

4. **`RECEIVE-MENU-SCREEN`**:
   - Receives user input from the menu screen.

5. **`POPULATE-HEADER-INFO`**:
   - Populates header information such as current date, time, program name, and transaction ID.

6. **`BUILD-MENU-OPTIONS`**:
   - Dynamically builds menu options based on the available options in the system.

### CICS Commands
The program uses several CICS commands:
- `EXEC CICS RETURN`: Returns control to CICS with transaction and communication area.
- `EXEC CICS XCTL`: Transfers control to another program.
- `EXEC CICS SEND`: Sends a map to the terminal.
- `EXEC CICS RECEIVE`: Receives user input from the terminal.

## Insights

### Error Handling
The program uses the `WS-ERR-FLG` variable to track errors. It provides user-friendly error messages and ensures invalid inputs are handled gracefully.

### Dynamic Menu Generation
The `BUILD-MENU-OPTIONS` subroutine dynamically constructs menu options based on the available options in the system. This allows flexibility in menu design and supports up to 12 options.

### User Access Control
The program enforces access restrictions for admin-only options. Regular users attempting to access these options are presented with an error message.

### Inter-Program Communication
The program uses the `DFHCOMMAREA` structure to pass data between programs. This ensures seamless transitions and data consistency.

### Licensing
The program is licensed under the Apache License, Version 2.0, which allows for open-source usage and distribution under specified conditions.

### Dependencies
The program includes several copybooks:
- `COCOM01Y`, `COMEN02Y`, `COMEN01`, `COTTL01Y`, `CSDAT01Y`, `CSMSG01Y`, `CSUSR01Y`
- CICS-specific copybooks: `DFHAID`, `DFHBMSCA`

These copybooks provide definitions for data structures, constants, and mappings used in the program.

### Target Audience
This program is designed for regular users of the CardDemo system, providing them with a simple and intuitive menu interface for navigating the application.

### Version Information
- **Version**: `CardDemo_v1.0-15-g27d6c6f-68`
- **Date**: `2022-07-19 23:12:33 CDT`
