# Documentation for `COTRN01C.cbl`

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| `COTRN01C.cbl` | CardDemo | CICS COBOL Program | View a Transaction from `TRANSACT` file |

---

## Overview

The program `COTRN01C` is a CICS COBOL application designed to retrieve and display transaction details from the `TRANSACT` file. It interacts with the user interface via CICS maps and handles user inputs, errors, and file operations. The program is structured into multiple sections, including data declarations, linkage, and procedural logic.

---

## Sections

### **1. Data Structures**

#### **Working-Storage Section**
This section defines variables used throughout the program for temporary storage and control flags.

| **Variable Name**       | **Type**         | **Purpose**                                                                 |
|--------------------------|------------------|-----------------------------------------------------------------------------|
| `WS-PGMNAME`            | `PIC X(08)`      | Stores the program name (`COTRN01C`).                                       |
| `WS-TRANID`             | `PIC X(04)`      | Stores the transaction ID (`CT01`).                                         |
| `WS-MESSAGE`            | `PIC X(80)`      | Stores error or informational messages.                                     |
| `WS-TRANSACT-FILE`      | `PIC X(08)`      | Name of the transaction file (`TRANSACT`).                                  |
| `WS-ERR-FLG`            | `PIC X(01)`      | Error flag (`Y` for error, `N` for no error).                               |
| `WS-RESP-CD`            | `PIC S9(09) COMP`| Stores CICS response codes.                                                 |
| `WS-REAS-CD`            | `PIC S9(09) COMP`| Stores CICS reason codes.                                                   |
| `WS-USR-MODIFIED`       | `PIC X(01)`      | Tracks if the user modified the data (`Y` for yes, `N` for no).             |
| `WS-TRAN-AMT`           | `PIC +99999999.99`| Stores the transaction amount.                                              |
| `WS-TRAN-DATE`          | `PIC X(08)`      | Stores the transaction date (`00/00/00` by default).                        |

#### **Linkage Section**
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

| **Variable Name** | **Type** | **Purpose** |
|--------------------|----------|-------------|
| `DFHCOMMAREA`     | `PIC X(01)` OCCURS 1 TO 32767 | Used for inter-program communication. |

---

### **2. Logic and Procedures**

#### **Main Logic (`MAIN-PARA`)**
The main entry point of the program. It initializes flags, handles user inputs, and determines the flow based on the `EIBAID` (attention identifier).

Key operations:
- Initializes error and modification flags.
- Handles cases where `EIBCALEN` (communication area length) is zero.
- Processes user inputs based on attention keys (`DFHENTER`, `DFHPF3`, `DFHPF4`, `DFHPF5`).
- Sends or receives screens using CICS commands.

#### **Process-Enter-Key**
Handles the logic for processing the `ENTER` key. Validates the transaction ID and reads the transaction file.

Key operations:
- Validates `TRNIDINI` (transaction ID input).
- Reads the `TRANSACT` file using the `READ-TRANSACT-FILE` procedure.
- Populates transaction details into the working storage.

#### **Return-to-Prev-Screen**
Transfers control back to the previous screen or program.

Key operations:
- Sets the target program (`CDEMO-TO-PROGRAM`).
- Executes the `XCTL` command to transfer control.

#### **Send-TrnView-Screen**
Sends the transaction view screen to the user.

Key operations:
- Populates header information using `POPULATE-HEADER-INFO`.
- Sends the screen using the `SEND` CICS command.

#### **Receive-TrnView-Screen**
Receives user input from the transaction view screen.

Key operations:
- Uses the `RECEIVE` CICS command to retrieve data into `COTRN1AI`.

#### **Populate-Header-Info**
Populates header information for the transaction view screen.

Key operations:
- Retrieves the current date and time.
- Populates program and transaction details into the screen fields.

#### **Read-Transact-File**
Reads the `TRANSACT` file to retrieve transaction details.

Key operations:
- Uses the `READ` CICS command to fetch data based on the transaction ID.
- Handles response codes (`NORMAL`, `NOTFND`, and others).

#### **Clear-Current-Screen**
Clears the current screen and reinitializes fields.

Key operations:
- Calls `INITIALIZE-ALL-FIELDS` to reset all fields.
- Sends the cleared screen using `SEND-TRNVIEW-SCREEN`.

#### **Initialize-All-Fields**
Resets all fields to their default values.

Key operations:
- Sets transaction-related fields to spaces or default values.
- Clears the error message field.

---

## Insights

1. **Error Handling**:
   - The program uses flags (`WS-ERR-FLG`) and response codes (`WS-RESP-CD`, `WS-REAS-CD`) to manage errors effectively.
   - Specific error messages are displayed to guide the user when invalid inputs or file read errors occur.

2. **CICS Integration**:
   - The program heavily relies on CICS commands (`SEND`, `RECEIVE`, `READ`, `RETURN`, `XCTL`) for screen management and file operations.
   - Attention keys (`DFHENTER`, `DFHPF3`, etc.) are used to control program flow based on user actions.

3. **Modular Design**:
   - Procedures are well-organized and modular, making the program easier to maintain and extend.
   - Common operations like screen clearing and field initialization are encapsulated in dedicated procedures.

4. **Dynamic Communication Area**:
   - The use of `DFHCOMMAREA` with dynamic sizing (`EIBCALEN`) allows flexible data exchange between programs.

5. **User Interaction**:
   - The program provides clear feedback to the user through error messages and screen updates.
   - It ensures data integrity by validating inputs before processing.

6. **File Operations**:
   - The program reads the `TRANSACT` file using a key-based approach (`TRAN-ID`), ensuring efficient data retrieval.

---

## References to External Copies

The program includes several `COPY` statements to import external definitions:
- `COCOM01Y`, `COTRN01`, `COTTL01Y`, `CSDAT01Y`, `CSMSG01Y`, `CVTRA05Y`, `DFHAID`, `DFHBMSCA`.

These copies likely contain shared data structures, constants, or utility procedures used across multiple programs in the application.
