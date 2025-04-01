# CBSTM03A - COBOL Program Documentation

## Overview

### Program Information
| **Attribute**       | **Value**                          |
|---------------------|------------------------------------|
| **Program Name**    | `CBSTM03A`                        |
| **Application**     | `CardDemo`                        |
| **Type**            | Batch COBOL Program               |
| **Function**        | Generates account statements in two formats: plain text and HTML. |

### Purpose
This program processes transaction data to generate account statements in two formats:
1. Plain text format.
2. HTML format.

It demonstrates several COBOL features, including:
- Mainframe control block addressing.
- Use of `ALTER` and `GO TO` statements.
- Handling of `COMP` and `COMP-3` variables.
- Use of two-dimensional arrays.
- Subroutine calls.

### Licensing
The program is licensed under the Apache License, Version 2.0.

---

## File Handling

### Input/Output Files
| **File Name**   | **Description**                     | **Record Layout**       |
|-----------------|-------------------------------------|-------------------------|
| `STMT-FILE`     | Output file for plain text statements. | `FD-STMTFILE-REC` (80 bytes) |
| `HTML-FILE`     | Output file for HTML statements.     | `FD-HTMLFILE-REC` (100 bytes) |

### File Operations
- **Open**: Files are opened for output.
- **Write**: Records are written to the respective files.
- **Close**: Files are closed after processing.

---

## Data Structures

### Key Data Structures
#### Working-Storage Section
| **Variable Name**      | **Type**       | **Purpose**                                                                 |
|-------------------------|---------------|-----------------------------------------------------------------------------|
| `COMP-VARIABLES`        | `COMP`        | Stores counters and jump variables for processing.                          |
| `COMP3-VARIABLES`       | `COMP-3`      | Stores total transaction amount.                                            |
| `MISC-VARIABLES`        | Various       | Miscellaneous variables for file handling and control.                      |
| `STATEMENT-LINES`       | Structured    | Defines the layout of the plain text statement.                             |
| `HTML-LINES`            | Structured    | Defines the layout of the HTML statement.                                   |
| `WS-TRNX-TABLE`         | 2D Array      | Stores transaction data for multiple cards.                                 |
| `WS-TRN-TBL-CNTR`       | Array         | Stores transaction counters for each card.                                  |

#### Linkage Section
| **Variable Name** | **Purpose**                                                                 |
|--------------------|---------------------------------------------------------------------------|
| `PSA-BLOCK`        | Represents the Program Status Area (PSA) for control block addressing.   |
| `TCB-BLOCK`        | Represents the Task Control Block (TCB).                                |
| `TIOT-BLOCK`       | Represents the Task Input/Output Table (TIOT).                          |

---

## Program Logic

### Main Processing Flow
1. **Initialization**:
   - Open output files (`STMT-FILE` and `HTML-FILE`).
   - Initialize working storage tables and counters.

2. **File Processing**:
   - Process transaction data (`TRNXFILE`) and cross-reference data (`XREFFILE`).
   - Retrieve customer and account details from respective files (`CUSTFILE` and `ACCTFILE`).

3. **Statement Generation**:
   - Generate plain text and HTML statements for each account.
   - Write transaction details and summary to the output files.

4. **File Closure**:
   - Close all input and output files.

5. **Error Handling**:
   - Display error messages and terminate the program in case of file operation failures.

### Key Procedures
| **Procedure Name**       | **Description**                                                                 |
|---------------------------|-------------------------------------------------------------------------------|
| `1000-MAINLINE`           | Main loop for processing transactions and generating statements.              |
| `5000-CREATE-STATEMENT`   | Creates the account statement in both plain text and HTML formats.            |
| `6000-WRITE-TRANS`        | Writes individual transaction details to the statement files.                 |
| `8100-FILE-OPEN`          | Opens the transaction file (`TRNXFILE`).                                      |
| `8500-READTRNX-READ`      | Reads transaction records and populates the transaction table.                |
| `9100-TRNXFILE-CLOSE`     | Closes the transaction file.                                                  |
| `9999-ABEND-PROGRAM`      | Handles program termination in case of errors.                                |

---

## Features Demonstrated

### COBOL Features
1. **Control Block Addressing**:
   - Uses `PSA-BLOCK`, `TCB-BLOCK`, and `TIOT-BLOCK` for mainframe control block addressing.
   - Displays job and step information from the TIOT.

2. **`ALTER` and `GO TO` Statements**:
   - Dynamically alters the flow of execution based on file type (`TRNXFILE`, `XREFFILE`, etc.).

3. **`COMP` and `COMP-3` Variables**:
   - `COMP` variables are used for counters and jump variables.
   - `COMP-3` variables are used for precise arithmetic operations (e.g., transaction totals).

4. **Two-Dimensional Arrays**:
   - `WS-TRNX-TABLE` stores transaction data for multiple cards, with each card having multiple transactions.

5. **Subroutine Calls**:
   - Calls subroutine `CBSTM03B` for file operations (open, read, write, close).

---

## Insights

### Strengths
- **Modular Design**: The program is divided into well-defined procedures, making it easier to maintain and extend.
- **Dual Output Formats**: Supports both plain text and HTML formats, catering to different use cases.
- **Error Handling**: Includes robust error handling for file operations, ensuring program stability.

### Potential Improvements
- **Modernization**: Replace `ALTER` and `GO TO` statements with structured programming constructs for better readability and maintainability.
- **Dynamic HTML Generation**: Use external templates or libraries for HTML generation to simplify the code.
- **Performance Optimization**: Optimize the use of arrays and loops for large datasets.

### Use Cases
- Generating account statements for banking or financial applications.
- Demonstrating COBOL features for training or modernization purposes.
