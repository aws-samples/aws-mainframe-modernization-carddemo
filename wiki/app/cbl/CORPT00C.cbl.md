# Documentation for `CORPT00C.CBL`

## Metadata
| **File Name** | **Application** | **Type** | **Function** |
|---------------|-----------------|----------|--------------|
| `CORPT00C.CBL` | CardDemo       | CICS COBOL Program | Print transaction reports by submitting batch jobs from online using extra partition TDQ. |

---

## Overview

The `CORPT00C.CBL` program is a CICS COBOL application designed to generate transaction reports. It allows users to submit batch jobs for report generation through an extra partition Transient Data Queue (TDQ). The program supports multiple report types, including Monthly, Yearly, and Custom reports, and validates user inputs for date ranges before processing.

---

## Sections

### Identification Division
- **Program ID**: `CORPT00C`
- **Author**: AWS

### Environment Division
- **Configuration Section**: Defines the environment setup for the program.

### Data Division

#### Working-Storage Section
This section contains variables used throughout the program for processing and control. Key variables include:

| **Variable Name**       | **Description**                          | **Picture Clause** | **Initial Value** |
|--------------------------|------------------------------------------|---------------------|--------------------|
| `WS-PGMNAME`             | Program name                            | `PIC X(08)`         | `'CORPT00C'`       |
| `WS-TRANID`              | Transaction ID                          | `PIC X(04)`         | `'CR00'`           |
| `WS-MESSAGE`             | Message buffer                          | `PIC X(80)`         | Spaces             |
| `WS-TRANSACT-FILE`       | Transaction file name                   | `PIC X(08)`         | `'TRANSACT'`       |
| `WS-ERR-FLG`             | Error flag                              | `PIC X(01)`         | `'N'`              |
| `WS-START-DATE`          | Start date in `YYYY-MM-DD` format       | Composite structure | Spaces             |
| `WS-END-DATE`            | End date in `YYYY-MM-DD` format         | Composite structure | Spaces             |
| `WS-TRAN-AMT`            | Transaction amount                      | `PIC +99999999.99`  | -                  |
| `JCL-RECORD`             | JCL record buffer                       | `PIC X(80)`         | Spaces             |

#### Job Data
Contains predefined JCL templates for submitting batch jobs. The structure includes placeholders for parameters such as start and end dates.

#### Linkage Section
Defines the communication area (`DFHCOMMAREA`) for passing data between programs.

---

### Procedure Division

#### Main Logic (`MAIN-PARA`)
The main entry point of the program performs the following:
1. Initializes flags and variables.
2. Checks if the communication area (`EIBCALEN`) is empty.
3. Handles user input and navigates between screens.
4. Processes user actions based on the function key pressed (`EIBAID`).

#### Key Subroutines

| **Subroutine Name**       | **Description**                                                                 |
|----------------------------|---------------------------------------------------------------------------------|
| `PROCESS-ENTER-KEY`        | Handles the "Enter" key action, validates input, and prepares report parameters. |
| `SUBMIT-JOB-TO-INTRDR`     | Submits the batch job to the internal reader (INTRDR) for processing.            |
| `WIRTE-JOBSUB-TDQ`         | Writes JCL records to the TDQ (`JOBS`).                                          |
| `RETURN-TO-PREV-SCREEN`    | Navigates back to the previous screen.                                          |
| `SEND-TRNRPT-SCREEN`       | Sends the transaction report screen to the user.                                |
| `RECEIVE-TRNRPT-SCREEN`    | Receives user input from the transaction report screen.                         |
| `POPULATE-HEADER-INFO`     | Populates header information for the screen.                                    |
| `INITIALIZE-ALL-FIELDS`    | Resets all fields to their initial state.                                       |

---

## Key Features

### Report Types
The program supports three types of reports:
1. **Monthly**: Generates a report for the current month.
2. **Yearly**: Generates a report for the current year.
3. **Custom**: Allows users to specify a custom date range.

### Input Validation
- Validates user-provided start and end dates for numeric and logical correctness.
- Ensures required fields are not empty.

### Batch Job Submission
- Constructs JCL dynamically based on user input.
- Submits the job to the internal reader (INTRDR) for execution.

### Error Handling
- Flags errors using `WS-ERR-FLG`.
- Displays appropriate error messages to the user.

---

## Insights

1. **CICS Integration**: The program leverages CICS commands (`EXEC CICS`) for screen handling, TDQ operations, and program control.
2. **Dynamic JCL Generation**: The use of predefined templates and dynamic parameter substitution ensures flexibility in batch job submission.
3. **User Input Validation**: Comprehensive validation logic ensures data integrity and prevents invalid operations.
4. **Modular Design**: The program is structured into reusable subroutines, enhancing maintainability and readability.
5. **Error Messaging**: Error messages are dynamically constructed and displayed to guide users in correcting input issues.

---

## Dependencies
The program includes several copybooks for shared definitions and functionality:
- `COCOM01Y`
- `CORPT00`
- `COTTL01Y`
- `CSDAT01Y`
- `CSMSG01Y`
- `CVTRA05Y`
- `DFHAID`
- `DFHBMSCA`

---

## Licensing
The program is licensed under the Apache License, Version 2.0. For details, visit [Apache License](http://www.apache.org/licenses/LICENSE-2.0).
