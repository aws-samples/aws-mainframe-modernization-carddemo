# COBOL Program Documentation: CSUTLDTC.cbl

## Overview

This COBOL program, `CSUTLDTC`, is designed to interact with the `CEEDAYS` API to validate and convert a given date into the Lillian date format. The program processes input date strings, applies a specified date format, and evaluates the results based on feedback codes returned by the API. It also provides detailed feedback on the validity of the input date.

---

## Metadata

| **Attribute**       | **Value**                          |
|----------------------|------------------------------------|
| **File Name**        | CSUTLDTC.cbl                      |
| **Version**          | CardDemo_v1.0-15-g27d6c6f-68      |
| **Last Modified**    | 2022-07-19 23:12:35 CDT           |
| **License**          | Apache License, Version 2.0       |
| **Copyright**        | Amazon.com, Inc. or its affiliates|

---

## Data Structures

### **Working-Storage Section**

The program defines several key data structures to handle input, output, and feedback from the `CEEDAYS` API.

#### **Date Input to CEEDAYS**
| **Field Name**         | **Description**                                                                 |
|-------------------------|---------------------------------------------------------------------------------|
| `WS-DATE-TO-TEST`       | Structure to hold the input date string.                                        |
| `Vstring-length`        | Length of the input date string (binary).                                       |
| `Vstring-text`          | Text of the input date string, with a maximum length of 256 characters.         |

#### **Date Format Input to CEEDAYS**
| **Field Name**         | **Description**                                                                 |
|-------------------------|---------------------------------------------------------------------------------|
| `WS-DATE-FORMAT`        | Structure to hold the input date format string.                                 |
| `Vstring-length`        | Length of the input date format string (binary).                                |
| `Vstring-text`          | Text of the input date format string, with a maximum length of 256 characters.  |

#### **Output from CEEDAYS**
| **Field Name**         | **Description**                                                                 |
|-------------------------|---------------------------------------------------------------------------------|
| `OUTPUT-LILLIAN`        | Lillian date format output (binary).                                            |

#### **Message Structure**
| **Field Name**         | **Description**                                                                 |
|-------------------------|---------------------------------------------------------------------------------|
| `WS-MESSAGE`            | Structure to hold the result message.                                           |
| `WS-SEVERITY`           | Severity code of the feedback.                                                  |
| `WS-SEVERITY-N`         | Numeric representation of the severity code.                                    |
| `WS-MSG-NO`             | Message number of the feedback.                                                 |
| `WS-MSG-NO-N`           | Numeric representation of the message number.                                   |
| `WS-RESULT`             | Result message text (15 characters).                                            |
| `WS-DATE`               | Input date string.                                                              |
| `WS-DATE-FMT`           | Input date format string.                                                       |

#### **Feedback Code**
| **Field Name**         | **Description**                                                                 |
|-------------------------|---------------------------------------------------------------------------------|
| `FEEDBACK-CODE`         | Structure to hold feedback from the `CEEDAYS` API.                              |
| `FEEDBACK-TOKEN-VALUE`  | Token values for specific feedback conditions.                                  |
| `CASE-1-CONDITION-ID`   | Severity and message number for feedback.                                       |
| `CASE-2-CONDITION-ID`   | Class and cause codes for feedback.                                             |
| `CASE-SEV-CTL`          | Severity control.                                                               |
| `FACILITY-ID`           | Facility identifier.                                                            |
| `I-S-INFO`              | Additional feedback information.                                                |

### **Linkage Section**

| **Field Name**         | **Description**                                                                 |
|-------------------------|---------------------------------------------------------------------------------|
| `LS-DATE`              | Input date string passed to the program.                                        |
| `LS-DATE-FORMAT`       | Input date format string passed to the program.                                 |
| `LS-RESULT`            | Result message returned by the program.                                         |

---

## Logic

### **Procedure Division**

The program's logic is implemented in the `PROCEDURE DIVISION`, which includes the following key steps:

1. **Initialization**:
   - Clears the `WS-MESSAGE` structure.
   - Sets `WS-DATE` to spaces.

2. **Main Processing**:
   - Transfers the input date and format from the `LINKAGE SECTION` to the `WORKING-STORAGE SECTION`.
   - Calls the `CEEDAYS` API with the input date, format, and feedback structures.
   - Processes the feedback code to determine the validity of the input date.

3. **Feedback Evaluation**:
   - Uses the `EVALUATE` statement to map specific feedback codes to result messages.
   - Updates the `WS-RESULT` field with the appropriate message.

4. **Output**:
   - Moves the result message and severity code to the `LINKAGE SECTION` for return to the caller.

### **Key Subroutines**

#### **A000-MAIN**
Handles the main processing logic, including:
- Preparing input for the `CEEDAYS` API.
- Calling the API.
- Processing feedback and generating result messages.

#### **A000-MAIN-EXIT**
Marks the end of the main processing logic.

---

## Feedback Code Mapping

The program defines specific feedback codes to handle various error conditions returned by the `CEEDAYS` API.

| **Feedback Code**         | **Condition**                | **Result Message**       |
|----------------------------|------------------------------|---------------------------|
| `FC-INVALID-DATE`          | Valid date                  | `Date is valid`           |
| `FC-INSUFFICIENT-DATA`     | Insufficient data           | `Insufficient`            |
| `FC-BAD-DATE-VALUE`        | Invalid date value          | `Datevalue error`         |
| `FC-INVALID-ERA`           | Invalid era                 | `Invalid Era`             |
| `FC-UNSUPP-RANGE`          | Unsupported range           | `Unsupp. Range`           |
| `FC-INVALID-MONTH`         | Invalid month               | `Invalid month`           |
| `FC-BAD-PIC-STRING`        | Bad picture string          | `Bad Pic String`          |
| `FC-NON-NUMERIC-DATA`      | Non-numeric data            | `Nonnumeric data`         |
| `FC-YEAR-IN-ERA-ZERO`      | Year in era is zero         | `YearInEra is 0`          |
| `OTHER`                    | Unhandled condition         | `Date is invalid`         |

---

## Insights

1. **API Integration**:
   - The program integrates with the `CEEDAYS` API to validate and convert dates. This demonstrates the use of external APIs in COBOL for specialized functionality.

2. **Dynamic String Handling**:
   - The use of `OCCURS ... DEPENDING ON` allows for dynamic string handling, which is essential for processing variable-length input.

3. **Error Handling**:
   - The program provides comprehensive error handling by mapping feedback codes to specific result messages.

4. **Binary Data Usage**:
   - Binary fields are used for performance optimization in handling numeric data, such as lengths and feedback codes.

5. **Reusability**:
   - The modular structure of the program, with clear separation of input, processing, and output, enhances reusability and maintainability.
