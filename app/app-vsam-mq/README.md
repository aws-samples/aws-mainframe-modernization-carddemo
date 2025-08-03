# Account Extractions using MQ and VSAM - CardDemo Extension

![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)
![License](https://img.shields.io/badge/license-Apache%202.0-green.svg)

## Overview

The Account Extractions module is an optional extension for the CardDemo application that demonstrates integration between VSAM and IBM MQ. This module enables the extraction and transmission of account data through MQ channels, showcasing asynchronous processing patterns commonly used in mainframe environments.

## Table of Contents
- [Features](#features)
- [Components](#components)
- [Installation](#installation)
- [Usage](#usage)
- [Technical Details](#technical-details)
- [Dependencies](#dependencies)

## Features

This extension provides the following capabilities:

- **System Date Inquiry via MQ**: Query the system date through an MQ request/response pattern (CDRD transaction)
- **Account Details Inquiry via MQ**: Retrieve account information through MQ channels (CDRA transaction)
- **Asynchronous Processing**: Demonstrates asynchronous communication patterns between systems
- **MQ Integration**: Shows how to integrate MQ with existing VSAM-based applications

## Components

### Online Components

| Transaction | Program  | Function                        | Notes                                  |
|:------------|:---------|:--------------------------------|:---------------------------------------|
| CDRD        | CODATE01 | Inquire System Date via MQ      | Demonstrates MQ request/response pattern |
| CDRA        | COACCT01 | Inquire account details via MQ  | Demonstrates MQ request/response pattern |

### Directory Structure

- **cbl/**: COBOL programs for MQ integration
- **csd/**: CICS resource definitions for MQ transactions

## Installation

### Prerequisites
- Base CardDemo application installed and operational
- IBM MQ configured and accessible from CICS
- CICS with MQ support

### Installation Steps

1. **Configure MQ Resources**
   - Create the necessary MQ queues:
     ```
     DEFINE QLOCAL('CARDDEMO.REQUEST.QUEUE') REPLACE
     DEFINE QLOCAL('CARDDEMO.RESPONSE.QUEUE') REPLACE
     ```

2. **Compile Programs**
   - Compile the COBOL programs with MQ support

3. **Define CICS Resources**
   - Use the provided CSD definitions to add the transactions to your CICS region:
     ```
     DEFINE TRANSACTION(CDRD) GROUP(CARDDEMO) PROGRAM(CODATE01)
     DEFINE TRANSACTION(CDRA) GROUP(CARDDEMO) PROGRAM(COACCT01)
     ```

4. **Configure CICS-MQ Connection**
   - Define the necessary CICS-MQ connection resources:
     ```
     DEFINE MQCONN(MQ01) GROUP(CARDDEMO)
     DEFINE MQQUEUE(CARDREQ) GROUP(CARDDEMO) QNAME(CARDDEMO.REQUEST.QUEUE)
     DEFINE MQQUEUE(CARDRES) GROUP(CARDDEMO) QNAME(CARDDEMO.RESPONSE.QUEUE)
     ```

## Usage

### System Date Inquiry (CDRD)

The CDRD transaction demonstrates a simple MQ request/response pattern to retrieve the system date:

1. Execute the CDRD transaction in CICS
2. The transaction sends a request message to the request queue
3. A listener program processes the request and sends the system date to the response queue
4. The CDRD transaction retrieves and displays the response

### Account Details Inquiry (CDRA)

The CDRA transaction demonstrates how to retrieve account information via MQ:

1. Execute the CDRA transaction in CICS with an account number
2. The transaction sends a request message containing the account number to the request queue
3. A listener program retrieves the account details from VSAM and sends them to the response queue
4. The CDRA transaction retrieves and displays the account information

## Technical Details

### Message Formats

**Date Request Message Format:**
```
01 DATE-REQUEST-MSG.
   05 REQUEST-TYPE        PIC X(4) VALUE 'DATE'.
   05 REQUEST-ID          PIC X(8).
```

**Date Response Message Format:**
```
01 DATE-RESPONSE-MSG.
   05 RESPONSE-TYPE       PIC X(4) VALUE 'DATE'.
   05 RESPONSE-ID         PIC X(8).
   05 SYSTEM-DATE         PIC X(10).
```

**Account Request Message Format:**
```
01 ACCT-REQUEST-MSG.
   05 REQUEST-TYPE        PIC X(4) VALUE 'ACCT'.
   05 REQUEST-ID          PIC X(8).
   05 ACCOUNT-NUMBER      PIC X(11).
```

**Account Response Message Format:**
```
01 ACCT-RESPONSE-MSG.
   05 RESPONSE-TYPE       PIC X(4) VALUE 'ACCT'.
   05 RESPONSE-ID         PIC X(8).
   05 ACCOUNT-DATA        PIC X(300).
```

### Integration Patterns

This extension demonstrates several MQ integration patterns:

1. **Request/Response**: Shows how to implement a synchronous request/response pattern using asynchronous MQ
2. **Message Correlation**: Demonstrates how to correlate request and response messages
3. **Data Extraction**: Shows how to extract data from VSAM files for transmission via MQ
4. **Error Handling**: Includes proper error handling for MQ operations

## Dependencies

- Base CardDemo application
- IBM MQ
- CICS with MQ support

