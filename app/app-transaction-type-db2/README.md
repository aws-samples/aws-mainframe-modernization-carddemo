# Transaction Type Management with DB2 - CardDemo Extension

![Version](https://img.shields.io/badge/version-1.0.0-blue.svg)
![License](https://img.shields.io/badge/license-Apache%202.0-green.svg)

## Overview

The Transaction Type Management module is an optional extension for the CardDemo application that demonstrates DB2 integration patterns using embedded static SQL. This module allows administrators to maintain transaction type reference data in DB2 tables, providing a more robust and relational approach to managing transaction metadata while integrating with the existing VSAM-based transaction processing system.

## Table of Contents
- [Features](#features)
- [Components](#components)
- [Installation](#installation)
- [Usage](#usage)
- [Technical Details](#technical-details)
- [Integration with Base Application](#integration-with-base-application)
- [Dependencies](#dependencies)

## Features

This extension provides the following capabilities:

- **Transaction Type Management**: Add, update, or delete transaction types through CICS transactions
- **Batch Management**: Maintain transaction types through batch jobs and extract data for VSAM integration
- **DB2 Integration**: Demonstrates various DB2 integration patterns including:
  - Static embedded SQL with host variables
  - Forward and backward cursor processing
  - Standard CRUD operations (Create, Read, Update, Delete)
  - DB2 precompiler integration in CICS environment
  - Proper error handling with SQLCA

## Components

### Online Components

| Transaction | BMS Map | Program  | Function                        | Notes                                  |
|:------------|:--------|:---------|:--------------------------------|:---------------------------------------|
| CTTU        | COTRTUP | COTRTUPC | Transaction Type add/edit       | Add and update operations using static embedded SQL |
| CTLI        | COTRTLI | COTRTLIC | Transaction Type list/update/delete | Demonstrates forward/backward cursors and delete operations |

### Batch Components

| Job      | Program  | Function                                      |
|:---------|:---------|:----------------------------------------------|
| CREADB21 | DSNTEP4  | Creates CardDemo DB2 database and loads tables|
| TRANEXTR | DSNTIAUL | Extracts latest DB2 data for Transaction types and creates VSAM-compatible files|
| MNTTRDB2 | COBTUPDT | Batch maintenance program for transaction type updates|

### Directory Structure

- **bms/**: BMS map definitions for transaction screens
- **cbl/**: COBOL programs for transaction processing
- **cpy/**: Copybooks for data structures
- **cpy-bms/**: Copybooks for BMS maps
- **csd/**: CICS resource definitions
- **dcl/**: DB2 declarations
- **ddl/**: DB2 table definitions
- **jcl/**: JCL for batch operations
- **ctl/**: Control files for DB2 operations

## Installation

### Prerequisites
- Base CardDemo application installed and operational
- DB2 subsystem configured and accessible
- CICS with DB2 support

### Installation Steps

1. **Create DB2 Objects**
   - Execute the CREADB21 job to create the necessary DB2 tables

2. **Compile Programs**
   - Compile the COBOL programs with DB2 precompiler support

3. **Define CICS Resources**
   - Use the provided CSD definitions to add the transactions to your CICS region:
     ```
     DEFINE TRANSACTION(CTTU) GROUP(CARDDEMO) PROGRAM(COTRTUPC)
     DEFINE TRANSACTION(CTLI) GROUP(CARDDEMO) PROGRAM(COTRTLIC)
     ```

4. **Extract Initial Data**
   - Run the TRANEXTR job to extract transaction type data from DB2

5. **Enable Admin Menu Options**
   - The Transaction Type Management options (5 and 6) will now be available in the Admin Menu (CA00)
   - Option 5: Transaction Type list/update/delete (CTLI)
   - Option 6: Transaction Type add/edit (CTTU)

## Usage

### Online Transaction Type Management

1. Log in to CardDemo using an admin account to access the Admin Menu (e.g., ADMIN001/PASSWORD)
2. Select option '5' to list/update/delete transaction types (CTLI)
   - Browse transaction types using forward/backward paging
   - Update transaction descriptions inline
   - Delete transaction types (with referential integrity checking)
3. Select option '6' to add/edit transaction types (CTTU)
   - Add new transaction types
   - Edit existing transaction type descriptions

### Batch Transaction Type Management

Execute the MNTTRDB2 job to perform batch maintenance of transaction types. This job uses the COBTUPDT program to update transaction type information in batch mode.

## Technical Details

### DB2 Tables

The extension uses the following DB2 tables:

- **CARDDEMO.TRANSACTION_TYPE**: Stores transaction type information
  - TR_TYPE (CHAR(2)): Transaction type code (Primary Key)
  - TR_DESCRIPTION (VARCHAR(50)): Transaction description

- **CARDDEMO.TRANSACTION_TYPE_CATEGORY**: Stores transaction category information
  - TRC_TYPE_CODE (CHAR(2)): Transaction type code (Primary Key, Foreign Key to TRANSACTION_TYPE.TR_TYPE)
  - TRC_TYPE_CATEGORY (CHAR(4)): Transaction category code (Primary Key)
  - TRC_CAT_DATA (VARCHAR(50)): Category description
  - Foreign Key: TRC_TYPE_CODE references TRANSACTION_TYPE.TR_TYPE with DELETE RESTRICT

### Integration Patterns

This extension demonstrates several DB2 integration patterns:

1. **Static Embedded SQL**: All programs use static SQL statements embedded in COBOL code with host variables for parameter passing
2. **Cursor Processing**: The CTLI transaction demonstrates how to use DB2 cursors to navigate through result sets with both forward and backward paging capabilities
3. **CRUD Operations**: Complete Create, Read, Update, Delete operations using embedded SQL
4. **Transaction Management**: Shows proper commit/rollback handling in a CICS environment with SQLCA error checking
5. **DB2 Precompiler Integration**: Demonstrates the use of DB2 precompiler in a CICS environment
6. **Host Variable Usage**: Proper use of COBOL host variables for data exchange between COBOL and DB2

## Integration with Base Application

This module integrates with the base CardDemo application in the following ways:

1. **Data Synchronization**: The TRANEXTR job extracts transaction type data from DB2 and creates VSAM-compatible files that can be used by the base application's transaction processing
2. **Admin Menu Integration**: Adds options 5 and 6 to the Admin Menu (CA00) for transaction type management
3. **Reference Data Management**: Provides a centralized way to manage transaction type reference data that feeds into the VSAM-based transaction processing system
4. **Dual Storage Strategy**: Maintains transaction types in both DB2 (for administrative functions) and VSAM (for high-performance transaction processing)

## Dependencies

- Base CardDemo application
- DB2 subsystem
- CICS with DB2 support

