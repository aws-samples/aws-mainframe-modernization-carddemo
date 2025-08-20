# CardDemo Features

A comprehensive overview of the CardDemo mainframe application features, showcasing COBOL, CICS, and mainframe technologies.

## Table of Contents

- [Core Business Processes](#core-business-processes)
- [Core Business Functions](#core-business-functions)
- [Integration Points](#integration-points)
- [System Components](#system-components)

---

## Core Business Processes

### Authorization Workflow

**Category:** Core Business Processes

Processes credit card authorizations, retrieves customer data, and logs transactions in DB2 tables. Includes fraud detection and batch purging of expired authorizations.

### Transaction Type Management

**Category:** Core Business Processes

Provides CRUD operations for transaction types via CICS transactions and batch jobs, demonstrating DB2 integration patterns like cursors and SQL operations.

### User Management

**Category:** Core Business Processes

Allows admin users to add, update, and delete user accounts.

### Error Handling and Recovery

**Category:** Core Business Processes

Implements error handling mechanisms for unexpected conditions, including ABEND routines and message logging.

### Pending Authorization Management

**Category:** Core Business Processes

Handles pending credit card authorizations, including navigation through authorization lists, viewing details, and marking transactions as fraudulent.

---

## Core Business Functions

### Reporting and Analytics

**Category:** Core Business Functions

Generates detailed transaction reports, including daily summaries and account-level totals.

### Security and Compliance

**Category:** Core Business Functions

Implements RACF for access control and ensures compliance with financial regulations through data integrity and audit trails.

### Fraud Detection and Reporting

**Category:** Core Business Functions

Implements fraud detection capabilities by analyzing transaction data and marking suspicious activities. Integrates with DB2 for fraud analytics.

### Bill Payment Processing

**Category:** Core Business Functions

Facilitates bill payments by capturing user inputs, validating account details, and processing payments through batch and online workflows.

---

## Integration Points

### Account Data Extraction

**Category:** Integration Points

Extracts and transmits account data through MQ channels, showcasing asynchronous processing patterns.

### Data Synchronization

**Category:** Integration Points

Synchronizes transaction type data between DB2 and VSAM for dual storage and high-performance processing.

### Database Integration

**Category:** Integration Points

Integrates with IMS DB for hierarchical data, DB2 for relational data, and VSAM for high-performance file storage.

### Messaging Integration

**Category:** Integration Points

Uses MQ for asynchronous communication between systems, including system date and account inquiries.

### IMS DB Hierarchical Data Management

**Category:** Integration Points

Manages hierarchical data structures for authorization and transaction details using IMS DB.

---

## System Components

### Screen Layouts and User Interface

**Category:** System Components

Defines BMS maps for user interface screens, including field attributes and positions for account, transaction, and authorization workflows.

### Batch Processing

**Category:** System Components

Executes batch jobs for data initialization, synchronization, and reporting using JCL.

### Advanced Data Modeling

**Category:** System Components

Utilizes complex COBOL copybook structures, including REDEFINES and OCCURS, for data modeling and processing.

### CICS Resource Configuration

**Category:** System Components

Defines and manages CICS resources, including transactions, programs, and mapsets, to support application workflows.

### JCL Utilities

**Category:** System Components

Provides JCL scripts for tasks like file initialization, database setup, and batch processing.

---

## Technology Stack

### Mainframe Technologies

- **COBOL** - Primary programming language
- **CICS** - Transaction processing system
- **BMS** - Basic Mapping Support for screen layouts
- **JCL** - Job Control Language for batch processing
- **VSAM** - Virtual Storage Access Method for file storage

### Database Technologies

- **DB2** - Relational database management system
- **IMS DB** - Hierarchical database management system
- **VSAM** - High-performance file storage

### Integration Technologies

- **MQ (Message Queuing)** - Asynchronous messaging
- **RACF** - Security and access control

### Development Tools

- **BMS Macros** - Screen definition and mapping
- **COBOL Copybooks** - Data structure definitions
- **CICS Commands** - Transaction and resource management

---

## Application Architecture

The CardDemo application demonstrates a complete mainframe credit card processing system with:

- **Online Transaction Processing** via CICS
- **Batch Processing** via JCL jobs
- **Database Integration** with multiple database types
- **Security Implementation** with RACF
- **Screen Management** with BMS maps
- **Data Synchronization** between different storage systems
- **Reporting and Analytics** capabilities
- **Error Handling and Recovery** mechanisms

This comprehensive system showcases real-world mainframe application patterns and best practices for financial systems.
