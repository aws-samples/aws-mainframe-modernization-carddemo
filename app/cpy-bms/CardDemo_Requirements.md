# CardDemo Requirements Document

A comprehensive mainframe-based credit card management system designed to simulate real-world financial operations and showcase mainframe migration and modernization scenarios.

---

## Table of Contents

- [Executive Overview](#executive-overview)
- [Purpose and Primary Functions](#purpose-and-primary-functions)
- [Business Domain and Industry Context](#business-domain-and-industry-context)
- [Key Capabilities and Workflows](#key-capabilities-and-workflows)
- [High-Level Technical Architecture](#high-level-technical-architecture)
- [Application Features](#application-features)

---

## Executive Overview

**CardDemo** is a comprehensive mainframe-based credit card management system designed to simulate real-world financial operations. It serves as a testing and demonstration platform for mainframe migration and modernization scenarios, showcasing integration with AWS and partner technologies.

The application supports both online and batch processing for managing accounts, transactions, credit cards, and authorizations, providing a robust framework for simulating and modernizing mainframe applications.

---

## Purpose and Primary Functions

### Core Business Functions

The application provides comprehensive credit card management capabilities including:

- **Credit card authorization processing** with fraud detection and transaction validation
- **Account and customer data management** including updates and inquiries
- **Transaction type management** enabling administrators to add, update, and delete transaction types
- **Bill payment processing** and transaction reporting
- **Integration** with hierarchical and relational databases (IMS DB and DB2) and message queuing systems (MQ)

### Detailed Description

CardDemo serves as a testing and demonstration platform for mainframe migration and modernization scenarios, showcasing integration with AWS and partner technologies. The application supports both online and batch processing for managing accounts, transactions, credit cards, and authorizations.

---

## Business Domain and Industry Context

### Financial Services Industry

The application is tailored for the financial services industry, specifically credit card management, addressing:

- **High-performance transaction processing** requirements
- **Secure data handling** and protection
- **Robust integration** with legacy systems
- **Scalability** for enterprise-level operations

### Regulatory and Compliance Considerations

The system ensures compliance with financial industry standards through:

- **RACF (Resource Access Control Facility)** for access control and security
- **Data integrity** and audit trail maintenance
- **Referential integrity** and validation rules for database operations
- **Compliance** with financial regulations through accurate transaction records

---

## Key Capabilities and Workflows

### Major System Capabilities

#### Authorization Management

Processes credit card authorizations, retrieves customer data, and logs transactions in DB2 tables. Includes fraud detection and batch purging of expired authorizations.

#### Transaction Type Management

Provides CRUD operations for transaction types via CICS transactions and batch jobs, demonstrating DB2 integration patterns like cursors and SQL operations.

#### Account Data Extraction

Extracts and transmits account data through MQ channels, showcasing asynchronous processing patterns.

#### Reporting and Analytics

Generates detailed transaction reports, including daily summaries and account-level totals.

### Core Business Processes and Workflows

#### User Management

Admin users can add, update, and delete user accounts with proper access controls.

#### Transaction Processing

Handles transaction submissions, inquiries, and updates through both online and batch workflows.

#### Authorization Workflow

Includes summary and detail screens for authorization requests, fraud marking, and integration with IMS DB and DB2.

#### Data Synchronization

Synchronizes transaction type data between DB2 and VSAM for dual storage and high-performance processing.

### Integration Points with Other Systems

#### Database Integration

- **IMS DB**: Hierarchical database for storing authorization data
- **DB2**: Relational database for transaction type and fraud tracking
- **VSAM**: High-performance file storage for transaction data

#### Messaging Integration

- **MQ**: Message queuing for asynchronous data exchange between systems

---

## High-Level Technical Architecture

### Technology Stack and Frameworks

#### Programming Languages

- **COBOL**: Primary language for core application logic
- **Assembler**: For system-level operations and BMS map definitions

#### Transaction Processing

- **CICS**: Customer Information Control System for online transaction management
- **BMS**: Basic Mapping Support for screen layouts and user interface

#### Data Storage

- **IMS DB**: Hierarchical database management system
- **DB2**: Relational database management system
- **VSAM**: Virtual Storage Access Method for high-performance file storage

#### Batch Processing

- **JCL**: Job Control Language for executing batch jobs and data processing

#### Security

- **RACF**: Resource Access Control Facility for access control and security

### System Components and Their Relationships

#### Online Components

CICS transactions for user interactions, including:

- Account inquiries and updates
- Transaction processing
- Authorization processing
- User management screens

#### Batch Components

JCL jobs for:

- Data initialization and setup
- Data synchronization between systems
- Reporting and analytics generation
- System maintenance tasks

#### Database Integration

- **DB2 tables**: Transaction type and fraud tracking
- **IMS DB**: Hierarchical authorization data storage
- **VSAM files**: High-performance transaction data storage

#### Messaging Infrastructure

- **MQ queues**: Asynchronous communication between systems
- **Message routing**: System-to-system data exchange

#### Screen Management

- **BMS maps**: User interface screen definitions
- **Field attributes**: Input/output field specifications
- **Screen layouts**: Position and formatting definitions

---

## Application Features

### Core Business Processes

1. **Authorization Workflow**

   - Credit card authorization processing
   - Fraud detection and validation
   - Customer data retrieval
   - Transaction logging in DB2

2. **Transaction Type Management**

   - CRUD operations for transaction types
   - CICS transaction integration
   - DB2 cursor operations
   - Batch job processing

3. **User Management**

   - Admin user account management
   - Access control and permissions
   - User authentication and authorization

4. **Error Handling and Recovery**

   - ABEND routine implementation
   - Message logging and tracking
   - System recovery procedures

5. **Pending Authorization Management**
   - Authorization list navigation
   - Transaction detail viewing
   - Fraud marking capabilities

### Core Business Functions

1. **Reporting and Analytics**

   - Detailed transaction reports
   - Daily summaries
   - Account-level totals
   - Performance metrics

2. **Security and Compliance**

   - RACF access control
   - Financial regulation compliance
   - Data integrity maintenance
   - Audit trail generation

3. **Fraud Detection and Reporting**

   - Transaction analysis
   - Suspicious activity marking
   - DB2 fraud analytics integration
   - Fraud reporting capabilities

4. **Bill Payment Processing**
   - User input capture
   - Account validation
   - Payment processing workflows
   - Batch and online processing

### Integration Points

1. **Account Data Extraction**

   - MQ channel integration
   - Asynchronous processing patterns
   - Data transmission protocols

2. **Data Synchronization**

   - DB2 to VSAM synchronization
   - Dual storage implementation
   - High-performance processing

3. **Database Integration**

   - Multi-database architecture
   - Hierarchical and relational data
   - High-performance file storage

4. **Messaging Integration**

   - MQ system communication
   - Asynchronous data exchange
   - System date and account inquiries

5. **IMS DB Hierarchical Data Management**
   - Hierarchical data structures
   - Authorization and transaction details
   - IMS DB integration patterns

### System Components

1. **Screen Layouts and User Interface**

   - BMS map definitions
   - Field attributes and positions
   - Account and transaction workflows
   - Authorization interfaces

2. **Batch Processing**

   - JCL job execution
   - Data initialization
   - Synchronization processes
   - Reporting generation

3. **Advanced Data Modeling**

   - COBOL copybook structures
   - REDEFINES and OCCURS usage
   - Data modeling patterns
   - Processing logic

4. **CICS Resource Configuration**

   - Transaction definitions
   - Program management
   - Mapset configuration
   - Application workflows

5. **JCL Utilities**
   - File initialization scripts
   - Database setup procedures
   - Batch processing jobs
   - System maintenance utilities

---

## Summary

CardDemo provides a robust framework for simulating and modernizing mainframe applications, enabling organizations to:

- **Test migration strategies** for legacy mainframe systems
- **Optimize legacy systems** for contemporary business needs
- **Demonstrate integration** with modern cloud and partner technologies
- **Showcase mainframe capabilities** in financial services applications
- **Provide training and development** environments for mainframe technologies

This comprehensive system showcases real-world mainframe application patterns and best practices for financial systems, making it an invaluable tool for organizations looking to understand, maintain, or modernize their mainframe infrastructure.

