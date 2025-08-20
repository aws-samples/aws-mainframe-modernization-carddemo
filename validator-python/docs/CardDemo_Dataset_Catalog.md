# CardDemo Dataset Catalog

**Generated:** September 1, 2022 at 15:36:44  
**System:** IDCAMS System Services  
**Application:** AWS.M2.CARDDEMO

---

## Table of Contents

- [Executive Summary](#executive-summary)
- [Core Business Data Files](#core-business-data-files)
- [VSAM Data Stores](#vsam-data-stores)
- [Backup and Recovery Files](#backup-and-recovery-files)
- [Application Libraries](#application-libraries)
- [Reporting and Output Files](#reporting-and-output-files)
- [System Configuration Files](#system-configuration-files)
- [Dataset Statistics Summary](#dataset-statistics-summary)

---

## Executive Summary

The CardDemo application contains **50+ datasets** organized across multiple categories:

- **Core Business Data**: Account, customer, card, and transaction information
- **VSAM Data Stores**: High-performance indexed data structures
- **Backup Systems**: Generation Data Groups (GDG) for data protection
- **Application Libraries**: COBOL programs, copybooks, and load modules
- **Reporting Outputs**: Transaction reports and daily summaries

**Total Storage**: Multiple volumes across various storage classes  
**Security**: RACF-protected with encryption capabilities  
**Backup Strategy**: Automated daily backups with retention policies

---

## Core Business Data Files

### Account Management

| Dataset                              | Type       | Records | Purpose                  | Last Updated |
| ------------------------------------ | ---------- | ------- | ------------------------ | ------------ |
| `AWS.M2.CARDDEMO.ACCTDATA.PS`        | Sequential | -       | Account data input file  | 2022.136     |
| `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS` | VSAM KSDS  | 50      | Primary account database | 2022.242     |

### Customer Management

| Dataset                              | Type       | Records | Purpose                   | Last Updated |
| ------------------------------------ | ---------- | ------- | ------------------------- | ------------ |
| `AWS.M2.CARDDEMO.CUSTDATA.PS`        | Sequential | -       | Customer data input file  | 2022.136     |
| `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS` | VSAM KSDS  | 50      | Primary customer database | 2022.242     |

### Credit Card Management

| Dataset                              | Type       | Records | Purpose                              | Last Updated |
| ------------------------------------ | ---------- | ------- | ------------------------------------ | ------------ |
| `AWS.M2.CARDDEMO.CARDDATA.PS`        | Sequential | -       | Card data input file                 | 2022.136     |
| `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS` | VSAM KSDS  | 50      | Primary card database                | 2022.242     |
| `AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX`  | VSAM AIX   | 50      | Alternate index for card lookups     | 2022.242     |
| `AWS.M2.CARDDEMO.CARDXREF.PS`        | Sequential | -       | Card cross-reference input           | 2022.136     |
| `AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS` | VSAM KSDS  | 50      | Card cross-reference database        | 2022.242     |
| `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX`  | VSAM AIX   | 50      | Alternate index for cross-references | 2022.242     |

### Transaction Processing

| Dataset                            | Type       | Records | Purpose                  | Last Updated |
| ---------------------------------- | ---------- | ------- | ------------------------ | ------------ |
| `AWS.M2.CARDDEMO.DALYTRAN.PS`      | Sequential | -       | Daily transaction input  | 2022.153     |
| `AWS.M2.CARDDEMO.DALYTRAN.PS.INIT` | Sequential | -       | Initial transaction data | 2022.189     |

### Reference Data

| Dataset                              | Type       | Records | Purpose                       | Last Updated |
| ------------------------------------ | ---------- | ------- | ----------------------------- | ------------ |
| `AWS.M2.CARDDEMO.DISCGRP.PS`         | Sequential | -       | Discount group input          | 2022.136     |
| `AWS.M2.CARDDEMO.DISCGRP.VSAM.KSDS`  | VSAM KSDS  | 51      | Discount group database       | 2022.242     |
| `AWS.M2.CARDDEMO.TRANCATG.PS`        | Sequential | -       | Transaction category input    | 2022.149     |
| `AWS.M2.CARDDEMO.TRANCATG.VSAM.KSDS` | VSAM KSDS  | 18      | Transaction category database | 2022.201     |

---

## VSAM Data Stores

### Key-Sequenced Data Sets (KSDS)

VSAM KSDS files provide high-performance indexed access to business data:

#### Account Data VSAM

- **Cluster**: `AWS.M2.CARDDEMO.ACCTDATA.VSAM.KSDS`
- **Key Length**: 11 bytes
- **Record Length**: 300 bytes average
- **Statistics**: 50 total records, 311 updates, 350 retrievals
- **Storage**: 1 cylinder primary, 5 cylinders secondary

#### Customer Data VSAM

- **Cluster**: `AWS.M2.CARDDEMO.CUSTDATA.VSAM.KSDS`
- **Key Length**: 9 bytes
- **Record Length**: 500 bytes average
- **Statistics**: 50 total records, 0 updates, 0 retrievals
- **Storage**: 1 cylinder primary, 5 cylinders secondary

#### Card Data VSAM

- **Cluster**: `AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS`
- **Key Length**: 16 bytes
- **Record Length**: 150 bytes average
- **Statistics**: 50 total records, 0 updates, 50 retrievals
- **Storage**: 1 cylinder primary, 5 cylinders secondary

### Alternate Indexes (AIX)

Provide alternative access paths to VSAM data:

#### Card Data AIX

- **AIX**: `AWS.M2.CARDDEMO.CARDDATA.VSAM.AIX`
- **Key Length**: 11 bytes (different from primary key)
- **Purpose**: Alternative lookup by customer account number
- **Statistics**: 50 total records, 0 retrievals

#### Card Cross-Reference AIX

- **AIX**: `AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX`
- **Key Length**: 11 bytes
- **Purpose**: Alternative lookup for card-to-account mapping
- **Statistics**: 50 total records, 50 retrievals

---

## Backup and Recovery Files

### Generation Data Groups (GDG)

#### Daily Rejections Backup

- **GDG Base**: `AWS.M2.CARDDEMO.DALYREJS`
- **Limit**: 5 generations
- **Current Generations**: G0022V00 through G0026V00
- **Purpose**: Backup of rejected daily transactions
- **Retention**: LIFO (Last In, First Out), no purge

#### System Transactions Backup

- **GDG Base**: `AWS.M2.CARDDEMO.SYSTRAN`
- **Limit**: 5 generations
- **Current Generations**: G0018V00 through G0022V00
- **Purpose**: Backup of system transaction data
- **Retention**: LIFO, no purge

#### Transaction Backup

- **GDG Base**: `AWS.M2.CARDDEMO.TRANSACT.BKUP`
- **Limit**: 5 generations
- **Current Generations**: G0072V00 through G0076V00
- **Purpose**: Backup of transaction data
- **Retention**: LIFO, no purge, no scratch

#### Transaction Category Balance Backup

- **GDG Base**: `AWS.M2.CARDDEMO.TCATBALF.BKUP`
- **Limit**: 5 generations
- **Current Generations**: G0005V00 through G0009V00
- **Purpose**: Backup of transaction category balances
- **Retention**: LIFO, no purge

#### Transaction Reports Backup

- **GDG Base**: `AWS.M2.CARDDEMO.TRANREPT`
- **Limit**: 5 generations
- **Current Generations**: G0010V00 through G0014V00
- **Purpose**: Backup of transaction reports
- **Retention**: LIFO, no purge

---

## Application Libraries

### Source Code Libraries

| Dataset               | Type       | Purpose               | Last Updated |
| --------------------- | ---------- | --------------------- | ------------ |
| `AWS.M2.CARDDEMO.CBL` | Sequential | COBOL source programs | 2022.035     |
| `AWS.M2.CARDDEMO.CPY` | Sequential | COBOL copybooks       | 2022.046     |
| `AWS.M2.CARDDEMO.BMS` | Sequential | BMS map definitions   | 2022.046     |
| `AWS.M2.CARDDEMO.DCL` | Sequential | Data control language | 2022.046     |

### Control and Configuration

| Dataset                    | Type       | Purpose                      | Last Updated |
| -------------------------- | ---------- | ---------------------------- | ------------ |
| `AWS.M2.CARDDEMO.CNTL`     | Sequential | Control cards and parameters | 2022.154     |
| `AWS.M2.CARDDEMO.JCL`      | Sequential | Job Control Language scripts | 2022.046     |
| `AWS.M2.CARDDEMO.JCL.UTIL` | Sequential | JCL utility scripts          | 2022.046     |
| `AWS.M2.CARDDEMO.PROC`     | Sequential | JCL procedures               | 2022.154     |
| `AWS.M2.CARDDEMO.PRC.UTIL` | Sequential | Procedure utilities          | 2022.195     |

### Load Libraries

| Dataset                   | Type       | Purpose                       | Last Updated |
| ------------------------- | ---------- | ----------------------------- | ------------ |
| `AWS.M2.CARDDEMO.LOADLIB` | Library    | Compiled program load modules | 2022.046     |
| `AWS.M2.CARDDEMO.LISTING` | Library    | Compilation listings          | 2022.137     |
| `AWS.M2.CARDDEMO.LST`     | Sequential | List files                    | 2022.035     |

### Utilities and Tools

| Dataset                     | Type       | Purpose                      | Last Updated |
| --------------------------- | ---------- | ---------------------------- | ------------ |
| `AWS.M2.CARDDEMO.REXX.UTIL` | Sequential | REXX utility scripts         | 2022.047     |
| `AWS.M2.CARDDEMO.BIND`      | Sequential | Database binding information | 2022.046     |

---

## Reporting and Output Files

### Transaction Reports

- **GDG Base**: `AWS.M2.CARDDEMO.TRANREPT`
- **Purpose**: Daily transaction summaries and reports
- **Format**: Sequential files with report data
- **Retention**: 5 generations maintained

### Transaction Category Balances

| Dataset                              | Type       | Records | Purpose               | Last Updated |
| ------------------------------------ | ---------- | ------- | --------------------- | ------------ |
| `AWS.M2.CARDDEMO.TCATBALF.PS`        | Sequential | -       | Balance input file    | 2022.136     |
| `AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS` | VSAM KSDS  | 100     | Balance database      | 2022.242     |
| `AWS.M2.CARDDEMO.TCATBALF.REPT`      | Sequential | -       | Balance report output | 2022.189     |

**TCATBALF Statistics:**

- **Total Records**: 100
- **Inserted**: 49 records
- **Updated**: 212 records
- **Retrieved**: 312 records
- **Key Length**: 17 bytes
- **Record Length**: 50 bytes average

---

## System Configuration Files

### Security and Parameters

| Dataset                       | Type       | Purpose                | Last Updated |
| ----------------------------- | ---------- | ---------------------- | ------------ |
| `AWS.M2.CARDDEMO.SECURITY.PS` | Sequential | Security configuration | 2022.040     |
| `AWS.M2.CARDDEMO.DATEPARM`    | Sequential | Date parameters        | 2022.187     |
| `AWS.M2.CARDDEMO.LISTCAT`     | Sequential | Catalog listing output | 2022.244     |

---

## Dataset Statistics Summary

### VSAM Performance Metrics

| Dataset  | Total Records | Updates | Retrievals | EXCPs |
| -------- | ------------- | ------- | ---------- | ----- |
| ACCTDATA | 50            | 311     | 350        | 315   |
| CARDDATA | 50            | 0       | 50         | 3     |
| CUSTDATA | 50            | 0       | 0          | 2     |
| CARDXREF | 50            | 0       | 400        | 54    |
| DISCGRP  | 51            | 0       | 100        | 3     |
| TCATBALF | 100           | 212     | 312        | 266   |
| TRANCATG | 18            | 0       | 0          | 2     |

### Storage Allocation Summary

- **Primary Storage**: 1 cylinder per dataset
- **Secondary Storage**: 5 cylinders per dataset
- **Index Storage**: 1 track per index
- **Total Volumes**: Multiple volumes across YYYY series
- **Storage Class**: SCTECH (Standard Technology)

### Security and Protection

- **RACF Protection**: Not enabled (NO)
- **Encryption**: Not enabled (NO)
- **Password Protection**: Not enabled (NULL)
- **Backup Status**: Most datasets have recent backups

---

## Key Insights

1. **Data Volume**: Moderate-sized application with ~400 total records across core business entities
2. **Performance**: High retrieval activity on account and cross-reference data
3. **Backup Strategy**: Comprehensive GDG-based backup system with 5-generation retention
4. **Storage Efficiency**: Standard allocation with room for growth
5. **Security**: Basic protection level suitable for development/demo environment

This catalog provides a complete picture of the CardDemo application's data architecture and can be used for:

- **Migration Planning**: Understanding data dependencies
- **Capacity Planning**: Storage and performance requirements
- **Backup Verification**: Ensuring data protection
- **System Documentation**: Complete application inventory
