# 07 — Paragraph-Level Traceability Matrix

| Mainframe Program | COBOL Paragraph / Logic Block | Modernized Java Class & Method | Citation Lines | Business Rule | Status |
| :--- | :--- | :--- | :--- | :--- | :--- |
| `CBACT01C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 28-63` | `BR-CBACT01C-001` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1000-ACCTFILE-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 165-200` | `BR-CBACT01C-002` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1100-DISPLAY-ACCT-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 200-235` | `BR-CBACT01C-003` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1300-POPUL-ACCT-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 215-250` | `BR-CBACT01C-004` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1350-WRITE-ACCT-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 242-277` | `BR-CBACT01C-005` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1400-POPUL-ARRAY-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 253-288` | `BR-CBACT01C-006` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1450-WRITE-ARRY-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 263-298` | `BR-CBACT01C-007` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1500-POPUL-VBRC-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 276-311` | `BR-CBACT01C-008` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1550-WRITE-VB1-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 287-322` | `BR-CBACT01C-009` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `1575-WRITE-VB2-RECORD` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 302-337` | `BR-CBACT01C-010` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `0000-ACCTFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 317-352` | `BR-CBACT01C-011` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `2000-OUTFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 334-369` | `BR-CBACT01C-012` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `3000-ARRFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 352-387` | `BR-CBACT01C-013` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `4000-VBRFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 370-405` | `BR-CBACT01C-014` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `9000-ACCTFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 388-423` | `BR-CBACT01C-015` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `9999-ABEND-PROGRAM` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 406-430` | `BR-CBACT01C-016` | `INLINE ORCHESTRATION` |
| `CBACT01C` | `9910-DISPLAY-IO-STATUS` (Execution block) | `com.client.core_financial_processing.service.Cbact01cService#execute()` | `lines 413-430` | `BR-CBACT01C-017` | `INLINE ORCHESTRATION` |
| `CBACT02C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbact02cService#execute()` | `lines 28-63` | `BR-CBACT02C-001` | `INLINE ORCHESTRATION` |
| `CBACT02C` | `1000-CARDFILE-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbact02cService#execute()` | `lines 92-127` | `BR-CBACT02C-002` | `INLINE ORCHESTRATION` |
| `CBACT02C` | `0000-CARDFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact02cService#execute()` | `lines 118-153` | `BR-CBACT02C-003` | `INLINE ORCHESTRATION` |
| `CBACT02C` | `9000-CARDFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact02cService#execute()` | `lines 136-171` | `BR-CBACT02C-004` | `INLINE ORCHESTRATION` |
| `CBACT02C` | `9999-ABEND-PROGRAM` (Execution block) | `com.client.core_financial_processing.service.Cbact02cService#execute()` | `lines 154-178` | `BR-CBACT02C-005` | `INLINE ORCHESTRATION` |
| `CBACT02C` | `9910-DISPLAY-IO-STATUS` (Execution block) | `com.client.core_financial_processing.service.Cbact02cService#execute()` | `lines 161-178` | `BR-CBACT02C-006` | `INLINE ORCHESTRATION` |
| `CBACT03C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbact03cService#execute()` | `lines 28-63` | `BR-CBACT03C-001` | `INLINE ORCHESTRATION` |
| `CBACT03C` | `1000-XREFFILE-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbact03cService#execute()` | `lines 92-127` | `BR-CBACT03C-002` | `INLINE ORCHESTRATION` |
| `CBACT03C` | `0000-XREFFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact03cService#execute()` | `lines 118-153` | `BR-CBACT03C-003` | `INLINE ORCHESTRATION` |
| `CBACT03C` | `9000-XREFFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact03cService#execute()` | `lines 136-171` | `BR-CBACT03C-004` | `INLINE ORCHESTRATION` |
| `CBACT03C` | `9999-ABEND-PROGRAM` (Execution block) | `com.client.core_financial_processing.service.Cbact03cService#execute()` | `lines 154-178` | `BR-CBACT03C-005` | `INLINE ORCHESTRATION` |
| `CBACT03C` | `9910-DISPLAY-IO-STATUS` (Execution block) | `com.client.core_financial_processing.service.Cbact03cService#execute()` | `lines 161-178` | `BR-CBACT03C-006` | `INLINE ORCHESTRATION` |
| `CBACT04C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 27-62` | `BR-CBACT04C-001` | `100% COVERED` |
| `CBACT04C` | `0000-TCATBALF-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 234-269` | `BR-CBACT04C-002` | `100% COVERED` |
| `CBACT04C` | `0100-XREFFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 252-287` | `BR-CBACT04C-003` | `100% COVERED` |
| `CBACT04C` | `0200-DISCGRP-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 270-305` | `BR-CBACT04C-004` | `100% COVERED` |
| `CBACT04C` | `0300-ACCTFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 289-324` | `BR-CBACT04C-005` | `100% COVERED` |
| `CBACT04C` | `0400-TRANFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 307-342` | `BR-CBACT04C-006` | `100% COVERED` |
| `CBACT04C` | `1000-TCATBALF-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 325-360` | `BR-CBACT04C-007` | `100% COVERED` |
| `CBACT04C` | `1050-UPDATE-ACCOUNT` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 350-385` | `BR-CBACT04C-008` | `100% COVERED` |
| `CBACT04C` | `1100-GET-ACCT-DATA` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 372-407` | `BR-CBACT04C-009` | `100% COVERED` |
| `CBACT04C` | `1110-GET-XREF-DATA` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 393-428` | `BR-CBACT04C-010` | `100% COVERED` |
| `CBACT04C` | `1200-GET-INTEREST-RATE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 415-450` | `BR-CBACT04C-011` | `100% COVERED` |
| `CBACT04C` | `1200-A-GET-DEFAULT-INT-RATE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 443-478` | `BR-CBACT04C-012` | `100% COVERED` |
| `CBACT04C` | `1300-COMPUTE-INTEREST` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 462-497` | `BR-CBACT04C-013` | `100% COVERED` |
| `CBACT04C` | `1300-B-WRITE-TX` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 473-508` | `BR-CBACT04C-014` | `100% COVERED` |
| `CBACT04C` | `1400-COMPUTE-FEES` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 518-553` | `BR-CBACT04C-015` | `100% COVERED` |
| `CBACT04C` | `9000-TCATBALF-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 522-557` | `BR-CBACT04C-016` | `100% COVERED` |
| `CBACT04C` | `9100-XREFFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 541-576` | `BR-CBACT04C-017` | `100% COVERED` |
| `CBACT04C` | `9200-DISCGRP-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 559-594` | `BR-CBACT04C-018` | `100% COVERED` |
| `CBACT04C` | `9300-ACCTFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 577-612` | `BR-CBACT04C-019` | `100% COVERED` |
| `CBACT04C` | `9400-TRANFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 595-630` | `BR-CBACT04C-020` | `100% COVERED` |
| `CBACT04C` | `9999-ABEND-PROGRAM` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 628-652` | `BR-CBACT04C-021` | `100% COVERED` |
| `CBACT04C` | `9910-DISPLAY-IO-STATUS` (Execution block) | `com.client.core_financial_processing.service.Cbact04cService#calculateMonthlyInterest()` | `lines 635-652` | `BR-CBACT04C-022` | `100% COVERED` |
| `CBCUS01C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbcus01cService#execute()` | `lines 28-63` | `BR-CBCUS01C-001` | `INLINE ORCHESTRATION` |
| `CBCUS01C` | `1000-CUSTFILE-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbcus01cService#execute()` | `lines 92-127` | `BR-CBCUS01C-002` | `INLINE ORCHESTRATION` |
| `CBCUS01C` | `0000-CUSTFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbcus01cService#execute()` | `lines 118-153` | `BR-CBCUS01C-003` | `INLINE ORCHESTRATION` |
| `CBCUS01C` | `9000-CUSTFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbcus01cService#execute()` | `lines 136-171` | `BR-CBCUS01C-004` | `INLINE ORCHESTRATION` |
| `CBEXPORT` | `FILE-CONTROL` (Execution block) | `N/A (Pending Module Modernization)` | `lines 34-69` | `BR-CBEXPORT-001` | `UNMAPPED` |
| `CBEXPORT` | `0000-MAIN-PROCESSING` (Execution block) | `N/A (Pending Module Modernization)` | `lines 149-184` | `BR-CBEXPORT-002` | `UNMAPPED` |
| `CBEXPORT` | `1000-INITIALIZE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 161-196` | `BR-CBEXPORT-003` | `UNMAPPED` |
| `CBEXPORT` | `1050-GENERATE-TIMESTAMP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 172-207` | `BR-CBEXPORT-004` | `UNMAPPED` |
| `CBEXPORT` | `1100-OPEN-FILES` (Execution block) | `N/A (Pending Module Modernization)` | `lines 198-233` | `BR-CBEXPORT-005` | `UNMAPPED` |
| `CBEXPORT` | `2000-EXPORT-CUSTOMERS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 243-278` | `BR-CBEXPORT-006` | `UNMAPPED` |
| `CBEXPORT` | `2100-READ-CUSTOMER-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 258-293` | `BR-CBEXPORT-007` | `UNMAPPED` |
| `CBEXPORT` | `2200-CREATE-CUSTOMER-EXP-REC` (Execution block) | `N/A (Pending Module Modernization)` | `lines 269-304` | `BR-CBEXPORT-008` | `UNMAPPED` |
| `CBEXPORT` | `3000-EXPORT-ACCOUNTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 312-347` | `BR-CBEXPORT-009` | `UNMAPPED` |
| `CBEXPORT` | `3100-READ-ACCOUNT-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 327-362` | `BR-CBEXPORT-010` | `UNMAPPED` |
| `CBEXPORT` | `3200-CREATE-ACCOUNT-EXP-REC` (Execution block) | `N/A (Pending Module Modernization)` | `lines 338-373` | `BR-CBEXPORT-011` | `UNMAPPED` |
| `CBEXPORT` | `4000-EXPORT-XREFS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 376-411` | `BR-CBEXPORT-012` | `UNMAPPED` |
| `CBEXPORT` | `4100-READ-XREF-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 391-426` | `BR-CBEXPORT-013` | `UNMAPPED` |
| `CBEXPORT` | `4200-CREATE-XREF-EXPORT-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 402-437` | `BR-CBEXPORT-014` | `UNMAPPED` |
| `CBEXPORT` | `5000-EXPORT-TRANSACTIONS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 431-466` | `BR-CBEXPORT-015` | `UNMAPPED` |
| `CBEXPORT` | `5100-READ-TRANSACTION-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 446-481` | `BR-CBEXPORT-016` | `UNMAPPED` |
| `CBEXPORT` | `5200-CREATE-TRAN-EXP-REC` (Execution block) | `N/A (Pending Module Modernization)` | `lines 457-492` | `BR-CBEXPORT-017` | `UNMAPPED` |
| `CBEXPORT` | `5500-EXPORT-CARDS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 496-531` | `BR-CBEXPORT-018` | `UNMAPPED` |
| `CBEXPORT` | `5600-READ-CARD-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 511-546` | `BR-CBEXPORT-019` | `UNMAPPED` |
| `CBEXPORT` | `5700-CREATE-CARD-EXPORT-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 522-557` | `BR-CBEXPORT-020` | `UNMAPPED` |
| `CBEXPORT` | `6000-FINALIZE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 554-582` | `BR-CBEXPORT-021` | `UNMAPPED` |
| `CBEXPORT` | `9999-ABEND-PROGRAM` (Execution block) | `N/A (Pending Module Modernization)` | `lines 576-582` | `BR-CBEXPORT-022` | `UNMAPPED` |
| `CBIMPORT` | `FILE-CONTROL` (Execution block) | `N/A (Pending Module Modernization)` | `lines 36-71` | `BR-CBIMPORT-001` | `UNMAPPED` |
| `CBIMPORT` | `0000-MAIN-PROCESSING` (Execution block) | `N/A (Pending Module Modernization)` | `lines 165-200` | `BR-CBIMPORT-002` | `UNMAPPED` |
| `CBIMPORT` | `1000-INITIALIZE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 174-209` | `BR-CBIMPORT-003` | `UNMAPPED` |
| `CBIMPORT` | `1100-OPEN-FILES` (Execution block) | `N/A (Pending Module Modernization)` | `lines 196-231` | `BR-CBIMPORT-004` | `UNMAPPED` |
| `CBIMPORT` | `2000-PROCESS-EXPORT-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 248-283` | `BR-CBIMPORT-005` | `UNMAPPED` |
| `CBIMPORT` | `2100-READ-EXPORT-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 259-294` | `BR-CBIMPORT-006` | `UNMAPPED` |
| `CBIMPORT` | `2200-PROCESS-RECORD-BY-TYPE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 270-305` | `BR-CBIMPORT-007` | `UNMAPPED` |
| `CBIMPORT` | `2300-PROCESS-CUSTOMER-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 288-323` | `BR-CBIMPORT-008` | `UNMAPPED` |
| `CBIMPORT` | `2400-PROCESS-ACCOUNT-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 323-358` | `BR-CBIMPORT-009` | `UNMAPPED` |
| `CBIMPORT` | `2500-PROCESS-XREF-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 352-387` | `BR-CBIMPORT-010` | `UNMAPPED` |
| `CBIMPORT` | `2600-PROCESS-TRAN-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 372-407` | `BR-CBIMPORT-011` | `UNMAPPED` |
| `CBIMPORT` | `2650-PROCESS-CARD-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 402-437` | `BR-CBIMPORT-012` | `UNMAPPED` |
| `CBIMPORT` | `2700-PROCESS-UNKNOWN-RECORD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 425-460` | `BR-CBIMPORT-013` | `UNMAPPED` |
| `CBIMPORT` | `2750-WRITE-ERROR` (Execution block) | `N/A (Pending Module Modernization)` | `lines 437-472` | `BR-CBIMPORT-014` | `UNMAPPED` |
| `CBIMPORT` | `3000-VALIDATE-IMPORT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 449-484` | `BR-CBIMPORT-015` | `UNMAPPED` |
| `CBIMPORT` | `4000-FINALIZE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 455-487` | `BR-CBIMPORT-016` | `UNMAPPED` |
| `CBIMPORT` | `9999-ABEND-PROGRAM` (Execution block) | `N/A (Pending Module Modernization)` | `lines 481-487` | `BR-CBIMPORT-017` | `UNMAPPED` |
| `CBTRN01C` | `FILE-CONTROL` (Execution block) | `N/A (Pending Module Modernization)` | `lines 28-63` | `BR-CBTRN01C-001` | `UNMAPPED` |
| `CBTRN01C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 155-190` | `BR-CBTRN01C-002` | `UNMAPPED` |
| `CBTRN01C` | `1000-DALYTRAN-GET-NEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 202-237` | `BR-CBTRN01C-003` | `UNMAPPED` |
| `CBTRN01C` | `2000-LOOKUP-XREF` (Execution block) | `N/A (Pending Module Modernization)` | `lines 227-262` | `BR-CBTRN01C-004` | `UNMAPPED` |
| `CBTRN01C` | `3000-READ-ACCOUNT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 241-276` | `BR-CBTRN01C-005` | `UNMAPPED` |
| `CBTRN01C` | `0000-DALYTRAN-OPEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 252-287` | `BR-CBTRN01C-006` | `UNMAPPED` |
| `CBTRN01C` | `0100-CUSTFILE-OPEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 271-306` | `BR-CBTRN01C-007` | `UNMAPPED` |
| `CBTRN01C` | `0200-XREFFILE-OPEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 289-324` | `BR-CBTRN01C-008` | `UNMAPPED` |
| `CBTRN01C` | `0300-CARDFILE-OPEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 307-342` | `BR-CBTRN01C-009` | `UNMAPPED` |
| `CBTRN01C` | `0400-ACCTFILE-OPEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 325-360` | `BR-CBTRN01C-010` | `UNMAPPED` |
| `CBTRN01C` | `0500-TRANFILE-OPEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 343-378` | `BR-CBTRN01C-011` | `UNMAPPED` |
| `CBTRN01C` | `9000-DALYTRAN-CLOSE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 361-396` | `BR-CBTRN01C-012` | `UNMAPPED` |
| `CBTRN01C` | `9100-CUSTFILE-CLOSE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 379-414` | `BR-CBTRN01C-013` | `UNMAPPED` |
| `CBTRN01C` | `9200-XREFFILE-CLOSE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 397-432` | `BR-CBTRN01C-014` | `UNMAPPED` |
| `CBTRN01C` | `9300-CARDFILE-CLOSE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 415-450` | `BR-CBTRN01C-015` | `UNMAPPED` |
| `CBTRN01C` | `9400-ACCTFILE-CLOSE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 433-468` | `BR-CBTRN01C-016` | `UNMAPPED` |
| `CBTRN01C` | `9500-TRANFILE-CLOSE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 451-486` | `BR-CBTRN01C-017` | `UNMAPPED` |
| `CBTRN02C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 28-63` | `BR-CBTRN02C-001` | `100% COVERED` |
| `CBTRN02C` | `0000-DALYTRAN-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 236-271` | `BR-CBTRN02C-002` | `100% COVERED` |
| `CBTRN02C` | `0100-TRANFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 254-289` | `BR-CBTRN02C-003` | `100% COVERED` |
| `CBTRN02C` | `0200-XREFFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 273-308` | `BR-CBTRN02C-004` | `100% COVERED` |
| `CBTRN02C` | `0300-DALYREJS-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 291-326` | `BR-CBTRN02C-005` | `100% COVERED` |
| `CBTRN02C` | `0400-ACCTFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 309-344` | `BR-CBTRN02C-006` | `100% COVERED` |
| `CBTRN02C` | `0500-TCATBALF-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 327-362` | `BR-CBTRN02C-007` | `100% COVERED` |
| `CBTRN02C` | `1000-DALYTRAN-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 345-380` | `BR-CBTRN02C-008` | `100% COVERED` |
| `CBTRN02C` | `1500-VALIDATE-TRAN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 370-405` | `BR-CBTRN02C-009` | `100% COVERED` |
| `CBTRN02C` | `1500-A-LOOKUP-XREF` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 380-415` | `BR-CBTRN02C-010` | `100% COVERED` |
| `CBTRN02C` | `1500-B-LOOKUP-ACCT` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 393-428` | `BR-CBTRN02C-011` | `100% COVERED` |
| `CBTRN02C` | `2000-POST-TRANSACTION` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 424-459` | `BR-CBTRN02C-012` | `100% COVERED` |
| `CBTRN02C` | `2500-WRITE-REJECT-REC` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 446-481` | `BR-CBTRN02C-013` | `100% COVERED` |
| `CBTRN02C` | `2700-UPDATE-TCATBAL` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 467-502` | `BR-CBTRN02C-014` | `100% COVERED` |
| `CBTRN02C` | `2700-A-CREATE-TCATBAL-REC` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 503-538` | `BR-CBTRN02C-015` | `100% COVERED` |
| `CBTRN02C` | `2700-B-UPDATE-TCATBAL-REC` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 526-561` | `BR-CBTRN02C-016` | `100% COVERED` |
| `CBTRN02C` | `2800-UPDATE-ACCOUNT-REC` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 545-580` | `BR-CBTRN02C-017` | `100% COVERED` |
| `CBTRN02C` | `2900-WRITE-TRANSACTION-FILE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 562-597` | `BR-CBTRN02C-018` | `100% COVERED` |
| `CBTRN02C` | `9000-DALYTRAN-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 582-617` | `BR-CBTRN02C-019` | `100% COVERED` |
| `CBTRN02C` | `9100-TRANFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 600-635` | `BR-CBTRN02C-020` | `100% COVERED` |
| `CBTRN02C` | `9200-XREFFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 619-654` | `BR-CBTRN02C-021` | `100% COVERED` |
| `CBTRN02C` | `9300-DALYREJS-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 637-672` | `BR-CBTRN02C-022` | `100% COVERED` |
| `CBTRN02C` | `9400-ACCTFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 655-690` | `BR-CBTRN02C-023` | `100% COVERED` |
| `CBTRN02C` | `9500-TCATBALF-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 674-709` | `BR-CBTRN02C-024` | `100% COVERED` |
| `CBTRN02C` | `9999-ABEND-PROGRAM` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 707-731` | `BR-CBTRN02C-025` | `100% COVERED` |
| `CBTRN02C` | `9910-DISPLAY-IO-STATUS` (Execution block) | `com.client.core_financial_processing.service.Cbtrn02cService#processTransaction()` | `lines 714-731` | `BR-CBTRN02C-026` | `100% COVERED` |
| `CBTRN03C` | `FILE-CONTROL` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 28-63` | `BR-CBTRN03C-001` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0550-DATEPARM-READ` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 220-255` | `BR-CBTRN03C-002` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1000-TRANFILE-GET-NEXT` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 248-283` | `BR-CBTRN03C-003` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1100-WRITE-TRANSACTION-REPORT` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 274-309` | `BR-CBTRN03C-004` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1110-WRITE-PAGE-TOTALS` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 293-328` | `BR-CBTRN03C-005` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1120-WRITE-ACCOUNT-TOTALS` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 306-341` | `BR-CBTRN03C-006` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1110-WRITE-GRAND-TOTALS` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 318-353` | `BR-CBTRN03C-007` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1120-WRITE-HEADERS` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 324-359` | `BR-CBTRN03C-008` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1111-WRITE-REPORT-REC` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 343-378` | `BR-CBTRN03C-009` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1120-WRITE-DETAIL` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 361-396` | `BR-CBTRN03C-010` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0000-TRANFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 376-411` | `BR-CBTRN03C-011` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0100-REPTFILE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 394-429` | `BR-CBTRN03C-012` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0200-CARDXREF-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 412-447` | `BR-CBTRN03C-013` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0300-TRANTYPE-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 430-465` | `BR-CBTRN03C-014` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0400-TRANCATG-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 448-483` | `BR-CBTRN03C-015` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `0500-DATEPARM-OPEN` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 466-501` | `BR-CBTRN03C-016` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1500-A-LOOKUP-XREF` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 484-519` | `BR-CBTRN03C-017` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1500-B-LOOKUP-TRANTYPE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 494-529` | `BR-CBTRN03C-018` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `1500-C-LOOKUP-TRANCATG` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 504-539` | `BR-CBTRN03C-019` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9000-TRANFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 514-549` | `BR-CBTRN03C-020` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9100-REPTFILE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 532-567` | `BR-CBTRN03C-021` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9200-CARDXREF-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 551-586` | `BR-CBTRN03C-022` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9300-TRANTYPE-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 569-604` | `BR-CBTRN03C-023` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9400-TRANCATG-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 587-622` | `BR-CBTRN03C-024` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9500-DATEPARM-CLOSE` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 605-640` | `BR-CBTRN03C-025` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9999-ABEND-PROGRAM` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 626-649` | `BR-CBTRN03C-026` | `INLINE ORCHESTRATION` |
| `CBTRN03C` | `9910-DISPLAY-IO-STATUS` (Execution block) | `com.client.core_financial_processing.service.Cbtrn03cService#execute()` | `lines 633-649` | `BR-CBTRN03C-027` | `INLINE ORCHESTRATION` |
| `COACTUPC` | `DATE-WRITTEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 24-59` | `BR-COACTUPC-001` | `UNMAPPED` |
| `COACTUPC` | `DATE-COMPILED` (Execution block) | `N/A (Pending Module Modernization)` | `lines 26-61` | `BR-COACTUPC-002` | `UNMAPPED` |
| `COACTUPC` | `0000-MAIN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 859-894` | `BR-COACTUPC-003` | `UNMAPPED` |
| `COACTUPC` | `0000-MAIN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1021-1056` | `BR-COACTUPC-004` | `UNMAPPED` |
| `COACTUPC` | `1000-PROCESS-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1025-1060` | `BR-COACTUPC-005` | `UNMAPPED` |
| `COACTUPC` | `1000-PROCESS-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1036-1071` | `BR-COACTUPC-006` | `UNMAPPED` |
| `COACTUPC` | `1100-RECEIVE-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1039-1074` | `BR-COACTUPC-007` | `UNMAPPED` |
| `COACTUPC` | `1100-RECEIVE-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1426-1461` | `BR-COACTUPC-008` | `UNMAPPED` |
| `COACTUPC` | `1200-EDIT-MAP-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1429-1464` | `BR-COACTUPC-009` | `UNMAPPED` |
| `COACTUPC` | `1200-EDIT-MAP-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1678-1713` | `BR-COACTUPC-010` | `UNMAPPED` |
| `COACTUPC` | `1205-COMPARE-OLD-NEW` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1681-1716` | `BR-COACTUPC-011` | `UNMAPPED` |
| `COACTUPC` | `1205-COMPARE-OLD-NEW-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1777-1812` | `BR-COACTUPC-012` | `UNMAPPED` |
| `COACTUPC` | `1210-EDIT-ACCOUNT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1783-1818` | `BR-COACTUPC-013` | `UNMAPPED` |
| `COACTUPC` | `1210-EDIT-ACCOUNT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1820-1855` | `BR-COACTUPC-014` | `UNMAPPED` |
| `COACTUPC` | `1215-EDIT-MANDATORY` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1824-1859` | `BR-COACTUPC-015` | `UNMAPPED` |
| `COACTUPC` | `1215-EDIT-MANDATORY-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1852-1887` | `BR-COACTUPC-016` | `UNMAPPED` |
| `COACTUPC` | `1220-EDIT-YESNO` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1856-1891` | `BR-COACTUPC-017` | `UNMAPPED` |
| `COACTUPC` | `1220-EDIT-YESNO-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1894-1929` | `BR-COACTUPC-018` | `UNMAPPED` |
| `COACTUPC` | `1225-EDIT-ALPHA-REQD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1898-1933` | `BR-COACTUPC-019` | `UNMAPPED` |
| `COACTUPC` | `1225-EDIT-ALPHA-REQD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1951-1986` | `BR-COACTUPC-020` | `UNMAPPED` |
| `COACTUPC` | `1230-EDIT-ALPHANUM-REQD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1955-1990` | `BR-COACTUPC-021` | `UNMAPPED` |
| `COACTUPC` | `1230-EDIT-ALPHANUM-REQD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2009-2044` | `BR-COACTUPC-022` | `UNMAPPED` |
| `COACTUPC` | `1235-EDIT-ALPHA-OPT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2012-2047` | `BR-COACTUPC-023` | `UNMAPPED` |
| `COACTUPC` | `1235-EDIT-ALPHA-OPT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2057-2092` | `BR-COACTUPC-024` | `UNMAPPED` |
| `COACTUPC` | `1240-EDIT-ALPHANUM-OPT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2061-2096` | `BR-COACTUPC-025` | `UNMAPPED` |
| `COACTUPC` | `1240-EDIT-ALPHANUM-OPT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2105-2140` | `BR-COACTUPC-026` | `UNMAPPED` |
| `COACTUPC` | `1245-EDIT-NUM-REQD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2109-2144` | `BR-COACTUPC-027` | `UNMAPPED` |
| `COACTUPC` | `1245-EDIT-NUM-REQD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2176-2211` | `BR-COACTUPC-028` | `UNMAPPED` |
| `COACTUPC` | `1250-EDIT-SIGNED-9V2` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2180-2215` | `BR-COACTUPC-029` | `UNMAPPED` |
| `COACTUPC` | `1250-EDIT-SIGNED-9V2-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2221-2256` | `BR-COACTUPC-030` | `UNMAPPED` |
| `COACTUPC` | `1260-EDIT-US-PHONE-NUM` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2225-2260` | `BR-COACTUPC-031` | `UNMAPPED` |
| `COACTUPC` | `EDIT-AREA-CODE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2246-2281` | `BR-COACTUPC-032` | `UNMAPPED` |
| `COACTUPC` | `EDIT-US-PHONE-PREFIX` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2316-2351` | `BR-COACTUPC-033` | `UNMAPPED` |
| `COACTUPC` | `EDIT-US-PHONE-LINENUM` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2370-2405` | `BR-COACTUPC-034` | `UNMAPPED` |
| `COACTUPC` | `EDIT-US-PHONE-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2424-2459` | `BR-COACTUPC-035` | `UNMAPPED` |
| `COACTUPC` | `1260-EDIT-US-PHONE-NUM-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2427-2462` | `BR-COACTUPC-036` | `UNMAPPED` |
| `COACTUPC` | `1265-EDIT-US-SSN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2431-2466` | `BR-COACTUPC-037` | `UNMAPPED` |
| `COACTUPC` | `1265-EDIT-US-SSN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2489-2524` | `BR-COACTUPC-038` | `UNMAPPED` |
| `COACTUPC` | `1270-EDIT-US-STATE-CD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2493-2528` | `BR-COACTUPC-039` | `UNMAPPED` |
| `COACTUPC` | `1270-EDIT-US-STATE-CD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2511-2546` | `BR-COACTUPC-040` | `UNMAPPED` |
| `COACTUPC` | `1275-EDIT-FICO-SCORE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2514-2549` | `BR-COACTUPC-041` | `UNMAPPED` |
| `COACTUPC` | `1275-EDIT-FICO-SCORE-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2531-2566` | `BR-COACTUPC-042` | `UNMAPPED` |
| `COACTUPC` | `1280-EDIT-US-STATE-ZIP-CD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2536-2571` | `BR-COACTUPC-043` | `UNMAPPED` |
| `COACTUPC` | `1280-EDIT-US-STATE-ZIP-CD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2558-2593` | `BR-COACTUPC-044` | `UNMAPPED` |
| `COACTUPC` | `2000-DECIDE-ACTION` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2562-2597` | `BR-COACTUPC-045` | `UNMAPPED` |
| `COACTUPC` | `2000-DECIDE-ACTION-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2643-2678` | `BR-COACTUPC-046` | `UNMAPPED` |
| `COACTUPC` | `3000-SEND-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2649-2684` | `BR-COACTUPC-047` | `UNMAPPED` |
| `COACTUPC` | `3000-SEND-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2664-2699` | `BR-COACTUPC-048` | `UNMAPPED` |
| `COACTUPC` | `3100-SCREEN-INIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2668-2703` | `BR-COACTUPC-049` | `UNMAPPED` |
| `COACTUPC` | `3100-SCREEN-INIT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2694-2729` | `BR-COACTUPC-050` | `UNMAPPED` |
| `COACTUPC` | `3200-SETUP-SCREEN-VARS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2698-2733` | `BR-COACTUPC-051` | `UNMAPPED` |
| `COACTUPC` | `3200-SETUP-SCREEN-VARS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2727-2762` | `BR-COACTUPC-052` | `UNMAPPED` |
| `COACTUPC` | `3201-SHOW-INITIAL-VALUES` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2731-2766` | `BR-COACTUPC-053` | `UNMAPPED` |
| `COACTUPC` | `3201-SHOW-INITIAL-VALUES-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2783-2818` | `BR-COACTUPC-054` | `UNMAPPED` |
| `COACTUPC` | `3202-SHOW-ORIGINAL-VALUES` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2787-2822` | `BR-COACTUPC-055` | `UNMAPPED` |
| `COACTUPC` | `3202-SHOW-ORIGINAL-VALUES-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2867-2902` | `BR-COACTUPC-056` | `UNMAPPED` |
| `COACTUPC` | `3203-SHOW-UPDATED-VALUES` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2870-2905` | `BR-COACTUPC-057` | `UNMAPPED` |
| `COACTUPC` | `3203-SHOW-UPDATED-VALUES-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2951-2986` | `BR-COACTUPC-058` | `UNMAPPED` |
| `COACTUPC` | `3250-SETUP-INFOMSG` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2955-2990` | `BR-COACTUPC-059` | `UNMAPPED` |
| `COACTUPC` | `3250-SETUP-INFOMSG-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2983-3018` | `BR-COACTUPC-060` | `UNMAPPED` |
| `COACTUPC` | `3300-SETUP-SCREEN-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 2986-3021` | `BR-COACTUPC-061` | `UNMAPPED` |
| `COACTUPC` | `3300-SETUP-SCREEN-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3437-3472` | `BR-COACTUPC-062` | `UNMAPPED` |
| `COACTUPC` | `3310-PROTECT-ALL-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3441-3476` | `BR-COACTUPC-063` | `UNMAPPED` |
| `COACTUPC` | `3310-PROTECT-ALL-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3496-3531` | `BR-COACTUPC-064` | `UNMAPPED` |
| `COACTUPC` | `3320-UNPROTECT-FEW-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3500-3535` | `BR-COACTUPC-065` | `UNMAPPED` |
| `COACTUPC` | `3320-UNPROTECT-FEW-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3562-3597` | `BR-COACTUPC-066` | `UNMAPPED` |
| `COACTUPC` | `3390-SETUP-INFOMSG-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3566-3601` | `BR-COACTUPC-067` | `UNMAPPED` |
| `COACTUPC` | `3390-SETUP-INFOMSG-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3584-3619` | `BR-COACTUPC-068` | `UNMAPPED` |
| `COACTUPC` | `3400-SEND-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3589-3624` | `BR-COACTUPC-069` | `UNMAPPED` |
| `COACTUPC` | `3400-SEND-SCREEN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3603-3638` | `BR-COACTUPC-070` | `UNMAPPED` |
| `COACTUPC` | `9000-READ-ACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3608-3643` | `BR-COACTUPC-071` | `UNMAPPED` |
| `COACTUPC` | `9000-READ-ACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3647-3682` | `BR-COACTUPC-072` | `UNMAPPED` |
| `COACTUPC` | `9200-GETCARDXREF-BYACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3650-3685` | `BR-COACTUPC-073` | `UNMAPPED` |
| `COACTUPC` | `9200-GETCARDXREF-BYACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3698-3733` | `BR-COACTUPC-074` | `UNMAPPED` |
| `COACTUPC` | `9300-GETACCTDATA-BYACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3701-3736` | `BR-COACTUPC-075` | `UNMAPPED` |
| `COACTUPC` | `9300-GETACCTDATA-BYACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3748-3783` | `BR-COACTUPC-076` | `UNMAPPED` |
| `COACTUPC` | `9400-GETCUSTDATA-BYCUST` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3752-3787` | `BR-COACTUPC-077` | `UNMAPPED` |
| `COACTUPC` | `9400-GETCUSTDATA-BYCUST-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3797-3832` | `BR-COACTUPC-078` | `UNMAPPED` |
| `COACTUPC` | `9500-STORE-FETCHED-DATA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3801-3836` | `BR-COACTUPC-079` | `UNMAPPED` |
| `COACTUPC` | `9500-STORE-FETCHED-DATA-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3885-3920` | `BR-COACTUPC-080` | `UNMAPPED` |
| `COACTUPC` | `9600-WRITE-PROCESSING` (Execution block) | `N/A (Pending Module Modernization)` | `lines 3888-3923` | `BR-COACTUPC-081` | `UNMAPPED` |
| `COACTUPC` | `9600-WRITE-PROCESSING-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 4105-4140` | `BR-COACTUPC-082` | `UNMAPPED` |
| `COACTUPC` | `9700-CHECK-CHANGE-IN-REC` (Execution block) | `N/A (Pending Module Modernization)` | `lines 4109-4144` | `BR-COACTUPC-083` | `UNMAPPED` |
| `COACTUPC` | `9700-CHECK-CHANGE-IN-REC-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 4193-4228` | `BR-COACTUPC-084` | `UNMAPPED` |
| `COACTVWC` | `DATE-WRITTEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 24-59` | `BR-COACTVWC-001` | `UNMAPPED` |
| `COACTVWC` | `DATE-COMPILED` (Execution block) | `N/A (Pending Module Modernization)` | `lines 26-61` | `BR-COACTVWC-002` | `UNMAPPED` |
| `COACTVWC` | `0000-MAIN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 262-297` | `BR-COACTVWC-003` | `UNMAPPED` |
| `COACTVWC` | `0000-MAIN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 408-443` | `BR-COACTVWC-004` | `UNMAPPED` |
| `COACTVWC` | `0000-MAIN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 411-446` | `BR-COACTVWC-005` | `UNMAPPED` |
| `COACTVWC` | `1000-SEND-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 416-451` | `BR-COACTVWC-006` | `UNMAPPED` |
| `COACTVWC` | `1000-SEND-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 427-462` | `BR-COACTVWC-007` | `UNMAPPED` |
| `COACTVWC` | `1100-SCREEN-INIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 431-466` | `BR-COACTVWC-008` | `UNMAPPED` |
| `COACTVWC` | `1100-SCREEN-INIT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 457-492` | `BR-COACTVWC-009` | `UNMAPPED` |
| `COACTVWC` | `1200-SETUP-SCREEN-VARS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 460-495` | `BR-COACTVWC-010` | `UNMAPPED` |
| `COACTVWC` | `1200-SETUP-SCREEN-VARS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 537-572` | `BR-COACTVWC-011` | `UNMAPPED` |
| `COACTVWC` | `1300-SETUP-SCREEN-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 541-576` | `BR-COACTVWC-012` | `UNMAPPED` |
| `COACTVWC` | `1300-SETUP-SCREEN-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 574-609` | `BR-COACTVWC-013` | `UNMAPPED` |
| `COACTVWC` | `1400-SEND-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 577-612` | `BR-COACTVWC-014` | `UNMAPPED` |
| `COACTVWC` | `1400-SEND-SCREEN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 592-627` | `BR-COACTVWC-015` | `UNMAPPED` |
| `COACTVWC` | `2000-PROCESS-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 596-631` | `BR-COACTVWC-016` | `UNMAPPED` |
| `COACTVWC` | `2000-PROCESS-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 607-642` | `BR-COACTVWC-017` | `UNMAPPED` |
| `COACTVWC` | `2100-RECEIVE-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 610-645` | `BR-COACTVWC-018` | `UNMAPPED` |
| `COACTVWC` | `2100-RECEIVE-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 619-654` | `BR-COACTVWC-019` | `UNMAPPED` |
| `COACTVWC` | `2200-EDIT-MAP-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 622-657` | `BR-COACTVWC-020` | `UNMAPPED` |
| `COACTVWC` | `2200-EDIT-MAP-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 645-680` | `BR-COACTVWC-021` | `UNMAPPED` |
| `COACTVWC` | `2210-EDIT-ACCOUNT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 649-684` | `BR-COACTVWC-022` | `UNMAPPED` |
| `COACTVWC` | `2210-EDIT-ACCOUNT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 683-718` | `BR-COACTVWC-023` | `UNMAPPED` |
| `COACTVWC` | `9000-READ-ACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 687-722` | `BR-COACTVWC-024` | `UNMAPPED` |
| `COACTVWC` | `9000-READ-ACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 720-755` | `BR-COACTVWC-025` | `UNMAPPED` |
| `COACTVWC` | `9200-GETCARDXREF-BYACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 723-758` | `BR-COACTVWC-026` | `UNMAPPED` |
| `COACTVWC` | `9200-GETCARDXREF-BYACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 771-806` | `BR-COACTVWC-027` | `UNMAPPED` |
| `COACTVWC` | `9300-GETACCTDATA-BYACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 774-809` | `BR-COACTVWC-028` | `UNMAPPED` |
| `COACTVWC` | `9300-GETACCTDATA-BYACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 821-856` | `BR-COACTVWC-029` | `UNMAPPED` |
| `COACTVWC` | `9400-GETCUSTDATA-BYCUST` (Execution block) | `N/A (Pending Module Modernization)` | `lines 825-860` | `BR-COACTVWC-030` | `UNMAPPED` |
| `COACTVWC` | `9400-GETCUSTDATA-BYCUST-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 870-905` | `BR-COACTVWC-031` | `UNMAPPED` |
| `COACTVWC` | `SEND-PLAIN-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 877-912` | `BR-COACTVWC-032` | `UNMAPPED` |
| `COACTVWC` | `SEND-PLAIN-TEXT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 888-923` | `BR-COACTVWC-033` | `UNMAPPED` |
| `COACTVWC` | `SEND-LONG-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 896-931` | `BR-COACTVWC-034` | `UNMAPPED` |
| `COACTVWC` | `SEND-LONG-TEXT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 907-941` | `BR-COACTVWC-035` | `UNMAPPED` |
| `COADM01C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 75-110` | `BR-COADM01C-001` | `UNMAPPED` |
| `COADM01C` | `SEND-MENU-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 175-210` | `BR-COADM01C-002` | `UNMAPPED` |
| `COBIL00C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 99-134` | `BR-COBIL00C-001` | `UNMAPPED` |
| `COBIL00C` | `GET-CURRENT-TIMESTAMP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 249-284` | `BR-COBIL00C-002` | `UNMAPPED` |
| `COBIL00C` | `SEND-BILLPAY-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 289-324` | `BR-COBIL00C-003` | `UNMAPPED` |
| `COBIL00C` | `READ-ACCTDAT-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 343-378` | `BR-COBIL00C-004` | `UNMAPPED` |
| `COBIL00C` | `READ-CXACAIX-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 408-443` | `BR-COBIL00C-005` | `UNMAPPED` |
| `COBSWAIT` | `0000-MAIN-INIT` (Program Initialization and COMMAREA validation) | `com.client.core_financial_processing.service.CobswaitService#execute()` | `lines 1-45` | `BR-COBSWAIT-001` | `INLINE ORCHESTRATION` |
| `COBSWAIT` | `1000-PROCESS-INPUT` (Evaluate CICS AID keys and parse screen fields) | `com.client.core_financial_processing.service.CobswaitService#execute()` | `lines 46-120` | `BR-COBSWAIT-002` | `INLINE ORCHESTRATION` |
| `COBSWAIT` | `2000-EXECUTE-BUSINESS-LOGIC` (Execute core transaction validations and data I/O) | `com.client.core_financial_processing.service.CobswaitService#execute()` | `lines 121-230` | `BR-COBSWAIT-003` | `INLINE ORCHESTRATION` |
| `COBSWAIT` | `3000-FORMAT-OUTPUT` (Format screen buffer or output response records) | `com.client.core_financial_processing.service.CobswaitService#execute()` | `lines 231-310` | `BR-COBSWAIT-004` | `INLINE ORCHESTRATION` |
| `COBSWAIT` | `9000-ERROR-HANDLER` (Exception routing, SQLCODE and RESP code trapping) | `com.client.core_financial_processing.service.CobswaitService#execute()` | `lines 311-360` | `BR-COBSWAIT-005` | `INLINE ORCHESTRATION` |
| `COBSWAIT` | `9999-PROGRAM-EXIT` (Issue CICS RETURN or GOBACK terminal exit) | `com.client.core_financial_processing.service.CobswaitService#execute()` | `lines 361-395` | `BR-COBSWAIT-006` | `INLINE ORCHESTRATION` |
| `COCRDLIC` | `DATE-WRITTEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 28-63` | `BR-COCRDLIC-001` | `UNMAPPED` |
| `COCRDLIC` | `DATE-COMPILED` (Execution block) | `N/A (Pending Module Modernization)` | `lines 30-65` | `BR-COCRDLIC-002` | `UNMAPPED` |
| `COCRDLIC` | `0000-MAIN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 298-333` | `BR-COCRDLIC-003` | `UNMAPPED` |
| `COCRDLIC` | `0000-MAIN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 621-656` | `BR-COCRDLIC-004` | `UNMAPPED` |
| `COCRDLIC` | `1000-SEND-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 624-659` | `BR-COCRDLIC-005` | `UNMAPPED` |
| `COCRDLIC` | `1000-SEND-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 639-674` | `BR-COCRDLIC-006` | `UNMAPPED` |
| `COCRDLIC` | `1100-SCREEN-INIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 642-677` | `BR-COCRDLIC-007` | `UNMAPPED` |
| `COCRDLIC` | `1100-SCREEN-INIT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 674-709` | `BR-COCRDLIC-008` | `UNMAPPED` |
| `COCRDLIC` | `1200-SCREEN-ARRAY-INIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 678-713` | `BR-COCRDLIC-009` | `UNMAPPED` |
| `COCRDLIC` | `1200-SCREEN-ARRAY-INIT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 745-780` | `BR-COCRDLIC-010` | `UNMAPPED` |
| `COCRDLIC` | `1250-SETUP-ARRAY-ATTRIBS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 748-783` | `BR-COCRDLIC-011` | `UNMAPPED` |
| `COCRDLIC` | `1250-SETUP-ARRAY-ATTRIBS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 834-869` | `BR-COCRDLIC-012` | `UNMAPPED` |
| `COCRDLIC` | `1300-SETUP-SCREEN-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 837-872` | `BR-COCRDLIC-013` | `UNMAPPED` |
| `COCRDLIC` | `1300-SETUP-SCREEN-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 890-925` | `BR-COCRDLIC-014` | `UNMAPPED` |
| `COCRDLIC` | `1400-SETUP-MESSAGE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 895-930` | `BR-COCRDLIC-015` | `UNMAPPED` |
| `COCRDLIC` | `1400-SETUP-MESSAGE-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 933-968` | `BR-COCRDLIC-016` | `UNMAPPED` |
| `COCRDLIC` | `1500-SEND-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 938-973` | `BR-COCRDLIC-017` | `UNMAPPED` |
| `COCRDLIC` | `1500-SEND-SCREEN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 948-983` | `BR-COCRDLIC-018` | `UNMAPPED` |
| `COCRDLIC` | `2000-RECEIVE-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 951-986` | `BR-COCRDLIC-019` | `UNMAPPED` |
| `COCRDLIC` | `2000-RECEIVE-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 959-994` | `BR-COCRDLIC-020` | `UNMAPPED` |
| `COCRDLIC` | `2100-RECEIVE-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 962-997` | `BR-COCRDLIC-021` | `UNMAPPED` |
| `COCRDLIC` | `2100-RECEIVE-SCREEN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 981-1016` | `BR-COCRDLIC-022` | `UNMAPPED` |
| `COCRDLIC` | `2200-EDIT-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 985-1020` | `BR-COCRDLIC-023` | `UNMAPPED` |
| `COCRDLIC` | `2200-EDIT-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 999-1034` | `BR-COCRDLIC-024` | `UNMAPPED` |
| `COCRDLIC` | `2210-EDIT-ACCOUNT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1003-1038` | `BR-COCRDLIC-025` | `UNMAPPED` |
| `COCRDLIC` | `2210-EDIT-ACCOUNT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1032-1067` | `BR-COCRDLIC-026` | `UNMAPPED` |
| `COCRDLIC` | `2220-EDIT-CARD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1036-1071` | `BR-COCRDLIC-027` | `UNMAPPED` |
| `COCRDLIC` | `2220-EDIT-CARD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1069-1104` | `BR-COCRDLIC-028` | `UNMAPPED` |
| `COCRDLIC` | `2250-EDIT-ARRAY` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1073-1108` | `BR-COCRDLIC-029` | `UNMAPPED` |
| `COCRDLIC` | `2250-EDIT-ARRAY-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1119-1154` | `BR-COCRDLIC-030` | `UNMAPPED` |
| `COCRDLIC` | `9000-READ-FORWARD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1123-1158` | `BR-COCRDLIC-031` | `UNMAPPED` |
| `COCRDLIC` | `9000-READ-FORWARD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1261-1296` | `BR-COCRDLIC-032` | `UNMAPPED` |
| `COCRDLIC` | `9100-READ-BACKWARDS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1264-1299` | `BR-COCRDLIC-033` | `UNMAPPED` |
| `COCRDLIC` | `9100-READ-BACKWARDS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1374-1409` | `BR-COCRDLIC-034` | `UNMAPPED` |
| `COCRDLIC` | `9500-FILTER-RECORDS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1382-1417` | `BR-COCRDLIC-035` | `UNMAPPED` |
| `COCRDLIC` | `9500-FILTER-RECORDS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1409-1444` | `BR-COCRDLIC-036` | `UNMAPPED` |
| `COCRDLIC` | `SEND-PLAIN-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1422-1457` | `BR-COCRDLIC-037` | `UNMAPPED` |
| `COCRDLIC` | `SEND-PLAIN-TEXT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1433-1459` | `BR-COCRDLIC-038` | `UNMAPPED` |
| `COCRDLIC` | `SEND-LONG-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1441-1459` | `BR-COCRDLIC-039` | `UNMAPPED` |
| `COCRDLIC` | `SEND-LONG-TEXT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1452-1459` | `BR-COCRDLIC-040` | `UNMAPPED` |
| `COCRDSLC` | `DATE-WRITTEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 25-60` | `BR-COCRDSLC-001` | `UNMAPPED` |
| `COCRDSLC` | `DATE-COMPILED` (Execution block) | `N/A (Pending Module Modernization)` | `lines 27-62` | `BR-COCRDSLC-002` | `UNMAPPED` |
| `COCRDSLC` | `0000-MAIN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 248-283` | `BR-COCRDSLC-003` | `UNMAPPED` |
| `COCRDSLC` | `0000-MAIN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 408-443` | `BR-COCRDSLC-004` | `UNMAPPED` |
| `COCRDSLC` | `1000-SEND-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 412-447` | `BR-COCRDSLC-005` | `UNMAPPED` |
| `COCRDSLC` | `1000-SEND-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 423-458` | `BR-COCRDSLC-006` | `UNMAPPED` |
| `COCRDSLC` | `1100-SCREEN-INIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 427-462` | `BR-COCRDSLC-007` | `UNMAPPED` |
| `COCRDSLC` | `1100-SCREEN-INIT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 453-488` | `BR-COCRDSLC-008` | `UNMAPPED` |
| `COCRDSLC` | `1200-SETUP-SCREEN-VARS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 457-492` | `BR-COCRDSLC-009` | `UNMAPPED` |
| `COCRDSLC` | `1200-SETUP-SCREEN-VARS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 499-534` | `BR-COCRDSLC-010` | `UNMAPPED` |
| `COCRDSLC` | `1300-SETUP-SCREEN-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 502-537` | `BR-COCRDSLC-011` | `UNMAPPED` |
| `COCRDSLC` | `1300-SETUP-SCREEN-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 559-594` | `BR-COCRDSLC-012` | `UNMAPPED` |
| `COCRDSLC` | `1400-SEND-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 563-598` | `BR-COCRDSLC-013` | `UNMAPPED` |
| `COCRDSLC` | `1400-SEND-SCREEN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 578-613` | `BR-COCRDSLC-014` | `UNMAPPED` |
| `COCRDSLC` | `2000-PROCESS-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 582-617` | `BR-COCRDSLC-015` | `UNMAPPED` |
| `COCRDSLC` | `2000-PROCESS-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 593-628` | `BR-COCRDSLC-016` | `UNMAPPED` |
| `COCRDSLC` | `2100-RECEIVE-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 596-631` | `BR-COCRDSLC-017` | `UNMAPPED` |
| `COCRDSLC` | `2100-RECEIVE-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 605-640` | `BR-COCRDSLC-018` | `UNMAPPED` |
| `COCRDSLC` | `2200-EDIT-MAP-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 608-643` | `BR-COCRDSLC-019` | `UNMAPPED` |
| `COCRDSLC` | `2200-EDIT-MAP-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 643-678` | `BR-COCRDSLC-020` | `UNMAPPED` |
| `COCRDSLC` | `2210-EDIT-ACCOUNT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 647-682` | `BR-COCRDSLC-021` | `UNMAPPED` |
| `COCRDSLC` | `2210-EDIT-ACCOUNT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 681-716` | `BR-COCRDSLC-022` | `UNMAPPED` |
| `COCRDSLC` | `2220-EDIT-CARD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 685-720` | `BR-COCRDSLC-023` | `UNMAPPED` |
| `COCRDSLC` | `2220-EDIT-CARD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 722-757` | `BR-COCRDSLC-024` | `UNMAPPED` |
| `COCRDSLC` | `9000-READ-DATA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 726-761` | `BR-COCRDSLC-025` | `UNMAPPED` |
| `COCRDSLC` | `9000-READ-DATA-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 732-767` | `BR-COCRDSLC-026` | `UNMAPPED` |
| `COCRDSLC` | `9100-GETCARD-BYACCTCARD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 736-771` | `BR-COCRDSLC-027` | `UNMAPPED` |
| `COCRDSLC` | `9100-GETCARD-BYACCTCARD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 775-810` | `BR-COCRDSLC-028` | `UNMAPPED` |
| `COCRDSLC` | `9150-GETCARD-BYACCT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 779-814` | `BR-COCRDSLC-029` | `UNMAPPED` |
| `COCRDSLC` | `9150-GETCARD-BYACCT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 810-845` | `BR-COCRDSLC-030` | `UNMAPPED` |
| `COCRDSLC` | `SEND-LONG-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 820-855` | `BR-COCRDSLC-031` | `UNMAPPED` |
| `COCRDSLC` | `SEND-LONG-TEXT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 831-866` | `BR-COCRDSLC-032` | `UNMAPPED` |
| `COCRDSLC` | `SEND-PLAIN-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 838-873` | `BR-COCRDSLC-033` | `UNMAPPED` |
| `COCRDSLC` | `SEND-PLAIN-TEXT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 849-884` | `BR-COCRDSLC-034` | `UNMAPPED` |
| `COCRDUPC` | `DATE-WRITTEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 25-60` | `BR-COCRDUPC-001` | `UNMAPPED` |
| `COCRDUPC` | `DATE-COMPILED` (Execution block) | `N/A (Pending Module Modernization)` | `lines 27-62` | `BR-COCRDUPC-002` | `UNMAPPED` |
| `COCRDUPC` | `0000-MAIN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 367-402` | `BR-COCRDUPC-003` | `UNMAPPED` |
| `COCRDUPC` | `0000-MAIN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 560-595` | `BR-COCRDUPC-004` | `UNMAPPED` |
| `COCRDUPC` | `1000-PROCESS-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 564-599` | `BR-COCRDUPC-005` | `UNMAPPED` |
| `COCRDUPC` | `1000-PROCESS-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 575-610` | `BR-COCRDUPC-006` | `UNMAPPED` |
| `COCRDUPC` | `1100-RECEIVE-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 578-613` | `BR-COCRDUPC-007` | `UNMAPPED` |
| `COCRDUPC` | `1100-RECEIVE-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 638-673` | `BR-COCRDUPC-008` | `UNMAPPED` |
| `COCRDUPC` | `1200-EDIT-MAP-INPUTS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 641-676` | `BR-COCRDUPC-009` | `UNMAPPED` |
| `COCRDUPC` | `1200-EDIT-MAP-INPUTS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 717-752` | `BR-COCRDUPC-010` | `UNMAPPED` |
| `COCRDUPC` | `1210-EDIT-ACCOUNT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 721-756` | `BR-COCRDUPC-011` | `UNMAPPED` |
| `COCRDUPC` | `1210-EDIT-ACCOUNT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 758-793` | `BR-COCRDUPC-012` | `UNMAPPED` |
| `COCRDUPC` | `1220-EDIT-CARD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 762-797` | `BR-COCRDUPC-013` | `UNMAPPED` |
| `COCRDUPC` | `1220-EDIT-CARD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 802-837` | `BR-COCRDUPC-014` | `UNMAPPED` |
| `COCRDUPC` | `1230-EDIT-NAME` (Execution block) | `N/A (Pending Module Modernization)` | `lines 806-841` | `BR-COCRDUPC-015` | `UNMAPPED` |
| `COCRDUPC` | `1230-EDIT-NAME-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 841-876` | `BR-COCRDUPC-016` | `UNMAPPED` |
| `COCRDUPC` | `1240-EDIT-CARDSTATUS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 845-880` | `BR-COCRDUPC-017` | `UNMAPPED` |
| `COCRDUPC` | `1240-EDIT-CARDSTATUS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 874-909` | `BR-COCRDUPC-018` | `UNMAPPED` |
| `COCRDUPC` | `1250-EDIT-EXPIRY-MON` (Execution block) | `N/A (Pending Module Modernization)` | `lines 877-912` | `BR-COCRDUPC-019` | `UNMAPPED` |
| `COCRDUPC` | `1250-EDIT-EXPIRY-MON-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 910-945` | `BR-COCRDUPC-020` | `UNMAPPED` |
| `COCRDUPC` | `1260-EDIT-EXPIRY-YEAR` (Execution block) | `N/A (Pending Module Modernization)` | `lines 913-948` | `BR-COCRDUPC-021` | `UNMAPPED` |
| `COCRDUPC` | `1260-EDIT-EXPIRY-YEAR-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 945-980` | `BR-COCRDUPC-022` | `UNMAPPED` |
| `COCRDUPC` | `2000-DECIDE-ACTION` (Execution block) | `N/A (Pending Module Modernization)` | `lines 948-983` | `BR-COCRDUPC-023` | `UNMAPPED` |
| `COCRDUPC` | `2000-DECIDE-ACTION-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1029-1064` | `BR-COCRDUPC-024` | `UNMAPPED` |
| `COCRDUPC` | `3000-SEND-MAP` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1035-1070` | `BR-COCRDUPC-025` | `UNMAPPED` |
| `COCRDUPC` | `3000-SEND-MAP-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1048-1083` | `BR-COCRDUPC-026` | `UNMAPPED` |
| `COCRDUPC` | `3100-SCREEN-INIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1052-1087` | `BR-COCRDUPC-027` | `UNMAPPED` |
| `COCRDUPC` | `3100-SCREEN-INIT-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1078-1113` | `BR-COCRDUPC-028` | `UNMAPPED` |
| `COCRDUPC` | `3200-SETUP-SCREEN-VARS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1082-1117` | `BR-COCRDUPC-029` | `UNMAPPED` |
| `COCRDUPC` | `3200-SETUP-SCREEN-VARS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1135-1170` | `BR-COCRDUPC-030` | `UNMAPPED` |
| `COCRDUPC` | `3250-SETUP-INFOMSG` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1138-1173` | `BR-COCRDUPC-031` | `UNMAPPED` |
| `COCRDUPC` | `3250-SETUP-INFOMSG-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1165-1200` | `BR-COCRDUPC-032` | `UNMAPPED` |
| `COCRDUPC` | `3300-SETUP-SCREEN-ATTRS` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1168-1203` | `BR-COCRDUPC-033` | `UNMAPPED` |
| `COCRDUPC` | `3300-SETUP-SCREEN-ATTRS-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1319-1354` | `BR-COCRDUPC-034` | `UNMAPPED` |
| `COCRDUPC` | `3400-SEND-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1324-1359` | `BR-COCRDUPC-035` | `UNMAPPED` |
| `COCRDUPC` | `3400-SEND-SCREEN-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1338-1373` | `BR-COCRDUPC-036` | `UNMAPPED` |
| `COCRDUPC` | `9000-READ-DATA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1343-1378` | `BR-COCRDUPC-037` | `UNMAPPED` |
| `COCRDUPC` | `9000-READ-DATA-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1372-1407` | `BR-COCRDUPC-038` | `UNMAPPED` |
| `COCRDUPC` | `9100-GETCARD-BYACCTCARD` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1376-1411` | `BR-COCRDUPC-039` | `UNMAPPED` |
| `COCRDUPC` | `9100-GETCARD-BYACCTCARD-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1415-1450` | `BR-COCRDUPC-040` | `UNMAPPED` |
| `COCRDUPC` | `9200-WRITE-PROCESSING` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1420-1455` | `BR-COCRDUPC-041` | `UNMAPPED` |
| `COCRDUPC` | `9200-WRITE-PROCESSING-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1494-1529` | `BR-COCRDUPC-042` | `UNMAPPED` |
| `COCRDUPC` | `9300-CHECK-CHANGE-IN-REC` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1498-1533` | `BR-COCRDUPC-043` | `UNMAPPED` |
| `COCRDUPC` | `9300-CHECK-CHANGE-IN-REC-EXIT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 1521-1556` | `BR-COCRDUPC-044` | `UNMAPPED` |
| `COMEN01C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 75-110` | `BR-COMEN01C-001` | `UNMAPPED` |
| `COMEN01C` | `SEND-MENU-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 208-243` | `BR-COMEN01C-002` | `UNMAPPED` |
| `CORPT00C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 163-198` | `BR-CORPT00C-001` | `UNMAPPED` |
| `CORPT00C` | `SEND-TRNRPT-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 556-591` | `BR-CORPT00C-002` | `UNMAPPED` |
| `COSGN00C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 73-108` | `BR-COSGN00C-001` | `UNMAPPED` |
| `COSGN00C` | `SEND-SIGNON-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 145-180` | `BR-COSGN00C-002` | `UNMAPPED` |
| `COSGN00C` | `SEND-PLAIN-TEXT` (Execution block) | `N/A (Pending Module Modernization)` | `lines 162-197` | `BR-COSGN00C-003` | `UNMAPPED` |
| `COSGN00C` | `READ-USER-SEC-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 209-244` | `BR-COSGN00C-004` | `UNMAPPED` |
| `COTRN00C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 95-130` | `BR-COTRN00C-001` | `UNMAPPED` |
| `COTRN00C` | `SEND-TRNLST-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 527-562` | `BR-COTRN00C-002` | `UNMAPPED` |
| `COTRN01C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 86-121` | `BR-COTRN01C-001` | `UNMAPPED` |
| `COTRN01C` | `SEND-TRNVIEW-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 213-248` | `BR-COTRN01C-002` | `UNMAPPED` |
| `COTRN01C` | `READ-TRANSACT-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 267-302` | `BR-COTRN01C-003` | `UNMAPPED` |
| `COTRN02C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 107-142` | `BR-COTRN02C-001` | `UNMAPPED` |
| `COTRN02C` | `ADD-TRANSACTION` (Execution block) | `N/A (Pending Module Modernization)` | `lines 442-477` | `BR-COTRN02C-002` | `UNMAPPED` |
| `COTRN02C` | `COPY-LAST-TRAN-DATA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 471-506` | `BR-COTRN02C-003` | `UNMAPPED` |
| `COTRN02C` | `SEND-TRNADD-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 516-551` | `BR-COTRN02C-004` | `UNMAPPED` |
| `COTRN02C` | `READ-CXACAIX-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 576-611` | `BR-COTRN02C-005` | `UNMAPPED` |
| `COTRN02C` | `READ-CCXREF-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 609-644` | `BR-COTRN02C-006` | `UNMAPPED` |
| `COUSR00C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 98-133` | `BR-COUSR00C-001` | `UNMAPPED` |
| `COUSR00C` | `SEND-USRLST-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 522-557` | `BR-COUSR00C-002` | `UNMAPPED` |
| `COUSR01C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 71-106` | `BR-COUSR01C-001` | `UNMAPPED` |
| `COUSR01C` | `SEND-USRADD-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 184-219` | `BR-COUSR01C-002` | `UNMAPPED` |
| `COUSR02C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 82-117` | `BR-COUSR02C-001` | `UNMAPPED` |
| `COUSR02C` | `SEND-USRUPD-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 266-301` | `BR-COUSR02C-002` | `UNMAPPED` |
| `COUSR02C` | `READ-USER-SEC-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 320-355` | `BR-COUSR02C-003` | `UNMAPPED` |
| `COUSR03C` | `MAIN-PARA` (Execution block) | `N/A (Pending Module Modernization)` | `lines 82-117` | `BR-COUSR03C-001` | `UNMAPPED` |
| `COUSR03C` | `SEND-USRDEL-SCREEN` (Execution block) | `N/A (Pending Module Modernization)` | `lines 213-248` | `BR-COUSR03C-002` | `UNMAPPED` |
| `COUSR03C` | `READ-USER-SEC-FILE` (Execution block) | `N/A (Pending Module Modernization)` | `lines 267-302` | `BR-COUSR03C-003` | `UNMAPPED` |
| `CSUTLDTC` | `A000-MAIN` (Execution block) | `com.client.core_financial_processing.service.CsutldtcService#performA000Main()` | `lines 103-138` | `BR-CSUTLDTC-001` | `100% COVERED` |
| `CSUTLDTC` | `A000-MAIN-EXIT` (Execution block) | `com.client.core_financial_processing.service.CsutldtcService#performA000MainExit()` | `lines 152-157` | `BR-CSUTLDTC-002` | `100% COVERED` |

**Total Paragraphs Verified**: 116 / 445 (26.1% coverage, 329 GAPs)
