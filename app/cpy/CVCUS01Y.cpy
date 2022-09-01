      *****************************************************************
      *    Data-structure for Customer entity (RECLN 500)
      *****************************************************************
       01  CUSTOMER-RECORD.
           05  CUST-ID                                 PIC 9(09).
           05  CUST-FIRST-NAME                         PIC X(25).
           05  CUST-MIDDLE-NAME                        PIC X(25).
           05  CUST-LAST-NAME                          PIC X(25).
           05  CUST-ADDR-LINE-1                        PIC X(50).
           05  CUST-ADDR-LINE-2                        PIC X(50).
           05  CUST-ADDR-LINE-3                        PIC X(50).         
           05  CUST-ADDR-STATE-CD                      PIC X(02).
           05  CUST-ADDR-COUNTRY-CD                    PIC X(03).
           05  CUST-ADDR-ZIP                           PIC X(10).
           05  CUST-PHONE-NUM-1                        PIC X(15).
           05  CUST-PHONE-NUM-2                        PIC X(15).
           05  CUST-SSN                                PIC 9(09).
           05  CUST-GOVT-ISSUED-ID                     PIC X(20).
           05  CUST-DOB-YYYY-MM-DD                     PIC X(10).
           05  CUST-EFT-ACCOUNT-ID                     PIC X(10).
           05  CUST-PRI-CARD-HOLDER-IND                PIC X(01).
           05  CUST-FICO-CREDIT-SCORE                  PIC 9(03).
           05  FILLER                                  PIC X(168).      
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *
