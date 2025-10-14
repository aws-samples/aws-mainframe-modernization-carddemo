      ******************************************************************
      * CBAUDP01 - Audit Log Population Batch Program
      * 
      * Purpose: Generate audit log entries from customer, account,
      *          and transaction records using in-stream data
      *
      * Input Files:
      *   CUSTIN  - Customer records (CUSTREC format)
      *   ACCTIN  - Account records (CVACT01Y format)  
      *   TRANIN  - Transaction records (CVTRA05Y format)
      *   CARDIN  - CARD records 
      *
      * Output Files:
      *   AUDOUT  - Audit log records (AUDITLOG format)
      *
      * Processing:
      *   1. Process customer records and create audit entries
      *   2. Process account records and create audit entries
      *   3. Process transaction records and create audit entries
      *   4. Generate realistic user IDs and timestamps
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBAUDP01.
       AUTHOR. CARDDEMO DEVELOPMENT TEAM.
       DATE-WRITTEN. 2024-12-19.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTIN-FILE
               ASSIGN TO CUSTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CUSTIN-STATUS
               .
           SELECT ACCTIN-FILE
               ASSIGN TO ACCTIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-ACCTIN-STATUS
               .
           SELECT TRANIN-FILE
               ASSIGN TO TRANIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-TRANIN-STATUS
               .
           SELECT CARDIN-FILE
               ASSIGN TO CARDIN
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-CARDIN-STATUS
               .
           SELECT AUDOUT-FILE
               ASSIGN TO AUDOUT
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-AUDOUT-STATUS
               .
               
       DATA DIVISION.
       FILE SECTION.
       
       FD  CUSTIN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 500 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           .
           COPY CUSTREC.
           
       FD  ACCTIN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 300 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           .
           COPY CVACT01Y.
           
       FD  TRANIN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 350 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           .
           COPY CVTRA05Y.
           
       FD  CARDIN-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 150 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           .
           COPY CVACT02Y.
           
       FD  AUDOUT-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 565 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           .
           COPY AUDITLOG.
           
       WORKING-STORAGE SECTION.
       
      * File Status Variables
       01  WS-FILE-STATUS.
           05  WS-CUSTIN-STATUS            PIC X(02) VALUE SPACES.
               88  WS-CUSTIN-EOF           VALUE '10'.
               88  WS-CUSTIN-OK            VALUE '00'.
           05  WS-ACCTIN-STATUS            PIC X(02) VALUE SPACES.
               88  WS-ACCTIN-EOF           VALUE '10'.
               88  WS-ACCTIN-OK            VALUE '00'.
           05  WS-TRANIN-STATUS            PIC X(02) VALUE SPACES.
               88  WS-TRANIN-EOF           VALUE '10'.
               88  WS-TRANIN-OK            VALUE '00'.
           05  WS-CARDIN-STATUS            PIC X(02) VALUE SPACES.
               88  WS-CARDIN-EOF           VALUE '10'.
               88  WS-CARDIN-OK            VALUE '00'.
           05  WS-AUDOUT-STATUS            PIC X(02) VALUE SPACES.
               88  WS-AUDOUT-OK            VALUE '00'.
               
      * Processing Control Variables
       01  WS-CONTROL-FLAGS.
           05  WS-PROGRAM-STATUS           PIC X(01) VALUE 'N'.
               88  WS-PROGRAM-OK           VALUE 'Y'.
               88  WS-PROGRAM-ERROR        VALUE 'N'.
           05  WS-RETURN-CODE              PIC 9(02) VALUE ZERO.
               88  WS-SUCCESS              VALUE 00.
               88  WS-WARNING              VALUE 04.
               88  WS-ERROR                VALUE 08.
               88  WS-SEVERE-ERROR         VALUE 12.
               
      * Processing Counters
       01  WS-COUNTERS.
           05  WS-CUSTOMER-COUNT           PIC 9(07) VALUE ZERO.
           05  WS-ACCOUNT-COUNT            PIC 9(07) VALUE ZERO.
           05  WS-TRANSACTION-COUNT        PIC 9(07) VALUE ZERO.
           05  WS-CARD-COUNT               PIC 9(07) VALUE ZERO.
           05  WS-AUDIT-COUNT              PIC 9(07) VALUE ZERO.
           05  WS-ERROR-COUNT              PIC 9(07) VALUE ZERO.
           
      * User ID Generation Variables
       01  WS-USER-ID-CONTROL.
           05  WS-USER-SEQUENCE            PIC 9(05) VALUE 1.
           05  WS-USER-ID                  PIC X(08) VALUE SPACES.
           
      * Timestamp Variables
       01  WS-TIMESTAMP-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURR-YEAR            PIC 9(04).
               10  WS-CURR-MONTH           PIC 9(02).
               10  WS-CURR-DAY             PIC 9(02).
           05  WS-CURRENT-TIME.
               10  WS-CURR-HOUR            PIC 9(02).
               10  WS-CURR-MINUTE          PIC 9(02).
               10  WS-CURR-SECOND          PIC 9(02).
               10  WS-CURR-HUNDREDTH       PIC 9(02).
           05  WS-FORMATTED-TIMESTAMP      PIC X(26) VALUE SPACES.
           05  WS-WORK-DATE                PIC X(10) VALUE SPACES.
           05  WS-WORK-TIME                PIC X(15) VALUE SPACES.
           
      * Display Messages
       01  WS-MESSAGES.
           05  WS-MSG-PROGRAM-START        PIC X(50) VALUE
               'CBAUDP01: Audit Log Population Started'.
           05  WS-MSG-PROGRAM-END          PIC X(50) VALUE
               'CBAUDP01: Audit Log Population Completed'.
           05  WS-MSG-FILE-ERROR           PIC X(50) VALUE
               'CBAUDP01: File Operation Error'.
           05  WS-MSG-PROCESSING           PIC X(50) VALUE
               'CBAUDP01: Processing Records'.
               
       PROCEDURE DIVISION.
       
      ******************************************************************
      * Main Processing Control
      ******************************************************************
       0000-MAIN-PROCESS.
           DISPLAY WS-MSG-PROGRAM-START
           
           PERFORM 1000-INITIALIZE
           
           IF WS-PROGRAM-OK
               PERFORM 2000-PROCESS-CUSTOMERS
           END-IF
           
           IF WS-PROGRAM-OK
               PERFORM 3000-PROCESS-ACCOUNTS
           END-IF
           
           IF WS-PROGRAM-OK
               PERFORM 4000-PROCESS-TRANSACTIONS
           END-IF
           
           IF WS-PROGRAM-OK
               PERFORM 4500-PROCESS-CARDS
           END-IF
           
           PERFORM 9000-TERMINATE
           
           DISPLAY WS-MSG-PROGRAM-END
           
           MOVE WS-RETURN-CODE TO RETURN-CODE
           
           STOP RUN
           .
           
      ******************************************************************
      * Program Initialization
      ******************************************************************
       1000-INITIALIZE.
           
           DISPLAY 'CBAUDP01: Initializing Program'
           
      *    Initialize control flags and variables
           SET WS-PROGRAM-OK TO TRUE
           MOVE ZERO TO WS-RETURN-CODE
           
      *    Initialize all counters to zero
           INITIALIZE WS-COUNTERS
           
      *    Initialize user ID generation sequence
           MOVE 1 TO WS-USER-SEQUENCE
           MOVE SPACES TO WS-USER-ID
           
      *    Initialize timestamp fields
           INITIALIZE WS-TIMESTAMP-FIELDS
           
      *    Initialize file status fields
           MOVE SPACES TO WS-CUSTIN-STATUS
           MOVE SPACES TO WS-ACCTIN-STATUS
           MOVE SPACES TO WS-TRANIN-STATUS
           MOVE SPACES TO WS-CARDIN-STATUS
           MOVE SPACES TO WS-AUDOUT-STATUS
           
           DISPLAY 'CBAUDP01: Opening input files...'
           
      *    Open customer input file
           PERFORM 1100-OPEN-CUSTIN-FILE
           
      *    Open account input file  
           IF WS-PROGRAM-OK
               PERFORM 1200-OPEN-ACCTIN-FILE
           END-IF
           
      *    Open transaction input file
           IF WS-PROGRAM-OK
               PERFORM 1300-OPEN-TRANIN-FILE
           END-IF
           
      *    Open card input file
           IF WS-PROGRAM-OK
               PERFORM 1350-OPEN-CARDIN-FILE
           END-IF
           
      *    Open audit output file
           IF WS-PROGRAM-OK
               PERFORM 1400-OPEN-AUDOUT-FILE
           END-IF
           
           IF WS-PROGRAM-OK
               DISPLAY 'CBAUDP01: All files opened successfully'
               DISPLAY 'CBAUDP01: Initialization completed'
           ELSE
               DISPLAY 'CBAUDP01: Initialization failed'
               DISPLAY 'CBAUDP01: Program will terminate'
           END-IF
           
           .
           
      ******************************************************************
      * Open Customer Input File
      ******************************************************************
       1100-OPEN-CUSTIN-FILE.
           
           OPEN INPUT CUSTIN-FILE
           
           EVALUATE WS-CUSTIN-STATUS
               WHEN '00'
                   DISPLAY 'CBAUDP01: CUSTIN file opened successfully'
               WHEN '35'
                   DISPLAY 'CBAUDP01: CUSTIN file not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '37'
                   DISPLAY 'CBAUDP01: CUSTIN file not available'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: CUSTIN file record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   DISPLAY 'CBAUDP01: CUSTIN file open error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Open Account Input File
      ******************************************************************
       1200-OPEN-ACCTIN-FILE.
           
           OPEN INPUT ACCTIN-FILE
           
           EVALUATE WS-ACCTIN-STATUS
               WHEN '00'
                   DISPLAY 'CBAUDP01: ACCTIN file opened successfully'
               WHEN '35'
                   DISPLAY 'CBAUDP01: ACCTIN file not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '37'
                   DISPLAY 'CBAUDP01: ACCTIN file not available'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: ACCTIN file record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   DISPLAY 'CBAUDP01: ACCTIN file open error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Open Transaction Input File
      ******************************************************************
       1300-OPEN-TRANIN-FILE.
           
           OPEN INPUT TRANIN-FILE
           
           EVALUATE WS-TRANIN-STATUS
               WHEN '00'
                   DISPLAY 'CBAUDP01: TRANIN file opened successfully'
               WHEN '35'
                   DISPLAY 'CBAUDP01: TRANIN file not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '37'
                   DISPLAY 'CBAUDP01: TRANIN file not available'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: TRANIN file record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   DISPLAY 'CBAUDP01: TRANIN file open error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Open Card Input File
      ******************************************************************
       1350-OPEN-CARDIN-FILE.
           
           OPEN INPUT CARDIN-FILE
           
           EVALUATE WS-CARDIN-STATUS
               WHEN '00'
                   DISPLAY 'CBAUDP01: CARDIN file opened successfully'
               WHEN '35'
                   DISPLAY 'CBAUDP01: CARDIN file not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '37'
                   DISPLAY 'CBAUDP01: CARDIN file not available'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: CARDIN file record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   DISPLAY 'CBAUDP01: CARDIN file open error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Open Audit Output File
      ******************************************************************
       1400-OPEN-AUDOUT-FILE.
           
           OPEN OUTPUT AUDOUT-FILE
           
           EVALUATE WS-AUDOUT-STATUS
               WHEN '00'
                   DISPLAY 'CBAUDP01: AUDOUT file opened successfully'
               WHEN '24'
                   DISPLAY 'CBAUDP01: AUDOUT file boundary violation'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '30'
                   DISPLAY 'CBAUDP01: AUDOUT file permanent error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '34'
                   DISPLAY 'CBAUDP01: AUDOUT file boundary violation'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '37'
                   DISPLAY 'CBAUDP01: AUDOUT file not available'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: AUDOUT file record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   DISPLAY 'CBAUDP01: AUDOUT file open error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Process Customer Records
      ******************************************************************
       2000-PROCESS-CUSTOMERS.
           
           DISPLAY 'CBAUDP01: Processing Customer Records'
           
      *    Initialize customer processing
           MOVE ZERO TO WS-CUSTOMER-COUNT
           
      *    Read first customer record
           PERFORM 2100-READ-CUSTOMER
           
      *    Process all customer records until end of file
           PERFORM UNTIL WS-CUSTIN-EOF OR WS-PROGRAM-ERROR
               ADD 1 TO WS-CUSTOMER-COUNT
               
      *        Validate and create audit record for this customer
               PERFORM 2150-VALIDATE-CUSTOMER
               PERFORM 2200-CREATE-CUSTOMER-AUDIT
               
      *        Read next customer record
               PERFORM 2100-READ-CUSTOMER
           END-PERFORM
           
           DISPLAY 'CBAUDP01: Customer processing completed'
           DISPLAY 'CBAUDP01: Total customers processed: '
                   WS-CUSTOMER-COUNT
           
           .
           
      ******************************************************************
      * Read Customer Record
      ******************************************************************
       2100-READ-CUSTOMER.
           
           READ CUSTIN-FILE
           
           EVALUATE WS-CUSTIN-STATUS
               WHEN '00'
      *            Successful read - no action needed
                   CONTINUE
               WHEN '10'
                   DISPLAY 'CBAUDP01: End of customer file reached'
               WHEN '23'
                   DISPLAY 'CBAUDP01: Customer record key not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN '30'
                   DISPLAY 'CBAUDP01: Customer file permanent error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: Customer record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN OTHER
                   DISPLAY 'CBAUDP01: Customer file read error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CUSTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Validate Customer Record
      ******************************************************************
       2150-VALIDATE-CUSTOMER.
           
      *    Validate customer ID is not zero
           IF CUST-ID = ZERO
               DISPLAY 'CBAUDP01: Invalid customer ID (zero) - '
                       'Record: ' WS-CUSTOMER-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate customer name fields are not spaces
           IF CUST-FIRST-NAME = SPACES AND CUST-LAST-NAME = SPACES
               DISPLAY 'CBAUDP01: Missing customer name - '
                       'Record: ' WS-CUSTOMER-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate SSN is numeric and not zero
           IF CUST-SSN = ZERO
               DISPLAY 'CBAUDP01: Invalid SSN (zero) - '
                       'Record: ' WS-CUSTOMER-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
           .
           
      ******************************************************************
      * Create Customer Audit Record
      ******************************************************************
       2200-CREATE-CUSTOMER-AUDIT.
           
      *    Initialize audit record
           INITIALIZE AUDIT-LOG-RECORD
           
      *    Set audit log type to Customer
           SET AUDIT-CUSTOMER TO TRUE
           
      *    Generate timestamp
           PERFORM 5100-GENERATE-TIMESTAMP
           
      *    Generate user ID
           PERFORM 5200-GENERATE-USER-ID
           
      *    Set audit metadata
           MOVE 'B' TO AUDIT-USER-TYPE
           SET AUDIT-INSERT TO TRUE
           
      *    Map customer record fields to audit structure
           MOVE CUST-ID TO AUDIT-CUST-ID
           MOVE CUST-FIRST-NAME TO AUDIT-CUST-FIRST-NAME
           MOVE CUST-MIDDLE-NAME TO AUDIT-CUST-MIDDLE-NAME
           MOVE CUST-LAST-NAME TO AUDIT-CUST-LAST-NAME
           MOVE CUST-ADDR-LINE-1 TO AUDIT-CUST-ADDR-LINE-1
           MOVE CUST-ADDR-LINE-2 TO AUDIT-CUST-ADDR-LINE-2
           MOVE CUST-ADDR-LINE-3 TO AUDIT-CUST-ADDR-LINE-3
           MOVE CUST-ADDR-STATE-CD TO AUDIT-CUST-ADDR-STATE-CD
           MOVE CUST-ADDR-COUNTRY-CD TO AUDIT-CUST-ADDR-COUNTRY-CD
           MOVE CUST-ADDR-ZIP TO AUDIT-CUST-ADDR-ZIP
           MOVE CUST-PHONE-NUM-1 TO AUDIT-CUST-PHONE-NUM-1
           MOVE CUST-PHONE-NUM-2 TO AUDIT-CUST-PHONE-NUM-2
           MOVE CUST-SSN TO AUDIT-CUST-SSN
           MOVE CUST-GOVT-ISSUED-ID TO AUDIT-CUST-GOVT-ISSUED-ID
           MOVE CUST-DOB-YYYYMMDD TO AUDIT-CUST-DOB-YYYYMMDD
           MOVE CUST-EFT-ACCOUNT-ID TO AUDIT-CUST-EFT-ACCOUNT-ID
           MOVE CUST-PRI-CARD-HOLDER-IND TO AUDIT-CUST-PRI-CARD-IND
           MOVE CUST-FICO-CREDIT-SCORE TO AUDIT-CUST-FICO-SCORE
           
      *    Write audit record to output file
           PERFORM 5000-WRITE-AUDIT-RECORD
           
           .
           
      ******************************************************************
      * Process Account Records  
      ******************************************************************
       3000-PROCESS-ACCOUNTS.
           
           DISPLAY 'CBAUDP01: Processing Account Records'
           
      *    Initialize account processing
           MOVE ZERO TO WS-ACCOUNT-COUNT
           
      *    Read first account record
           PERFORM 3100-READ-ACCOUNT
           
      *    Process all account records until end of file
           PERFORM UNTIL WS-ACCTIN-EOF OR WS-PROGRAM-ERROR
               ADD 1 TO WS-ACCOUNT-COUNT
               
      *        Validate and create audit record for this account
               PERFORM 3150-VALIDATE-ACCOUNT
               PERFORM 3200-CREATE-ACCOUNT-AUDIT
               
      *        Read next account record
               PERFORM 3100-READ-ACCOUNT
           END-PERFORM
           
           DISPLAY 'CBAUDP01: Account processing completed'
           DISPLAY 'CBAUDP01: Total accounts processed: '
                   WS-ACCOUNT-COUNT
           
           .
           
      ******************************************************************
      * Read Account Record
      ******************************************************************
       3100-READ-ACCOUNT.
           
           READ ACCTIN-FILE
           
           EVALUATE WS-ACCTIN-STATUS
               WHEN '00'
      *            Successful read - no action needed
                   CONTINUE
               WHEN '10'
                   DISPLAY 'CBAUDP01: End of account file reached'
               WHEN '23'
                   DISPLAY 'CBAUDP01: Account record key not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN '30'
                   DISPLAY 'CBAUDP01: Account file permanent error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: Account record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN OTHER
                   DISPLAY 'CBAUDP01: Account file read error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-ACCTIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Validate Account Record
      ******************************************************************
       3150-VALIDATE-ACCOUNT.
           
      *    Validate account ID is not zero
           IF ACCT-ID = ZERO
               DISPLAY 'CBAUDP01: Invalid account ID (zero) - '
                       'Record: ' WS-ACCOUNT-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate account status is valid
           IF ACCT-ACTIVE-STATUS NOT = 'Y' AND 
              ACCT-ACTIVE-STATUS NOT = 'N'
               DISPLAY 'CBAUDP01: Invalid account status - '
                       'Record: ' WS-ACCOUNT-COUNT
                       ' Status: ' ACCT-ACTIVE-STATUS
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate credit limit is not negative
           IF ACCT-CREDIT-LIMIT < ZERO
               DISPLAY 'CBAUDP01: Negative credit limit - '
                       'Record: ' WS-ACCOUNT-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
           .
           
      ******************************************************************
      * Create Account Audit Record
      ******************************************************************
       3200-CREATE-ACCOUNT-AUDIT.
           
      *    Initialize audit record
           INITIALIZE AUDIT-LOG-RECORD
           
      *    Set audit log type to Account
           SET AUDIT-ACCOUNT TO TRUE
           
      *    Generate timestamp
           PERFORM 5100-GENERATE-TIMESTAMP
           
      *    Generate user ID
           PERFORM 5200-GENERATE-USER-ID
           
      *    Set audit metadata
           MOVE 'B' TO AUDIT-USER-TYPE
           SET AUDIT-INSERT TO TRUE
           
      *    Map account record fields to audit structure
           MOVE ACCT-ID TO AUDIT-ACCT-ID
           MOVE ACCT-ACTIVE-STATUS TO AUDIT-ACCT-ACTIVE-STATUS
           MOVE ACCT-CURR-BAL TO AUDIT-ACCT-CURR-BAL
           MOVE ACCT-CREDIT-LIMIT TO AUDIT-ACCT-CREDIT-LIMIT
           MOVE ACCT-CASH-CREDIT-LIMIT TO AUDIT-ACCT-CASH-LIMIT
           MOVE ACCT-OPEN-DATE TO AUDIT-ACCT-OPEN-DATE
           MOVE ACCT-EXPIRAION-DATE TO AUDIT-ACCT-EXPIRATION-DATE
           MOVE ACCT-REISSUE-DATE TO AUDIT-ACCT-REISSUE-DATE
           MOVE ACCT-CURR-CYC-CREDIT TO AUDIT-ACCT-CYC-CREDIT
           MOVE ACCT-CURR-CYC-DEBIT TO AUDIT-ACCT-CYC-DEBIT
           MOVE ACCT-ADDR-ZIP TO AUDIT-ACCT-ADDR-ZIP
           MOVE ACCT-GROUP-ID TO AUDIT-ACCT-GROUP-ID
           
      *    Write audit record to output file
           PERFORM 5000-WRITE-AUDIT-RECORD
           
           .
           
      ******************************************************************
      * Process Transaction Records
      ******************************************************************
       4000-PROCESS-TRANSACTIONS.
           
           DISPLAY 'CBAUDP01: Processing Transaction Records'
           
      *    Initialize transaction processing
           MOVE ZERO TO WS-TRANSACTION-COUNT
           
      *    Read first transaction record
           PERFORM 4100-READ-TRANSACTION
           
      *    Process all transaction records until end of file
           PERFORM UNTIL WS-TRANIN-EOF OR WS-PROGRAM-ERROR
               ADD 1 TO WS-TRANSACTION-COUNT
               
      *        Validate and create audit record for this transaction
               PERFORM 4150-VALIDATE-TRANSACTION
               PERFORM 4200-CREATE-TRANSACTION-AUDIT
               
      *        Read next transaction record
               PERFORM 4100-READ-TRANSACTION
           END-PERFORM
           
           DISPLAY 'CBAUDP01: Transaction processing completed'
           DISPLAY 'CBAUDP01: Total transactions processed: '
                   WS-TRANSACTION-COUNT
           
           .
           
      ******************************************************************
      * Process Card Records
      ******************************************************************
       4500-PROCESS-CARDS.
           
           DISPLAY 'CBAUDP01: Processing Card Records'
           
      *    Initialize card processing
           MOVE ZERO TO WS-CARD-COUNT
           
      *    Read first card record
           PERFORM 4600-READ-CARD
           
      *    Process all card records until end of file
           PERFORM UNTIL WS-CARDIN-EOF OR WS-PROGRAM-ERROR
               ADD 1 TO WS-CARD-COUNT
               
      *        Validate and create audit record for this card
               PERFORM 4700-VALIDATE-CARD-RECORD
               
               IF WS-SUCCESS OR WS-WARNING
                   PERFORM 4800-CREATE-CARD-AUDIT-RECORD
               END-IF
               
      *        Read next card record
               PERFORM 4600-READ-CARD
           END-PERFORM
           
           DISPLAY 'CBAUDP01: Card processing completed'
           DISPLAY 'CBAUDP01: Total cards processed: '
                   WS-CARD-COUNT
           
           .
           
      ******************************************************************
      * Read Card Record
      ******************************************************************
       4600-READ-CARD.
           
           READ CARDIN-FILE
           
           EVALUATE WS-CARDIN-STATUS
               WHEN '00'
      *            Successful read - no action needed
                   CONTINUE
               WHEN '10'
                   DISPLAY 'CBAUDP01: End of card file reached'
               WHEN '23'
                   DISPLAY 'CBAUDP01: Card record key not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN '30'
                   DISPLAY 'CBAUDP01: Card file permanent error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: Card record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN OTHER
                   DISPLAY 'CBAUDP01: Card file read error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-CARDIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Validate Card Record
      ******************************************************************
       4700-VALIDATE-CARD-RECORD.
           
      *    Validate card number is not spaces
           IF CARD-NUM = SPACES
               DISPLAY 'CBAUDP01: Missing card number - '
                       'Record: ' WS-CARD-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate account ID is not zero
           IF CARD-ACCT-ID = ZERO
               DISPLAY 'CBAUDP01: Invalid account ID (zero) - '
                       'Record: ' WS-CARD-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate card status
           IF CARD-ACTIVE-STATUS NOT = 'Y' AND
              CARD-ACTIVE-STATUS NOT = 'N'
               DISPLAY 'CBAUDP01: Invalid card status - '
                       'Record: ' WS-CARD-COUNT
                       ' Status: ' CARD-ACTIVE-STATUS
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate embossed name is not spaces
           IF CARD-EMBOSSED-NAME = SPACES
               DISPLAY 'CBAUDP01: Missing embossed name - '
                       'Record: ' WS-CARD-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
           .
           
      ******************************************************************
      * Create Card Audit Record
      ******************************************************************
       4800-CREATE-CARD-AUDIT-RECORD.
           
      *    Initialize audit record
           INITIALIZE AUDIT-LOG-RECORD
           
      *    Set audit log type to Card
           SET AUDIT-CARD TO TRUE
           
      *    Set common audit fields
           PERFORM 5100-GENERATE-TIMESTAMP
           PERFORM 5200-GENERATE-USER-ID
           MOVE 'B' TO AUDIT-USER-TYPE
           SET AUDIT-INSERT TO TRUE
           
      *    Move card data to audit record
           MOVE CARD-NUM TO AUDIT-CARD-NUM
           MOVE CARD-ACCT-ID TO AUDIT-CARD-ACCT-ID
           MOVE CARD-CVV-CD TO AUDIT-CARD-CVV-CD
           MOVE CARD-EMBOSSED-NAME TO AUDIT-CARD-EMBOSSED-NAME
           MOVE CARD-EXPIRAION-DATE TO AUDIT-CARD-EXPIRAION-DATE
           MOVE CARD-ACTIVE-STATUS TO AUDIT-CARD-ACTIVE-STATUS
           
      *    Write audit record
           PERFORM 5000-WRITE-AUDIT-RECORD
           
           .
           
      ******************************************************************
      * Read Transaction Record
      ******************************************************************
       4100-READ-TRANSACTION.
           
           READ TRANIN-FILE
           
           EVALUATE WS-TRANIN-STATUS
               WHEN '00'
      *            Successful read - no action needed
                   CONTINUE
               WHEN '10'
                   DISPLAY 'CBAUDP01: End of transaction file reached'
               WHEN '23'
                   DISPLAY 'CBAUDP01: Transaction record key not found'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN '30'
                   DISPLAY 'CBAUDP01: Transaction file permanent error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '90'
                   DISPLAY 'CBAUDP01: Transaction record length error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   ADD 1 TO WS-ERROR-COUNT
                   IF WS-SUCCESS
                       SET WS-WARNING TO TRUE
                   END-IF
               WHEN OTHER
                   DISPLAY 'CBAUDP01: Transaction file read error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-TRANIN-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Validate Transaction Record
      ******************************************************************
       4150-VALIDATE-TRANSACTION.
           
      *    Validate transaction ID is not spaces
           IF TRAN-ID = SPACES
               DISPLAY 'CBAUDP01: Missing transaction ID - '
                       'Record: ' WS-TRANSACTION-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate transaction type code is not spaces
           IF TRAN-TYPE-CD = SPACES
               DISPLAY 'CBAUDP01: Missing transaction type - '
                       'Record: ' WS-TRANSACTION-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate transaction amount is not zero
           IF TRAN-AMT = ZERO
               DISPLAY 'CBAUDP01: Zero transaction amount - '
                       'Record: ' WS-TRANSACTION-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Validate card number is not spaces
           IF TRAN-CARD-NUM = SPACES
               DISPLAY 'CBAUDP01: Missing card number - '
                       'Record: ' WS-TRANSACTION-COUNT
               ADD 1 TO WS-ERROR-COUNT
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
           .
           
      ******************************************************************
      * Create Transaction Audit Record
      ******************************************************************
       4200-CREATE-TRANSACTION-AUDIT.
           
      *    Initialize audit record
           INITIALIZE AUDIT-LOG-RECORD
           
      *    Set audit log type to Transaction
           SET AUDIT-TRANSACTION TO TRUE
           
      *    Generate timestamp
           PERFORM 5100-GENERATE-TIMESTAMP
           
      *    Generate user ID
           PERFORM 5200-GENERATE-USER-ID
           
      *    Set audit metadata
           MOVE 'B' TO AUDIT-USER-TYPE
           SET AUDIT-INSERT TO TRUE
           
      *    Map transaction record fields to audit structure
           MOVE TRAN-ID TO AUDIT-TRAN-ID
           MOVE TRAN-TYPE-CD TO AUDIT-TRAN-TYPE-CD
           MOVE TRAN-CAT-CD TO AUDIT-TRAN-CAT-CD
           MOVE TRAN-SOURCE TO AUDIT-TRAN-SOURCE
           MOVE TRAN-DESC TO AUDIT-TRAN-DESC
           MOVE TRAN-AMT TO AUDIT-TRAN-AMT
           MOVE TRAN-MERCHANT-ID TO AUDIT-TRAN-MERCHANT-ID
           MOVE TRAN-MERCHANT-NAME TO AUDIT-TRAN-MERCHANT-NAME
           MOVE TRAN-MERCHANT-CITY TO AUDIT-TRAN-MERCHANT-CITY
           MOVE TRAN-MERCHANT-ZIP TO AUDIT-TRAN-MERCHANT-ZIP
           MOVE TRAN-CARD-NUM TO AUDIT-TRAN-CARD-NUM
           MOVE TRAN-ORIG-TS TO AUDIT-TRAN-ORIG-TS
           MOVE TRAN-PROC-TS TO AUDIT-TRAN-PROC-TS
           
      *    Write audit record to output file
           PERFORM 5000-WRITE-AUDIT-RECORD
           
           .
           
      ******************************************************************
      * Write Audit Record
      ******************************************************************
       5000-WRITE-AUDIT-RECORD.
           
           WRITE AUDIT-LOG-RECORD
           
           EVALUATE WS-AUDOUT-STATUS
               WHEN '00'
                   ADD 1 TO WS-AUDIT-COUNT
               WHEN '24'
                   DISPLAY 'CBAUDP01: Audit file boundary violation'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN '30'
                   DISPLAY 'CBAUDP01: Audit file permanent error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
               WHEN OTHER
                   DISPLAY 'CBAUDP01: Audit file write error'
                   DISPLAY 'CBAUDP01: File Status: ' WS-AUDOUT-STATUS
                   SET WS-PROGRAM-ERROR TO TRUE
                   SET WS-ERROR TO TRUE
                   ADD 1 TO WS-ERROR-COUNT
           END-EVALUATE
           
           .
           
      ******************************************************************
      * Generate Timestamp
      ******************************************************************
       5100-GENERATE-TIMESTAMP.
           
      *    Get current date and time
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           
      *    Format date as YYYY-MM-DD
           STRING WS-CURR-YEAR '-' WS-CURR-MONTH '-' WS-CURR-DAY
               DELIMITED BY SIZE
               INTO WS-WORK-DATE
           END-STRING
           
      *    Format time as HH:MM:SS.HH
           STRING WS-CURR-HOUR ':' WS-CURR-MINUTE ':' 
                  WS-CURR-SECOND '.' WS-CURR-HUNDREDTH
               DELIMITED BY SIZE
               INTO WS-WORK-TIME
           END-STRING
           
      *    Combine date and time with space separator
           STRING WS-WORK-DATE ' ' WS-WORK-TIME
               DELIMITED BY SIZE
               INTO AUDIT-TIMESTAMP
           END-STRING
           
           .
           
      ******************************************************************
      * Generate User ID
      ******************************************************************
       5200-GENERATE-USER-ID.
           
      *    Generate user ID in format USRnnnnn
           STRING 'USR' WS-USER-SEQUENCE
               DELIMITED BY SIZE
               INTO WS-USER-ID
           END-STRING
           
      *    Move to audit record
           MOVE WS-USER-ID TO AUDIT-USER-ID
           
      *    Increment sequence for next user ID
           ADD 1 TO WS-USER-SEQUENCE
           
      *    Reset sequence if it exceeds 99999
           IF WS-USER-SEQUENCE > 99999
               MOVE 1 TO WS-USER-SEQUENCE
           END-IF
           
           .
           
      ******************************************************************
      * Program Termination and Cleanup
      ******************************************************************
       9000-TERMINATE.
           
           DISPLAY 'CBAUDP01: Terminating Program'
           
      *    Close all files with error checking
           PERFORM 9100-CLOSE-FILES
           
      *    Display processing statistics
           DISPLAY 'CBAUDP01: Processing Statistics:'
           DISPLAY '  Customer Records: ' WS-CUSTOMER-COUNT
           DISPLAY '  Account Records:  ' WS-ACCOUNT-COUNT
           DISPLAY '  Transaction Records: ' WS-TRANSACTION-COUNT
           DISPLAY '  Card Records: ' WS-CARD-COUNT
           DISPLAY '  Audit Records Created: ' WS-AUDIT-COUNT
           DISPLAY '  Error Records: ' WS-ERROR-COUNT
           DISPLAY '  Return Code: ' WS-RETURN-CODE
           
           .
           
      ******************************************************************
      * Close All Files
      ******************************************************************
       9100-CLOSE-FILES.
           
           DISPLAY 'CBAUDP01: Closing files...'
           
      *    Close customer input file
           CLOSE CUSTIN-FILE
           IF NOT WS-CUSTIN-OK AND NOT WS-CUSTIN-EOF
               DISPLAY 'CBAUDP01: Warning - CUSTIN close status: '
                       WS-CUSTIN-STATUS
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Close account input file
           CLOSE ACCTIN-FILE
           IF NOT WS-ACCTIN-OK AND NOT WS-ACCTIN-EOF
               DISPLAY 'CBAUDP01: Warning - ACCTIN close status: '
                       WS-ACCTIN-STATUS
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Close transaction input file
           CLOSE TRANIN-FILE
           IF NOT WS-TRANIN-OK AND NOT WS-TRANIN-EOF
               DISPLAY 'CBAUDP01: Warning - TRANIN close status: '
                       WS-TRANIN-STATUS
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Close card input file
           CLOSE CARDIN-FILE
           IF NOT WS-CARDIN-OK AND NOT WS-CARDIN-EOF
               DISPLAY 'CBAUDP01: Warning - CARDIN close status: '
                       WS-CARDIN-STATUS
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
      *    Close audit output file
           CLOSE AUDOUT-FILE
           IF NOT WS-AUDOUT-OK
               DISPLAY 'CBAUDP01: Warning - AUDOUT close status: '
                       WS-AUDOUT-STATUS
               IF WS-SUCCESS
                   SET WS-WARNING TO TRUE
               END-IF
           END-IF
           
           DISPLAY 'CBAUDP01: All files closed'
           
           .