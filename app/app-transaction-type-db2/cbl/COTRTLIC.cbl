000100*****************************************************************
000200* Program:     COTRTLIC.CBL                                     *
000300* Layer:       Business logic                                   *
000400* Function:    List Transaction Type for updates and deletes    *
000500*              Demonstrates paging with cursors in Db2          *
000600*              and Simple, select, delete and update use cases  *
000700*****************************************************************
000800* Copyright Amazon.com, Inc. or its affiliates.
000900* All Rights Reserved.
001000*
001100* Licensed under the Apache License, Version 2.0 (the "License").
001200* You may not use this file except in compliance with the License.
001300* You may obtain a copy of the License at
001400*
001500*    http://www.apache.org/licenses/LICENSE-2.0
001600*
001700* Unless required by applicable law or agreed to in writing,
001800* software distributed under the License is distributed on an
001900* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
002000* either express or implied. See the License for the specific
002100* language governing permissions and limitations under the License
002200******************************************************************
002300
002400 IDENTIFICATION DIVISION.
002500 PROGRAM-ID.
002600     COTRTLIC.
002700 DATE-WRITTEN.
002800     Jan 2023.
002900 DATE-COMPILED.
003000     Today.
003100
003200 ENVIRONMENT DIVISION.
003300 INPUT-OUTPUT SECTION.
003400
003500 DATA DIVISION.
003600
003700 WORKING-STORAGE SECTION.
003800
003900******************************************************************
004000* Literals and Constants
004100******************************************************************
004200 01 WS-CONSTANTS.
004300   05  LIT-THISPGM             PIC X(8)        VALUE 'COTRTLIC'.
004400   05  LIT-THISTRANID          PIC X(4)        VALUE 'CTLI'.
004500   05  LIT-THISMAPSET          PIC X(7)        VALUE 'COTRTLI'.
004600   05  LIT-THISMAP             PIC X(7)        VALUE 'CTRTLIA'.
004700   05  LIT-ADMINPGM             PIC X(8)       VALUE 'COADM01C'.
004800   05  LIT-ADMINTRANID          PIC X(4)       VALUE 'CA00'.
004900   05  LIT-ADMINMAPSET          PIC X(7)       VALUE 'COADM01'.
005000   05  LIT-ADDTPGM             PIC X(8)        VALUE 'COTRTUPC'.
005100   05  LIT-ADDTTRANID          PIC X(4)        VALUE 'CTTU'.
005200   05  LIT-ADDTMAPSET          PIC X(7)        VALUE 'COTRTUP'.
005300   05  LIT-ADDTMAP             PIC X(7)        VALUE 'CTRTUPA'.
005400   05  LIT-DSNTIAC             PIC X(7)        VALUE 'DSNTIAC'.
005500   05  LIT-ASTERISK            PIC X(7)        VALUE '*'.
005600   05  LIT-TRANTYPE-TABLE      PIC X(30)       VALUE
005700                                             'TRANSACTION_TYPE '.
005800   05  LIT-DELETE-FLAG         PIC X(1)        VALUE 'D'.
005900   05  LIT-UPDATE-FLAG         PIC X(1)        VALUE 'U'.
006000   05  WS-MAX-SCREEN-LINES     PIC S9(4)      COMP VALUE 7.
006100
006200******************************************************************
006300* Literals for use in INSPECT statements
006400******************************************************************
006500    05 LIT-ALL-ALPHANUM-FROM-X.
006600       10 LIT-ALL-ALPHA-FROM-X.
006700          15 LIT-UPPER                       PIC X(26)
006800                           VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
006900          15 LIT-LOWER                       PIC X(26)
007000                           VALUE 'abcdefghijklmnopqrstuvwxyz'.
007100       10 LIT-NUMBERS                        PIC X(10)
007200                           VALUE '0123456789'.
007300
007400******************************************************************
007500* Variables for use in INSPECT statements
007600******************************************************************
007700 01  LIT-ALL-ALPHA-FROM     PIC X(52) VALUE SPACES.
007800 01  LIT-ALL-ALPHANUM-FROM  PIC X(62) VALUE SPACES.
007900 01  LIT-ALL-NUM-FROM       PIC X(10) VALUE SPACES.
008000 77  LIT-ALPHA-SPACES-TO    PIC X(52) VALUE SPACES.
008100 77  LIT-ALPHANUM-SPACES-TO PIC X(62) VALUE SPACES.
008200 77  LIT-NUM-SPACES-TO      PIC X(10) VALUE SPACES.
008300
008400 01  WS-MISC-STORAGE.
008500******************************************************************
008600* General CICS related
008700******************************************************************
008800
008900   05 WS-CICS-PROCESSNG-VARS.
009000      07 WS-RESP-CD            PIC S9(9) COMP VALUE ZEROS.
009100      07 WS-REAS-CD            PIC S9(9) COMP VALUE ZEROS.
009200      07 WS-TRANID             PIC X(4)       VALUE SPACES.
009300
009400
009500******************************************************************
009600* Input edits
009700******************************************************************
009800   05 WS-INPUT-FLAG                          PIC X(1).
009900     88  INPUT-OK                            VALUES '0'
010000                                                    ' '
010100                                             LOW-VALUES.
010200     88  INPUT-ERROR                         VALUE '1'.
010300   05  WS-EDIT-TYPE-FLAG                     PIC X(1).
010400     88  FLG-TYPEFILTER-NOT-OK               VALUE '0'.
010500     88  FLG-TYPEFILTER-ISVALID              VALUE '1'.
010600     88  FLG-TYPEFILTER-BLANK                VALUE ' '.
010700   05  WS-EDIT-DESC-FLAG                     PIC X(1).
010800     88  FLG-DESCFILTER-NOT-OK               VALUE '0'.
010900     88  FLG-DESCFILTER-ISVALID              VALUE '1'.
011000     88  FLG-DESCFILTER-BLANK                VALUE ' '.
011100   05 WS-TYPEFILTER-CHANGED                  PIC X(1).
011200     88  FLG-TYPEFILTER-CHANGED-NO           VALUE LOW-VALUES.
011300     88  FLG-TYPEFILTER-CHANGED-YES          VALUE 'Y'.
011400   05 WS-DESCFILTER-CHANGED                  PIC X(1).
011500     88  FLG-DESCFILTER-CHANGED-NO           VALUE LOW-VALUES.
011600     88  FLG-DESCFILTER-CHANGED-YES          VALUE 'Y'.
011700   05 WS-ROW-RECORDS-CHANGED                 PIC X(01)
011800                                             OCCURS 7 TIMES.
011900     88  FLG-ROW-DESCR-CHANGED-NO            VALUE LOW-VALUES.
012000     88  FLG-ROW-DESCR-CHANGED-YES           VALUE 'Y'.
012100   05 WS-DELETE-STATUS                       PIC X(1).
012200     88  FLG-DELETED-NO                      VALUE LOW-VALUES.
012300     88  FLG-DELETED-YES                     VALUE 'Y'.
012400   05 WS-UPDATE-STATUS                       PIC X(1).
012500     88  FLG-UPDATED-NO                      VALUE LOW-VALUES.
012600     88  FLG-UPDATE-COMPLETED                     VALUE 'Y'.
012700   05 WS-ROW-SELECTION-CHANGED               PIC X(1).
012800     88  FLG-ROW-SELECTION-CHANGED-NO        VALUE LOW-VALUES.
012900     88  FLG-ROW-SELECTION-CHANGED-YES       VALUE 'Y'.
013000   05 WS-BAD-SELECTION-ACTION                PIC X(1).
013100     88  FLG-BAD-ACTIONS-SELECTED-NO         VALUE LOW-VALUES.
013200     88  FLG-BAD-ACTIONS-SELECTED-YES        VALUE 'Y'.
013300   05 WS-ARRAY-DESCRIPTION-FLGS              PIC X(1).
013400     88  FLG-ROW-DESCRIPTION-ISVALID         VALUE LOW-VALUES
013500                                                   SPACES.
013600     88  FLG-ROW-DESCRIPTION-NOT-OK          VALUE '0'.
013700     88  FLG-ROW-DESCRIPTION-BLANK           VALUE 'B'.
013800   05  WS-DATACHANGED-FLAG                   PIC X(1).
013900     88  NO-CHANGES-FOUND                    VALUE '0'.
014000     88  CHANGES-HAVE-OCCURRED               VALUE '1'.
014100
014200*  Generic Input Edits
014300   05  WS-GENERIC-EDITS.
014400     10 WS-EDIT-VARIABLE-NAME                PIC X(25).
014500
014600     10 WS-EDIT-ALPHANUM-ONLY                PIC X(256).
014700     10 WS-EDIT-ALPHANUM-LENGTH              PIC S9(4) COMP-3.
014800
014900     10 WS-EDIT-ALPHANUM-ONLY-FLAGS          PIC X(1).
015000        88  FLG-ALPHNANUM-ISVALID            VALUE LOW-VALUES.
015100        88  FLG-ALPHNANUM-NOT-OK             VALUE '0'.
015200        88  FLG-ALPHNANUM-BLANK              VALUE 'B'.
015300
015400   05  WS-OTHER-EDIT-VARS.
015500     10 WS-RECORDS-COUNT                     PIC S9(4) COMP-3
015600                                             VALUE 0.
015700
015800******************************************************************
015900*  Input edits array variables
016000******************************************************************
016100******************************************************************
016200*  Screen Data Array         52 CHARS X 7 ROWS = 364
016300******************************************************************
016400
016500   05 WS-SCREEN-DATA-IN.
016600      10 WS-ALL-ROWS-IN                      PIC X(364).
016700      10 FILLER REDEFINES WS-ALL-ROWS-IN.
016800         15 WS-SCREEN-ROWS-IN OCCURS  7 TIMES.
016900            20 WS-EACH-ROW-IN.
017000               25 WS-EACH-TTYP-IN.
017100                  30 WS-ROW-TR-CODE-IN       PIC X(02).
017200                  30 WS-ROW-TR-DESC-IN       PIC X(50).
017300
017400
017500   05 WS-EDIT-SELECT-COUNTER                 PIC S9(04)
017600                                             USAGE COMP-3
017700                                             VALUE 0.
017800   05 WS-EDIT-SELECT-FLAGS                   PIC X(7)
017900                                             VALUE LOW-VALUES.
018000   05 FILLER  REDEFINES  WS-EDIT-SELECT-FLAGS.
018100      10 WS-EDIT-SELECT                      PIC X(1)
018200                                             OCCURS 7 TIMES.
018300         88 SELECT-OK                        VALUES 'D', 'U'.
018400         88 DELETE-REQUESTED-ON              VALUE 'D'.
018500         88 UPDATE-REQUESTED-ON              VALUE 'U'.
018600         88 SELECT-BLANK                     VALUES
018700                                             ' ',
018800                                             LOW-VALUES.
018900
019000   05 WS-EDIT-SELECT-ERROR-FLAGS             PIC X(7)
019100                                             VALUE LOW-VALUES.
019200   05 FILLER  REDEFINES WS-EDIT-SELECT-ERROR-FLAGS.
019300      10 WS-EDIT-SELECT-ERRORS               OCCURS 7 TIMES.
019400         20 WS-ROW-TRTSELECT-ERROR           PIC X(1).
019500            88 WS-ROW-SELECT-ERROR           VALUE '1'.
019600
019700   05 WS-SUBSCRIPT-VARS.
019800      10 I                                  PIC S9(4) COMP
019900                                            VALUE 0.
020000      10 I-SELECTED                         PIC S9(4) COMP
020100                                            VALUE 0.
020200   05 WS-ACTIONS-SELECTED.
020300      07 WS-ACTIONS-REQUESTED               PIC S9(04)
020400                                            USAGE COMP-3
020500                                            VALUE 0.
020600         88 WS-ONLY-1-ACTION                VALUE 1.
020700         88 WS-MORETHAN1ACTION              VALUES 2 THRU 7.
020800      07 WS-DELETES-REQUESTED               PIC S9(04)
020900                                            USAGE COMP-3
021000                                            VALUE 0.
021100      07 WS-UPDATES-REQUESTED               PIC S9(04)
021200                                            USAGE COMP-3
021300                                            VALUE 0.
021400      07 WS-NO-ACTIONS-SELECTED             PIC S9(04)
021500                                            COMP-3
021600                                            VALUE 0.
021700   05 WS-VALID-ACTIONS-SELECTED             PIC S9(04)
021800                                            USAGE COMP-3
021900                                            VALUE 0.
022000      88 WS-ONLY-1-VALID-ACTION             VALUE 1.
022100
022200******************************************************************
022300* Output edits
022400******************************************************************
022500   05 CICS-OUTPUT-EDIT-VARS.
022600     10  TRAN-TYPE-CD-X                      PIC X(02).
022700     10  TRAN-TYPE-CD-N REDEFINES TRAN-TYPE-CD-X
022800                                             PIC 9(02).
022900     10  FLG-PROTECT-SELECT-ROWS             PIC X(1).
023000     88  FLG-PROTECT-SELECT-ROWS-NO          VALUE '0'.
023100     88  FLG-PROTECT-SELECT-ROWS-YES         VALUE '1'.
023200******************************************************************
023300* Output Message Construction
023400******************************************************************
023500   05  WS-LONG-MSG                           PIC X(800).
023600   05  WS-INFO-MSG                           PIC X(45).
023700     88  WS-NO-INFO-MESSAGE                  VALUES
023800                                             SPACES LOW-VALUES.
023900     88  WS-INFORM-REC-ACTIONS               VALUE
024000         'Type U to update, D to delete any record'.
024100     88  WS-INFORM-DELETE                    VALUE
024200         'Delete HIGHLIGHTED row ? Press F10 to confirm'.
024300     88  WS-INFORM-UPDATE                    VALUE
024400         'Update HIGHLIGHTED row. Press F10 to save'.
024500     88  WS-INFORM-DELETE-SUCCESS            VALUE
024600         'HIGHLIGHTED row deleted.Hit Enter to continue'.
024700     88  WS-INFORM-UPDATE-SUCCESS            VALUE
024800         'HIGHLIGHTED row was updated'.
024900   05  WS-RETURN-MSG                         PIC X(75).
025000     88  WS-RETURN-MSG-OFF                   VALUE SPACES.
025100     88  WS-EXIT-MESSAGE                     VALUE
025200         'PF03 pressed. Exiting'.
025300     88  WS-MESG-NO-RECORDS-FOUND            VALUE
025400         'No records found for this search condition.'.
025500     88  WS-MESG-NO-MORE-RECORDS             VALUE
025600         'No more pages for these search conditions'.
025700     88  WS-MESG-MORE-THAN-1-ACTION          VALUE
025800         'Please select only 1 action'.
025900     88  WS-MESG-INVALID-ACTION-CODE         VALUE
026000         'Action code selected is invalid'.
026100     88  WS-MESG-NO-CHANGES-DETECTED         VALUE
026200         'No change detected with respect to database values.'.
026300   05  WS-PFK-FLAG                           PIC X(1).
026400     88  PFK-VALID                           VALUE '0'.
026500     88  PFK-INVALID                         VALUE '1'.
026600   05 WS-STRING-FORMAT-VARS.
026700      10 WS-STRING-MID                      PIC 9(3) VALUE 0.
026800      10 WS-STRING-LEN                      PIC 9(3) VALUE 0.
026900      10 WS-STRING-OUT                      PIC X(45).
027000
027100******************************************************************
027200* Data Handling
027300******************************************************************
027400   05 WS-DATA-FILTERS.
027500      10  WS-START-KEY                      PIC X(02).
027600      10  WS-TYPE-CD-FILTER                 PIC X(02)
027700                                            VALUE SPACES.
027800      10  WS-TYPE-DESC-FILTER               PIC X(52).
027900      10  WS-TYPE-CD-DELETE-FILTER.
028000          15 FILLER                         PIC X(01)
028100                                            VALUE '('.
028200          15 WS-TYPE-CD-DELETE-FILTER-X.
028300             20 WS-TYPE-CD-DELETE-KEYS      OCCURS 7 TIMES.
028400                25 FILLER                   PIC X(01)
028500                                            VALUE QUOTE.
028600                25 WS-TYPE-CD-DELETE-KEY    PIC X(02)
028700                                            VALUE SPACES.
028800                25 FILLER                   PIC X(01)
028900                                            VALUE QUOTE.
029000                25 FILLER                   PIC X(01)
029100                                            VALUE ','.
029200             20 WS-DUMMY.
029300                25 FILLER                   PIC X(01)
029400                                            VALUE QUOTE.
029500                25 FILLER                   PIC X(01)
029600                                            VALUE SPACE.
029700                25 FILLER                   PIC X(01)
029800                                            VALUE QUOTE.
029900
030000          15 FILLER                         PIC X(1)
030100                                            VALUE ')'.
030200
030300
030400     EXEC SQL INCLUDE CSDB2RWY END-EXEC
030500
030600******************************************************************
030700* Screen Edit Vars
030800******************************************************************
030900   05 WS-SCREEN-EDIT-VARS.
031000      10 WS-IN-TYPE-CD                      PIC X(02)
031100                                            VALUE SPACES.
031200      10 WS-IN-TYPE-CD-N    REDEFINES WS-IN-TYPE-CD PIC 9(02).
031300      10 WS-IN-TYPE-DESC                    PIC X(50).
031400
031500******************************************************************
031600* Screen Array Vars
031700******************************************************************
031800   05  WS-ROW-NUMBER               PIC S9(4) COMP VALUE 0.
031900
032000   05  WS-RECORDS-TO-PROCESS-FLAG            PIC X(1).
032100     88  READ-LOOP-EXIT                      VALUE '0'.
032200     88  MORE-RECORDS-TO-READ                VALUE '1'.
032300
032400******************************************************************
032500*Other common working storage Variables
032600******************************************************************
032700 COPY CVCRD01Y.
032800******************************************************************
032900* Relational Database stuff
033000******************************************************************
033100      EXEC SQL INCLUDE SQLCA    END-EXEC
033200
033300      EXEC SQL INCLUDE DCLTRTYP END-EXEC
033400
033500******************************************************************
033600*Cursor Declarations
033700******************************************************************
033800      EXEC SQL
033900           DECLARE C-TR-TYPE-FORWARD CURSOR FOR
034000               SELECT TR_TYPE
034100                     ,TR_DESCRIPTION
034200             FROM  CARDDEMO.TRANSACTION_TYPE
034300                WHERE TR_TYPE >= :WS-START-KEY
034400                AND  ((:WS-EDIT-TYPE-FLAG = '1'
034500                AND   TR_TYPE = :WS-TYPE-CD-FILTER)
034600                OR   (:WS-EDIT-TYPE-FLAG <> '1'))
034700                AND  ((:WS-EDIT-DESC-FLAG = '1'
034800                AND   TR_DESCRIPTION LIKE
034900                           TRIM(:WS-TYPE-DESC-FILTER))
035000                OR   (:WS-EDIT-DESC-FLAG <> '1'))
035100             ORDER BY TR_TYPE
035200      END-EXEC
035300
035400      EXEC SQL
035500           DECLARE C-TR-TYPE-BACKWARD CURSOR FOR
035600               SELECT TR_TYPE
035700                     ,TR_DESCRIPTION
035800             FROM  CARDDEMO.TRANSACTION_TYPE
035900                WHERE TR_TYPE < :WS-START-KEY
036000                and  ((:WS-EDIT-TYPE-FLAG = '1'
036100                and   TR_TYPE = :WS-TYPE-CD-FILTER)
036200                OR   (:WS-EDIT-TYPE-FLAG <> '1'))
036300                AND  ((:WS-EDIT-DESC-FLAG = '1'
036400                AND   TR_DESCRIPTION LIKE
036500                           TRIM(:WS-TYPE-DESC-FILTER))
036600                OR   (:WS-EDIT-DESC-FLAG <> '1'))
036700               ORDER BY TR_TYPE DESC
036800      END-EXEC
036900
037000
037100******************************************************************
037200*  Commarea manipulations
037300******************************************************************
037400*Application Commmarea Copybook
037500 COPY COCOM01Y.
037600
037700 01 WS-THIS-PROGCOMMAREA.
037800      10 WS-CA-TYPE-CD                          PIC X(02)
037900                                                VALUE SPACES.
038000      10 WS-CA-TYPE-CD-N REDEFINES WS-CA-TYPE-CD PIC 9(02).
038100      10 WS-CA-TYPE-DESC                        PIC X(50).
038200
038300******************************************************************
038400*  Screen Data Array         52 CHARS X 7 ROWS = 364
038500******************************************************************
038600       10 FILLER.
038700          15 WS-CA-ALL-ROWS-OUT                 PIC X(364).
038800          15 FILLER REDEFINES WS-CA-ALL-ROWS-OUT.
038900             20 WS-CA-SCREEN-ROWS-OUT   OCCURS  7 TIMES.
039000                30 WS-CA-EACH-ROW-OUT.
039100                   35 WS-CA-ROW-TR-CODE-OUT     PIC X(02).
039200                   35 WS-CA-ROW-TR-DESC-OUT     PIC X(50).
039300
039400
039500      10 WS-CA-ROW-SELECTED                     PIC S9(4) COMP
039600                                                VALUE 0.
039700      10 WS-CA-PAGING-VARIABLES.
039800         15 WS-CA-LAST-TTYPEKEY.
039900            20  WS-CA-LAST-TR-CODE              PIC X(02).
040000         15 WS-CA-FIRST-TTYPEKEY.
040100            20  WS-CA-FIRST-TR-CODE             PIC X(02).
040200
040300         15 WS-CA-SCREEN-NUM                    PIC 9(1).
040400            88 CA-FIRST-PAGE                    VALUE 1.
040500         15 WS-CA-LAST-PAGE-DISPLAYED           PIC 9(1).
040600            88 CA-LAST-PAGE-SHOWN               VALUE 0.
040700            88 CA-LAST-PAGE-NOT-SHOWN           VALUE 9.
040800         15 WS-CA-NEXT-PAGE-IND                 PIC X(1).
040900            88 CA-NEXT-PAGE-NOT-EXISTS          VALUE LOW-VALUES.
041000            88 CA-NEXT-PAGE-EXISTS              VALUE 'Y'.
041100       10 WS-CA-DELETE-FLAG                     PIC X.
041200            88 CA-DELETE-NOT-REQUESTED          VALUE LOW-VALUES.
041300            88 CA-DELETE-REQUESTED              VALUE 'Y'.
041400            88 CA-DELETE-SUCCEEDED              VALUE LOW-VALUES.
041500       10 WS-CA-UPDATE-FLAG                     PIC X.
041600            88 CA-UPDATE-NOT-REQUESTED          VALUE LOW-VALUES.
041700            88 CA-UPDATE-REQUESTED              VALUE 'Y'.
041800            88 CA-UPDATE-SUCCEEDED              VALUE LOW-VALUES.
041900
042000 01  WS-COMMAREA                                PIC X(2000).
042100
042200
042300
042400*IBM SUPPLIED COPYBOOKS
042500 COPY DFHBMSCA.
042600 COPY DFHAID.
042700
042800*COMMON COPYBOOKS
042900*Screen Titles
043000 COPY COTTL01Y.
043100
043200*Credit Card List Screen Layout
043300 COPY COTRTLI.
043400   01 FILLER REDEFINES CTRTLIAI.
043500    05 FILLER                           PIC X(238).
043600    05 WS-ROW-DATAI.
043700         06 EACH-ROWI OCCURS 7 TIMES.
043800            07 TRTSELL                  PIC S9(4) COMP.
043900            07 TRTSELF                  PIC X.
044000            07 FILLER REDEFINES TRTSELF.
044100               10 TRTSELA               PIC X.
044200            07 FILLER                   PIC X(4).
044300            07 TRTSELI                  PIC X(1).
044400            07 TRTTYPL                  PIC S9(4) COMP.
044500            07 TRTTYPF                  PIC X.
044600            07 FILLER REDEFINES TRTTYPF.
044700               10 TRTTYPA               PIC X.
044800            07 FILLER                   PIC X(4).
044900            07 TRTTYPI                  PIC X(2).
045000            07 TRTYPDL                  PIC S9(4) COMP.
045100            07 TRTYPDF                 PIC X.
045200            07 FILLER REDEFINES TRTYPDF.
045300               10 TRTYPDA               PIC X.
045400            07 FILLER                   PIC X(4).
045500            07 TRTYPDI                  PIC X(50).
045600    05 FILLER                           PIC X(137).
045700   01 FILLER REDEFINES CTRTLIAO.
045800    05 FILLER                           PIC X(238).
045900    05 EACH-ROWO OCCURS 7 TIMES.
046000            07 FILLER                   PIC X(3).
046100            07 TRTSELC                  PIC X.
046200            07 TRTSELP                  PIC X.
046300            07 TRTSELH                  PIC X.
046400            07 TRTSELV                  PIC X.
046500            07 TRTSELO                  PIC X(1).
046600            07 FILLER                   PIC X(3).
046700            07 TRTTYPC                  PIC X.
046800            07 TRTTYPP                  PIC X.
046900            07 TRTTYPH                  PIC X.
047000            07 TRTTYPV                  PIC X.
047100            07 TRTTYPO                  PIC X(2).
047200            07 FILLER                   PIC X(3).
047300            07 TRTYPDC                  PIC X.
047400            07 TRTYPDP                  PIC X.
047500            07 TRTYPDH                  PIC X.
047600            07 TRTYPDV                  PIC X.
047700            07 TRTYPDO                  PIC X(50).
047800    05 FILLER                           PIC X(137).
047900*Current Date
048000 COPY CSDAT01Y.
048100*Common Messages
048200 COPY CSMSG01Y.
048300
048400*Signed on user data
048500 COPY CSUSR01Y.
048600
048700*Dataset layouts
048800
048900*CARD RECORD LAYOUT
049000 COPY CVACT02Y.
049100
049200 LINKAGE SECTION.
049300 01  DFHCOMMAREA.
049400   05  FILLER                                PIC X(1)
049500       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.
049600
049700 PROCEDURE DIVISION.
049800 0000-MAIN.
049900
050000     INITIALIZE CC-WORK-AREA
050100                WS-MISC-STORAGE
050200                WS-COMMAREA
050300
050400*****************************************************************
050500* Store our context
050600*****************************************************************
050700     MOVE LIT-THISTRANID       TO WS-TRANID
050800*****************************************************************
050900* Ensure error message is cleared                               *
051000*****************************************************************
051100     SET WS-RETURN-MSG-OFF  TO TRUE
051200*****************************************************************
051300* Retrieve passed data if  any. Initialize them if first run.
051400*****************************************************************
051500     IF EIBCALEN = 0
051600        INITIALIZE CARDDEMO-COMMAREA
051700                   WS-THIS-PROGCOMMAREA
051800        MOVE LIT-THISTRANID        TO CDEMO-FROM-TRANID
051900        MOVE LIT-THISPGM           TO CDEMO-FROM-PROGRAM
052000        SET CDEMO-USRTYP-ADMIN     TO TRUE
052100        SET CDEMO-PGM-ENTER        TO TRUE
052200        MOVE LIT-THISMAP           TO CDEMO-LAST-MAP
052300        MOVE LIT-THISMAPSET        TO CDEMO-LAST-MAPSET
052400        SET CA-FIRST-PAGE          TO TRUE
052500        SET CA-LAST-PAGE-NOT-SHOWN TO TRUE
052600     ELSE
052700        MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA) TO
052800                          CARDDEMO-COMMAREA
052900        MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
053000                         LENGTH OF WS-THIS-PROGCOMMAREA )TO
053100                          WS-THIS-PROGCOMMAREA
053200     END-IF
053300
053400******************************************************************
053500* Remap PFkeys as needed.
053600* Store the Mapped PF Key
053700*****************************************************************
053800     PERFORM YYYY-STORE-PFKEY
053900        THRU YYYY-STORE-PFKEY-EXIT
054000
054100*****************************************************************
054200* If coming in from menu. Lets forget the past and start afresh *
054300*****************************************************************
054400     IF (CDEMO-PGM-ENTER
054500     AND CDEMO-FROM-PROGRAM NOT EQUAL LIT-THISPGM)
054600     OR ( CCARD-AID-PFK03
054700     AND CDEMO-FROM-TRANID  EQUAL LIT-ADDTTRANID)
054800         INITIALIZE WS-THIS-PROGCOMMAREA
054900         SET CDEMO-PGM-ENTER      TO TRUE
055000         SET CCARD-AID-ENTER      TO TRUE
055100         MOVE LIT-THISMAP         TO CDEMO-LAST-MAP
055200         SET CA-FIRST-PAGE        TO TRUE
055300         SET CA-LAST-PAGE-NOT-SHOWN TO TRUE
055400     END-IF
055500
055600******************************************************************
055700* If something is present in commarea
055800* and the from program is this program itself,
055900* read and edit the inputs given
056000*****************************************************************
056100     IF  EIBCALEN > 0
056200     AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM
056300         PERFORM 1000-RECEIVE-MAP
056400         THRU    1000-RECEIVE-MAP-EXIT
056500
056600     END-IF
056700*****************************************************************
056800* Check the mapped key  to see if its valid at this point       *
056900* F3    - Exit
057000* Enter - List of cards for current start key
057100* F8    - Page down
057200* F7    - Page up
057300*****************************************************************
057400     SET PFK-INVALID TO TRUE
057500     IF CCARD-AID-ENTER OR
057600        CCARD-AID-PFK02 OR
057700        CCARD-AID-PFK03 OR
057800        CCARD-AID-PFK07 OR
057900        CCARD-AID-PFK08 OR
058000       (CCARD-AID-PFK10 AND CA-DELETE-REQUESTED) OR
058100       (CCARD-AID-PFK10 AND CA-UPDATE-REQUESTED)
058200        SET PFK-VALID TO TRUE
058300     END-IF
058400
058500     IF PFK-INVALID
058600        SET CCARD-AID-ENTER TO TRUE
058700     END-IF
058800*****************************************************************
058900* If the user pressed PF3 go back to main menu
059000*****************************************************************
059100     IF CCARD-AID-PFK03
059200        IF CDEMO-FROM-TRANID     EQUAL LOW-VALUES
059300        OR CDEMO-FROM-TRANID     EQUAL SPACES
059400        OR CDEMO-FROM-TRANID     EQUAL LIT-THISTRANID
059500           MOVE LIT-ADMINTRANID   TO CDEMO-TO-TRANID
059600        ELSE
059700           MOVE CDEMO-FROM-TRANID TO CDEMO-TO-TRANID
059800        END-IF
059900
060000        IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES
060100        OR CDEMO-FROM-PROGRAM   EQUAL SPACES
060200        OR CDEMO-FROM-PROGRAM   EQUAL LIT-THISPGM
060300           MOVE LIT-ADMINPGM       TO CDEMO-TO-PROGRAM
060400        ELSE
060500           MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
060600        END-IF
060700
060800        MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
060900        MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM
061000
061100        SET  CDEMO-USRTYP-ADMIN TO TRUE
061200        SET  CDEMO-PGM-ENTER    TO TRUE
061300        MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET
061400        MOVE LIT-THISMAP        TO CDEMO-LAST-MAP
061500
061600        EXEC CICS
061700             SYNCPOINT
061800        END-EXEC
061900*
062000        EXEC CICS XCTL
062100             PROGRAM (CDEMO-TO-PROGRAM)
062200             COMMAREA(CARDDEMO-COMMAREA)
062300        END-EXEC
062400
062500     END-IF
062600
062700*****************************************************************
062800* If the user pressed PF2 transfer to add screen
062900*****************************************************************
063000     IF  (CCARD-AID-PFK02
063100     AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM)
063200        MOVE LIT-THISTRANID   TO CDEMO-FROM-TRANID
063300        MOVE LIT-THISPGM      TO CDEMO-FROM-PROGRAM
063400        SET  CDEMO-USRTYP-USER TO TRUE
063500        SET  CDEMO-PGM-ENTER  TO TRUE
063600        MOVE LIT-THISMAPSET   TO CDEMO-LAST-MAPSET
063700        MOVE LIT-THISMAP      TO CDEMO-LAST-MAP
063800        MOVE LIT-ADDTPGM      TO CDEMO-TO-PROGRAM
063900
064000        MOVE LIT-ADDTMAPSET   TO CCARD-NEXT-MAPSET
064100        MOVE LIT-ADDTMAP      TO CCARD-NEXT-MAP
064200        SET WS-EXIT-MESSAGE            TO TRUE
064300
064400*       CALL MENU PROGRAM
064500*
064600        SET CDEMO-PGM-ENTER   TO TRUE
064700*
064800        EXEC CICS XCTL
064900                  PROGRAM (LIT-ADDTPGM)
065000                  COMMAREA(CARDDEMO-COMMAREA)
065100        END-EXEC
065200     END-IF
065300
065400*****************************************************************
065500* If the user did not press PF8, lets reset the last page flag
065600*****************************************************************
065700     IF CCARD-AID-PFK08
065800        CONTINUE
065900     ELSE
066000        SET CA-LAST-PAGE-NOT-SHOWN   TO TRUE
066100     END-IF
066200*****************************************************************
066300*    If the user pressed F10 to confirm delete
066400*    But changed some criteria on screen. Treat it as ENTER
066500*****************************************************************
066600     IF  CCARD-AID-PFK10
066700         IF  (CA-DELETE-REQUESTED
066800         OR   CA-UPDATE-REQUESTED)
066900         AND FLG-TYPEFILTER-CHANGED-NO
067000         AND FLG-DESCFILTER-CHANGED-NO
067100         AND FLG-ROW-SELECTION-CHANGED-NO
067200             CONTINUE
067300         ELSE
067400             SET CCARD-AID-ENTER TO TRUE
067500         END-IF
067600     ELSE
067700        CONTINUE
067800     END-IF
067900
068000
068100*****************************************************************
068200*  Check Db2 connectivity. Quit if no Access.
068300*****************************************************************
068400     PERFORM 9998-PRIMING-QUERY
068500        THRU 9998-PRIMING-QUERY-EXIT
068600
068700     IF WS-DB2-ERROR
068800        PERFORM SEND-LONG-TEXT
068900           THRU SEND-LONG-TEXT-EXIT
069000        GO TO COMMON-RETURN
069100     END-IF
069200
069300
069400
069500*****************************************************************
069600* Now we decide what to do
069700*****************************************************************
069800     EVALUATE TRUE
069900         WHEN INPUT-ERROR
070000*****************************************************************
070100*        ASK FOR CORRECTIONS TO INPUTS
070200*****************************************************************
070300              MOVE WS-RETURN-MSG   TO CCARD-ERROR-MSG
070400              MOVE LIT-THISPGM     TO CDEMO-FROM-PROGRAM
070500              MOVE LIT-THISMAPSET  TO CDEMO-LAST-MAPSET
070600              MOVE LIT-THISMAP     TO CDEMO-LAST-MAP
070700
070800              MOVE LIT-THISPGM     TO CCARD-NEXT-PROG
070900              MOVE LIT-THISMAPSET  TO CCARD-NEXT-MAPSET
071000              MOVE LIT-THISMAP     TO CCARD-NEXT-MAP
071100              MOVE WS-CA-FIRST-TR-CODE
071200                                   TO WS-START-KEY
071300              IF  NOT FLG-TYPEFILTER-NOT-OK
071400              AND NOT FLG-DESCFILTER-NOT-OK
071500                 PERFORM 8000-READ-FORWARD
071600                    THRU 8000-READ-FORWARD-EXIT
071700              END-IF
071800              PERFORM 2000-SEND-MAP
071900                 THRU 2000-SEND-MAP-EXIT
072000              GO TO COMMON-RETURN
072100         WHEN CCARD-AID-PFK07
072200              AND CA-FIRST-PAGE
072300*****************************************************************
072400*        PAGE UP - PF7 - BUT ALREADY ON FIRST PAGE
072500*****************************************************************
072600         WHEN CCARD-AID-PFK07
072700              AND CA-FIRST-PAGE
072800              MOVE WS-CA-FIRST-TR-CODE
072900                            TO WS-START-KEY
073000              PERFORM 8000-READ-FORWARD
073100                 THRU 8000-READ-FORWARD-EXIT
073200              PERFORM 2000-SEND-MAP
073300                 THRU 2000-SEND-MAP-EXIT
073400              GO TO COMMON-RETURN
073500*****************************************************************
073600*        BACK - PF3 IF WE CAME FROM SOME OTHER PROGRAM
073700*****************************************************************
073800         WHEN CCARD-AID-PFK03
073900         WHEN CDEMO-PGM-REENTER AND
074000              CDEMO-FROM-PROGRAM NOT EQUAL LIT-THISPGM
074100
074200              INITIALIZE CARDDEMO-COMMAREA
074300                         WS-THIS-PROGCOMMAREA
074400                         WS-MISC-STORAGE
074500
074600              MOVE LIT-THISTRANID      TO CDEMO-FROM-TRANID
074700              MOVE LIT-THISPGM         TO CDEMO-FROM-PROGRAM
074800              MOVE LIT-THISMAP         TO CDEMO-LAST-MAP
074900              MOVE LIT-THISMAPSET      TO CDEMO-LAST-MAPSET
075000
075100              SET CDEMO-USRTYP-ADMIN   TO TRUE
075200              SET CDEMO-PGM-ENTER      TO TRUE
075300              SET CA-FIRST-PAGE        TO TRUE
075400              SET CA-LAST-PAGE-NOT-SHOWN TO TRUE
075500
075600              MOVE WS-CA-FIRST-TR-CODE TO WS-START-KEY
075700
075800              PERFORM 8000-READ-FORWARD
075900                 THRU 8000-READ-FORWARD-EXIT
076000              PERFORM 2000-SEND-MAP
076100                 THRU 2000-SEND-MAP-EXIT
076200              GO TO COMMON-RETURN
076300*****************************************************************
076400*        PAGE DOWN
076500*****************************************************************
076600         WHEN CCARD-AID-PFK08
076700              AND CA-NEXT-PAGE-EXISTS
076800              MOVE WS-CA-LAST-TR-CODE
076900                            TO WS-START-KEY
077000              ADD   +1      TO WS-CA-SCREEN-NUM
077100              PERFORM 8000-READ-FORWARD
077200                 THRU 8000-READ-FORWARD-EXIT
077300              INITIALIZE WS-EDIT-SELECT-FLAGS
077400              PERFORM 2000-SEND-MAP
077500                 THRU 2000-SEND-MAP-EXIT
077600              GO TO COMMON-RETURN
077700*****************************************************************
077800*        PAGE UP
077900*****************************************************************
078000         WHEN CCARD-AID-PFK07
078100              AND NOT CA-FIRST-PAGE
078200              MOVE WS-CA-FIRST-TR-CODE
078300                            TO WS-START-KEY
078400              SUBTRACT 1    FROM WS-CA-SCREEN-NUM
078500              PERFORM 8100-READ-BACKWARDS
078600                 THRU 8100-READ-BACKWARDS-EXIT
078700              INITIALIZE WS-EDIT-SELECT-FLAGS
078800              PERFORM 2000-SEND-MAP
078900                 THRU 2000-SEND-MAP-EXIT
079000              GO TO COMMON-RETURN
079100*****************************************************************
079200*        ENTER AND DELETE REQUESTED
079300*****************************************************************
079400         WHEN CCARD-AID-ENTER
079500          AND WS-DELETES-REQUESTED > 0
079600          AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM
079700              MOVE WS-CA-FIRST-TR-CODE
079800                                   TO WS-START-KEY
079900              IF  NOT FLG-TYPEFILTER-NOT-OK
080000              AND NOT FLG-DESCFILTER-NOT-OK
080100                 PERFORM 8000-READ-FORWARD
080200                    THRU 8000-READ-FORWARD-EXIT
080300              END-IF
080400              PERFORM 2000-SEND-MAP
080500                 THRU 2000-SEND-MAP-EXIT
080600              GO TO COMMON-RETURN
080700*****************************************************************
080800*        F10  AFTER DELETE CONFIRM REQUESTED
080900*****************************************************************
081000         WHEN CCARD-AID-PFK10
081100          AND WS-DELETES-REQUESTED > 0
081200          AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM
081300
081400              PERFORM 9300-DELETE-RECORD
081500                 THRU 9300-DELETE-RECORD-EXIT
081600
081700              IF CA-DELETE-SUCCEEDED
081800                 SET FLG-DELETED-YES   TO TRUE
081900              ELSE
082000                 SET FLG-DELETED-NO    TO TRUE
082100              END-IF
082200
082300              PERFORM 2000-SEND-MAP
082400                 THRU 2000-SEND-MAP-EXIT
082500
082600              IF FLG-DELETED-YES
082700                 INITIALIZE CARDDEMO-COMMAREA
082800                         WS-THIS-PROGCOMMAREA
082900                         WS-MISC-STORAGE
083000                 SET CDEMO-PGM-ENTER      TO TRUE
083100                 SET CA-FIRST-PAGE        TO TRUE
083200                 SET CA-LAST-PAGE-NOT-SHOWN TO TRUE
083300              END-IF
083400             GO TO COMMON-RETURN
083500*****************************************************************
083600*        ENTER AND UPDATE REQUESTED
083700*****************************************************************
083800         WHEN CCARD-AID-ENTER
083900          AND WS-UPDATES-REQUESTED > 0
084000          AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM
084100              MOVE WS-CA-FIRST-TR-CODE
084200                                   TO WS-START-KEY
084300              IF  NOT FLG-TYPEFILTER-NOT-OK
084400              AND NOT FLG-DESCFILTER-NOT-OK
084500                 PERFORM 8000-READ-FORWARD
084600                    THRU 8000-READ-FORWARD-EXIT
084700              END-IF
084800              PERFORM 2000-SEND-MAP
084900                 THRU 2000-SEND-MAP-EXIT
085000              GO TO COMMON-RETURN
085100*****************************************************************
085200*        F10  AFTER UPDATE CONFIRM REQUESTED
085300*****************************************************************
085400         WHEN CCARD-AID-PFK10
085500          AND WS-UPDATES-REQUESTED > 0
085600          AND CDEMO-FROM-PROGRAM  EQUAL LIT-THISPGM
085700
085800              PERFORM 9200-UPDATE-RECORD
085900                 THRU 9200-UPDATE-RECORD-EXIT
086000              IF CA-UPDATE-SUCCEEDED
086100                 SET FLG-UPDATE-COMPLETED TO TRUE
086200              END-IF
086300                MOVE WS-CA-FIRST-TR-CODE
086400                            TO WS-START-KEY
086500              PERFORM 8000-READ-FORWARD
086600                 THRU 8000-READ-FORWARD-EXIT
086700              PERFORM 2000-SEND-MAP
086800                 THRU 2000-SEND-MAP-EXIT
086900*****************************************************************
087000         WHEN OTHER
087100*****************************************************************
087200              MOVE WS-CA-FIRST-TR-CODE
087300                            TO WS-START-KEY
087400              PERFORM 8000-READ-FORWARD
087500                 THRU 8000-READ-FORWARD-EXIT
087600              PERFORM 2000-SEND-MAP
087700                 THRU 2000-SEND-MAP-EXIT
087800              GO TO COMMON-RETURN
087900     END-EVALUATE
088000
088100* If we had an error setup error message to display and return
088200     IF INPUT-ERROR
088300        MOVE WS-RETURN-MSG   TO CCARD-ERROR-MSG
088400        MOVE LIT-THISPGM     TO CDEMO-FROM-PROGRAM
088500        MOVE LIT-THISMAPSET  TO CDEMO-LAST-MAPSET
088600        MOVE LIT-THISMAP     TO CDEMO-LAST-MAP
088700
088800        MOVE LIT-THISPGM     TO CCARD-NEXT-PROG
088900        MOVE LIT-THISMAPSET  TO CCARD-NEXT-MAPSET
089000        MOVE LIT-THISMAP     TO CCARD-NEXT-MAP
089100
089200        GO TO COMMON-RETURN
089300     END-IF
089400
089500     MOVE LIT-THISPGM        TO CCARD-NEXT-PROG
089600     GO TO COMMON-RETURN
089700     .
089800
089900 COMMON-RETURN.
090000     MOVE  LIT-THISTRANID  TO CDEMO-FROM-TRANID
090100     MOVE  LIT-THISPGM     TO CDEMO-FROM-PROGRAM
090200     MOVE  LIT-THISMAPSET  TO CDEMO-LAST-MAPSET
090300     MOVE  LIT-THISMAP     TO CDEMO-LAST-MAP
090400     MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA
090500     MOVE  WS-THIS-PROGCOMMAREA TO
090600            WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
090700                         LENGTH OF WS-THIS-PROGCOMMAREA )
090800
090900
091000     EXEC CICS RETURN
091100          TRANSID (LIT-THISTRANID)
091200          COMMAREA (WS-COMMAREA)
091300          LENGTH(LENGTH OF WS-COMMAREA)
091400     END-EXEC
091500     .
091600 0000-MAIN-EXIT.
091700     EXIT
091800     .
091900 1000-RECEIVE-MAP.
092000     PERFORM 1100-RECEIVE-SCREEN
092100        THRU 1100-RECEIVE-SCREEN-EXIT
092200
092300     PERFORM 1200-EDIT-INPUTS
092400      THRU   1200-EDIT-INPUTS-EXIT
092500     .
092600 1000-RECEIVE-MAP-EXIT.
092700     EXIT
092800     .
092900
093000 1100-RECEIVE-SCREEN.
093100     EXEC CICS RECEIVE MAP(LIT-THISMAP)
093200                    MAPSET(LIT-THISMAPSET)
093300                    INTO(CTRTLIAI)
093400                    RESP(WS-RESP-CD)
093500     END-EXEC
093600
093700     MOVE TRTYPEI  OF CTRTLIAI  TO WS-IN-TYPE-CD
093800     MOVE TRDESCI  OF CTRTLIAI  TO WS-IN-TYPE-DESC
093900
094000     PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MAX-SCREEN-LINES
094100         MOVE TRTSELI(I)           TO WS-EDIT-SELECT(I)
094200         MOVE TRTTYPI(I)           TO WS-ROW-TR-CODE-IN(I)
094300
094400         MOVE LOW-VALUES           TO WS-ROW-TR-DESC-IN(I)
094500         IF   TRTYPDI(I) = LIT-ASTERISK
094600         OR   TRTYPDI(I) = SPACES
094700             CONTINUE
094800         ELSE
094900             MOVE FUNCTION TRIM(TRTYPDI(I))
095000                                   TO WS-ROW-TR-DESC-IN(I)
095100         END-IF
095200
095300     END-PERFORM
095400     .
095500
095600 1100-RECEIVE-SCREEN-EXIT.
095700     EXIT
095800     .
095900
096000 1200-EDIT-INPUTS.
096100
096200     SET INPUT-OK                   TO TRUE
096300     SET FLG-PROTECT-SELECT-ROWS-NO TO TRUE
096400
096500     PERFORM 1210-EDIT-ARRAY
096600        THRU 1210-EDIT-ARRAY-EXIT
096700
096800     PERFORM 1230-EDIT-DESC
096900        THRU 1230-EDIT-DESC-EXIT
097000
097100     PERFORM 1220-EDIT-TYPECD
097200        THRU 1220-EDIT-TYPECD-EXIT
097300
097400     PERFORM 1290-CROSS-EDITS
097500        THRU 1290-CROSS-EDITS-EXIT
097600     .
097700
097800 1200-EDIT-INPUTS-EXIT.
097900     EXIT
098000     .
098100
098200 1210-EDIT-ARRAY.
098300
098400     MOVE ZERO                     TO WS-ACTIONS-REQUESTED
098500                                      WS-NO-ACTIONS-SELECTED
098600                                      WS-DELETES-REQUESTED
098700                                      WS-UPDATES-REQUESTED
098800                                      WS-VALID-ACTIONS-SELECTED
098900
099000
099100     IF  FLG-TYPEFILTER-CHANGED-YES
099200     OR  FLG-DESCFILTER-CHANGED-YES
099300         INITIALIZE                 WS-EDIT-SELECT-FLAGS
099400         GO TO 1210-EDIT-ARRAY-EXIT
099500     ELSE
099600
099700     INSPECT  WS-EDIT-SELECT-FLAGS
099800     TALLYING WS-NO-ACTIONS-SELECTED FOR ALL SPACES
099900                                         LOW-VALUES
100000              WS-DELETES-REQUESTED   FOR ALL LIT-DELETE-FLAG
100100              WS-UPDATES-REQUESTED   FOR ALL LIT-UPDATE-FLAG
100200
100300     COMPUTE  WS-ACTIONS-REQUESTED
100400           =  WS-MAX-SCREEN-LINES
100500           -  WS-NO-ACTIONS-SELECTED
100600     END-COMPUTE
100700
100800
100900     COMPUTE WS-VALID-ACTIONS-SELECTED =
101000             WS-DELETES-REQUESTED
101100           + WS-UPDATES-REQUESTED
101200     END-COMPUTE
101300
101400     MOVE ZERO TO I-SELECTED
101500     SET FLG-BAD-ACTIONS-SELECTED-NO    TO TRUE
101600
101700     PERFORM VARYING I
101800                FROM WS-MAX-SCREEN-LINES
101900                  BY -1
102000               UNTIL I = 0
102100         EVALUATE TRUE
102200           WHEN SELECT-OK(I)
102300             MOVE I TO I-SELECTED
102400             IF WS-MORETHAN1ACTION
102500                MOVE '1' TO WS-ROW-TRTSELECT-ERROR(I)
102600                SET FLG-BAD-ACTIONS-SELECTED-YES   TO TRUE
102700             END-IF
102800             IF UPDATE-REQUESTED-ON(I)
102900                PERFORM 1211-EDIT-ARRAY-DESC
103000                   THRU 1211-EDIT-ARRAY-DESC-EXIT
103100             END-IF
103200           WHEN SELECT-BLANK(I)
103300             CONTINUE
103400           WHEN OTHER
103500             SET INPUT-ERROR TO TRUE
103600             MOVE '1' TO WS-ROW-TRTSELECT-ERROR(I)
103700             SET FLG-BAD-ACTIONS-SELECTED-YES     TO TRUE
103800             SET WS-MESG-INVALID-ACTION-CODE      TO TRUE
103900        END-EVALUATE
104000     END-PERFORM
104100
104200     IF I-SELECTED EQUAL  WS-CA-ROW-SELECTED
104300        SET FLG-ROW-SELECTION-CHANGED-NO          TO TRUE
104400     ELSE
104500        SET FLG-ROW-SELECTION-CHANGED-YES         TO TRUE
104600        MOVE I-SELECTED TO   WS-CA-ROW-SELECTED
104700     END-IF
104800
104900     IF WS-MORETHAN1ACTION
105000         SET INPUT-ERROR                          TO TRUE
105100         SET WS-MESG-MORE-THAN-1-ACTION           TO TRUE
105200     END-IF
105300     .
105400
105500 1210-EDIT-ARRAY-EXIT.
105600      EXIT
105700      .
105800
105900
106000 1211-EDIT-ARRAY-DESC.
106100
106200      SET NO-CHANGES-FOUND           TO TRUE
106300
106400     IF  FUNCTION UPPER-CASE (
106500         FUNCTION TRIM (WS-ROW-TR-DESC-IN(I)))=
106600         FUNCTION UPPER-CASE (
106700         FUNCTION TRIM (WS-CA-ROW-TR-DESC-OUT(I)))
106800     AND FUNCTION LENGTH (
106900         FUNCTION TRIM (WS-ROW-TR-DESC-IN(I)))=
107000         FUNCTION LENGTH (
107100         FUNCTION TRIM (WS-CA-ROW-TR-DESC-OUT(I)))
107200         SET WS-MESG-NO-CHANGES-DETECTED   TO TRUE
107300         GO TO 1211-EDIT-ARRAY-DESC-EXIT
107400     ELSE
107500         SET CHANGES-HAVE-OCCURRED    TO TRUE
107600     END-IF
107700
107800     SET FLG-ROW-DESCRIPTION-NOT-OK  TO TRUE
107900
108000******************************************************************
108100*    Edit Description
108200******************************************************************
108300     MOVE 'Transaction Desc'       TO WS-EDIT-VARIABLE-NAME
108400     MOVE WS-ROW-TR-DESC-IN(I)     TO WS-EDIT-ALPHANUM-ONLY
108500     MOVE 50                       TO WS-EDIT-ALPHANUM-LENGTH
108600     PERFORM 1240-EDIT-ALPHANUM-REQD
108700        THRU 1240-EDIT-ALPHANUM-REQD-EXIT
108800     MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS
108900                                   TO WS-ARRAY-DESCRIPTION-FLGS
109000     .
109100
109200 1211-EDIT-ARRAY-DESC-EXIT.
109300     EXIT
109400     .
109500
109600 1220-EDIT-TYPECD.
109700
109800     SET FLG-TYPEFILTER-BLANK TO TRUE
109900
110000*    Not supplied
110100     IF WS-IN-TYPE-CD   EQUAL LOW-VALUES
110200     OR WS-IN-TYPE-CD   EQUAL SPACES
110300     OR WS-IN-TYPE-CD   EQUAL ZEROS
110400        SET FLG-TYPEFILTER-BLANK  TO TRUE
110500        MOVE ZEROES       TO WS-TYPE-CD-FILTER
110600        GO TO  1220-EDIT-TYPECD-EXIT
110700     END-IF
110800*
110900*    Not numeric
111000*    Not 2 characters
111100     IF WS-IN-TYPE-CD  IS NOT NUMERIC
111200        SET INPUT-ERROR TO TRUE
111300        SET FLG-TYPEFILTER-NOT-OK TO TRUE
111400        SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE
111500        MOVE
111600        'TYPE CODE FILTER,IF SUPPLIED MUST BE A 2 DIGIT NUMBER'
111700                        TO WS-RETURN-MSG
111800        GO TO 1220-EDIT-TYPECD-EXIT
111900     ELSE
112000        MOVE WS-IN-TYPE-CD TO WS-TYPE-CD-FILTER
112100        SET FLG-TYPEFILTER-ISVALID TO TRUE
112200     END-IF
112300     .
112400
112500 1220-EDIT-TYPECD-EXIT.
112600
112700     IF WS-IN-TYPE-CD EQUAL WS-CA-TYPE-CD
112800     OR FLG-TYPEFILTER-BLANK
112900                      AND  (WS-CA-TYPE-CD EQUAL ZEROES
113000                       OR   WS-CA-TYPE-CD EQUAL LOW-VALUES
113100                       OR   WS-CA-TYPE-CD EQUAL SPACES)
113200        SET FLG-TYPEFILTER-CHANGED-NO  TO TRUE
113300     ELSE
113400        INITIALIZE WS-CA-PAGING-VARIABLES
113500        MOVE WS-IN-TYPE-CD             TO WS-CA-TYPE-CD
113600        SET FLG-TYPEFILTER-CHANGED-YES TO TRUE
113700     END-IF
113800
113900     EXIT
114000     .
114100
114200 1230-EDIT-DESC.
114300
114400     SET FLG-DESCFILTER-BLANK TO TRUE
114500
114600*    Not supplied
114700     IF WS-IN-TYPE-DESC   EQUAL LOW-VALUES
114800     OR WS-IN-TYPE-DESC   EQUAL SPACES
114900        SET FLG-DESCFILTER-BLANK  TO TRUE
115000        GO TO 1230-EDIT-DESC-EXIT
115100     ELSE
115200        SET FLG-DESCFILTER-ISVALID TO TRUE
115300     END-IF
115400
115500     IF FLG-DESCFILTER-ISVALID
115600        STRING '%'
115700               FUNCTION TRIM(WS-IN-TYPE-DESC)
115800               '%'
115900         DELIMITED BY SIZE
116000         INTO
116100         WS-TYPE-DESC-FILTER
116200        END-STRING
116300     END-IF
116400     .
116500 1230-EDIT-DESC-EXIT.
116600     IF WS-IN-TYPE-DESC EQUAL WS-CA-TYPE-DESC
116700     OR FLG-DESCFILTER-BLANK
116800                      AND  (WS-CA-TYPE-DESC EQUAL LOW-VALUES
116900                       OR   WS-CA-TYPE-DESC EQUAL SPACES)
117000        SET FLG-DESCFILTER-CHANGED-NO   TO TRUE
117100     ELSE
117200        INITIALIZE WS-CA-PAGING-VARIABLES
117300        MOVE WS-IN-TYPE-DESC            TO WS-CA-TYPE-DESC
117400        SET FLG-DESCFILTER-CHANGED-YES  TO TRUE
117500     END-IF
117600
117700     EXIT
117800     .
117900
118000
118100 1240-EDIT-ALPHANUM-REQD.
118200*    Initialize
118300     SET FLG-ALPHNANUM-NOT-OK          TO TRUE
118400
118500*    Not supplied
118600     IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
118700                                       EQUAL LOW-VALUES
118800     OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
118900         EQUAL SPACES
119000     OR FUNCTION LENGTH(FUNCTION TRIM(
119100        WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0
119200
119300        SET INPUT-ERROR                TO TRUE
119400        SET FLG-ALPHNANUM-BLANK        TO TRUE
119500        IF WS-RETURN-MSG-OFF
119600           STRING
119700             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
119800             ' must be supplied.'
119900             DELIMITED BY SIZE
120000             INTO WS-RETURN-MSG
120100           END-STRING
120200        END-IF
120300
120400        GO TO  1240-EDIT-ALPHANUM-REQD-EXIT
120500     END-IF
120600
120700*    Only Alphabets,numbers and space allowed
120800     MOVE LIT-ALL-ALPHANUM-FROM-X TO LIT-ALL-ALPHANUM-FROM
120900
121000     INSPECT WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
121100       CONVERTING LIT-ALL-ALPHANUM-FROM
121200               TO LIT-ALPHANUM-SPACES-TO
121300
121400     IF FUNCTION LENGTH(
121500             FUNCTION TRIM(
121600             WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)
121700                            )) = 0
121800        CONTINUE
121900     ELSE
122000        SET INPUT-ERROR           TO TRUE
122100        SET FLG-ALPHNANUM-NOT-OK  TO TRUE
122200        IF WS-RETURN-MSG-OFF
122300           STRING
122400             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)
122500             ' can have numbers or alphabets only.'
122600             DELIMITED BY SIZE
122700             INTO WS-RETURN-MSG
122800           END-STRING
122900        END-IF
123000        GO TO  1240-EDIT-ALPHANUM-REQD-EXIT
123100     END-IF
123200
123300     SET FLG-ALPHNANUM-ISVALID    TO TRUE
123400     .
123500 1240-EDIT-ALPHANUM-REQD-EXIT.
123600     EXIT
123700     .
123800
123900 1290-CROSS-EDITS.
124000
124100     IF FLG-TYPEFILTER-ISVALID
124200     OR FLG-DESCFILTER-ISVALID
124300        CONTINUE
124400     ELSE
124500         GO TO 1290-CROSS-EDITS-EXIT
124600     END-IF
124700
124800     PERFORM 9100-CHECK-FILTERS
124900        THRU 9100-CHECK-FILTERS-EXIT
125000
125100     IF WS-RECORDS-COUNT = 0
125200        SET INPUT-ERROR TO TRUE
125300        IF FLG-TYPEFILTER-ISVALID
125400           SET FLG-TYPEFILTER-NOT-OK TO TRUE
125500        END-IF
125600
125700        IF FLG-DESCFILTER-ISVALID
125800           SET FLG-DESCFILTER-NOT-OK TO TRUE
125900        END-IF
126000
126100
126200        SET FLG-PROTECT-SELECT-ROWS-YES TO TRUE
126300        MOVE
126400        'No Records found for these filter conditions'
126500                        TO WS-RETURN-MSG
126600        GO TO 1290-CROSS-EDITS-EXIT
126700     END-IF
126800     .
126900 1290-CROSS-EDITS-EXIT.
127000     EXIT
127100     .
127200
127300
127400 2000-SEND-MAP
127500      .
127600     PERFORM 2100-SCREEN-INIT
127700        THRU 2100-SCREEN-INIT-EXIT
127800     PERFORM 2200-SETUP-ARRAY-ATTRIBS
127900        THRU 2200-SETUP-ARRAY-ATTRIBS-EXIT
128000     PERFORM 2300-SCREEN-ARRAY-INIT
128100        THRU 2300-SCREEN-ARRAY-INIT-EXIT
128200     PERFORM 2400-SETUP-SCREEN-ATTRS
128300        THRU 2400-SETUP-SCREEN-ATTRS-EXIT
128400     PERFORM 2500-SETUP-MESSAGE
128500        THRU 2500-SETUP-MESSAGE-EXIT
128600     PERFORM 2600-SEND-SCREEN
128700        THRU 2600-SEND-SCREEN-EXIT
128800     .
128900
129000 2000-SEND-MAP-EXIT.
129100     EXIT
129200     .
129300 2100-SCREEN-INIT.
129400     MOVE LOW-VALUES             TO CTRTLIAO
129500
129600     MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA
129700
129800     MOVE CCDA-TITLE01           TO TITLE01O OF CTRTLIAO
129900     MOVE CCDA-TITLE02           TO TITLE02O OF CTRTLIAO
130000     MOVE LIT-THISTRANID         TO TRNNAMEO OF CTRTLIAO
130100     MOVE LIT-THISPGM            TO PGMNAMEO OF CTRTLIAO
130200
130300     MOVE FUNCTION CURRENT-DATE  TO WS-CURDATE-DATA
130400
130500     MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
130600     MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
130700     MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY
130800
130900     MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CTRTLIAO
131000
131100     MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
131200     MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
131300     MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS
131400
131500     MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CTRTLIAO
131600*    PAGE NUMBER
131700*
131800     MOVE WS-CA-SCREEN-NUM       TO PAGENOO  OF CTRTLIAO
131900
132000     SET WS-NO-INFO-MESSAGE      TO TRUE
132100     MOVE WS-INFO-MSG            TO INFOMSGO OF CTRTLIAO
132200     MOVE DFHBMDAR               TO INFOMSGC OF CTRTLIAO
132300     .
132400
132500 2100-SCREEN-INIT-EXIT.
132600     EXIT
132700     .
132800
132900 2200-SETUP-ARRAY-ATTRIBS.
133000*    REPLACE BMS GENERATED MAP WITH PROVIDED COPYBOOK
133100*    AND CLEAN UP REPETITIVE CODE !!
133200
133300     PERFORM VARYING I
133400                FROM WS-MAX-SCREEN-LINES
133500                  BY -1
133600               UNTIL I = 0
133700        MOVE DFHBMPRF                 TO TRTYPDA(I)
133800
133900        IF   WS-CA-EACH-ROW-OUT(I)    EQUAL LOW-VALUES
134000        OR   FLG-PROTECT-SELECT-ROWS-YES
134100           MOVE DFHBMPRO              TO TRTSELA (I)
134200        ELSE
134300           IF WS-ROW-TRTSELECT-ERROR(I) = '1'
134400              MOVE DFHRED             TO TRTSELC(I)
134500              MOVE -1                 TO TRTSELL(I)
134600           END-IF
134700
134800           IF DELETE-REQUESTED-ON(I)
134900           AND WS-ONLY-1-VALID-ACTION
135000           AND FLG-BAD-ACTIONS-SELECTED-NO
135100              MOVE DFHNEUTR           TO TRTTYPC(I)
135200                                         TRTYPDC(I)
135300              MOVE -1                 TO TRTSELL(I)
135400           END-IF
135500
135600           IF UPDATE-REQUESTED-ON(I)
135700           AND WS-ONLY-1-VALID-ACTION
135800           AND FLG-BAD-ACTIONS-SELECTED-NO
135900              MOVE DFHNEUTR           TO TRTTYPC(I)
136000              IF  FLG-UPDATE-COMPLETED
136100                  MOVE -1             TO TRTSELL(I)
136200                  MOVE DFHNEUTR       TO TRTYPDC(I)
136300              ELSE
136400                  MOVE -1             TO TRTYPDL(I)
136500                  MOVE DFHBMFSE       TO TRTYPDA(I)
136600                  IF NOT FLG-ROW-DESCRIPTION-ISVALID
136700                     MOVE DFHRED      TO TRTYPDC(I)
136800                  END-IF
136900              END-IF
137000           END-IF
137100           MOVE DFHBMFSE              TO TRTSELA(I)
137200        END-IF
137300     END-PERFORM
137400     .
137500
137600
137700 2200-SETUP-ARRAY-ATTRIBS-EXIT.
137800     EXIT
137900     .
138000
138100
138200
138300 2300-SCREEN-ARRAY-INIT.
138400*    USING REDEFINES TO AVOID UP REPETITIVE CODE !!
138500*
138600     PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-MAX-SCREEN-LINES
138700
138800        IF   WS-CA-EACH-ROW-OUT(I)         EQUAL LOW-VALUES
138900           CONTINUE
139000        ELSE
139100           IF  DELETE-REQUESTED-ON(I)
139200           AND WS-ONLY-1-VALID-ACTION
139300           AND FLG-BAD-ACTIONS-SELECTED-NO
139400               IF  FLG-DELETED-YES
139500                   SET SELECT-BLANK(I)          TO TRUE
139600               ELSE
139700                   SET CA-DELETE-REQUESTED      TO TRUE
139800               END-IF
139900           END-IF
140000
140100*          Type code
140200           MOVE WS-CA-ROW-TR-CODE-OUT(I)        TO TRTTYPO(I)
140300*          Type Description
140400           IF UPDATE-REQUESTED-ON(I)
140500           AND WS-ONLY-1-VALID-ACTION
140600           AND FLG-BAD-ACTIONS-SELECTED-NO
140700               IF  FLG-UPDATE-COMPLETED
140800                   SET SELECT-BLANK(I)          TO TRUE
140900               ELSE
141000                   SET CA-UPDATE-REQUESTED      TO TRUE
141100               END-IF
141200               IF CHANGES-HAVE-OCCURRED
141300                  EVALUATE TRUE
141400                      WHEN FLG-ROW-DESCRIPTION-BLANK
141500                           MOVE LIT-ASTERISK    TO TRTYPDO(I)
141600                      WHEN OTHER
141700                           MOVE WS-ROW-TR-DESC-IN(I)
141800                                                TO TRTYPDO(I)
141900                  END-EVALUATE
142000               ELSE
142100                  MOVE WS-CA-ROW-TR-DESC-OUT(I) TO TRTYPDO(I)
142200               END-IF
142300           ELSE
142400               MOVE WS-CA-ROW-TR-DESC-OUT(I)    TO TRTYPDO(I)
142500           END-IF
142600
142700*          Select flag because we may update it above
142800           MOVE WS-EDIT-SELECT(I)               TO TRTSELO(I)
142900        END-IF
143000     END-PERFORM
143100     .
143200
143300 2300-SCREEN-ARRAY-INIT-EXIT.
143400     EXIT
143500     .
143600
143700
143800 2400-SETUP-SCREEN-ATTRS.
143900*    INITIALIZE SEARCH CRITERIA
144000     IF EIBCALEN = 0
144100     OR (CDEMO-PGM-ENTER
144200     AND CDEMO-FROM-PROGRAM = LIT-ADMINPGM)
144300        CONTINUE
144400     ELSE
144500        EVALUATE TRUE
144600            WHEN  WS-ACTIONS-REQUESTED > 0
144700               MOVE WS-IN-TYPE-CD    TO TRTYPEO OF CTRTLIAO
144800               MOVE DFHBMASF         TO TRTYPEA OF CTRTLIAI
144900               MOVE DFHBLUE          TO TRTYPEC OF CTRTLIAO
145000            WHEN FLG-TYPEFILTER-ISVALID
145100            WHEN FLG-TYPEFILTER-NOT-OK
145200               MOVE WS-IN-TYPE-CD    TO TRTYPEO OF CTRTLIAO
145300               MOVE DFHBMFSE         TO TRTYPEA OF CTRTLIAI
145400            WHEN WS-IN-TYPE-CD = 0
145500               MOVE LOW-VALUES       TO TRTYPEO OF CTRTLIAO
145600            WHEN OTHER
145700              MOVE LOW-VALUES        TO TRTYPEO OF CTRTLIAO
145800              MOVE DFHBMFSE          TO TRTYPEA OF CTRTLIAI
145900        END-EVALUATE
146000
146100        EVALUATE TRUE
146200            WHEN WS-ACTIONS-REQUESTED > 0
146300               MOVE WS-IN-TYPE-DESC  TO TRDESCO OF CTRTLIAO
146400               MOVE DFHBMASF         TO TRDESCA OF CTRTLIAI
146500               MOVE DFHBLUE          TO TRDESCC OF CTRTLIAO
146600            WHEN FLG-DESCFILTER-ISVALID
146700            WHEN FLG-DESCFILTER-NOT-OK
146800               MOVE WS-IN-TYPE-DESC  TO TRDESCO OF CTRTLIAO
146900               MOVE DFHBMFSE         TO TRDESCA OF CTRTLIAI
147000            WHEN OTHER
147100              MOVE DFHBMFSE          TO TRDESCA OF CTRTLIAI
147200        END-EVALUATE
147300     END-IF
147400
147500*    POSITION CURSOR
147600
147700     IF FLG-TYPEFILTER-NOT-OK
147800        MOVE  DFHRED                 TO TRTYPEC OF CTRTLIAO
147900        MOVE  -1                     TO TRTYPEL OF CTRTLIAI
148000     END-IF
148100
148200     IF FLG-DESCFILTER-NOT-OK
148300        MOVE  DFHRED                 TO TRDESCC OF CTRTLIAO
148400        MOVE  -1                     TO TRDESCL OF CTRTLIAI
148500     END-IF
148600
148700
148800*    IF NO ERRORS POSITION CURSOR
148900     IF INPUT-OK
149000        IF WS-ACTIONS-REQUESTED > 0
149100        AND NOT CCARD-AID-PFK07
149200        AND NOT CCARD-AID-PFK08
149300            CONTINUE
149400        ELSE
149500            MOVE   -1                 TO TRTYPEL OF CTRTLIAI
149600        END-IF
149700     END-IF
149800     .
149900 2400-SETUP-SCREEN-ATTRS-EXIT.
150000     EXIT
150100     .
150200
150300
150400 2500-SETUP-MESSAGE.
150500*    SETUP MESSAGE
150600     EVALUATE TRUE
150700          WHEN FLG-DELETED-YES
150800               SET WS-INFORM-DELETE-SUCCESS TO TRUE
150900          WHEN FLG-UPDATE-COMPLETED
151000               SET WS-INFORM-UPDATE-SUCCESS TO TRUE
151100          WHEN FLG-TYPEFILTER-NOT-OK
151200          WHEN FLG-DESCFILTER-NOT-OK
151300            CONTINUE
151400          WHEN CCARD-AID-ENTER
151500          AND WS-DELETES-REQUESTED > 0
151600          AND WS-ONLY-1-ACTION
151700          AND WS-ONLY-1-VALID-ACTION
151800             IF  WS-NO-INFO-MESSAGE
151900             AND FLG-TYPEFILTER-CHANGED-NO
152000             AND FLG-DESCFILTER-CHANGED-NO
152100                SET WS-INFORM-DELETE        TO TRUE
152200             END-IF
152300          WHEN CCARD-AID-ENTER
152400          AND WS-UPDATES-REQUESTED > 0
152500          AND WS-ONLY-1-ACTION
152600          AND WS-ONLY-1-VALID-ACTION
152700             IF  WS-NO-INFO-MESSAGE
152800             AND FLG-TYPEFILTER-CHANGED-NO
152900             AND FLG-DESCFILTER-CHANGED-NO
153000                SET WS-INFORM-UPDATE        TO TRUE
153100             END-IF
153200          WHEN CCARD-AID-PFK07
153300              AND CA-FIRST-PAGE
153400            MOVE 'No previous pages to display'
153500            TO WS-RETURN-MSG
153600          WHEN CCARD-AID-PFK08
153700           AND CA-NEXT-PAGE-NOT-EXISTS
153800           AND CA-LAST-PAGE-SHOWN
153900            MOVE 'No more pages to display'
154000            TO WS-RETURN-MSG
154100          WHEN CCARD-AID-PFK08
154200           AND CA-NEXT-PAGE-NOT-EXISTS
154300            IF WS-NO-INFO-MESSAGE
154400               SET WS-INFORM-REC-ACTIONS    TO TRUE
154500            END-IF
154600            IF  CA-LAST-PAGE-NOT-SHOWN
154700            AND CA-NEXT-PAGE-NOT-EXISTS
154800                SET CA-LAST-PAGE-SHOWN      TO TRUE
154900            END-IF
155000          WHEN WS-NO-INFO-MESSAGE
155100          WHEN CA-NEXT-PAGE-EXISTS
155200            SET WS-INFORM-REC-ACTIONS       TO TRUE
155300          WHEN OTHER
155400             SET WS-NO-INFO-MESSAGE         TO TRUE
155500     END-EVALUATE
155600
155700     MOVE WS-RETURN-MSG          TO ERRMSGO OF CTRTLIAO
155800
155900
156000* Center justify the text
156100*
156200     COMPUTE WS-STRING-LEN =
156300             FUNCTION LENGTH(
156400                      FUNCTION TRIM(WS-INFO-MSG)
156500                            )
156600     COMPUTE WS-STRING-MID =
156700            (FUNCTION LENGTH(WS-INFO-MSG)
156800                          - WS-STRING-LEN) / 2 + 1
156900     MOVE WS-INFO-MSG(1:WS-STRING-LEN)
157000       TO WS-STRING-OUT(WS-STRING-MID:
157100                        WS-STRING-LEN)
157200
157300
157400
157500     IF  NOT WS-NO-INFO-MESSAGE
157600     AND NOT WS-MESG-NO-RECORDS-FOUND
157700        MOVE WS-STRING-OUT      TO INFOMSGO OF CTRTLIAO
157800        MOVE DFHNEUTR           TO INFOMSGC OF CTRTLIAO
157900     END-IF
158000
158100     .
158200 2500-SETUP-MESSAGE-EXIT.
158300     EXIT
158400     .
158500
158600
158700 2600-SEND-SCREEN.
158800     EXEC CICS SEND MAP(LIT-THISMAP)
158900                    MAPSET(LIT-THISMAPSET)
159000                    FROM(CTRTLIAO)
159100                    CURSOR
159200                    ERASE
159300                    RESP(WS-RESP-CD)
159400                    FREEKB
159500     END-EXEC
159600     .
159700 2600-SEND-SCREEN-EXIT.
159800     EXIT
159900     .
160000
160100
160200
160300 8000-READ-FORWARD.
160400     MOVE LOW-VALUES           TO WS-CA-ALL-ROWS-OUT
160500
160600*****************************************************************
160700*    Start Reading
160800*****************************************************************
160900     PERFORM 9400-OPEN-FORWARD-CURSOR
161000        THRU 9400-OPEN-FORWARD-CURSOR-EXIT
161100
161200     IF WS-DB2-ERROR
161300        GO TO 8000-READ-FORWARD-EXIT
161400     END-IF
161500*****************************************************************
161600*    Loop through records and fetch max screen records
161700*****************************************************************
161800     MOVE ZEROES TO WS-ROW-NUMBER
161900     SET CA-NEXT-PAGE-EXISTS    TO TRUE
162000     SET MORE-RECORDS-TO-READ   TO TRUE
162100
162200     PERFORM UNTIL READ-LOOP-EXIT
162300
162400     INITIALIZE DCLTRANSACTION-TYPE
162500
162600     EXEC SQL
162700          FETCH C-TR-TYPE-FORWARD
162800          INTO :DCL-TR-TYPE
162900              ,:DCL-TR-DESCRIPTION
163000     END-EXEC
163100
163200     MOVE SQLCODE               TO WS-DISP-SQLCODE
163300
163400     EVALUATE TRUE
163500         WHEN SQLCODE = ZERO
163600             ADD 1              TO WS-ROW-NUMBER
163700
163800             MOVE DCL-TR-TYPE   TO WS-CA-ROW-TR-CODE-OUT(
163900             WS-ROW-NUMBER)
164000
164100             MOVE DCL-TR-DESCRIPTION-TEXT
164200                                TO WS-CA-ROW-TR-DESC-OUT(
164300                                WS-ROW-NUMBER)
164400             IF WS-ROW-NUMBER = 1
164500                MOVE DCL-TR-TYPE  TO WS-CA-FIRST-TR-CODE
164600                IF   WS-CA-SCREEN-NUM = 0
164700                     ADD   +1     TO WS-CA-SCREEN-NUM
164800                ELSE
164900                    CONTINUE
165000                END-IF
165100             ELSE
165200                CONTINUE
165300             END-IF
165400******************************************************************
165500*            Max Screen size
165600******************************************************************
165700             IF WS-ROW-NUMBER = WS-MAX-SCREEN-LINES
165800                SET READ-LOOP-EXIT  TO TRUE
165900                MOVE DCL-TR-TYPE    TO WS-CA-LAST-TR-CODE
166000
166100                EXEC SQL
166200                         FETCH C-TR-TYPE-FORWARD
166300                         INTO :DCL-TR-TYPE
166400                             ,:DCL-TR-DESCRIPTION
166500                END-EXEC
166600
166700                MOVE SQLCODE        TO WS-DISP-SQLCODE
166800
166900                EVALUATE TRUE
167000                   WHEN SQLCODE = ZERO
167100                        SET CA-NEXT-PAGE-EXISTS
167200                                          TO TRUE
167300                        MOVE DCL-TR-TYPE  TO WS-CA-LAST-TR-CODE
167400                   WHEN SQLCODE = +100
167500                      SET CA-NEXT-PAGE-NOT-EXISTS     TO TRUE
167600
167700                      IF WS-RETURN-MSG-OFF
167800                      AND CCARD-AID-PFK08
167900                          SET WS-MESG-NO-MORE-RECORDS TO TRUE
168000                      END-IF
168100                   WHEN OTHER
168200*                     This is some kind of error. Close Cursor
168300*                     And exit
168400                      SET READ-LOOP-EXIT      TO TRUE
168500                      IF WS-RETURN-MSG-OFF
168600                         MOVE 'C-TR-TYPE-FORWARD fetch'
168700                                              TO
168800                                            WS-DB2-CURRENT-ACTION
168900                         PERFORM 9999-FORMAT-DB2-MESSAGE
169000                            THRU 9999-FORMAT-DB2-MESSAGE-EXIT
169100                      END-IF
169200                END-EVALUATE
169300            END-IF
169400        WHEN SQLCODE = +100
169500            SET READ-LOOP-EXIT              TO TRUE
169600            SET CA-NEXT-PAGE-NOT-EXISTS     TO TRUE
169700            MOVE DCL-TR-TYPE                TO WS-CA-LAST-TR-CODE
169800            IF WS-RETURN-MSG-OFF
169900            AND CCARD-AID-PFK08
170000               SET  WS-MESG-NO-MORE-RECORDS     TO TRUE
170100            END-IF
170200            IF WS-CA-SCREEN-NUM = 1
170300            AND WS-ROW-NUMBER = 0
170400                SET WS-MESG-NO-RECORDS-FOUND    TO TRUE
170500            END-IF
170600         WHEN OTHER
170700*           This is some kind of error. Change to END BR
170800*           And exit
170900            SET READ-LOOP-EXIT             TO TRUE
171000            SET WS-DB2-ERROR               TO TRUE
171100            IF WS-RETURN-MSG-OFF
171200              MOVE 'C-TR-TYPE-FORWARD close'
171300                              TO WS-DB2-CURRENT-ACTION
171400
171500              PERFORM 9999-FORMAT-DB2-MESSAGE
171600                 THRU 9999-FORMAT-DB2-MESSAGE-EXIT
171700             END-IF
171800     END-EVALUATE
171900     END-PERFORM
172000
172100     PERFORM 9450-CLOSE-FORWARD-CURSOR
172200        THRU 9450-CLOSE-FORWARD-CURSOR-EXIT
172300     .
172400 8000-READ-FORWARD-EXIT.
172500     EXIT
172600     .
172700 8100-READ-BACKWARDS.
172800
172900     MOVE LOW-VALUES           TO WS-CA-ALL-ROWS-OUT
173000
173100     MOVE WS-CA-FIRST-TTYPEKEY TO WS-CA-LAST-TTYPEKEY
173200*****************************************************************
173300*    Loop through records and fetch max screen records
173400*****************************************************************
173500     COMPUTE WS-ROW-NUMBER =
173600                             WS-MAX-SCREEN-LINES
173700     END-COMPUTE
173800     SET CA-NEXT-PAGE-EXISTS    TO TRUE
173900     SET MORE-RECORDS-TO-READ   TO TRUE
174000
174100*****************************************************************
174200*    Now we show the records from previous set.
174300*****************************************************************
174400*    Start Reading Backwards
174500*****************************************************************
174600     PERFORM 9500-OPEN-BACKWARD-CURSOR
174700        THRU 9500-OPEN-BACKWARD-CURSOR-EXIT
174800
174900     PERFORM UNTIL READ-LOOP-EXIT
175000
175100     INITIALIZE DCLTRANSACTION-TYPE
175200
175300     EXEC SQL
175400          FETCH C-TR-TYPE-BACKWARD
175500          INTO :DCL-TR-TYPE
175600              ,:DCL-TR-DESCRIPTION
175700     END-EXEC
175800
175900     MOVE SQLCODE               TO WS-DISP-SQLCODE
176000
176100     EVALUATE TRUE
176200         WHEN SQLCODE = ZERO
176300              MOVE DCL-TR-TYPE
176400                          TO WS-CA-ROW-TR-CODE-OUT(WS-ROW-NUMBER)
176500              MOVE DCL-TR-DESCRIPTION-TEXT
176600                          TO
176700                          WS-CA-ROW-TR-DESC-OUT(WS-ROW-NUMBER)
176800
176900              SUBTRACT 1  FROM WS-ROW-NUMBER
177000              IF WS-ROW-NUMBER = 0
177100                 SET READ-LOOP-EXIT  TO TRUE
177200                 MOVE DCL-TR-TYPE
177300                          TO WS-CA-FIRST-TR-CODE
177400              ELSE
177500                 CONTINUE
177600              END-IF
177700         WHEN OTHER
177800*           This is some kind of error. Change to END BR
177900*           And exit
178000            SET READ-LOOP-EXIT             TO TRUE
178100            SET WS-DB2-ERROR               TO TRUE
178200
178300            IF WS-RETURN-MSG-OFF
178400               MOVE 'Error on fetch Cursor C-TR-TYPE-BACKWARD'
178500                                        TO WS-DB2-CURRENT-ACTION
178600               PERFORM 9999-FORMAT-DB2-MESSAGE
178700                  THRU 9999-FORMAT-DB2-MESSAGE-EXIT
178800
178900             END-IF
179000     END-EVALUATE
179100     END-PERFORM
179200     .
179300
179400 8100-READ-BACKWARDS-EXIT.
179500     PERFORM 9550-CLOSE-BACK-CURSOR
179600        THRU 9550-CLOSE-BACK-CURSOR-EXIT
179700
179800     EXIT
179900     .
180000
180100 9100-CHECK-FILTERS.
180200
180300     EXEC SQL
180400          SELECT COUNT(1)
180500            INTO :WS-RECORDS-COUNT
180600            FROM CARDDEMO.TRANSACTION_TYPE
180700           WHERE ((:WS-EDIT-TYPE-FLAG = '1'
180900                 AND  TR_TYPE = :WS-TYPE-CD-FILTER)
181000                 OR  :WS-EDIT-TYPE-FLAG <> '1')
181200             AND
181300           	 ((:WS-EDIT-DESC-FLAG = '1'
181500                  AND TR_DESCRIPTION LIKE
181600                        TRIM(:WS-TYPE-DESC-FILTER))
181700                  OR :WS-EDIT-DESC-FLAG <> '1')
181900     END-EXEC
182000
182100     MOVE SQLCODE                             TO WS-DISP-SQLCODE
182200
182300     EVALUATE TRUE
182400         WHEN SQLCODE = ZERO
182500             CONTINUE
182600         WHEN OTHER
182700            SET INPUT-ERROR                   TO TRUE
182800
182900            IF WS-RETURN-MSG-OFF
183000                MOVE 'Error reading TRANSACTION_TYPE table '
183100                                         TO WS-DB2-CURRENT-ACTION
183200                PERFORM 9999-FORMAT-DB2-MESSAGE
183300                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
183400            END-IF
183500            GO TO 9100-CHECK-FILTERS-EXIT
183600     END-EVALUATE
183700     .
183800 9100-CHECK-FILTERS-EXIT.
183900     EXIT
184000     .
184100 9200-UPDATE-RECORD.
184200
184300     MOVE WS-ROW-TR-CODE-IN (I-SELECTED)
184400                             TO DCL-TR-TYPE
184500     MOVE FUNCTION TRIM(WS-ROW-TR-DESC-IN (I-SELECTED))
184600                             TO DCL-TR-DESCRIPTION-TEXT
184700     COMPUTE DCL-TR-DESCRIPTION-LEN
184800      = FUNCTION LENGTH(WS-ROW-TR-DESC-IN (I-SELECTED))
184900
185000     EXEC SQL
185100          UPDATE CARDDEMO.TRANSACTION_TYPE
185200             SET TR_DESCRIPTION = :DCL-TR-DESCRIPTION
185300           WHERE TR_TYPE = :DCL-TR-TYPE
185400     END-EXEC
185500
185600     MOVE SQLCODE                             TO WS-DISP-SQLCODE
185700
185800     EVALUATE TRUE
185900         WHEN SQLCODE = ZERO
186000            EXEC CICS SYNCPOINT END-EXEC
186100            SET CA-UPDATE-SUCCEEDED           TO TRUE
186200            IF WS-NO-INFO-MESSAGE
186300               SET WS-INFORM-UPDATE-SUCCESS   TO TRUE
186400            END-IF
186500         WHEN SQLCODE = +100
186600            SET CA-UPDATE-REQUESTED           TO TRUE
186700            IF WS-RETURN-MSG-OFF
186800                MOVE 'Record not found. Deleted by others ? '
186900                                         TO WS-DB2-CURRENT-ACTION
187000                PERFORM 9999-FORMAT-DB2-MESSAGE
187100                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
187200            END-IF
187300            GO TO 9200-UPDATE-RECORD-EXIT
187400         WHEN SQLCODE = -911
187500            SET CA-UPDATE-REQUESTED           TO TRUE
187600            SET INPUT-ERROR                   TO TRUE
187700            IF WS-RETURN-MSG-OFF
187800                MOVE 'Deadlock. Someone else updating ?'
187900                                         TO WS-DB2-CURRENT-ACTION
188000                PERFORM 9999-FORMAT-DB2-MESSAGE
188100                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
188200            END-IF
188300            GO TO 9200-UPDATE-RECORD-EXIT
188400         WHEN SQLCODE < 0
188500            SET CA-UPDATE-REQUESTED           TO TRUE
188600            IF WS-RETURN-MSG-OFF
188700                MOVE 'Update failed with'
188800                                         TO WS-DB2-CURRENT-ACTION
188900                PERFORM 9999-FORMAT-DB2-MESSAGE
189000                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
189100            END-IF
189200            GO TO 9200-UPDATE-RECORD-EXIT
189300     END-EVALUATE
189400     .
189500
189600 9200-UPDATE-RECORD-EXIT.
189700     EXIT
189800     .
189900
190000 9300-DELETE-RECORD.
190100
190200     MOVE WS-ROW-TR-CODE-IN (I-SELECTED)      TO  DCL-TR-TYPE
190300
190400     EXEC SQL
190500          DELETE FROM CARDDEMO.TRANSACTION_TYPE
190600           WHERE TR_TYPE = :DCL-TR-TYPE
190700     END-EXEC
190800
190900     MOVE SQLCODE                             TO WS-DISP-SQLCODE
191000
191100     EVALUATE TRUE
191200         WHEN SQLCODE = ZERO
191300            EXEC CICS SYNCPOINT END-EXEC
191400            SET CA-DELETE-SUCCEEDED           TO TRUE
191500            IF WS-NO-INFO-MESSAGE
191600               SET WS-INFORM-DELETE-SUCCESS   TO TRUE
191700            END-IF
191800         WHEN SQLCODE = -532
191900            SET CA-DELETE-REQUESTED           TO TRUE
192000
192100            IF WS-RETURN-MSG-OFF
192200                MOVE
192300                'Please delete associated child records first:'
192400                                         TO WS-DB2-CURRENT-ACTION
192500                PERFORM 9999-FORMAT-DB2-MESSAGE
192600                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
192700            END-IF
192800
192900            GO TO 9300-DELETE-RECORD-EXIT
193000         WHEN OTHER
193100            IF WS-RETURN-MSG-OFF
193200                MOVE
193300                'Delete failed with message:'
193400                                         TO WS-DB2-CURRENT-ACTION
193500                PERFORM 9999-FORMAT-DB2-MESSAGE
193600                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
193700            END-IF
193800            GO TO 9300-DELETE-RECORD-EXIT
193900     END-EVALUATE
194000     .
194100
194200 9300-DELETE-RECORD-EXIT.
194300     EXIT
194400     .
194500
194600 9400-OPEN-FORWARD-CURSOR.
194700     EXEC SQL
194800          OPEN C-TR-TYPE-FORWARD
194900     END-EXEC
195000
195100     MOVE SQLCODE        TO WS-DISP-SQLCODE
195200
195300     EVALUATE TRUE
195400        WHEN SQLCODE = ZERO
195500           CONTINUE
195600        WHEN OTHER
195700*          This is some kind of error. Close Cursor
195800*          And exit
195900           SET WS-DB2-ERROR        TO TRUE
196000           IF WS-RETURN-MSG-OFF
196100                MOVE
196200                'C-TR-TYPE-FORWARD Open'
196300                                         TO WS-DB2-CURRENT-ACTION
196400                PERFORM 9999-FORMAT-DB2-MESSAGE
196500                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
196600           END-IF
196700      END-EVALUATE
196800      .
196900 9400-OPEN-FORWARD-CURSOR-EXIT.
197000     EXIT
197100     .
197200
197300
197400 9450-CLOSE-FORWARD-CURSOR.
197500     EXEC SQL
197600          CLOSE C-TR-TYPE-FORWARD
197700     END-EXEC
197800
197900     MOVE SQLCODE        TO WS-DISP-SQLCODE
198000
198100     EVALUATE TRUE
198200        WHEN SQLCODE = ZERO
198300           CONTINUE
198400        WHEN OTHER
198500*          This is some kind of error. Close Cursor
198600*          And exit
198700           SET WS-DB2-ERROR        TO TRUE
198800           IF WS-RETURN-MSG-OFF
198900                MOVE
199000                'C-TR-TYPE-FORWARD close'
199100                                         TO WS-DB2-CURRENT-ACTION
199200                PERFORM 9999-FORMAT-DB2-MESSAGE
199300                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
199400           END-IF
199500      END-EVALUATE
199600      .
199700 9450-CLOSE-FORWARD-CURSOR-EXIT.
199800     EXIT
199900     .
200000
200100 9500-OPEN-BACKWARD-CURSOR.
200200     EXEC SQL
200300          OPEN C-TR-TYPE-BACKWARD
200400     END-EXEC
200500
200600     MOVE SQLCODE        TO WS-DISP-SQLCODE
200700
200800     EVALUATE TRUE
200900        WHEN SQLCODE = ZERO
201000           CONTINUE
201100        WHEN OTHER
201200*          This is some kind of error. Close Cursor
201300*          And exit
201400           SET WS-DB2-ERROR        TO TRUE
201500           IF WS-RETURN-MSG-OFF
201600                MOVE
201700                'C-TR-TYPE-BACKWARD Open'
201800                                         TO WS-DB2-CURRENT-ACTION
201900                PERFORM 9999-FORMAT-DB2-MESSAGE
202000                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
202100           END-IF
202200*
202300      END-EVALUATE
202400      .
202500 9500-OPEN-BACKWARD-CURSOR-EXIT.
202600     EXIT
202700     .
202800
202900
203000 9550-CLOSE-BACK-CURSOR.
203100     EXEC SQL
203200          CLOSE C-TR-TYPE-BACKWARD
203300     END-EXEC
203400
203500     MOVE SQLCODE        TO WS-DISP-SQLCODE
203600
203700     EVALUATE TRUE
203800        WHEN SQLCODE = ZERO
203900           CONTINUE
204000        WHEN OTHER
204100*          This is some kind of error. Close Cursor
204200*          And exit
204300           SET WS-DB2-ERROR        TO TRUE
204400           IF WS-RETURN-MSG-OFF
204500                MOVE
204600                'C-TR-TYPE-BACKWARD close'
204700                                         TO WS-DB2-CURRENT-ACTION
204800                PERFORM 9999-FORMAT-DB2-MESSAGE
204900                   THRU 9999-FORMAT-DB2-MESSAGE-EXIT
205000           END-IF
205100      END-EVALUATE
205200      .
205300 9550-CLOSE-BACK-CURSOR-EXIT.
205400     EXIT
205500     .
205600*****************************************************************
205700*Common Db2 routines
205800*****************************************************************
205900     EXEC SQL INCLUDE CSDB2RPY END-EXEC
206000
206100*****************************************************************
206200*Common code to store PFKey
206300*****************************************************************
206400 COPY 'CSSTRPFY'
206500     .
206600
206700*****************************************************************
206800* Plain text exit - Dont use in production                      *
206900*****************************************************************
207000 SEND-PLAIN-TEXT.
207100     EXEC CICS SEND TEXT
207200               FROM(WS-RETURN-MSG)
207300               LENGTH(LENGTH OF WS-RETURN-MSG)
207400               ERASE
207500               FREEKB
207600     END-EXEC
207700
207800     EXEC CICS RETURN
207900     END-EXEC
208000     .
208100 SEND-PLAIN-TEXT-EXIT.
208200     EXIT
208300     .
208400*****************************************************************
208500* Display Long text and exit                                    *
208600* This is primarily for debugging and should not be used in     *
208700* regular course                                                *
208800*****************************************************************
208900 SEND-LONG-TEXT.
209000     EXEC CICS SEND TEXT
209100               FROM(WS-LONG-MSG)
209200               LENGTH(LENGTH OF WS-LONG-MSG)
209300               ERASE
209400               FREEKB
209500     END-EXEC
209600
209700     EXEC CICS RETURN
209800     END-EXEC
209900     .
210000 SEND-LONG-TEXT-EXIT.
210100     EXIT
210200     .
