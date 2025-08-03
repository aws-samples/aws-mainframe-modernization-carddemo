000100**************************************** *************************00010000
000200* Program:     COTRTUPC.CBL                                      *00020000
000300* Layer:       Business logic                                    *00030000
000400* Function:    Accept and process TRANSACTION TYPE UPDATE        *00040000
000500******************************************************************00050000
000600* Copyright Amazon.com, Inc. or its affiliates.                   00060000
000700* All Rights Reserved.                                            00070000
000800*                                                                 00080000
000900* Licensed under the Apache License, Version 2.0 (the "License"). 00090000
001000* You may not use this file except in compliance with the License.00100000
001100* You may obtain a copy of the License at                         00110000
001200*                                                                 00120000
001300*    http://www.apache.org/licenses/LICENSE-2.0                   00130000
001400*                                                                 00140000
001500* Unless required by applicable law or agreed to in writing,      00150000
001600* software distributed under the License is distributed on an     00160000
001700* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    00170000
001800* either express or implied. See the License for the specific     00180000
001900* language governing permissions and limitations under the License00190000
002000******************************************************************00200000
002100 IDENTIFICATION DIVISION.                                         00210000
002200 PROGRAM-ID.                                                      00220000
002300     COTRTUPC.                                                    00230000
002400 DATE-WRITTEN.                                                    00240000
002500     Dec 2022.                                                    00250000
002600 DATE-COMPILED.                                                   00260000
002700     Today.                                                       00270000
002800                                                                  00280000
002900 ENVIRONMENT DIVISION.                                            00290000
003000 INPUT-OUTPUT SECTION.                                            00300000
003100                                                                  00310000
003200 DATA DIVISION.                                                   00320000
003300                                                                  00330000
003400 WORKING-STORAGE SECTION.                                         00340000
003500 01  WS-MISC-STORAGE.                                             00350000
003600******************************************************************00360000
003700* General CICS related                                            00370000
003800******************************************************************00380000
003900   05 WS-CICS-PROCESSNG-VARS.                                     00390000
004000      07 WS-RESP-CD                          PIC S9(09) COMP      00400000
004100                                             VALUE ZEROS.         00410000
004200      07 WS-REAS-CD                          PIC S9(09) COMP      00420000
004300                                             VALUE ZEROS.         00430000
004400      07 WS-TRANID                           PIC X(4)             00440000
004500                                             VALUE SPACES.        00450000
004600      07 WS-UCTRANS                          PIC X(4)             00460000
004700                                             VALUE SPACES.        00470000
004800******************************************************************00480000
004900*      Input edits                                                00490000
005000******************************************************************00500000
005100*  Generic Input Edits                                            00510000
005200   05  WS-GENERIC-EDITS.                                          00520000
005300     10 WS-EDIT-VARIABLE-NAME                PIC X(25).           00530000
005400                                                                  00540000
005500     10 WS-EDIT-ALPHANUM-ONLY                PIC X(256).          00550000
005600     10 WS-EDIT-ALPHANUM-LENGTH              PIC S9(4) COMP-3.    00560000
005700                                                                  00570000
005800     10 WS-EDIT-ALPHANUM-ONLY-FLAGS          PIC X(1).            00580000
005900        88  FLG-ALPHNANUM-ISVALID            VALUE LOW-VALUES.    00590000
006000        88  FLG-ALPHNANUM-NOT-OK             VALUE '0'.           00600000
006100        88  FLG-ALPHNANUM-BLANK              VALUE 'B'.           00610000
006200                                                                  00620000
006300                                                                  00630000
006400******************************************************************00640000
006500*    Work variables                                               00650000
006600******************************************************************00660000
006700    05 WS-MISC-VARS.                                              00670000
006800      10 WS-DISP-SQLCODE                    PIC ----9.            00680000
006900      10 WS-STRING-MID                      PIC 9(3) VALUE 0.     00690000
007000      10 WS-STRING-LEN                      PIC 9(3) VALUE 0.     00700000
007100      10 WS-STRING-OUT                      PIC X(40).            00710000
007200                                                                  00720000
007300******************************************************************00730000
007400*    Generic date edit variables CCYYMMDD                         00740000
007500******************************************************************00750000
007600     COPY 'CSUTLDWY'.                                             00760000
007700******************************************************************00770000
007800   05  WS-DATACHANGED-FLAG                   PIC X(1).            00780000
007900     88  NO-CHANGES-FOUND                    VALUE '0'.           00790000
008000     88  CHANGE-HAS-OCCURRED                 VALUE '1'.           00800000
008100   05  WS-INPUT-FLAG                         PIC X(1).            00810000
008200     88  INPUT-OK                            VALUE '0'.           00820000
008300     88  INPUT-ERROR                         VALUE '1'.           00830000
008400     88  INPUT-PENDING                       VALUE LOW-VALUES.    00840000
008500   05  WS-RETURN-FLAG                        PIC X(1).            00850000
008600     88  WS-RETURN-FLAG-OFF                  VALUE LOW-VALUES.    00860000
008700     88  WS-RETURN-FLAG-ON                   VALUE '1'.           00870000
008800   05  WS-PFK-FLAG                           PIC X(1).            00880000
008900     88  PFK-VALID                           VALUE '0'.           00890000
009000     88  PFK-INVALID                         VALUE '1'.           00900000
009100                                                                  00910000
009200*  Program specific edits                                         00920000
009300*                                                                 00930000
009400   05  WS-EDIT-TTYP-FLAG                     PIC X(1).            00940000
009500     88  FLG-TRANFILTER-ISVALID              VALUE LOW-VALUES.    00950000
009600     88  FLG-TRANFILTER-NOT-OK               VALUE '0'.           00960000
009700     88  FLG-TRANFILTER-BLANK                VALUE 'B'.           00970000
009800                                                                  00980000
009900   05 WS-NON-KEY-FLAGS.                                           00990000
010000     10  WS-EDIT-DESC-FLAGS                  PIC X(1).            01000000
010100         88  FLG-DESCRIPTION-ISVALID          VALUE LOW-VALUES.   01010000
010200         88  FLG-DESCRIPTION-NOT-OK           VALUE '0'.          01020000
010300         88  FLG-DESCRIPTION-BLANK            VALUE 'B'.          01030000
010400******************************************************************01040000
010500* Output edits                                                    01050000
010600******************************************************************01060000
010700   05 CICS-OUTPUT-EDIT-VARS.                                      01070000
010800     10  WS-EDIT-DATE-X                      PIC X(10).           01080000
010900     10  FILLER REDEFINES WS-EDIT-DATE-X.                         01090000
011000         20 WS-EDIT-DATE-X-YEAR              PIC X(4).            01100000
011100         20 FILLER                           PIC X(1).            01110000
011200         20 WS-EDIT-DATE-MONTH               PIC X(2).            01120000
011300         20 FILLER                           PIC X(1).            01130000
011400         20 WS-EDIT-DATE-DAY                 PIC X(2).            01140000
011500     10  WS-EDIT-DATE-X REDEFINES                                 01150000
011600         WS-EDIT-DATE-X                      PIC 9(10).           01160000
011700     10  WS-EDIT-CURRENCY-9-2                PIC X(15).           01170000
011800     10  WS-EDIT-CURRENCY-9-2-F              PIC +ZZZ,ZZZ,ZZZ.99. 01180000
011900     10  WS-EDIT-NUMERIC-2                   PIC 9(02).           01190000
012000     10  WS-EDIT-ALPHANUMERIC-2              PIC X(02).           01200000
012100                                                                  01210000
012200******************************************************************01220000
012300*      File and data Handling                                     01230000
012400******************************************************************01240000
012500   05  WS-TABLE-READ-FLAGS.                                       01250000
012600     10 WS-TRANTYPE-MASTER-READ-FLAG         PIC X(1).            01260000
012700        88 FOUND-TRANTYPE-IN-TABLE          VALUE '1'.            01270000
012800*  Alpha variables for editing numerics                           01280000
012900*                                                                 01290000
013000    05 TTYP-UPDATE-RECORD.                                        01300000
013100***************************************************************** 01310000
013200*    Data-structure for  TRANSACTION TYPE (RECLN 60)              01320000
013300***************************************************************** 01330000
013400         15  TTUP-UPDATE-TTYP-TYPE               PIC X(02).       01340000
013500         15  TTUP-UPDATE-TTYP-TYPE-DESC          PIC X(50).       01350000
013600         15  FILLER                              PIC X(08).       01360000
013700                                                                  01370000
013800                                                                  01380000
013900******************************************************************01390000
014000*      Output Message Construction                                01400000
014100******************************************************************01410000
014200   05  WS-INFO-MSG                           PIC X(40).           01420000
014300     88  WS-NO-INFO-MESSAGE                 VALUES                01430000
014400                                            SPACES LOW-VALUES.    01440000
014500     88  FOUND-TRANTYPE-DATA                 VALUE                01450000
014600         'Selected transaction type shown above'.                 01460000
014700     88  PROMPT-FOR-SEARCH-KEYS              VALUE                01470000
014800         'Enter transaction type to be maintained'.               01480000
014900     88  PROMPT-CREATE-NEW-RECORD            VALUE                01490000
015000         'Press F05 to add. F12 to cancel'.                       01500000
015100     88  PROMPT-DELETE-CONFIRM               VALUE                01510000
015200         'Delete this record ? Press F4 to confirm'.              01520000
015300     88  CONFIRM-DELETE-SUCCESS              VALUE                01530000
015400         'Delete successful.'.                                    01540000
015500     88  PROMPT-FOR-CHANGES                  VALUE                01550000
015600         'Update transaction type details shown.'.                01560000
015700     88  PROMPT-FOR-NEWDATA                  VALUE                01570000
015800         'Enter new transaction type details.'.                   01580000
015900                                                                  01590000
016000     88  PROMPT-FOR-CONFIRMATION             VALUE                01600000
016100         'Changes validated.Press F5 to save'.                    01610000
016200     88  CONFIRM-UPDATE-SUCCESS              VALUE                01620000
016300         'Changes committed to database'.                         01630000
016400     88  INFORM-FAILURE                      VALUE                01640000
016500         'Changes unsuccessful'.                                  01650000
016600                                                                  01660000
016700   05  WS-RETURN-MSG                         PIC X(75).           01670000
016800     88  WS-RETURN-MSG-OFF                   VALUE SPACES.        01680000
016900     88  WS-EXIT-MESSAGE                     VALUE                01690000
017000         'PF03 pressed.Exiting              '.                    01700000
017100     88  WS-INVALID-KEY                      VALUE                01710000
017200         'Invalid Key pressed. '.                                 01720000
017300     88  WS-NAME-MUST-BE-ALPHA               VALUE                01730000
017400         'Name can only contain alphabets and spaces'.            01740000
017500     88  WS-RECORD-NOT-FOUND                 VALUE                01750000
017600         'No record found for this key in database' .             01760000
017700     88  NO-SEARCH-CRITERIA-RECEIVED         VALUE                01770000
017800         'No input received'.                                     01780000
017900     88  NO-CHANGES-DETECTED                 VALUE                01790000
018000         'No change detected with respect to values fetched.'.    01800000
018100     88  COULD-NOT-LOCK-REC-FOR-UPDATE       VALUE                01810000
018200         'Could not lock record for update'.                      01820000
018300     88  DATA-WAS-CHANGED-BEFORE-UPDATE      VALUE                01830000
018400         'Record changed by some one else. Please review'.        01840000
018500     88  WS-UPDATE-WAS-CANCELLED             VALUE                01850000
018600         'Update was cancelled'.                                  01860000
018700     88  TABLE-UPDATE-FAILED                 VALUE                01870000
018800         'Update of record failed'.                               01880000
018900     88  RECORD-DELETE-FAILED                VALUE                01890000
019000         'Delete of record failed'.                               01900000
019100     88  WS-DELETE-WAS-CANCELLED             VALUE                01910000
019200         'Delete was cancelled'.                                  01920000
019300     88  WS-INVALID-KEY-PRESSED              VALUE                01930000
019400         'Invalid key pressed'.                                   01940000
019500     88  CODING-TO-BE-DONE                   VALUE                01950000
019600         'Looks Good.... so far'.                                 01960000
019700******************************************************************01970000
019800*      Literals and Constants                                     01980000
019900******************************************************************01990000
020000 01 WS-LITERALS.                                                  02000000
020100    05 LIT-THISPGM                           PIC X(8)             02010000
020200                                             VALUE 'COTRTUPC'.    02020000
020300    05 LIT-THISTRANID                        PIC X(4)             02030000
020400                                             VALUE 'CTTU'.        02040000
020500    05 LIT-THISMAPSET                        PIC X(8)             02050000
020600                                             VALUE 'COTRTUP '.    02060000
020700    05 LIT-THISMAP                           PIC X(7)             02070000
020800                                             VALUE 'CTRTUPA'.     02080000
020900    05 LIT-ADMINPGM                           PIC X(8)            02090000
021000                                             VALUE 'COADM01C'.    02100000
021100    05 LIT-ADMINTRANID                        PIC X(4)            02110000
021200                                             VALUE 'CA00'.        02120000
021300    05 LIT-ADMINMAPSET                        PIC X(7)            02130000
021400                                             VALUE 'COADM01'.     02140000
021500    05 LIT-ADMINMAP                           PIC X(7)            02150000
021600                                             VALUE 'COADM1A'.     02160000
021700    05 LIT-LISTTPGM                           PIC X(8)            02170000
021800                                             VALUE 'COTRTLIC'.    02180000
021900    05 LIT-LISTTTRANID                        PIC X(4)            02190000
022000                                             VALUE 'CTLI'.        02200000
022100    05 LIT-LISTTMAPSET                        PIC X(7)            02210000
022200                                             VALUE 'COTRTLI'.     02220000
022300    05 LIT-LISTTMAP                           PIC X(7)            02230000
022400                                             VALUE 'CTRTLIA'.     02240000
022500                                                                  02250000
022600                                                                  02260000
022700******************************************************************02270000
022800* Literals for use in INSPECT statements                          02280000
022900******************************************************************02290000
023000    05 LIT-ALL-ALPHANUM-FROM-X.                                   02300000
023100       10 LIT-ALL-ALPHA-FROM-X.                                   02310000
023200          15 LIT-UPPER                       PIC X(26)            02320000
023300                           VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.    02330000
023400          15 LIT-LOWER                       PIC X(26)            02340000
023500                           VALUE 'abcdefghijklmnopqrstuvwxyz'.    02350000
023600       10 LIT-NUMBERS                        PIC X(10)            02360000
023700                           VALUE '0123456789'.                    02370000
023800******************************************************************02380000
023900*Other common working storage Variables                           02390000
024000******************************************************************02400000
024100 COPY CVCRD01Y.                                                   02410000
024200******************************************************************02420000
024300*Lookups                                                          02430000
024400******************************************************************02440000
024500                                                                  02450000
024600******************************************************************02460000
024700* Variables for use in INSPECT statements                         02470000
024800******************************************************************02480000
024900 01  LIT-ALL-ALPHA-FROM     PIC X(52) VALUE SPACES.               02490000
025000 01  LIT-ALL-ALPHANUM-FROM  PIC X(62) VALUE SPACES.               02500000
025100 01  LIT-ALL-NUM-FROM       PIC X(10) VALUE SPACES.               02510000
025200 77  LIT-ALPHA-SPACES-TO    PIC X(52) VALUE SPACES.               02520000
025300 77  LIT-ALPHANUM-SPACES-TO PIC X(62) VALUE SPACES.               02530000
025400 77  LIT-NUM-SPACES-TO      PIC X(10) VALUE SPACES.               02540000
025500                                                                  02550000
025600*IBM SUPPLIED COPYBOOKS                                           02560000
025700 COPY DFHBMSCA.                                                   02570000
025800 COPY DFHAID.                                                     02580000
025900                                                                  02590000
026000*COMMON COPYBOOKS                                                 02600000
026100*Screen Titles                                                    02610000
026200 COPY COTTL01Y.                                                   02620000
026300                                                                  02630000
026400*Transaction Type Update Screen Layout                            02640000
026500 COPY COTRTUP.                                                    02650000
026600                                                                  02660000
026700*Current Date                                                     02670000
026800 COPY CSDAT01Y.                                                   02680000
026900                                                                  02690000
027000*Common Messages                                                  02700000
027100 COPY CSMSG01Y.                                                   02710000
027200                                                                  02720000
027300*Abend Variables                                                  02730000
027400 COPY CSMSG02Y.                                                   02740000
027500                                                                  02750000
027600*Signed on user data                                              02760000
027700 COPY CSUSR01Y.                                                   02770000
027800                                                                  02780000
027900******************************************************************02790000
028000* Relational Database stuff                                       02800000
028100******************************************************************02810000
028200      EXEC SQL                                                    02820000
028300          INCLUDE SQLCA                                           02830000
028400      END-EXEC                                                    02840000
028500                                                                  02850000
028600      EXEC SQL INCLUDE DCLTRTYP END-EXEC                          02860000
028700                                                                  02870000
028800      EXEC SQL INCLUDE DCLTRCAT END-EXEC                          02880000
028900                                                                  02890000
029000******************************************************************02900000
029100*Application Commmarea Copybook                                   02910000
029200 COPY COCOM01Y.                                                   02920000
029300                                                                  02930000
029400 01 WS-THIS-PROGCOMMAREA.                                         02940000
029500    05 TTUP-UPDATE-SCREEN-DATA.                                   02950000
029600       10 TTUP-CHANGE-ACTION                     PIC X(1)         02960000
029700                                                 VALUE LOW-VALUES.02970000
029800          88 TTUP-DETAILS-NOT-FETCHED            VALUES           02980000
029900                                                 LOW-VALUES,      02990000
030000                                                 SPACES.          03000000
030100          88 TTUP-INVALID-SEARCH-KEYS            VALUE 'K'.       03010000
030200          88 TTUP-DETAILS-NOT-FOUND              VALUE 'X'.       03020000
030300          88 TTUP-SHOW-DETAILS                   VALUE 'S'.       03030000
030400*                                                                 03040000
030500          88 TTUP-CREATE-NEW-RECORD              VALUE 'R'.       03050000
030600          88 TTUP-REVIEW-NEW-RECORD              VALUE 'V'.       03060000
030700          88 TTUP-DELETE-IN-PROGRESS             VALUES '9'       03070000
030800                                                      , '8', '7'  03080000
030900                                                      , '6'.      03090000
031000          88 TTUP-CONFIRM-DELETE                 VALUE '9'.       03100000
031100          88 TTUP-START-DELETE                   VALUE '8'.       03110000
031200          88 TTUP-DELETE-DONE                    VALUE '7'.       03120000
031300          88 TTUP-DELETE-FAILED                  VALUE '6'.       03130000
031400***                                                               03140000
031500          88 TTUP-CHANGES-MADE                   VALUES 'E', 'N'  03150000
031600                                                      , 'L'       03160000
031700                                                      , 'F'.      03170000
031800          88 TTUP-CHANGES-NOT-OK                 VALUE 'E'.       03180000
031900          88 TTUP-CHANGES-OK-NOT-CONFIRMED       VALUE 'N'.       03190000
032000                                                                  03200000
032100***                                                               03210000
032200          88 TTUP-CHANGES-FAILED                 VALUES 'L', 'F'. 03220000
032300          88 TTUP-CHANGES-OKAYED-LOCK-ERROR      VALUE 'L'.       03230000
032400          88 TTUP-CHANGES-OKAYED-BUT-FAILED      VALUE 'F'.       03240000
032500                                                                  03250000
032600          88 TTUP-CHANGES-OKAYED-AND-DONE        VALUE 'C'.       03260000
032700          88 TTUP-CHANGES-BACKED-OUT             VALUE 'B'.       03270000
032800    05 TTUP-OLD-DETAILS.                                          03280000
032900       10 TTUP-OLD-TTYP-DATA.                                     03290000
033000          15  TTUP-OLD-TTYP-TYPE                 PIC X(02).       03300000
033100          15  TTUP-OLD-TTYP-TYPE-DESC            PIC X(50).       03310000
033200    05 TTUP-NEW-DETAILS.                                          03320000
033300       10 TTUP-NEW-TTYP-DATA.                                     03330000
033400          15  TTUP-NEW-TTYP-TYPE                 PIC X(02).       03340000
033500          15  TTUP-NEW-TTYP-TYPE-DESC            PIC X(50).       03350000
033600 01  WS-COMMAREA                                 PIC X(2000).     03360000
033700                                                                  03370000
033800                                                                  03380000
033900 LINKAGE SECTION.                                                 03390000
034000 01  DFHCOMMAREA.                                                 03400000
034100   05  FILLER                                PIC X(1)             03410000
034200       OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.             03420000
034300                                                                  03430000
034400 PROCEDURE DIVISION.                                              03440000
034500 0000-MAIN.                                                       03450000
034600                                                                  03460000
034700                                                                  03470000
034800     EXEC CICS HANDLE ABEND                                       03480000
034900               LABEL(ABEND-ROUTINE)                               03490000
035000     END-EXEC                                                     03500000
035100                                                                  03510000
035200     INITIALIZE CC-WORK-AREA                                      03520000
035300                WS-MISC-STORAGE                                   03530000
035400                WS-COMMAREA                                       03540000
035500***************************************************************** 03550000
035600* Store our context                                               03560000
035700***************************************************************** 03570000
035800     MOVE LIT-THISTRANID       TO WS-TRANID                       03580000
035900***************************************************************** 03590000
036000* Ensure error message is cleared                               * 03600000
036100***************************************************************** 03610000
036200     SET WS-RETURN-MSG-OFF  TO TRUE                               03620000
036300***************************************************************** 03630000
036400* Store passed data if  any                *                      03640000
036500***************************************************************** 03650000
036600     IF EIBCALEN IS EQUAL TO 0                                    03660000
036700         OR (CDEMO-FROM-PROGRAM = LIT-ADMINPGM                    03670000
036800         AND NOT CDEMO-PGM-REENTER)                               03680000
036900         OR (CDEMO-FROM-PROGRAM = LIT-LISTTPGM                    03690000
037000         AND NOT CDEMO-PGM-REENTER)                               03700000
037100        INITIALIZE CARDDEMO-COMMAREA                              03710000
037200                   WS-THIS-PROGCOMMAREA                           03720000
037300        SET CDEMO-PGM-ENTER TO TRUE                               03730000
037400        SET TTUP-DETAILS-NOT-FETCHED TO TRUE                      03740000
037500     ELSE                                                         03750000
037600        MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA)  TO      03760000
037700                          CARDDEMO-COMMAREA                       03770000
037800        MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:         03780000
037900                         LENGTH OF WS-THIS-PROGCOMMAREA ) TO      03790000
038000                          WS-THIS-PROGCOMMAREA                    03800000
038100     END-IF                                                       03810000
038200***************************************************************** 03820000
038300* Store the Mapped PF Key                                         03830000
038400* Remap PFkeys as needed.                                         03840000
038500***************************************************************** 03850000
038600     PERFORM YYYY-STORE-PFKEY                                     03860000
038700        THRU YYYY-STORE-PFKEY-EXIT                                03870000
038800                                                                  03880000
038900***************************************************************** 03890000
039000* Check the AID to see if its valid at this point               * 03900000
039100* Change the key to some valid value if possible                  03910000
039200* F3 - Exit                                                       03920000
039300* Enter show screen again                                         03930000
039400* F4 - Delete                                                     03940000
039500* F5 - Save                                                       03950000
039600* F12 - Cancel                                                    03960000
039700***************************************************************** 03970000
039800     SET PFK-INVALID TO TRUE                                      03980000
039900                                                                  03990000
040000     PERFORM 0001-CHECK-PFKEYS                                    04000000
040100        THRU 0001-CHECK-PFKEYS-EXIT                               04010000
040200***************************************************************** 04020000
040300*       Simulate initial entry if the following flags are set     04030000
040400***************************************************************** 04040000
040500     EVALUATE TRUE                                                04050000
040600        WHEN CCARD-AID-PFK12                                      04060000
040700         AND (TTUP-SHOW-DETAILS                                   04070000
040800          OR  TTUP-CREATE-NEW-RECORD                              04080000
040900          OR  TTUP-DETAILS-NOT-FOUND)                             04090000
041000        WHEN TTUP-CHANGES-OKAYED-AND-DONE                         04100000
041100        WHEN TTUP-CHANGES-FAILED                                  04110000
041200        WHEN TTUP-CHANGES-BACKED-OUT                              04120000
041300         AND  (TTUP-OLD-DETAILS EQUAL LOW-VALUES                  04130000
041400          OR   TTUP-OLD-DETAILS EQUAL SPACES)                     04140000
041500        WHEN TTUP-DELETE-DONE                                     04150000
041600        WHEN TTUP-DELETE-FAILED                                   04160000
041700             SET CDEMO-PGM-ENTER          TO TRUE                 04170000
041800             SET TTUP-DETAILS-NOT-FETCHED TO TRUE                 04180000
041900     END-EVALUATE                                                 04190000
042000***************************************************************** 04200000
042100* Decide what to do based on PF KEY PRESSED AND CONTEXT           04210000
042200***************************************************************** 04220000
042300     EVALUATE TRUE                                                04230000
042400******************************************************************04240000
042500*       USER PRESSES PF03 TO EXIT                                 04250000
042600*  OR   USER IS DONE WITH UPDATE                                  04260000
042700*            XCTL TO CALLING PROGRAM OR MAIN MENU                 04270000
042800******************************************************************04280000
042900        WHEN CCARD-AID-PFK03                                      04290000
043000                                                                  04300000
043100             IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES             04310000
043200             OR CDEMO-FROM-TRANID    EQUAL SPACES                 04320000
043300                MOVE LIT-ADMINTRANID   TO CDEMO-TO-TRANID         04330000
043400             ELSE                                                 04340000
043500                MOVE CDEMO-FROM-TRANID TO CDEMO-TO-TRANID         04350000
043600             END-IF                                               04360000
043700                                                                  04370000
043800             IF CDEMO-FROM-PROGRAM   EQUAL LOW-VALUES             04380000
043900             OR CDEMO-FROM-PROGRAM   EQUAL SPACES                 04390000
044000                MOVE LIT-ADMINPGM     TO CDEMO-TO-PROGRAM         04400000
044100             ELSE                                                 04410000
044200                MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM       04420000
044300             END-IF                                               04430000
044400                                                                  04440000
044500             MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID         04450000
044600             MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM        04460000
044700                                                                  04470000
044800             SET  CDEMO-USRTYP-ADMIN TO TRUE                      04480000
044900             SET  CDEMO-PGM-ENTER    TO TRUE                      04490000
045000             MOVE LIT-THISMAPSET     TO CDEMO-LAST-MAPSET         04500000
045100             MOVE LIT-THISMAP        TO CDEMO-LAST-MAP            04510000
045200                                                                  04520000
045300             EXEC CICS                                            04530000
045400                  SYNCPOINT                                       04540000
045500             END-EXEC                                             04550000
045600                                                                  04560000
045700             EXEC CICS XCTL                                       04570000
045800                  PROGRAM (CDEMO-TO-PROGRAM)                      04580000
045900                  COMMAREA(CARDDEMO-COMMAREA)                     04590000
046000             END-EXEC                                             04600000
046100******************************************************************04610000
046200*       CLEAR SCREEN, CLEAR SAVED CONTEXT                         04620000
046300*       ASK USER FOR SEARCH KEYS                                  04630000
046400******************************************************************04640000
046500        WHEN NOT CDEMO-PGM-REENTER                                04650000
046600         AND CDEMO-FROM-PROGRAM   EQUAL LIT-ADMINPGM              04660000
046700        WHEN NOT CDEMO-PGM-REENTER                                04670000
046800         AND CDEMO-FROM-PROGRAM   EQUAL LIT-LISTTPGM              04680000
046900        WHEN CDEMO-PGM-ENTER                                      04690000
047000         AND TTUP-DETAILS-NOT-FETCHED                             04700000
047100             INITIALIZE WS-THIS-PROGCOMMAREA                      04710000
047200                        WS-MISC-STORAGE                           04720000
047300                        CDEMO-ACCT-ID                             04730000
047400             PERFORM 3000-SEND-MAP THRU                           04740000
047500                     3000-SEND-MAP-EXIT                           04750000
047600             SET CDEMO-PGM-REENTER        TO TRUE                 04760000
047700             SET TTUP-DETAILS-NOT-FETCHED TO TRUE                 04770000
047800             GO TO COMMON-RETURN                                  04780000
047900******************************************************************04790000
048000*       USER PRESSED F04 AFTER BEING ASKED TO VERIFY DELETE       04800000
048100******************************************************************04810000
048200        WHEN CCARD-AID-PFK04                                      04820000
048300         AND TTUP-CONFIRM-DELETE                                  04830000
048400             SET TTUP-START-DELETE                TO TRUE         04840000
048500             PERFORM 9800-DELETE-PROCESSING                       04850000
048600                THRU 9800-DELETE-PROCESSING-EXIT                  04860000
048700             PERFORM 3000-SEND-MAP THRU                           04870000
048800                     3000-SEND-MAP-EXIT                           04880000
048900             GO TO COMMON-RETURN                                  04890000
049000******************************************************************04900000
049100*       USER PRESSED F04.ASK FOR DELETE CONFIRMATION              04910000
049200******************************************************************04920000
049300        WHEN CCARD-AID-PFK04                                      04930000
049400         AND TTUP-SHOW-DETAILS                                    04940000
049500             SET TTUP-CONFIRM-DELETE              TO TRUE         04950000
049600             PERFORM 3000-SEND-MAP THRU                           04960000
049700                     3000-SEND-MAP-EXIT                           04970000
049800             GO TO COMMON-RETURN                                  04980000
049900******************************************************************04990000
050000*       USER PRESSED F05. WHEN NO RECORD WAS FOUND.               05000000
050100*       ASK TO CONFIRM NEW RECORD CREATION                        05010000
050200******************************************************************05020000
050300        WHEN CCARD-AID-PFK05                                      05030000
050400         AND TTUP-DETAILS-NOT-FOUND                               05040000
050500            SET TTUP-CREATE-NEW-RECORD TO TRUE                    05050000
050600             PERFORM 3000-SEND-MAP THRU                           05060000
050700                     3000-SEND-MAP-EXIT                           05070000
050800             GO TO COMMON-RETURN                                  05080000
050900******************************************************************05090000
051000*       USER PRESSED F05 AND CONFIRMED THAT CHANGES CAN BE SAVED  05100000
051100*       EDITS HAVE PASSED                                         05110000
051200*       SO SAVE THE CHANGES                                       05120000
051300******************************************************************05130000
051400        WHEN CCARD-AID-PFK05                                      05140000
051500         AND TTUP-CHANGES-OK-NOT-CONFIRMED                        05150000
051600           PERFORM 9600-WRITE-PROCESSING                          05160000
051700              THRU 9600-WRITE-PROCESSING-EXIT                     05170000
051800             PERFORM 3000-SEND-MAP                                05180000
051900                THRU 3000-SEND-MAP-EXIT                           05190000
052000             GO TO COMMON-RETURN                                  05200000
052100******************************************************************05210000
052200*       USER PRESSED F12. CANCEL THE ACTION                       05220000
052300******************************************************************05230000
052400         WHEN CCARD-AID-PFK12                                     05240000
052500         AND (TTUP-CHANGES-OK-NOT-CONFIRMED                       05250000
052600          OR  TTUP-CONFIRM-DELETE                                 05260000
052700          OR  TTUP-SHOW-DETAILS)                                  05270000
052800             SET FOUND-TRANTYPE-IN-TABLE  TO TRUE                 05280000
052900             PERFORM 2000-DECIDE-ACTION                           05290000
053000                THRU 2000-DECIDE-ACTION-EXIT                      05300000
053100             PERFORM 3000-SEND-MAP                                05310000
053200                THRU 3000-SEND-MAP-EXIT                           05320000
053300             GO TO COMMON-RETURN                                  05330000
053400******************************************************************05340000
053500*       CHECK THE USER INPUTS                                     05350000
053600*       DECIDE WHAT TO DO                                         05360000
053700*       PRESENT NEXT STEPS TO USER                                05370000
053800******************************************************************05380000
053900        WHEN WS-INVALID-KEY-PRESSED                               05390000
054000             PERFORM 3000-SEND-MAP                                05400000
054100                THRU 3000-SEND-MAP-EXIT                           05410000
054200             GO TO COMMON-RETURN                                  05420000
054300******************************************************************05430000
054400*       CHECK THE USER INPUTS                                     05440000
054500*       DECIDE WHAT TO DO                                         05450000
054600*       PRESENT NEXT STEPS TO USER                                05460000
054700******************************************************************05470000
054800        WHEN OTHER                                                05480000
054900             PERFORM 1000-PROCESS-INPUTS                          05490000
055000                THRU 1000-PROCESS-INPUTS-EXIT                     05500000
055100             PERFORM 2000-DECIDE-ACTION                           05510000
055200                THRU 2000-DECIDE-ACTION-EXIT                      05520000
055300             PERFORM 3000-SEND-MAP                                05530000
055400                THRU 3000-SEND-MAP-EXIT                           05540000
055500             GO TO COMMON-RETURN                                  05550000
055600     END-EVALUATE                                                 05560000
055700     .                                                            05570000
055800                                                                  05580000
055900 COMMON-RETURN.                                                   05590000
056000     MOVE WS-RETURN-MSG     TO CCARD-ERROR-MSG                    05600000
056100                                                                  05610000
056200     MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA                    05620000
056300     MOVE  WS-THIS-PROGCOMMAREA TO                                05630000
056400            WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:          05640000
056500                         LENGTH OF WS-THIS-PROGCOMMAREA )         05650000
056600                                                                  05660000
056700     EXEC CICS RETURN                                             05670000
056800          TRANSID (LIT-THISTRANID)                                05680000
056900          COMMAREA (WS-COMMAREA)                                  05690000
057000          LENGTH(LENGTH OF WS-COMMAREA)                           05700000
057100     END-EXEC                                                     05710000
057200     .                                                            05720000
057300 0000-MAIN-EXIT.                                                  05730000
057400     EXIT                                                         05740000
057500     .                                                            05750000
057600                                                                  05760000
057700 0001-CHECK-PFKEYS.                                               05770000
057800                                                                  05780000
057900*    Should mirror logic in PFKey attribut para                   05790000
058000*    3391-PFKEY-ATTRS                                             05800000
058100                                                                  05810000
058200     IF (CCARD-AID-PFK03)                                         05820000
058300     OR (CCARD-AID-ENTER AND NOT TTUP-CONFIRM-DELETE)             05830000
058400     OR (CCARD-AID-PFK04 AND (TTUP-SHOW-DETAILS                   05840000
058500                        OR   TTUP-CONFIRM-DELETE )                05850000
058600        )                                                         05860000
058700                                                                  05870000
058800     OR (CCARD-AID-PFK05 AND (                                    05880000
058900                             TTUP-CHANGES-OK-NOT-CONFIRMED        05890000
059000                        OR   TTUP-DETAILS-NOT-FOUND               05900000
059100                        OR   TTUP-DELETE-IN-PROGRESS              05910000
059200                             )                                    05920000
059300        )                                                         05930000
059400     OR (CCARD-AID-PFK12 AND (                                    05940000
059500                             TTUP-CHANGES-OK-NOT-CONFIRMED        05950000
059600                        OR   TTUP-SHOW-DETAILS                    05960000
059700                        OR   TTUP-DETAILS-NOT-FOUND               05970000
059800                        OR   TTUP-CONFIRM-DELETE                  05980000
059900                        OR   TTUP-CREATE-NEW-RECORD               05990000
060000                             )                                    06000000
060100       )                                                          06010000
060200        SET PFK-VALID                  TO TRUE                    06020000
060300     ELSE                                                         06030000
060400        SET PFK-INVALID                TO TRUE                    06040000
060500        IF WS-RETURN-MSG-OFF                                      06050000
060600           SET WS-INVALID-KEY-PRESSED  TO TRUE                    06060000
060700        END-IF                                                    06070000
060800     END-IF                                                       06080000
060900                                                                  06090000
061000                                                                  06100000
061100*    IF PFK-INVALID                                               06110000
061200*      SET WS-INVALID-KEY  TO TRUE                                06120000
061300*      SET CCARD-AID-ENTER TO TRUE                                06130000
061400*    ELSE                                                         06140000
061500*      CONTINUE                                                   06150000
061600*    END-IF                                                       06160000
061700                                                                  06170000
061800     .                                                            06180000
061900                                                                  06190000
062000                                                                  06200000
062100 0001-CHECK-PFKEYS-EXIT.                                          06210000
062200     EXIT                                                         06220000
062300     .                                                            06230000
062400                                                                  06240000
062500 1000-PROCESS-INPUTS.                                             06250000
062600     PERFORM 1100-RECEIVE-MAP                                     06260000
062700        THRU 1100-RECEIVE-MAP-EXIT                                06270000
062800     PERFORM 1150-STORE-MAP-IN-NEW                                06280000
062900        THRU 1150-STORE-MAP-IN-NEW-EXIT                           06290000
063000     PERFORM 1200-EDIT-MAP-INPUTS                                 06300000
063100        THRU 1200-EDIT-MAP-INPUTS-EXIT                            06310000
063200     MOVE WS-RETURN-MSG  TO CCARD-ERROR-MSG                       06320000
063300     MOVE LIT-THISPGM    TO CCARD-NEXT-PROG                       06330000
063400     MOVE LIT-THISMAPSET TO CCARD-NEXT-MAPSET                     06340000
063500     MOVE LIT-THISMAP    TO CCARD-NEXT-MAP                        06350000
063600     .                                                            06360000
063700*                                                                 06370000
063800 1000-PROCESS-INPUTS-EXIT.                                        06380000
063900     EXIT                                                         06390000
064000     .                                                            06400000
064100 1100-RECEIVE-MAP.                                                06410000
064200     EXEC CICS RECEIVE MAP(LIT-THISMAP)                           06420000
064300               MAPSET(LIT-THISMAPSET)                             06430000
064400               INTO(CTRTUPAI)                                     06440000
064500               RESP(WS-RESP-CD)                                   06450000
064600               RESP2(WS-REAS-CD)                                  06460000
064700     END-EXEC                                                     06470000
064800     .                                                            06480000
064900 1100-RECEIVE-MAP-EXIT.                                           06490000
065000     EXIT.                                                        06500000
065100                                                                  06510000
065200 1150-STORE-MAP-IN-NEW.                                           06520000
065300                                                                  06530000
065400     IF  TTUP-DETAILS-NOT-FOUND                                   06540000
065500     AND NOT CCARD-AID-PFK05                                      06550000
065600     AND FUNCTION TRIM(TRTYPCDI OF CTRTUPAI)                      06560000
065700           = TTUP-NEW-TTYP-TYPE                                   06570000
065800         GO TO 1150-STORE-MAP-IN-NEW-EXIT                         06580000
065900     ELSE                                                         06590000
066000         CONTINUE                                                 06600000
066100     END-IF                                                       06610000
066200                                                                  06620000
066300     INITIALIZE TTUP-NEW-DETAILS                                  06630000
066400******************************************************************06640000
066500*    Transaction Type                                             06650000
066600******************************************************************06660000
066700     IF  TRTYPCDI OF CTRTUPAI = '*'                               06670000
066800     OR  TRTYPCDI OF CTRTUPAI = SPACES                            06680000
066900         MOVE LOW-VALUES           TO TTUP-NEW-TTYP-TYPE          06690000
067000     ELSE                                                         06700000
067100         MOVE FUNCTION TRIM(TRTYPCDI OF CTRTUPAI)                 06710000
067200                                   TO TTUP-NEW-TTYP-TYPE          06720000
067300     END-IF                                                       06730000
067400                                                                  06740000
067500******************************************************************06750000
067600*    Transaction Desc                                             06760000
067700******************************************************************06770000
067800     IF  TRTYDSCI OF CTRTUPAI = '*'                               06780000
067900     OR  TRTYDSCI OF CTRTUPAI = SPACES                            06790000
068000         MOVE LOW-VALUES           TO TTUP-NEW-TTYP-TYPE-DESC     06800000
068100     ELSE                                                         06810000
068200         MOVE FUNCTION TRIM(TRTYDSCI OF CTRTUPAI)                 06820000
068300                                   TO TTUP-NEW-TTYP-TYPE-DESC     06830000
068400     END-IF                                                       06840000
068500     .                                                            06850000
068600 1150-STORE-MAP-IN-NEW-EXIT.                                      06860000
068700     EXIT                                                         06870000
068800     .                                                            06880000
068900 1200-EDIT-MAP-INPUTS.                                            06890000
069000     SET INPUT-OK                  TO TRUE                        06900000
069100******************************************************************06910000
069200*    VALIDATE THE SEARCH KEYS                                     06920000
069300******************************************************************06930000
069400*    The key  was not in database. User sent the same key. So     06940000
069500*    dont edit again. Set tran filter to valid and skip           06950000
069600*    rest of edits                                                06960000
069700*                                                                 06970000
069800     IF  TTUP-DETAILS-NOT-FOUND                                   06980000
069900     AND FUNCTION TRIM(TRTYPCDI OF CTRTUPAI)                      06990000
070000           = TTUP-NEW-TTYP-TYPE                                   07000000
070100         IF CCARD-AID-PFK05                                       07010000
070200            CONTINUE                                              07020000
070300         ELSE                                                     07030000
070400            SET TTUP-DETAILS-NOT-FETCHED    TO TRUE               07040000
070500         END-IF                                                   07050000
070600         SET FLG-TRANFILTER-ISVALID         TO TRUE               07060000
070700         GO TO 1200-EDIT-MAP-INPUTS-EXIT                          07070000
070800     ELSE                                                         07080000
070900         CONTINUE                                                 07090000
071000     END-IF                                                       07100000
071100                                                                  07110000
071200     IF  TTUP-CREATE-NEW-RECORD                                   07120000
071300     OR  TTUP-CHANGES-OK-NOT-CONFIRMED                            07130000
071400         CONTINUE                                                 07140000
071500     ELSE                                                         07150000
071600         PERFORM 1210-EDIT-TRANTYPE                               07160000
071700            THRU 1210-EDIT-TRANTYPE-EXIT                          07170000
071800                                                                  07180000
071900*        IF THE SEARCH CONDITIONS HAVE PROBLEMS FLAG THEM         07190000
072000         IF  FLG-TRANFILTER-BLANK                                 07200000
072100             IF WS-RETURN-MSG-OFF                                 07210000
072200                SET NO-SEARCH-CRITERIA-RECEIVED TO TRUE           07220000
072300             END-IF                                               07230000
072400             SET TTUP-DETAILS-NOT-FETCHED       TO TRUE           07240000
072500             GO TO 1200-EDIT-MAP-INPUTS-EXIT                      07250000
072600         END-IF                                                   07260000
072700                                                                  07270000
072800         IF  FLG-TRANFILTER-NOT-OK                                07280000
072900             SET TTUP-INVALID-SEARCH-KEYS       TO TRUE           07290000
073000             SET TTUP-DETAILS-NOT-FETCHED       TO TRUE           07300000
073100             GO TO 1200-EDIT-MAP-INPUTS-EXIT                      07310000
073200         END-IF                                                   07320000
073300                                                                  07330000
073400         IF TTUP-DETAILS-NOT-FETCHED                              07340000
073500            GO TO 1200-EDIT-MAP-INPUTS-EXIT                       07350000
073600         END-IF                                                   07360000
073700     END-IF                                                       07370000
073800******************************************************************07380000
073900*    SEARCH KEYS ALREADY VALIDATED. CHECK OTHER INPUTS            07390000
074000******************************************************************07400000
074100     SET FLG-TRANFILTER-ISVALID    TO TRUE                        07410000
074200*                                                                 07420000
074300     PERFORM 1205-COMPARE-OLD-NEW                                 07430000
074400        THRU 1205-COMPARE-OLD-NEW-EXIT                            07440000
074500                                                                  07450000
074600     IF  NO-CHANGES-FOUND                                         07460000
074700     OR  TTUP-CHANGES-OK-NOT-CONFIRMED                            07470000
074800     OR  TTUP-CHANGES-OKAYED-AND-DONE                             07480000
074900         MOVE LOW-VALUES           TO WS-NON-KEY-FLAGS            07490000
075000         GO TO 1200-EDIT-MAP-INPUTS-EXIT                          07500000
075100     END-IF                                                       07510000
075200                                                                  07520000
075300     SET TTUP-CHANGES-NOT-OK       TO TRUE                        07530000
075400                                                                  07540000
075500******************************************************************07550000
075600*    Edit Description                                             07560000
075700******************************************************************07570000
075800     MOVE 'Transaction Desc'       TO WS-EDIT-VARIABLE-NAME       07580000
075900     MOVE TTUP-NEW-TTYP-TYPE-DESC  TO WS-EDIT-ALPHANUM-ONLY       07590000
076000     MOVE 50                       TO WS-EDIT-ALPHANUM-LENGTH     07600000
076100     PERFORM 1230-EDIT-ALPHANUM-REQD                              07610000
076200        THRU 1230-EDIT-ALPHANUM-REQD-EXIT                         07620000
076300     MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS                             07630000
076400                                   TO WS-EDIT-DESC-FLAGS          07640000
076500                                                                  07650000
076600*    Cross field edits begin here                                 07660000
076700*                                                                 07670000
076800*       No cross edits in this program so far                     07680000
076900                                                                  07690000
077000*    Set green light for confirmation if no errors found          07700000
077100                                                                  07710000
077200     IF INPUT-ERROR                                               07720000
077300        CONTINUE                                                  07730000
077400     ELSE                                                         07740000
077500        SET TTUP-CHANGES-OK-NOT-CONFIRMED TO TRUE                 07750000
077600     END-IF                                                       07760000
077700     .                                                            07770000
077800                                                                  07780000
077900 1200-EDIT-MAP-INPUTS-EXIT.                                       07790000
078000     EXIT                                                         07800000
078100     .                                                            07810000
078200                                                                  07820000
078300 1205-COMPARE-OLD-NEW.                                            07830000
078400     SET NO-CHANGES-FOUND           TO TRUE                       07840000
078500                                                                  07850000
078600     IF  FUNCTION UPPER-CASE (                                    07860000
078700         TTUP-NEW-TTYP-TYPE)    =                                 07870000
078800         FUNCTION UPPER-CASE (                                    07880000
078900         TTUP-OLD-TTYP-TYPE)                                      07890000
079000     AND FUNCTION UPPER-CASE (                                    07900000
079100         FUNCTION TRIM (TTUP-NEW-TTYP-TYPE-DESC))=                07910000
079200         FUNCTION UPPER-CASE (                                    07920000
079300         FUNCTION TRIM (TTUP-OLD-TTYP-TYPE-DESC))                 07930000
079400     AND FUNCTION LENGTH (                                        07940000
079500         FUNCTION TRIM (TTUP-NEW-TTYP-TYPE-DESC))=                07950000
079600         FUNCTION LENGTH (                                        07960000
079700         FUNCTION TRIM (TTUP-OLD-TTYP-TYPE-DESC))                 07970000
079800                                                                  07980000
079900         IF WS-RETURN-MSG-OFF                                     07990000
080000            SET NO-CHANGES-DETECTED   TO TRUE                     08000000
080100         ELSE                                                     08010000
080200            CONTINUE                                              08020000
080300         END-IF                                                   08030000
080400     ELSE                                                         08040000
080500         IF WS-RETURN-MSG-OFF                                     08050000
080600            SET CHANGE-HAS-OCCURRED   TO TRUE                     08060000
080700         ELSE                                                     08070000
080800             CONTINUE                                             08080000
080900         END-IF                                                   08090000
081000         GO TO 1205-COMPARE-OLD-NEW-EXIT                          08100000
081100     END-IF                                                       08110000
081200     .                                                            08120000
081300                                                                  08130000
081400 1205-COMPARE-OLD-NEW-EXIT.                                       08140000
081500     EXIT                                                         08150000
081600     .                                                            08160000
081700                                                                  08170000
081800                                                                  08180000
081900*                                                                 08190000
082000 1210-EDIT-TRANTYPE.                                              08200000
082100     SET FLG-TRANFILTER-NOT-OK    TO TRUE                         08210000
082200                                                                  08220000
082300******************************************************************08230000
082400*    Edit Tran Type code                                          08240000
082500******************************************************************08250000
082600     MOVE 'Tran Type code'         TO WS-EDIT-VARIABLE-NAME       08260000
082700     MOVE TTUP-NEW-TTYP-TYPE       TO WS-EDIT-ALPHANUM-ONLY       08270000
082800     MOVE 2                        TO WS-EDIT-ALPHANUM-LENGTH     08280000
082900     PERFORM 1245-EDIT-NUM-REQD                                   08290000
083000        THRU 1245-EDIT-NUM-REQD-EXIT                              08300000
083100     MOVE WS-EDIT-ALPHANUM-ONLY-FLAGS                             08310000
083200                                   TO WS-EDIT-TTYP-FLAG           08320000
083300                                                                  08330000
083400     IF FLG-TRANFILTER-ISVALID                                    08340000
083500        COMPUTE WS-EDIT-NUMERIC-2                                 08350000
083600             = FUNCTION NUMVAL(TTUP-NEW-TTYP-TYPE)                08360000
083700        END-COMPUTE                                               08370000
083800        MOVE WS-EDIT-NUMERIC-2      TO WS-EDIT-ALPHANUMERIC-2     08380000
083900        INSPECT WS-EDIT-ALPHANUMERIC-2                            08390000
084000                REPLACING ALL SPACES BY ZEROS                     08400000
084100        MOVE WS-EDIT-ALPHANUMERIC-2 TO TTUP-NEW-TTYP-TYPE         08410000
084200     END-IF                                                       08420000
084300     .                                                            08430000
084400                                                                  08440000
084500 1210-EDIT-TRANTYPE-EXIT.                                         08450000
084600     EXIT                                                         08460000
084700     .                                                            08470000
084800                                                                  08480000
084900 1230-EDIT-ALPHANUM-REQD.                                         08490000
085000*    Initialize                                                   08500000
085100     SET FLG-ALPHNANUM-NOT-OK          TO TRUE                    08510000
085200                                                                  08520000
085300*    Not supplied                                                 08530000
085400     IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)          08540000
085500                                       EQUAL LOW-VALUES           08550000
085600     OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)          08560000
085700         EQUAL SPACES                                             08570000
085800     OR FUNCTION LENGTH(FUNCTION TRIM(                            08580000
085900        WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0    08590000
086000                                                                  08600000
086100        SET INPUT-ERROR                TO TRUE                    08610000
086200        SET FLG-ALPHNANUM-BLANK        TO TRUE                    08620000
086300        IF WS-RETURN-MSG-OFF                                      08630000
086400           STRING                                                 08640000
086500             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)                 08650000
086600             ' must be supplied.'                                 08660000
086700             DELIMITED BY SIZE                                    08670000
086800             INTO WS-RETURN-MSG                                   08680000
086900           END-STRING                                             08690000
087000        END-IF                                                    08700000
087100                                                                  08710000
087200        GO TO  1230-EDIT-ALPHANUM-REQD-EXIT                       08720000
087300     END-IF                                                       08730000
087400                                                                  08740000
087500*    Only Alphabets,numbers and space allowed                     08750000
087600     MOVE LIT-ALL-ALPHANUM-FROM-X TO LIT-ALL-ALPHANUM-FROM        08760000
087700                                                                  08770000
087800     INSPECT WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)     08780000
087900       CONVERTING LIT-ALL-ALPHANUM-FROM                           08790000
088000               TO LIT-ALPHANUM-SPACES-TO                          08800000
088100                                                                  08810000
088200     IF FUNCTION LENGTH(                                          08820000
088300             FUNCTION TRIM(                                       08830000
088400             WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)     08840000
088500                            )) = 0                                08850000
088600        CONTINUE                                                  08860000
088700     ELSE                                                         08870000
088800        SET INPUT-ERROR           TO TRUE                         08880000
088900        SET FLG-ALPHNANUM-NOT-OK  TO TRUE                         08890000
089000        IF WS-RETURN-MSG-OFF                                      08900000
089100           STRING                                                 08910000
089200             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)                 08920000
089300             ' can have numbers or alphabets only.'               08930000
089400             DELIMITED BY SIZE                                    08940000
089500             INTO WS-RETURN-MSG                                   08950000
089600           END-STRING                                             08960000
089700        END-IF                                                    08970000
089800        GO TO  1230-EDIT-ALPHANUM-REQD-EXIT                       08980000
089900     END-IF                                                       08990000
090000                                                                  09000000
090100     SET FLG-ALPHNANUM-ISVALID    TO TRUE                         09010000
090200     .                                                            09020000
090300 1230-EDIT-ALPHANUM-REQD-EXIT.                                    09030000
090400     EXIT                                                         09040000
090500     .                                                            09050000
090600                                                                  09060000
090700 1245-EDIT-NUM-REQD.                                              09070000
090800*    Initialize                                                   09080000
090900     SET FLG-ALPHNANUM-NOT-OK          TO TRUE                    09090000
091000                                                                  09100000
091100*    Not supplied                                                 09110000
091200     IF WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)          09120000
091300                                       EQUAL LOW-VALUES           09130000
091400     OR WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH)          09140000
091500         EQUAL SPACES                                             09150000
091600     OR FUNCTION LENGTH(FUNCTION TRIM(                            09160000
091700        WS-EDIT-ALPHANUM-ONLY(1:WS-EDIT-ALPHANUM-LENGTH))) = 0    09170000
091800                                                                  09180000
091900        SET INPUT-ERROR                TO TRUE                    09190000
092000        SET FLG-ALPHNANUM-BLANK        TO TRUE                    09200000
092100        IF WS-RETURN-MSG-OFF                                      09210000
092200           STRING                                                 09220000
092300             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)                 09230000
092400             ' must be supplied.'                                 09240000
092500             DELIMITED BY SIZE                                    09250000
092600             INTO WS-RETURN-MSG                                   09260000
092700           END-STRING                                             09270000
092800        END-IF                                                    09280000
092900        GO TO  1245-EDIT-NUM-REQD-EXIT                            09290000
093000     END-IF                                                       09300000
093100                                                                  09310000
093200*    Only all numeric allowed                                     09320000
093300                                                                  09330000
093400     IF FUNCTION TEST-NUMVAL(WS-EDIT-ALPHANUM-ONLY(1:             09340000
093500                             WS-EDIT-ALPHANUM-LENGTH)) = 0        09350000
093600        CONTINUE                                                  09360000
093700     ELSE                                                         09370000
093800        SET INPUT-ERROR           TO TRUE                         09380000
093900        SET FLG-ALPHNANUM-NOT-OK  TO TRUE                         09390000
094000        IF WS-RETURN-MSG-OFF                                      09400000
094100           STRING                                                 09410000
094200             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)                 09420000
094300             ' must be numeric.'                                  09430000
094400             DELIMITED BY SIZE                                    09440000
094500             INTO WS-RETURN-MSG                                   09450000
094600           END-STRING                                             09460000
094700        END-IF                                                    09470000
094800        GO TO  1245-EDIT-NUM-REQD-EXIT                            09480000
094900     END-IF                                                       09490000
095000*                                                                 09500000
095100                                                                  09510000
095200*    Must not be zero                                             09520000
095300                                                                  09530000
095400     IF FUNCTION NUMVAL(WS-EDIT-ALPHANUM-ONLY(1:                  09540000
095500                        WS-EDIT-ALPHANUM-LENGTH)) = 0             09550000
095600        SET INPUT-ERROR           TO TRUE                         09560000
095700        SET FLG-ALPHNANUM-NOT-OK  TO TRUE                         09570000
095800        IF WS-RETURN-MSG-OFF                                      09580000
095900           STRING                                                 09590000
096000             FUNCTION TRIM(WS-EDIT-VARIABLE-NAME)                 09600000
096100             ' must not be zero.'                                 09610000
096200             DELIMITED BY SIZE                                    09620000
096300             INTO WS-RETURN-MSG                                   09630000
096400           END-STRING                                             09640000
096500        END-IF                                                    09650000
096600        GO TO  1245-EDIT-NUM-REQD-EXIT                            09660000
096700     ELSE                                                         09670000
096800        CONTINUE                                                  09680000
096900     END-IF                                                       09690000
097000                                                                  09700000
097100                                                                  09710000
097200     SET FLG-ALPHNANUM-ISVALID    TO TRUE                         09720000
097300     .                                                            09730000
097400 1245-EDIT-NUM-REQD-EXIT.                                         09740000
097500     EXIT                                                         09750000
097600     .                                                            09760000
097700                                                                  09770000
097800 2000-DECIDE-ACTION.                                              09780000
097900     EVALUATE TRUE                                                09790000
098000******************************************************************09800000
098100*       NO DETAILS SHOWN.                                         09810000
098200*       SO GET THEM AND SETUP DETAIL EDIT SCREEN                  09820000
098300******************************************************************09830000
098400        WHEN TTUP-DETAILS-NOT-FETCHED                             09840000
098500******************************************************************09850000
098600*       CHANGES MADE. BUT USER CANCELS                            09860000
098700******************************************************************09870000
098800        WHEN CCARD-AID-PFK12                                      09880000
098900           IF  FLG-TRANFILTER-ISVALID                             09890000
099000               SET WS-RETURN-MSG-OFF               TO TRUE        09900000
099100               PERFORM 9000-READ-TRANTYPE                         09910000
099200                  THRU 9000-READ-TRANTYPE-EXIT                    09920000
099300               IF FOUND-TRANTYPE-IN-TABLE                         09930000
099400                  SET TTUP-SHOW-DETAILS            TO TRUE        09940000
099500               ELSE                                               09950000
099600                  SET TTUP-DETAILS-NOT-FOUND       TO TRUE        09960000
099700               END-IF                                             09970000
099800           ELSE                                                   09980000
099900               EVALUATE TRUE                                      09990000
100000                  WHEN TTUP-CONFIRM-DELETE                        10000000
100100                   SET WS-DELETE-WAS-CANCELLED     TO TRUE        10010000
100200                   SET TTUP-DETAILS-NOT-FETCHED    TO TRUE        10020000
100300                  WHEN TTUP-CHANGES-OK-NOT-CONFIRMED              10030000
100400                   SET WS-UPDATE-WAS-CANCELLED     TO TRUE        10040000
100500                   SET TTUP-CHANGES-BACKED-OUT     TO TRUE        10050000
100600                  WHEN OTHER                                      10060000
100700                   SET TTUP-DETAILS-NOT-FETCHED    TO TRUE        10070000
100800               END-EVALUATE                                       10080000
100900                                                                  10090000
101000           END-IF                                                 10100000
101100******************************************************************10110000
101200*       DETAILS SHOWN                                             10120000
101300*       BUT USER PRESSES F4 FOR DELETE                            10130000
101400*       ASK THE USER TO CONFIRM THE DELETE                        10140000
101500******************************************************************10150000
101600        WHEN TTUP-CONFIRM-DELETE                                  10160000
101700         AND CCARD-AID-PFK12                                      10170000
101800           SET TTUP-CONFIRM-DELETE                 TO TRUE        10180000
101900******************************************************************10190000
102000*       DETAILS SHOWN                                             10200000
102100*       CHECK CHANGES AND ASK CONFIRMATION IF GOOD                10210000
102200******************************************************************10220000
102300        WHEN TTUP-SHOW-DETAILS                                    10230000
102400           IF INPUT-ERROR                                         10240000
102500           OR NO-CHANGES-DETECTED                                 10250000
102600           OR WS-INVALID-KEY                                      10260000
102700              CONTINUE                                            10270000
102800           ELSE                                                   10280000
102900              SET TTUP-CHANGES-OK-NOT-CONFIRMED TO TRUE           10290000
103000           END-IF                                                 10300000
103100******************************************************************10310000
103200*       DETAILS SHOWN                                             10320000
103300*       BUT INPUT EDIT ERRORS FOUND                               10330000
103400******************************************************************10340000
103500        WHEN TTUP-CHANGES-NOT-OK                                  10350000
103600            CONTINUE                                              10360000
103700******************************************************************10370000
103800*       CHANGES BACKED OUT                                        10380000
103900*       GO BACK TO CHANGES NOT OK STATE                           10390000
104000******************************************************************10400000
104100        WHEN TTUP-CHANGES-BACKED-OUT                              10410000
104200            SET TTUP-CHANGES-NOT-OK            TO TRUE            10420000
104300******************************************************************10430000
104400*       PROBLEMS FOUND IN SEARCH KEYS                             10440000
104500******************************************************************10450000
104600        WHEN TTUP-INVALID-SEARCH-KEYS                             10460000
104700            CONTINUE                                              10470000
104800******************************************************************10480000
104900*       SEARCH KEY WAS VALID.                                     10490000
105000*       BUT DATA WAS NOT FOUND IN TABLE                           10500000
105100*       CUSTOMER DECIDES TO CONTINUE AND ADD RECORD               10510000
105200******************************************************************10520000
105300        WHEN CCARD-AID-PFK05                                      10530000
105400         AND TTUP-DETAILS-NOT-FOUND                               10540000
105500            SET TTUP-CREATE-NEW-RECORD TO TRUE                    10550000
105600******************************************************************10560000
105700*       DETAILS EDITED , FOUND OK, CONFIRM SAVE REQUESTED         10570000
105800*       CONFIRMATION NOT GIVEN. SO SHOW DETAILS AGAIN             10580000
105900******************************************************************10590000
106000        WHEN TTUP-CHANGES-OK-NOT-CONFIRMED                        10600000
106100            CONTINUE                                              10610000
106200******************************************************************10620000
106300*       SHOW CONFIRMATION. GO BACK TO SQUARE 1                    10630000
106400******************************************************************10640000
106500        WHEN TTUP-CHANGES-OKAYED-AND-DONE                         10650000
106600            SET TTUP-SHOW-DETAILS TO TRUE                         10660000
106700            IF CDEMO-FROM-TRANID    EQUAL LOW-VALUES              10670000
106800            OR CDEMO-FROM-TRANID    EQUAL SPACES                  10680000
106900               MOVE ZEROES       TO CDEMO-ACCT-ID                 10690000
107000                                    CDEMO-CARD-NUM                10700000
107100               MOVE LOW-VALUES   TO CDEMO-ACCT-STATUS             10710000
107200            END-IF                                                10720000
107300        WHEN OTHER                                                10730000
107400             MOVE LIT-THISPGM    TO ABEND-CULPRIT                 10740000
107500             MOVE '0001'         TO ABEND-CODE                    10750000
107600             MOVE SPACES         TO ABEND-REASON                  10760000
107700             MOVE 'UNEXPECTED DATA SCENARIO'                      10770000
107800                                 TO ABEND-MSG                     10780000
107900             PERFORM ABEND-ROUTINE                                10790000
108000                THRU ABEND-ROUTINE-EXIT                           10800000
108100     END-EVALUATE                                                 10810000
108200     .                                                            10820000
108300 2000-DECIDE-ACTION-EXIT.                                         10830000
108400     EXIT                                                         10840000
108500     .                                                            10850000
108600                                                                  10860000
108700                                                                  10870000
108800                                                                  10880000
108900 3000-SEND-MAP.                                                   10890000
109000     PERFORM 3100-SCREEN-INIT                                     10900000
109100        THRU 3100-SCREEN-INIT-EXIT                                10910000
109200     PERFORM 3200-SETUP-SCREEN-VARS                               10920000
109300        THRU 3200-SETUP-SCREEN-VARS-EXIT                          10930000
109400     PERFORM 3250-SETUP-INFOMSG                                   10940000
109500        THRU 3250-SETUP-INFOMSG-EXIT                              10950000
109600     PERFORM 3300-SETUP-SCREEN-ATTRS                              10960000
109700        THRU 3300-SETUP-SCREEN-ATTRS-EXIT                         10970000
109800     PERFORM 3390-SETUP-INFOMSG-ATTRS                             10980000
109900        THRU 3390-SETUP-INFOMSG-ATTRS-EXIT                        10990000
110000     PERFORM 3391-SETUP-PFKEY-ATTRS                               11000000
110100        THRU 3391-SETUP-PFKEY-ATTRS-EXIT                          11010000
110200     PERFORM 3400-SEND-SCREEN                                     11020000
110300        THRU 3400-SEND-SCREEN-EXIT                                11030000
110400     .                                                            11040000
110500                                                                  11050000
110600 3000-SEND-MAP-EXIT.                                              11060000
110700     EXIT                                                         11070000
110800     .                                                            11080000
110900                                                                  11090000
111000 3100-SCREEN-INIT.                                                11100000
111100     MOVE LOW-VALUES                TO CTRTUPAO                   11110000
111200                                                                  11120000
111300     MOVE FUNCTION CURRENT-DATE     TO WS-CURDATE-DATA            11130000
111400                                                                  11140000
111500     MOVE CCDA-TITLE01              TO TITLE01O OF CTRTUPAO       11150000
111600     MOVE CCDA-TITLE02              TO TITLE02O OF CTRTUPAO       11160000
111700     MOVE LIT-THISTRANID            TO TRNNAMEO OF CTRTUPAO       11170000
111800     MOVE LIT-THISPGM               TO PGMNAMEO OF CTRTUPAO       11180000
111900                                                                  11190000
112000     MOVE FUNCTION CURRENT-DATE     TO WS-CURDATE-DATA            11200000
112100                                                                  11210000
112200     MOVE WS-CURDATE-MONTH          TO WS-CURDATE-MM              11220000
112300     MOVE WS-CURDATE-DAY            TO WS-CURDATE-DD              11230000
112400     MOVE WS-CURDATE-YEAR(3:2)      TO WS-CURDATE-YY              11240000
112500                                                                  11250000
112600     MOVE WS-CURDATE-MM-DD-YY       TO CURDATEO OF CTRTUPAO       11260000
112700                                                                  11270000
112800     MOVE WS-CURTIME-HOURS          TO WS-CURTIME-HH              11280000
112900     MOVE WS-CURTIME-MINUTE         TO WS-CURTIME-MM              11290000
113000     MOVE WS-CURTIME-SECOND         TO WS-CURTIME-SS              11300000
113100                                                                  11310000
113200     MOVE WS-CURTIME-HH-MM-SS       TO CURTIMEO OF CTRTUPAO       11320000
113300                                                                  11330000
113400     .                                                            11340000
113500                                                                  11350000
113600 3100-SCREEN-INIT-EXIT.                                           11360000
113700     EXIT                                                         11370000
113800     .                                                            11380000
113900                                                                  11390000
114000 3200-SETUP-SCREEN-VARS.                                          11400000
114100*    INITIALIZE SEARCH CRITERIA                                   11410000
114200     IF CDEMO-PGM-ENTER                                           11420000
114300        CONTINUE                                                  11430000
114400     ELSE                                                         11440000
114500        EVALUATE TRUE                                             11450000
114600         WHEN TTUP-DETAILS-NOT-FETCHED                            11460000
114700            PERFORM 3201-SHOW-INITIAL-VALUES                      11470000
114800               THRU 3201-SHOW-INITIAL-VALUES-EXIT                 11480000
114900         WHEN TTUP-SHOW-DETAILS                                   11490000
115000         WHEN TTUP-CONFIRM-DELETE                                 11500000
115100         WHEN TTUP-DELETE-FAILED                                  11510000
115200         WHEN TTUP-DELETE-DONE                                    11520000
115300         WHEN TTUP-CHANGES-BACKED-OUT                             11530000
115400            INITIALIZE TTUP-NEW-DETAILS                           11540000
115500            PERFORM 3202-SHOW-ORIGINAL-VALUES                     11550000
115600               THRU 3202-SHOW-ORIGINAL-VALUES-EXIT                11560000
115700         WHEN TTUP-CHANGES-MADE                                   11570000
115800         WHEN TTUP-CHANGES-NOT-OK                                 11580000
115900         WHEN TTUP-DETAILS-NOT-FOUND                              11590000
116000         WHEN TTUP-INVALID-SEARCH-KEYS                            11600000
116100         WHEN TTUP-CREATE-NEW-RECORD                              11610000
116200         WHEN TTUP-CHANGES-OKAYED-AND-DONE                        11620000
116300            PERFORM 3203-SHOW-UPDATED-VALUES                      11630000
116400               THRU 3203-SHOW-UPDATED-VALUES-EXIT                 11640000
116500         WHEN OTHER                                               11650000
116600            INITIALIZE TTUP-NEW-DETAILS                           11660000
116700            PERFORM 3202-SHOW-ORIGINAL-VALUES                     11670000
116800               THRU 3202-SHOW-ORIGINAL-VALUES-EXIT                11680000
116900        END-EVALUATE                                              11690000
117000      END-IF                                                      11700000
117100     .                                                            11710000
117200 3200-SETUP-SCREEN-VARS-EXIT.                                     11720000
117300     EXIT                                                         11730000
117400     .                                                            11740000
117500                                                                  11750000
117600 3201-SHOW-INITIAL-VALUES.                                        11760000
117700     MOVE LOW-VALUES                     TO  TRTYPCDO OF CTRTUPAO 11770000
117800                                             TRTYPCDO OF CTRTUPAO 11780000
117900     .                                                            11790000
118000                                                                  11800000
118100 3201-SHOW-INITIAL-VALUES-EXIT.                                   11810000
118200     EXIT                                                         11820000
118300     .                                                            11830000
118400                                                                  11840000
118500 3202-SHOW-ORIGINAL-VALUES.                                       11850000
118600                                                                  11860000
118700     MOVE LOW-VALUES                     TO WS-NON-KEY-FLAGS      11870000
118800                                                                  11880000
118900     MOVE TTUP-OLD-TTYP-TYPE             TO TRTYPCDO OF CTRTUPAO  11890000
119000     MOVE TTUP-OLD-TTYP-TYPE-DESC        TO TRTYDSCO OF CTRTUPAO  11900000
119100                                                                  11910000
119200     .                                                            11920000
119300                                                                  11930000
119400 3202-SHOW-ORIGINAL-VALUES-EXIT.                                  11940000
119500     EXIT                                                         11950000
119600     .                                                            11960000
119700 3203-SHOW-UPDATED-VALUES.                                        11970000
119800                                                                  11980000
119900     MOVE TTUP-NEW-TTYP-TYPE             TO TRTYPCDO OF CTRTUPAO  11990000
120000     MOVE TTUP-NEW-TTYP-TYPE-DESC        TO TRTYDSCO OF CTRTUPAO  12000000
120100     .                                                            12010000
120200                                                                  12020000
120300 3203-SHOW-UPDATED-VALUES-EXIT.                                   12030000
120400     EXIT                                                         12040000
120500     .                                                            12050000
120600                                                                  12060000
120700                                                                  12070000
120800                                                                  12080000
120900                                                                  12090000
121000 3250-SETUP-INFOMSG.                                              12100000
121100*    SETUP INFORMATION MESSAGE                                    12110000
121200                                                                  12120000
121300         EVALUATE TRUE                                            12130000
121400         WHEN CDEMO-PGM-ENTER                                     12140000
121500              SET  PROMPT-FOR-SEARCH-KEYS    TO TRUE              12150000
121600         WHEN TTUP-DETAILS-NOT-FETCHED                            12160000
121700         WHEN TTUP-INVALID-SEARCH-KEYS                            12170000
121800              SET PROMPT-FOR-SEARCH-KEYS     TO TRUE              12180000
121900         WHEN TTUP-DETAILS-NOT-FOUND                              12190000
122000              SET PROMPT-CREATE-NEW-RECORD   TO TRUE              12200000
122100         WHEN TTUP-SHOW-DETAILS                                   12210000
122200         WHEN TTUP-CHANGES-BACKED-OUT                             12220000
122300         AND (TTUP-OLD-TTYP-TYPE    = LOW-VALUES                  12230000
122400         OR   TTUP-OLD-TTYP-TYPE    = SPACES)                     12240000
122500              SET  PROMPT-FOR-SEARCH-KEYS    TO TRUE              12250000
122600         WHEN TTUP-CHANGES-BACKED-OUT                             12260000
122700         WHEN TTUP-CHANGES-NOT-OK                                 12270000
122800              SET PROMPT-FOR-CHANGES         TO TRUE              12280000
122900         WHEN TTUP-CONFIRM-DELETE                                 12290000
123000              SET PROMPT-DELETE-CONFIRM      TO TRUE              12300000
123100         WHEN TTUP-DELETE-FAILED                                  12310000
123200              SET INFORM-FAILURE             TO TRUE              12320000
123300         WHEN TTUP-DELETE-DONE                                    12330000
123400              SET CONFIRM-DELETE-SUCCESS     TO TRUE              12340000
123500         WHEN TTUP-CREATE-NEW-RECORD                              12350000
123600              SET PROMPT-FOR-NEWDATA         TO TRUE              12360000
123700         WHEN TTUP-CHANGES-OK-NOT-CONFIRMED                       12370000
123800              SET PROMPT-FOR-CONFIRMATION    TO TRUE              12380000
123900         WHEN TTUP-CHANGES-OKAYED-AND-DONE                        12390000
124000              SET CONFIRM-UPDATE-SUCCESS     TO TRUE              12400000
124100         WHEN TTUP-CHANGES-OKAYED-LOCK-ERROR                      12410000
124200              SET INFORM-FAILURE             TO TRUE              12420000
124300         WHEN TTUP-CHANGES-OKAYED-BUT-FAILED                      12430000
124400              SET INFORM-FAILURE             TO TRUE              12440000
124500         WHEN WS-NO-INFO-MESSAGE                                  12450000
124600             SET PROMPT-FOR-SEARCH-KEYS      TO TRUE              12460000
124700     END-EVALUATE                                                 12470000
124800                                                                  12480000
124900* Center justify the text                                         12490000
125000*                                                                 12500000
125100     COMPUTE WS-STRING-LEN =                                      12510000
125200             FUNCTION LENGTH(                                     12520000
125300                      FUNCTION TRIM(WS-INFO-MSG)                  12530000
125400                            )                                     12540000
125500     COMPUTE WS-STRING-MID =                                      12550000
125600            (FUNCTION LENGTH(WS-INFO-MSG)                         12560000
125700                          - WS-STRING-LEN) / 2 + 1                12570000
125800     MOVE WS-INFO-MSG(1:WS-STRING-LEN)                            12580000
125900       TO WS-STRING-OUT(WS-STRING-MID:                            12590000
126000                        WS-STRING-LEN)                            12600000
126100                                                                  12610000
126200     MOVE WS-STRING-OUT                  TO INFOMSGO OF CTRTUPAO  12620000
126300                                                                  12630000
126400     MOVE WS-RETURN-MSG                  TO ERRMSGO  OF CTRTUPAO  12640000
126500     .                                                            12650000
126600 3250-SETUP-INFOMSG-EXIT.                                         12660000
126700     EXIT                                                         12670000
126800     .                                                            12680000
126900 3300-SETUP-SCREEN-ATTRS.                                         12690000
127000                                                                  12700000
127100*    PROTECT ALL FIELDS                                           12710000
127200     PERFORM 3310-PROTECT-ALL-ATTRS                               12720000
127300        THRU 3310-PROTECT-ALL-ATTRS-EXIT                          12730000
127400                                                                  12740000
127500*    UNPROTECT BASED ON CONTEXT                                   12750000
127600     EVALUATE TRUE                                                12760000
127700        WHEN TTUP-DETAILS-NOT-FETCHED                             12770000
127800        WHEN TTUP-INVALID-SEARCH-KEYS                             12780000
127900        WHEN TTUP-DETAILS-NOT-FOUND                               12790000
128000        WHEN TTUP-CHANGES-BACKED-OUT                              12800000
128100         AND (TTUP-OLD-TTYP-TYPE    = LOW-VALUES                  12810000
128200         OR   TTUP-OLD-TTYP-TYPE    = SPACES)                     12820000
128300*            Make Search Keys editable                            12830000
128400             MOVE DFHBMFSE      TO TRTYPCDA OF CTRTUPAI           12840000
128500        WHEN TTUP-SHOW-DETAILS                                    12850000
128600        WHEN TTUP-CHANGES-NOT-OK                                  12860000
128700        WHEN TTUP-CREATE-NEW-RECORD                               12870000
128800        WHEN TTUP-CHANGES-BACKED-OUT                              12880000
128900             PERFORM 3320-UNPROTECT-FEW-ATTRS                     12890000
129000                THRU 3320-UNPROTECT-FEW-ATTRS-EXIT                12900000
129100        WHEN TTUP-CHANGES-OK-NOT-CONFIRMED                        12910000
129200        WHEN TTUP-CHANGES-OKAYED-AND-DONE                         12920000
129300        WHEN TTUP-DELETE-IN-PROGRESS                              12930000
129400*            Keep all fields protected                            12940000
129500             CONTINUE                                             12950000
129600        WHEN OTHER                                                12960000
129700             MOVE DFHBMFSE      TO TRTYPCDA OF CTRTUPAI           12970000
129800     END-EVALUATE                                                 12980000
129900                                                                  12990000
130000******************************************************************13000000
130100*    POSITION CURSOR - ORDER BASED ON SCREEN LOCATION             13010000
130200******************************************************************13020000
130300     EVALUATE TRUE                                                13030000
130400        WHEN TTUP-DETAILS-NOT-FETCHED                             13040000
130500        WHEN TTUP-DETAILS-NOT-FOUND                               13050000
130600        WHEN TTUP-INVALID-SEARCH-KEYS                             13060000
130700        WHEN FLG-TRANFILTER-NOT-OK                                13070000
130800        WHEN FLG-TRANFILTER-BLANK                                 13080000
130900        WHEN TTUP-CHANGES-OKAYED-AND-DONE                         13090000
131000        WHEN TTUP-CHANGES-BACKED-OUT                              13100000
131100         AND (TTUP-OLD-TTYP-TYPE    = LOW-VALUES                  13110000
131200         OR   TTUP-OLD-TTYP-TYPE    = SPACES)                     13120000
131300             MOVE -1             TO TRTYPCDL OF CTRTUPAI          13130000
131400*    Description                                                  13140000
131500        WHEN TTUP-CREATE-NEW-RECORD                               13150000
131600        WHEN NO-CHANGES-DETECTED                                  13160000
131700        WHEN FLG-DESCRIPTION-NOT-OK                               13170000
131800        WHEN FLG-DESCRIPTION-BLANK                                13180000
131900        WHEN TTUP-CHANGES-MADE                                    13190000
132000        WHEN TTUP-CHANGES-BACKED-OUT                              13200000
132100        WHEN TTUP-SHOW-DETAILS                                    13210000
132200            MOVE -1              TO TRTYDSCL OF CTRTUPAI          13220000
132300        WHEN OTHER                                                13230000
132400            MOVE -1              TO TRTYPCDL OF CTRTUPAI          13240000
132500      END-EVALUATE                                                13250000
132600                                                                  13260000
132700******************************************************************13270000
132800*    SETUP COLOR                                                  13280000
132900******************************************************************13290000
133000*    Transaction Type code filer                                  13300000
133100     IF FLG-TRANFILTER-NOT-OK                                     13310000
133200     OR TTUP-DELETE-FAILED                                        13320000
133300        MOVE DFHRED              TO TRTYPCDC OF CTRTUPAO          13330000
133400     END-IF                                                       13340000
133500                                                                  13350000
133600     IF  FLG-TRANFILTER-BLANK                                     13360000
133700     AND CDEMO-PGM-REENTER                                        13370000
133800         MOVE '*'                TO TRTYPCDO OF CTRTUPAO          13380000
133900         MOVE DFHRED             TO TRTYPCDC OF CTRTUPAO          13390000
134000     END-IF                                                       13400000
134100                                                                  13410000
134200     IF TTUP-DETAILS-NOT-FETCHED                                  13420000
134300     OR TTUP-DETAILS-NOT-FOUND                                    13430000
134400     OR TTUP-INVALID-SEARCH-KEYS                                  13440000
134500     OR FLG-TRANFILTER-BLANK                                      13450000
134600     OR FLG-TRANFILTER-NOT-OK                                     13460000
134700        GO TO 3300-SETUP-SCREEN-ATTRS-EXIT                        13470000
134800     ELSE                                                         13480000
134900        CONTINUE                                                  13490000
135000     END-IF                                                       13500000
135100                                                                  13510000
135200******************************************************************13520000
135300*    Using Copy replacing to set attribs for remaining vars       13530000
135400*    Write specific code only if rules differ                     13540000
135500******************************************************************13550000
135600                                                                  13560000
135700*    Transaction Description Status                               13570000
135800     COPY CSSETATY REPLACING                                      13580000
135900       ==(TESTVAR1)== BY ==DESCRIPTION==                          13590000
136000       ==(SCRNVAR2)== BY ==TRTYDSC==                              13600000
136100       ==(MAPNAME3)== BY ==CTRTUPA== .                            13610000
136200                                                                  13620000
136300     .                                                            13630000
136400 3300-SETUP-SCREEN-ATTRS-EXIT.                                    13640000
136500     EXIT                                                         13650000
136600     .                                                            13660000
136700                                                                  13670000
136800 3310-PROTECT-ALL-ATTRS.                                          13680000
136900     MOVE DFHBMPRF              TO TRTYPCDA OF CTRTUPAI           13690000
137000                                   TRTYDSCA OF CTRTUPAI           13700000
137100                                   INFOMSGA OF CTRTUPAI           13710000
137200     .                                                            13720000
137300 3310-PROTECT-ALL-ATTRS-EXIT.                                     13730000
137400     EXIT                                                         13740000
137500     .                                                            13750000
137600                                                                  13760000
137700 3320-UNPROTECT-FEW-ATTRS.                                        13770000
137800                                                                  13780000
137900     MOVE DFHBMFSE              TO TRTYDSCA OF CTRTUPAI           13790000
138000     MOVE DFHBMPRF              TO INFOMSGA OF CTRTUPAI           13800000
138100     .                                                            13810000
138200 3320-UNPROTECT-FEW-ATTRS-EXIT.                                   13820000
138300     EXIT                                                         13830000
138400     .                                                            13840000
138500                                                                  13850000
138600 3390-SETUP-INFOMSG-ATTRS.                                        13860000
138700     IF  WS-NO-INFO-MESSAGE                                       13870000
138800         MOVE DFHBMDAR           TO INFOMSGA OF CTRTUPAI          13880000
138900     ELSE                                                         13890000
139000         MOVE DFHBMASB           TO INFOMSGA OF CTRTUPAI          13900000
139100     END-IF                                                       13910000
139200     .                                                            13920000
139300 3390-SETUP-INFOMSG-ATTRS-EXIT.                                   13930000
139400     EXIT                                                         13940000
139500     .                                                            13950000
139600                                                                  13960000
139700 3391-SETUP-PFKEY-ATTRS.                                          13970000
139800*    Should reflect in 0001-CHECK-PFKEYS                          13980000
139900*    Enter key                                                    13990000
140000     IF TTUP-CONFIRM-DELETE                                       14000000
140100        MOVE DFHBMDAR            TO FKEYSA   OF CTRTUPAI          14010000
140200     ELSE                                                         14020000
140300        MOVE DFHBMASB            TO FKEYSA   OF CTRTUPAI          14030000
140400     END-IF                                                       14040000
140500*    F4                                                           14050000
140600     IF TTUP-SHOW-DETAILS                                         14060000
140700     OR TTUP-CONFIRM-DELETE                                       14070000
140800         MOVE DFHBMASB           TO FKEY04A  OF CTRTUPAI          14080000
140900     END-IF                                                       14090000
141000*    F5                                                           14100000
141100     IF TTUP-CHANGES-OK-NOT-CONFIRMED                             14110000
141200     OR TTUP-DETAILS-NOT-FOUND                                    14120000
141300         MOVE DFHBMASB           TO FKEY05A  OF CTRTUPAI          14130000
141400     END-IF                                                       14140000
141500*    F12                                                          14150000
141600     IF TTUP-CHANGES-OK-NOT-CONFIRMED                             14160000
141700     OR TTUP-SHOW-DETAILS                                         14170000
141800     OR TTUP-DETAILS-NOT-FOUND                                    14180000
141900     OR TTUP-CONFIRM-DELETE                                       14190000
142000     OR TTUP-CREATE-NEW-RECORD                                    14200000
142100         MOVE DFHBMASB           TO FKEY12A  OF CTRTUPAI          14210000
142200     END-IF                                                       14220000
142300     .                                                            14230000
142400 3391-SETUP-PFKEY-ATTRS-EXIT.                                     14240000
142500     EXIT                                                         14250000
142600     .                                                            14260000
142700                                                                  14270000
142800 3400-SEND-SCREEN.                                                14280000
142900                                                                  14290000
143000     MOVE LIT-THISMAPSET         TO CCARD-NEXT-MAPSET             14300000
143100     MOVE LIT-THISMAP            TO CCARD-NEXT-MAP                14310000
143200                                                                  14320000
143300     EXEC CICS SEND MAP(CCARD-NEXT-MAP)                           14330000
143400                    MAPSET(CCARD-NEXT-MAPSET)                     14340000
143500                    FROM(CTRTUPAO)                                14350000
143600                    CURSOR                                        14360000
143700                    ERASE                                         14370000
143800                    FREEKB                                        14380000
143900                    RESP(WS-RESP-CD)                              14390000
144000     END-EXEC                                                     14400000
144100     .                                                            14410000
144200 3400-SEND-SCREEN-EXIT.                                           14420000
144300     EXIT                                                         14430000
144400     .                                                            14440000
144500                                                                  14450000
144600                                                                  14460000
144700 9000-READ-TRANTYPE.                                              14470000
144800                                                                  14480000
144900     INITIALIZE TTUP-OLD-DETAILS                                  14490000
145000                                                                  14500000
145100     SET  WS-NO-INFO-MESSAGE      TO TRUE                         14510000
145200                                                                  14520000
145300     PERFORM 9100-GET-TRANSACTION-TYPE                            14530000
145400        THRU 9100-GET-TRANSACTION-TYPE-EXIT                       14540000
145500                                                                  14550000
145600     IF FLG-TRANFILTER-NOT-OK                                     14560000
145700        GO TO 9000-READ-TRANTYPE-EXIT                             14570000
145800     END-IF                                                       14580000
145900                                                                  14590000
146000                                                                  14600000
146100     PERFORM 9500-STORE-FETCHED-DATA                              14610000
146200        THRU 9500-STORE-FETCHED-DATA-EXIT                         14620000
146300     .                                                            14630000
146400                                                                  14640000
146500                                                                  14650000
146600 9000-READ-TRANTYPE-EXIT.                                         14660000
146700     EXIT                                                         14670000
146800     .                                                            14680000
146900 9100-GET-TRANSACTION-TYPE.                                       14690000
147000                                                                  14700000
147100*    Read the Card file. Access via alternate index ACCTID        14710000
147200*                                                                 14720000
147300     MOVE TTUP-NEW-TTYP-TYPE TO DCL-TR-TYPE                       14730000
147400                                                                  14740000
147500     EXEC SQL                                                     14750000
147600          SELECT TR_TYPE                                          14760000
147700                ,TR_DESCRIPTION                                   14770000
147800            INTO :DCL-TR-TYPE                                     14780000
147900                ,:DCL-TR-DESCRIPTION                              14790000
148000            FROM CARDDEMO.TRANSACTION_TYPE                        14800000
148100           WHERE TR_TYPE = :DCL-TR-TYPE                           14810000
148200     END-EXEC                                                     14820000
148300                                                                  14830000
148400     MOVE SQLCODE                            TO WS-DISP-SQLCODE   14840000
148500                                                                  14850000
148600     EVALUATE TRUE                                                14860000
148700         WHEN SQLCODE = ZERO                                      14870000
148800            SET FOUND-TRANTYPE-IN-TABLE     TO TRUE               14880000
148900         WHEN SQLCODE = +100                                      14890000
149000            SET INPUT-ERROR                 TO TRUE               14900000
149100            SET FLG-TRANFILTER-NOT-OK       TO TRUE               14910000
149200            IF WS-RETURN-MSG-OFF                                  14920000
149300              SET WS-RECORD-NOT-FOUND       TO TRUE               14930000
149400            END-IF                                                14940000
149500         WHEN SQLCODE < 0                                         14950000
149600            SET INPUT-ERROR                 TO TRUE               14960000
149700            SET FLG-TRANFILTER-NOT-OK       TO TRUE               14970000
149800            IF WS-RETURN-MSG-OFF                                  14980000
149900              STRING                                              14990000
150000              'Error accessing:'                                  15000000
150100              ' TRANSACTION_TYPE table. SQLCODE:'                 15010000
150200              WS-DISP-SQLCODE                                     15020000
150300              ':'                                                 15030000
150400              SQLERRM OF SQLCA                                    15040000
150500              DELIMITED BY SIZE                                   15050000
150600              INTO WS-RETURN-MSG                                  15060000
150700              END-STRING                                          15070000
150800            END-IF                                                15080000
150900     END-EVALUATE                                                 15090000
151000     EXIT                                                         15100000
151100     .                                                            15110000
151200 9100-GET-TRANSACTION-TYPE-EXIT.                                  15120000
151300     EXIT                                                         15130000
151400     .                                                            15140000
151500                                                                  15150000
151600                                                                  15160000
151700 9500-STORE-FETCHED-DATA.                                         15170000
151800                                                                  15180000
151900     INITIALIZE TTUP-OLD-DETAILS                                  15190000
152000******************************************************************15200000
152100*    Transaction Type data                                        15210000
152200******************************************************************15220000
152300     MOVE DCL-TR-TYPE         TO TTUP-OLD-TTYP-TYPE               15230000
152400     MOVE DCL-TR-DESCRIPTION-TEXT(1: DCL-TR-DESCRIPTION-LEN)      15240000
152500                              TO TTUP-OLD-TTYP-TYPE-DESC          15250000
152600                                                                  15260000
152700     .                                                            15270000
152800 9500-STORE-FETCHED-DATA-EXIT.                                    15280000
152900     EXIT                                                         15290000
153000     .                                                            15300000
153100 9600-WRITE-PROCESSING.                                           15310000
153200                                                                  15320000
153300***************************************************************** 15330000
153400* Update Transaction Type *                                       15340000
153500***************************************************************** 15350000
153600*    Issue Update                                                 15360000
153700*                                                                 15370000
153800     MOVE TTUP-NEW-TTYP-TYPE TO DCL-TR-TYPE                       15380000
153900     MOVE FUNCTION TRIM(TTUP-NEW-TTYP-TYPE-DESC)                  15390000
154000                             TO DCL-TR-DESCRIPTION-TEXT           15400000
154100     COMPUTE DCL-TR-DESCRIPTION-LEN                               15410000
154200      = FUNCTION LENGTH(TTUP-NEW-TTYP-TYPE-DESC)                  15420000
154300                                                                  15430000
154400     EXEC SQL                                                     15440000
154500          UPDATE CARDDEMO.TRANSACTION_TYPE                        15450000
154600             SET TR_DESCRIPTION = :DCL-TR-DESCRIPTION             15460000
154700           WHERE TR_TYPE = :DCL-TR-TYPE                           15470000
154800     END-EXEC                                                     15480000
154900                                                                  15490000
155000***************************************************************** 15500000
155100* Did Transaction Type update succeed ?  *                        15510000
155200***************************************************************** 15520000
155300     MOVE SQLCODE                       TO WS-DISP-SQLCODE        15530000
155400                                                                  15540000
155500     EVALUATE TRUE                                                15550000
155600         WHEN SQLCODE = ZERO                                      15560000
155700            EXEC CICS SYNCPOINT END-EXEC                          15570000
155800         WHEN SQLCODE = +100                                      15580000
155900            PERFORM 9700-INSERT-RECORD                            15590000
156000               THRU 9700-INSERT-RECORD-EXIT                       15600000
156100         WHEN SQLCODE = -911                                      15610000
156200            SET INPUT-ERROR                    TO TRUE            15620000
156300            IF  WS-RETURN-MSG-OFF                                 15630000
156400                SET COULD-NOT-LOCK-REC-FOR-UPDATE                 15640000
156500                                               TO TRUE            15650000
156600            END-IF                                                15660000
156700         WHEN SQLCODE < 0                                         15670000
156800            SET TABLE-UPDATE-FAILED            TO TRUE            15680000
156900              STRING                                              15690000
157000              'Error updating:'                                   15700000
157100              ' TRANSACTION_TYPE Table. SQLCODE:'                 15710000
157200              WS-DISP-SQLCODE                                     15720000
157300              ':'                                                 15730000
157400              SQLERRM OF SQLCA                                    15740000
157500              DELIMITED BY SIZE                                   15750000
157600              INTO WS-RETURN-MSG                                  15760000
157700              END-STRING                                          15770000
157800     END-EVALUATE                                                 15780000
157900                                                                  15790000
158000     EVALUATE TRUE                                                15800000
158100        WHEN COULD-NOT-LOCK-REC-FOR-UPDATE                        15810000
158200             SET TTUP-CHANGES-OKAYED-LOCK-ERROR TO TRUE           15820000
158300        WHEN TABLE-UPDATE-FAILED                                  15830000
158400             SET TTUP-CHANGES-OKAYED-BUT-FAILED TO TRUE           15840000
158500        WHEN DATA-WAS-CHANGED-BEFORE-UPDATE                       15850000
158600             SET TTUP-SHOW-DETAILS              TO TRUE           15860000
158700        WHEN OTHER                                                15870000
158800           SET TTUP-CHANGES-OKAYED-AND-DONE     TO TRUE           15880000
158900     END-EVALUATE                                                 15890000
159000                                                                  15900000
159100     EXIT                                                         15910000
159200     .                                                            15920000
159300 9600-WRITE-PROCESSING-EXIT.                                      15930000
159400     EXIT                                                         15940000
159500     .                                                            15950000
159600 9700-INSERT-RECORD.                                              15960000
159700     EXEC SQL                                                     15970000
159800          INSERT INTO CARDDEMO.TRANSACTION_TYPE                   15980000
159900                     (TR_TYPE, TR_DESCRIPTION)                    15990000
160000             VALUES ( :DCL-TR-TYPE                                16000000
160100                     ,:DCL-TR-DESCRIPTION)                        16010000
160200     END-EXEC                                                     16020000
160300                                                                  16030000
160400     EVALUATE TRUE                                                16040000
160500         WHEN SQLCODE = ZERO                                      16050000
160600            EXEC CICS SYNCPOINT END-EXEC                          16060000
160700         WHEN OTHER                                               16070000
160800            SET TABLE-UPDATE-FAILED            TO TRUE            16080000
160900              STRING                                              16090000
161000              'Error inserting record into:'                      16100000
161100              ' TRANSACTION_TYPE Table. SQLCODE:'                 16110000
161200              WS-DISP-SQLCODE                                     16120000
161300              ':'                                                 16130000
161400              SQLERRM OF SQLCA                                    16140000
161500              DELIMITED BY SIZE                                   16150000
161600              INTO WS-RETURN-MSG                                  16160000
161700              END-STRING                                          16170000
161800            GO TO 9700-INSERT-RECORD-EXIT                         16180000
161900     END-EVALUATE                                                 16190000
162000     .                                                            16200000
162100 9700-INSERT-RECORD-EXIT.                                         16210000
162200     EXIT                                                         16220000
162300     .                                                            16230000
162400 9800-DELETE-PROCESSING.                                          16240000
162500     MOVE TTUP-OLD-TTYP-TYPE TO DCL-TR-TYPE                       16250000
162600                                                                  16260000
162700     EXEC SQL                                                     16270000
162800          DELETE FROM CARDDEMO.TRANSACTION_TYPE                   16280000
162900           WHERE TR_TYPE = :DCL-TR-TYPE                           16290000
163000     END-EXEC                                                     16300000
163100                                                                  16310000
163200     MOVE SQLCODE                             TO WS-DISP-SQLCODE  16320000
163300                                                                  16330000
163400     EVALUATE TRUE                                                16340000
163500         WHEN SQLCODE = ZERO                                      16350000
163600            SET TTUP-DELETE-DONE              TO TRUE             16360000
163700            EXEC CICS SYNCPOINT END-EXEC                          16370000
163800         WHEN SQLCODE = -532                                      16380000
163900            SET RECORD-DELETE-FAILED          TO TRUE             16390000
164000              STRING                                              16400000
164100              'Please delete associated child records first:'     16410000
164200              'SQLCODE :'                                         16420000
164300              WS-DISP-SQLCODE                                     16430000
164400              ':'                                                 16440000
164500              SQLERRM OF SQLCA                                    16450000
164600              SQLERRM OF SQLCA                                    16460000
164700              DELIMITED BY SIZE                                   16470000
164800              INTO WS-RETURN-MSG                                  16480000
164900              END-STRING                                          16490000
165000         WHEN OTHER                                               16500000
165100            SET RECORD-DELETE-FAILED          TO TRUE             16510000
165200            SET TTUP-DELETE-FAILED            TO TRUE             16520000
165300              STRING                                              16530000
165400              'Delete failed with message:'                       16540000
165500              'SQLCODE :'                                         16550000
165600              WS-DISP-SQLCODE                                     16560000
165700              ':'                                                 16570000
165800              SQLERRM OF SQLCA                                    16580000
165900              DELIMITED BY SIZE                                   16590000
166000              INTO WS-RETURN-MSG                                  16600000
166100              END-STRING                                          16610000
166200     END-EVALUATE                                                 16620000
166300     .                                                            16630000
166400 9800-DELETE-PROCESSING-EXIT.                                     16640000
166500     EXIT                                                         16650000
166600     .                                                            16660000
166700                                                                  16670000
166800******************************************************************16680000
166900*Common code to store PFKey                                       16690000
167000******************************************************************16700000
167100 COPY 'CSSTRPFY'                                                  16710000
167200     .                                                            16720000
167300                                                                  16730000
167400                                                                  16740000
167500 ABEND-ROUTINE.                                                   16750000
167600                                                                  16760000
167700     IF ABEND-MSG EQUAL LOW-VALUES                                16770000
167800        MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG            16780000
167900     END-IF                                                       16790000
168000                                                                  16800000
168100     MOVE LIT-THISPGM       TO ABEND-CULPRIT                      16810000
168200     MOVE '9999'            TO ABEND-CODE                         16820000
168300                                                                  16830000
168400     EXEC CICS SEND                                               16840000
168500                      FROM (ABEND-DATA)                           16850000
168600                      LENGTH(LENGTH OF ABEND-DATA)                16860000
168700                      NOHANDLE                                    16870000
168800                      ERASE                                       16880000
168900     END-EXEC                                                     16890000
169000                                                                  16900000
169100     EXEC CICS HANDLE ABEND                                       16910000
169200          CANCEL                                                  16920000
169300     END-EXEC                                                     16930000
169400                                                                  16940000
169500     EXEC CICS ABEND                                              16950000
169600          ABCODE(ABEND-CODE)                                      16960000
169700     END-EXEC                                                     16970000
169800     .                                                            16980000
169900 ABEND-ROUTINE-EXIT.                                              16990000
170000     EXIT                                                         17000000
170100     .                                                            17010000
170200                                                                  17020000
