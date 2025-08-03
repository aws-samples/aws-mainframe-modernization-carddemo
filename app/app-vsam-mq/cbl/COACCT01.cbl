000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID.           COACCT01 IS INITIAL.                       00020001
000300 AUTHOR.               AWS.                                       00030000
000400 DATE-WRITTEN.         03/21.                                     00040000
000500 DATE-COMPILED.                                                   00050000
000600                                                                  00060000
000700 ENVIRONMENT DIVISION.                                            00070000
000800                                                                  00080000
000900 DATA DIVISION.                                                   00090000
001000                                                                  00100000
001100 WORKING-STORAGE SECTION.                                         00110000
001700                                                                  00170000
001800 01 WS-MQ-MSG-FLAG                PIC X(01) VALUE 'N'.            00180007
001900    88  NO-MORE-MSGS              VALUE 'Y'.                      00190007
002000                                                                  00200000
002100 01 WS-RESP-QUEUE-STS            PIC X(01) VALUE 'N'.             00210007
002200    88  RESP-QUEUE-OPEN          VALUE 'Y'.                       00220007
002300                                                                  00230000
002400 01 WS-ERR-QUEUE-STS             PIC X(01) VALUE 'N'.             00240007
002500    88  ERR-QUEUE-OPEN          VALUE 'Y'.                        00250007
002600                                                                  00260000
002700 01 WS-REPLY-QUEUE-STS           PIC X(01) VALUE 'N'.             00270007
002800    88  REPLY-QUEUE-OPEN         VALUE 'Y'.                       00280007
002900                                                                  00290000
003700                                                                  00370000
003800 01 WS-CICS-RESP-CDS.                                             00380007
003900    05  WS-CICS-RESP1-CD        PIC S9(08) COMP VALUE ZERO.       00390007
004000    05  WS-CICS-RESP2-CD        PIC S9(08) COMP VALUE ZERO.       00400007
004300    05  WS-CICS-RESP1-CD-D      PIC 9(08) VALUE ZERO.             00430007
004400    05  WS-CICS-RESP2-CD-D      PIC 9(08) VALUE ZERO.             00440007
004500                                                                  00450000
004600***********************************************                   00460000
004700**             DATE FIELDS                   **                   00470000
004800***********************************************                   00480000
004900 01 WS-DATE-TIME.                                                 00490000
005000    10 WS-ABS-TIME                  PIC S9(15) COMP-3 VALUE ZERO. 00500000
005100    10 WS-MMDDYYYY                  PIC X(10) VALUE SPACES.       00510000
005200    10 WS-TIME                      PIC X(8)  VALUE SPACES.       00520000
004600***********************************************                   00530000
004700**             MQ FIELDS                     **                   00540000
004800***********************************************                   00550000
005000 01 MQ-QUEUE                        PIC X(48).                    00570000
005100 01 MQ-QUEUE-REPLY                  PIC X(48).                    00580000
005200 01 MQ-HCONN                        PIC S9(09) BINARY VALUE 0.    00590000
005300 01 MQ-CONDITION-CODE               PIC S9(09) BINARY VALUE 0.    00600000
005400 01 MQ-REASON-CODE                  PIC S9(09) BINARY VALUE 0.    00610000
005500 01 MQ-HOBJ                         PIC S9(09) BINARY VALUE 0.    00620000
005600 01 MQ-OPTIONS                      PIC S9(09) BINARY VALUE 0.    00630000
005700 01 MQ-BUFFER-LENGTH                PIC S9(09) BINARY.            00640000
005800 01 MQ-BUFFER                       PIC X(1000).                  00650000
005900 01 MQ-DATA-LENGTH                  PIC S9(09) BINARY.            00660000
006000 01 MQ-CORRELID                     PIC X(24).                    00670000
006100 01 MQ-MSG-ID                       PIC X(24).                    00680000
006200 01 MQ-MSG-COUNT                    PIC 9(09).                    00690000
006300 01 SAVE-CORELID                    PIC X(24).                    00700000
006400 01 SAVE-MSGID                      PIC X(24).                    00710000
006500 01 SAVE-REPLY2Q                    PIC X(48).                    00720000
006600 01 MQ-ERR-DISPLAY.                                               00730000
006700     05 MQ-ERROR-PARA                   PIC X(25) .               00740000
006800     05 FILLER                          PIC X(02) VALUE SPACES.   00750000
006900     05 MQ-APPL-RETURN-MESSAGE          PIC X(25).                00760000
007000     05 FILLER                          PIC X(02) VALUE SPACES.   00770000
007100     05 MQ-APPL-CONDITION-CODE          PIC 9(02).                00780000
007200     05 FILLER                          PIC X(02) VALUE SPACES.   00790000
007300     05 MQ-APPL-REASON-CODE             PIC 9(05).                00800000
007400     05 FILLER                          PIC X(02) VALUE SPACES.   00810000
007500     05 MQ-APPL-QUEUE-NAME              PIC X(48).                00820000
007600                                                                  00830000
007700                                                                  00840000
007800 01 MQ-GET-MESSAGE-OPTIONS.                                       00850000
007900 COPY CMQGMOV.                                                    00860000
008000                                                                  00870000
008100                                                                  00880000
008200 01 MQ-PUT-MESSAGE-OPTIONS.                                       00890000
008300 COPY CMQPMOV.                                                    00900000
008400                                                                  00910000
008500                                                                  00920000
008600 01 MQ-MESSAGE-DESCRIPTOR.                                        00930000
008700 COPY CMQMDV.                                                     00940000
008800                                                                  00950000
008900                                                                  00960000
009000 01 MQ-OBJECT-DESCRIPTOR.                                         00970000
009100 COPY CMQODV.                                                     00980000
009200                                                                  00990000
009300                                                                  01000000
009400 01 MQ-CONSTANTS.                                                 01010000
009500 COPY CMQV.                                                       01020000
009600                                                                  01030000
009700 01 MQ-GET-QUEUE-MESSAGE.                                         01040000
009800 COPY CMQTML.                                                     01050000
009900                                                                  01060000
010000 01  QUEUE-INFO.                                                  01070000
010100     05 QMGR-NAME                   PIC X(48) VALUE SPACES.       01080007
010200     05 INPUT-QUEUE-NAME            PIC X(48) VALUE SPACES.       01090000
010300     05 REPLY-QUEUE-NAME            PIC X(48) VALUE SPACES.       01100000
010400     05 ERROR-QUEUE-NAME            PIC X(48) VALUE SPACES.       01110000
010500                                                                  01120000
010600 01 INPUT-QUEUE-HANDLE              PIC S9(09) BINARY VALUE 0.    01130000
010700                                                                  01140000
010800 01 OUTPUT-QUEUE-HANDLE             PIC S9(09) BINARY VALUE 0.    01150000
010900                                                                  01160000
011000 01 ERROR-QUEUE-HANDLE              PIC S9(09) BINARY VALUE 0.    01170000
011100                                                                  01180000
011200 01 QMGR-HANDLE-CONN                PIC S9(09) BINARY VALUE 0.    01190000
011300 01 QUEUE-MESSAGE                   PIC X(1000).                  01200000
011400 01 REQUEST-MESSAGE                 PIC X(1000).                  01210000
011500 01 REPLY-MESSAGE                   PIC X(1000).                  01220000
011600 01 ERROR-MESSAGE                   PIC X(1000).                  01230000
011700 01 REQUEST-MSG-COPY.                                             01240002
011700    10 WS-FUNC                      PIC X(04) VALUE SPACES.       01241000
011700    10 WS-KEY                       PIC 9(11) VALUE ZEROES.       01242000
011700    10 WS-FILLER                    PIC X(985) VALUE SPACES.      01243000
011800                                                                  01250000
       01 WS-VARIABLES.                                                 01251000
          05 LIT-ACCTFILENAME                      PIC X(8)             01251100
                                                   VALUE 'ACCTDAT '.    01251200
          05 WS-RESP-CD                          PIC S9(09) COMP        01251300
                                                   VALUE ZEROS.         01251400
          05 WS-REAS-CD                          PIC S9(09) COMP        01251500
                                                   VALUE ZEROS.         01251600
         05  WS-XREF-RID.                                               01251700
           10  WS-CARD-RID-CARDNUM                 PIC X(16).           01252000
           10  WS-CARD-RID-CUST-ID                 PIC 9(09).           01253000
           10  WS-CARD-RID-CUST-ID-X REDEFINES                          01254000
                  WS-CARD-RID-CUST-ID              PIC X(09).           01255000
           10  WS-CARD-RID-ACCT-ID                 PIC 9(11).           01256000
           10  WS-CARD-RID-ACCT-ID-X REDEFINES                          01257000
                  WS-CARD-RID-ACCT-ID              PIC X(11).           01258000
                                                                        01259000
       01 WS-ACCT-RESPONSE.                                             01259107
                                                                        01259207
           05  WS-ACCT-LBL                       PIC X(13) VALUE        01259307
                                                     'ACCOUNT ID : '.   01259407
           05  WS-ACCT-ID                        PIC 9(11) VALUE ZEROES.01259507
           05  WS-STATUS-LBL                     PIC X(17) VALUE        01259608
                                                'ACCOUNT STATUS : '.    01259707
           05  WS-ACCT-ACTIVE-STATUS             PIC X(01) VALUE SPACES.01259807
           05  WS-CURR-BAL-LBL                   PIC X(10) VALUE        01259907
                                                     'BALANCE : '.      01260007
           05  WS-ACCT-CURR-BAL                  PIC S9(10)V99          01260107
                                                           VALUE ZEROES.01260207
           05  WS-CRDT-LMT-LBL                   PIC X(15) VALUE        01260307
                                                 'CREDIT LIMIT : '.     01260407
           05  WS-ACCT-CREDIT-LIMIT              PIC S9(10)V99          01260507
                                                           VALUE ZEROES.01260607
           05  WS-CASH-LIMIT-LBL                 PIC X(13) VALUE        01260707
                                                 'CASH LIMIT : '.       01260807
           05  WS-ACCT-CASH-CREDIT-LIMIT         PIC S9(10)V99          01260909
                                                           VALUE ZEROES.01261007
           05  WS-OPEN-DATE-LBL                  PIC X(12) VALUE        01261107
                                                 'OPEN DATE : '.        01261207
           05  WS-ACCT-OPEN-DATE                 PIC X(10) VALUE SPACES.01261307
           05  WS-EXPR-DATE-LBL                  PIC X(12) VALUE        01261407
                                                 'EXPR DATE : '.        01261507
           05  WS-ACCT-EXPIRAION-DATE            PIC X(10) VALUE SPACES.01261607
           05  WS-REISSUE-DT-LBL                 PIC X(12) VALUE        01261707
                                                 'REIS DATE : '.        01261807
           05  WS-ACCT-REISSUE-DATE              PIC X(10) VALUE SPACES.01261907
           05  WS-CURR-CYC-CREDIT-LBL            PIC X(13) VALUE        01262007
                                                 'CREDIT BAL : '.       01262107
           05  WS-ACCT-CURR-CYC-CREDIT           PIC S9(10)V99          01262207
                                                           VALUE ZEROES.01262307
           05  WS-CURR-CYC-DEBIT-LBL             PIC X(12) VALUE        01262407
                                                 'DEBIT BAL : '.        01262507
           05  WS-ACCT-CURR-CYC-DEBIT            PIC S9(10)V99          01262607
                                                           VALUE ZEROES.01262707
           05  WS-ACCT-GRP-LBL                   PIC X(11) VALUE        01262807
                                                 'GROUP ID : '.         01262907
           05  WS-ACCT-GROUP-ID                  PIC X(10) VALUE SPACES.01263010
      *ACCOUNT RECORD LAYOUT                                            01263107
       COPY CVACT01Y.                                                   01263207
                                                                        01263307
011900                                                                  01264000
012000 LINKAGE SECTION.                                                 01270000
012100                                                                  01280000
012200 PROCEDURE DIVISION.                                              01290000
012300                                                                  01300000
012400 1000-CONTROL.                                                    01310007
012500                                                                  01320000
013600      MOVE SPACES TO                                              01321007
013700                    INPUT-QUEUE-NAME                              01322007
013800                    QMGR-NAME                                     01323007
013900                    QUEUE-MESSAGE                                 01324007
014000                                                                  01325007
014100      INITIALIZE MQ-ERR-DISPLAY                                   01326007
014200                                                                  01327007
014600     PERFORM 2100-OPEN-ERROR-QUEUE                                01327107
015300******************************************************************01327207
015400* GET THE QUEUE NAME WHICH STARTED THE TRANSACTION               *01327307
015500******************************************************************01327407
015600     EXEC CICS RETRIEVE                                           01327507
015700       INTO(MQTM)                                                 01327607
015800       RESP(WS-CICS-RESP1-CD)                                     01327707
015900       RESP2(WS-CICS-RESP2-CD)                                    01327807
016000     END-EXEC                                                     01327907
016100     IF WS-CICS-RESP1-CD =  DFHRESP(NORMAL)                       01328007
016200       MOVE MQTM-QNAME  TO INPUT-QUEUE-NAME                       01328107
016300       MOVE 'CARD.DEMO.REPLY.ACCT' TO REPLY-QUEUE-NAME            01328207
016400     ELSE                                                         01328307
016500       MOVE 'CICS RETREIVE' TO MQ-ERROR-PARA                      01328407
016600       MOVE WS-CICS-RESP1-CD TO WS-CICS-RESP1-CD-D                01328507
016700       MOVE WS-CICS-RESP2-CD TO WS-CICS-RESP2-CD                  01328607
016800       STRING 'RESP: ', WS-CICS-RESP1-CD-D , WS-CICS-RESP2-CD-D,  01328707
016900              'END' DELIMITED BY SIZE                             01328807
017000              INTO MQ-APPL-RETURN-MESSAGE                         01328907
017100       END-STRING                                                 01329007
017200                                                                  01329107
             PERFORM 9000-ERROR                                         01329207
017400       PERFORM 8000-TERMINATION                                   01329307
017500     END-IF                                                       01329407
014500                                                                  01329507
014800     PERFORM 2300-OPEN-INPUT-QUEUE                                01329807
014900     PERFORM 2400-OPEN-OUTPUT-QUEUE                               01329907
012700     PERFORM 3000-GET-REQUEST                                     01340007
012800     PERFORM 4000-MAIN-PROCESS UNTIL                              01350007
012900             NO-MORE-MSGS                                         01360007
013000                                                                  01370000
013100     PERFORM 8000-TERMINATION.                                    01380007
013200                                                                  01390000
015000     .                                                            01570000
015100                                                                  01580000
017800 2300-OPEN-INPUT-QUEUE.                                           01850007
017900* OPEN-INPUT WILL OPEN A QUEUE FOR GET PROCESSING                 01860000
018000                                                                  01870000
018400                                                                  01910000
018500     MOVE SPACES           TO MQOD-OBJECTQMGRNAME                 01920007
018600     MOVE INPUT-QUEUE-NAME TO MQOD-OBJECTNAME                     01930007
018700                                                                  01940000
018800     COMPUTE MQ-OPTIONS = MQOO-INPUT-SHARED                       01950000
018900                        + MQOO-SAVE-ALL-CONTEXT                   01960000
019000                        + MQOO-FAIL-IF-QUIESCING                  01970007
019100                                                                  01980000
019200     CALL 'MQOPEN' USING QMGR-HANDLE-CONN                         01990000
019300                         MQ-OBJECT-DESCRIPTOR                     02000000
019400                         MQ-OPTIONS                               02010000
019500                         MQ-HOBJ                                  02020000
019600                         MQ-CONDITION-CODE                        02030000
019700                         MQ-REASON-CODE                           02040007
019800                                                                  02050000
019900     EVALUATE MQ-CONDITION-CODE                                   02060000
020000         WHEN MQCC-OK                                             02070000
020100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02080000
020200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02090000
020300              MOVE MQ-HOBJ           TO INPUT-QUEUE-HANDLE        02100000
020400              SET  REPLY-QUEUE-OPEN  TO TRUE                      02110007
020500         WHEN OTHER                                               02120000
020600              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02130000
020700              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02140000
020800              MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        02150000
020900              MOVE 'INP MQOPEN ERR'  TO MQ-APPL-RETURN-MESSAGE    02160000
021000              PERFORM 9000-ERROR                                  02170007
021100              PERFORM 8000-TERMINATION                            02180007
021200     END-EVALUATE.                                                02190000
021300                                                                  02200000
021400 2400-OPEN-OUTPUT-QUEUE.                                          02210007
021500                                                                  02220000
021600* OPEN-OUTPUT WILL OPEN A QUEUE FOR PUT PROCESSING                02230000
021700                                                                  02240000
022100                                                                  02280000
022200     MOVE SPACES            TO MQOD-OBJECTQMGRNAME                02290007
022300     MOVE REPLY-QUEUE-NAME  TO MQOD-OBJECTNAME                    02300007
022400                                                                  02310000
022500     COMPUTE MQ-OPTIONS = MQOO-OUTPUT                             02320000
022600                        + MQOO-PASS-ALL-CONTEXT                   02330000
022700                        + MQOO-FAIL-IF-QUIESCING                  02340007
022800                                                                  02350000
022900     CALL 'MQOPEN' USING QMGR-HANDLE-CONN                         02360000
023000                         MQ-OBJECT-DESCRIPTOR                     02370000
023100                         MQ-OPTIONS                               02380000
023200                         MQ-HOBJ                                  02390000
023300                         MQ-CONDITION-CODE                        02400000
023400                         MQ-REASON-CODE                           02410007
023500                                                                  02420000
023600     EVALUATE MQ-CONDITION-CODE                                   02430000
023700         WHEN MQCC-OK                                             02440000
023800              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02450000
023900              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02460000
024000              MOVE MQ-HOBJ           TO OUTPUT-QUEUE-HANDLE       02470000
024100              SET  RESP-QUEUE-OPEN   TO TRUE                      02480007
024200         WHEN OTHER                                               02490000
024300              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02500000
024400              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02510000
024500              MOVE REPLY-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        02520000
024600              MOVE 'OUT MQOPEN ERR'  TO MQ-APPL-RETURN-MESSAGE    02530000
024700              PERFORM 9000-ERROR                                  02540007
024800              PERFORM 8000-TERMINATION                            02550007
024900     END-EVALUATE.                                                02560000
025000                                                                  02570000
025100 2100-OPEN-ERROR-QUEUE.                                           02580007
025200                                                                  02590000
025300* OPEN-OUTPUT WILL OPEN A QUEUE FOR PUT PROCESSING                02600000
025400                                                                  02610000
025800                                                                  02650000
025900     MOVE 'CARD.DEMO.ERROR' TO ERROR-QUEUE-NAME                   02660000
026000     MOVE SPACES            TO MQOD-OBJECTQMGRNAME                02670007
026100     MOVE ERROR-QUEUE-NAME  TO MQOD-OBJECTNAME                    02680007
026200                                                                  02690000
026300     COMPUTE MQ-OPTIONS = MQOO-OUTPUT                             02700000
026400                        + MQOO-PASS-ALL-CONTEXT                   02710000
026500                        + MQOO-FAIL-IF-QUIESCING                  02720007
026600                                                                  02730000
026700     CALL 'MQOPEN' USING QMGR-HANDLE-CONN                         02740000
026800                         MQ-OBJECT-DESCRIPTOR                     02750000
026900                         MQ-OPTIONS                               02760000
027000                         MQ-HOBJ                                  02770000
027100                         MQ-CONDITION-CODE                        02780000
027200                         MQ-REASON-CODE                           02790007
027300                                                                  02800000
027400     EVALUATE MQ-CONDITION-CODE                                   02810000
027500         WHEN MQCC-OK                                             02820000
027600              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02830000
027700              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02840000
027800              MOVE MQ-HOBJ           TO ERROR-QUEUE-HANDLE        02850000
027900              SET  ERR-QUEUE-OPEN   TO TRUE                       02860007
028000         WHEN OTHER                                               02870000
028100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02880000
028200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02890000
028300              MOVE ERROR-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        02900000
028400              MOVE 'ERR MQOPEN ERR'  TO MQ-APPL-RETURN-MESSAGE    02910000
028500              DISPLAY MQ-ERR-DISPLAY                              02920000
028600              PERFORM 8000-TERMINATION                            02930007
028700     END-EVALUATE.                                                02940000
028800                                                                  02950000
028900                                                                  02960000
029000 4000-MAIN-PROCESS.                                               02970007
029100     EXEC CICS                                                    02980000
029200          SYNCPOINT                                               02990000
029300     END-EXEC                                                     03000000
029400                                                                  03010000
029500     PERFORM 3000-GET-REQUEST                                     03020007
029600     .                                                            03030000
029700                                                                  03040000
029800                                                                  03050000
029900 3000-GET-REQUEST.                                                03060007
030000* GET WILL GET A MESSAGE FROM THE QUEUE                           03070012
030700*** ADDED 5000 MS (5 SECS) AS THE WAIT INTERVAL FOR GET           03140000
030800     MOVE 5000                            TO MQGMO-WAITINTERVAL   03150000
030900     MOVE SPACES                          TO MQ-CORRELID          03160000
031000     MOVE SPACES                          TO MQ-MSG-ID            03170000
031100     MOVE INPUT-QUEUE-NAME                TO MQ-QUEUE             03180000
031200     MOVE INPUT-QUEUE-HANDLE              TO MQ-HOBJ              03190000
031300     MOVE 1000                            TO MQ-BUFFER-LENGTH     03200000
031400     MOVE MQMI-NONE         TO MQMD-MSGID                         03210000
031500     MOVE MQCI-NONE         TO MQMD-CORRELID                      03220000
031500     INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES     03221000
031600                                                                  03230000
031700     COMPUTE MQGMO-OPTIONS = MQGMO-SYNCPOINT                      03240000
031800                           + MQGMO-FAIL-IF-QUIESCING              03250000
031900                           + MQGMO-CONVERT                        03260000
032000                           + MQGMO-WAIT                           03270000
032100                                                                  03280000
032200     CALL 'MQGET'  USING MQ-HCONN                                 03290000
032300                         MQ-HOBJ                                  03300000
032400                         MQ-MESSAGE-DESCRIPTOR                    03310000
032500                         MQ-GET-MESSAGE-OPTIONS                   03320000
032600                         MQ-BUFFER-LENGTH                         03330000
032700                         MQ-BUFFER                                03340000
032800                         MQ-DATA-LENGTH                           03350000
032900                         MQ-CONDITION-CODE                        03360000
033000                         MQ-REASON-CODE                           03370007
033100                                                                  03380000
033200                                                                  03390000
033300     IF MQ-CONDITION-CODE = MQCC-OK                               03400000
033400        MOVE MQMD-MSGID        TO MQ-MSG-ID                       03410000
033500        MOVE MQMD-CORRELID     TO MQ-CORRELID                     03420000
033600        MOVE MQMD-REPLYTOQ     TO MQ-QUEUE-REPLY                  03430000
033700        MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE          03440000
033800        MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE             03450000
033900        MOVE MQ-BUFFER         TO REQUEST-MESSAGE                 03460000
034000        MOVE MQ-CORRELID       TO SAVE-CORELID                    03470000
034100        MOVE MQ-QUEUE-REPLY    TO SAVE-REPLY2Q                    03480000
034200        MOVE MQ-MSG-ID         TO SAVE-MSGID                      03490000
034300        MOVE REQUEST-MESSAGE   TO REQUEST-MSG-COPY                03500000
034400        PERFORM 4000-PROCESS-REQUEST-REPLY                        03510010
034500        ADD  1                 TO MQ-MSG-COUNT                    03520000
034600     ELSE                                                         03530000
034700        IF MQ-REASON-CODE  =  MQRC-NO-MSG-AVAILABLE               03540011
034800          SET NO-MORE-MSGS             TO  TRUE                   03550007
034900                                                                  03560000
035000        ELSE                                                      03570000
035100           MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE       03580000
035200           MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE          03590000
035300           MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME           03600000
035400           MOVE 'INP MQGET ERR:'  TO MQ-APPL-RETURN-MESSAGE       03610000
035500           PERFORM 9000-ERROR                                     03620007
035600           PERFORM 8000-TERMINATION                               03630007
035700       END-IF                                                     03640000
035800     END-IF.                                                      03650000
035900                                                                  03660000
036000 4000-PROCESS-REQUEST-REPLY.                                      03670010
036100     MOVE SPACES TO REPLY-MESSAGE                                 03680000
036100     INITIALIZE WS-DATE-TIME REPLACING NUMERIC BY ZEROES          03690000
036100     IF WS-FUNC = 'INQA' AND WS-KEY > ZEROES                      03700000
              MOVE WS-KEY       TO  WS-CARD-RID-ACCT-ID                 03700106
                                                                        03700206
           EXEC CICS READ                                               03700306
                DATASET   (LIT-ACCTFILENAME)                            03700406
                RIDFLD    (WS-CARD-RID-ACCT-ID-X)                       03700506
                KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)             03700606
                INTO      (ACCOUNT-RECORD)                              03700706
                LENGTH    (LENGTH OF ACCOUNT-RECORD)                    03700806
                RESP      (WS-RESP-CD)                                  03700906
                RESP2     (WS-REAS-CD)                                  03701006
           END-EXEC                                                     03701106
                                                                        03701206
           EVALUATE WS-RESP-CD                                          03701306
               WHEN DFHRESP(NORMAL)                                     03701406
                    MOVE ACCT-ID          TO WS-ACCT-ID                 03701510
                    MOVE ACCT-ACTIVE-STATUS                             03701610
                                          TO WS-ACCT-ACTIVE-STATUS      03701710
                    MOVE ACCT-CURR-BAL    TO WS-ACCT-CURR-BAL           03701810
                    MOVE ACCT-CREDIT-LIMIT                              03701910
                                          TO WS-ACCT-CREDIT-LIMIT       03702110
                    MOVE ACCT-CASH-CREDIT-LIMIT                         03702210
                                          TO WS-ACCT-CASH-CREDIT-LIMIT  03702310
                    MOVE ACCT-OPEN-DATE   TO WS-ACCT-OPEN-DATE          03702410
                    MOVE ACCT-EXPIRAION-DATE                            03702510
                                          TO WS-ACCT-EXPIRAION-DATE     03702610
                    MOVE ACCT-REISSUE-DATE                              03702710
                                          TO WS-ACCT-REISSUE-DATE       03702810
                    MOVE ACCT-CURR-CYC-CREDIT                           03702910
                                          TO WS-ACCT-CURR-CYC-CREDIT    03703010
                    MOVE ACCT-CURR-CYC-DEBIT                            03703110
                                          TO WS-ACCT-CURR-CYC-DEBIT     03703210
                    MOVE ACCT-GROUP-ID    TO WS-ACCT-GROUP-ID           03703310
                    MOVE WS-ACCT-RESPONSE TO REPLY-MESSAGE              03703510
                    PERFORM 4100-PUT-REPLY                              03703610
               WHEN DFHRESP(NOTFND)                                     03703710
                    STRING 'INVALID REQUEST PARAMETERS '                03703810
                           'ACCT ID : 'WS-KEY                           03703910
                           DELIMITED BY SIZE                            03704010
                           INTO                                         03704110
                           REPLY-MESSAGE                                03704210
                    END-STRING                                          03704310
                    PERFORM 4100-PUT-REPLY                              03704410
      *                                                                 03704510
               WHEN OTHER                                               03704610
017200                                                                  03704800
035100           MOVE WS-RESP-CD        TO MQ-APPL-CONDITION-CODE       03704903
035200           MOVE WS-REAS-CD        TO MQ-APPL-REASON-CODE          03705003
035300           MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME           03705103
035400           MOVE 'ERROR WHILE READING ACCTFILE'                    03705204
035400                                  TO MQ-APPL-RETURN-MESSAGE       03705303
                  PERFORM 9000-ERROR                                    03705407
017400            PERFORM 8000-TERMINATION                              03705507
      *           PERFORM SEND-LONG-TEXT                                03705603
           END-EVALUATE                                                 03705703
           ELSE                                                         03705805
                    STRING 'INVALID REQUEST PARAMETERS '                03705905
                           'ACCT ID : 'WS-KEY                           03706005
                           'FUNCTION : 'WS-FUNC                         03706105
                           DELIMITED BY SIZE                            03706205
                           INTO                                         03706305
                           REPLY-MESSAGE                                03706405
                    END-STRING                                          03706505
                    PERFORM 4100-PUT-REPLY                              03706610
036100     END-IF                                                       03706705
036100                                                                  03707005
036100                                                                  03780000
036800     .                                                            03860000
036900                                                                  03870000
037000 4100-PUT-REPLY.                                                  03880010
037100                                                                  03890000
037200* PUT WILL PUT A MESSAGE ON THE QUEUE AND CONVERT IT TO A STRING  03900000
037300                                                                  03910000
037600                                                                  03940000
037700     MOVE REPLY-MESSAGE                TO MQ-BUFFER               03950007
037800     MOVE 1000                         TO MQ-BUFFER-LENGTH        03960007
037900     MOVE SAVE-MSGID                   TO MQMD-MSGID              03970007
038000     MOVE SAVE-CORELID                 TO MQMD-CORRELID           03980007
038100     MOVE MQFMT-STRING                 TO MQMD-FORMAT             03990007
038200                                                                  04000000
038300     COMPUTE MQMD-CODEDCHARSETID      =  MQCCSI-Q-MGR             04010007
038400                                                                  04020000
038500     COMPUTE MQPMO-OPTIONS = MQPMO-SYNCPOINT                      04030000
038600                           + MQPMO-DEFAULT-CONTEXT                04040000
038700                           + MQPMO-FAIL-IF-QUIESCING              04050007
038800                                                                  04060000
038900     CALL 'MQPUT'  USING MQ-HCONN                                 04070000
039000                         OUTPUT-QUEUE-HANDLE                      04080000
039100                         MQ-MESSAGE-DESCRIPTOR                    04090000
039200                         MQ-PUT-MESSAGE-OPTIONS                   04100000
039300                         MQ-BUFFER-LENGTH                         04110000
039400                         MQ-BUFFER                                04120000
039500                         MQ-CONDITION-CODE                        04130000
039600                         MQ-REASON-CODE                           04140007
039700                                                                  04150000
039800     EVALUATE MQ-CONDITION-CODE                                   04160000
039900         WHEN MQCC-OK                                             04170000
040000              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04180000
040100              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04190000
040200         WHEN OTHER                                               04200000
040300              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04210000
040400              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04220000
040500              MOVE REPLY-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        04230000
040600              MOVE 'MQPUT ERR'       TO MQ-APPL-RETURN-MESSAGE    04240000
040700              PERFORM 9000-ERROR                                  04250007
040800              PERFORM 8000-TERMINATION                            04260007
040900     END-EVALUATE.                                                04270000
041000                                                                  04280000
041100 9000-ERROR.                                                      04290007
041200* PUT WILL PUT A MESSAGE ON THE QUEUE AND CONVERT IT TO A STRING  04300000
041300                                                                  04310000
041600                                                                  04340000
041700     MOVE MQ-ERR-DISPLAY               TO ERROR-MESSAGE,          04350000
041800     MOVE ERROR-MESSAGE                TO MQ-BUFFER               04360007
041900     MOVE 1000                         TO MQ-BUFFER-LENGTH        04370007
042200     MOVE MQFMT-STRING                 TO MQMD-FORMAT             04400007
042300                                                                  04410000
042400     COMPUTE MQMD-CODEDCHARSETID      =  MQCCSI-Q-MGR             04420007
042500                                                                  04430000
042600     COMPUTE MQPMO-OPTIONS = MQPMO-SYNCPOINT                      04440000
042700                           + MQPMO-DEFAULT-CONTEXT                04450000
042800                           + MQPMO-FAIL-IF-QUIESCING              04460007
042900                                                                  04470000
043000     CALL 'MQPUT'  USING MQ-HCONN                                 04480000
043100                         ERROR-QUEUE-HANDLE                       04490000
043200                         MQ-MESSAGE-DESCRIPTOR                    04500000
043300                         MQ-PUT-MESSAGE-OPTIONS                   04510000
043400                         MQ-BUFFER-LENGTH                         04520000
043500                         MQ-BUFFER                                04530000
043600                         MQ-CONDITION-CODE                        04540000
043700                         MQ-REASON-CODE                           04550007
043800                                                                  04560000
043900     EVALUATE MQ-CONDITION-CODE                                   04570000
044000         WHEN MQCC-OK                                             04580000
044100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04590000
044200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04600000
044300         WHEN OTHER                                               04610000
044400              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04620000
044500              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04630000
044600              MOVE ERROR-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        04640000
044700              MOVE 'MQPUT ERR'       TO MQ-APPL-RETURN-MESSAGE    04650000
044800              DISPLAY MQ-ERR-DISPLAY                              04660000
044900              PERFORM 8000-TERMINATION                            04670007
045000     END-EVALUATE.                                                04680000
045100     .                                                            04690000
045200 8000-TERMINATION.                                                04700007
045300                                                                  04710000
045400     IF REPLY-QUEUE-OPEN                                          04720007
045500        PERFORM 5000-CLOSE-INPUT-QUEUE                            04730010
045600     END-IF                                                       04740000
045700     IF RESP-QUEUE-OPEN                                           04750007
045800        PERFORM 5100-CLOSE-OUTPUT-QUEUE                           04760010
045900     END-IF                                                       04770000
046000     IF ERR-QUEUE-OPEN                                            04780007
046100        PERFORM 5200-CLOSE-ERROR-QUEUE                            04790010
046200     END-IF                                                       04800000
046300     EXEC CICS RETURN END-EXEC                                    04810000
046400     GOBACK.                                                      04820000
046500                                                                  04830000
046600 5000-CLOSE-INPUT-QUEUE.                                          04840010
046700     MOVE INPUT-QUEUE-NAME           TO MQ-QUEUE                  04850000
046800     MOVE INPUT-QUEUE-HANDLE         TO MQ-HOBJ                   04860000
046900     COMPUTE MQ-OPTIONS = MQCO-NONE                               04870007
047000                                                                  04880000
047100     CALL 'MQCLOSE' USING MQ-HCONN                                04890000
047200                          MQ-HOBJ                                 04900000
047300                          MQ-OPTIONS                              04910000
047400                          MQ-CONDITION-CODE                       04920000
047500                          MQ-REASON-CODE                          04930007
047600                                                                  04940000
047700     EVALUATE MQ-CONDITION-CODE                                   04950000
047800         WHEN MQCC-OK                                             04960000
047900              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04970000
048000              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04980000
048100         WHEN OTHER                                               04990000
048200              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05000000
048300              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05010000
048400              MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        05020000
048500              MOVE 'MQCLOSE ERR'     TO MQ-APPL-RETURN-MESSAGE    05030000
048600              PERFORM 8000-TERMINATION                            05040007
048700     END-EVALUATE.                                                05050000
048800 5100-CLOSE-OUTPUT-QUEUE.                                         05060010
048900     MOVE REPLY-QUEUE-NAME            TO MQ-QUEUE                 05070000
049000     MOVE OUTPUT-QUEUE-HANDLE         TO MQ-HOBJ                  05080000
049100     COMPUTE MQ-OPTIONS = MQCO-NONE                               05090007
049200                                                                  05100000
049300     CALL 'MQCLOSE' USING MQ-HCONN                                05110000
049400                          MQ-HOBJ                                 05120000
049500                          MQ-OPTIONS                              05130000
049600                          MQ-CONDITION-CODE                       05140000
049700                          MQ-REASON-CODE                          05150007
049800                                                                  05160000
049900     EVALUATE MQ-CONDITION-CODE                                   05170000
050000         WHEN MQCC-OK                                             05180000
050100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05190000
050200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05200000
050300         WHEN OTHER                                               05210000
050400              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05220000
050500              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05230000
050600              MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        05240000
050700              MOVE 'MQCLOSE ERR'     TO MQ-APPL-RETURN-MESSAGE    05250000
050800              PERFORM 8000-TERMINATION                            05260007
050900     END-EVALUATE.                                                05270000
051000                                                                  05280000
051100 5200-CLOSE-ERROR-QUEUE.                                          05290010
051200     MOVE ERROR-QUEUE-NAME          TO MQ-QUEUE                   05300000
051300     MOVE ERROR-QUEUE-HANDLE         TO MQ-HOBJ                   05310000
051400     COMPUTE MQ-OPTIONS = MQCO-NONE                               05320007
051500                                                                  05330000
051600     CALL 'MQCLOSE' USING MQ-HCONN                                05340000
051700                          MQ-HOBJ                                 05350000
051800                          MQ-OPTIONS                              05360000
051900                          MQ-CONDITION-CODE                       05370000
052000                          MQ-REASON-CODE                          05380007
052100                                                                  05390000
052200     EVALUATE MQ-CONDITION-CODE                                   05400000
052300         WHEN MQCC-OK                                             05410000
052400              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05420000
052500              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05430000
052600         WHEN OTHER                                               05440000
052700              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05450000
052800              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05460000
052900              MOVE ERROR-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        05470000
053000              MOVE 'MQCLOSE ERR'     TO MQ-APPL-RETURN-MESSAGE    05480000
053100              PERFORM 9000-ERROR                                  05490007
053200              PERFORM 8000-TERMINATION                            05500007
053300     END-EVALUATE.                                                05510000
053400                                                                  05520000
