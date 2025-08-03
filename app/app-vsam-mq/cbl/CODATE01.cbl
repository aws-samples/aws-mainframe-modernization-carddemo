000100 IDENTIFICATION DIVISION.                                         00010012
000200 PROGRAM-ID.           CODATE01 IS INITIAL.                       00020012
000300 AUTHOR.               AWS.                                       00030012
000400 DATE-WRITTEN.         03/21.                                     00040012
000500 DATE-COMPILED.                                                   00050012
000600                                                                  00060012
000700 ENVIRONMENT DIVISION.                                            00070012
000800                                                                  00080012
000900 DATA DIVISION.                                                   00090012
001000                                                                  00100012
001100 WORKING-STORAGE SECTION.                                         00110012
001700                                                                  00120012
001800 01 WS-MQ-MSG-FLAG                PIC X(01) VALUE 'N'.            00130012
001900    88  NO-MORE-MSGS              VALUE 'Y'.                      00140012
002000                                                                  00150012
002100 01 WS-RESP-QUEUE-STS            PIC X(01) VALUE 'N'.             00160012
002200    88  RESP-QUEUE-OPEN          VALUE 'Y'.                       00170012
002300                                                                  00180012
002400 01 WS-ERR-QUEUE-STS             PIC X(01) VALUE 'N'.             00190012
002500    88  ERR-QUEUE-OPEN          VALUE 'Y'.                        00200012
002600                                                                  00210012
002700 01 WS-REPLY-QUEUE-STS           PIC X(01) VALUE 'N'.             00220012
002800    88  REPLY-QUEUE-OPEN         VALUE 'Y'.                       00230012
002900                                                                  00240012
003700                                                                  00250012
003800 01 WS-CICS-RESP-CDS.                                             00260012
003900    05  WS-CICS-RESP1-CD        PIC S9(08) COMP VALUE ZERO.       00270012
004000    05  WS-CICS-RESP2-CD        PIC S9(08) COMP VALUE ZERO.       00280012
004300    05  WS-CICS-RESP1-CD-D      PIC 9(08) VALUE ZERO.             00290012
004400    05  WS-CICS-RESP2-CD-D      PIC 9(08) VALUE ZERO.             00300012
004500                                                                  00310012
004600***********************************************                   00320012
004700**             DATE FIELDS                   **                   00330012
004800***********************************************                   00340012
004900 01 WS-DATE-TIME.                                                 00350012
005000    10 WS-ABS-TIME                  PIC S9(15) COMP-3 VALUE ZERO. 00360012
005100    10 WS-MMDDYYYY                  PIC X(10) VALUE SPACES.       00370012
005200    10 WS-TIME                      PIC X(8)  VALUE SPACES.       00380012
004600***********************************************                   00390012
004700**             MQ FIELDS                     **                   00400012
004800***********************************************                   00410012
005000 01 MQ-QUEUE                        PIC X(48).                    00420012
005100 01 MQ-QUEUE-REPLY                  PIC X(48).                    00430012
005200 01 MQ-HCONN                        PIC S9(09) BINARY VALUE 0.    00440012
005300 01 MQ-CONDITION-CODE               PIC S9(09) BINARY VALUE 0.    00450012
005400 01 MQ-REASON-CODE                  PIC S9(09) BINARY VALUE 0.    00460012
005500 01 MQ-HOBJ                         PIC S9(09) BINARY VALUE 0.    00470012
005600 01 MQ-OPTIONS                      PIC S9(09) BINARY VALUE 0.    00480012
005700 01 MQ-BUFFER-LENGTH                PIC S9(09) BINARY.            00490012
005800 01 MQ-BUFFER                       PIC X(1000).                  00500012
005900 01 MQ-DATA-LENGTH                  PIC S9(09) BINARY.            00510012
006000 01 MQ-CORRELID                     PIC X(24).                    00520012
006100 01 MQ-MSG-ID                       PIC X(24).                    00530012
006200 01 MQ-MSG-COUNT                    PIC 9(09).                    00540012
006300 01 SAVE-CORELID                    PIC X(24).                    00550012
006400 01 SAVE-MSGID                      PIC X(24).                    00560012
006500 01 SAVE-REPLY2Q                    PIC X(48).                    00570012
006600 01 MQ-ERR-DISPLAY.                                               00580012
006700     05 MQ-ERROR-PARA                   PIC X(25) .               00590012
006800     05 FILLER                          PIC X(02) VALUE SPACES.   00600012
006900     05 MQ-APPL-RETURN-MESSAGE          PIC X(25).                00610012
007000     05 FILLER                          PIC X(02) VALUE SPACES.   00620012
007100     05 MQ-APPL-CONDITION-CODE          PIC 9(02).                00630012
007200     05 FILLER                          PIC X(02) VALUE SPACES.   00640012
007300     05 MQ-APPL-REASON-CODE             PIC 9(05).                00650012
007400     05 FILLER                          PIC X(02) VALUE SPACES.   00660012
007500     05 MQ-APPL-QUEUE-NAME              PIC X(48).                00670012
007600                                                                  00680012
007700                                                                  00690012
007800 01 MQ-GET-MESSAGE-OPTIONS.                                       00700012
007900 COPY CMQGMOV.                                                    00710012
008000                                                                  00720012
008100                                                                  00730012
008200 01 MQ-PUT-MESSAGE-OPTIONS.                                       00740012
008300 COPY CMQPMOV.                                                    00750012
008400                                                                  00760012
008500                                                                  00770012
008600 01 MQ-MESSAGE-DESCRIPTOR.                                        00780012
008700 COPY CMQMDV.                                                     00790012
008800                                                                  00800012
008900                                                                  00810012
009000 01 MQ-OBJECT-DESCRIPTOR.                                         00820012
009100 COPY CMQODV.                                                     00830012
009200                                                                  00840012
009300                                                                  00850012
009400 01 MQ-CONSTANTS.                                                 00860012
009500 COPY CMQV.                                                       00870012
009600                                                                  00880012
009700 01 MQ-GET-QUEUE-MESSAGE.                                         00890012
009800 COPY CMQTML.                                                     00900012
009900                                                                  00910012
010000 01  QUEUE-INFO.                                                  00920012
010100     05 QMGR-NAME                   PIC X(48) VALUE SPACES.       00930012
010200     05 INPUT-QUEUE-NAME            PIC X(48) VALUE SPACES.       00940012
010300     05 REPLY-QUEUE-NAME            PIC X(48) VALUE SPACES.       00950012
010400     05 ERROR-QUEUE-NAME            PIC X(48) VALUE SPACES.       00960012
010500                                                                  00970012
010600 01 INPUT-QUEUE-HANDLE              PIC S9(09) BINARY VALUE 0.    00980012
010700                                                                  00990012
010800 01 OUTPUT-QUEUE-HANDLE             PIC S9(09) BINARY VALUE 0.    01000012
010900                                                                  01010012
011000 01 ERROR-QUEUE-HANDLE              PIC S9(09) BINARY VALUE 0.    01020012
011100                                                                  01030012
011200 01 QMGR-HANDLE-CONN                PIC S9(09) BINARY VALUE 0.    01040012
011300 01 QUEUE-MESSAGE                   PIC X(1000).                  01050012
011400 01 REQUEST-MESSAGE                 PIC X(1000).                  01060012
011500 01 REPLY-MESSAGE                   PIC X(1000).                  01070012
011600 01 ERROR-MESSAGE                   PIC X(1000).                  01080012
011700 01 REQUEST-MSG-COPY.                                             01090012
011700    10 WS-FUNC                      PIC X(04) VALUE SPACES.       01100012
011700    10 WS-KEY                       PIC 9(11) VALUE ZEROES.       01110012
011700    10 WS-FILLER                    PIC X(985) VALUE SPACES.      01120012
011800                                                                  01130012
       01 WS-VARIABLES.                                                 01140012
          05 LIT-ACCTFILENAME                      PIC X(8)             01150012
                                                   VALUE 'ACCTDAT '.    01160012
          05 WS-RESP-CD                          PIC S9(09) COMP        01170012
                                                   VALUE ZEROS.         01180012
          05 WS-REAS-CD                          PIC S9(09) COMP        01190012
                                                   VALUE ZEROS.         01200012
                                                                        01210012
011900                                                                  01220012
012000 LINKAGE SECTION.                                                 01230012
012100                                                                  01240012
012200 PROCEDURE DIVISION.                                              01250012
012300                                                                  01260012
012400 1000-CONTROL.                                                    01270012
012500                                                                  01280012
013600      MOVE SPACES TO                                              01290012
013700                    INPUT-QUEUE-NAME                              01300012
013800                    QMGR-NAME                                     01310012
013900                    QUEUE-MESSAGE                                 01320012
014000                                                                  01330012
014100      INITIALIZE MQ-ERR-DISPLAY                                   01340012
014200                                                                  01350012
014600     PERFORM 2100-OPEN-ERROR-QUEUE                                01360012
015300******************************************************************01370012
015400* GET THE QUEUE NAME WHICH STARTED THE TRANSACTION               *01380012
015500******************************************************************01390012
015600     EXEC CICS RETRIEVE                                           01400012
015700       INTO(MQTM)                                                 01410012
015800       RESP(WS-CICS-RESP1-CD)                                     01420012
015900       RESP2(WS-CICS-RESP2-CD)                                    01430012
016000     END-EXEC                                                     01440012
016100     IF WS-CICS-RESP1-CD =  DFHRESP(NORMAL)                       01450012
016200       MOVE MQTM-QNAME  TO INPUT-QUEUE-NAME                       01460012
016300       MOVE 'CARD.DEMO.REPLY.DATE' TO REPLY-QUEUE-NAME            01470012
016400     ELSE                                                         01480012
016500       MOVE 'CICS RETRIEVE' TO MQ-ERROR-PARA                      01490012
016600       MOVE WS-CICS-RESP1-CD TO WS-CICS-RESP1-CD-D                01500012
016700       MOVE WS-CICS-RESP2-CD TO WS-CICS-RESP2-CD                  01510012
016800       STRING 'RESP: ', WS-CICS-RESP1-CD-D , WS-CICS-RESP2-CD-D,  01520012
016900              'END' DELIMITED BY SIZE                             01530012
017000              INTO MQ-APPL-RETURN-MESSAGE                         01540012
017100       END-STRING                                                 01550012
017200                                                                  01560012
             PERFORM 9000-ERROR                                         01570012
017400       PERFORM 8000-TERMINATION                                   01580012
017500     END-IF                                                       01590012
014500                                                                  01600012
014800     PERFORM 2300-OPEN-INPUT-QUEUE                                01610012
014900     PERFORM 2400-OPEN-OUTPUT-QUEUE                               01620012
012700     PERFORM 3000-GET-REQUEST                                     01630012
012800     PERFORM 4000-MAIN-PROCESS UNTIL                              01640012
012900             NO-MORE-MSGS                                         01650012
013000                                                                  01660012
013100     PERFORM 8000-TERMINATION.                                    01670012
013200                                                                  01680012
015000     .                                                            01690012
015100                                                                  01700012
017800 2300-OPEN-INPUT-QUEUE.                                           01710012
017900* OPEN-INPUT WILL OPEN A QUEUE FOR GET PROCESSING                 01720012
018000                                                                  01730012
018400                                                                  01740012
018500     MOVE SPACES           TO MQOD-OBJECTQMGRNAME                 01750012
018600     MOVE INPUT-QUEUE-NAME TO MQOD-OBJECTNAME                     01760012
018700                                                                  01770012
018800     COMPUTE MQ-OPTIONS = MQOO-INPUT-SHARED                       01780012
018900                        + MQOO-SAVE-ALL-CONTEXT                   01790012
019000                        + MQOO-FAIL-IF-QUIESCING                  01800012
019100                                                                  01810012
019200     CALL 'MQOPEN' USING QMGR-HANDLE-CONN                         01820012
019300                         MQ-OBJECT-DESCRIPTOR                     01830012
019400                         MQ-OPTIONS                               01840012
019500                         MQ-HOBJ                                  01850012
019600                         MQ-CONDITION-CODE                        01860012
019700                         MQ-REASON-CODE                           01870012
019800                                                                  01880012
019900     EVALUATE MQ-CONDITION-CODE                                   01890012
020000         WHEN MQCC-OK                                             01900012
020100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    01910012
020200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       01920012
020300              MOVE MQ-HOBJ           TO INPUT-QUEUE-HANDLE        01930012
020400              SET  REPLY-QUEUE-OPEN  TO TRUE                      01940012
020500         WHEN OTHER                                               01950012
020600              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    01960012
020700              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       01970012
020800              MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        01980012
020900              MOVE 'INP MQOPEN ERR'  TO MQ-APPL-RETURN-MESSAGE    01990012
021000              PERFORM 9000-ERROR                                  02000012
021100              PERFORM 8000-TERMINATION                            02010012
021200     END-EVALUATE.                                                02020012
021300                                                                  02030012
021400 2400-OPEN-OUTPUT-QUEUE.                                          02040012
021500                                                                  02050012
021600* OPEN-OUTPUT WILL OPEN A QUEUE FOR PUT PROCESSING                02060012
021700                                                                  02070012
022100                                                                  02080012
022200     MOVE SPACES            TO MQOD-OBJECTQMGRNAME                02090012
022300     MOVE REPLY-QUEUE-NAME  TO MQOD-OBJECTNAME                    02100012
022400                                                                  02110012
022500     COMPUTE MQ-OPTIONS = MQOO-OUTPUT                             02120012
022600                        + MQOO-PASS-ALL-CONTEXT                   02130012
022700                        + MQOO-FAIL-IF-QUIESCING                  02140012
022800                                                                  02150012
022900     CALL 'MQOPEN' USING QMGR-HANDLE-CONN                         02160012
023000                         MQ-OBJECT-DESCRIPTOR                     02170012
023100                         MQ-OPTIONS                               02180012
023200                         MQ-HOBJ                                  02190012
023300                         MQ-CONDITION-CODE                        02200012
023400                         MQ-REASON-CODE                           02210012
023500                                                                  02220012
023600     EVALUATE MQ-CONDITION-CODE                                   02230012
023700         WHEN MQCC-OK                                             02240012
023800              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02250012
023900              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02260012
024000              MOVE MQ-HOBJ           TO OUTPUT-QUEUE-HANDLE       02270012
024100              SET  RESP-QUEUE-OPEN   TO TRUE                      02280012
024200         WHEN OTHER                                               02290012
024300              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02300012
024400              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02310012
024500              MOVE REPLY-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        02320012
024600              MOVE 'OUT MQOPEN ERR'  TO MQ-APPL-RETURN-MESSAGE    02330012
024700              PERFORM 9000-ERROR                                  02340012
024800              PERFORM 8000-TERMINATION                            02350012
024900     END-EVALUATE.                                                02360012
025000                                                                  02370012
025100 2100-OPEN-ERROR-QUEUE.                                           02380012
025200                                                                  02390012
025300* OPEN-OUTPUT WILL OPEN A QUEUE FOR PUT PROCESSING                02400012
025400                                                                  02410012
025800                                                                  02420012
025900     MOVE 'CARD.DEMO.ERROR' TO ERROR-QUEUE-NAME                   02430012
026000     MOVE SPACES            TO MQOD-OBJECTQMGRNAME                02440012
026100     MOVE ERROR-QUEUE-NAME  TO MQOD-OBJECTNAME                    02450012
026200                                                                  02460012
026300     COMPUTE MQ-OPTIONS = MQOO-OUTPUT                             02470012
026400                        + MQOO-PASS-ALL-CONTEXT                   02480012
026500                        + MQOO-FAIL-IF-QUIESCING                  02490012
026600                                                                  02500012
026700     CALL 'MQOPEN' USING QMGR-HANDLE-CONN                         02510012
026800                         MQ-OBJECT-DESCRIPTOR                     02520012
026900                         MQ-OPTIONS                               02530012
027000                         MQ-HOBJ                                  02540012
027100                         MQ-CONDITION-CODE                        02550012
027200                         MQ-REASON-CODE                           02560012
027300                                                                  02570012
027400     EVALUATE MQ-CONDITION-CODE                                   02580012
027500         WHEN MQCC-OK                                             02590012
027600              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02600012
027700              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02610012
027800              MOVE MQ-HOBJ           TO ERROR-QUEUE-HANDLE        02620012
027900              SET  ERR-QUEUE-OPEN   TO TRUE                       02630012
028000         WHEN OTHER                                               02640012
028100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    02650012
028200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       02660012
028300              MOVE ERROR-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        02670012
028400              MOVE 'ERR MQOPEN ERR'  TO MQ-APPL-RETURN-MESSAGE    02680012
028500              DISPLAY MQ-ERR-DISPLAY                              02690012
028600              PERFORM 8000-TERMINATION                            02700012
028700     END-EVALUATE.                                                02710012
028800                                                                  02720012
028900                                                                  02730012
029000 4000-MAIN-PROCESS.                                               02740012
029100     EXEC CICS                                                    02750012
029200          SYNCPOINT                                               02760012
029300     END-EXEC                                                     02770012
029400                                                                  02780012
029500     PERFORM 3000-GET-REQUEST                                     02790012
029600     .                                                            02800012
029700                                                                  02810012
029800                                                                  02820012
029900 3000-GET-REQUEST.                                                02830012
030000* GET WILL GET A MESSAGE FROM THE QUEUE                           02840012
030700*** ADDED 5000 MS (5 SECS) AS THE WAIT INTERVAL FOR GET           02850012
030800     MOVE 5000                            TO MQGMO-WAITINTERVAL   02860012
030900     MOVE SPACES                          TO MQ-CORRELID          02870012
031000     MOVE SPACES                          TO MQ-MSG-ID            02880012
031100     MOVE INPUT-QUEUE-NAME                TO MQ-QUEUE             02890012
031200     MOVE INPUT-QUEUE-HANDLE              TO MQ-HOBJ              02900012
031300     MOVE 1000                            TO MQ-BUFFER-LENGTH     02910012
031400     MOVE MQMI-NONE         TO MQMD-MSGID                         02920012
031500     MOVE MQCI-NONE         TO MQMD-CORRELID                      02930012
031500     INITIALIZE REQUEST-MSG-COPY  REPLACING NUMERIC BY ZEROES     02940012
031600                                                                  02950012
031700     COMPUTE MQGMO-OPTIONS = MQGMO-SYNCPOINT                      02960012
031800                           + MQGMO-FAIL-IF-QUIESCING              02970012
031900                           + MQGMO-CONVERT                        02980012
032000                           + MQGMO-WAIT                           02990012
032100                                                                  03000012
032200     CALL 'MQGET'  USING MQ-HCONN                                 03010012
032300                         MQ-HOBJ                                  03020012
032400                         MQ-MESSAGE-DESCRIPTOR                    03030012
032500                         MQ-GET-MESSAGE-OPTIONS                   03040012
032600                         MQ-BUFFER-LENGTH                         03050012
032700                         MQ-BUFFER                                03060012
032800                         MQ-DATA-LENGTH                           03070012
032900                         MQ-CONDITION-CODE                        03080012
033000                         MQ-REASON-CODE                           03090012
033100                                                                  03100012
033200                                                                  03110012
033300     IF MQ-CONDITION-CODE = MQCC-OK                               03120012
033400        MOVE MQMD-MSGID        TO MQ-MSG-ID                       03130012
033500        MOVE MQMD-CORRELID     TO MQ-CORRELID                     03140012
033600        MOVE MQMD-REPLYTOQ     TO MQ-QUEUE-REPLY                  03150012
033700        MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE          03160012
033800        MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE             03170012
033900        MOVE MQ-BUFFER         TO REQUEST-MESSAGE                 03180012
034000        MOVE MQ-CORRELID       TO SAVE-CORELID                    03190012
034100        MOVE MQ-QUEUE-REPLY    TO SAVE-REPLY2Q                    03200012
034200        MOVE MQ-MSG-ID         TO SAVE-MSGID                      03210012
034300        MOVE REQUEST-MESSAGE   TO REQUEST-MSG-COPY                03220012
034400        PERFORM 4000-PROCESS-REQUEST-REPLY                        03230012
034500        ADD  1                 TO MQ-MSG-COUNT                    03240012
034600     ELSE                                                         03250012
034700        IF MQ-REASON-CODE  =  MQRC-NO-MSG-AVAILABLE               03260012
034800          SET NO-MORE-MSGS             TO  TRUE                   03270012
034900                                                                  03280012
035000        ELSE                                                      03290012
035100           MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE       03300012
035200           MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE          03310012
035300           MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME           03320012
035400           MOVE 'INP MQGET ERR:'  TO MQ-APPL-RETURN-MESSAGE       03330012
035500           PERFORM 9000-ERROR                                     03340012
035600           PERFORM 8000-TERMINATION                               03350012
035700       END-IF                                                     03360012
035800     END-IF.                                                      03370012
035900                                                                  03380012
036000 4000-PROCESS-REQUEST-REPLY.                                      03390012
036100     MOVE SPACES TO REPLY-MESSAGE                                 03400012
036100     INITIALIZE WS-DATE-TIME REPLACING NUMERIC BY ZEROES          03410012
036100                                                                  03420012
036100     EXEC CICS ASKTIME                                            03430012
036100          ABSTIME (WS-ABS-TIME)                                   03440012
036100     END-EXEC                                                     03450012
036100                                                                  03460012
036100     EXEC CICS FORMATTIME                                         03470012
036100          ABSTIME(WS-ABS-TIME)                                    03480012
036100          MMDDYYYY(WS-MMDDYYYY)                                   03490012
036100          DATESEP('-')                                            03500012
036100          TIME(WS-TIME)                                           03510012
036100          TIMESEP                                                 03520012
036100     END-EXEC                                                     03530012
036100                                                                  03540012
036200     STRING  'SYSTEM DATE : ' WS-MMDDYYYY                         03550012
036200             'SYSTEM TIME : ' WS-TIME                             03560012
036200             DELIMITED BY SIZE                                    03570012
036400             INTO                                                 03580012
036500             REPLY-MESSAGE                                        03590012
036600     END-STRING                                                   03600012
           PERFORM 4100-PUT-REPLY                                       03610012
036100                                                                  03620012
036100                                                                  03630012
036800     .                                                            03640012
036900                                                                  03650012
037000 4100-PUT-REPLY.                                                  03660012
037100                                                                  03670012
037200* PUT WILL PUT A MESSAGE ON THE QUEUE AND CONVERT IT TO A STRING  03680012
037300                                                                  03690012
037600                                                                  03700012
037700     MOVE REPLY-MESSAGE                TO MQ-BUFFER               03710012
037800     MOVE 1000                         TO MQ-BUFFER-LENGTH        03720012
037900     MOVE SAVE-MSGID                   TO MQMD-MSGID              03730012
038000     MOVE SAVE-CORELID                 TO MQMD-CORRELID           03740012
038100     MOVE MQFMT-STRING                 TO MQMD-FORMAT             03750012
038200                                                                  03760012
038300     COMPUTE MQMD-CODEDCHARSETID      =  MQCCSI-Q-MGR             03770012
038400                                                                  03780012
038500     COMPUTE MQPMO-OPTIONS = MQPMO-SYNCPOINT                      03790012
038600                           + MQPMO-DEFAULT-CONTEXT                03800012
038700                           + MQPMO-FAIL-IF-QUIESCING              03810012
038800                                                                  03820012
038900     CALL 'MQPUT'  USING MQ-HCONN                                 03830012
039000                         OUTPUT-QUEUE-HANDLE                      03840012
039100                         MQ-MESSAGE-DESCRIPTOR                    03850012
039200                         MQ-PUT-MESSAGE-OPTIONS                   03860012
039300                         MQ-BUFFER-LENGTH                         03870012
039400                         MQ-BUFFER                                03880012
039500                         MQ-CONDITION-CODE                        03890012
039600                         MQ-REASON-CODE                           03900012
039700                                                                  03910012
039800     EVALUATE MQ-CONDITION-CODE                                   03920012
039900         WHEN MQCC-OK                                             03930012
040000              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    03940012
040100              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       03950012
040200         WHEN OTHER                                               03960012
040300              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    03970012
040400              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       03980012
040500              MOVE REPLY-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        03990012
040600              MOVE 'MQPUT ERR'       TO MQ-APPL-RETURN-MESSAGE    04000012
040700              PERFORM 9000-ERROR                                  04010012
040800              PERFORM 8000-TERMINATION                            04020012
040900     END-EVALUATE.                                                04030012
041000                                                                  04040012
041100 9000-ERROR.                                                      04050012
041200* PUT WILL PUT A MESSAGE ON THE QUEUE AND CONVERT IT TO A STRING  04060012
041300                                                                  04070012
041600                                                                  04080012
041700     MOVE MQ-ERR-DISPLAY               TO ERROR-MESSAGE,          04090012
041800     MOVE ERROR-MESSAGE                TO MQ-BUFFER               04100012
041900     MOVE 1000                         TO MQ-BUFFER-LENGTH        04110012
042200     MOVE MQFMT-STRING                 TO MQMD-FORMAT             04120012
042300                                                                  04130012
042400     COMPUTE MQMD-CODEDCHARSETID      =  MQCCSI-Q-MGR             04140012
042500                                                                  04150012
042600     COMPUTE MQPMO-OPTIONS = MQPMO-SYNCPOINT                      04160012
042700                           + MQPMO-DEFAULT-CONTEXT                04170012
042800                           + MQPMO-FAIL-IF-QUIESCING              04180012
042900                                                                  04190012
043000     CALL 'MQPUT'  USING MQ-HCONN                                 04200012
043100                         ERROR-QUEUE-HANDLE                       04210012
043200                         MQ-MESSAGE-DESCRIPTOR                    04220012
043300                         MQ-PUT-MESSAGE-OPTIONS                   04230012
043400                         MQ-BUFFER-LENGTH                         04240012
043500                         MQ-BUFFER                                04250012
043600                         MQ-CONDITION-CODE                        04260012
043700                         MQ-REASON-CODE                           04270012
043800                                                                  04280012
043900     EVALUATE MQ-CONDITION-CODE                                   04290012
044000         WHEN MQCC-OK                                             04300012
044100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04310012
044200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04320012
044300         WHEN OTHER                                               04330012
044400              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04340012
044500              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04350012
044600              MOVE ERROR-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        04360012
044700              MOVE 'MQPUT ERR'       TO MQ-APPL-RETURN-MESSAGE    04370012
044800              DISPLAY MQ-ERR-DISPLAY                              04380012
044900              PERFORM 8000-TERMINATION                            04390012
045000     END-EVALUATE.                                                04400012
045100     .                                                            04410012
045200 8000-TERMINATION.                                                04420012
045300                                                                  04430012
045400     IF REPLY-QUEUE-OPEN                                          04440012
045500        PERFORM 5000-CLOSE-INPUT-QUEUE                            04450012
045600     END-IF                                                       04460012
045700     IF RESP-QUEUE-OPEN                                           04470012
045800        PERFORM 5100-CLOSE-OUTPUT-QUEUE                           04480012
045900     END-IF                                                       04490012
046000     IF ERR-QUEUE-OPEN                                            04500012
046100        PERFORM 5200-CLOSE-ERROR-QUEUE                            04510012
046200     END-IF                                                       04520012
046300     EXEC CICS RETURN END-EXEC                                    04530012
046400     GOBACK.                                                      04540012
046500                                                                  04550012
046600 5000-CLOSE-INPUT-QUEUE.                                          04560012
046700     MOVE INPUT-QUEUE-NAME           TO MQ-QUEUE                  04570012
046800     MOVE INPUT-QUEUE-HANDLE         TO MQ-HOBJ                   04580012
046900     COMPUTE MQ-OPTIONS = MQCO-NONE                               04590012
047000                                                                  04600012
047100     CALL 'MQCLOSE' USING MQ-HCONN                                04610012
047200                          MQ-HOBJ                                 04620012
047300                          MQ-OPTIONS                              04630012
047400                          MQ-CONDITION-CODE                       04640012
047500                          MQ-REASON-CODE                          04650012
047600                                                                  04660012
047700     EVALUATE MQ-CONDITION-CODE                                   04670012
047800         WHEN MQCC-OK                                             04680012
047900              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04690012
048000              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04700012
048100         WHEN OTHER                                               04710012
048200              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04720012
048300              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04730012
048400              MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        04740012
048500              MOVE 'MQCLOSE ERR'     TO MQ-APPL-RETURN-MESSAGE    04750012
048600              PERFORM 8000-TERMINATION                            04760012
048700     END-EVALUATE.                                                04770012
048800 5100-CLOSE-OUTPUT-QUEUE.                                         04780012
048900     MOVE REPLY-QUEUE-NAME            TO MQ-QUEUE                 04790012
049000     MOVE OUTPUT-QUEUE-HANDLE         TO MQ-HOBJ                  04800012
049100     COMPUTE MQ-OPTIONS = MQCO-NONE                               04810012
049200                                                                  04820012
049300     CALL 'MQCLOSE' USING MQ-HCONN                                04830012
049400                          MQ-HOBJ                                 04840012
049500                          MQ-OPTIONS                              04850012
049600                          MQ-CONDITION-CODE                       04860012
049700                          MQ-REASON-CODE                          04870012
049800                                                                  04880012
049900     EVALUATE MQ-CONDITION-CODE                                   04890012
050000         WHEN MQCC-OK                                             04900012
050100              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04910012
050200              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04920012
050300         WHEN OTHER                                               04930012
050400              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    04940012
050500              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       04950012
050600              MOVE INPUT-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        04960012
050700              MOVE 'MQCLOSE ERR'     TO MQ-APPL-RETURN-MESSAGE    04970012
050800              PERFORM 8000-TERMINATION                            04980012
050900     END-EVALUATE.                                                04990012
051000                                                                  05000012
051100 5200-CLOSE-ERROR-QUEUE.                                          05010012
051200     MOVE ERROR-QUEUE-NAME          TO MQ-QUEUE                   05020012
051300     MOVE ERROR-QUEUE-HANDLE         TO MQ-HOBJ                   05030012
051400     COMPUTE MQ-OPTIONS = MQCO-NONE                               05040012
051500                                                                  05050012
051600     CALL 'MQCLOSE' USING MQ-HCONN                                05060012
051700                          MQ-HOBJ                                 05070012
051800                          MQ-OPTIONS                              05080012
051900                          MQ-CONDITION-CODE                       05090012
052000                          MQ-REASON-CODE                          05100012
052100                                                                  05110012
052200     EVALUATE MQ-CONDITION-CODE                                   05120012
052300         WHEN MQCC-OK                                             05130012
052400              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05140012
052500              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05150012
052600         WHEN OTHER                                               05160012
052700              MOVE MQ-CONDITION-CODE TO MQ-APPL-CONDITION-CODE    05170012
052800              MOVE MQ-REASON-CODE    TO MQ-APPL-REASON-CODE       05180012
052900              MOVE ERROR-QUEUE-NAME  TO MQ-APPL-QUEUE-NAME        05190012
053000              MOVE 'MQCLOSE ERR'     TO MQ-APPL-RETURN-MESSAGE    05200012
053100              PERFORM 9000-ERROR                                  05210012
053200              PERFORM 8000-TERMINATION                            05220012
053300     END-EVALUATE.                                                05230012
053400                                                                  05240012
