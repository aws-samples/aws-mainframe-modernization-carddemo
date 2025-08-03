      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      *
      * Licensed under the Apache License, Version 2.0 (the "License").
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at
      *
      *    http://www.apache.org/licenses/LICENSE-2.0
      *
      * Unless required by applicable law or agreed to in writing,
      * software distributed under the License is distributed on an
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
      * either express or implied. See the License for the specific
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. DBUNLDGS.                                            00020000
       AUTHOR.     AWS.                                                 00030000
                                                                        00040000
       ENVIRONMENT DIVISION.                                            00050000
       CONFIGURATION SECTION.                                           00060000
                                                                        00070000
       INPUT-OUTPUT SECTION.                                            00080000
       FILE-CONTROL.                                                    00090000
      *    SELECT OPFILE1 ASSIGN TO OUTFIL1                             00100000
      *    ORGANIZATION IS SEQUENTIAL                                   00110000
      *    ACCESS MODE  IS SEQUENTIAL                                   00120000
      *    FILE STATUS IS WS-OUTFL1-STATUS.                             00130000
                                                                        00140000
      *                                                                 00150000
      *    SELECT OPFILE2 ASSIGN TO OUTFIL2                             00160000
      *    ORGANIZATION IS SEQUENTIAL                                   00170000
      *    ACCESS MODE  IS SEQUENTIAL                                   00180000
      *    FILE STATUS IS WS-OUTFL2-STATUS.                             00190000
                                                                        00200000
      *                                                                 00210000
      *----------------------------------------------------------------*00220000
       DATA DIVISION.                                                   00230000
      *----------------------------------------------------------------*00240000
      *                                                                 00250000
      *FILE SECTION.                                                    00260000
      *FD OPFILE1.                                                      00270000
      *01 OPFIL1-REC                    PIC X(100).                     00280000
      *FD OPFILE2.                                                      00290000
      *01 OPFIL2-REC.                                                   00300000
      *   05 ROOT-SEG-KEY               PIC S9(11) COMP-3.              00310000
      *   05 CHILD-SEG-REC              PIC X(200).                     00320000
      *                                                                 00330000
      *----------------------------------------------------------------*00340000
       WORKING-STORAGE SECTION.                                         00350000
      *----------------------------------------------------------------*00360000
       01 OPFIL1-REC                    PIC X(100).                     00361000
       01 OPFIL2-REC.                                                   00362000
          05 ROOT-SEG-KEY               PIC S9(11) COMP-3.              00363000
          05 CHILD-SEG-REC              PIC X(200).                     00364000
       01 WS-VARIABLES.                                                 00370000
         05 WS-PGMNAME                 PIC X(08) VALUE 'IMSUNLOD'.      00380000
         05 CURRENT-DATE               PIC 9(06).                       00390000
         05 CURRENT-YYDDD              PIC 9(05).                       00400000
         05 WS-AUTH-DATE               PIC 9(05).                       00410000
         05 WS-EXPIRY-DAYS             PIC S9(4) COMP.                  00420000
         05 WS-DAY-DIFF                PIC S9(4) COMP.                  00430000
         05 IDX                        PIC S9(4) COMP.                  00440000
         05 WS-CURR-APP-ID             PIC 9(11).                       00450000
      *                                                                 00460000
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.               00470000
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.               00480000
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.          00490000
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.          00500000
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.          00510000
         05 WS-NO-DTL-READ             PIC S9(8) COMP VALUE 0.          00520000
         05 WS-NO-DTL-DELETED          PIC S9(8) COMP VALUE 0.          00530000
      *                                                                 00540000
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.             00550000
           88 ERR-FLG-ON                         VALUE 'Y'.             00560000
           88 ERR-FLG-OFF                        VALUE 'N'.             00570000
         05 WS-END-OF-AUTHDB-FLAG      PIC X(01) VALUE 'N'.             00580000
           88 END-OF-AUTHDB                      VALUE 'Y'.             00590000
           88 NOT-END-OF-AUTHDB                  VALUE 'N'.             00600000
         05 WS-MORE-AUTHS-FLAG         PIC X(01) VALUE 'N'.             00610000
           88 MORE-AUTHS                         VALUE 'Y'.             00620000
           88 NO-MORE-AUTHS                      VALUE 'N'.             00630000
         05 WS-END-OF-ROOT-SEG         PIC X(01) VALUE SPACES.          00640000
         05 WS-END-OF-CHILD-SEG        PIC X(01) VALUE SPACES.          00650000
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.          00660000
         05 WS-OUTFL1-STATUS           PIC X(02) VALUE SPACES.          00670000
         05 WS-OUTFL2-STATUS           PIC X(02) VALUE SPACES.          00680000
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.          00690000
            88 END-OF-FILE                       VALUE '10'.            00700000
      *                                                                 00710000
         05 WK-CHKPT-ID.                                                00720000
            10  FILLER              PIC  X(04) VALUE 'RMAD'.            00730000
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.            00740000
      *                                                                 00750000
       01 WS-IMS-VARIABLES.                                             00760000
      *   05 PSB-NAME                        PIC X(8) VALUE 'IMSUNLOD'. 00770000
      *   05 PCB-OFFSET.                                                00780000
      *      10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +2.   00790000
          05 IMS-RETURN-CODE                 PIC X(02).                 00800000
             88 STATUS-OK                    VALUE '  ', 'FW'.          00810000
             88 SEGMENT-NOT-FOUND            VALUE 'GE'.                00820000
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.                00830000
             88 WRONG-PARENTAGE              VALUE 'GP'.                00840000
             88 END-OF-DB                    VALUE 'GB'.                00850000
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.                00860000
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.                00870000
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.                00880000
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.    00890000
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).                  00900000
             88  IMS-PSB-SCHD                VALUE 'Y'.                 00910000
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.                 00920000
                                                                        00930000
      *                                                                 00940000
       01 ROOT-UNQUAL-SSA.                                              00950000
          05 FILLER                 PIC X(08) VALUE 'PAUTSUM0'.         00960000
          05 FILLER                 PIC X(01) VALUE ' '.                00970000
      *                                                                 00980000
       01 CHILD-UNQUAL-SSA.                                             00990000
          05 FILLER                 PIC X(08) VALUE 'PAUTDTL1'.         01000000
          05 FILLER                 PIC X(01) VALUE ' '.                01010000
      *                                                                 01020000
       01 PRM-INFO.                                                     01030000
          05 P-EXPIRY-DAYS          PIC 9(02).                          01040000
          05 FILLER                 PIC X(01).                          01050000
          05 P-CHKP-FREQ            PIC X(05).                          01060000
          05 FILLER                 PIC X(01).                          01070000
          05 P-CHKP-DIS-FREQ        PIC X(05).                          01080000
          05 FILLER                 PIC X(01).                          01090000
          05 P-DEBUG-FLAG           PIC X(01).                          01100000
             88 DEBUG-ON            VALUE 'Y'.                          01110000
             88 DEBUG-OFF           VALUE 'N'.                          01120000
          05 FILLER                 PIC X(01).                          01130000
      *                                                                 01140000
      *                                                                 01150000
       COPY IMSFUNCS.                                                   01160000
      *----------------------------------------------------------------*01170000
      *  IMS SEGMENT LAYOUT                                             01180000
      *----------------------------------------------------------------*01190000
                                                                        01200000
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                   01210000
       01 PENDING-AUTH-SUMMARY.                                         01220000
       COPY CIPAUSMY.                                                   01230000
                                                                        01240000
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                  01250000
       01 PENDING-AUTH-DETAILS.                                         01260000
       COPY CIPAUDTY.                                                   01270000
                                                                        01280000
      *                                                                 01290000
      *----------------------------------------------------------------*01300000
       LINKAGE SECTION.                                                 01310000
      *----------------------------------------------------------------*01320000
      * PCB MASKS FOLLOW                                                01330000
        COPY PAUTBPCB.                                                  01340000
        COPY PASFLPCB.                                                  01341000
        COPY PADFLPCB.                                                  01342000
      *                                                                 01350000
      *----------------------------------------------------------------*01360000
       PROCEDURE DIVISION                  USING PAUTBPCB               01370000
                                                 PASFLPCB               01380000
                                                 PADFLPCB.              01381000
      *----------------------------------------------------------------*01390000
      *                                                                 01400000
       MAIN-PARA.                                                       01410000
            ENTRY 'DLITCBL'                 USING PAUTBPCB              01420000
                                                  PASFLPCB              01421000
                                                  PADFLPCB.             01422000
                                                                        01430000
      *                                                                 01440000
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01450000
      *                                                                 01460000
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT        01470000
           UNTIL   WS-END-OF-ROOT-SEG = 'Y'                             01480000
                                                                        01490000
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01500000
      *                                                                 01510000
      *                                                                 01520000
      *                                                                 01530000
           GOBACK.                                                      01540000
      *                                                                 01550000
      *----------------------------------------------------------------*01560000
       1000-INITIALIZE.                                                 01570000
      *----------------------------------------------------------------*01580000
      *                                                                 01590000
           ACCEPT CURRENT-DATE     FROM DATE                            01600000
           ACCEPT CURRENT-YYDDD    FROM DAY                             01610000
                                                                        01620000
      *    ACCEPT PRM-INFO FROM SYSIN                                   01630000
           DISPLAY 'STARTING PROGRAM DBUNLDGS::'                        01640000
           DISPLAY '*-------------------------------------*'            01650000
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01660000
           DISPLAY ' '                                                  01670000
                                                                        01680000
           .                                                            01690000
      *    OPEN OUTPUT OPFILE1                                          01700000
      *    IF WS-OUTFL1-STATUS =  SPACES OR '00'                        01710000
      *       CONTINUE                                                  01720000
      *    ELSE                                                         01730000
      *       DISPLAY 'ERROR IN OPENING OPFILE1:' WS-OUTFL1-STATUS      01740000
      *       PERFORM 9999-ABEND                                        01750000
      *    END-IF                                                       01760000
      *                                                                 01770000
      *    OPEN OUTPUT OPFILE2                                          01780000
      *    IF WS-OUTFL2-STATUS =  SPACES OR '00'                        01790000
      *       CONTINUE                                                  01800000
      *    ELSE                                                         01810000
      *       DISPLAY 'ERROR IN OPENING OPFILE2:' WS-OUTFL2-STATUS      01820000
      *       PERFORM 9999-ABEND                                        01830000
      *    END-IF.                                                      01840000
      *                                                                 01850000
      *                                                                 01860000
       1000-EXIT.                                                       01870000
            EXIT.                                                       01880000
      *                                                                 01890000
      *----------------------------------------------------------------*01900000
       2000-FIND-NEXT-AUTH-SUMMARY.                                     01910000
      *----------------------------------------------------------------*01920000
      *                                                                 01930000
      *     DISPLAY 'IN 2000 READ ROOT SEGMENT PARA'                    01940002
      *              PAUT-PCB-STATUS                                    01950000
            INITIALIZE PAUT-PCB-STATUS                                  01960000
            CALL 'CBLTDLI'            USING  FUNC-GN                    01970000
                                        PAUTBPCB                        01980000
                                        PENDING-AUTH-SUMMARY            01990000
                                        ROOT-UNQUAL-SSA.                02000000
      *     DISPLAY ' *******************************'                  02010002
      *     DISPLAY ' AFTER THE ROOT SEG IMS CALL    '                  02020002
      *     DISPLAY 'SEG LEVEL: ' PAUT-SEG-LEVEL                        02030002
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02040002
      *     DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                       02050002
      *     DISPLAY ' *******************************'                  02060000
               IF PAUT-PCB-STATUS = SPACES                              02070000
      *             SET NOT-END-OF-AUTHDB TO TRUE                       02080000
                    ADD 1                 TO WS-NO-SUMRY-READ           02090000
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02100000
                    MOVE PENDING-AUTH-SUMMARY TO OPFIL1-REC             02110000
                    INITIALIZE ROOT-SEG-KEY                             02120000
                    INITIALIZE CHILD-SEG-REC                            02130000
                    MOVE PA-ACCT-ID           TO ROOT-SEG-KEY           02140000
      *             DISPLAY 'WRITING FIRST FILE'                        02150000
                    IF PA-ACCT-ID IS NUMERIC                            02160000
      *             WRITE OPFIL1-REC                                    02170000
                    PERFORM 3100-INSERT-PARENT-SEG-GSAM THRU 3100-EXIT  02171000
                    INITIALIZE WS-END-OF-CHILD-SEG                      02180000
                    PERFORM 3000-FIND-NEXT-AUTH-DTL THRU 3000-EXIT      02190000
                    UNTIL  WS-END-OF-CHILD-SEG='Y'                      02200000
                    END-IF                                              02210000
               END-IF                                                   02220000
               IF PAUT-PCB-STATUS = 'GB'                                02230000
                    SET END-OF-AUTHDB     TO TRUE                       02240000
                    MOVE 'Y' TO WS-END-OF-ROOT-SEG                      02250000
               END-IF                                                   02260000
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GB'         02270000
                  DISPLAY 'AUTH SUM  GN FAILED  :' PAUT-PCB-STATUS      02280000
                  DISPLAY 'KEY FEEDBACK AREA    :' PAUT-KEYFB           02290000
                    PERFORM 9999-ABEND                                  02300000
            .                                                           02310000
       2000-EXIT.                                                       02320000
            EXIT.                                                       02330000
      *                                                                 02340000
      *                                                                 02350000
      *----------------------------------------------------------------*02360000
       3000-FIND-NEXT-AUTH-DTL.                                         02370000
      *----------------------------------------------------------------*02380000
      *                                                                 02390000
      *     DISPLAY 'IN 3000 READ CHILD SEGMENT PARA'                   02400002
            CALL 'CBLTDLI'            USING  FUNC-GNP                   02410000
                                        PAUTBPCB                        02420000
                                        PENDING-AUTH-DETAILS            02430000
                                        CHILD-UNQUAL-SSA.               02440000
      *        DISPLAY '***************************'                    02450002
      *        DISPLAY ' AFTER CHILD SEG IMS CALL  '                    02460002
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02470002
      *        DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                    02480002
      *        DISPLAY '***************************'                    02490002
               IF PAUT-PCB-STATUS = SPACES                              02500000
                    SET MORE-AUTHS       TO TRUE                        02510000
                    ADD 1                 TO WS-NO-SUMRY-READ           02520000
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02530000
                    MOVE PENDING-AUTH-DETAILS TO CHILD-SEG-REC          02540000
      *             WRITE OPFIL2-REC                                    02550000
                    PERFORM 3200-INSERT-CHILD-SEG-GSAM THRU 3200-EXIT   02551000
               END-IF                                                   02560000
               IF PAUT-PCB-STATUS = 'GE'                                02570000
      *             SET NO-MORE-AUTHS    TO TRUE                        02580000
                    MOVE 'Y' TO WS-END-OF-CHILD-SEG                     02590000
                    DISPLAY 'CHILD SEG FLAG GE : '                      02600000
                             WS-END-OF-CHILD-SEG                        02610000
               END-IF                                                   02620000
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GE'         02630000
                  DISPLAY 'GNP CALL FAILED  :' PAUT-PCB-STATUS          02640000
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02650000
                    PERFORM 9999-ABEND                                  02660000
               END-IF.                                                  02670000
               INITIALIZE PAUT-PCB-STATUS.                              02680000
       3000-EXIT.                                                       02690000
            EXIT.                                                       02700000
      *                                                                 02710000
      *----------------------------------------------------------------*02710100
       3100-INSERT-PARENT-SEG-GSAM.                                     02710200
      *     DISPLAY 'IN 3100 INSERT-PARENT-SEG-GSAM'                    02710302
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02710400
                                        PASFLPCB                        02710500
                                        PENDING-AUTH-SUMMARY.           02710600
      *        DISPLAY '***************************'                    02710802
      *        DISPLAY ' AFTER PARENT GSAM IMS CALL'                    02710902
      *        DISPLAY ' PASFL-DBDNAME : ' PASFL-DBDNAME                02711002
      *        DISPLAY ' PASFL-PCB-PROCOPT : ' PASFL-PCB-PROCOPT        02711102
      *        DISPLAY 'PCB STATUS: ' PASFL-PCB-STATUS                  02711202
      *        DISPLAY '***************************'                    02711302
               IF PASFL-PCB-STATUS NOT EQUAL TO SPACES                  02711401
                  DISPLAY 'GSAM PARENT FAIL :' PASFL-PCB-STATUS         02711501
                  DISPLAY 'KFB AREA IN GSAM:' PASFL-KEYFB               02711601
                  PERFORM 9999-ABEND                                    02711701
               END-IF.                                                  02711801
       3100-EXIT.                                                       02712000
            EXIT.                                                       02713000
      *----------------------------------------------------------------*02720000
       3200-INSERT-CHILD-SEG-GSAM.                                      02721000
      *     DISPLAY 'IN 3200 INSERT-CHILD-SEG-GSAM'                     02721102
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02721200
                                        PADFLPCB                        02721300
                                        PENDING-AUTH-DETAILS.           02721400
      *        DISPLAY '***************************'                    02721502
      *        DISPLAY ' AFTER CHILD GSAM IMS CALL'                     02721602
      *        DISPLAY 'PADFL-DBDNAME : ' PADFL-DBDNAME                 02721702
      *        DISPLAY 'PCB STATUS: ' PADFL-PCB-STATUS                  02721802
      *        DISPLAY 'PADFL-PCB-PROCOPT : ' PADFL-PCB-PROCOPT         02721902
      *        DISPLAY '***************************'                    02722002
               IF PADFL-PCB-STATUS NOT EQUAL TO SPACES                  02722101
                  DISPLAY 'GSAM PARENT FAIL :' PADFL-PCB-STATUS         02722201
                  DISPLAY 'KFB AREA IN GSAM:' PADFL-KEYFB               02722301
                  PERFORM 9999-ABEND                                    02722401
               END-IF.                                                  02722501
       3200-EXIT.                                                       02722601
            EXIT.                                                       02723000
      *----------------------------------------------------------------*02724000
       4000-FILE-CLOSE.                                                 02730000
            DISPLAY 'CLOSING THE FILE'.                                 02740000
      *     CLOSE OPFILE1.                                              02750000
      *                                                                 02760000
      *     IF WS-OUTFL1-STATUS =  SPACES OR '00'                       02770000
      *      CONTINUE                                                   02780000
      *     ELSE                                                        02790000
      *      DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-OUTFL1-STATUS       02800000
      *     END-IF.                                                     02810000
      *     CLOSE OPFILE2.                                              02820000
      *                                                                 02830000
      *     IF WS-OUTFL2-STATUS =  SPACES OR '00'                       02840000
      *      CONTINUE                                                   02850000
      *     ELSE                                                        02860000
      *      DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-OUTFL2-STATUS       02870000
      *     END-IF.                                                     02880000
       4000-EXIT.                                                       02890000
            EXIT.                                                       02900000
      *----------------------------------------------------------------*02910000
       9999-ABEND.                                                      02920000
      *----------------------------------------------------------------*02930000
      *                                                                 02940000
           DISPLAY 'DBUNLDGS ABENDING ...'                              02950000
                                                                        02960000
           MOVE 16 TO RETURN-CODE                                       02970000
           GOBACK.                                                      02980000
      *                                                                 02990000
       9999-EXIT.                                                       03000000
            EXIT.                                                       03010000
