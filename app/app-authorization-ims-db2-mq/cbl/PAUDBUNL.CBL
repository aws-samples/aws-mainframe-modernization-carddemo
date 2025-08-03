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
       IDENTIFICATION DIVISION.                                         00010026
       PROGRAM-ID. PAUDBUNL.                                            00020037
       AUTHOR.     AWS.                                                 00030026
                                                                        00040026
       ENVIRONMENT DIVISION.                                            00050026
       CONFIGURATION SECTION.                                           00060026
                                                                        00070026
       INPUT-OUTPUT SECTION.                                            00080026
       FILE-CONTROL.                                                    00090026
           SELECT OPFILE1 ASSIGN TO OUTFIL1                             00100035
           ORGANIZATION IS SEQUENTIAL                                   00110026
           ACCESS MODE  IS SEQUENTIAL                                   00120026
           FILE STATUS IS WS-OUTFL1-STATUS.                             00130026
                                                                        00140026
      *                                                                 00150026
           SELECT OPFILE2 ASSIGN TO OUTFIL2                             00151035
           ORGANIZATION IS SEQUENTIAL                                   00152026
           ACCESS MODE  IS SEQUENTIAL                                   00153026
           FILE STATUS IS WS-OUTFL2-STATUS.                             00154026
                                                                        00155026
      *                                                                 00156026
      *----------------------------------------------------------------*00160026
       DATA DIVISION.                                                   00170026
      *----------------------------------------------------------------*00180026
      *                                                                 00190026
       FILE SECTION.                                                    00200026
       FD OPFILE1.                                                      00210026
       01 OPFIL1-REC                    PIC X(100).                     00220026
       FD OPFILE2.                                                      00221026
       01 OPFIL2-REC.                                                   00222036
          05 ROOT-SEG-KEY               PIC S9(11) COMP-3.              00223036
          05 CHILD-SEG-REC              PIC X(200).                     00224036
      *                                                                 00230026
      *----------------------------------------------------------------*00240026
       WORKING-STORAGE SECTION.                                         00250026
      *----------------------------------------------------------------*00260026
       01 WS-VARIABLES.                                                 00270026
         05 WS-PGMNAME                 PIC X(08) VALUE 'IMSUNLOD'.      00280026
         05 CURRENT-DATE               PIC 9(06).                       00290026
         05 CURRENT-YYDDD              PIC 9(05).                       00300026
         05 WS-AUTH-DATE               PIC 9(05).                       00310026
         05 WS-EXPIRY-DAYS             PIC S9(4) COMP.                  00320026
         05 WS-DAY-DIFF                PIC S9(4) COMP.                  00330026
         05 IDX                        PIC S9(4) COMP.                  00340026
         05 WS-CURR-APP-ID             PIC 9(11).                       00350026
      *                                                                 00360026
         05 WS-NO-CHKP                 PIC  9(8) VALUE 0.               00370026
         05 WS-AUTH-SMRY-PROC-CNT      PIC  9(8) VALUE 0.               00380026
         05 WS-TOT-REC-WRITTEN         PIC S9(8) COMP VALUE 0.          00390026
         05 WS-NO-SUMRY-READ           PIC S9(8) COMP VALUE 0.          00400026
         05 WS-NO-SUMRY-DELETED        PIC S9(8) COMP VALUE 0.          00410026
         05 WS-NO-DTL-READ             PIC S9(8) COMP VALUE 0.          00420026
         05 WS-NO-DTL-DELETED          PIC S9(8) COMP VALUE 0.          00430026
      *                                                                 00440026
         05 WS-ERR-FLG                 PIC X(01) VALUE 'N'.             00450026
           88 ERR-FLG-ON                         VALUE 'Y'.             00460026
           88 ERR-FLG-OFF                        VALUE 'N'.             00470026
         05 WS-END-OF-AUTHDB-FLAG      PIC X(01) VALUE 'N'.             00480026
           88 END-OF-AUTHDB                      VALUE 'Y'.             00490026
           88 NOT-END-OF-AUTHDB                  VALUE 'N'.             00500026
         05 WS-MORE-AUTHS-FLAG         PIC X(01) VALUE 'N'.             00510026
           88 MORE-AUTHS                         VALUE 'Y'.             00520026
           88 NO-MORE-AUTHS                      VALUE 'N'.             00530026
         05 WS-END-OF-ROOT-SEG         PIC X(01) VALUE SPACES.          00540050
         05 WS-END-OF-CHILD-SEG        PIC X(01) VALUE SPACES.          00550050
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.          00570026
         05 WS-OUTFL1-STATUS           PIC X(02) VALUE SPACES.          00571026
         05 WS-OUTFL2-STATUS           PIC X(02) VALUE SPACES.          00572026
         05 WS-CUSTID-STATUS           PIC X(02) VALUE SPACES.          00580026
            88 END-OF-FILE                       VALUE '10'.            00590026
      *                                                                 00600026
         05 WK-CHKPT-ID.                                                00610026
            10  FILLER              PIC  X(04) VALUE 'RMAD'.            00620026
            10  WK-CHKPT-ID-CTR     PIC  9(04) VALUE ZEROES.            00630026
      *                                                                 00640026
       01 WS-IMS-VARIABLES.                                             00650026
      *   05 PSB-NAME                        PIC X(8) VALUE 'IMSUNLOD'. 00660042
      *   05 PCB-OFFSET.                                                00670042
      *      10 PAUT-PCB-NUM                 PIC S9(4) COMP VALUE +2.   00680042
          05 IMS-RETURN-CODE                 PIC X(02).                 00690026
             88 STATUS-OK                    VALUE '  ', 'FW'.          00700026
             88 SEGMENT-NOT-FOUND            VALUE 'GE'.                00710026
             88 DUPLICATE-SEGMENT-FOUND      VALUE 'II'.                00720026
             88 WRONG-PARENTAGE              VALUE 'GP'.                00730026
             88 END-OF-DB                    VALUE 'GB'.                00740026
             88 DATABASE-UNAVAILABLE         VALUE 'BA'.                00750026
             88 PSB-SCHEDULED-MORE-THAN-ONCE VALUE 'TC'.                00760026
             88 COULD-NOT-SCHEDULE-PSB       VALUE 'TE'.                00770026
             88 RETRY-CONDITION              VALUE 'BA', 'FH', 'TE'.    00780026
          05 WS-IMS-PSB-SCHD-FLG             PIC X(1).                  00790026
             88  IMS-PSB-SCHD                VALUE 'Y'.                 00800026
             88  IMS-PSB-NOT-SCHD            VALUE 'N'.                 00810026
                                                                        00820026
      *                                                                 00830026
       01 ROOT-UNQUAL-SSA.                                              00831029
          05 FILLER                 PIC X(08) VALUE 'PAUTSUM0'.         00831129
          05 FILLER                 PIC X(01) VALUE ' '.                00831229
      *                                                                 00831329
       01 CHILD-UNQUAL-SSA.                                             00831429
          05 FILLER                 PIC X(08) VALUE 'PAUTDTL1'.         00831529
          05 FILLER                 PIC X(01) VALUE ' '.                00831629
      *                                                                 00833029
       01 PRM-INFO.                                                     00840026
          05 P-EXPIRY-DAYS          PIC 9(02).                          00850026
          05 FILLER                 PIC X(01).                          00860026
          05 P-CHKP-FREQ            PIC X(05).                          00870026
          05 FILLER                 PIC X(01).                          00880026
          05 P-CHKP-DIS-FREQ        PIC X(05).                          00890026
          05 FILLER                 PIC X(01).                          00900026
          05 P-DEBUG-FLAG           PIC X(01).                          00910026
             88 DEBUG-ON            VALUE 'Y'.                          00920026
             88 DEBUG-OFF           VALUE 'N'.                          00930026
          05 FILLER                 PIC X(01).                          00940026
      *                                                                 00950026
      *                                                                 00960026
       COPY IMSFUNCS.                                                   00961032
      *----------------------------------------------------------------*00970026
      *  IMS SEGMENT LAYOUT                                             00980026
      *----------------------------------------------------------------*00990026
                                                                        01000026
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                   01010026
       01 PENDING-AUTH-SUMMARY.                                         01020026
       COPY CIPAUSMY.                                                   01030026
                                                                        01040026
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                  01050026
       01 PENDING-AUTH-DETAILS.                                         01060026
       COPY CIPAUDTY.                                                   01070026
                                                                        01080026
      *                                                                 01090026
      *----------------------------------------------------------------*01100026
       LINKAGE SECTION.                                                 01110026
      *----------------------------------------------------------------*01120026
      * PCB MASKS FOLLOW                                                01130026
       COPY PAUTBPCB.                                                   01140027
      *                                                                 01160026
      *----------------------------------------------------------------*01170026
       PROCEDURE DIVISION                  USING PAUTBPCB.              01180028
      *                                          PGM-PCB-MASK.          01190028
      *----------------------------------------------------------------*01200026
      *                                                                 01210026
       MAIN-PARA.                                                       01220026
            ENTRY 'DLITCBL'                 USING PAUTBPCB.             01225033
                                                                        01226029
      *                                                                 01230026
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01240026
      *                                                                 01250026
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT        01260026
           UNTIL   WS-END-OF-ROOT-SEG = 'Y'                             01280050
                                                                        01531150
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01532030
      *                                                                 01540026
      *                                                                 01560026
      *                                                                 01650026
           GOBACK.                                                      01660026
      *                                                                 01670026
      *----------------------------------------------------------------*01680026
       1000-INITIALIZE.                                                 01690026
      *----------------------------------------------------------------*01700026
      *                                                                 01710026
           ACCEPT CURRENT-DATE     FROM DATE                            01720026
           ACCEPT CURRENT-YYDDD    FROM DAY                             01730026
                                                                        01740026
      *    ACCEPT PRM-INFO FROM SYSIN                                   01750038
           DISPLAY 'STARTING PROGRAM PAUDBUNL::'                        01760054
           DISPLAY '*-------------------------------------*'            01770026
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01790043
           DISPLAY ' '                                                  01800026
                                                                        01810026
           .                                                            01960026
           OPEN OUTPUT OPFILE1                                          01961028
           IF WS-OUTFL1-STATUS =  SPACES OR '00'                        01962028
              CONTINUE                                                  01963028
           ELSE                                                         01964028
              DISPLAY 'ERROR IN OPENING OPFILE1:' WS-OUTFL1-STATUS      01965028
              PERFORM 9999-ABEND                                        01966028
           END-IF                                                       01967028
      *                                                                 01968028
           OPEN OUTPUT OPFILE2                                          01969028
           IF WS-OUTFL2-STATUS =  SPACES OR '00'                        01969128
              CONTINUE                                                  01969228
           ELSE                                                         01969328
              DISPLAY 'ERROR IN OPENING OPFILE2:' WS-OUTFL2-STATUS      01969428
              PERFORM 9999-ABEND                                        01969528
           END-IF.                                                      01969634
      *                                                                 01969728
      *                                                                 01970026
       1000-EXIT.                                                       01980026
            EXIT.                                                       01990026
      *                                                                 02000026
      *----------------------------------------------------------------*02010026
       2000-FIND-NEXT-AUTH-SUMMARY.                                     02020026
      *----------------------------------------------------------------*02030026
      *                                                                 02040026
      *     DISPLAY 'IN 2000 READ ROOT SEGMENT PARA'                    02041057
      *              PAUT-PCB-STATUS                                    02065050
            INITIALIZE PAUT-PCB-STATUS                                  02066047
            CALL 'CBLTDLI'            USING  FUNC-GN                    02070034
                                        PAUTBPCB                        02080029
                                        PENDING-AUTH-SUMMARY            02090029
                                        ROOT-UNQUAL-SSA.                02100029
      *     DISPLAY ' *******************************'                  02130057
      *     DISPLAY ' AFTER THE ROOT SEG IMS CALL    '                  02130157
      *     DISPLAY 'SEG LEVEL: ' PAUT-SEG-LEVEL                        02132057
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02133057
      *     DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                       02135057
      *     DISPLAY ' *******************************'                  02138043
               IF PAUT-PCB-STATUS = SPACES                              02140029
      *             SET NOT-END-OF-AUTHDB TO TRUE                       02160050
                    ADD 1                 TO WS-NO-SUMRY-READ           02170026
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02180026
                    MOVE PENDING-AUTH-SUMMARY TO OPFIL1-REC             02190030
                    INITIALIZE ROOT-SEG-KEY                             02190156
                    INITIALIZE CHILD-SEG-REC                            02190256
                    MOVE PA-ACCT-ID           TO ROOT-SEG-KEY           02190356
      *             DISPLAY 'WRITING FIRST FILE'                        02190456
                    IF PA-ACCT-ID IS NUMERIC                            02190556
                    WRITE OPFIL1-REC                                    02190656
                    INITIALIZE WS-END-OF-CHILD-SEG                      02190756
                    PERFORM 3000-FIND-NEXT-AUTH-DTL THRU 3000-EXIT      02190856
                    UNTIL  WS-END-OF-CHILD-SEG='Y'                      02190956
                    END-IF                                              02191056
               END-IF                                                   02191156
               IF PAUT-PCB-STATUS = 'GB'                                02192029
                    SET END-OF-AUTHDB     TO TRUE                       02194029
                    MOVE 'Y' TO WS-END-OF-ROOT-SEG                      02195050
               END-IF                                                   02197029
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GB'         02200029
                  DISPLAY 'AUTH SUM  GN FAILED  :' PAUT-PCB-STATUS      02230029
                  DISPLAY 'KEY FEEDBACK AREA    :' PAUT-KEYFB           02240048
                    PERFORM 9999-ABEND                                  02260026
            .                                                           02280026
       2000-EXIT.                                                       02290026
            EXIT.                                                       02300026
      *                                                                 02310026
      *                                                                 02320026
      *----------------------------------------------------------------*02330026
       3000-FIND-NEXT-AUTH-DTL.                                         02340026
      *----------------------------------------------------------------*02350026
      *                                                                 02360026
      *     DISPLAY 'IN 3000 READ CHILD SEGMENT PARA'                   02361057
            CALL 'CBLTDLI'            USING  FUNC-GNP                   02370034
                                        PAUTBPCB                        02380030
                                        PENDING-AUTH-DETAILS            02390030
                                        CHILD-UNQUAL-SSA.               02400030
      *        DISPLAY '***************************'                    02401057
      *        DISPLAY ' AFTER CHILD SEG IMS CALL  '                    02402057
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02410057
      *        DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                    02411057
      *        DISPLAY '***************************'                    02412057
               IF PAUT-PCB-STATUS = SPACES                              02420030
                    SET MORE-AUTHS       TO TRUE                        02430030
                    ADD 1                 TO WS-NO-SUMRY-READ           02440030
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02450030
                    MOVE PENDING-AUTH-DETAILS TO CHILD-SEG-REC          02460036
                    WRITE OPFIL2-REC                                    02470030
               END-IF                                                   02480030
               IF PAUT-PCB-STATUS = 'GE'                                02490030
      *             SET NO-MORE-AUTHS    TO TRUE                        02500050
                    MOVE 'Y' TO WS-END-OF-CHILD-SEG                     02500150
                    DISPLAY 'CHILD SEG FLAG GE : '                      02501044
                             WS-END-OF-CHILD-SEG                        02502050
               END-IF                                                   02510030
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'GE'         02520030
                  DISPLAY 'GNP CALL FAILED  :' PAUT-PCB-STATUS          02530030
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02531048
                    PERFORM 9999-ABEND                                  02540049
               END-IF.                                                  02550051
               INITIALIZE PAUT-PCB-STATUS.                              02580052
       3000-EXIT.                                                       02590026
            EXIT.                                                       02600026
      *                                                                 02610026
      *----------------------------------------------------------------*02620026
       4000-FILE-CLOSE.                                                 02630030
            DISPLAY 'CLOSING THE FILE'                                  02631043
            CLOSE OPFILE1.                                              02640034
                                                                        02650030
            IF WS-OUTFL1-STATUS =  SPACES OR '00'                       02660034
             CONTINUE                                                   02670030
            ELSE                                                        02680034
             DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-OUTFL1-STATUS       02690030
            END-IF.                                                     02700034
            CLOSE OPFILE2.                                              02710034
                                                                        02720030
            IF WS-OUTFL2-STATUS =  SPACES OR '00'                       02730034
             CONTINUE                                                   02740030
            ELSE                                                        02750034
             DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-OUTFL2-STATUS       02760030
            END-IF.                                                     02770034
       4000-EXIT.                                                       02780030
            EXIT.                                                       02790030
      *----------------------------------------------------------------*03620026
       9999-ABEND.                                                      03630026
      *----------------------------------------------------------------*03640026
      *                                                                 03650026
           DISPLAY 'IMSUNLOD ABENDING ...'                              03660030
                                                                        03670026
           MOVE 16 TO RETURN-CODE                                       03680026
           GOBACK.                                                      03690026
      *                                                                 03700026
       9999-EXIT.                                                       03710026
            EXIT.                                                       03720026
