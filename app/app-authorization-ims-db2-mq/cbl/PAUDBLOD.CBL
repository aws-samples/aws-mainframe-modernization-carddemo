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
       PROGRAM-ID. PAUDBLOD.                                            00020053
       AUTHOR.     AWS.                                                 00030026
                                                                        00040026
       ENVIRONMENT DIVISION.                                            00050026
       CONFIGURATION SECTION.                                           00060026
                                                                        00070026
       INPUT-OUTPUT SECTION.                                            00080026
       FILE-CONTROL.                                                    00090026
           SELECT INFILE1 ASSIGN TO INFILE1                             00100053
           ORGANIZATION IS SEQUENTIAL                                   00110026
           ACCESS MODE  IS SEQUENTIAL                                   00120026
           FILE STATUS IS WS-INFIL1-STATUS.                             00130053
                                                                        00140026
      *                                                                 00150026
           SELECT INFILE2 ASSIGN TO INFILE2                             00151053
           ORGANIZATION IS SEQUENTIAL                                   00152026
           ACCESS MODE  IS SEQUENTIAL                                   00153026
           FILE STATUS IS WS-INFIL2-STATUS.                             00154053
                                                                        00155026
      *                                                                 00156026
      *----------------------------------------------------------------*00160026
       DATA DIVISION.                                                   00170026
      *----------------------------------------------------------------*00180026
      *                                                                 00190026
       FILE SECTION.                                                    00200026
       FD INFILE1.                                                      00210053
       01 INFIL1-REC                    PIC X(100).                     00220053
       FD INFILE2.                                                      00221053
       01 INFIL2-REC.                                                   00222053
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
         05 WS-END-OF-INFILE1          PIC X(01) VALUE SPACES.          00540053
         05 WS-END-OF-INFILE2          PIC X(01) VALUE SPACES.          00550053
         05 WS-INFILE-STATUS           PIC X(02) VALUE SPACES.          00570026
         05 WS-INFIL1-STATUS           PIC X(02) VALUE SPACES.          00571053
         05 WS-INFIL2-STATUS           PIC X(02) VALUE SPACES.          00572053
         05 END-ROOT-SEG-FILE          PIC X(01) VALUE SPACES.          00573053
         05 END-CHILD-SEG-FILE         PIC X(01) VALUE SPACES.          00574053
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
       01 ROOT-QUAL-SSA.                                                00831053
          05 QUAL-SSA-SEG-NAME      PIC X(08) VALUE 'PAUTSUM0'.         00831153
          05 FILLER                 PIC X(01) VALUE '('.                00831253
          05 QUAL-SSA-KEY-FIELD     PIC X(08) VALUE 'ACCNTID '.         00831354
          05 QUAL-SSA-REL-OPER      PIC X(02) VALUE 'EQ'.               00831454
          05 QUAL-SSA-KEY-VALUE     PIC S9(11) COMP-3.                  00831553
          05 FILLER                 PIC X(01) VALUE ')'.                00831653
      *                                                                 00831753
       01 ROOT-UNQUAL-SSA.                                              00831853
          05 FILLER                 PIC X(08) VALUE 'PAUTSUM0'.         00831953
          05 FILLER                 PIC X(01) VALUE ' '.                00832053
      *                                                                 00832153
       01 CHILD-UNQUAL-SSA.                                             00832253
          05 FILLER                 PIC X(08) VALUE 'PAUTDTL1'.         00832353
          05 FILLER                 PIC X(01) VALUE ' '.                00832453
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
       01 IO-PCB-MASK   PIC X(1).                                       01131057
       COPY PAUTBPCB.                                                   01140027
      *                                                                 01160026
      *----------------------------------------------------------------*01170026
       PROCEDURE DIVISION                  USING IO-PCB-MASK            01180057
                                                 PAUTBPCB.              01181056
      *                                          PGM-PCB-MASK.          01190028
      *----------------------------------------------------------------*01200026
      *                                                                 01210026
       MAIN-PARA.                                                       01220026
      *     DISPLAY 'CHECK PROG PCB:' PAUTBPCB.                         01222039
            ENTRY 'DLITCBL'                 USING PAUTBPCB.             01225033
                                                                        01226029
            DISPLAY 'STARTING PAUDBLOD'.                                01227053
      *                                                                 01230026
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01240026
      *                                                                 01250026
           PERFORM 2000-READ-ROOT-SEG-FILE        THRU 2000-EXIT        01260053
           UNTIL   END-ROOT-SEG-FILE  = 'Y'                             01280053
                                                                        01281053
           PERFORM 3000-READ-CHILD-SEG-FILE       THRU 3000-EXIT        01290058
           UNTIL   END-CHILD-SEG-FILE = 'Y'                             01300053
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
           DISPLAY '*-------------------------------------*'            01770026
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01790043
           DISPLAY ' '                                                  01800026
                                                                        01810026
           .                                                            01960026
           OPEN INPUT  INFILE1                                          01961054
           IF WS-INFIL1-STATUS =  SPACES OR '00'                        01962053
              CONTINUE                                                  01963028
           ELSE                                                         01964028
              DISPLAY 'ERROR IN OPENING INFILE1:' WS-INFIL1-STATUS      01965053
              PERFORM 9999-ABEND                                        01966028
           END-IF                                                       01967028
      *                                                                 01968028
           OPEN INPUT INFILE2                                           01969054
           IF WS-INFIL2-STATUS =  SPACES OR '00'                        01969153
              CONTINUE                                                  01969228
           ELSE                                                         01969328
              DISPLAY 'ERROR IN OPENING INFILE2:' WS-INFIL2-STATUS      01969453
              PERFORM 9999-ABEND                                        01969528
           END-IF.                                                      01969634
      *                                                                 01969728
      *                                                                 01970026
       1000-EXIT.                                                       01980026
            EXIT.                                                       01990026
      *                                                                 02000026
      *----------------------------------------------------------------*02010026
       2000-READ-ROOT-SEG-FILE.                                         02020053
      *----------------------------------------------------------------*02030026
      *                                                                 02040026
      *     DISPLAY 'IN 2000 READ ROOT SEG FILE PARA'                   02041061
            READ INFILE1                                                02042053
                                                                        02042153
            IF WS-INFIL1-STATUS =  SPACES OR '00'                       02042253
               MOVE INFIL1-REC TO PENDING-AUTH-SUMMARY                  02042353
               PERFORM 2100-INSERT-ROOT-SEG THRU 2100-EXIT              02042454
            ELSE                                                        02042553
               IF WS-INFIL1-STATUS = '10'                               02042653
                  MOVE 'Y' TO END-ROOT-SEG-FILE                         02042753
               ELSE                                                     02042853
                  DISPLAY 'ERROR READING ROOT SEG INFILE'               02042953
               END-IF                                                   02043053
            END-IF.                                                     02043153
                                                                        02043253
       2000-EXIT.                                                       02043353
            EXIT.                                                       02043453
                                                                        02043553
       2100-INSERT-ROOT-SEG.                                            02043653
                                                                        02043753
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02070053
                                        PAUTBPCB                        02080029
                                        PENDING-AUTH-SUMMARY            02090029
                                        ROOT-UNQUAL-SSA.                02100053
            DISPLAY ' *******************************'                  02130040
      *     DISPLAY ' AFTER THE ROOT SEG INSERT CALL '                  02130167
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02133067
      *     DISPLAY 'SEG NAME : ' PAUT-SEG-NAME                         02135067
            DISPLAY ' *******************************'                  02138053
            IF PAUT-PCB-STATUS = SPACES                                 02140053
               DISPLAY 'ROOT INSERT SUCCESS    '                        02160053
            END-IF                                                      02191053
            IF PAUT-PCB-STATUS = 'II'                                   02192053
               DISPLAY 'ROOT SEGMENT ALREADY IN DB'                     02194153
            END-IF                                                      02197053
            IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'            02200053
                  DISPLAY 'ROOT INSERT FAILED  :' PAUT-PCB-STATUS       02230053
                  PERFORM 9999-ABEND                                    02260053
            END-IF                                                      02270053
            .                                                           02271053
       2100-EXIT.                                                       02280053
            EXIT.                                                       02290053
      *                                                                 02310026
      *                                                                 02320026
      *----------------------------------------------------------------*02330026
       3000-READ-CHILD-SEG-FILE.                                        02340053
      *----------------------------------------------------------------*02350026
      *     DISPLAY 'IN 3000 READ CHILD SEG FILE PARA'                  02351067
            READ INFILE2                                                02352053
                                                                        02353053
            IF WS-INFIL2-STATUS =  SPACES OR '00'                       02354053
               IF ROOT-SEG-KEY IS NUMERIC                               02354162
      *        DISPLAY 'GNGTO ROOT SEG KEY'                             02355067
               MOVE ROOT-SEG-KEY  TO QUAL-SSA-KEY-VALUE                 02355260
      *        DISPLAY 'ROOT-SEG-KEY : '    QUAL-SSA-KEY-VALUE          02355367
      *        DISPLAY 'MOVED ROOT SEG KEY'                             02355467
               MOVE CHILD-SEG-REC TO PENDING-AUTH-DETAILS               02355562
               PERFORM 3100-INSERT-CHILD-SEG THRU 3100-EXIT             02356054
               END-IF                                                   02356162
            ELSE                                                        02357053
               IF WS-INFIL2-STATUS = '10'                               02358053
                  MOVE 'Y' TO END-CHILD-SEG-FILE                        02359053
               ELSE                                                     02359153
                  DISPLAY 'ERROR READING CHILD SEG INFILE'              02359253
               END-IF                                                   02359353
            END-IF.                                                     02359453
       3000-EXIT.                                                       02359553
            EXIT.                                                       02359653
       3100-INSERT-CHILD-SEG.                                           02359753
      *                                                                 02360026
      *     DISPLAY 'IN 3100 INSERT CHILD SEG PARA'                     02361061
            INITIALIZE PAUT-PCB-STATUS                                  02362058
            CALL 'CBLTDLI'       USING  FUNC-GU                         02370053
                                        PAUTBPCB                        02380030
                                        PENDING-AUTH-SUMMARY            02390053
                                        ROOT-QUAL-SSA.                  02400053
               DISPLAY '***************************'                    02401043
      *        DISPLAY ' AFTER ROOT SEG GU CALL    '                    02402067
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02410067
      *        DISPLAY 'SEG NAME : ' PAUT-SEG-NAME                      02411067
               DISPLAY '***************************'                    02412043
               IF PAUT-PCB-STATUS = SPACES                              02420030
                  DISPLAY 'GU CALL TO ROOT SEG SUCCESS'                 02430053
      *           ADD 2 TO PA-AUTH-DATE-9C                              02430167
      *           ADD 2 TO PA-AUTH-TIME-9C                              02430267
                  PERFORM 3200-INSERT-IMS-CALL  THRU 3200-EXIT          02430353
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'         02520059
                  DISPLAY 'ROOT GU CALL FAIL:' PAUT-PCB-STATUS          02530053
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02531048
                    PERFORM 9999-ABEND                                  02540049
               END-IF.                                                  02550051
       3100-EXIT.                                                       02590053
            EXIT.                                                       02600026
      *                                                                 02610026
       3200-INSERT-IMS-CALL.                                            02611053
      *                                                                 02611153
      *     DISPLAY 'IN 3200 INSERT CALL'                               02611261
                  CALL 'CBLTDLI' USING  FUNC-ISRT                       02611353
                                        PAUTBPCB                        02611453
                                        PENDING-AUTH-DETAILS            02611553
                                        CHILD-UNQUAL-SSA.               02611654
                                                                        02611753
            IF PAUT-PCB-STATUS = SPACES                                 02611866
               DISPLAY 'CHILD SEGMENT INSERTED SUCCESS'                 02611966
            END-IF                                                      02612055
            IF PAUT-PCB-STATUS = 'II'                                   02612166
               DISPLAY 'CHILD SEGMENT ALREADY IN DB'                    02612266
            END-IF                                                      02612366
            IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'            02612466
                  DISPLAY 'INSERT CALL FAIL FOR CHILD:' PAUT-PCB-STATUS 02612566
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02612666
                    PERFORM 9999-ABEND                                  02612766
            END-IF.                                                     02612866
                                                                        02612966
       3200-EXIT.                                                       02613066
            EXIT.                                                       02614066
      *----------------------------------------------------------------*02620026
       4000-FILE-CLOSE.                                                 02630030
            DISPLAY 'CLOSING THE FILE'                                  02631043
            CLOSE INFILE1.                                              02640053
                                                                        02650030
            IF WS-INFIL1-STATUS =  SPACES OR '00'                       02660053
             CONTINUE                                                   02670030
            ELSE                                                        02680034
             DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-INFIL1-STATUS       02690053
            END-IF.                                                     02700034
            CLOSE INFILE2.                                              02710053
                                                                        02720030
            IF WS-INFIL2-STATUS =  SPACES OR '00'                       02730053
             CONTINUE                                                   02740030
            ELSE                                                        02750034
             DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-INFIL2-STATUS       02760053
            END-IF.                                                     02770034
       4000-EXIT.                                                       02780030
            EXIT.                                                       02790030
      *----------------------------------------------------------------*03620026
       9999-ABEND.                                                      03630026
      *----------------------------------------------------------------*03640026
      *                                                                 03650026
           DISPLAY 'IMS LOAD ABENDING ...'                              03660054
                                                                        03670026
           MOVE 16 TO RETURN-CODE                                       03680026
           GOBACK.                                                      03690026
      *                                                                 03700026
       9999-EXIT.                                                       03710026
            EXIT.                                                       03720026
