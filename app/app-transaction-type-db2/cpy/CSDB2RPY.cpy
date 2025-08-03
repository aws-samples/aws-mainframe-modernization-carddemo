000100******************************************************************00010000
000200* CardDemo - Common Procedures for Db2                            00020000
000300******************************************************************00030000
000400* Copyright Amazon.com, Inc. or its affiliates.                   00040000
000500* All Rights Reserved.                                            00050000
000600*                                                                 00060000
000700* Licensed under the Apache License, Version 2.0 (the "License"). 00070000
000800* You may not use this file except in compliance with the License.00080000
000900* You may obtain a copy of the License at                         00090000
001000*                                                                 00100000
001100*    http://www.apache.org/licenses/LICENSE-2.0                   00110000
001200*                                                                 00120000
001300* Unless required by applicable law or agreed to in writing,      00130000
001400* software distributed under the License is distributed on an     00140000
001500* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    00150000
001600* either express or implied. See the License for the specific     00160000
001700* language governing permissions and limitations under the License00170000
176800***************************************************************** 17680000
176900* Dummy call to verify connectivity to Db2                      * 17690000
177000***************************************************************** 17700000
177100 9998-PRIMING-QUERY.                                              17710000
177200                                                                  17720000
177300     EXEC SQL                                                     17730000
177500          SELECT 1                                                17750000
177600            INTO :WS-DUMMY-DB2-INT                                17760000
177700            FROM SYSIBM.SYSDUMMY1                                 17770000
177900            FETCH FIRST 1 ROW ONLY                                17790000
178000     END-EXEC                                                     17800000
178100                                                                  17810000
178200                                                                  17820000
178300     MOVE SQLCODE        TO WS-DISP-SQLCODE                       17830000
178400                                                                  17840000
178500     EVALUATE TRUE                                                17850000
178600        WHEN SQLCODE = ZERO                                       17860000
178700           CONTINUE                                               17870000
178800        WHEN OTHER                                                17880000
178900*          This is some kind of error. Format message and exit    17890000
179000           SET WS-DB2-ERROR        TO TRUE                        17900000
179100                                                                  17910000
179200           MOVE 'Db2 access failure. '                            17920000
179300                                        TO WS-DB2-CURRENT-ACTION  17930000
179400           PERFORM 9999-FORMAT-DB2-MESSAGE                        17940000
179500              THRU 9999-FORMAT-DB2-MESSAGE-EXIT                   17950000
179600      END-EVALUATE                                                17960000
179700     .                                                            17970000
179800 9998-PRIMING-QUERY-EXIT.                                         17980000
179900     EXIT                                                         17990000
180000     .                                                            18000000
205300***************************************************************** 20530000
205400* Construct formatted message using DSNTIAC utility             * 20540000
205500* This is for Db2                                               * 20550000
205600***************************************************************** 20560000
205700 9999-FORMAT-DB2-MESSAGE.                                         20570000
205800
           MOVE +72                 TO WS-DSNTIAC-LRECL                 20580000
205900
           CALL LIT-DSNTIAC  USING  DFHEIBLK,                           20590000
206000                              DFHCOMMAREA,                        20600000
206100                              SQLCA,                              20610000
206200                              WS-DSNTIAC-FORMATTED,               20620000
206300                              WS-DSNTIAC-LRECL
 06400     END-CALL                                                     20640000
206401                                                                  20640100
206402     COMPUTE WS-DSNTIAC-ERR-CD = RETURN-CODE

206403                                                                  20640301
206410     IF WS-DSNTIAC-ERR-CD = ZEROES                                20641000
206420        CONTINUE                                                  20642002
206430     ELSE                                                         20643000
206431        MOVE 'DSNTIAC CD: '       TO WS-DSNTIAC-ERR-MSG           20643102
206432        MOVE WS-DSNTIAC-ERROR     TO WS-DSNTIAC-FMTD-TEXT         20643202
206450     END-IF                                                       20645000
206500                                                                  20650000
206600     STRING                                                       20660000
206700          FUNCTION TRIM(WS-DB2-CURRENT-ACTION)                    20670000
206800                   ' SQLCODE:'                                    20680000
206900                   WS-DISP-SQLCODE                                20690000
207000                   ' '                                            20700000
207100*                  SQLERRM OF SQLCA                               20710000
207200                   WS-DSNTIAC-FMTD-TEXT                           20720002
207300          DELIMITED BY SIZE                                       20730000
207400     INTO WS-LONG-MSG                                             20740000
207500     END-STRING                                                   20750000
207600     MOVE WS-LONG-MSG       TO WS-RETURN-MSG                      20760000
207700     .                                                            20770000
207800                                                                  20780000
207900 9999-FORMAT-DB2-MESSAGE-EXIT.                                    20790000
208000     EXIT                                                         20800000
208100     .                                                            20810000
