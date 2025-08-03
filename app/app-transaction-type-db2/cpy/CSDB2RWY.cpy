000100******************************************************************00010000
000200* CardDemo - Common Working Storage for Db2                       00020000
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
036900******************************************************************03690000
037000*  Db2 Common variables                                           03700000
037100******************************************************************03710000
037101   05  WS-DB2-COMMON-VARS.                                        03710100
037102       10 WS-DISP-SQLCODE                    PIC ----9.           03710200
037103       10 WS-DUMMY-DB2-INT                   PIC S9(4) COMP-3     03710300
037104                                             VALUE 0.             03710400
037105       10 WS-DB2-PROCESSING-FLAG             PIC X(1).            03710500
037106          88  WS-DB2-OK                      VALUE '0'.           03710600
037107          88  WS-DB2-ERROR                   VALUE '1'.           03710700
037108       10 WS-DB2-CURRENT-ACTION              PIC X(72)            03710800
037109                                             VALUE SPACES.        03710900
037110******************************************************************03711000
037111*  Db2 DSNTIAC Message Construction                               03711100
037112******************************************************************03711200
037200   05  WS-DSNTIAC-FORMATTED.                                      03720000
037300       10  WS-DSNTIAC-MESG-LEN   PIC S9(4) USAGE COMP VALUE +720. 03730000
037400       10  WS-DSNTIAC-FMTD-TEXT.                                  03740001
037500           15 WS-DSNTIAC-FMTD-TEXT-LINE                           03750001
037600                                 PIC X(72)                        03760000
037700                                 OCCURS 10 TIMES                  03770000
037800                                 VALUE SPACES.                    03780000
037900                                                                  03790000
038000    05 WS-DSNTIAC-LRECL          PIC S9(4) USAGE COMP VALUE +72.  03800000
038100    05 WS-DSNTIAC-ERROR.                                          03810000
038200       10 WS-DSNTIAC-ERR-MSG     PIC X(10) VALUE 'DSNTIAC CD'.    03820000
038300       10 WS-DSNTIAC-ERR-CD-X    PIC X(02) VALUE SPACES.
             10 WS-DSNTIAC-ERR-CD      REDEFINES
                WS-DSNTIAC-ERR-CD-X    PIC 9(02).                       03830000
