000100******************************************************************00010000
000200* CardDemo - Admin Menu Options                                   00020000
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
001800******************************************************************00180000
001900 01 CARDDEMO-ADMIN-MENU-OPTIONS.                                  00190000
002000*    Option added for Db2 release                                 00200000
002100*  05 CDEMO-ADMIN-OPT-COUNT           PIC 9(02) VALUE 4.          00210000
002200   05 CDEMO-ADMIN-OPT-COUNT           PIC 9(02) VALUE 6.          00220000
002300*Option added for Db2 release                                     00230000
002400   05 CDEMO-ADMIN-OPTIONS-DATA.                                   00240000
002500                                                                  00250000
002600     10 FILLER                        PIC 9(02) VALUE 1.          00260000
002700     10 FILLER                        PIC X(35) VALUE             00270000
002800         'User List (Security)               '.                   00280000
002900     10 FILLER                        PIC X(08) VALUE 'COUSR00C'. 00290000
003000                                                                  00300000
003100     10 FILLER                        PIC 9(02) VALUE 2.          00310000
003200     10 FILLER                        PIC X(35) VALUE             00320000
003300         'User Add (Security)                '.                   00330000
003400     10 FILLER                        PIC X(08) VALUE 'COUSR01C'. 00340000
003500                                                                  00350000
003600     10 FILLER                        PIC 9(02) VALUE 3.          00360000
003700     10 FILLER                        PIC X(35) VALUE             00370000
003800         'User Update (Security)             '.                   00380000
003900     10 FILLER                        PIC X(08) VALUE 'COUSR02C'. 00390000
004000                                                                  00400000
004100     10 FILLER                        PIC 9(02) VALUE 4.          00410000
004200     10 FILLER                        PIC X(35) VALUE             00420000
004300         'User Delete (Security)             '.                   00430000
004400     10 FILLER                        PIC X(08) VALUE 'COUSR03C'. 00440000
004500*    Option added for Db2 V1 release start                        00450002
004600     10 FILLER                        PIC 9(02) VALUE 5.          00460000
004700     10 FILLER                        PIC X(35) VALUE             00470000
004701         'Transaction Type List/Update (Db2) '.                   00470102
004900     10 FILLER                        PIC X(08) VALUE 'COTRTLIC'. 00490002
004910     10 FILLER                        PIC 9(02) VALUE 6.          00491000
004920     10 FILLER                        PIC X(35) VALUE             00492000
004930         'Transaction Type Maintenance (Db2) '.                   00493003
004940     10 FILLER                        PIC X(08) VALUE 'COTRTUPC'. 00494002
005000*    Option added for Db2 v1 release end                          00500002
005100   05 CDEMO-ADMIN-OPTIONS REDEFINES CDEMO-ADMIN-OPTIONS-DATA.     00510000
005200     10 CDEMO-ADMIN-OPT OCCURS 9 TIMES.                           00520000
005300       15 CDEMO-ADMIN-OPT-NUM           PIC 9(02).                00530000
005400       15 CDEMO-ADMIN-OPT-NAME          PIC X(35).                00540000
005500       15 CDEMO-ADMIN-OPT-PGMNAME       PIC X(08).                00550000
005600*                                                                 00560000
005700* Ver: CardDemo_v2.0-16-gbdcb6ea-226 Date: 2024-01-21 17:49:00 CST
005800*                                                                 00580000
