      ******************************************************************
      * CardDemo - Admin Menu Options
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
       01 CARDDEMO-MAIN-MENU-OPTIONS.

         05 CDEMO-MENU-OPT-COUNT           PIC 9(02) VALUE 11.

         05 CDEMO-MENU-OPTIONS-DATA.

           10 FILLER                       PIC 9(02) VALUE 1.
           10 FILLER                       PIC X(35) VALUE
               'Account View                       '.
           10 FILLER                       PIC X(08) VALUE 'COACTVWC'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 2.
           10 FILLER                       PIC X(35) VALUE
               'Account Update                     '.
           10 FILLER                       PIC X(08) VALUE 'COACTUPC'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 3.
           10 FILLER                       PIC X(35) VALUE
               'Credit Card List                   '.
           10 FILLER                       PIC X(08) VALUE 'COCRDLIC'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 4.
           10 FILLER                       PIC X(35) VALUE
               'Credit Card View                   '.
           10 FILLER                       PIC X(08) VALUE 'COCRDSLC'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 5.
           10 FILLER                       PIC X(35) VALUE
               'Credit Card Update                 '.
           10 FILLER                       PIC X(08) VALUE 'COCRDUPC'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 6.
           10 FILLER                       PIC X(35) VALUE
               'Transaction List                   '.
           10 FILLER                       PIC X(08) VALUE 'COTRN00C'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 7.
           10 FILLER                       PIC X(35) VALUE
               'Transaction View                   '.
           10 FILLER                       PIC X(08) VALUE 'COTRN01C'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                        PIC 9(02) VALUE 8.
           10 FILLER                       PIC X(35) VALUE
      *        'Transaction Add (Admin Only)       '.
               'Transaction Add                    '.
           10 FILLER                       PIC X(08) VALUE 'COTRN02C'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 9.
           10 FILLER                       PIC X(35) VALUE
               'Transaction Reports                '.
           10 FILLER                       PIC X(08) VALUE 'CORPT00C'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 10.
           10 FILLER                       PIC X(35) VALUE
               'Bill Payment                       '.
           10 FILLER                       PIC X(08) VALUE 'COBIL00C'.
           10 FILLER                       PIC X(01) VALUE 'U'.

           10 FILLER                       PIC 9(02) VALUE 11.
           10 FILLER                       PIC X(35) VALUE
               'Pending Authorization View         '.
           10 FILLER                       PIC X(08) VALUE 'COPAUS0C'.
           10 FILLER                       PIC X(01) VALUE 'U'.


         05 CDEMO-MENU-OPTIONS REDEFINES CDEMO-MENU-OPTIONS-DATA.
           10 CDEMO-MENU-OPT OCCURS 12 TIMES.
             15 CDEMO-MENU-OPT-NUM           PIC 9(02).
             15 CDEMO-MENU-OPT-NAME          PIC X(35).
             15 CDEMO-MENU-OPT-PGMNAME       PIC X(08).
             15 CDEMO-MENU-OPT-USRTYPE       PIC X(01).
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:15:58 CDT
      *
