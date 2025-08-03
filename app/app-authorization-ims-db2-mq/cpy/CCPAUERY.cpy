      *****************************************************************
      *   PENDING AUTHORIZATION ERROR LOGS                                      
      *****************************************************************
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
          01 ERROR-LOG-RECORD.                                                  
               05 ERR-DATE                     PIC  X(06).                      
               05 ERR-TIME                     PIC  X(06).                      
               05 ERR-APPLICATION              PIC  X(08).                      
               05 ERR-PROGRAM                  PIC  X(08).                      
               05 ERR-LOCATION                 PIC  X(04).                      
               05 ERR-LEVEL                    PIC  X(01).                      
                  88 ERR-LOG                   VALUE 'L'.                       
                  88 ERR-INFO                  VALUE 'I'.                       
                  88 ERR-WARNING               VALUE 'W'.                       
                  88 ERR-CRITICAL              VALUE 'C'.                       
               05 ERR-SUBSYSTEM                PIC  X(01).                      
                  88 ERR-APP                   VALUE 'A'.                       
                  88 ERR-CICS                  VALUE 'C'.                       
                  88 ERR-IMS                   VALUE 'I'.                       
                  88 ERR-DB2                   VALUE 'D'.                       
                  88 ERR-MQ                    VALUE 'M'.                       
                  88 ERR-FILE                  VALUE 'F'.                       
               05 ERR-CODE-1                   PIC  X(09).                      
               05 ERR-CODE-2                   PIC  X(09).                      
               05 ERR-MESSAGE                  PIC  X(50).                      
               05 ERR-EVENT-KEY                PIC  X(20).                      
