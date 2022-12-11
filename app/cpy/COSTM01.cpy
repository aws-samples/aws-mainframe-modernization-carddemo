       01  TRNX-RECORD.                                                         
           05  TRNX-KEY.                                                        
               10  TRNX-CARD-NUM                       PIC X(16).               
               10  TRNX-ID                             PIC X(16).               
           05  TRNX-REST.                                                       
               10  TRNX-TYPE-CD                        PIC X(02).               
               10  TRNX-CAT-CD                         PIC 9(04).               
               10  TRNX-SOURCE                         PIC X(10).               
               10  TRNX-DESC                           PIC X(100).              
               10  TRNX-AMT                            PIC S9(09)V99.           
               10  TRNX-MERCHANT-ID                    PIC 9(09).               
               10  TRNX-MERCHANT-NAME                  PIC X(50).               
               10  TRNX-MERCHANT-CITY                  PIC X(50).               
               10  TRNX-MERCHANT-ZIP                   PIC X(10).               
               10  TRNX-ORIG-TS                        PIC X(26).               
               10  TRNX-PROC-TS                        PIC X(26).               
               10  FILLER                              PIC X(20).               
