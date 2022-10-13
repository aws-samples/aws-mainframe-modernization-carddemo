      *****************************************************************         
      *    Data-structure for DALYTRANsaction record (RECLN = 350)              
      *****************************************************************         
       01  DALYTRAN-RECORD.                                                     
           05  DALYTRAN-ID                             PIC X(16).               
           05  DALYTRAN-TYPE-CD                        PIC X(02).               
           05  DALYTRAN-CAT-CD                         PIC 9(04).               
           05  DALYTRAN-SOURCE                         PIC X(10).               
           05  DALYTRAN-DESC                           PIC X(100).              
           05  DALYTRAN-AMT                            PIC S9(09)V99.           
           05  DALYTRAN-MERCHANT-ID                    PIC 9(09).               
           05  DALYTRAN-MERCHANT-NAME                  PIC X(50).               
           05  DALYTRAN-MERCHANT-CITY                  PIC X(50).               
           05  DALYTRAN-MERCHANT-ZIP                   PIC X(10).               
           05  DALYTRAN-CARD-NUM                       PIC X(16).               
           05  DALYTRAN-ORIG-TS                        PIC X(26).               
           05  DALYTRAN-PROC-TS                        PIC X(26).               
           05  FILLER                                  PIC X(20).       
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *
