      *****************************************************************         
      *    Data-structure for transaction category balance (RECLN = 50)         
      *****************************************************************         
       01  TRAN-CAT-BAL-RECORD.                                                 
           05  TRAN-CAT-KEY.                                                    
              10 TRANCAT-ACCT-ID                       PIC 9(11).               
              10 TRANCAT-TYPE-CD                       PIC X(02).               
              10 TRANCAT-CD                            PIC 9(04).               
           05  TRAN-CAT-BAL                            PIC S9(09)V99.           
           05  FILLER                                  PIC X(22).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:00 CDT
      *
