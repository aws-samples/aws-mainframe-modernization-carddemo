      *****************************************************************         
      *    Data-structure for transaction category type (RECLN = 60)            
      *****************************************************************         
       01  TRAN-CAT-RECORD.                                                     
           05  TRAN-CAT-KEY.                                                    
              10  TRAN-TYPE-CD                         PIC X(02).               
              10  TRAN-CAT-CD                          PIC 9(04).               
           05  TRAN-CAT-TYPE-DESC                      PIC X(50).               
           05  FILLER                                  PIC X(04).               
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *
