      *****************************************************************         
      *    Reporting data structure for transaction report                      
      *****************************************************************         
       01  REPORT-NAME-HEADER.                                                  
           05  REPT-SHORT-NAME                  PIC X(38) VALUE                 
           'DALYREPT'.                                                          
           05  REPT-LONG-NAME                   PIC X(41) VALUE                 
           'Daily Transaction Report'.                                          
           05  REPT-DATE-HEADER                 PIC X(12) VALUE                 
           'Date Range: '.                                                      
           05  REPT-START-DATE                  PIC X(10) VALUE SPACES.         
           05  FILLER                           PIC X(04) VALUE ' to '.         
           05  REPT-END-DATE                    PIC X(10) VALUE SPACES.         
                                                                                
       01  TRANSACTION-DETAIL-REPORT.                                           
           05  TRAN-REPORT-TRANS-ID             PIC X(16).                      
           05  FILLER                           PIC X(01) VALUE SPACES.         
           05  TRAN-REPORT-ACCOUNT-ID           PIC X(11).                      
           05  FILLER                           PIC X(01) VALUE SPACES.         
           05  TRAN-REPORT-TYPE-CD              PIC X(02).                      
           05  FILLER                           PIC X(01) VALUE '-'.            
           05  TRAN-REPORT-TYPE-DESC            PIC X(15).                      
           05  FILLER                           PIC X(01) VALUE SPACES.         
           05  TRAN-REPORT-CAT-CD               PIC 9(04).                      
           05  FILLER                           PIC X(01) VALUE '-'.            
           05  TRAN-REPORT-CAT-DESC             PIC X(29).                      
           05  FILLER                           PIC X(01) VALUE SPACES.         
           05  TRAN-REPORT-SOURCE               PIC X(10).                      
           05  FILLER                           PIC X(04) VALUE SPACES.         
           05  TRAN-REPORT-AMT                  PIC -ZZZ,ZZZ,ZZZ.ZZ.            
           05  FILLER                           PIC X(02) VALUE SPACES.         
                                                                                
       01  TRANSACTION-HEADER-1.                                                
           05  FILLER                           PIC X(17) VALUE                 
           'Transaction ID'.                                                    
           05  FILLER                           PIC X(12) VALUE                 
           'Account ID'.                                                        
           05  FILLER                           PIC X(19) VALUE                 
           'Transaction Type'.                                                  
           05  FILLER                           PIC X(35) VALUE                 
           'Tran Category'.                                                     
           05  FILLER                           PIC X(14) VALUE                 
           'Tran Source'.                                                       
           05  FILLER                           PIC X VALUE SPACES.             
           05  FILLER                           PIC X(16) VALUE                 
           '        Amount'.                                                    
                                                                                
       01  TRANSACTION-HEADER-2  PIC X(133) VALUE ALL '-'.                      
                                                                                
       01  REPORT-PAGE-TOTALS.                                                  
           05  FILLER                           PIC X(11) VALUE                 
           'Page Total'.                                                        
           05  FILLER                           PIC X(86) VALUE ALL '.'.        
           05  REPT-PAGE-TOTAL                  PIC +ZZZ,ZZZ,ZZZ.ZZ.            
                                                                                
       01  REPORT-ACCOUNT-TOTALS.                                               
           05  FILLER                           PIC X(13) VALUE                 
           'Account Total'.                                                     
           05  FILLER                           PIC X(84) VALUE ALL '.'.        
           05  REPT-ACCOUNT-TOTAL               PIC +ZZZ,ZZZ,ZZZ.ZZ.            
                                                                                
       01  REPORT-GRAND-TOTALS.                                                 
           05  FILLER                           PIC X(11) VALUE                 
           'Grand Total'.                                                       
           05  FILLER                           PIC X(86) VALUE ALL '.'.        
           05  REPT-GRAND-TOTAL                 PIC +ZZZ,ZZZ,ZZZ.ZZ.            
                                                                                
                                                                                
                                                                                
                                                                                
      *
      * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
      *
