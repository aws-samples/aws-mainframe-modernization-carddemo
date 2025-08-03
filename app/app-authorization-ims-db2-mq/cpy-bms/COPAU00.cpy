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
       01  COPAU0AI.
           02  FILLER PIC X(12).                                                
           02  TRNNAMEL    COMP  PIC  S9(4).                                    
           02  TRNNAMEF    PICTURE X.                                           
           02  FILLER REDEFINES TRNNAMEF.                                       
             03 TRNNAMEA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TRNNAMEI  PIC X(4).                                              
           02  TITLE01L    COMP  PIC  S9(4).                                    
           02  TITLE01F    PICTURE X.                                           
           02  FILLER REDEFINES TITLE01F.                                       
             03 TITLE01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TITLE01I  PIC X(40).                                             
           02  CURDATEL    COMP  PIC  S9(4).                                    
           02  CURDATEF    PICTURE X.                                           
           02  FILLER REDEFINES CURDATEF.                                       
             03 CURDATEA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CURDATEI  PIC X(8).                                              
           02  PGMNAMEL    COMP  PIC  S9(4).                                    
           02  PGMNAMEF    PICTURE X.                                           
           02  FILLER REDEFINES PGMNAMEF.                                       
             03 PGMNAMEA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PGMNAMEI  PIC X(8).                                              
           02  TITLE02L    COMP  PIC  S9(4).                                    
           02  TITLE02F    PICTURE X.                                           
           02  FILLER REDEFINES TITLE02F.                                       
             03 TITLE02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TITLE02I  PIC X(40).                                             
           02  CURTIMEL    COMP  PIC  S9(4).                                    
           02  CURTIMEF    PICTURE X.                                           
           02  FILLER REDEFINES CURTIMEF.                                       
             03 CURTIMEA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CURTIMEI  PIC X(8).                                              
           02  ACCTIDL    COMP  PIC  S9(4).                                     
           02  ACCTIDF    PICTURE X.                                            
           02  FILLER REDEFINES ACCTIDF.                                        
             03 ACCTIDA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  ACCTIDI  PIC X(11).                                              
           02  CNAMEL    COMP  PIC  S9(4).                                      
           02  CNAMEF    PICTURE X.                                             
           02  FILLER REDEFINES CNAMEF.                                         
             03 CNAMEA    PICTURE X.                                            
           02  FILLER   PICTURE X(4).                                           
           02  CNAMEI  PIC X(25).                                               
           02  CUSTIDL    COMP  PIC  S9(4).                                     
           02  CUSTIDF    PICTURE X.                                            
           02  FILLER REDEFINES CUSTIDF.                                        
             03 CUSTIDA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  CUSTIDI  PIC X(9).                                               
           02  ADDR001L    COMP  PIC  S9(4).                                    
           02  ADDR001F    PICTURE X.                                           
           02  FILLER REDEFINES ADDR001F.                                       
             03 ADDR001A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  ADDR001I  PIC X(25).                                             
           02  ACCSTATL    COMP  PIC  S9(4).                                    
           02  ACCSTATF    PICTURE X.                                           
           02  FILLER REDEFINES ACCSTATF.                                       
             03 ACCSTATA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  ACCSTATI  PIC X(1).                                              
           02  ADDR002L    COMP  PIC  S9(4).                                    
           02  ADDR002F    PICTURE X.                                           
           02  FILLER REDEFINES ADDR002F.                                       
             03 ADDR002A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  ADDR002I  PIC X(25).                                             
           02  PHONE1L    COMP  PIC  S9(4).                                     
           02  PHONE1F    PICTURE X.                                            
           02  FILLER REDEFINES PHONE1F.                                        
             03 PHONE1A    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  PHONE1I  PIC X(13).                                              
           02  APPRCNTL    COMP  PIC  S9(4).                                    
           02  APPRCNTF    PICTURE X.                                           
           02  FILLER REDEFINES APPRCNTF.                                       
             03 APPRCNTA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  APPRCNTI  PIC X(3).                                              
           02  DECLCNTL    COMP  PIC  S9(4).                                    
           02  DECLCNTF    PICTURE X.                                           
           02  FILLER REDEFINES DECLCNTF.                                       
             03 DECLCNTA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  DECLCNTI  PIC X(3).                                              
           02  CREDLIML    COMP  PIC  S9(4).                                    
           02  CREDLIMF    PICTURE X.                                           
           02  FILLER REDEFINES CREDLIMF.                                       
             03 CREDLIMA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CREDLIMI  PIC X(12).                                             
           02  CASHLIML    COMP  PIC  S9(4).                                    
           02  CASHLIMF    PICTURE X.                                           
           02  FILLER REDEFINES CASHLIMF.                                       
             03 CASHLIMA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CASHLIMI  PIC X(9).                                              
           02  APPRAMTL    COMP  PIC  S9(4).                                    
           02  APPRAMTF    PICTURE X.                                           
           02  FILLER REDEFINES APPRAMTF.                                       
             03 APPRAMTA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  APPRAMTI  PIC X(10).                                             
           02  CREDBALL    COMP  PIC  S9(4).                                    
           02  CREDBALF    PICTURE X.                                           
           02  FILLER REDEFINES CREDBALF.                                       
             03 CREDBALA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CREDBALI  PIC X(12).                                             
           02  CASHBALL    COMP  PIC  S9(4).                                    
           02  CASHBALF    PICTURE X.                                           
           02  FILLER REDEFINES CASHBALF.                                       
             03 CASHBALA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CASHBALI  PIC X(9).                                              
           02  DECLAMTL    COMP  PIC  S9(4).                                    
           02  DECLAMTF    PICTURE X.                                           
           02  FILLER REDEFINES DECLAMTF.                                       
             03 DECLAMTA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  DECLAMTI  PIC X(10).                                             
           02  SEL0001L    COMP  PIC  S9(4).                                    
           02  SEL0001F    PICTURE X.                                           
           02  FILLER REDEFINES SEL0001F.                                       
             03 SEL0001A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  SEL0001I  PIC X(1).                                              
           02  TRNID01L    COMP  PIC  S9(4).                                    
           02  TRNID01F    PICTURE X.                                           
           02  FILLER REDEFINES TRNID01F.                                       
             03 TRNID01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TRNID01I  PIC X(16).                                             
           02  PDATE01L    COMP  PIC  S9(4).                                    
           02  PDATE01F    PICTURE X.                                           
           02  FILLER REDEFINES PDATE01F.                                       
             03 PDATE01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PDATE01I  PIC X(8).                                              
           02  PTIME01L    COMP  PIC  S9(4).                                    
           02  PTIME01F    PICTURE X.                                           
           02  FILLER REDEFINES PTIME01F.                                       
             03 PTIME01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTIME01I  PIC X(8).                                              
           02  PTYPE01L    COMP  PIC  S9(4).                                    
           02  PTYPE01F    PICTURE X.                                           
           02  FILLER REDEFINES PTYPE01F.                                       
             03 PTYPE01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTYPE01I  PIC X(4).                                              
           02  PAPRV01L    COMP  PIC  S9(4).                                    
           02  PAPRV01F    PICTURE X.                                           
           02  FILLER REDEFINES PAPRV01F.                                       
             03 PAPRV01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAPRV01I  PIC X(1).                                              
           02  PSTAT01L    COMP  PIC  S9(4).                                    
           02  PSTAT01F    PICTURE X.                                           
           02  FILLER REDEFINES PSTAT01F.                                       
             03 PSTAT01A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PSTAT01I  PIC X(1).                                              
           02  PAMT001L    COMP  PIC  S9(4).                                    
           02  PAMT001F    PICTURE X.                                           
           02  FILLER REDEFINES PAMT001F.                                       
             03 PAMT001A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAMT001I  PIC X(12).                                             
           02  SEL0002L    COMP  PIC  S9(4).                                    
           02  SEL0002F    PICTURE X.                                           
           02  FILLER REDEFINES SEL0002F.                                       
             03 SEL0002A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  SEL0002I  PIC X(1).                                              
           02  TRNID02L    COMP  PIC  S9(4).                                    
           02  TRNID02F    PICTURE X.                                           
           02  FILLER REDEFINES TRNID02F.                                       
             03 TRNID02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TRNID02I  PIC X(16).                                             
           02  PDATE02L    COMP  PIC  S9(4).                                    
           02  PDATE02F    PICTURE X.                                           
           02  FILLER REDEFINES PDATE02F.                                       
             03 PDATE02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PDATE02I  PIC X(8).                                              
           02  PTIME02L    COMP  PIC  S9(4).                                    
           02  PTIME02F    PICTURE X.                                           
           02  FILLER REDEFINES PTIME02F.                                       
             03 PTIME02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTIME02I  PIC X(8).                                              
           02  PTYPE02L    COMP  PIC  S9(4).                                    
           02  PTYPE02F    PICTURE X.                                           
           02  FILLER REDEFINES PTYPE02F.                                       
             03 PTYPE02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTYPE02I  PIC X(4).                                              
           02  PAPRV02L    COMP  PIC  S9(4).                                    
           02  PAPRV02F    PICTURE X.                                           
           02  FILLER REDEFINES PAPRV02F.                                       
             03 PAPRV02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAPRV02I  PIC X(1).                                              
           02  PSTAT02L    COMP  PIC  S9(4).                                    
           02  PSTAT02F    PICTURE X.                                           
           02  FILLER REDEFINES PSTAT02F.                                       
             03 PSTAT02A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PSTAT02I  PIC X(1).                                              
           02  PAMT002L    COMP  PIC  S9(4).                                    
           02  PAMT002F    PICTURE X.                                           
           02  FILLER REDEFINES PAMT002F.                                       
             03 PAMT002A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAMT002I  PIC X(12).                                             
           02  SEL0003L    COMP  PIC  S9(4).                                    
           02  SEL0003F    PICTURE X.                                           
           02  FILLER REDEFINES SEL0003F.                                       
             03 SEL0003A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  SEL0003I  PIC X(1).                                              
           02  TRNID03L    COMP  PIC  S9(4).                                    
           02  TRNID03F    PICTURE X.                                           
           02  FILLER REDEFINES TRNID03F.                                       
             03 TRNID03A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TRNID03I  PIC X(16).                                             
           02  PDATE03L    COMP  PIC  S9(4).                                    
           02  PDATE03F    PICTURE X.                                           
           02  FILLER REDEFINES PDATE03F.                                       
             03 PDATE03A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PDATE03I  PIC X(8).                                              
           02  PTIME03L    COMP  PIC  S9(4).                                    
           02  PTIME03F    PICTURE X.                                           
           02  FILLER REDEFINES PTIME03F.                                       
             03 PTIME03A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTIME03I  PIC X(8).                                              
           02  PTYPE03L    COMP  PIC  S9(4).                                    
           02  PTYPE03F    PICTURE X.                                           
           02  FILLER REDEFINES PTYPE03F.                                       
             03 PTYPE03A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTYPE03I  PIC X(4).                                              
           02  PAPRV03L    COMP  PIC  S9(4).                                    
           02  PAPRV03F    PICTURE X.                                           
           02  FILLER REDEFINES PAPRV03F.                                       
             03 PAPRV03A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAPRV03I  PIC X(1).                                              
           02  PSTAT03L    COMP  PIC  S9(4).                                    
           02  PSTAT03F    PICTURE X.                                           
           02  FILLER REDEFINES PSTAT03F.                                       
             03 PSTAT03A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PSTAT03I  PIC X(1).                                              
           02  PAMT003L    COMP  PIC  S9(4).                                    
           02  PAMT003F    PICTURE X.                                           
           02  FILLER REDEFINES PAMT003F.                                       
             03 PAMT003A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAMT003I  PIC X(12).                                             
           02  SEL0004L    COMP  PIC  S9(4).                                    
           02  SEL0004F    PICTURE X.                                           
           02  FILLER REDEFINES SEL0004F.                                       
             03 SEL0004A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  SEL0004I  PIC X(1).                                              
           02  TRNID04L    COMP  PIC  S9(4).                                    
           02  TRNID04F    PICTURE X.                                           
           02  FILLER REDEFINES TRNID04F.                                       
             03 TRNID04A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TRNID04I  PIC X(16).                                             
           02  PDATE04L    COMP  PIC  S9(4).                                    
           02  PDATE04F    PICTURE X.                                           
           02  FILLER REDEFINES PDATE04F.                                       
             03 PDATE04A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PDATE04I  PIC X(8).                                              
           02  PTIME04L    COMP  PIC  S9(4).                                    
           02  PTIME04F    PICTURE X.                                           
           02  FILLER REDEFINES PTIME04F.                                       
             03 PTIME04A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTIME04I  PIC X(8).                                              
           02  PTYPE04L    COMP  PIC  S9(4).                                    
           02  PTYPE04F    PICTURE X.                                           
           02  FILLER REDEFINES PTYPE04F.                                       
             03 PTYPE04A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTYPE04I  PIC X(4).                                              
           02  PAPRV04L    COMP  PIC  S9(4).                                    
           02  PAPRV04F    PICTURE X.                                           
           02  FILLER REDEFINES PAPRV04F.                                       
             03 PAPRV04A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAPRV04I  PIC X(1).                                              
           02  PSTAT04L    COMP  PIC  S9(4).                                    
           02  PSTAT04F    PICTURE X.                                           
           02  FILLER REDEFINES PSTAT04F.                                       
             03 PSTAT04A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PSTAT04I  PIC X(1).                                              
           02  PAMT004L    COMP  PIC  S9(4).                                    
           02  PAMT004F    PICTURE X.                                           
           02  FILLER REDEFINES PAMT004F.                                       
             03 PAMT004A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAMT004I  PIC X(12).                                             
           02  TRNID05L    COMP  PIC  S9(4).                                    
           02  TRNID05F    PICTURE X.                                           
           02  FILLER REDEFINES TRNID05F.                                       
             03 TRNID05A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  TRNID05I  PIC X(16).                                             
           02  PDATE05L    COMP  PIC  S9(4).                                    
           02  PDATE05F    PICTURE X.                                           
           02  FILLER REDEFINES PDATE05F.                                       
             03 PDATE05A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PDATE05I  PIC X(8).                                              
           02  PTIME05L    COMP  PIC  S9(4).                                    
           02  PTIME05F    PICTURE X.                                           
           02  FILLER REDEFINES PTIME05F.                                       
             03 PTIME05A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTIME05I  PIC X(8).                                              
           02  PTYPE05L    COMP  PIC  S9(4).                                    
           02  PTYPE05F    PICTURE X.                                           
           02  FILLER REDEFINES PTYPE05F.                                       
             03 PTYPE05A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PTYPE05I  PIC X(4).                                              
           02  PAPRV05L    COMP  PIC  S9(4).                                    
           02  PAPRV05F    PICTURE X.                                           
           02  FILLER REDEFINES PAPRV05F.                                       
             03 PAPRV05A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAPRV05I  PIC X(1).                                              
           02  PSTAT05L    COMP  PIC  S9(4).                                    
           02  PSTAT05F    PICTURE X.                                           
           02  FILLER REDEFINES PSTAT05F.                                       
             03 PSTAT05A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PSTAT05I  PIC X(1).                                              
           02  PAMT005L    COMP  PIC  S9(4).                                    
           02  PAMT005F    PICTURE X.                                           
           02  FILLER REDEFINES PAMT005F.                                       
             03 PAMT005A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  PAMT005I  PIC X(12).                                             
           02  SEL0005L    COMP  PIC  S9(4).                                    
           02  SEL0005F    PICTURE X.                                           
           02  FILLER REDEFINES SEL0005F.                                       
             03 SEL0005A    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  SEL0005I  PIC X(1).                                              
           02  ERRMSGL    COMP  PIC  S9(4).                                     
           02  ERRMSGF    PICTURE X.                                            
           02  FILLER REDEFINES ERRMSGF.                                        
             03 ERRMSGA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  ERRMSGI  PIC X(78).                                              
       01  COPAU0AO REDEFINES COPAU0AI.                                         
           02  FILLER PIC X(12).                                                
           02  FILLER PICTURE X(3).                                             
           02  TRNNAMEC    PICTURE X.                                           
           02  TRNNAMEP    PICTURE X.                                           
           02  TRNNAMEH    PICTURE X.                                           
           02  TRNNAMEV    PICTURE X.                                           
           02  TRNNAMEO  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  TITLE01C    PICTURE X.                                           
           02  TITLE01P    PICTURE X.                                           
           02  TITLE01H    PICTURE X.                                           
           02  TITLE01V    PICTURE X.                                           
           02  TITLE01O  PIC X(40).                                             
           02  FILLER PICTURE X(3).                                             
           02  CURDATEC    PICTURE X.                                           
           02  CURDATEP    PICTURE X.                                           
           02  CURDATEH    PICTURE X.                                           
           02  CURDATEV    PICTURE X.                                           
           02  CURDATEO  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PGMNAMEC    PICTURE X.                                           
           02  PGMNAMEP    PICTURE X.                                           
           02  PGMNAMEH    PICTURE X.                                           
           02  PGMNAMEV    PICTURE X.                                           
           02  PGMNAMEO  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  TITLE02C    PICTURE X.                                           
           02  TITLE02P    PICTURE X.                                           
           02  TITLE02H    PICTURE X.                                           
           02  TITLE02V    PICTURE X.                                           
           02  TITLE02O  PIC X(40).                                             
           02  FILLER PICTURE X(3).                                             
           02  CURTIMEC    PICTURE X.                                           
           02  CURTIMEP    PICTURE X.                                           
           02  CURTIMEH    PICTURE X.                                           
           02  CURTIMEV    PICTURE X.                                           
           02  CURTIMEO  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  ACCTIDC    PICTURE X.                                            
           02  ACCTIDP    PICTURE X.                                            
           02  ACCTIDH    PICTURE X.                                            
           02  ACCTIDV    PICTURE X.                                            
           02  ACCTIDO  PIC X(11).                                              
           02  FILLER PICTURE X(3).                                             
           02  CNAMEC    PICTURE X.                                             
           02  CNAMEP    PICTURE X.                                             
           02  CNAMEH    PICTURE X.                                             
           02  CNAMEV    PICTURE X.                                             
           02  CNAMEO  PIC X(25).                                               
           02  FILLER PICTURE X(3).                                             
           02  CUSTIDC    PICTURE X.                                            
           02  CUSTIDP    PICTURE X.                                            
           02  CUSTIDH    PICTURE X.                                            
           02  CUSTIDV    PICTURE X.                                            
           02  CUSTIDO  PIC X(9).                                               
           02  FILLER PICTURE X(3).                                             
           02  ADDR001C    PICTURE X.                                           
           02  ADDR001P    PICTURE X.                                           
           02  ADDR001H    PICTURE X.                                           
           02  ADDR001V    PICTURE X.                                           
           02  ADDR001O  PIC X(25).                                             
           02  FILLER PICTURE X(3).                                             
           02  ACCSTATC    PICTURE X.                                           
           02  ACCSTATP    PICTURE X.                                           
           02  ACCSTATH    PICTURE X.                                           
           02  ACCSTATV    PICTURE X.                                           
           02  ACCSTATO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ADDR002C    PICTURE X.                                           
           02  ADDR002P    PICTURE X.                                           
           02  ADDR002H    PICTURE X.                                           
           02  ADDR002V    PICTURE X.                                           
           02  ADDR002O  PIC X(25).                                             
           02  FILLER PICTURE X(3).                                             
           02  PHONE1C    PICTURE X.                                            
           02  PHONE1P    PICTURE X.                                            
           02  PHONE1H    PICTURE X.                                            
           02  PHONE1V    PICTURE X.                                            
           02  PHONE1O  PIC X(13).                                              
           02  FILLER PICTURE X(3).                                             
           02  APPRCNTC    PICTURE X.                                           
           02  APPRCNTP    PICTURE X.                                           
           02  APPRCNTH    PICTURE X.                                           
           02  APPRCNTV    PICTURE X.                                           
           02  APPRCNTO  PIC X(3).                                              
           02  FILLER PICTURE X(3).                                             
           02  DECLCNTC    PICTURE X.                                           
           02  DECLCNTP    PICTURE X.                                           
           02  DECLCNTH    PICTURE X.                                           
           02  DECLCNTV    PICTURE X.                                           
           02  DECLCNTO  PIC X(3).                                              
           02  FILLER PICTURE X(3).                                             
           02  CREDLIMC    PICTURE X.                                           
           02  CREDLIMP    PICTURE X.                                           
           02  CREDLIMH    PICTURE X.                                           
           02  CREDLIMV    PICTURE X.                                           
           02  CREDLIMO  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  CASHLIMC    PICTURE X.                                           
           02  CASHLIMP    PICTURE X.                                           
           02  CASHLIMH    PICTURE X.                                           
           02  CASHLIMV    PICTURE X.                                           
           02  CASHLIMO  PIC X(9).                                              
           02  FILLER PICTURE X(3).                                             
           02  APPRAMTC    PICTURE X.                                           
           02  APPRAMTP    PICTURE X.                                           
           02  APPRAMTH    PICTURE X.                                           
           02  APPRAMTV    PICTURE X.                                           
           02  APPRAMTO  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  CREDBALC    PICTURE X.                                           
           02  CREDBALP    PICTURE X.                                           
           02  CREDBALH    PICTURE X.                                           
           02  CREDBALV    PICTURE X.                                           
           02  CREDBALO  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  CASHBALC    PICTURE X.                                           
           02  CASHBALP    PICTURE X.                                           
           02  CASHBALH    PICTURE X.                                           
           02  CASHBALV    PICTURE X.                                           
           02  CASHBALO  PIC X(9).                                              
           02  FILLER PICTURE X(3).                                             
           02  DECLAMTC    PICTURE X.                                           
           02  DECLAMTP    PICTURE X.                                           
           02  DECLAMTH    PICTURE X.                                           
           02  DECLAMTV    PICTURE X.                                           
           02  DECLAMTO  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  SEL0001C    PICTURE X.                                           
           02  SEL0001P    PICTURE X.                                           
           02  SEL0001H    PICTURE X.                                           
           02  SEL0001V    PICTURE X.                                           
           02  SEL0001O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  TRNID01C    PICTURE X.                                           
           02  TRNID01P    PICTURE X.                                           
           02  TRNID01H    PICTURE X.                                           
           02  TRNID01V    PICTURE X.                                           
           02  TRNID01O  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  PDATE01C    PICTURE X.                                           
           02  PDATE01P    PICTURE X.                                           
           02  PDATE01H    PICTURE X.                                           
           02  PDATE01V    PICTURE X.                                           
           02  PDATE01O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTIME01C    PICTURE X.                                           
           02  PTIME01P    PICTURE X.                                           
           02  PTIME01H    PICTURE X.                                           
           02  PTIME01V    PICTURE X.                                           
           02  PTIME01O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTYPE01C    PICTURE X.                                           
           02  PTYPE01P    PICTURE X.                                           
           02  PTYPE01H    PICTURE X.                                           
           02  PTYPE01V    PICTURE X.                                           
           02  PTYPE01O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAPRV01C    PICTURE X.                                           
           02  PAPRV01P    PICTURE X.                                           
           02  PAPRV01H    PICTURE X.                                           
           02  PAPRV01V    PICTURE X.                                           
           02  PAPRV01O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PSTAT01C    PICTURE X.                                           
           02  PSTAT01P    PICTURE X.                                           
           02  PSTAT01H    PICTURE X.                                           
           02  PSTAT01V    PICTURE X.                                           
           02  PSTAT01O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAMT001C    PICTURE X.                                           
           02  PAMT001P    PICTURE X.                                           
           02  PAMT001H    PICTURE X.                                           
           02  PAMT001V    PICTURE X.                                           
           02  PAMT001O  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  SEL0002C    PICTURE X.                                           
           02  SEL0002P    PICTURE X.                                           
           02  SEL0002H    PICTURE X.                                           
           02  SEL0002V    PICTURE X.                                           
           02  SEL0002O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  TRNID02C    PICTURE X.                                           
           02  TRNID02P    PICTURE X.                                           
           02  TRNID02H    PICTURE X.                                           
           02  TRNID02V    PICTURE X.                                           
           02  TRNID02O  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  PDATE02C    PICTURE X.                                           
           02  PDATE02P    PICTURE X.                                           
           02  PDATE02H    PICTURE X.                                           
           02  PDATE02V    PICTURE X.                                           
           02  PDATE02O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTIME02C    PICTURE X.                                           
           02  PTIME02P    PICTURE X.                                           
           02  PTIME02H    PICTURE X.                                           
           02  PTIME02V    PICTURE X.                                           
           02  PTIME02O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTYPE02C    PICTURE X.                                           
           02  PTYPE02P    PICTURE X.                                           
           02  PTYPE02H    PICTURE X.                                           
           02  PTYPE02V    PICTURE X.                                           
           02  PTYPE02O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAPRV02C    PICTURE X.                                           
           02  PAPRV02P    PICTURE X.                                           
           02  PAPRV02H    PICTURE X.                                           
           02  PAPRV02V    PICTURE X.                                           
           02  PAPRV02O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PSTAT02C    PICTURE X.                                           
           02  PSTAT02P    PICTURE X.                                           
           02  PSTAT02H    PICTURE X.                                           
           02  PSTAT02V    PICTURE X.                                           
           02  PSTAT02O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAMT002C    PICTURE X.                                           
           02  PAMT002P    PICTURE X.                                           
           02  PAMT002H    PICTURE X.                                           
           02  PAMT002V    PICTURE X.                                           
           02  PAMT002O  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  SEL0003C    PICTURE X.                                           
           02  SEL0003P    PICTURE X.                                           
           02  SEL0003H    PICTURE X.                                           
           02  SEL0003V    PICTURE X.                                           
           02  SEL0003O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  TRNID03C    PICTURE X.                                           
           02  TRNID03P    PICTURE X.                                           
           02  TRNID03H    PICTURE X.                                           
           02  TRNID03V    PICTURE X.                                           
           02  TRNID03O  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  PDATE03C    PICTURE X.                                           
           02  PDATE03P    PICTURE X.                                           
           02  PDATE03H    PICTURE X.                                           
           02  PDATE03V    PICTURE X.                                           
           02  PDATE03O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTIME03C    PICTURE X.                                           
           02  PTIME03P    PICTURE X.                                           
           02  PTIME03H    PICTURE X.                                           
           02  PTIME03V    PICTURE X.                                           
           02  PTIME03O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTYPE03C    PICTURE X.                                           
           02  PTYPE03P    PICTURE X.                                           
           02  PTYPE03H    PICTURE X.                                           
           02  PTYPE03V    PICTURE X.                                           
           02  PTYPE03O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAPRV03C    PICTURE X.                                           
           02  PAPRV03P    PICTURE X.                                           
           02  PAPRV03H    PICTURE X.                                           
           02  PAPRV03V    PICTURE X.                                           
           02  PAPRV03O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PSTAT03C    PICTURE X.                                           
           02  PSTAT03P    PICTURE X.                                           
           02  PSTAT03H    PICTURE X.                                           
           02  PSTAT03V    PICTURE X.                                           
           02  PSTAT03O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAMT003C    PICTURE X.                                           
           02  PAMT003P    PICTURE X.                                           
           02  PAMT003H    PICTURE X.                                           
           02  PAMT003V    PICTURE X.                                           
           02  PAMT003O  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  SEL0004C    PICTURE X.                                           
           02  SEL0004P    PICTURE X.                                           
           02  SEL0004H    PICTURE X.                                           
           02  SEL0004V    PICTURE X.                                           
           02  SEL0004O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  TRNID04C    PICTURE X.                                           
           02  TRNID04P    PICTURE X.                                           
           02  TRNID04H    PICTURE X.                                           
           02  TRNID04V    PICTURE X.                                           
           02  TRNID04O  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  PDATE04C    PICTURE X.                                           
           02  PDATE04P    PICTURE X.                                           
           02  PDATE04H    PICTURE X.                                           
           02  PDATE04V    PICTURE X.                                           
           02  PDATE04O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTIME04C    PICTURE X.                                           
           02  PTIME04P    PICTURE X.                                           
           02  PTIME04H    PICTURE X.                                           
           02  PTIME04V    PICTURE X.                                           
           02  PTIME04O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTYPE04C    PICTURE X.                                           
           02  PTYPE04P    PICTURE X.                                           
           02  PTYPE04H    PICTURE X.                                           
           02  PTYPE04V    PICTURE X.                                           
           02  PTYPE04O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAPRV04C    PICTURE X.                                           
           02  PAPRV04P    PICTURE X.                                           
           02  PAPRV04H    PICTURE X.                                           
           02  PAPRV04V    PICTURE X.                                           
           02  PAPRV04O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PSTAT04C    PICTURE X.                                           
           02  PSTAT04P    PICTURE X.                                           
           02  PSTAT04H    PICTURE X.                                           
           02  PSTAT04V    PICTURE X.                                           
           02  PSTAT04O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAMT004C    PICTURE X.                                           
           02  PAMT004P    PICTURE X.                                           
           02  PAMT004H    PICTURE X.                                           
           02  PAMT004V    PICTURE X.                                           
           02  PAMT004O  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  TRNID05C    PICTURE X.                                           
           02  TRNID05P    PICTURE X.                                           
           02  TRNID05H    PICTURE X.                                           
           02  TRNID05V    PICTURE X.                                           
           02  TRNID05O  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  PDATE05C    PICTURE X.                                           
           02  PDATE05P    PICTURE X.                                           
           02  PDATE05H    PICTURE X.                                           
           02  PDATE05V    PICTURE X.                                           
           02  PDATE05O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTIME05C    PICTURE X.                                           
           02  PTIME05P    PICTURE X.                                           
           02  PTIME05H    PICTURE X.                                           
           02  PTIME05V    PICTURE X.                                           
           02  PTIME05O  PIC X(8).                                              
           02  FILLER PICTURE X(3).                                             
           02  PTYPE05C    PICTURE X.                                           
           02  PTYPE05P    PICTURE X.                                           
           02  PTYPE05H    PICTURE X.                                           
           02  PTYPE05V    PICTURE X.                                           
           02  PTYPE05O  PIC X(4).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAPRV05C    PICTURE X.                                           
           02  PAPRV05P    PICTURE X.                                           
           02  PAPRV05H    PICTURE X.                                           
           02  PAPRV05V    PICTURE X.                                           
           02  PAPRV05O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PSTAT05C    PICTURE X.                                           
           02  PSTAT05P    PICTURE X.                                           
           02  PSTAT05H    PICTURE X.                                           
           02  PSTAT05V    PICTURE X.                                           
           02  PSTAT05O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  PAMT005C    PICTURE X.                                           
           02  PAMT005P    PICTURE X.                                           
           02  PAMT005H    PICTURE X.                                           
           02  PAMT005V    PICTURE X.                                           
           02  PAMT005O  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  SEL0005C    PICTURE X.                                           
           02  SEL0005P    PICTURE X.                                           
           02  SEL0005H    PICTURE X.                                           
           02  SEL0005V    PICTURE X.                                           
           02  SEL0005O  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  ERRMSGC    PICTURE X.                                            
           02  ERRMSGP    PICTURE X.                                            
           02  ERRMSGH    PICTURE X.                                            
           02  ERRMSGV    PICTURE X.                                            
           02  ERRMSGO  PIC X(78).                                              
