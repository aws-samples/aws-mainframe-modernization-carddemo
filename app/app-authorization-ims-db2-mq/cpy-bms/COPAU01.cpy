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
       01  COPAU1AI.
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
           02  CARDNUML    COMP  PIC  S9(4).                                    
           02  CARDNUMF    PICTURE X.                                           
           02  FILLER REDEFINES CARDNUMF.                                       
             03 CARDNUMA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  CARDNUMI  PIC X(16).                                             
           02  AUTHDTL    COMP  PIC  S9(4).                                     
           02  AUTHDTF    PICTURE X.                                            
           02  FILLER REDEFINES AUTHDTF.                                        
             03 AUTHDTA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  AUTHDTI  PIC X(10).                                              
           02  AUTHTML    COMP  PIC  S9(4).                                     
           02  AUTHTMF    PICTURE X.                                            
           02  FILLER REDEFINES AUTHTMF.                                        
             03 AUTHTMA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  AUTHTMI  PIC X(10).                                              
           02  AUTHRSPL    COMP  PIC  S9(4).                                    
           02  AUTHRSPF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHRSPF.                                       
             03 AUTHRSPA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHRSPI  PIC X(1).                                              
           02  AUTHRSNL    COMP  PIC  S9(4).                                    
           02  AUTHRSNF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHRSNF.                                       
             03 AUTHRSNA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHRSNI  PIC X(20).                                             
           02  AUTHCDL    COMP  PIC  S9(4).                                     
           02  AUTHCDF    PICTURE X.                                            
           02  FILLER REDEFINES AUTHCDF.                                        
             03 AUTHCDA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  AUTHCDI  PIC X(6).                                               
           02  AUTHAMTL    COMP  PIC  S9(4).                                    
           02  AUTHAMTF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHAMTF.                                       
             03 AUTHAMTA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHAMTI  PIC X(12).                                             
           02  POSEMDL    COMP  PIC  S9(4).                                     
           02  POSEMDF    PICTURE X.                                            
           02  FILLER REDEFINES POSEMDF.                                        
             03 POSEMDA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  POSEMDI  PIC X(4).                                               
           02  AUTHSRCL    COMP  PIC  S9(4).                                    
           02  AUTHSRCF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHSRCF.                                       
             03 AUTHSRCA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHSRCI  PIC X(10).                                             
           02  MCCCDL    COMP  PIC  S9(4).                                      
           02  MCCCDF    PICTURE X.                                             
           02  FILLER REDEFINES MCCCDF.                                         
             03 MCCCDA    PICTURE X.                                            
           02  FILLER   PICTURE X(4).                                           
           02  MCCCDI  PIC X(4).                                                
           02  CRDEXPL    COMP  PIC  S9(4).                                     
           02  CRDEXPF    PICTURE X.                                            
           02  FILLER REDEFINES CRDEXPF.                                        
             03 CRDEXPA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  CRDEXPI  PIC X(5).                                               
           02  AUTHTYPL    COMP  PIC  S9(4).                                    
           02  AUTHTYPF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHTYPF.                                       
             03 AUTHTYPA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHTYPI  PIC X(14).                                             
           02  TRNIDL    COMP  PIC  S9(4).                                      
           02  TRNIDF    PICTURE X.                                             
           02  FILLER REDEFINES TRNIDF.                                         
             03 TRNIDA    PICTURE X.                                            
           02  FILLER   PICTURE X(4).                                           
           02  TRNIDI  PIC X(15).                                               
           02  AUTHMTCL    COMP  PIC  S9(4).                                    
           02  AUTHMTCF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHMTCF.                                       
             03 AUTHMTCA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHMTCI  PIC X(1).                                              
           02  AUTHFRDL    COMP  PIC  S9(4).                                    
           02  AUTHFRDF    PICTURE X.                                           
           02  FILLER REDEFINES AUTHFRDF.                                       
             03 AUTHFRDA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  AUTHFRDI  PIC X(10).                                             
           02  MERNAMEL    COMP  PIC  S9(4).                                    
           02  MERNAMEF    PICTURE X.                                           
           02  FILLER REDEFINES MERNAMEF.                                       
             03 MERNAMEA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  MERNAMEI  PIC X(25).                                             
           02  MERIDL    COMP  PIC  S9(4).                                      
           02  MERIDF    PICTURE X.                                             
           02  FILLER REDEFINES MERIDF.                                         
             03 MERIDA    PICTURE X.                                            
           02  FILLER   PICTURE X(4).                                           
           02  MERIDI  PIC X(15).                                               
           02  MERCITYL    COMP  PIC  S9(4).                                    
           02  MERCITYF    PICTURE X.                                           
           02  FILLER REDEFINES MERCITYF.                                       
             03 MERCITYA    PICTURE X.                                          
           02  FILLER   PICTURE X(4).                                           
           02  MERCITYI  PIC X(25).                                             
           02  MERSTL    COMP  PIC  S9(4).                                      
           02  MERSTF    PICTURE X.                                             
           02  FILLER REDEFINES MERSTF.                                         
             03 MERSTA    PICTURE X.                                            
           02  FILLER   PICTURE X(4).                                           
           02  MERSTI  PIC X(2).                                                
           02  MERZIPL    COMP  PIC  S9(4).                                     
           02  MERZIPF    PICTURE X.                                            
           02  FILLER REDEFINES MERZIPF.                                        
             03 MERZIPA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  MERZIPI  PIC X(10).                                              
           02  ERRMSGL    COMP  PIC  S9(4).                                     
           02  ERRMSGF    PICTURE X.                                            
           02  FILLER REDEFINES ERRMSGF.                                        
             03 ERRMSGA    PICTURE X.                                           
           02  FILLER   PICTURE X(4).                                           
           02  ERRMSGI  PIC X(78).                                              
       01  COPAU1AO REDEFINES COPAU1AI.                                         
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
           02  CARDNUMC    PICTURE X.                                           
           02  CARDNUMP    PICTURE X.                                           
           02  CARDNUMH    PICTURE X.                                           
           02  CARDNUMV    PICTURE X.                                           
           02  CARDNUMO  PIC X(16).                                             
           02  FILLER PICTURE X(3).                                             
           02  AUTHDTC    PICTURE X.                                            
           02  AUTHDTP    PICTURE X.                                            
           02  AUTHDTH    PICTURE X.                                            
           02  AUTHDTV    PICTURE X.                                            
           02  AUTHDTO  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  AUTHTMC    PICTURE X.                                            
           02  AUTHTMP    PICTURE X.                                            
           02  AUTHTMH    PICTURE X.                                            
           02  AUTHTMV    PICTURE X.                                            
           02  AUTHTMO  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  AUTHRSPC    PICTURE X.                                           
           02  AUTHRSPP    PICTURE X.                                           
           02  AUTHRSPH    PICTURE X.                                           
           02  AUTHRSPV    PICTURE X.                                           
           02  AUTHRSPO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  AUTHRSNC    PICTURE X.                                           
           02  AUTHRSNP    PICTURE X.                                           
           02  AUTHRSNH    PICTURE X.                                           
           02  AUTHRSNV    PICTURE X.                                           
           02  AUTHRSNO  PIC X(20).                                             
           02  FILLER PICTURE X(3).                                             
           02  AUTHCDC    PICTURE X.                                            
           02  AUTHCDP    PICTURE X.                                            
           02  AUTHCDH    PICTURE X.                                            
           02  AUTHCDV    PICTURE X.                                            
           02  AUTHCDO  PIC X(6).                                               
           02  FILLER PICTURE X(3).                                             
           02  AUTHAMTC    PICTURE X.                                           
           02  AUTHAMTP    PICTURE X.                                           
           02  AUTHAMTH    PICTURE X.                                           
           02  AUTHAMTV    PICTURE X.                                           
           02  AUTHAMTO  PIC X(12).                                             
           02  FILLER PICTURE X(3).                                             
           02  POSEMDC    PICTURE X.                                            
           02  POSEMDP    PICTURE X.                                            
           02  POSEMDH    PICTURE X.                                            
           02  POSEMDV    PICTURE X.                                            
           02  POSEMDO  PIC X(4).                                               
           02  FILLER PICTURE X(3).                                             
           02  AUTHSRCC    PICTURE X.                                           
           02  AUTHSRCP    PICTURE X.                                           
           02  AUTHSRCH    PICTURE X.                                           
           02  AUTHSRCV    PICTURE X.                                           
           02  AUTHSRCO  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  MCCCDC    PICTURE X.                                             
           02  MCCCDP    PICTURE X.                                             
           02  MCCCDH    PICTURE X.                                             
           02  MCCCDV    PICTURE X.                                             
           02  MCCCDO  PIC X(4).                                                
           02  FILLER PICTURE X(3).                                             
           02  CRDEXPC    PICTURE X.                                            
           02  CRDEXPP    PICTURE X.                                            
           02  CRDEXPH    PICTURE X.                                            
           02  CRDEXPV    PICTURE X.                                            
           02  CRDEXPO  PIC X(5).                                               
           02  FILLER PICTURE X(3).                                             
           02  AUTHTYPC    PICTURE X.                                           
           02  AUTHTYPP    PICTURE X.                                           
           02  AUTHTYPH    PICTURE X.                                           
           02  AUTHTYPV    PICTURE X.                                           
           02  AUTHTYPO  PIC X(14).                                             
           02  FILLER PICTURE X(3).                                             
           02  TRNIDC    PICTURE X.                                             
           02  TRNIDP    PICTURE X.                                             
           02  TRNIDH    PICTURE X.                                             
           02  TRNIDV    PICTURE X.                                             
           02  TRNIDO  PIC X(15).                                               
           02  FILLER PICTURE X(3).                                             
           02  AUTHMTCC    PICTURE X.                                           
           02  AUTHMTCP    PICTURE X.                                           
           02  AUTHMTCH    PICTURE X.                                           
           02  AUTHMTCV    PICTURE X.                                           
           02  AUTHMTCO  PIC X(1).                                              
           02  FILLER PICTURE X(3).                                             
           02  AUTHFRDC    PICTURE X.                                           
           02  AUTHFRDP    PICTURE X.                                           
           02  AUTHFRDH    PICTURE X.                                           
           02  AUTHFRDV    PICTURE X.                                           
           02  AUTHFRDO  PIC X(10).                                             
           02  FILLER PICTURE X(3).                                             
           02  MERNAMEC    PICTURE X.                                           
           02  MERNAMEP    PICTURE X.                                           
           02  MERNAMEH    PICTURE X.                                           
           02  MERNAMEV    PICTURE X.                                           
           02  MERNAMEO  PIC X(25).                                             
           02  FILLER PICTURE X(3).                                             
           02  MERIDC    PICTURE X.                                             
           02  MERIDP    PICTURE X.                                             
           02  MERIDH    PICTURE X.                                             
           02  MERIDV    PICTURE X.                                             
           02  MERIDO  PIC X(15).                                               
           02  FILLER PICTURE X(3).                                             
           02  MERCITYC    PICTURE X.                                           
           02  MERCITYP    PICTURE X.                                           
           02  MERCITYH    PICTURE X.                                           
           02  MERCITYV    PICTURE X.                                           
           02  MERCITYO  PIC X(25).                                             
           02  FILLER PICTURE X(3).                                             
           02  MERSTC    PICTURE X.                                             
           02  MERSTP    PICTURE X.                                             
           02  MERSTH    PICTURE X.                                             
           02  MERSTV    PICTURE X.                                             
           02  MERSTO  PIC X(2).                                                
           02  FILLER PICTURE X(3).                                             
           02  MERZIPC    PICTURE X.                                            
           02  MERZIPP    PICTURE X.                                            
           02  MERZIPH    PICTURE X.                                            
           02  MERZIPV    PICTURE X.                                            
           02  MERZIPO  PIC X(10).                                              
           02  FILLER PICTURE X(3).                                             
           02  ERRMSGC    PICTURE X.                                            
           02  ERRMSGP    PICTURE X.                                            
           02  ERRMSGH    PICTURE X.                                            
           02  ERRMSGV    PICTURE X.                                            
           02  ERRMSGO  PIC X(78).                                              
