      program fm034
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon02
      integer :: ivon03
!      COMMENT SECTION                                                   
!                                                                        
!      FM034                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM                                                              
!                INTEGER VARIABLE = ARITHMETIC EXPRESSION                
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR *, INTEGER VARIABLE AND INTEGER CONSTANT.  SOME OF THE   
!      TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE EXPRESSION AND TO  
!      ALLOW THE USE OF NEGATIVE CONSTANTS FOLLOWING THE * OPERATOR.     
!      THE INTEGER VARIABLES CONTAIN POSITIVE AND NEGATIVE VALUES.       
!                                                                        
!      THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS          
!          (1)  INTEGER VARIABLE * INTEGER CONSTANT                      
!               INTEGER CONSTANT * INTEGER VARIABLE                      
!          (2)  INTEGER CONSTANT * INTEGER VARIABLE * INTEGER CONSTANT   
!          (3)  SAME AS (2) BUT WITH PARENS TO GROUP ELEMENTS.           
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  
!                                                                        
!       **********************************************************       
!                                                                        
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  
!      PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 
!      FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     
!      VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED
!      DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   
!      PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  
!      LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 
!      OF EXECUTING THESE TESTS.                                         
!                                                                        
!          THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 
!      FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 
!                                                                        
!          SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             
!                                                                        
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!       **********************************************************       
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION                                            
!                                                                        
!      INITIALIZE CONSTANTS                                              
!       **************                                                   
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD. 
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD. 
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass=0                                                          
      ivfail=0                                                          
      ivdele=0                                                          
      iczero=0                                                          
!                                                                        
!      WRITE PAGE HEADERS                                                
      write (i02,90000)                                                 
      write (i02,90001)                                                 
      write (i02,90002)                                                 
      write (i02, 90002)                                                
      write (i02,90003)                                                 
      write (i02,90002)                                                 
      write (i02,90004)                                                 
      write (i02,90002)                                                 
      write (i02,90011)                                                 
      write (i02,90002)                                                 
      write (i02,90002)                                                 
      write (i02,90005)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!      TEST 395 THROUGH TEST 414 CONTAIN AN INTEGER VARIABLE, AN INTEGER 
!      CONSTANT, AND OPERATOR * IN AN ARITHMETIC EXPRESSION.             
!                                                                        
!      TEST 395 THROUGH TEST 406     -  IV= IV * IC                      
!                                                                        
!          TEST 395 THROUGH TEST 398                                     
!               POSITIVE INTEGER VARIABLE, POSITIVE INTEGER CONSTANT     
!                                                                        
 3951 continue                                                          
      ivtnum = 395                                                      
!                                                                        
!       ****  TEST 395  ****                                             
!                                                                        
      if (iczero) 33950, 3950, 33950                                    
 3950 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 * 3                                               
      goto 43950                                                       
33950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43950, 3961, 43950                                    
43950 if (ivcomp -6) 23950,13950,23950                                  
13950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3961                                                        
23950 ivfail = ivfail + 1                                               
      ivcorr =6                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3961 continue                                                          
      ivtnum = 396                                                      
!                                                                        
!       ****  TEST 396  ****                                             
!                                                                        
      if (iczero) 33960, 3960, 33960                                    
 3960 continue                                                          
      ivon01 = 13                                                       
      ivcomp = ivon01 * 11                                              
      goto 43960                                                       
33960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43960, 3971, 43960                                    
43960 if (ivcomp - 143) 23960,13960,23960                               
13960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3971                                                        
23960 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3971 continue                                                          
      ivtnum = 397                                                      
!                                                                        
!       ****  TEST 397  ****                                             
!                                                                        
      if (iczero) 33970, 3970, 33970                                    
 3970 continue                                                          
      ivon01 = 223                                                      
      ivcomp = ivon01 * 99                                              
      goto 43970                                                       
33970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43970, 3981, 43970                                    
43970 if (ivcomp - 22077) 23970,13970,23970                             
13970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3981                                                        
23970 ivfail = ivfail + 1                                               
      ivcorr = 22077                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3981 continue                                                          
      ivtnum = 398                                                      
!                                                                        
!       ****  TEST 398  ****                                             
!                                                                        
      if (iczero) 33980, 3980, 33980                                    
 3980 continue                                                          
      ivon01 = 11235                                                    
      ivcomp = ivon01 * 2                                               
      goto 43980                                                       
33980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43980, 3991, 43980                                    
43980 if (ivcomp - 22470) 23980,13980,23980                             
13980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3991                                                        
23980 ivfail = ivfail + 1                                               
      ivcorr = 22470                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TEST 399 THROUGH TEST 402                                     
!              NEGATIVE INTEGER VARIABLE, POSITIVE INTEGER CONSTANT      
!                                                                        
 3991 continue                                                          
      ivtnum = 399                                                      
!                                                                        
!        ****  TEST 399  ****                                            
!                                                                        
      if (iczero) 33990, 3990, 33990                                    
 3990 continue                                                          
      ivon01 = -2                                                       
      ivcomp = ivon01 * 3                                               
      goto 43990                                                       
33990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43990, 4001, 43990                                    
43990 if (ivcomp +6) 23990,13990,23990                                  
13990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4001                                                        
23990 ivfail = ivfail + 1                                               
      ivcorr = -6                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4001 continue                                                          
      ivtnum = 400                                                      
!                                                                        
!       ****  TEST 400  ****                                             
!                                                                        
      if (iczero) 34000, 4000, 34000                                    
 4000 continue                                                          
      ivon01 = -13                                                      
      ivcomp =ivon01*11                                                 
      goto 44000                                                       
34000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44000, 4011, 44000                                    
44000 if (ivcomp +143) 24000,14000,24000                                
14000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4011                                                        
24000 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4011 continue                                                          
      ivtnum = 401                                                      
!                                                                        
!        ****  TEST 401  ****                                            
!                                                                        
      if (iczero) 34010, 4010, 34010                                    
 4010 continue                                                          
      ivon01 = -223                                                     
      ivcomp = ivon01*99                                                
      goto 44010                                                       
34010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44010, 4021, 44010                                    
44010 if (ivcomp + 22077) 24010,14010,24010                             
14010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4021                                                        
24010 ivfail = ivfail + 1                                               
      ivcorr = -22077                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4021 continue                                                          
      ivtnum = 402                                                      
!                                                                        
!        ****  TEST 402  ****                                            
!                                                                        
      if (iczero) 34020, 4020, 34020                                    
 4020 continue                                                          
      ivon01 = -11235                                                   
      ivcomp = ivon01*2                                                 
      goto 44020                                                       
34020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44020, 4031, 44020                                    
44020 if (ivcomp+22470) 24020,14020,24020                               
14020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4031                                                        
24020 ivfail = ivfail + 1                                               
      ivcorr = -22470                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!           TEST 403 AND TEST 404                                        
!               NEGATIVE INTEGER VARIABLE, NEGATIVE INTEGER CONSTANT     
!                                                                        
 4031 continue                                                          
      ivtnum = 403                                                      
!                                                                        
!        ****  TEST 403  ****                                            
!                                                                        
      if (iczero) 34030, 4030, 34030                                    
 4030 continue                                                          
      ivon01=-2                                                         
      ivcomp = ivon01*(-3)                                              
      goto 44030                                                       
34030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44030, 4041, 44030                                    
44030 if (ivcomp -6) 24030,14030,24030                                  
14030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4041                                                        
24030 ivfail = ivfail + 1                                               
      ivcorr =6                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4041 continue                                                          
      ivtnum = 404                                                      
!                                                                        
!        ****  TEST 404  ****                                            
!                                                                        
      if (iczero) 34040, 4040, 34040                                    
 4040 continue                                                          
      ivon01 = -13                                                      
      ivcomp = ivon01 * (-11)                                           
      goto 44040                                                       
34040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44040, 4051, 44040                                    
44040 if (ivcomp -143) 24040,14040,24040                                
14040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4051                                                        
24040 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!           TEST 405 AND TEST 406                                        
!               POSITIVE INTEGER VARIABLE, NEGATIVE INTEGER CONSTANT     
!                                                                        
 4051 continue                                                          
      ivtnum = 405                                                      
!                                                                        
!        ****  TEST 405  ****                                            
!                                                                        
      if (iczero) 34050, 4050, 34050                                    
 4050 continue                                                          
      ivon01 = 223                                                      
      ivcomp = ivon01 * (-99)                                           
      goto 44050                                                       
34050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44050, 4061, 44050                                    
44050 if (ivcomp + 22077) 24050,14050,24050                             
14050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4061                                                        
24050 ivfail = ivfail + 1                                               
      ivcorr = -22077                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4061 continue                                                          
      ivtnum = 406                                                      
!                                                                        
!        ****  TEST 406  ****                                            
!                                                                        
      if (iczero) 34060, 4060, 34060                                    
 4060 continue                                                          
      ivon01 = 11235                                                    
      ivcomp = ivon01 * (-2)                                            
      goto 44060                                                       
34060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44060, 4071, 44060                                    
44060 if (ivcomp + 22470) 24060,14060,24060                             
14060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4071                                                        
24060 ivfail = ivfail + 1                                               
      ivcorr = -22470                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!       TEST 407 THROUGH TEST 414    -   IV = IC * IV                    
!                                                                        
!           TEST 407 AND TEST 408                                        
!                POSITIVE INTEGER CONSTANT, POSITIVE INTEGER VARIABLE    
!                                                                        
 4071 continue                                                          
      ivtnum = 407                                                      
!                                                                        
!        ****  TEST 407  ****                                            
!                                                                        
      if (iczero) 34070, 4070, 34070                                    
 4070 continue                                                          
      ivon02 = 11                                                       
      ivcomp = 13*ivon02                                                
      goto 44070                                                       
34070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44070, 4081, 44070                                    
44070 if (ivcomp - 143) 24070,14070,24070                               
14070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4081                                                        
24070 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4081 continue                                                          
      ivtnum = 408                                                      
!                                                                        
!        ****  TEST 408  ****                                            
!                                                                        
      if (iczero) 34080, 4080, 34080                                    
 4080 continue                                                          
      ivon02 = +11                                                      
      ivcomp = +13 * ivon02                                             
      goto 44080                                                       
34080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44080, 4091, 44080                                    
44080 if (ivcomp - 143) 24080,14080,24080                               
14080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4091                                                        
24080 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!           TEST 409 AND TEST 410                                        
!                POSITIVE INTEGER CONSTANT, NEGATIVE INTEGER VARIABLE    
!                                                                        
 4091 continue                                                          
      ivtnum = 409                                                      
!                                                                        
!        ****  TEST 409  ****                                            
!                                                                        
      if (iczero) 34090, 4090, 34090                                    
 4090 continue                                                          
      ivon02 = -99                                                      
      ivcomp = 223 * ivon02                                             
      goto 44090                                                       
34090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44090, 4101, 44090                                    
44090 if (ivcomp + 22077) 24090,14090,24090                             
14090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4101                                                        
24090 ivfail = ivfail + 1                                               
      ivcorr =-22077                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4101 continue                                                          
      ivtnum = 410                                                      
!                                                                        
!        ****  TEST 410  ****                                            
!                                                                        
      if (iczero) 34100, 4100, 34100                                    
 4100 continue                                                          
      ivon02 = -99                                                      
      ivcomp = +223*ivon02                                              
      goto 44100                                                       
34100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44100, 4111, 44100                                    
44100 if (ivcomp + 22077) 24100,14100,24100                             
14100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4111                                                        
24100 ivfail = ivfail + 1                                               
      ivcorr = -22077                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!           TEST 411 AND TEST 412                                        
!               NEGATIVE INTEGER CONSTANT, POSITIVE INTEGER VARIABLE     
!                                                                        
 4111 continue                                                          
      ivtnum = 411                                                      
!                                                                        
!        ****  TEST 411  ****                                            
!                                                                        
      if (iczero) 34110, 4110, 34110                                    
 4110 continue                                                          
      ivon02 = 2                                                        
      ivcomp = (-11235) * ivon02                                        
      goto 44110                                                       
34110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44110, 4121, 44110                                    
44110 if (ivcomp + 22470) 24110,14110,24110                             
14110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4121                                                        
24110 ivfail = ivfail + 1                                               
      ivcorr = -22470                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4121 continue                                                          
      ivtnum = 412                                                      
!                                                                        
!        ****  TEST 412  ****                                            
!                                                                        
      if (iczero) 34120, 4120, 34120                                    
 4120 continue                                                          
      ivon02 = +2                                                       
      ivcomp = -11235 * ivon02                                          
      goto 44120                                                       
34120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44120, 4131, 44120                                    
44120 if (ivcomp + 22470) 24120,14120,24120                             
14120 ivpass=ivpass + 1                                                 
      write (i02,80001) ivtnum                                          
      goto 4131                                                        
24120 ivfail = ivfail + 1                                               
      ivcorr = -22470                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!           TEST 413 AND TEST 414                                        
!                 NEGATIVE INTEGER CONSTANT, NEGATIVE INTEGER VARIABLE   
!                                                                        
 4131 continue                                                          
      ivtnum = 413                                                      
!                                                                        
!        ****  TEST 413  ****                                            
!                                                                        
      if (iczero) 34130, 4130, 34130                                    
 4130 continue                                                          
      ivon02 = -3                                                       
      ivcomp = (-2) * ivon02                                            
      goto 44130                                                       
34130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44130, 4141, 44130                                    
44130 if (ivcomp - 6) 24130,14130,24130                                 
14130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4141                                                        
24130 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4141 continue                                                          
      ivtnum = 414                                                      
!                                                                        
!        ****  TEST 414  ****                                            
!                                                                        
      if (iczero) 34140, 4140, 34140                                    
 4140 continue                                                          
      ivon02 = -3                                                       
      ivcomp = -2 * ivon02                                              
      goto 44140                                                       
34140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44140, 4151, 44140                                    
44140 if (ivcomp - 6) 24140,14140,24140                                 
14140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4151                                                        
24140 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!       TEST 415  THROUGH TEST 429 CONTAIN TWO INTEGER CONSTANTS,        
!       ONE INTEGER VARIABLE AND OPERATOR * IN ARITHMETIC EXPRESSION.    
!                                                                        
 4151 continue                                                          
      ivtnum = 415                                                      
!                                                                        
!        ****  TEST 415  ****                                            
!                                                                        
      if (iczero) 34150, 4150, 34150                                    
 4150 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 * 3 * 4                                           
      goto 44150                                                       
34150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44150, 4161, 44150                                    
44150 if (ivcomp - 24) 24150,14150,24150                                
14150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4161                                                        
24150 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4161 continue                                                          
      ivtnum = 416                                                      
!                                                                        
!        ****  TEST 416  ****                                            
!                                                                        
      if (iczero) 34160, 4160, 34160                                    
 4160 continue                                                          
      ivon01 = -2                                                       
      ivcomp = ivon01 *3*4                                              
      goto 44160                                                       
34160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44160, 4171, 44160                                    
44160 if (ivcomp +24) 24160,14160,24160                                 
14160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4171                                                        
24160 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4171 continue                                                          
      ivtnum = 417                                                      
!                                                                        
!        ****  TEST 417  ****                                            
!                                                                        
      if (iczero) 34170, 4170, 34170                                    
 4170 continue                                                          
      ivon01 = -2                                                       
      ivcomp = ivon01*3*(-4)                                            
      goto 44170                                                       
34170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44170, 4181, 44170                                    
44170 if (ivcomp -24) 24170,14170,24170                                 
14170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4181                                                        
24170 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4181 continue                                                          
      ivtnum = 418                                                      
!                                                                        
!        ****  TEST 418  ****                                            
!                                                                        
      if (iczero) 34180, 4180, 34180                                    
 4180 continue                                                          
      ivon01 = -2                                                       
      ivcomp = ivon01*(-3)*(-4)                                         
      goto 44180                                                       
34180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44180, 4191, 44180                                    
44180 if (ivcomp +24) 24180,14180,24180                                 
14180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4191                                                        
24180 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4191 continue                                                          
      ivtnum = 419                                                      
!                                                                        
!        ****  TEST 419  ****                                            
!                                                                        
      if (iczero) 34190, 4190, 34190                                    
 4190 continue                                                          
      ivon02 = 51                                                       
      ivcomp = 23*ivon02*13                                             
      goto 44190                                                       
34190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44190, 4201, 44190                                    
44190 if (ivcomp-15249) 24190,14190,24190                               
14190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4201                                                        
24190 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4201 continue                                                          
      ivtnum = 420                                                      
!                                                                        
!        ****  TEST 420  ****                                            
!                                                                        
      if (iczero) 34200, 4200, 34200                                    
 4200 continue                                                          
      ivon02 = -51                                                      
      ivcomp = 23*ivon02*(-13)                                          
      goto 44200                                                       
34200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44200, 4211, 44200                                    
44200 if (ivcomp - 15249) 24200,14200,24200                             
14200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4211                                                        
24200 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4211 continue                                                          
      ivtnum = 421                                                      
!                                                                        
!        ****  TEST 421  ****                                            
!                                                                        
      if (iczero) 34210, 4210, 34210                                    
 4210 continue                                                          
      ivon02 = -51                                                      
      ivcomp = 23*ivon02*13                                             
      goto 44210                                                       
34210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44210, 4221, 44210                                    
44210 if (ivcomp+15249) 24210,14210,24210                               
14210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4221                                                        
24210 ivfail = ivfail + 1                                               
      ivcorr = -15249                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4221 continue                                                          
      ivtnum = 422                                                      
!                                                                        
!        ****  TEST 422  ****                                            
!                                                                        
      if (iczero) 34220, 4220, 34220                                    
 4220 continue                                                          
      ivon02 = -51                                                      
      ivcomp =(-23)*ivon02*(-13)                                        
      goto 44220                                                       
34220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44220, 4231, 44220                                    
44220 if (ivcomp+15249) 24220,14220,24220                               
14220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4231                                                        
24220 ivfail = ivfail + 1                                               
      ivcorr = -15249                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4231 continue                                                          
      ivtnum = 423                                                      
!                                                                        
!        ****  TEST 423  ****                                            
!                                                                        
      if (iczero) 34230, 4230, 34230                                    
 4230 continue                                                          
      ivon03 = 5461                                                     
      ivcomp = 2*3*ivon03                                               
      goto 44230                                                       
34230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44230, 4241, 44230                                    
44230 if (ivcomp - 32766) 24230,14230,24230                             
14230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4241                                                        
24230 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4241 continue                                                          
      ivtnum = 424                                                      
!                                                                        
!        ****  TEST 424  ****                                            
!                                                                        
      if (iczero) 34240, 4240, 34240                                    
 4240 continue                                                          
      ivon03 = -5461                                                    
      ivcomp = 2*3*ivon03                                               
      goto 44240                                                       
34240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44240, 4251, 44240                                    
44240 if (ivcomp +32766) 24240,14240,24240                              
14240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4251                                                        
24240 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4251 continue                                                          
      ivtnum = 425                                                      
!                                                                        
!        ****  TEST 425  ****                                            
!                                                                        
      if (iczero) 34250, 4250, 34250                                    
 4250 continue                                                          
      ivon03 = -5461                                                    
      ivcomp = -2*3*ivon03                                              
      goto 44250                                                       
34250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44250, 4261, 44250                                    
44250 if (ivcomp - 32766) 24250,14250,24250                             
14250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4261                                                        
24250 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!       TEST 426 THROUGH TEST 429 USE PARENTHESES TO GROUP ELEMENTS      
!       IN ARITHMETIC EXPRESSION.                                        
!                                                                        
 4261 continue                                                          
      ivtnum = 426                                                      
!                                                                        
!        ****  TEST 426  ****                                            
!                                                                        
      if (iczero) 34260, 4260, 34260                                    
 4260 continue                                                          
      ivon02 = 51                                                       
      ivcomp = (23*ivon02)*13                                           
      goto 44260                                                       
34260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44260, 4271, 44260                                    
44260 if (ivcomp -15249) 24260,14260,24260                              
14260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4271                                                        
24260 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4271 continue                                                          
      ivtnum = 427                                                      
!                                                                        
!        ****  TEST 427  ****                                            
!                                                                        
      if (iczero) 34270, 4270, 34270                                    
 4270 continue                                                          
      ivon02 = 51                                                       
      ivcomp = 23*(ivon02*13)                                           
      goto 44270                                                       
34270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44270, 4281, 44270                                    
44270 if (ivcomp-15249) 24270,14270,24270                               
14270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4281                                                        
24270 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4281 continue                                                          
      ivtnum = 428                                                      
!                                                                        
!        ****  TEST 428  ****                                            
!                                                                        
      if (iczero) 34280, 4280, 34280                                    
 4280 continue                                                          
      ivon02 = -51                                                      
      ivcomp = -23 * (ivon02*(+13))                                     
      goto 44280                                                       
34280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44280, 4291, 44280                                    
44280 if (ivcomp - 15249)24280,14280,24280                              
14280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4291                                                        
24280 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4291 continue                                                          
      ivtnum = 429                                                      
!                                                                        
!        ****  TEST 429  ****                                            
!                                                                        
      if (iczero) 34290, 4290, 34290                                    
 4290 continue                                                          
      ivon02 = -51                                                      
      ivcomp = (-23)*(ivon02*(-13))                                     
      goto 44290                                                       
34290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44290, 4301, 44290                                    
44290 if (ivcomp + 15249) 24290,14290,24290                             
14290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4301                                                        
24290 ivfail = ivfail + 1                                               
      ivcorr = -15249                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!      ****   END OF TESTS   ****                                        
 4301 continue                                                          
!                                                                        
!      WRITE PAGE FOOTINGS AND RUN SUMMARIES                             
99999 continue                                                          
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
      write (i02,90002)                                                 
      write (i02,90007)                                                 
      write (i02,90002)                                                 
      write (i02,90008)  ivfail                                         
      write (i02,90009) ivpass                                          
      write (i02,90010) ivdele                                          
!                                                                        
!                                                                        
!      TERMINATE ROUTINE EXECUTION                                       
      stop                                                              
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
90000 format ("1")                                                      
90002 format (" ")                                                      
90001 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90003 format (" ",21x,"VERSION 2.1" )                                   
90004 format (" ",10x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )        
90005 format (" ",5x,"TEST",5x,"PASS/FAIL", 5x,"COMPUTED",8x,"CORRECT") 
90006 format (" ",5x,"----------------------------------------------" ) 
90011 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARIES                               
90008 format (" ",15x,i5," ERRORS ENCOUNTERED" )                        
90009 format (" ",15x,i5," TESTS PASSED" )                              
90010 format (" ",15x,i5," TESTS DELETED" )                             
!                                                                        
!      FORMAT STATEMENTS FOR TEST RESULTS                                
80001 format (" ",4x,i5,7x,"PASS")                                      
80002 format (" ",4x,i5,7x,"FAIL")                                      
80003 format (" ",4x,i5,7x,"DELETED")                                   
80004 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80005 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
!                                                                        
90007 format (" ",20x,"END OF PROGRAM FM034" )                          
      end program fm034
