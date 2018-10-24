      program fm040
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivon02
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon03
!      COMMENT SECTION                                                   
!                                                                        
!      FM040                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!          FORM      INTEGER VARIABLE = ARITHMETIC EXPRESSION            
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR /, INTEGER VARIABLES AND AN INTEGER CONSTANT.  BOTH      
!      POSITIVE AND NEGATIVE VALUES ARE USED FOR THE INTEGER VARIABLES   
!      AND THE INTEGER CONSTANT.                                         
!                                                                        
!          THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT AND 
!      TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED IN   
!      THE RESULTANT INTEGER VARIABLE.  SOME OF THE TESTS USE PARENS     
!      TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.                   
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!              (1) INTEGER VARIABLE/INTEGER VARIABLE                     
!              (2) INTEGER VARIABLE/INTEGER VARIABLE/INTEGER CONSTANT    
!                  INTEGER VARIABLE/INTEGER CONSTANT/INTEGER VARIABLE    
!                  INTEGER CONSTANT/INTEGER VARIABLE/INTEGER VARIABLE    
!              (3) SAME AS (2) BUT WITH PARENTHESES TO GROUP ELEMENTS    
!                    IN THE ARITHMETIC EXPRESSION.                       
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 6.6, EVALUATION OF EXPRESSIONS                         
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
!      TEST 582 THROUGH TEST 597 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM       INTEGER VARIABLE=INTEGER VARIABLE/INTEGER VAR.  
!                                                                        
!      TEST 582 THROUGH TEST 585 - POSITIVE VALUES                       
!                    NO TRUNCATION REQUIRED                              
!                                                                        
 5821 continue                                                          
      ivtnum = 582                                                      
!                                                                        
!       ****  TEST 582  ****                                             
!                                                                        
      if (iczero) 35820, 5820, 35820                                    
 5820 continue                                                          
      ivon01 = 4                                                        
      ivon02 = 2                                                        
      ivcomp = ivon01 / ivon02                                          
      goto 45820                                                       
35820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45820, 5831, 45820                                    
45820 if (ivcomp -2) 25820,15820,25820                                  
15820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5831                                                        
25820 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5831 continue                                                          
      ivtnum = 583                                                      
!                                                                        
!       ****  TEST 583  ****                                             
!                                                                        
      if (iczero) 35830, 5830, 35830                                    
 5830 continue                                                          
      ivon01 = 3575                                                     
      ivon02 = 25                                                       
      ivcomp = ivon01/ivon02                                            
      goto 45830                                                       
35830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45830, 5841, 45830                                    
45830 if (ivcomp - 143) 25830,15830,25830                               
15830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5841                                                        
25830 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5841 continue                                                          
      ivtnum = 584                                                      
!                                                                        
!       ****  TEST 584  ****                                             
!                                                                        
      if (iczero) 35840, 5840, 35840                                    
 5840 continue                                                          
      ivon01 = 6170                                                     
      ivon02 = 1234                                                     
      ivcomp = ivon01/ivon02                                            
      goto 45840                                                       
35840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45840, 5851, 45840                                    
45840 if (ivcomp - 5) 25840,15840,25840                                 
15840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5851                                                        
25840 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5851 continue                                                          
      ivtnum = 585                                                      
!                                                                        
!       ****  TEST 585  ****                                             
!                                                                        
      if (iczero) 35850, 5850, 35850                                    
 5850 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 1                                                        
      ivcomp = ivon01/ivon02                                            
      goto 45850                                                       
35850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45850, 5861, 45850                                    
45850 if (ivcomp - 32767) 25850,15850,25850                             
15850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5861                                                        
25850 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 586 THROUGH TEST 589  -  POSITIVE VALUES                     
!                    TRUNCATION OF RESULT REQUIRED                       
!                                                                        
 5861 continue                                                          
      ivtnum = 586                                                      
!                                                                        
!       ****  TEST 586  ****                                             
!                                                                        
      if (iczero) 35860, 5860, 35860                                    
 5860 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01/ivon02                                            
      goto 45860                                                       
35860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45860, 5871, 45860                                    
45860 if (ivcomp) 25860,15860,25860                                     
15860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5871                                                        
25860 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5871 continue                                                          
      ivtnum = 587                                                      
!                                                                        
!       ****  TEST 587  ****                                             
!                                                                        
      if (iczero) 35870, 5870, 35870                                    
 5870 continue                                                          
      ivon01 = 959                                                      
      ivon02 = 120                                                      
      ivcomp = ivon01/ivon02                                            
      goto 45870                                                       
35870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45870, 5881, 45870                                    
45870 if (ivcomp - 7) 25870,15870,25870                                 
15870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5881                                                        
25870 ivfail = ivfail + 1                                               
      ivcorr = 7                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5881 continue                                                          
      ivtnum = 588                                                      
!                                                                        
!       ****  TEST 588  ****                                             
!                                                                        
      if (iczero) 35880, 5880, 35880                                    
 5880 continue                                                          
      ivon01 = 26606                                                    
      ivon02 = 8                                                        
      ivcomp = ivon01/ivon02                                            
      goto 45880                                                       
35880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45880, 5891, 45880                                    
45880 if (ivcomp - 3325) 25880,15880,25880                              
15880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5891                                                        
25880 ivfail = ivfail + 1                                               
      ivcorr = 3325                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5891 continue                                                          
      ivtnum = 589                                                      
!                                                                        
!       ****  TEST 589  ****                                             
!                                                                        
      if (iczero) 35890, 5890, 35890                                    
 5890 continue                                                          
      ivon01 = 25603                                                    
      ivon02 = 10354                                                    
      ivcomp = ivon01/ivon02                                            
      goto 45890                                                       
35890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45890, 5901, 45890                                    
45890 if (ivcomp - 2) 25890,15890,25890                                 
15890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5901                                                        
25890 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 590 THROUGH TEST 593  - NEGATIVE VALUES INCLUDED             
!                NO TRUNCATION REQUIRED                                  
!                                                                        
 5901 continue                                                          
      ivtnum = 590                                                      
!                                                                        
!       ****  TEST 590  ****                                             
!                                                                        
      if (iczero) 35900, 5900, 35900                                    
 5900 continue                                                          
      ivon01 = 75                                                       
      ivon02 = -25                                                      
      ivcomp = ivon01/ivon02                                            
      goto 45900                                                       
35900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45900, 5911, 45900                                    
45900 if (ivcomp + 3) 25900,15900,25900                                 
15900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5911                                                        
25900 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5911 continue                                                          
      ivtnum = 591                                                      
!                                                                        
!       ****  TEST 591  ****                                             
!                                                                        
      if (iczero) 35910, 5910, 35910                                    
 5910 continue                                                          
      ivon01 = -6170                                                    
      ivon02 = -1234                                                    
      ivcomp = ivon01/ivon02                                            
      goto 45910                                                       
35910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45910, 5921, 45910                                    
45910 if (ivcomp -5) 25910,15910,25910                                  
15910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5921                                                        
25910 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5921 continue                                                          
      ivtnum = 592                                                      
!                                                                        
!       ****  TEST 592  ****                                             
!                                                                        
      if (iczero) 35920, 5920, 35920                                    
 5920 continue                                                          
      ivon01 = 32766                                                    
      ivon02 = -2                                                       
      ivcomp =-ivon01/ivon02                                            
      goto 45920                                                       
35920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45920, 5931, 45920                                    
45920 if (ivcomp - 16383) 25920,15920,25920                             
15920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5931                                                        
25920 ivfail = ivfail + 1                                               
      ivcorr = 16383                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5931 continue                                                          
      ivtnum = 593                                                      
!                                                                        
!       ****  TEST 593  ****                                             
!                                                                        
      if (iczero) 35930, 5930, 35930                                    
 5930 continue                                                          
      ivon01 = 4                                                        
      ivon02 = 2                                                        
      ivcomp = ivon01/(-ivon02)                                         
      goto 45930                                                       
35930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45930, 5941, 45930                                    
45930 if (ivcomp + 2) 25930,15930,25930                                 
15930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5941                                                        
25930 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 594 THROUGH TEST 597  -  NEGATIVE VALUES INCLUDED            
!                       TRUNCATION OF RESULT REQUIRED                    
!                                                                        
 5941 continue                                                          
      ivtnum = 594                                                      
!                                                                        
!       ****  TEST 594  ****                                             
!                                                                        
      if (iczero) 35940, 5940, 35940                                    
 5940 continue                                                          
      ivon01 = -5                                                       
      ivon02 = 2                                                        
      ivcomp = ivon01/ivon02                                            
      goto 45940                                                       
35940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45940, 5951, 45940                                    
45940 if (ivcomp + 2) 25940,15940,25940                                 
15940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5951                                                        
25940 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5951 continue                                                          
      ivtnum = 595                                                      
!                                                                        
!       ****  TEST 595  ****                                             
!                                                                        
      if (iczero) 35950, 5950, 35950                                    
 5950 continue                                                          
      ivon01 = -25603                                                   
      ivon02 = -10354                                                   
      ivcomp = ivon01/ivon02                                            
      goto 45950                                                       
35950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45950, 5961, 45950                                    
45950 if (ivcomp -2) 25950,15950,25950                                  
15950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5961                                                        
25950 ivfail = ivfail + 1                                               
      ivcorr =2                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5961 continue                                                          
      ivtnum = 596                                                      
!                                                                        
!       ****  TEST 596  ****                                             
!                                                                        
      if (iczero) 35960, 5960, 35960                                    
 5960 continue                                                          
      ivon01 = 25603                                                    
      ivon02 = 10354                                                    
      ivcomp = -ivon01/ivon02                                           
      goto 45960                                                       
35960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45960, 5971, 45960                                    
45960 if (ivcomp +2) 25960,15960,25960                                  
15960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5971                                                        
25960 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5971 continue                                                          
      ivtnum = 597                                                      
!                                                                        
!       ****  TEST 597  ****                                             
!                                                                        
      if (iczero) 35970, 5970, 35970                                    
 5970 continue                                                          
      ivon01 = 25603                                                    
      ivon02 = -2                                                       
      ivcomp = -(ivon01/ivon02)                                         
      goto 45970                                                       
35970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45970, 5981, 45970                                    
45970 if (ivcomp - 12801) 25970,15970,25970                             
15970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5981                                                        
25970 ivfail = ivfail + 1                                               
      ivcorr = 12801                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 598 THROUGH TEST 614 CONTAIN TWO INTEGER VARIABLES, AN       
!      INTEGER CONSTANT AND OPERATOR / IN AN ARITHMETIC EXPRESSION.      
!                                                                        
!          TEST 598 THROUGH TEST 603  -  NO PARENS TO GROUP ELEMENTS BUT 
!                    THERE ARE PARENS SURROUNDING NEGATIVE CONSTANTS     
!                                                                        
!      TEST 598 AND TEST 599  -  IV = IV/IV/IC.                          
!                                                                        
 5981 continue                                                          
      ivtnum = 598                                                      
!                                                                        
!       ****  TEST 598  ****                                             
!                                                                        
      if (iczero) 35980, 5980, 35980                                    
 5980 continue                                                          
      ivon01 = 32766                                                    
      ivon02 = 2                                                        
      ivcomp = ivon01/ivon02/3                                          
      goto 45980                                                       
35980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45980, 5991, 45980                                    
45980 if (ivcomp - 5461) 25980,15980,25980                              
15980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5991                                                        
25980 ivfail = ivfail + 1                                               
      ivcorr = 5461                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5991 continue                                                          
      ivtnum = 599                                                      
!                                                                        
!       ****  TEST 599  ****                                             
!                                                                        
      if (iczero) 35990, 5990, 35990                                    
 5990 continue                                                          
      ivon01 = 7151                                                     
      ivon02 = 3                                                        
      ivcomp = ivon01/ivon02/10                                         
      goto 45990                                                       
35990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45990, 6001, 45990                                    
45990 if (ivcomp -238) 25990,15990,25990                                
15990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6001                                                        
25990 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 600 AND TEST 601   -  IV= IV/IC/IV.                          
!                                                                        
 6001 continue                                                          
      ivtnum = 600                                                      
!                                                                        
!       ****  TEST 600  ****                                             
!                                                                        
      if (iczero) 36000, 6000, 36000                                    
 6000 continue                                                          
      ivon01 = -7150                                                    
      ivon03 = -25                                                      
      ivcomp = ivon01/(-2)/ivon03                                       
      goto 46000                                                       
36000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46000, 6011, 46000                                    
46000 if (ivcomp + 143)  26000,16000,26000                              
16000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6011                                                        
26000 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6011 continue                                                          
      ivtnum = 601                                                      
!                                                                        
!       ****  TEST 601  ****                                             
!                                                                        
      if (iczero) 36010, 6010, 36010                                    
 6010 continue                                                          
      ivon01 = 32767                                                    
      ivon03 = -1                                                       
      ivcomp = ivon01/2/ivon03                                          
      goto 46010                                                       
36010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46010, 6021, 46010                                    
46010 if (ivcomp + 16383) 26010,16010,26010                             
16010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6021                                                        
26010 ivfail = ivfail + 1                                               
      ivcorr = -16383                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6021 continue                                                          
      ivtnum = 602                                                      
!                                                                        
!       ****  TEST 602  ****                                             
!                                                                        
!      TEST 602 AND TEST 603   -  IV=IC/IV/IV                            
!                                                                        
!                                                                        
      if (iczero) 36020, 6020, 36020                                    
 6020 continue                                                          
      ivon02 = 13                                                       
      ivon03 = 51                                                       
      ivcomp = 15249/ivon02/ivon03                                      
      goto 46020                                                       
36020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46020, 6031, 46020                                    
46020 if (ivcomp - 23) 26020,16020,26020                                
16020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6031                                                        
26020 ivfail = ivfail + 1                                               
      ivcorr = 23                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6031 continue                                                          
      ivtnum = 603                                                      
!                                                                        
!       ****  TEST 603  ****                                             
!                                                                        
      if (iczero) 36030, 6030, 36030                                    
 6030 continue                                                          
      ivon02 = -13                                                      
      ivon03 = -51                                                      
      ivcomp = -15249/ivon02/ivon03                                     
      goto 46030                                                       
36030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46030, 6041, 46030                                    
46030 if (ivcomp +23) 26030,16030,26030                                 
16030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6041                                                        
26030 ivfail = ivfail + 1                                               
      ivcorr = -23                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 604 THROUGH TEST 614  - PARENTHESES ARE USED TO GROUP        
!      ELEMENTS IN THE ARITHMETIC EXPRESSIONS.                           
!                                                                        
!      TEST 604 AND TEST 605  -  IV=(IV/IV)/IC.                          
!                                                                        
 6041 continue                                                          
      ivtnum = 604                                                      
!                                                                        
!       ****  TEST 604  ****                                             
!                                                                        
      if (iczero) 36040, 6040, 36040                                    
 6040 continue                                                          
      ivon01 = 32766                                                    
      ivon02 = 2                                                        
      ivcomp =(ivon01/ivon02)/3                                         
      goto 46040                                                       
36040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46040, 6051, 46040                                    
46040 if (ivcomp -5461) 26040,16040,26040                               
16040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6051                                                        
26040 ivfail = ivfail + 1                                               
      ivcorr = 5461                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6051 continue                                                          
      ivtnum = 605                                                      
!                                                                        
!       ****  TEST 605  ****                                             
!                                                                        
      if (iczero) 36050, 6050, 36050                                    
 6050 continue                                                          
      ivon01 = 7151                                                     
      ivon02 =  3                                                       
      ivcomp = (ivon01/ivon02)/10                                       
      goto 46050                                                       
36050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46050, 6061, 46050                                    
46050 if (ivcomp - 238) 26050,16050,26050                               
16050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6061                                                        
26050 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 606 AND TEST 607  -  IV=IV/(IV/IC).                          
!                                                                        
 6061 continue                                                          
      ivtnum = 606                                                      
!                                                                        
!       ****  TEST 606  ****                                             
!                                                                        
      if (iczero) 36060, 6060, 36060                                    
 6060 continue                                                          
      ivon01 = -7154                                                    
      ivon02 =  26                                                      
      ivcomp = ivon01/(ivon02/5)                                        
      goto 46060                                                       
36060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46060, 6071, 46060                                    
46060 if (ivcomp + 1430) 26060,16060,26060                              
16060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6071                                                        
26060 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6071 continue                                                          
      ivtnum = 607                                                      
!                                                                        
!       ****  TEST 607  ****                                             
!                                                                        
      if (iczero) 36070, 6070, 36070                                    
 6070 continue                                                          
      ivon01 = 29                                                       
      ivon02 = -5                                                       
      ivcomp = ivon01/(ivon02/2)                                        
      goto 46070                                                       
36070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46070, 6081, 46070                                    
46070 if (ivcomp + 14) 26070,16070,26070                                
16070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6081                                                        
26070 ivfail = ivfail + 1                                               
      ivcorr = -14                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 608 AND TEST 609  -  IV = (IV/IC)/IV.                        
!                                                                        
 6081 continue                                                          
      ivtnum = 608                                                      
!                                                                        
!       ****  TEST 608  ****                                             
!                                                                        
      if (iczero) 36080, 6080, 36080                                    
 6080 continue                                                          
      ivon01 = 24                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01/3)/ivon03                                        
      goto 46080                                                       
36080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46080, 6091, 46080                                    
46080 if (ivcomp -2) 26080,16080,26080                                  
16080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6091                                                        
26080 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6091 continue                                                          
      ivtnum = 609                                                      
!                                                                        
!       ****  TEST 609  ****                                             
!                                                                        
      if (iczero) 36090, 6090, 36090                                    
 6090 continue                                                          
      ivon01 = 7151                                                     
      ivon03 = 10                                                       
      ivcomp = (ivon01/(-3))/ivon03                                     
      goto 46090                                                       
36090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46090, 6101, 46090                                    
46090 if (ivcomp + 238) 26090,16090,26090                               
16090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6101                                                        
26090 ivfail = ivfail + 1                                               
      ivcorr = -238                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 610 AND TEST 611  -  IV=IV(IC/IV)                            
!                                                                        
 6101 continue                                                          
      ivtnum = 610                                                      
!                                                                        
!       ****  TEST 610  ****                                             
!                                                                        
      if (iczero) 36100, 6100, 36100                                    
 6100 continue                                                          
      ivon01 = -7154                                                    
      ivon03 = -5                                                       
      ivcomp = ivon01/((-26)/ivon03)                                    
      goto 46100                                                       
36100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46100, 6111, 46100                                    
46100 if (ivcomp + 1430) 26100,16100,26100                              
16100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6111                                                        
26100 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6111 continue                                                          
      ivtnum = 611                                                      
!                                                                        
!       ****  TEST 611  ****                                             
!                                                                        
      if (iczero) 36110, 6110, 36110                                    
 6110 continue                                                          
      ivon01 = 7150                                                     
      ivon03 = 5                                                        
      ivcomp = ivon01/((+25)/ivon03)                                    
      goto 46110                                                       
36110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46110, 6121, 46110                                    
46110 if (ivcomp -1430) 26110,16110,26110                               
16110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6121                                                        
26110 ivfail = ivfail + 1                                               
      ivcorr = 1430                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6121 continue                                                          
      ivtnum = 612                                                      
!                                                                        
!       ****  TEST 612  ****                                             
!      TEST 612  -  IV= (IC/IV)/IV                                       
!                                                                        
      if (iczero) 36120, 6120, 36120                                    
 6120 continue                                                          
      ivon02 = -3                                                       
      ivon03 = -10                                                      
      ivcomp = (-7154/ivon02)/ivon03                                    
      goto 46120                                                       
36120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46120, 6131, 46120                                    
46120 if (ivcomp + 238) 26120,16120,26120                               
16120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6131                                                        
26120 ivfail = ivfail + 1                                               
      ivcorr = -238                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 613 AND TEST 614  -  IV=IC/(IV/IV)                           
!                                                                        
 6131 continue                                                          
      ivtnum = 613                                                      
!                                                                        
!       ****  TEST 613  ****                                             
!                                                                        
      if (iczero) 36130, 6130, 36130                                    
 6130 continue                                                          
      ivon02 = 8                                                        
      ivon03 = 4                                                        
      ivcomp = 24/(ivon02/ivon03)                                       
      goto 46130                                                       
36130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46130, 6141, 46130                                    
46130 if (ivcomp - 12) 26130,16130,26130                                
16130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6141                                                        
26130 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6141 continue                                                          
      ivtnum = 614                                                      
!                                                                        
!       ****  TEST 614  ****                                             
!                                                                        
      if (iczero) 36140, 6140, 36140                                    
 6140 continue                                                          
      ivon02 = 25                                                       
      ivon03 = 5                                                        
      ivcomp = 7150/(-(ivon02/ivon03))                                  
      goto 46140                                                       
36140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46140, 6151, 46140                                    
46140 if (ivcomp + 1430) 26140,16140,26140                              
16140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6151                                                        
26140 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!       ****    END OF TESTS    ****                                     
 6151 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM040" )                          
      end program fm040
