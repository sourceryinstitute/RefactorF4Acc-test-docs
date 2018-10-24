      program fm046
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      integer :: ivon06
      integer :: ivcomp
      integer :: ivcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM046                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS USING INTEGER       
!      VARIABLES CONNECTED BY A SERIES OF ARITHMETIC OPERATORS.          
!      DIFFERENT COMBINATIONS OF PARENTHETICAL NOTATION ARE EXERCIZED.   
!                                                                        
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
!                                                                        
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
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!                                                                        
!      TESTS 747 THROUGH 755 USE THE SAME STRING OF VARIABLES AND        
!      OPERATORS, BUT USE DIFFERENT COMBINATIONS OF PARENTHETICAL        
!      NOTATION  TO ALTER PRIORITIES IN ORDER OF EVALUATION.             
!                                                                        
!      TESTS 756 THROUGH 759 CHECK THE CAPABILITY TO ENCLOSE THE ENTIRE  
!      RIGHT HAND SIDE OF AN ASSIGNMENT STATEMENT IN PARENTHESES OR SETS 
!      OF NESTED PARENTHESES.                                            
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!                                                                        
      ivtnum = 747                                                      
!                                                                        
!       ****  TEST 747  ****                                             
!                                                                        
      if (iczero) 37470, 7470, 37470                                    
 7470 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 18                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = ivon01 + ivon02 - ivon03 * ivon04 / ivon05 ** ivon06     
      goto 47470                                                       
37470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47470, 7481, 47470                                    
47470 if (ivcomp - 22) 27470,17470,27470                                
17470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7481                                                        
27470 ivfail = ivfail + 1                                               
      ivcorr = 22                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7481 continue                                                          
      ivtnum = 748                                                      
!                                                                        
!       ****  TEST 748  ****                                             
!                                                                        
      if (iczero) 37480, 7480, 37480                                    
 7480 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 18                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = ((((ivon01 + ivon02) - ivon03) * ivon04) / ivon05)                ** ivon06                                                
      goto 47480                                                       
37480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47480, 7491, 47480                                    
47480 if (ivcomp - 3600) 27480,17480,27480                              
17480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7491                                                        
27480 ivfail = ivfail + 1                                               
      ivcorr = 3600                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7491 continue                                                          
      ivtnum = 749                                                      
!                                                                        
!       ****  TEST 749  ****                                             
!                                                                        
      if (iczero) 37490, 7490, 37490                                    
 7490 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = (ivon01 + ivon02 - ivon03) * (ivon04 / ivon05 ** ivon06) 
      goto 47490                                                       
37490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47490, 7501, 47490                                    
47490 if (ivcomp - 20) 27490,17490,27490                                
17490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7501                                                        
27490 ivfail = ivfail + 1                                               
      ivcorr = 20                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7501 continue                                                          
      ivtnum = 750                                                      
!                                                                        
!       ****  TEST 750  ****                                             
!                                                                        
      if (iczero) 37500, 7500, 37500                                    
 7500 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = (ivon01 + ivon02) - (ivon03 * ivon04) / (ivon05 **                ivon06)                                                  
      goto 47500                                                       
37500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47500, 7511, 47500                                    
47500 if (ivcomp - 20) 27500,17500,27500                                
17500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7511                                                        
27500 ivfail = ivfail + 1                                               
      ivcorr = 20                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7511 continue                                                          
      ivtnum = 751                                                      
!                                                                        
!       ****  TEST 751  ****                                             
!                                                                        
      if (iczero) 37510, 7510, 37510                                    
 7510 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = ((ivon01 + ivon02) - (ivon03 * ivon04)) / (ivon05 **              ivon06)                                                  
      goto 47510                                                       
37510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero)  47510, 7521, 47510                                   
47510 if (ivcomp + 3)  27510,17510,27510                                
17510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7521                                                        
27510 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
!      ACTUAL ANSWER IS  -3.333333...     TRUNCATION IS NECESSARY        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7521 continue                                                          
      ivtnum = 752                                                      
!                                                                        
!       ****  TEST 752  ****                                             
!                                                                        
      if (iczero) 37520, 7520, 37520                                    
 7520 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = (ivon01 + ivon02) - (ivon03 * ivon04 / ivon05) ** ivon06 
      goto 47520                                                       
37520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47520, 7531, 47520                                    
47520 if (ivcomp + 552) 27520,17520,27520                               
17520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7531                                                        
27520 ivfail = ivfail + 1                                               
      ivcorr = -552                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7531 continue                                                          
      ivtnum = 753                                                      
!                                                                        
!       ****  TEST 753  ****                                             
!                                                                        
      if (iczero) 37530, 7530, 37530                                    
 7530 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = ivon01 + (ivon02 - ivon03 * ivon04) / ivon05 ** ivon06   
      goto 47530                                                       
37530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47530, 7541, 47530                                    
47530 if (ivcomp - 12) 27530,17530,27530                                
17530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7541                                                        
27530 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
!      ACTUAL ANSWER IS  11.25            TRUNCATION IS NECESSARY        
!                                         DURING AN INTERMEDIATE STEP    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7541 continue                                                          
      ivtnum = 754                                                      
!                                                                        
!       ****  TEST 754  ****                                             
!                                                                        
      if (iczero) 37540, 7540, 37540                                    
 7540 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = ivon01 + (ivon02 - ivon03) * (ivon04 / ivon05) ** ivon06 
      goto 47540                                                       
37540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47540, 7551, 47540                                    
47540 if (ivcomp - 195) 27540,17540,27540                               
17540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7551                                                        
27540 ivfail = ivfail + 1                                               
      ivcorr = 195                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7551 continue                                                          
      ivtnum = 755                                                      
!                                                                        
!       ****  TEST 755  ****                                             
!                                                                        
      if (iczero) 37550, 7550, 37550                                    
 7550 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  4                                                       
      ivon04 = 36                                                       
      ivon05 =  6                                                       
      ivon06 =  2                                                       
      ivcomp = ((ivon01 + (ivon02 - ivon03) * ivon04) / ivon05) **               ivon06                                                   
      goto 47550                                                       
37550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47550, 7561, 47550                                    
47550 if (ivcomp - 1024)  27550,17550,27550                             
17550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7561                                                        
27550 ivfail = ivfail + 1                                               
      ivcorr = 1024                                                     
!      ACTUAL ANSWER IS  1056.25         TRUNCATION IS NECESSARY         
!                                        DURING AN INTERMEDIATE STEP     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7561 continue                                                          
      ivtnum = 756                                                      
!                                                                        
!       ****  TEST 756  ****                                             
!           SINGLE PARENTHESES                                           
!                                                                        
      if (iczero) 37560, 7560, 37560                                    
 7560 continue                                                          
      ivon01 = 13                                                       
      ivon02 = 37                                                       
      ivcomp = (ivon01 + ivon02)                                        
      goto 47560                                                       
37560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47560, 7571, 47560                                    
47560 if (ivcomp - 50) 27560,17560,27560                                
17560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7571                                                        
27560 ivfail = ivfail + 1                                               
      ivcorr = 50                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7571 continue                                                          
      ivtnum = 757                                                      
!                                                                        
!       ****  TEST 757  ****                                             
!           NESTED PARENTHESES (TWO SETS)                                
!                                                                        
      if (iczero) 37570, 7570, 37570                                    
 7570 continue                                                          
      ivon01 = 13                                                       
      ivon02 = 37                                                       
      ivcomp = ((ivon01 - ivon02))                                      
      goto 47570                                                       
37570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47570, 7581, 47570                                    
47570 if (ivcomp + 24) 27570,17570,27570                                
17570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7581                                                        
27570 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7581 continue                                                          
      ivtnum = 758                                                      
!                                                                        
!       ****  TEST 758  ****                                             
!           NESTED PARENTHESES (21 SETS - SAME LINE)                     
!                                                                        
      if (iczero) 37580, 7580, 37580                                    
 7580 continue                                                          
      ivon01 = 13                                                       
      ivon02 = 37                                                       
      ivcomp = (((((((((((((((((((((ivon01 * ivon02)))))))))))))))))))))
      goto 47580                                                       
37580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47580, 7591, 47580                                    
47580 if (ivcomp - 481) 27580,17580,27580                               
17580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7591                                                        
27580 ivfail = ivfail + 1                                               
      ivcorr = 481                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7591 continue                                                          
      ivtnum = 759                                                      
!                                                                        
!       ****  TEST 759  ****                                             
!           NESTED PARENTHESES (5 SETS - MULTIPLE LINES)                 
!                                                                        
      if (iczero) 37590, 7590, 37590                                    
 7590 continue                                                          
      ivon01 = 13                                                       
      ivon02 = 37                                                       
      ivcomp = ((                     (                               ((         ivon01 / ivon02                                          ))                     )                               ))
      goto 47590                                                       
37590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47590, 7601, 47590                                    
47590 if (ivcomp) 27590,17590,27590                                     
17590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7601                                                        
27590 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7601 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM046" )                          
      end program fm046
