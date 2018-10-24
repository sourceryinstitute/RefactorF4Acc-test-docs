      program fm036
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM036                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASIGNMENT STATEMENTS OF THE     
!      FORM                                                              
!               INTEGER VARIABLE = ARITHMETIC EXPRESSION                 
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR / AND INTEGER CONSTANTS.  BOTH POSITIVE AND NEGATIVE     
!      CONSTANTS ARE USED IN THE ARITHMETIC EXPRESSION.                  
!                                                                        
!          THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     
!      AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  
!      IN THE RESULTANT INTEGER VARIABLE.  THE STANDARD STATES 'THE VALUE
!      OF AN INTEGER FACTOR OR TERM IS THE NEAREST INTEGER WHOSE         
!      MAGNITUDE DOES NOT EXCEED THE MAGNITUDE OF THE MATHEMATICAL VALUE 
!      REPRESENTED BY THAT FACTOR OR TERM.'                              
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!              (1)  INTEGER CONSTANT/INTEGER CONSTANT                    
!                       NO TRUNCATION REQUIRED,                          
!              (2)  INTEGER CONSTANT/INTEGER CONSTANT                    
!                       TRUNCATION REQUIRED.                             
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
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!      TEST 462 THROUGH TEST 490 CONTAIN TWO INTEGER CONSTANTS AND       
!      OPERATOR / IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS       
!             INTEGER VARIABLE = INTEGER CONSTANT/INTEGER CONSTANT       
!                                                                        
!      TEST 462 THROUGH TEST 469 - POSITIVE CONSTANTS                    
!               NO TRUNCATION REQUIRED                                   
!                                                                        
 4621 continue                                                          
      ivtnum = 462                                                      
!                                                                        
!       ****  TEST 462  ****                                             
!                                                                        
      if (iczero) 34620, 4620, 34620                                    
 4620 continue                                                          
      ivcomp = 4/2                                                      
      goto 44620                                                       
34620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44620, 4631, 44620                                    
44620 if (ivcomp - 2) 24620,14620,24620                                 
14620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4631                                                        
24620 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4631 continue                                                          
      ivtnum = 463                                                      
!                                                                        
!       ****  TEST 463  ****                                             
!                                                                        
      if (iczero) 34630, 4630, 34630                                    
 4630 continue                                                          
      ivcomp = 75 / 25                                                  
      goto 44630                                                       
34630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44630, 4641, 44630                                    
44630 if (ivcomp - 3) 24630,14630,24630                                 
14630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4641                                                        
24630 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4641 continue                                                          
      ivtnum = 464                                                      
!                                                                        
!       ****  TEST 464  ****                                             
!                                                                        
      if (iczero) 34640, 4640, 34640                                    
 4640 continue                                                          
      ivcomp = 3575/143                                                 
      goto 44640                                                       
34640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44640, 4651, 44640                                    
44640 if (ivcomp - 25) 24640,14640,24640                                
14640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4651                                                        
24640 ivfail = ivfail + 1                                               
      ivcorr = 25                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4651 continue                                                          
      ivtnum = 465                                                      
!                                                                        
!       ****  TEST 465  ****                                             
!                                                                        
      if (iczero) 34650, 4650, 34650                                    
 4650 continue                                                          
      ivcomp = 3575/25                                                  
      goto 44650                                                       
34650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44650, 4661, 44650                                    
44650 if (ivcomp - 143) 24650,14650,24650                               
14650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4661                                                        
24650 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4661 continue                                                          
      ivtnum = 466                                                      
!                                                                        
!       ****  TEST 466  ****                                             
!                                                                        
      if (iczero) 34660, 4660, 34660                                    
 4660 continue                                                          
      ivcomp = 6170/1234                                                
      goto 44660                                                       
34660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44660, 4671, 44660                                    
44660 if (ivcomp - 5) 24660,14660,24660                                 
14660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4671                                                        
24660 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4671 continue                                                          
      ivtnum = 467                                                      
!                                                                        
!       ****  TEST 467  ****                                             
!                                                                        
      if (iczero) 34670, 4670, 34670                                    
 4670 continue                                                          
      ivcomp = 28600/8                                                  
      goto 44670                                                       
34670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44670, 4681, 44670                                    
44670 if (ivcomp - 3575) 24670,14670,24670                              
14670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4681                                                        
24670 ivfail = ivfail + 1                                               
      ivcorr = 3575                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4681 continue                                                          
      ivtnum = 468                                                      
!                                                                        
!       ****  TEST 468  ****                                             
!                                                                        
      if (iczero) 34680, 4680, 34680                                    
 4680 continue                                                          
      ivcomp = 32766/2                                                  
      goto 44680                                                       
34680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44680, 4691, 44680                                    
44680 if (ivcomp - 16383) 24680,14680,24680                             
14680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4691                                                        
24680 ivfail = ivfail + 1                                               
      ivcorr = 16383                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4691 continue                                                          
      ivtnum = 469                                                      
!                                                                        
!       ****  TEST 469  ****                                             
!                                                                        
      if (iczero) 34690, 4690, 34690                                    
 4690 continue                                                          
      ivcomp = 32767/1                                                  
      goto 44690                                                       
34690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44690, 4701, 44690                                    
44690 if (ivcomp - 32767) 24690,14690,24690                             
14690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4701                                                        
24690 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 470 THROUGH TEST 478 - POSITIVE CONSTANTS                    
!                TRUNCATION REQUIRED                                     
!                                                                        
 4701 continue                                                          
      ivtnum = 470                                                      
!                                                                        
!       ****  TEST 470  ****                                             
!                                                                        
      if (iczero) 34700, 4700, 34700                                    
 4700 continue                                                          
      ivcomp = 5/2                                                      
      goto 44700                                                       
34700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44700, 4711, 44700                                    
44700 if (ivcomp - 2) 24700,14700,24700                                 
14700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4711                                                        
24700 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4711 continue                                                          
      ivtnum = 471                                                      
!                                                                        
!       ****  TEST 471  ****                                             
!                                                                        
      if (iczero) 34710, 4710, 34710                                    
 4710 continue                                                          
      ivcomp = 2/3                                                      
      goto 44710                                                       
34710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44710, 4721, 44710                                    
44710 if (ivcomp - 0) 24710,14710,24710                                 
14710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4721                                                        
24710 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4721 continue                                                          
      ivtnum = 472                                                      
!                                                                        
!       ****  TEST 472  ****                                             
!                                                                        
      if (iczero) 34720, 4720, 34720                                    
 4720 continue                                                          
      ivcomp = 80/15                                                    
      goto 44720                                                       
34720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44720, 4731, 44720                                    
44720 if (ivcomp - 5) 24720,14720,24720                                 
14720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4731                                                        
24720 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4731 continue                                                          
      ivtnum = 473                                                      
!                                                                        
!       ****  TEST 473  ****                                             
!                                                                        
      if (iczero) 34730, 4730, 34730                                    
 4730 continue                                                          
      ivcomp = 959/120                                                  
      goto 44730                                                       
34730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44730, 4741, 44730                                    
44730 if (ivcomp - 7) 24730,14730,24730                                 
14730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4741                                                        
24730 ivfail = ivfail + 1                                               
      ivcorr = 7                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4741 continue                                                          
      ivtnum = 474                                                      
!                                                                        
!       ****  TEST 474  ****                                             
!                                                                        
      if (iczero) 34740, 4740, 34740                                    
 4740 continue                                                          
      ivcomp = 959 / 12                                                 
      goto 44740                                                       
34740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44740, 4751, 44740                                    
44740 if (ivcomp - 79) 24740,14740,24740                                
14740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4751                                                        
24740 ivfail = ivfail + 1                                               
      ivcorr = 79                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4751 continue                                                          
      ivtnum = 475                                                      
!                                                                        
!       ****  TEST 475  ****                                             
!                                                                        
      if (iczero) 34750, 4750, 34750                                    
 4750 continue                                                          
      ivcomp = 959/6                                                    
      goto 44750                                                       
34750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44750, 4761, 44750                                    
44750 if (ivcomp - 159) 24750,14750,24750                               
14750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4761                                                        
24750 ivfail = ivfail + 1                                               
      ivcorr = 159                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4761 continue                                                          
      ivtnum = 476                                                      
!                                                                        
!       ****  TEST 476  ****                                             
!                                                                        
      if (iczero) 34760, 4760, 34760                                    
 4760 continue                                                          
      ivcomp = 28606/8                                                  
      goto 44760                                                       
34760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44760, 4771, 44760                                    
44760 if (ivcomp - 3575) 24760,14760,24760                              
14760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4771                                                        
24760 ivfail = ivfail + 1                                               
      ivcorr = 3575                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4771 continue                                                          
      ivtnum = 477                                                      
!                                                                        
!       ****  TEST 477  ****                                             
!                                                                        
      if (iczero) 34770, 4770, 34770                                    
 4770 continue                                                          
      ivcomp = 25603/2                                                  
      goto 44770                                                       
34770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44770, 4781, 44770                                    
44770 if (ivcomp - 12801) 24770,14770,24770                             
14770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4781                                                        
24770 ivfail = ivfail + 1                                               
      ivcorr = 12801                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4781 continue                                                          
      ivtnum = 478                                                      
!                                                                        
!       ****  TEST 478  ****                                             
!                                                                        
      if (iczero) 34780, 4780, 34780                                    
 4780 continue                                                          
      ivcomp = 25603/10354                                              
      goto 44780                                                       
34780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44780, 4791, 44780                                    
44780 if (ivcomp - 2) 24780,14780,24780                                 
14780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4791                                                        
24780 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 479 THROUGH TEST 482 - NEGATIVE CONSTANTS INCLUDED           
!                 NO TRUNCATION REQUIRED                                 
!                                                                        
 4791 continue                                                          
      ivtnum = 479                                                      
!                                                                        
!       ****  TEST 479  ****                                             
!                                                                        
      if (iczero) 34790, 4790, 34790                                    
 4790 continue                                                          
      ivcomp = -4/2                                                     
      goto 44790                                                       
34790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44790, 4801, 44790                                    
44790 if (ivcomp + 2) 24790,14790,24790                                 
14790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4801                                                        
24790 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4801 continue                                                          
      ivtnum = 480                                                      
!                                                                        
!       ****  TEST 480  ****                                             
!                                                                        
      if (iczero) 34800, 4800, 34800                                    
 4800 continue                                                          
      ivcomp = 75 / (-25)                                               
      goto 44800                                                       
34800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44800, 4811, 44800                                    
44800 if (ivcomp + 3) 24800,14800,24800                                 
14800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4811                                                        
24800 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4811 continue                                                          
      ivtnum = 481                                                      
!                                                                        
!       ****  TEST 481  ****                                             
!                                                                        
      if (iczero) 34810, 4810, 34810                                    
 4810 continue                                                          
      ivcomp= (-6170) / (-1234)                                         
      goto 44810                                                       
34810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44810, 4821, 44810                                    
44810 if (ivcomp - 5) 24810,14810,24810                                 
14810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4821                                                        
24810 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4821 continue                                                          
      ivtnum = 482                                                      
!                                                                        
!       ****  TEST 482  ****                                             
!                                                                        
      if (iczero) 34820, 4820, 34820                                    
 4820 continue                                                          
      ivcomp = -32766/(-2)                                              
      goto 44820                                                       
34820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44820, 4831, 44820                                    
44820 if (ivcomp - 16383) 24820,14820,24820                             
14820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4831                                                        
24820 ivfail = ivfail + 1                                               
      ivcorr = 16383                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 483 THROUGH TEST 490 - NEGATIVE CONSTANTS INCLUDED           
!                 TRUNCATION REQUIRED                                    
!                                                                        
 4831 continue                                                          
      ivtnum = 483                                                      
!                                                                        
!       ****  TEST 483  ****                                             
!                                                                        
      if (iczero) 34830, 4830, 34830                                    
 4830 continue                                                          
      ivcomp = -5/2                                                     
      goto 44830                                                       
34830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44830, 4841, 44830                                    
44830 if (ivcomp +2) 24830,14830,24830                                  
14830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4841                                                        
24830 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4841 continue                                                          
      ivtnum = 484                                                      
!                                                                        
!       ****  TEST 484  ****                                             
!                                                                        
      if (iczero) 34840, 4840, 34840                                    
 4840 continue                                                          
      ivcomp = -2/3                                                     
      goto 44840                                                       
34840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44840, 4851, 44840                                    
44840 if (ivcomp) 24840,14840,24840                                     
14840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4851                                                        
24840 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4851 continue                                                          
      ivtnum = 485                                                      
!                                                                        
!       ****  TEST 485  ****                                             
!                                                                        
      if (iczero) 34850, 4850, 34850                                    
 4850 continue                                                          
      ivcomp = 80/(-15)                                                 
      goto 44850                                                       
34850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44850, 4861, 44850                                    
44850 if (ivcomp +5) 24850,14850,24850                                  
14850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4861                                                        
24850 ivfail = ivfail + 1                                               
      ivcorr = -5                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4861 continue                                                          
      ivtnum = 486                                                      
!                                                                        
!       ****  TEST 486  ****                                             
!                                                                        
      if (iczero) 34860, 4860, 34860                                    
 4860 continue                                                          
      ivcomp = -959/(-120)                                              
      goto 44860                                                       
34860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44860, 4871, 44860                                    
44860 if (ivcomp - 7) 24860,14860,24860                                 
14860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4871                                                        
24860 ivfail = ivfail + 1                                               
      ivcorr = 7                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4871 continue                                                          
      ivtnum = 487                                                      
!                                                                        
!       ****  TEST 487  ****                                             
!                                                                        
      if (iczero) 34870, 4870, 34870                                    
 4870 continue                                                          
      ivcomp = -959/6                                                   
      goto 44870                                                       
34870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44870, 4881, 44870                                    
44870 if (ivcomp + 159) 24870,14870,24870                               
14870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4881                                                        
24870 ivfail = ivfail + 1                                               
      ivcorr = -159                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4881 continue                                                          
      ivtnum = 488                                                      
!                                                                        
!       ****  TEST 488  ****                                             
!                                                                        
      if (iczero) 34880, 4880, 34880                                    
 4880 continue                                                          
      ivcomp = -28606/(-8)                                              
      goto 44880                                                       
34880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44880, 4891, 44880                                    
44880 if (ivcomp - 3575) 24880,14880,24880                              
14880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4891                                                        
24880 ivfail = ivfail + 1                                               
      ivcorr = 3575                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4891 continue                                                          
      ivtnum = 489                                                      
!                                                                        
!       ****  TEST 489  ****                                             
!                                                                        
      if (iczero) 34890, 4890, 34890                                    
 4890 continue                                                          
      ivcomp = -25603/2                                                 
      goto 44890                                                       
34890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44890, 4901, 44890                                    
44890 if (ivcomp + 12801) 24890,14890,24890                             
14890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4901                                                        
24890 ivfail = ivfail + 1                                               
      ivcorr = -12801                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4901 continue                                                          
      ivtnum = 490                                                      
!                                                                        
!       ****  TEST 490  ****                                             
!                                                                        
      if (iczero) 34900, 4900, 34900                                    
 4900 continue                                                          
      ivcomp = -25603/(-10354)                                          
      goto 44900                                                       
34900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44900, 4911, 44900                                    
44900 if (ivcomp - 2) 24900,14900,24900                                 
14900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4911                                                        
24900 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!       ****    END OF TESTS    ****                                     
 4911 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM036" )                          
      end program fm036
