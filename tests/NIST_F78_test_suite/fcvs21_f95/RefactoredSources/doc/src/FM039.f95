      program fm039
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
!         FM039                                                          
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM          INTEGER VARIABLE = ARITHMETIC EXPRESSION            
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR /, INTEGER CONSTANTS AND AN INTEGER VARIABLE.  BOTH      
!      POSITIVE AND NEGATIVE VALUES ARE USED FOR THE INTEGER CONSTANTS   
!      AND THE INTEGER VARIABLE.                                         
!                                                                        
!          THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     
!      AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  
!      IN THE RESULTANT INTEGER VARIABLE.  SOME OF THE TESTS USE PARENS  
!      TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.                   
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!              (1) INTEGER VARIABLE/INTEGER CONSTANT/INTEGER CONSTANT    
!                  INTEGER CONSTANT/INTEGER VARIABLE/INTEGER CONSTANT    
!                  INTEGER CONSTANT/INTEGER CONSTANT/INTEGER VARIABLE    
!              (2) SAME AS (1) BUT WITH PARENTHESES TO GROUP ELEMENTS    
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
!      TEST 552 THROUGH TEST 557 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM             IV = IV/IC/IC.                            
!                                                                        
 5521 continue                                                          
      ivtnum = 552                                                      
!                                                                        
!       ****  TEST 552  ****                                             
!                                                                        
      if (iczero) 35520, 5520, 35520                                    
 5520 continue                                                          
      ivon01 = 24                                                       
      ivcomp = ivon01/3/4                                               
      goto 45520                                                       
35520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45520, 5531, 45520                                    
45520 if (ivcomp - 2) 25520,15520,25520                                 
15520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5531                                                        
25520 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5531 continue                                                          
      ivtnum = 553                                                      
!                                                                        
!       ****  TEST 553  ****                                             
!                                                                        
      if (iczero) 35530, 5530, 35530                                    
 5530 continue                                                          
      ivon01 = 7151                                                     
      ivcomp = ivon01/3/10                                              
      goto 45530                                                       
35530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45530, 5541, 45530                                    
45530 if (ivcomp - 238) 25530,15530,25530                               
15530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5541                                                        
25530 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5541 continue                                                          
      ivtnum = 554                                                      
!                                                                        
!       ****  TEST 554  ****                                             
!                                                                        
      if (iczero) 35540, 5540, 35540                                    
 5540 continue                                                          
      ivon01 = -330                                                     
      ivcomp = ivon01/3/2                                               
      goto 45540                                                       
35540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45540, 5551, 45540                                    
45540 if (ivcomp + 55) 25540,15540,25540                                
15540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5551                                                        
25540 ivfail = ivfail + 1                                               
      ivcorr = -55                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5551 continue                                                          
      ivtnum = 555                                                      
!                                                                        
!       ****  TEST 555  ****                                             
!                                                                        
      if (iczero) 35550, 5550, 35550                                    
 5550 continue                                                          
      ivon01 = 15249                                                    
      ivcomp = ivon01/(-13)/51                                          
      goto 45550                                                       
35550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45550, 5561, 45550                                    
45550 if (ivcomp + 23) 25550,15550,25550                                
15550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5561                                                        
25550 ivfail = ivfail + 1                                               
      ivcorr = -23                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5561 continue                                                          
      ivtnum = 556                                                      
!                                                                        
!       ****  TEST 556  ****                                             
!                                                                        
      if (iczero) 35560, 5560, 35560                                    
 5560 continue                                                          
      ivon01 = -27342                                                   
      ivcomp = ivon01/(-4)/(-3)                                         
      goto 45560                                                       
35560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45560, 5571, 45560                                    
45560 if (ivcomp + 2278) 25560,15560,25560                              
15560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5571                                                        
25560 ivfail = ivfail + 1                                               
      ivcorr = -2278                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5571 continue                                                          
      ivtnum = 557                                                      
!                                                                        
!       ****  TEST 557  ****                                             
!                                                                        
      if (iczero) 35570, 5570, 35570                                    
 5570 continue                                                          
      ivon01 = -27342                                                   
      ivcomp = -ivon01/4/(-3)                                           
      goto 45570                                                       
35570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45570, 5581, 45570                                    
45570 if (ivcomp + 2278) 25570,15570,25570                              
15570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5581                                                        
25570 ivfail = ivfail + 1                                               
      ivcorr = -2278                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 558 THROUGH TEST 563 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM             IV=IC/IV/IC.                              
!                                                                        
 5581 continue                                                          
      ivtnum = 558                                                      
!                                                                        
!       ****  TEST 558  ****                                             
!                                                                        
      if (iczero) 35580, 5580, 35580                                    
 5580 continue                                                          
      ivon02 = 3                                                        
      ivcomp = 24/ivon02/4                                              
      goto 45580                                                       
35580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45580, 5591, 45580                                    
45580 if (ivcomp - 2) 25580,15580,25580                                 
15580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5591                                                        
25580 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5591 continue                                                          
      ivtnum = 559                                                      
!                                                                        
!       ****  TEST 559  ****                                             
!                                                                        
      if (iczero) 35590, 5590, 35590                                    
 5590 continue                                                          
      ivon02 = 3                                                        
      ivcomp = 7151/ivon02/10                                           
      goto 45590                                                       
35590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45590, 5601, 45590                                    
45590 if (ivcomp - 238) 25590,15590,25590                               
15590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5601                                                        
25590 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5601 continue                                                          
      ivtnum = 560                                                      
!                                                                        
!       ****  TEST 560  ****                                             
!                                                                        
      if (iczero) 35600, 5600, 35600                                    
 5600 continue                                                          
      ivon02 = -3                                                       
      ivcomp = 330/ivon02/2                                             
      goto 45600                                                       
35600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45600, 5611, 45600                                    
45600 if (ivcomp +55) 25600,15600,25600                                 
15600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5611                                                        
25600 ivfail = ivfail + 1                                               
      ivcorr = -55                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5611 continue                                                          
      ivtnum = 561                                                      
!                                                                        
!       ****  TEST 561  ****                                             
!                                                                        
      if (iczero) 35610, 5610, 35610                                    
 5610 continue                                                          
      ivon02 = +13                                                      
      ivcomp = 15249/ivon02/(-51)                                       
      goto 45610                                                       
35610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45610, 5621, 45610                                    
45610 if (ivcomp + 23) 25610,15610,25610                                
15610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5621                                                        
25610 ivfail = ivfail + 1                                               
      ivcorr = -23                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5621 continue                                                          
      ivtnum = 562                                                      
!                                                                        
!       ****  TEST 562  ****                                             
!                                                                        
      if (iczero) 35620, 5620, 35620                                    
 5620 continue                                                          
      ivon02 = -4                                                       
      ivcomp = (-27342)/ivon02/(-3)                                     
      goto 45620                                                       
35620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45620, 5631, 45620                                    
45620 if (ivcomp + 2278) 25620,15620,25620                              
15620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5631                                                        
25620 ivfail = ivfail + 1                                               
      ivcorr = -2278                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5631 continue                                                          
      ivtnum = 563                                                      
!                                                                        
!       ****  TEST 563  ****                                             
!                                                                        
      if (iczero) 35630, 5630, 35630                                    
 5630 continue                                                          
      ivon02 = -4                                                       
      ivcomp = -27342/(-ivon02)/(-3)                                    
      goto 45630                                                       
35630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45630, 5641, 45630                                    
45630 if (ivcomp - 2278) 25630,15630,25630                              
15630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5641                                                        
25630 ivfail = ivfail + 1                                               
      ivcorr = 2278                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 564 THROUGH TEST 569 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM             IV = IC/IC/IV.                            
!                                                                        
 5641 continue                                                          
      ivtnum = 564                                                      
!                                                                        
!       ****  TEST 564  ****                                             
!                                                                        
      if (iczero) 35640, 5640, 35640                                    
 5640 continue                                                          
      ivon03 = 4                                                        
      ivcomp = 24/3/ivon03                                              
      goto 45640                                                       
35640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45640, 5651, 45640                                    
45640 if (ivcomp -2) 25640,15640,25640                                  
15640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5651                                                        
25640 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5651 continue                                                          
      ivtnum = 565                                                      
!                                                                        
!       ****  TEST 565  ****                                             
!                                                                        
      if (iczero) 35650, 5650, 35650                                    
 5650 continue                                                          
      ivon03 = 10                                                       
      ivcomp = 7151/3/ivon03                                            
      goto 45650                                                       
35650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45650, 5661, 45650                                    
45650 if (ivcomp - 238) 25650,15650,25650                               
15650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5661                                                        
25650 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5661 continue                                                          
      ivtnum = 566                                                      
!                                                                        
!       ****  TEST 566  ****                                             
!                                                                        
      if (iczero) 35660, 5660, 35660                                    
 5660 continue                                                          
      ivon03 = -2                                                       
      ivcomp = 330/3/ivon03                                             
      goto 45660                                                       
35660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45660, 5671, 45660                                    
45660 if (ivcomp + 55) 25660,15660,25660                                
15660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5671                                                        
25660 ivfail = ivfail + 1                                               
      ivcorr = -55                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5671 continue                                                          
      ivtnum = 567                                                      
!                                                                        
!       ****  TEST 567  ****                                             
!                                                                        
      if (iczero) 35670, 5670, 35670                                    
 5670 continue                                                          
      ivon03 = +51                                                      
      ivcomp = 15249/(-13)/ivon03                                       
      goto 45670                                                       
35670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45670, 5681, 45670                                    
45670 if (ivcomp + 23) 25670,15670,25670                                
15670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5681                                                        
25670 ivfail = ivfail + 1                                               
      ivcorr = -23                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5681 continue                                                          
      ivtnum = 568                                                      
!                                                                        
!       ****  TEST 568  ****                                             
!                                                                        
      if (iczero) 35680, 5680, 35680                                    
 5680 continue                                                          
      ivon03 = -3                                                       
      ivcomp = (-27342)/(-4)/ivon03                                     
      goto 45680                                                       
35680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45680, 5691, 45680                                    
45680 if (ivcomp + 2278) 25680,15680,25680                              
15680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5691                                                        
25680 ivfail = ivfail + 1                                               
      ivcorr = -2278                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5691 continue                                                          
      ivtnum = 569                                                      
!                                                                        
!       ****  TEST 569  ****                                             
!                                                                        
      if (iczero) 35690, 5690, 35690                                    
 5690 continue                                                          
      ivon03 = -3                                                       
      ivcomp = -27342/(-4)/(-ivon03)                                    
      goto 45690                                                       
35690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45690, 5701, 45690                                    
45690 if (ivcomp - 2278) 25690,15690,25690                              
15690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5701                                                        
25690 ivfail = ivfail + 1                                               
      ivcorr = 2278                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 570 AND TEST 571  -   IV =(IV/IC)/IC                         
!                                                                        
 5701 continue                                                          
      ivtnum = 570                                                      
!                                                                        
!       ****  TEST 570  ****                                             
!                                                                        
      if (iczero) 35700, 5700, 35700                                    
 5700 continue                                                          
      ivon01 = 24                                                       
      ivcomp = (ivon01/3)/4                                             
      goto 45700                                                       
35700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45700, 5711, 45700                                    
45700 if (ivcomp -2) 25700,15700,25700                                  
15700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5711                                                        
25700 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5711 continue                                                          
      ivtnum = 571                                                      
!                                                                        
!       ****  TEST 571  ****                                             
!                                                                        
      if (iczero) 35710, 5710, 35710                                    
 5710 continue                                                          
      ivon01 = -330                                                     
      ivcomp = (ivon01/(-3))/4                                          
      goto 45710                                                       
35710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45710, 5721, 45710                                    
45710 if (ivcomp - 27) 25710,15710,25710                                
15710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5721                                                        
25710 ivfail = ivfail + 1                                               
      ivcorr = 27                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 572 AND TEST 573  -  IV= IV/(IC/IC)                          
!                                                                        
 5721 continue                                                          
      ivtnum = 572                                                      
!                                                                        
!       ****  TEST 572  ****                                             
!                                                                        
      if (iczero) 35720, 5720, 35720                                    
 5720 continue                                                          
      ivon01 = 24                                                       
      ivcomp = ivon01/(8/4)                                             
      goto 45720                                                       
35720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45720, 5731, 45720                                    
45720 if (ivcomp - 12) 25720,15720,25720                                
15720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5731                                                        
25720 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5731 continue                                                          
      ivtnum = 573                                                      
!                                                                        
!       ****  TEST 573  ****                                             
!                                                                        
      if (iczero) 35730, 5730, 35730                                    
 5730 continue                                                          
      ivon01 = -7154                                                    
      ivcomp = -ivon01/((-26)/5)                                        
      goto 45730                                                       
35730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45730, 5741, 45730                                    
45730 if (ivcomp + 1430) 25730,15730,25730                              
15730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5741                                                        
25730 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 574 AND TEST 575  -  IV=(IC/IV)/IC                           
!                                                                        
 5741 continue                                                          
      ivtnum = 574                                                      
!                                                                        
!       ****  TEST 574  ****                                             
!                                                                        
      if (iczero) 35740, 5740, 35740                                    
 5740 continue                                                          
      ivon02 = 3                                                        
      ivcomp = (24/ivon02)/4                                            
      goto 45740                                                       
35740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45740, 5751, 45740                                    
45740 if (ivcomp -2) 25740,15740,25740                                  
15740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5751                                                        
25740 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5751 continue                                                          
      ivtnum = 575                                                      
!                                                                        
!       ****  TEST 575  ****                                             
!                                                                        
      if (iczero) 35750, 5750, 35750                                    
 5750 continue                                                          
      ivon02 = -3                                                       
      ivcomp = (-330/ivon02)/(-4)                                       
      goto 45750                                                       
35750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45750, 5761, 45750                                    
45750 if (ivcomp + 27) 25750,15750,25750                                
15750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5761                                                        
25750 ivfail = ivfail + 1                                               
      ivcorr = -27                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 576 AND TEST 577  -  IV=IC/(IV/IC)                           
!                                                                        
 5761 continue                                                          
      ivtnum = 576                                                      
!                                                                        
!       ****  TEST 576  ****                                             
!                                                                        
      if (iczero) 35760, 5760, 35760                                    
 5760 continue                                                          
      ivon02 = 8                                                        
      ivcomp = 24/(ivon02/4)                                            
      goto 45760                                                       
35760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45760, 5771, 45760                                    
45760 if (ivcomp - 12) 25760,15760,25760                                
15760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5771                                                        
25760 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5771 continue                                                          
      ivtnum = 577                                                      
!                                                                        
!       ****  TEST 577  ****                                             
!                                                                        
      if (iczero) 35770, 5770, 35770                                    
 5770 continue                                                          
      ivon02 = -26                                                      
      ivcomp = 7154/((-ivon02)/(-5))                                    
      goto 45770                                                       
35770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45770, 5781, 45770                                    
45770 if (ivcomp + 1430) 25770,15770,25770                              
15770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5781                                                        
25770 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 578 AND TEST 579  -  IV=(IC/IC)/IV                           
!                                                                        
 5781 continue                                                          
      ivtnum = 578                                                      
!                                                                        
!       ****  TEST 578  ****                                             
!                                                                        
      if (iczero) 35780, 5780, 35780                                    
 5780 continue                                                          
      ivon03 = 4                                                        
      ivcomp = (24/3)/ivon03                                            
      goto 45780                                                       
35780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45780, 5791, 45780                                    
45780 if (ivcomp - 2) 25780,15780,25780                                 
15780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5791                                                        
25780 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5791 continue                                                          
      ivtnum = 579                                                      
!                                                                        
!       ****  TEST 579  ****                                             
!                                                                        
      if (iczero) 35790, 5790, 35790                                    
 5790 continue                                                          
      ivon03 = -4                                                       
      ivcomp = (330/(-3))/ivon03                                        
      goto 45790                                                       
35790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45790, 5801, 45790                                    
45790 if (ivcomp - 27) 25790,15790,25790                                
15790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5801                                                        
25790 ivfail = ivfail + 1                                               
      ivcorr = 27                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 580 AND TEST 581  -  IV= IC/(IC/IV)                          
!                                                                        
 5801 continue                                                          
      ivtnum = 580                                                      
!                                                                        
!       ****  TEST 580  ****                                             
!                                                                        
      if (iczero) 35800, 5800, 35800                                    
 5800 continue                                                          
      ivon03 = 4                                                        
      ivcomp = 24/(8/ivon03)                                            
      goto 45800                                                       
35800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45800, 5811, 45800                                    
45800 if (ivcomp - 12) 25800,15800,25800                                
15800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5811                                                        
25800 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5811 continue                                                          
      ivtnum = 581                                                      
!                                                                        
!       ****  TEST 581  ****                                             
!                                                                        
      if (iczero) 35810, 5810, 35810                                    
 5810 continue                                                          
      ivon03 = -5                                                       
      ivcomp = -7154/((-26)/ivon03)                                     
      goto 45810                                                       
35810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45810, 5821, 45810                                    
45810 if (ivcomp + 1430) 25810,15810,25810                              
15810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5821                                                        
25810 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!       ****    END OF TESTS    ****                                     
 5821 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM039" )                          
      end program fm039
