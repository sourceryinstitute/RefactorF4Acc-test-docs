      program fm060
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvcomp
      real :: rvon01
      real :: rvcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM060                                                             
!                                                                        
!          THIS ROUTINE CONTAINS BASIC ARITHMETIC IF STATEMENT TESTS FOR 
!      THE FORMAT                                                        
!                                                                        
!                    IF (E) K1,K2,K3                                     
!                                                                        
!      WHERE E IS A SIMPLE REAL EXPRESSION OF THE FORM                   
!                                                                        
!             REAL VARIABLE                                              
!             REAL VARIABLE - REAL CONSTANT                              
!             REAL VARIABLE + REAL CONSTANT                              
!                                                                        
!      AND K1, K2 AND K3 ARE STATEMENT LABELS.                           
!                                                                        
!          THIS ROUTINE ALSO TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF   
!      THE FORM                                                          
!                   REAL VARIABLE = REAL CONSTANT                        
!                   REAL VARIABLE = REAL VARIABLE                        
!                   REAL VARIABLE = -REAL VARIABLE                       
!                                                                        
!      THE REAL CONSTANTS AND REAL VARIABLES CONTAIN BOTH POSITIVE AND   
!      NEGATIVE VALUES.                                                  
!                                                                        
!          A REAL DATUM IS A PROCESSOR APPROXIMATION TO THE VALUE OF A   
!      REAL NUMBER.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES.   
!                                                                        
!          A BASIC REAL CONSTANT IS WRITTEN AS AN INTEGER PART, A DECIMAL
!      POINT, AND A DECIMAL FRACTION PART IN THAT ORDER.  BOTH THE       
!      INTEGER PART AND THE DECIMAL PART ARE STRINGS OF DIGITS; EITHER   
!      ONE OF THESE STRINGS MAY BE EMPTY BUT NOT BOTH.  THE CONSTANT IS  
!      AN APPROXIMATION TO THE DIGIT STRING INTERPRETED AS A DECIMAL     
!      NUMERAL.                                                          
!                                                                        
!          A DECIMAL EXPONENT IS WRITTEN AS THE LETTER E, FOLLOWED BY AN 
!      OPTIONALLY SIGNED INTEGER CONSTANT.                               
!                                                                        
!          A REAL CONSTANT IS INDICATED BY WRITING A BASIC REAL CONSTANT,
!      A BASIC REAL CONSTANT FOLLOWED BY A DECIMAL EXPONENT, OR AN       
!      INTEGER CONSTANT FOLLOWED BY A DECIMAL EXPONENT.                  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.4, REAL TYPE                                         
!         SECTION 4.4.1, REAL CONSTANT                                   
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  
!         SECTION 11.4, ARITHMETIC IF STATEMENT                          
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
!          ARITHMETIC IF STATEMENT                                       
!                                                                        
!      TEST 1 THROUGH TEST 3 CONTAIN BASIC ARITHMETIC IF STATEMENT TESTS 
!      WITH A REAL VARIABLE AS ARITHMETIC EXPRESSION.                    
!                                                                        
   11 continue                                                          
      ivtnum =   1                                                      
!                                                                        
!       ****  TEST   1  ****                                             
!          TEST 001  - LESS THAN ZERO BRANCH EXPECTED                    
!                                                                        
      if (iczero) 30010,   10, 30010                                    
   10 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = -1.0                                                     
      if (rvon01)  12,40010, 40010                                      
   12 rvcomp = rvon01                                                   
      goto 40010                                                       
30010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40010,   21, 40010                                    
40010 if (rvcomp) 10010,20010,20010                                     
10010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   21                                                        
20010 ivfail = ivfail + 1                                               
      rvcorr = -1.0                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   21 continue                                                          
      ivtnum =   2                                                      
!                                                                        
!       ****  TEST   2  ****                                             
!          TEST 002  -  EQUAL TO ZERO BRANCH EXPECTED                    
!                                                                        
      if (iczero) 30020,   20, 30020                                    
   20 continue                                                          
      rvcomp = 1.0                                                      
      rvon01 = 0.0                                                      
      if (rvon01) 40020,22,40020                                        
   22 rvcomp = rvon01                                                   
      goto 40020                                                       
30020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40020,   31, 40020                                    
40020 if (rvcomp)  20020,10020,20020                                    
10020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   31                                                        
20020 ivfail = ivfail + 1                                               
      rvcorr = 0.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   31 continue                                                          
      ivtnum =   3                                                      
!                                                                        
!       ****  TEST   3  ****                                             
!          TEST 003  -  GREATER THAN ZERO BRANCH EXPECTED                
!                                                                        
      if (iczero) 30030,   30, 30030                                    
   30 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = 1.0                                                      
      if (rvon01) 40030,40030,32                                        
   32 rvcomp = rvon01                                                   
      goto 40030                                                       
30030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40030,   41, 40030                                    
40030 if (rvcomp)  20030,20030,10030                                    
10030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   41                                                        
20030 ivfail = ivfail + 1                                               
      rvcorr = 1.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   41 continue                                                          
      ivtnum =   4                                                      
!                                                                        
!       ****  TEST   4  ****                                             
!      TEST 004  - BASIC IF STATEMENTS TEST                              
!            THESE IF STATEMENTS ARE USED IN REAL VARIABLE TEST          
!            VERIFICATION.  THE ARITHMETIC EXPRESSIONS ARE OF THE FORM   
!                    REAL VARIABLE - REAL CONSTANT                       
!                                                                        
      if (iczero) 30040,   40, 30040                                    
   40 continue                                                          
      rvcomp = 4.0                                                      
      rvon01 = 1.0                                                      
      if (rvon01 - .99995) 40040,42,42                                  
   42 if (rvon01 - 1.0005) 43,43,40040                                  
   43 rvcomp = 0.0                                                      
      goto 40040                                                       
30040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40040,   51, 40040                                    
40040 if (rvcomp) 20040,10040,20040                                     
10040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   51                                                        
20040 ivfail = ivfail + 1                                               
      rvcorr = 0.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   51 continue                                                          
      ivtnum =   5                                                      
!                                                                        
!       ****  TEST   5  ****                                             
!      TEST 005  -  BASIC IF STATEMENTS TEST                             
!            THESE IF STATEMENTS ARE USED IN REAL VARIABLE TEST          
!            VERIFICATION.  THE ARITHMETIC EXPRESSIONS ARE OF THE FORM   
!                    REAL VARIABLE + REAL CONSTANT                       
!                                                                        
      if (iczero) 30050,   50, 30050                                    
   50 continue                                                          
      rvcomp = -1.0                                                     
      rvon01 = -1.0                                                     
      if (rvon01 + 1.0005) 40050,52,52                                  
   52 if (rvon01 + .99995) 53,53,40050                                  
   53 rvcomp = 0.0                                                      
      goto 40050                                                       
30050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40050,   61, 40050                                    
40050 if (rvcomp) 20050,10050,20050                                     
10050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   61                                                        
20050 ivfail = ivfail + 1                                               
      rvcorr = 0.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
!                                                                        
!         ARITHMETIC ASSIGNMENT STATEMENT                                
!                                                                        
!                                                                        
!      TEST 006 THROUGH TEST 025 CONTAIN ARITHMETIC ASSIGNMENT           
!      STATEMENTS OF THE FORM                                            
!               REAL VARIABLE = REAL CONSTANT                            
!                                                                        
!           THE THREE TYPES OF REAL CONSTANTS ARE TESTED WITH POSITIVE   
!      AND NEGATIVE VALUES FOR THE CONSTANTS, AND POSITIVE AND NEGATIVE  
!      EXPONENTS.                                                        
!                                                                        
!      TEST 006 THROUGH TEST 011 - CONSTANT IS BASIC REAL CONSTANT       
!                                                                        
   61 continue                                                          
      ivtnum =   6                                                      
!                                                                        
!       ****  TEST   6  ****                                             
!                                                                        
      if (iczero) 30060,   60, 30060                                    
   60 continue                                                          
      rvcomp = 2.0                                                      
      goto 40060                                                       
30060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40060,   71, 40060                                    
40060 if (rvcomp - 1.9995) 20060,10060,40061                            
40061 if (rvcomp - 2.0005) 10060,10060,20060                            
10060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   71                                                        
20060 ivfail = ivfail + 1                                               
      rvcorr = 2.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   71 continue                                                          
      ivtnum =   7                                                      
!                                                                        
!       ****  TEST   7  ****                                             
!                                                                        
      if (iczero) 30070,   70, 30070                                    
   70 continue                                                          
      rvcomp = 44.5                                                     
      goto 40070                                                       
30070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40070,   81, 40070                                    
40070 if (rvcomp - 44.495) 20070,10070,40071                            
40071 if (rvcomp - 45.505) 10070,10070,20070                            
10070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   81                                                        
20070 ivfail = ivfail + 1                                               
      rvcorr = 44.5                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   81 continue                                                          
      ivtnum =   8                                                      
!                                                                        
!       ****  TEST   8  ****                                             
!                                                                        
      if (iczero) 30080,   80, 30080                                    
   80 continue                                                          
      rvcomp = -2.0                                                     
      goto 40080                                                       
30080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40080,   91, 40080                                    
40080 if (rvcomp + 2.0005) 20080,10080,40081                            
40081 if (rvcomp + 1.9995) 10080,10080,20080                            
10080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   91                                                        
20080 ivfail = ivfail + 1                                               
      rvcorr = -2.0                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
   91 continue                                                          
      ivtnum =   9                                                      
!                                                                        
!       ****  TEST   9  ****                                             
!                                                                        
      if (iczero) 30090,   90, 30090                                    
   90 continue                                                          
      rvcomp = 65001.                                                   
      goto 40090                                                       
30090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40090,  101, 40090                                    
40090 if (rvcomp - 64996.) 20090,10090,40091                            
40091 if (rvcomp - 65006.) 10090,10090,20090                            
10090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  101                                                        
20090 ivfail = ivfail + 1                                               
      rvcorr = 65001.                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  101 continue                                                          
      ivtnum =  10                                                      
!                                                                        
!       ****  TEST  10  ****                                             
!                                                                        
      if (iczero) 30100,  100, 30100                                    
  100 continue                                                          
      rvcomp = .65001                                                   
      goto 40100                                                       
30100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40100,  111, 40100                                    
40100 if (rvcomp - .64996) 20100,10100,40101                            
40101 if (rvcomp - .65006) 10100,10100,20100                            
10100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  111                                                        
20100 ivfail = ivfail + 1                                               
      rvcorr = .65001                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  111 continue                                                          
      ivtnum =  11                                                      
!                                                                        
!       ****  TEST  11  ****                                             
!                                                                        
      if (iczero) 30110,  110, 30110                                    
  110 continue                                                          
      rvcomp = -.33333                                                  
      goto 40110                                                       
30110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40110,  121, 40110                                    
40110 if (rvcomp + .33338) 20110,10110,40111                            
40111 if (rvcomp + .33328) 10110,10110,20110                            
10110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  121                                                        
20110 ivfail = ivfail + 1                                               
      rvcorr = -.33333                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
!                                                                        
!      TEST 012 THROUGH TEST 19 - REAL CONSTANT IS BASIC REAL CONSTANT   
!                               - FOLLOWED BY DECIMAL EXPONENT           
!                                                                        
  121 continue                                                          
      ivtnum =  12                                                      
!                                                                        
!       ****  TEST  12  ****                                             
!                                                                        
      if (iczero) 30120,  120, 30120                                    
  120 continue                                                          
      rvcomp = .2e+1                                                    
      goto 40120                                                       
30120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40120,  131, 40120                                    
40120 if (rvcomp - 1.9995) 20120,10120,40121                            
40121 if (rvcomp - 2.0005) 10120,10120,20120                            
10120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  131                                                        
20120 ivfail = ivfail + 1                                               
      rvcorr = 2.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  131 continue                                                          
      ivtnum =  13                                                      
!                                                                        
!       ****  TEST  13  ****                                             
!                                                                        
      if (iczero) 30130,  130, 30130                                    
  130 continue                                                          
      rvcomp = 2.0e+0                                                   
      goto 40130                                                       
30130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40130,  141, 40130                                    
40130 if (rvcomp - 1.9995) 20130,10130,40131                            
40131 if (rvcomp - 2.0005) 10130,10130,20130                            
10130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  141                                                        
20130 ivfail = ivfail + 1                                               
      rvcorr = 2.0                                                      
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  141 continue                                                          
      ivtnum =  14                                                      
!                                                                        
!       ****  TEST  14  ****                                             
!                                                                        
      if (iczero) 30140,  140, 30140                                    
  140 continue                                                          
      rvcomp = 445.0e-01                                                
      goto 40140                                                       
30140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40140,  151, 40140                                    
40140 if (rvcomp - 44.495) 20140,10140,40141                            
40141 if (rvcomp - 44.505) 10140,10140,20140                            
10140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  151                                                        
20140 ivfail = ivfail + 1                                               
      rvcorr = 44.5                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  151 continue                                                          
      ivtnum =  15                                                      
!                                                                        
!       ****  TEST  15  ****                                             
!                                                                        
      if (iczero) 30150,  150, 30150                                    
  150 continue                                                          
      rvcomp = 4.450e1                                                  
      goto 40150                                                       
30150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40150,  161, 40150                                    
40150 if (rvcomp - 44.495) 20150,10150,40151                            
40151 if (rvcomp - 44.505) 10150,10150,20150                            
10150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  161                                                        
20150 ivfail = ivfail + 1                                               
      rvcorr = 44.5                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  161 continue                                                          
      ivtnum =  16                                                      
!                                                                        
!       ****  TEST  16  ****                                             
!                                                                        
      if (iczero) 30160,  160, 30160                                    
  160 continue                                                          
      rvcomp = 2.e+15                                                   
      goto 40160                                                       
30160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40160,  171, 40160                                    
40160 if (rvcomp - 1.9995e+15) 20160,10160,40161                        
40161 if (rvcomp - 2.0005e+15) 10160,10160,20160                        
10160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  171                                                        
20160 ivfail = ivfail + 1                                               
      rvcorr = 2.0e+15                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  171 continue                                                          
      ivtnum =  17                                                      
!                                                                        
!       ****  TEST  17  ****                                             
!                                                                        
      if (iczero) 30170,  170, 30170                                    
  170 continue                                                          
      rvcomp = 44.5e-15                                                 
      goto 40170                                                       
30170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40170,  181, 40170                                    
40170 if (rvcomp - 44.495e-15) 20170,10170,40171                        
40171 if (rvcomp - 44.505e-15) 10170,10170,20170                        
10170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  181                                                        
20170 ivfail = ivfail + 1                                               
      rvcorr = 44.5e-15                                                 
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  181 continue                                                          
      ivtnum =  18                                                      
!                                                                        
!       ****  TEST  18  ****                                             
!                                                                        
      if (iczero) 30180,  180, 30180                                    
  180 continue                                                          
      rvcomp = -4.45e0                                                  
      goto 40180                                                       
30180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40180,  191, 40180                                    
40180 if (rvcomp + 4.4505) 20180,10180,40181                            
40181 if (rvcomp + 4.4495) 10180,10180,20180                            
10180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  191                                                        
20180 ivfail = ivfail + 1                                               
      rvcorr = -4.45                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  191 continue                                                          
      ivtnum =  19                                                      
!                                                                        
!       ****  TEST  19  ****                                             
!                                                                        
      if (iczero) 30190,  190, 30190                                    
  190 continue                                                          
      rvcomp = -6511.8e-0                                               
      goto 40190                                                       
30190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40190,  201, 40190                                    
40190 if (rvcomp + 6512.3) 20190,10190,40191                            
40191 if (rvcomp + 6511.3) 10190,10190,20190                            
10190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  201                                                        
20190 ivfail = ivfail + 1                                               
      rvcorr = -6511.8                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
!                                                                        
!      TEST 020 THROUGH TEST 025 - INTEGER CONSTANT FOLLOWED             
!                                - BY A DECIMAL EXPONENT                 
!                                                                        
  201 continue                                                          
      ivtnum =  20                                                      
!                                                                        
!       ****  TEST  20  ****                                             
!                                                                        
      if (iczero) 30200,  200, 30200                                    
  200 continue                                                          
      rvcomp = 2e+1                                                     
      goto 40200                                                       
30200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40200,  211, 40200                                    
40200 if (rvcomp - 19.995) 20200,10200,40201                            
40201 if (rvcomp - 20.005) 10200,10200,20200                            
10200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  211                                                        
20200 ivfail = ivfail + 1                                               
      rvcorr = 20.0                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  211 continue                                                          
      ivtnum =  21                                                      
!                                                                        
!       ****  TEST  21  ****                                             
!                                                                        
      if (iczero) 30210,  210, 30210                                    
  210 continue                                                          
      rvcomp = 445e-02                                                  
      goto 40210                                                       
30210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40210,  221, 40210                                    
40210 if (rvcomp - 4.4495) 20210,10210,40211                            
40211 if (rvcomp - 4.4505) 10210,10210,20210                            
10210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  221                                                        
20210 ivfail = ivfail + 1                                               
      rvcorr = 4.45                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  221 continue                                                          
      ivtnum =  22                                                      
!                                                                        
!       ****  TEST  22  ****                                             
!                                                                        
      if (iczero) 30220,  220, 30220                                    
  220 continue                                                          
      rvcomp = 7e3                                                      
      goto 40220                                                       
30220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40220,  231, 40220                                    
40220 if (rvcomp - 6999.0) 20220,10220,40221                            
40221 if (rvcomp - 7001.0) 10220,10220,20220                            
10220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  231                                                        
20220 ivfail = ivfail + 1                                               
      rvcorr = 7000.0                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  231 continue                                                          
      ivtnum =  23                                                      
!                                                                        
!       ****  TEST  23  ****                                             
!                                                                        
      if (iczero) 30230,  230, 30230                                    
  230 continue                                                          
      rvcomp = 214e0                                                  
      goto 40230                                                       
30230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40230,  241, 40230                                    
40230 if (rvcomp - 213.95) 20230,10230,40231                            
40231 if (rvcomp - 214.05) 10230,10230,20230                            
10230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  241                                                        
20230 ivfail = ivfail + 1                                               
      rvcorr = 214.0                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  241 continue                                                          
      ivtnum =  24                                                      
!                                                                        
!       ****  TEST  24  ****                                             
!                                                                        
      if (iczero) 30240,  240, 30240                                    
  240 continue                                                          
      rvcomp = -3276e+6                                                 
      goto 40240                                                       
30240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40240,  251, 40240                                    
40240 if (rvcomp + .32765e+10) 20240,10240,40241                        
40241 if (rvcomp + .32755e+10) 10240,10240,20240                        
10240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  251                                                        
20240 ivfail = ivfail + 1                                               
      rvcorr = -3276e+6                                                 
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  251 continue                                                          
      ivtnum =  25                                                      
!                                                                        
!       ****  TEST  25  ****                                             
!                                                                        
      if (iczero) 30250,  250, 30250                                    
  250 continue                                                          
      rvcomp = -7e3                                                     
      goto 40250                                                       
30250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40250,  261, 40250                                    
40250 if (rvcomp + 7001.)  20250,10250,40251                            
40251 if (rvcomp + 6999.) 10250,10250,20250                             
10250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  261                                                        
20250 ivfail = ivfail + 1                                               
      rvcorr = -7000.0                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
!                                                                        
!      TEST 026 THROUGH TEST 028 CONTAIN ARITHMETIC ASSIGNMENT STATEMENT 
!      OF THE FORM            REAL VARIABLE = REAL VARIABLE              
!                                                                        
  261 continue                                                          
      ivtnum =  26                                                      
!                                                                        
!       ****  TEST  26  ****                                             
!                                                                        
      if (iczero) 30260,  260, 30260                                    
  260 continue                                                          
      rvon01 = .2e+1                                                    
      rvcomp = rvon01                                                   
      goto 40260                                                       
30260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40260,  271, 40260                                    
40260 if (rvcomp - 1.9995) 20260,10260,40261                            
40261 if (rvcomp - 2.0005) 10260,10260,20260                            
10260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  271                                                        
20260 ivfail = ivfail + 1                                               
      rvcorr = 20.0                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  271 continue                                                          
      ivtnum =  27                                                      
!                                                                        
!       ****  TEST  27  ****                                             
!                                                                        
      if (iczero) 30270,  270, 30270                                    
  270 continue                                                          
      rvon01 = -445.e-01                                                
      rvcomp = rvon01                                                   
      goto 40270                                                       
30270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40270,  281, 40270                                    
40270 if (rvcomp + 44.505) 20270,10270,40271                            
40271 if (rvcomp + 44.495) 10270,10270,20270                            
10270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  281                                                        
20270 ivfail = ivfail + 1                                               
      rvcorr = -44.5                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  281 continue                                                          
      ivtnum =  28                                                      
!                                                                        
!       ****  TEST  28  ****                                             
!                                                                        
      if (iczero) 30280,  280, 30280                                    
  280 continue                                                          
      rvon01 = 7e3                                                      
      rvcomp = rvon01                                                   
      goto 40280                                                       
30280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40280,  291, 40280                                    
40280 if (rvcomp - 6999.0) 20280,10280,40281                            
40281 if (rvcomp-7001.0) 10280,10280,20280                              
10280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  291                                                        
20280 ivfail = ivfail + 1                                               
      rvcorr = 7000.0                                                   
!                                                                        
!      TEST 029 THROUGH TEST 031 CONTAIN ARITHMETIC ASSIGNMENT STATEMENT 
!      OF THE FORM            REAL VARIABLE = - REAL VARIABLE            
!                                                                        
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  291 continue                                                          
      ivtnum =  29                                                      
!                                                                        
!       ****  TEST  29  ****                                             
!                                                                        
      if (iczero) 30290,  290, 30290                                    
  290 continue                                                          
      rvon01 = .2e+1                                                    
      rvcomp = -rvon01                                                  
      goto 40290                                                       
30290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40290,  301, 40290                                    
40290 if (rvcomp + 2.0005) 20290,10290,40291                            
40291 if (rvcomp + 1.9995) 10290,10290,20290                            
10290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  301                                                        
20290 ivfail = ivfail + 1                                               
      rvcorr = -2.0                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  301 continue                                                          
      ivtnum =  30                                                      
!                                                                        
!       ****  TEST  30  ****                                             
!                                                                        
      if (iczero) 30300,  300, 30300                                    
  300 continue                                                          
      rvon01 = -445.e-01                                                
      rvcomp = -rvon01                                                  
      goto 40300                                                       
30300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40300,  311, 40300                                    
40300 if (rvcomp - 44.495) 20300,10300,40301                            
40301 if (rvcomp - 44.505) 10300,10300,20300                            
10300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  311                                                        
20300 ivfail = ivfail + 1                                               
      rvcorr = 44.5                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  311 continue                                                          
      ivtnum =  31                                                      
!                                                                        
!       ****  TEST  31  ****                                             
!                                                                        
      if (iczero) 30310,  310, 30310                                    
  310 continue                                                          
      rvon01 = -.44559e1                                                
      rvcomp = -rvon01                                                  
      goto 40310                                                       
30310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40310,  321, 40310                                    
40310 if (rvcomp - 4.4554) 20310,10310,40311                            
40311 if (rvcomp - 4.4564) 10310,10310,20310                            
10310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  321                                                        
20310 ivfail = ivfail + 1                                               
      rvcorr = 4.4559                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
!       ****    END OF TESTS    ****                                     
  321 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM060" )                          
      end program fm060
