      program fm201
!                                                                        
!                                                                        
!         THIS ROUTINE VERIFIES THAT                                     
!                                                                        
!         (1)  THE VALUE OF A SIGNED ZERO IS THE SAME AS THE VALUE OF    
!              AN UNSIGNED ZERO FOR INTEGER AND REAL VARIABLES.          
!                                                                        
!         (2)  A BASIC REAL CONSTANT MAY BE WRITTEN WITH MORE DIGITS     
!              THAN A PROCESSOR WILL USE TO APPROXIMATE THE VALUE OF     
!              THE CONSTANT.                                             
!                                                                        
!         (3)  AN IMPLICIT STATEMENT CAN BE USED TO CHANGE THE DEFAULT   
!              IMPLICIT INTEGER AND REAL TYPING.                         
!                                                                        
!         (4)  THE IMPLICIT INTEGER AND REAL TYPING OF AN IMPLICIT       
!              STATEMENT MAY BE OVERRIDDEN BY THE APPEARANCE OF A        
!              VARIABLE NAME IN A TYPE-STATEMENT.                        
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.1.3, DATA TYPE PROPERTIES                            
!         SECTION 4.4.1, BASIC REAL CONSTANT                             
!         SECTION 6.1.5, INTEGER DIVISION                                
!         SECTION 8.4,   TYPE-STATEMENTS                                 
!         SECTION 8.5,   IMPLICIT STATEMENT                              
!                                                                        
!                                                                        
!      ******************************************************************
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   
!      X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 
!      FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       
!      ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT
!      ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   
!      OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING
!      THE RESULT OF EXECUTING THESE TESTS.                              
!                                                                        
!      THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      
!      FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        
!                                                                        
!            SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!      ******************************************************************
!                                                                        
!                                                                        
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      real :: go
      real :: to
      real :: rvon01
      real :: rvon02
      real :: rvon03
      real :: rvon04
      real :: rvon05
      real :: rvon06
      real :: rvcomp
      real :: rvcorr
      integer :: yvin01
      integer :: vvin01
      integer :: wvin01
      integer :: xvin01
      real :: mvin01
      real :: rvtn01
      real :: rvtn02
      real :: rvtn03
      real :: yvtn02
      integer :: ivtn01
      integer :: ivtn02
      integer :: mvtn02
!         THE ABOVE THREE STATEMENTS ARE REFERENCED IN TESTS 29 THRU 35. 
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION.                                           
!                                                                        
!      INITIALIZE CONSTANTS                                              
!      ********************                                              
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010     THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD.
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD.
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass = 0                                                        
      ivfail = 0                                                        
      ivdele = 0                                                        
      iczero = 0                                                        
!                                                                        
!      WRITE OUT PAGE HEADERS                                            
!                                                                        
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90008)                                                 
      write (i02,90004)                                                 
      write (i02,90010)                                                 
      write (i02,90004)                                                 
      write (i02,90016)                                                 
      write (i02,90001)                                                 
      write (i02,90004)                                                 
      write (i02,90012)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
!                                                                        
!                                                                        
!         TEST 14 THROUGH TEST 17 COMPARE INTEGER VARIABLES WHICH ARE    
!      SET TO SIGNED ZERO AND UNSIGNED ZERO VALUES BY THE FOLLOWING      
!      STATEMENTS                                                        
!                                                                        
         ivon01 = 0                                                     
         ivon02 = -0                                                    
         ivon03 = +0                                                    
!                                                                        
!      REFERENCE   X3.9-1978, SECTION 4.1.3, DATA TYPE PROPERTIES        
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 014  ****                         
!                                                                        
!         COMPARE 0 TO -0                                                
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (ivon01  ==  ivon02) ivcomp = 0                                
40140 if (ivcomp) 20140, 10140, 20140                                   
30140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10140, 0151, 20140                                    
10140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0151                                                        
20140 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0151 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 015  ****                         
!                                                                        
!         COMPARE 0 TO +0                                                
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (ivon01  ==  ivon03) ivcomp = 0                                
40150 if (ivcomp) 20150, 10150, 20150                                   
30150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10150, 0161, 20150                                    
10150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0161                                                        
20150 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0161 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 016  ****                         
!                                                                        
!         COMPARE -0 TO +0                                               
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (ivon02  ==  ivon03) ivcomp = 0                                
40160 if (ivcomp) 20160, 10160, 20160                                   
30160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10160, 0171, 20160                                    
10160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0171                                                        
20160 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0171 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 017  ****                         
!                                                                        
!         MINUS ZERO (-0) SHOULD NOT BE LESS THAN PLUS ZERO (+0)         
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (ivon02  <  ivon03) goto 20170                               
      ivcomp = 0                                                        
      goto 10170                                                       
30170 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10170, 0181, 20170                                    
10170 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0181                                                        
20170 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0181 continue                                                          
!                                                                        
!         TEST 18 THROUGH TEST 24 COMPARE REAL VARIABLES WHICH ARE SET   
!      TO SIGNED ZERO AND UNSIGNED ZERO VALUES BY THE FOLLOWING          
!      STATEMENTS                                                        
!                                                                        
         rvon01 = 0.0                                                   
         rvon02 = -0.0                                                  
         rvon03 = +0.0                                                  
         rvon04 = -0.0e+01                                              
         rvon05 = -0e+10                                                
!                                                                        
!      REFERENCE   X3.9-1978, SECTION 4.1.3, DATA TYPE PROPERTIES        
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 018  ****                         
!                                                                        
!         COMPARE 0.0 TO -0.0                                            
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon01  ==  rvon02) ivcomp = 0                                
40180 if (ivcomp) 20180, 10180, 20180                                   
30180 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10180, 0191, 20180                                    
10180 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0191                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0191 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 019  ****                         
!                                                                        
!         COMPARE 0.0 TO +0.0                                            
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon01  ==  rvon03)  ivcomp = 0                               
40190 if (ivcomp) 20190, 10190, 20190                                   
30190 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10190, 0201, 20190                                    
10190 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0201                                                        
20190 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0201 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 020  ****                         
!                                                                        
!         COMPARE -0.0 TO +0.0                                           
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon02  ==  rvon03) ivcomp = 0                                
40200 if (ivcomp) 20200, 10200, 20200                                   
30200 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10200, 0211, 20200                                    
10200 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0211                                                        
20200 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0211 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 021  ****                         
!                                                                        
!         MINUS ZERO (-0.0) SHOULD NOT BE LESS THAN PLUS ZERO (+0.0)     
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon02  <  rvon03) goto 20210                               
      ivcomp = 0                                                        
      goto 10210                                                       
30210 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10210, 0221, 20210                                    
10210 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0221                                                        
20210 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0221 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 022  ****                         
!                                                                        
!         COMPARE -0.0E+01 TO 0.0                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon04  ==  rvon01) ivcomp = 0                                
40220 if (ivcomp) 20220, 10220, 20220                                   
30220 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10220, 0231, 20220                                    
10220 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0231                                                        
20220 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0231 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 023  ****                         
!                                                                        
!         COMPARE -0E+10 TO 0.0                                          
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon05  ==  rvon01) ivcomp = 0                                
40230 if (ivcomp) 20230, 10230, 20230                                   
30230 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10230, 0241, 20230                                    
10230 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0241                                                        
20230 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0241 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 024  ****                         
!                                                                        
!         COMPARE -0E+10 TO +0.0                                         
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (rvon05  /=  rvon03) goto 20240                               
      ivcomp = 0                                                        
      goto 10240                                                       
30240 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10240, 0251, 20240                                    
10240 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0251                                                        
20240 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0251 continue                                                          
!                                                                        
!         TEST 25 THROUGH TEST 28 VERIFY THAT A BASIC REAL CONSTANT MAY  
!      BE WRITTEN WITH MORE DIGITS THAN A PROCESSOR WILL USE TO APPROXI- 
!      MATE THE VALUE OF THE CONSTANT.                                   
!                                                                        
!      REFERENCE   X3.9-1978, SECTION 4.4.1, BASIC REAL CONSTANT         
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 025  ****                         
!                                                                        
!         EIGHT DIGITS IN BASIC REAL CONSTANT                            
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      rvon06 = 0.0                                                      
      rvcomp = 0.0                                                      
      rvon06 = 3.1561234                                                
      rvcomp = rvon06                                                   
      rvcorr = 3.1561                                                   
40250 if (rvcomp - 3.1556) 20250, 10250, 40251                          
40251 if (rvcomp - 3.1566) 10250, 10250, 20250                          
30250 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10250, 0261, 20250                                    
10250 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0261                                                        
20250 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0261 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 026  ****                         
!                                                                        
!         EIGHT DIGITS IN BASIC REAL CONSTANT PLUS A REAL EXPONENT.      
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      rvon06 = 0.0                                                      
      rvcomp = 0.0                                                      
      rvon06 = .31561234e+01                                            
      rvcomp = rvon06                                                   
      rvcorr = 3.1561                                                   
40260 if (rvcomp - 3.1556) 20260, 10260, 40261                          
40261 if (rvcomp - 3.1566) 10260, 10260, 20260                          
30260 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10260, 0271, 20260                                    
10260 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0271                                                        
20260 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0271 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 027  ****                         
!                                                                        
!         TWELVE DIGITS IN BASIC REAL CONSTANT.                          
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      rvon06 = 0.0                                                      
      rvcomp = 0.0                                                      
      rvon06 = 315612347833e-11                                        
      rvcomp = rvon06                                                   
      rvcorr = 3.1561                                                   
40270 if (rvcomp - 3.1556) 20270, 10270, 40271                          
40271 if (rvcomp - 3.1566) 10270, 10270, 20270                          
30270 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10270, 0281, 20270                                    
10270 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0281                                                        
20270 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0281 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 028  ****                         
!                                                                        
!         TWENTY-FIVE DIGITS IN BASIC REAL CONSTANT.                     
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      rvon06 = 0.0                                                      
      rvcomp = 0.0                                                      
      rvon06 = 31.56123478334867532834672e-1                            
      rvcomp = rvon06                                                   
      rvcorr = 3.1561                                                   
40280 if (rvcomp - 3.1556) 20280, 10280, 40281                          
40281 if (rvcomp - 3.1566) 10280, 10280, 20280                          
30280 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10280, 0291, 20280                                    
10280 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0291                                                        
20280 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0291 continue                                                          
!                                                                        
!         TEST 29 THROUGH TEST 33 REFERENCE VARIABLES WHOSE TYPE WAS     
!      SPECIFIED BY AN IMPLICIT STATEMENT.  DIVISION IS USED TO VERIFY   
!      THAT THE TYPE IS INTEGER OR REAL.                                 
!                                                                        
!      REFERENCE   X3.9-1978, SECTION 8.5, IMPLICIT STATEMENT            
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 029  ****                         
!                                                                        
!         VERIFY YVIN01 IS AN INTEGER VARIABLE.                          
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      rvcomp = 10.0                                                     
      yvin01 = 4.0                                                      
      rvcomp = yvin01/5                                                 
      rvcorr = 0.0                                                      
40290 if (rvcomp) 20290, 10290, 20290                                   
30290 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10290, 0301, 20290                                    
10290 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0301                                                        
20290 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0301 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 030  ****                         
!                                                                        
!         VERIFY VVIN01 IS AN INTEGER VARIABLE                           
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      rvcomp = 10.0                                                     
      vvin01 = 4.0                                                      
      rvcomp = vvin01/5                                                 
      rvcorr = 0.0                                                      
40300 if (rvcomp) 20300, 10300, 20300                                   
30300 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10300, 0311, 20300                                    
10300 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0311                                                        
20300 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0311 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 031  ****                         
!                                                                        
!         VERIFY WVIN01 IS AN INTEGER VARIABLE.                          
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      rvcomp = 10.0                                                     
      wvin01 = 4.0                                                      
      rvcomp = wvin01/5                                                 
      rvcorr = 0.0                                                      
40310 if (rvcomp) 20310, 10310, 20310                                   
30310 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10310, 0321, 20310                                    
10310 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0321                                                        
20310 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0321 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 032  ****                         
!                                                                        
!         VERIFY XVIN01 IS AN INTEGER VARIABLE.                          
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      xvin01 = 4                                                        
      rvcomp = 10.0                                                     
      rvcomp = xvin01/5                                                 
      rvcorr = 0.0                                                      
40320 if (rvcomp) 20320, 10320, 20320                                   
30320 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10320, 0331, 20320                                    
10320 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0331                                                        
20320 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0331 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 033  ****                         
!                                                                        
!         VERIFY MVIN01 IS A REAL VARIABLE.                              
!                                                                        
      ivtnum =  33                                                      
      if (iczero) 30330, 0330, 30330                                    
 0330 continue                                                          
      rvcomp = 10.0                                                     
      mvin01 = 4                                                        
      rvcomp = mvin01/5                                                 
      rvcorr = 0.8                                                      
40330 if (rvcomp - 0.79995) 20330, 10330, 40331                         
40331 if (rvcomp - 0.80005) 10330, 10330, 20330                         
30330 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10330, 0341, 20330                                    
10330 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0341                                                        
20330 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0341 continue                                                          
!                                                                        
!         TEST 34 AND TEST 35 VERIFY THAT THE IMPLICIT TYPE SPECIFICA-   
!      TION FOR A VARIABLE IS OVERRIDDEN BY THE APPEARANCE OF THAT       
!      VARIABLE NAME IN A TYPE-STATEMENT.                                
!                                                                        
!      REFERENCE   X3.9-1977, SECTION 8.4, TYPE-STATEMENTS               
!                             SECTION 8.5, IMPLICIT STATEMENT            
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 034  ****                         
!                                                                        
!         VERIFY YVTN02 IS A REAL VARIABLE.                              
!                                                                        
      ivtnum =  34                                                      
      if (iczero) 30340, 0340, 30340                                    
 0340 continue                                                          
      rvcomp = 10.0                                                     
      yvtn02 = 4                                                        
      rvcomp = yvtn02/5                                                 
      rvcorr = 0.8                                                      
40340 if (rvcomp - 0.79995) 20340, 10340, 40341                         
40341 if (rvcomp - 0.80005) 10340, 10340, 20340                         
30340 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10340, 0351, 20340                                    
10340 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0351                                                        
20340 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0351 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 201  -  TEST 035  ****                         
!                                                                        
!         VERIFY MVTN02 IS AN INTEGER VARIABLE.                          
!                                                                        
      ivtnum =  35                                                      
      if (iczero) 30350, 0350, 30350                                    
 0350 continue                                                          
      rvcomp = 10.0                                                     
      mvtn02 = 4.0                                                      
      rvcomp = mvtn02/5                                                 
      rvcorr = 0.0                                                      
40350 if (rvcomp) 20350, 10350, 20350                                   
30350 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10350, 0361, 20350                                    
10350 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0361                                                        
20350 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0361 continue                                                          
!                                                                        
!                                                                        
!      WRITE OUT TEST SUMMARY                                            
!                                                                        
      write (i02,90004)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
      write (i02,90000)                                                 
      write (i02,90004)                                                 
      write (i02,90020) ivfail                                          
      write (i02,90022) ivpass                                          
      write (i02,90024) ivdele                                          
      stop                                                              
90001 format (" ",24x,"FM201")                                          
90000 format (" ",20x,"END OF PROGRAM FM201" )                          
!                                                                        
!      FORMATS FOR TEST DETAIL LINES                                     
!                                                                        
80000 format (" ",4x,i5,6x,"DELETED")                                   
80002 format (" ",4x,i5,7x,"PASS")                                      
80010 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80012 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
80018 format (" ",4x,i5,7x,"FAIL",2x,a14,1x,a14)                        
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
!                                                                        
90002 format ("1")                                                      
90004 format (" ")                                                      
90006 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90008 format (" ",21x,"VERSION 2.1" )                                   
90010 format (" ",8x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )         
90012 format (" ",5x,"TEST",5x,"PASS/FAIL",5x,"COMPUTED",8x,"CORRECT")  
90014 format (" ",5x,"----------------------------------------------" ) 
90016 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARY                                 
!                                                                        
90020 format (" ",19x,i5," TESTS FAILED" )                              
90022 format (" ",19x,i5," TESTS PASSED" )                              
90024 format (" ",19x,i5," TESTS DELETED" )                             
      end program fm201
