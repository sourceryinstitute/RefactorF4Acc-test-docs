      program fm352
!                                                                        
!                                                                        
!           THIS PROGRAM CHECKS BASIC RELATIONAL EXPRESSIONS INVOLVING   
!      OPERANDS OF REAL DATA TYPE.  IN EACH TEST, NOT ONLY THE RELATIONAL
!      EXPRESSION IS TESTED, BUT THE TRICHOTOMY LAW OF MATHEMATICAL      
!      RELATIONSHIPS IS ALSO TESTED (E.G., IF A .LT. B, THEN A CAN NOT   
!      BE .GT. THAN B, AND A CAN NOT BE .EQ. B).  A TEST VARIABLE        
!      (IVCOMP) IS USED TO REPORT THE RESULT OF THE TEST AS FOLLOWS,     
!           IVCOMP = 0  IF BOTH THE TESTED RELATIONAL OPERATOR AND THE   
!                       TRICHOTOMY TEST PASS.                            
!           IVCOMP = 1  IF THE RELATIONAL TEST FAILS AND THE TRICHOTOMY  
!                       TEST PASSES (WHICH WOULD INDICATE THAT A TESTED  
!                       NOT .LT., .GT., OR .EQ. B).                      
!           IVCOMP = 2  IF THE RELATIONAL TEST PASSES AND THE TRICHOTOMY 
!                       TEST FAILS (WHICH WOULD INDICATE THAT A TESTED   
!                       .LT., .GT., AND .EQ. B).                         
!           IVCOMP = 3  IF BOTH THE RELATIONAL TEST AND THE TRICHOTOMY   
!                       TEST FAIL (WHICH WOULD INDICATE THE RELATIONAL   
!                       EXPRESSION TESTED OPPOSITE TO THAT EXPECTED      
!                       (E.G., WHERE A WAS SUPPOSED TO BE .LT. B, IN     
!                       FACT A .LT. B WAS FOUND TO BE FALSE AND A .GE. B 
!                       WAS FOUND TO BE TRUE).                           
!                                                                        
!                                                                        
!      REFERENCES -                                                      
!                                                                        
!      AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN, X3.9-1977
!           SECTION 4.4,  REAL TYPE                                      
!           SECTION 6.3,  RELATIONAL EXPRESSIONS                         
!           SECTION 6.5,  PRECEDENCE OF OPERATORS                        
!           SECTION 6.6,  EVALUATION OF EXPRESSIONS                      
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
      real :: rfos01
      real :: rdon01
      real :: rdon02
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvon01
      real :: rvon02
      integer :: ivcomp
      integer :: ivcorr
      real :: go
      real :: to
      real :: rvon03
      real, dimension(1:2) :: radn11
      rfos01(rdon01,rdon02) = rdon01 + rdon02                           
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
!           TESTS 1 THROUGH 13 CHECK BASIC RELATIONAL EXPRESSIONS USING  
!      ONLY REAL VARIABLE OPERANDS.  ALL THE VARIABLES ARE ASSIGNED REAL 
!      CONSTANTS WITH EXPONENTIAL FORMAT.                                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 001  ****                         
!                                                                        
!           TEST 1 CHECKS THE .LT. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.0001e18                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40010 if(rvon01  <  rvon02)  goto 40011                               
      ivcomp = 1                                                        
40011 if (rvon01  >=  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp) 20010, 10010, 20010                                   
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10010, 0021, 20010                                    
10010 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0021                                                        
20010 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 002  ****                         
!                                                                        
!           TEST 2 CHECKS THE .LT. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40020 if (rvon01  <  rvon02)  goto 40021                              
      ivcomp = 1                                                        
40021 if (rvon01  >=  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20020, 10020, 20020                                  
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10020, 0031, 20020                                    
10020 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0031                                                        
20020 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0031 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 003  ****                         
!                                                                        
!           TEST 3 CHECKS THE .LE. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.0001e18                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40030 if (rvon01  <=  rvon02)  goto 40031                              
      ivcomp = 1                                                        
40031 if (rvon01  >  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20030, 10030, 20030                                  
30030 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10030, 0041, 20030                                    
10030 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0041                                                        
20030 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0041 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 004  ****                         
!                                                                        
!           TEST 4 CHECKS THE .LE. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40040 if (rvon01  <=  rvon02)  goto 40041                              
      ivcomp = 1                                                        
40041 if (rvon01  >  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20040, 10040, 20040                                  
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10040, 0051, 20040                                    
10040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0051                                                        
20040 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 005  ****                         
!                                                                        
!           TEST 5 CHECKS THE .LE. OPERATOR USING TWO REAL OPERANDS      
!      WHICH HAVE BEEN ASSIGNED THE SAME REAL CONSTANT.                  
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40050 if (rvon01  <=  rvon02)  goto 40051                              
      ivcomp = 1                                                        
40051 if (rvon01  >  rvon02) ivcomp = ivcomp + 2                       
      if (ivcomp)  20050, 10050, 20050                                  
30050 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10050, 0061, 20050                                    
10050 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0061                                                        
20050 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 006  ****                         
!                                                                        
!           TEST 6 CHECKS THE .NE. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.0001e18                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40060 if (rvon01  /=  rvon02)  goto 40061                              
      ivcomp = 1                                                        
40061 if (rvon01  ==  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20060, 10060, 20060                                  
30060 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10060, 0071, 20060                                    
10060 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0071                                                        
20060 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0071 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 007  ****                         
!                                                                        
!           TEST 7 CHECKS THE .NE. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40070 if (rvon01  /=  rvon02)  goto 40071                              
      ivcomp = 1                                                        
40071 if (rvon01  ==  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20070, 10070, 20070                                  
30070 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10070, 0081, 20070                                    
10070 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0081                                                        
20070 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0081 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 008  ****                         
!                                                                        
!           TEST 8 CHECKS THE .EQ. OPERATOR USING TWO REAL OPERANDS      
!      WHICH HAVE BEEN ASSIGNED THE SAME REAL CONSTANT.                  
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40080 if (rvon01  ==  rvon02)  goto 40081                              
      ivcomp = 1                                                        
40081 if (rvon01  /=  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20080, 10080, 20080                                  
30080 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10080, 0091, 20080                                    
10080 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0091                                                        
20080 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0091 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 009  ****                         
!                                                                        
!           TEST 9 CHECKS THE .GT. OPERATOR USING TWO REAL OPERANDS      
!      WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      rvon01 = 1.0001e18                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40090 if(rvon01  >  rvon02)  goto 40091                               
      ivcomp = 1                                                        
40091 if (rvon01  <=  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20090, 10090, 20090                                  
30090 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10090, 0101, 20090                                    
10090 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0101                                                        
20090 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0101 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 010  ****                         
!                                                                        
!           TEST 10 CHECKS THE .GT. OPERATOR USING TWO REAL OPERANDS     
!      WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      rvon01 = 1.9999e17                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40100 if (rvon01  >  rvon02)  goto 40101                              
      ivcomp = 1                                                        
40101 if (rvon01  <=  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20100, 10100, 20100                                  
30100 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10100, 0111, 20100                                    
10100 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0111                                                        
20100 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0111 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 011  ****                         
!                                                                        
!           TEST 11 CHECKS THE .GE. OPERATOR USING TWO REAL OPERANDS     
!      WHERE THE MANTISSAS ARE EQUAL BUT THE EXPONENTS ARE DIFFERENT.    
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      rvon01 = 1.0001e18                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40110 if (rvon01  >=  rvon02)  goto 40111                              
      ivcomp = 1                                                        
40111 if (rvon01  <  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20110, 10110, 20110                                  
30110 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10110, 0121, 20110                                    
10110 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0121                                                        
20110 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0121 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 012  ****                         
!                                                                        
!           TEST 12 CHECKS THE .GE. OPERATOR USING TWO REAL OPERANDS     
!      WHERE THE EXPONENTS ARE EQUAL BUT THE MANTISSAS ARE DIFFERENT.    
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      rvon01 = 1.9999e17                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40120 if (rvon01  >=  rvon02)  goto 40121                              
      ivcomp = 1                                                        
40121 if (rvon01  <  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20120, 10120, 20120                                  
30120 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10120, 0131, 20120                                    
10120 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0131                                                        
20120 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0131 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 013  ****                         
!                                                                        
!           TEST 13 CHECKS THE .GE. OPERATOR USING TWO REAL OPERANDS     
!      WHERE EACH HAS BEEN ASSIGNED THE SAME REAL CONSTANT.              
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40130 if (rvon01  >=  rvon02)  goto 40131                              
      ivcomp = 1                                                        
40131 if (rvon01  <  rvon02)  ivcomp = ivcomp + 2                      
      if (ivcomp)  20130, 10130, 20130                                  
30130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10130, 0141, 20130                                    
10130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0141                                                        
20130 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0141 continue                                                          
!                                                                        
!           TESTS 14 THROUGH 28 REPETITIVELY CHECK THE .LT. RELATIONSHIP 
!      USING ALL TYPES AND ORDERINGS OF TWO REAL OPERANDS.               
!                                                                        
!                                                                        
!           TESTS 14 THROUGH 16 CHECK REAL-VARIABLE .LT OTHER-REAL-TYPES.
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 014  ****                         
!                                                                        
!           TEST 14 CHECKS REAL-VARIABLE .LT. REAL-CONSTANT              
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      rvon01 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40140 if (rvon01  <  1.9999e17) goto 40141                           
      ivcomp = 1                                                        
40141 if (rvon01  >=  1.9999e17)  ivcomp = ivcomp + 2                  
      if (ivcomp)  20140, 10140, 20140                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 015  ****                         
!                                                                        
!           TEST 15 CHECKS REAL-VARIABLE .LT. ARRAY-ELEMENT              
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      radn11(1) = 1.9999e17                                            
      rvon01 = 1.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40150 if (rvon01  <  radn11(1)) goto 40151                            
      ivcomp = 1                                                        
40151 if (rvon01  >=  radn11(1)) ivcomp = ivcomp + 2                    
      if (ivcomp)  20150, 10150, 20150                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 016  ****                         
!                                                                        
!           TEST 16 CHECKS REAL-VARIABLE .LT. FUNCTION-REFERENCE         
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      rvon01 = 1.0001e17                                               
      rvon02 = 1e17                                                    
      rvon03 = 0.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40160 if (rvon01  <  rfos01(rvon02,rvon03)) goto 40161                
      ivcomp = 1                                                        
40161 if (rvon01  >=  rfos01(rvon02,rvon03)) ivcomp = ivcomp + 2        
      if (ivcomp)  20160, 10160, 20160                                  
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
!           TESTS 17 THROUGH 20 CHECK REAL-CONSTANT .LT. OTHER-REAL-TYPES
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 017  ****                         
!                                                                        
!           TEST 17 CHECKS REAL-CONSTANT .LT. REAL-CONSTANT              
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40170 if (1.0001e17  <  1.9999e17)  goto 40171                      
      ivcomp = 1                                                        
40171 if (1.0001e17  >=  1.9999e17)  ivcomp = ivcomp + 2              
      if (ivcomp)  20170, 10170, 20170                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 018  ****                         
!                                                                        
!           TEST 18 CHECKS REAL-CONSTANT .LT. REAL-ARRAY-ELEMENT         
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      radn11(1) = 1.9999e17                                            
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40180 if (1.0001e17  <  radn11(1))  goto 40181                       
      ivcomp = 1                                                        
40181 if (1.0001e17  >=  radn11(1))  ivcomp = ivcomp + 2               
      if (ivcomp)  20180, 10180, 20180                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 019  ****                         
!                                                                        
!           TEST 19 CHECKS REAL-CONSTANT .LT. REAL-VARIABLE              
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      rvon01 = 1.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40190 if (1.0001e17  <  rvon01)  goto 40191                          
      ivcomp = 1                                                        
40191 if (1.0001e17  >=  rvon01)  ivcomp = ivcomp + 2                  
      if (ivcomp) 20190, 10190, 20190                                   
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
!      ****  FCVS PROGRAM 352  -  TEST 020  ****                         
!                                                                        
!           TEST 20 CHECKS REAL-CONSTANT .LT. REAL-FUNCTION-REFERENCE    
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      rvon01 = 1e17                                                    
      rvon02 = 0.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40200 if (1.0001e17  <  rfos01(rvon01,rvon02))  goto 40201           
      ivcomp = 1                                                        
40201 if (1.0001e17  >=  rfos01(rvon01,rvon02))  ivcomp = ivcomp + 2   
      if (ivcomp)  20200, 10200, 20200                                  
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
!           TESTS 21 THROUGH 24 CHECK REAL-ARRAY-ELEMENT .LT. OTHER-REALS
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 021  ****                         
!                                                                        
!           TEST 21 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-CONSTANT         
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      radn11(1) = 1.0001e17                                            
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40210 if (radn11(1)  <  1.9999e17)  goto 40211                       
      ivcomp = 1                                                        
40211 if (radn11(1)  >=  1.9999e17)  ivcomp = ivcomp + 2               
      if (ivcomp)  20210, 10210, 20210                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 022  ****                         
!                                                                        
!           TEST 22 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-ARRAY-ELEMENT    
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      radn11(1) = 1.0001e17                                            
      radn11(2) = 1.9999e17                                            
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40220 if (radn11(1)  <  radn11(2))  goto 40221                        
      ivcomp = 1                                                        
40221 if (radn11(1)  >=  radn11(2))  ivcomp = ivcomp + 2                
      if (ivcomp)  20220, 10220, 20220                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 023  ****                         
!                                                                        
!           TEST 23 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-VARIABLE         
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      rvon01 = 1.9999e17                                               
      radn11(1) = 1.0001e17                                            
      ivcorr = 0                                                        
      ivcomp = 0                                                        
40230 if (radn11(1)  <  rvon01)  goto 40231                           
      ivcomp = 1                                                        
40231 if (radn11(1)  >=  rvon01)  ivcomp = ivcomp + 2                   
      if (ivcomp)  20230, 10230, 20230                                  
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
!      ****  FCVS PROGRAM 352  -  TEST 024  ****                         
!                                                                        
!           TEST 24 CHECKS REAL-ARRAY-ELEMENT .LT. REAL-FUNCTION-REF.    
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      rvon01 = 1.0000e17                                               
      rvon02 = 0.9999e17                                               
      radn11(1) = 1.0001e17                                            
      ivcorr = 0                                                        
      ivcomp = 0                                                        
40240 if (radn11(1)  <  rfos01(rvon01,rvon02))  goto 40241            
      ivcomp = 1                                                        
40241 if (radn11(1)  >=  rfos01(rvon01,rvon02))  ivcomp = ivcomp + 2    
      if (ivcomp)  20240, 10240, 20240                                  
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
!           TESTS 25 THROUGH 28 CHECK REAL-FUNCTION-REFERENCE .LT.       
!                                     OTHER-REAL-TYPES                   
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 025  ****                         
!                                                                        
!           TEST 25 CHECKS REAL-FUNCTION-REFERENCE .LT. REAL-CONSTANT    
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      rvon01 = 1.0000e17                                               
      rvon02 = 0.0001e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40250 if (rfos01(rvon01,rvon02)  <  1.9999e17)  goto 40251           
      ivcomp = 1                                                        
40251 if (rfos01(rvon01,rvon02)  >=  1.9999e17)  ivcomp = ivcomp + 2   
      if (ivcomp)  20250, 10250, 20250                                  
30250 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10250, 0261, 20250                                    
10250 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0261                                                        
20250 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0261 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 026  ****                         
!                                                                        
!           TEST 26 CHECKS REAL-FUNCTION-REFERENCE .LT. REAL-ARRAY-ELEMNT
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      rvon01 = 1e17                                                    
      rvon02 = 0.0001e17                                               
      radn11(1) = 1.9999e17                                            
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40260 if (rfos01(rvon01,rvon02)  <  radn11(1))  goto 40261            
      ivcomp = 1                                                        
40261 if (rfos01(rvon01,rvon02)  >=  radn11(1))  ivcomp = ivcomp + 2    
      if (ivcomp)  20260, 10260, 20260                                  
30260 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10260, 0271, 20260                                    
10260 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0271                                                        
20260 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0271 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 027  ****                         
!                                                                        
!           TEST 27 CHECKS REAL-FUNCTION-REFERENCE .LT. REAL-VARIABLE    
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      rvon01 = 1e17                                                    
      rvon02 = 0.0001e17                                               
      rvon03 = 1.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40270 if (rfos01(rvon01,rvon02)  <  rvon03)  goto 40271               
      ivcomp = 1                                                        
40271 if (rfos01(rvon01,rvon02)  >=  rvon03)  ivcomp = ivcomp + 2       
      if (ivcomp)  20270, 10270, 20270                                  
30270 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10270, 0281, 20270                                    
10270 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0281                                                        
20270 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0281 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 352  -  TEST 028  ****                         
!                                                                        
!           TEST 28 CHECKS REAL-FUNCTION-REFERENCE .LT REAL-FUNCTION-REF.
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      rvon01 = 1e17                                                    
      rvon02 = 0.0001e17                                               
      rvon03 = 0.9999e17                                               
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40280 if (rfos01(rvon01,rvon02)  <  rfos01(rvon01,rvon03)) goto 40281 
      ivcomp = 1                                                        
40281 if (rfos01(rvon01,rvon02)  >=  rfos01(rvon01,rvon03))                      ivcomp = ivcomp + 2                                      
      if (ivcomp)  20280, 10280, 20280                                  
30280 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10280, 0291, 20280                                    
10280 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0291                                                        
20280 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0291 continue                                                          
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
90001 format (" ",24x,"FM352")                                          
90000 format (" ",20x,"END OF PROGRAM FM352" )                          
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
      end program fm352
