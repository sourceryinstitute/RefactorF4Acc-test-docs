      program fm351
!                                                                        
!                                                                        
!           THIS PROGRAM CONTAINS TESTS FOR COMPOUND ARITHMETIC          
!      EXPRESSIONS WHICH NECESSITATE THE APPLICATION OF THE RULES        
!      FOR ARITHMETIC OPERATOR PRECEDENCE.  THESE TESTS INCLUDE ONES     
!      WHICH EXERCIZE THE                                                
!                                                                        
!      (1)  USE OF ALL ARITHMETIC OPERATOR TYPES IN THE SAME STATEMENT.  
!      (2)  USE OF PARENTHESES TO OVERRIDE DEFAULT PRECEDENCES.          
!      (3)  USE OF ALL CLASSES OF PRIMARY OPERANDS.                      
!      (4)  USE OF NESTED FUNCTION REFERENCES.                           
!      (5)  USE OF MIXED DATA TYPES.                                     
!                                                                        
!      REFERENCES -                                                      
!                                                                        
!      AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN, X3.9-1977
!                                                                        
!           SECTION 6.1  ARITHMETIC EXPRESSIONS                          
!           SECTION 6.5  PRECEDENCE OF OPERATORS                         
!           SECTION 6.6  EVALUATION OF EXPRESSIONS                       
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
      integer :: ifos01
      integer :: idon02
      integer :: idon03
      integer :: idon01
      integer :: ifos02
      integer :: idon04
      integer :: idon05
      integer :: ifos04
      integer :: idon09
      integer :: idon10
      integer :: ifos03
      integer :: idon07
      integer :: idon08
      integer :: idon06
      real :: rfos01
      real :: rdon03
      real :: rdon02
      real :: rdon01
      real :: rfos02
      integer :: idon11
      integer :: idon12
      real :: rfos04
      integer :: idon13
      real :: rdon10
      real :: rfos03
      real :: rdon07
      real :: rdon06
      real :: rdon08
      integer :: ifos05
      integer :: idon14
      integer :: idon16
      real :: rfos06
      real :: rdon19
      integer :: idon18
      real :: rdon17
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
      integer :: ivon07
      real :: rvon01
      real :: rvon02
      real :: rvon03
      real :: rvon04
      real :: rvon05
      real :: rvon06
      real :: rvcomp
      real :: rvcorr
      real :: rvon07
      integer, dimension(1:5) :: iadn11
      real, dimension(1:5) :: radn11
      ifos01(idon01,idon02,idon03) = idon01 ** idon02 ** idon03         
      ifos02(idon04,idon05) = iadn11(idon04) / iadn11(idon05)           
      ifos04(idon09,idon10) = iadn11(idon09) + iabs(idon10)             
      ifos03(idon06,idon07,idon08) = ifos04(idon06,idon07) * idon08     
      rfos01(rdon01,rdon02,rdon03) = rdon01 ** rdon02 ** rdon03         
      rfos02(idon11,idon12) = radn11(idon11) / radn11(idon12)           
      rfos04(idon13,rdon10) = radn11(idon13) + abs(rdon10)              
      rfos03(rdon06,rdon07,rdon08) = rfos04(int(rdon06),rdon07) * rdon08
      ifos05(idon14,idon16) = radn11(idon14) + iabs(idon16)             
      rfos06(rdon17,idon18,rdon19) = ifos05(int(rdon17),idon18) * rdon19
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
!           TESTS 1 THROUGH 10 DEAL ENTIRELY WITH INTEGER EXPRESSIONS.   
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 001  ****                         
!                                                                        
!      TEST 1 CHECKS AN INTEGER EXPRESSION WHERE ALL FIVE ARITHMETIC     
!      OPERATORS ARE USED AND ALL OPERAND PRIMARIES ARE SIMPLE INTEGER   
!      VARIABLES.  NO PARENTHESES ARE USED TO UPSET DEFAULT PRECEDENCES. 
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivon03 = 573                                                      
      ivon04 = 23                                                       
      ivon05 = 3                                                        
      ivon06 = -7                                                       
      ivcomp = ivon01 ** ivon02 + ivon03 - ivon04 * ivon05 / ivon06     
      ivcorr = 925                                                      
40010 if (ivcomp - 925) 20010, 10010, 20010                             
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
!      ****  FCVS PROGRAM 351  -  TEST 002  ****                         
!                                                                        
!           TEST 2, LIKE TEST 1, CHECKS AN INTEGER EXPRESSION WHERE ALL  
!      FIVE ARITHMETIC OPERATORS ARE USED AND ALL OPERANDS ARE SIMPLE    
!      INTEGER VARIABLES; BUT IN THIS TEST, PARENTHESES ARE USED, AS IS  
!      A UNARY OPERATOR.                                                 
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivon03 = 5                                                        
      ivon04 = -3                                                       
      ivon05 = 3                                                        
      ivcomp = -(ivon01 / ivon02) + (ivon03 * ivon04 ** ivon05)         
      ivcorr = -137                                                     
40020 if (ivcomp + 137) 20020, 10020, 20020                             
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
!      ****  FCVS PROGRAM 351  -  TEST 003  ****                         
!                                                                        
!           TEST 3 IS SIMILAR TO TEST 2 EXCEPT THAT IT EMPLOYS NESTED    
!      PARENTHESES.                                                      
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivon01 = 5                                                        
      ivon02 = 3                                                        
      ivon03 = 5                                                        
      ivon04 = 17                                                       
      ivon05 = 14                                                       
      ivon06 = 3                                                        
      ivcomp = ivon01 ** (-(ivon02 + (ivon03 - ivon04)) - (ivon05 /              ivon06))                                                 
      ivcorr = 3125                                                     
40030 if (ivcomp - 3125) 20030, 10030, 20030                            
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
!      ****  FCVS PROGRAM 351  -  TEST 004  ****                         
!                                                                        
!           TEST 4 IS SIMILAR TO TEST 2 AND 3 EXCEPT THAT THE            
!      PARENTHESES USED ARE EFFECTIVELY EXTRANEOUS.                      
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 4                                                        
      ivon03 = 5                                                        
      ivon04 = 2                                                        
      ivon05 = 3                                                        
      ivon06 = 4                                                        
      ivcomp = ((ivon01) ** (ivon02) + (ivon03) - (ivon04) *                     (ivon05) / (ivon06))                                     
      ivcorr = 85                                                       
40040 if (ivcomp - 85) 20040, 10040, 20040                              
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
!      ****  FCVS PROGRAM 351  -  TEST 005  ****                         
!                                                                        
!           TEST 5 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY       
!      INTEGER VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS,      
!      AND USING PARENTHESES TO OVERRIDE PRECEDENCES.                    
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivon01 = 57                                                       
      ivon02 = -3                                                       
      ivon03 = 4                                                        
      ivon04 = -1                                                       
      ivon05 = -5                                                       
      ivon06 = -2                                                       
      ivcomp = -ivon01 ** (ivon02 + ivon03 - ivon04) *                           (ivon05 / ivon06)                                        
      ivcorr = -6498                                                    
40050 if (ivcomp + 6498) 20050, 10050, 20050                            
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
!      ****  FCVS PROGRAM 351  -  TEST 006  ****                         
!                                                                        
!           TEST 6 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY       
!      INTEGER VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS,      
!      AND USING PARENTHESES TO OVERRIDE PRECEDENCES.                    
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivon01 = 5                                                        
      ivon02 = 3                                                        
      ivon03 = 4                                                        
      ivon04 = 5496                                                     
      ivon05 = 7                                                        
      ivon06 = -3                                                       
      ivcomp = ((ivon01 * (ivon02 / ivon03)) + ivon04) / ivon05 -                (-ivon06)                                                
      ivcorr = 782                                                      
40060 if (ivcomp - 782) 20060, 10060, 20060                             
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
!      ****  FCVS PROGRAM 351  -  TEST 007  ****                         
!                                                                        
!           IN TEST 7, AN INTEGER EXPRESSION INVOLVING ALL FIVE          
!      ARITHMETIC OPERATORS TOGETHER WITH PARENTHESES IS EVALUATED,      
!      BUT UNLIKE TESTS 1 THROUGH 6 WHERE ALL OPERANDS WERE INTEGER      
!      VARIABLES, THE OPERANDS IN TEST 7 ARE CLASSED AS INTEGER          
!      VARIABLES, INTEGER CONSTANTS, INTEGER ARRAY ELEMENTS, AND INTEGER 
!      FUNCTION REFERENCES.                                              
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivon01 = 573                                                      
      ivon02 = 1                                                        
      ivon03 = 3                                                        
      ivon04 = 2                                                        
      ivon05 = 3                                                        
      iadn11(3) = 3071                                                  
      ivcomp = (ivon01 + 1) - (5 + iadn11(ivon03)) /                             (ifos01(ivon03,ivon04,ivon05) ** ivon02)                 
      ivcorr = 574                                                      
40070 if (ivcomp - 574) 20070, 10070, 20070                             
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
!      ****  FCVS PROGRAM 351  -  TEST 008  ****                         
!                                                                        
!           TEST 8 IS IDENTICAL TO TEST 7 EXCEPT THAT PARENTHESES ARE    
!      USED TO CHANGE THE ORDER OF SUB-EXPRESSION EVALUATION.            
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivon01 = 573                                                      
      ivon02 = 1                                                        
      ivon03 = 3                                                        
      ivon04 = 2                                                        
      ivon05 = 3                                                        
      iadn11(3) = 3071                                                  
      ivcomp = ((ivon01 + 1) - (5 + iadn11(ivon03))) /                           ifos01(ivon03,ivon04,ivon05) ** ivon02                   
      ivcorr = 0                                                        
40080 if (ivcomp) 20080, 10080, 20080                                   
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
!      ****  FCVS PROGRAM 351  -  TEST 009  ****                         
!                                                                        
!           TEST 9 IS SIMILAR TO TESTS 7 AND 8 EXCEPT THAT THE           
!      FUNCTION REFERENCE IN TURN EVALUATES ARRAY ELEMENTS.              
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivon03 = 2                                                        
      ivon04 = 1                                                        
      ivon05 = 4                                                        
      iadn11(1) = 5                                                     
      iadn11(2) = 2                                                     
      iadn11(4) = 2                                                     
      ivcomp = (ivon01 - 8 * ifos02(ivon04,ivon03)) / iadn11(ivon05) +           13 ** ivon02                                             
      ivcorr = 2193                                                     
40090 if (ivcomp - 2193) 20090, 10090, 20090                            
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
!      ****  FCVS PROGRAM 351  -  TEST 010  ****                         
!                                                                        
!           TEST 10 EVALUATES AN INTEGER EXPRESSION WHICH CONTAINS       
!      FUNCTION REFERENCES NESTED TO THREE LEVELS.  THE OUTER TWO        
!      LEVELS ARE STATEMENT FUNCTION REFERENCES AND THE INNERMOST LEVEL  
!      IS AN INTRINSIC FUNCTION REFERENCE.                               
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivon01 = -51                                                      
      ivon02 = 4                                                        
      ivon03 = -101                                                     
      ivon04 = 13                                                       
      ivon05 = 3                                                        
      ivon06 = 5                                                        
      ivon07 = -37                                                      
      iadn11(4) = 87                                                    
      iadn11(5) = 409                                                   
      ivcomp = (ivon01 + ifos03(ivon02,ivon03,ivon04)) * ivon05 -                ifos04(ivon06,ivon07)                                    
      ivcorr = 6733                                                     
40100 if (ivcomp - 6733) 20100, 10100, 20100                            
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
!           TESTS 11 THROUGH 20 REPEAT TESTS 1 THROUGH 10 EXCEPT THAT    
!      TESTS 11 THROUGH 20 DEAL ENTIRELY WITH REAL ARITHMETIC            
!      EXPRESSIONS.                                                      
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 011  ****                         
!                                                                        
!           TEST 11 TESTS A REAL EXPRESSION WHERE ALL FIVE ARITHMETIC    
!      OPERATORS ARE USED AND ALL OPERAND PRIMARIES ARE SIMPLE REAL      
!      VARIABLES.                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      rvon01 = 3.2                                                      
      rvon02 = 23.051                                                   
      rvon03 = 1545e7                                                  
      rvon04 = -23.457                                                  
      rvon05 = .02e3                                                   
      rvon06 = 7.210745323e-10                                         
      rvcomp = rvon01 ** rvon02 + rvon03 - rvon04 * rvon05 / rvon06     
      rvcorr = 1.10683e12                                              
40110 if (rvcomp - 1.1063e12)  20110, 10110, 40111                     
40111 if (rvcomp - 1.1073e12)  10110, 10110, 20110                     
30110 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10110, 0121, 20110                                    
10110 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0121                                                        
20110 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0121 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 012  ****                         
!                                                                        
!           TEST 12, LIKE TEST 11, CHECKS A REAL EXPRESSION WHERE ALL    
!      FIVE ARITHMETIC OPERATORS ARE USED AND ALL OPERANDS ARE REAL      
!      VARIABLES, BUT IN TEST 12, PARENTHESES ARE USED, AS IS ALSO A     
!      UNARY OPERATOR.                                                   
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      rvon01 = 3.2                                                      
      rvon02 = 23.051                                                   
      rvon03 = 1545e-3                                                 
      rvon04 = 5.75e-1                                                 
      rvon05 = 2.22e+1                                                 
      rvcomp = -(rvon01 / rvon02) + (rvon03 * rvon04 ** rvon05)         
      rvcorr = -.13882                                                  
40120 if (rvcomp + .13887) 20120, 10120, 40121                          
40121 if (rvcomp + .13877) 10120, 10120, 20120                          
30120 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10120, 0131, 20120                                    
10120 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0131                                                        
20120 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0131 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 013  ****                         
!                                                                        
!           TEST 13 IS SIMILAR TO TEST 12 EXCEPT THAT TEST 13 EMPLOYS    
!      NESTED PARENTHESES.                                               
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvon01 = 3.2                                                      
      rvon02 = -63.051                                                  
      rvon03 = 1545e-3                                                 
      rvon04 = 5.75e-1                                                 
      rvon05 = 2.22e1                                                  
      rvon06 = 0.523                                                    
      rvcomp = rvon01 ** (-(rvon02 + (rvon03 - rvon04)) -                        (rvon05 / rvon06))                                       
      rvcorr = 8.27757e9                                               
40130 if (rvcomp - 8.2770e9) 20130, 10130, 40131                       
40131 if (rvcomp - 8.2780e9) 10130, 10130, 20130                       
30130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10130, 0141, 20130                                    
10130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0141                                                        
20130 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0141 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 014  ****                         
!                                                                        
!           TEST 14 IS SIMILAR TO TESTS 12 AND 13 EXCEPT THAT THE        
!      PARENTHESES USED ARE EFFECTIVELY EXTRANEOUS.                      
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      rvon01 = 5.4515e18                                               
      rvon02 = .076923                                                  
      rvon03 = 23e-2                                                   
      rvon04 = 7e7                                                     
      rvon05 = 45.23e5                                                 
      rvon06 = 5.65375e12                                              
      rvcomp = ((rvon01) ** (rvon02) + (rvon03) - (rvon04) * (rvon05) /          (rvon06))                                                
      rvcorr = -28.147                                                  
40140 if (rvcomp + 28.152) 20140, 10140, 40141                          
40141 if (rvcomp + 28.142) 10140, 10140, 20140                          
30140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10140, 0151, 20140                                    
10140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0151                                                        
20140 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0151 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 015  ****                         
!                                                                        
!           TEST 15 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY      
!      REAL VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS, AND     
!      USING PARENTHESES TO OVERRIDE PRECEDENCES.                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      rvon01 = .11341e1                                                
      rvon02 = 7.1417                                                   
      rvon03 = 5.2113e1                                                
      rvon04 = 10.001                                                   
      rvon05 = 7.241e5                                                 
      rvon06 = 5.7777e-3                                               
      rvcomp = -rvon01 ** (rvon02 + rvon03 - rvon04) * (rvon05 / rvon06)
      rvcorr = -6.1635e10                                              
40150 if (rvcomp + 6.1640e10) 20150, 10150, 40151                      
40151 if (rvcomp + 6.1630e10) 10150, 10150, 20150                      
30150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10150, 0161, 20150                                    
10150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0161                                                        
20150 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0161 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 016  ****                         
!                                                                        
!           TEST 16 CONTINUES THE TESTING OF EXPRESSIONS USING ONLY      
!      REAL VARIABLE OPERANDS CONNECTED BY ARITHMETIC OPERATORS, AND     
!      USING PARENTHESES TO OVERRIDE PRECEDENCES.                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      rvon01 = 6.4003e18                                               
      rvon02 = -3.7717e-2                                              
      rvon03 = -5.1195e3                                               
      rvon04 = 1.7521e14                                               
      rvon05 = 1.0533e3                                                
      rvon06 = -9.4207e11                                              
      rvcomp = ((rvon01 * (rvon02 / rvon03)) + rvon04) / rvon05 -                (-rvon06)                                                
      rvcorr = -7.3096e11                                              
40160 if (rvcomp + 7.3101e11) 20160, 10160, 40161                      
40161 if (rvcomp + 7.3091e11) 10160, 10160, 20160                      
30160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10160, 0171, 20160                                    
10160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0171                                                        
20160 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0171 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 017  ****                         
!                                                                        
!           IN TEST 17, A REAL EXPRESSION INVOLVING ALL FIVE ARITHMETIC  
!      OPERATORS IS EVALUATED, BUT UNLIKE TESTS 11 THROUGH 16 WHERE      
!      ALL OPERANDS WERE REAL VARIABLES, THE OPERANDS IN TEST 17 ARE     
!      CLASSED AS REAL VARIABLES, REAL CONSTANTS, REAL ARRAY ELEMENTS,   
!      AND REAL FUNCTION REFERENCES.                                     
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      rvon01 = 5.247e10                                                
      ivon01 = 3                                                        
      rvon02 = 1.07e1                                                  
      rvon03 = 5.23                                                     
      rvon04 = 1.001                                                    
      rvon05 = 1.573                                                    
      radn11(3) = 0.3947e18                                            
      rvcomp = (rvon01 + 3.491e10) - (4e17 + radn11(ivon01)) /                 (rfos01(rvon03,rvon04,rvon05) ** rvon02)                 
      rvcorr = 7.1526e10                                               
40170 if (rvcomp - 7.1521e10) 20170, 10170, 40171                      
40171 if (rvcomp - 7.1531e10) 10170, 10170, 20170                      
30170 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10170, 0181, 20170                                    
10170 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0181                                                        
20170 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0181 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 018  ****                         
!                                                                        
!           TEST 18 IS IDENTICAL TO TEST 17 EXCEPT THAT PARENTHESES ARE  
!      USED TO CHANGE THE ORDER OF SUB-EXPRESSION EVALUATION.            
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      rvon01 = 5.247e10                                                
      ivon01 = 3                                                        
      rvon02 = 1.07e1                                                  
      rvon03 = 5.23                                                     
      rvon04 = 1.001                                                    
      rvon05 = 1.573                                                    
      radn11(3) = 0.3947e18                                            
      rvcomp = ((rvon01 + 3.491e10) - (4e17 + radn11(ivon01))) /               rfos01(rvon03,rvon04,rvon05) ** rvon02                   
      rvcorr = -1.5854e10                                              
40180 if (rvcomp + 1.5859e10) 20180, 10180, 40181                      
40181 if (rvcomp + 1.5849e10) 10180, 10180, 20180                      
30180 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10180, 0191, 20180                                    
10180 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0191                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0191 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 019  ****                         
!                                                                        
!           TEST 19 IS SIMILAR TO TESTS 17 AND 18 EXCEPT THAT THE        
!      FUNCTION REFERENCES IN TURN EVALUATE ARRAY ELEMENTS.              
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      rvon01 = 5.026e2                                                 
      rvon02 = 1.386e1                                                 
      ivon03 = 2                                                        
      rvon04 = 1.9999                                                   
      rvon05 = 4.0127                                                   
      radn11(1) = 3.004e18                                             
      radn11(2) = 2.5705e-1                                            
      radn11(4) = 7.993e16                                             
      rvcomp = (rvon01 - 5.902 * rfos02(int(rvon04),int(rvon05))) /              radn11(ivon03) + 1.5372 ** rvon02                        
      rvcorr = 1.4797e3                                                
40190 if (rvcorr - 1.4792e3) 20190, 10190, 40191                       
40191 if (rvcorr - 1.4802e3) 10190, 10190, 20190                       
30190 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10190, 0201, 20190                                    
10190 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0201                                                        
20190 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0201 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 020  ****                         
!                                                                        
!           TEST 20 EVALUATES A REAL EXPRESSION WHICH CONTAINS FUNCTION  
!      REFERENCES NESTED TO THREE LEVELS.  THE OUTER TWO LEVELS ARE      
!      STATEMENT FUNCTION REFERENCES AND THE INNERMOST LEVEL IS AN       
!      INTRINSIC FUNCTION REFERENCE.                                     
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      rvon01 = 4.7117e05                                               
      rvon02 = 5.987                                                    
      rvon03 = 2.00000e5                                               
      rvon04 = 1.0e2                                                   
      rvon05 = 1.5222e9                                                
      ivon06 = 4                                                        
      rvon07 = -3.2107e14                                              
      radn11(4) = 7.425e14                                             
      radn11(5) = -2.4015e5                                            
      rvcomp = (rvon01 + rfos03(rvon02,rvon03,rvon04)) * rvon05 -                rfos04(ivon06,rvon07)                                    
      rvcorr = -6.4580e15                                              
40200 if (rvcomp + 6.4585e15) 20200, 10200, 40201                      
40201 if (rvcomp + 6.4575e15) 10200, 10200, 20200                      
30200 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10200, 0211, 20200                                    
10200 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0211                                                        
20200 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0211 continue                                                          
!                                                                        
!           TESTS 21 THROUGH 25 DEAL WITH MIXTURES OF REAL AND INTEGER   
!      EXPRESSIONS; I.E., THESE ARE TESTS WHICH EVALUATE EXPRESSIONS     
!      CONTAINING BOTH REAL SUB-EXPRESSIONS AND INTEGER SUB-EXPRESSIONS  
!      AND THEN ASSIGN THE RESULTS TO EITHER AN INTEGER OR A REAL        
!      VARIABLE.                                                         
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 021  ****                         
!                                                                        
!           TEST 21 USES ALL FIVE ARITHMETIC OPERATORS AND A COMBINATION 
!      OF INTEGER AND REAL VARIABLES.  NO PARENTHESES ARE USED.  FINAL   
!      ASSIGNMENT IS TO AN INTEGER VARIABLE.                             
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivon01 = 17                                                       
      ivon02 = 3                                                        
      rvon03 = 5.4732e+2                                               
      rvon04 = 1.523                                                    
      ivon05 = 798                                                      
      ivcomp = ivon01 ** ivon02 + rvon03 - rvon04 * ivon05 / ivon01     
      ivcorr = 5388                                                     
40210 if (ivcomp - 5388) 20210, 10210, 20210                            
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
!      ****  FCVS PROGRAM 351  -  TEST 022  ****                         
!                                                                        
!           TEST 22 IS LIKE TEST 21 EXCEPT THAT PARENTHESES ARE USED,    
!      AS IS A UNARY OPERATOR.  FINAL ASSIGNMENT IS TO A REAL VARIABLE.  
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivon01 = 798                                                      
      ivon02 = 17                                                       
      rvon03 = 9.34578e-2                                              
      ivon04 = 15985                                                    
      rvon05 = 0.72357                                                  
      rvcomp = -(ivon01 / ivon02) + (rvon03 * ivon04 ** rvon05)         
      rvcorr = 5.68717e1                                               
40220 if (rvcomp - 5.6866e1) 20220, 10220, 40221                       
40221 if (rvcomp - 5.6876e1) 10220, 10220, 20220                       
30220 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10220, 0231, 20220                                    
10220 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0231                                                        
20220 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0231 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 023  ****                         
!                                                                        
!           TEST 23 IS SIMILAR TO TEST 22 EXCEPT THAT IT EMPLOYS NESTED  
!      PARENTHESES.                                                      
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 183                                                      
      rvon03 = 58.7025                                                  
      ivon04 = 197                                                      
      ivon05 = 87                                                       
      rvon06 = 2.4611e15                                               
      rvcomp = ivon01 ** (-(ivon02 + (rvon03 - ivon04)) -                        (ivon05 / rvon06))                                       
      rvcorr = 3.4931e-14                                              
40230 if (rvcomp - 3.4926e-14) 20230, 10230, 40231                     
40231 if (rvcomp - 3.4936e-14) 10230, 10230, 20230                     
30230 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10230, 0241, 20230                                    
10230 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0241                                                        
20230 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0241 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 351  -  TEST 024  ****                         
!                                                                        
!           TEST 24 IS IDENTICAL TO TEST 23 EXCEPT THAT THE FINAL        
!      ASSIGNMENT IS TO AN INTEGER VARIABLE INSTEAD OF A REAL VARIABLE.  
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 183                                                      
      rvon03 = 58.7025                                                  
      ivon04 = 197                                                      
      ivon05 = 87                                                       
      rvon06 = 2.4611e15                                               
      ivcomp = ivon01 ** (-(ivon02 + (rvon03 - ivon04)) -                        (ivon05 / rvon06))                                       
      ivcorr = 0                                                        
40240 if (ivcomp) 20240, 10240, 20240                                   
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
!      ****  FCVS PROGRAM 351  -  TEST 025  ****                         
!                                                                        
!           TEST 25 IS SIMILAR TO TESTS 9 AND 19 EXCEPT THAT A MIXTURE   
!      OF REAL AND INTEGER OPERANDS ARE USED, AND FINAL ASSIGNMENT IS    
!      TO A REAL VARIABLE.                                               
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      rvon01 = 4.7117                                                   
      rvon02 = 5.998                                                    
      ivon03 = 2                                                        
      rvon04 = 1e2                                                      
      ivon05 = 20                                                       
      ivon06 = 4                                                        
      ivon07 = -3                                                       
      radn11(4) = 7.425                                                 
      radn11(5) = -2.4015                                               
      rvcomp = (rvon01 + rfos06(aint(rvon02),ivon03,rvon04)) * ivon05 -          ifos05(ivon06,ivon07)                                    
      rvcorr =  84.234                                                  
40250 if (rvcomp - 84.229) 20250, 10250, 40251                          
40251 if (rvcomp - 84.239) 10250, 10250, 20250                          
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
90001 format (" ",24x,"FM351")                                          
90000 format (" ",20x,"END OF PROGRAM FM351" )                          
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
      end program fm351
