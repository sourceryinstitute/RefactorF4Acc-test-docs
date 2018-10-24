      program fm256
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
      logical  :: lvon01
      logical  :: lvon02
      integer :: ivon02
      real :: go
      real :: to
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      integer :: ivon06
      integer :: ivon07
      integer :: ivon08
      integer :: ivon09
      integer :: ivon10
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE IS A TEST OF THE DO STATEMENT.  THE DO IS TESTED  
!      BOTH OUTSIDE AND INSIDE THE BLOCK-IF STRUCTURE.  TESTS ARE MADE OF
!      THE DO-VARIABLE WHEN THE DO BECOMES INACTIVE.  OTHER TESTS CHECK  
!      LOOP AND INCREMENTATION PROCESSING.  THE DO-LOOP EXECUTION        
!      IS TESTED FOR THOSE CONDITIONS WHICH MAKE THE DO-LOOP INACTIVE.   
!                                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1978                                                 
!         SECTION 11.10,      DO STATEMENT                               
!         SECTION 11.10.1,    RANGE OF A DO-LOOP                         
!         SECTION 11.10.2,    ACTIVE AND INACTIVE DO-LOOPS               
!         SECTION 11.10.3,    EXECUTING A DO STATEMENT                   
!         SECTION 11.10.4,    LOOP CONTROL PROCESSING                    
!         SECTION 11.10.5,    EXECUTION OF THE RANGE                     
!         SECTION 11.10.6,    TERMINAL STATEMENT EXECUTION               
!         SECTION 11.10.7,    INCREMENTATION PROCESSING                  
!                                                                        
!         FM012 - TESTS THE DO STATEMENT WITH THE FORTRAN 66 CONCEPTS OF 
!                 EXTENDED RANGE OF A DO STATEMENT.                      
!                                                                        
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
!      ****  FCVS PROGRAM 256  -  TEST 001  ****                         
!                                                                        
!         TEST 001 CHECKS THE SIMPLE DO STATEMENT WITH THE OPTIONAL      
!      COMMAS AND ALL DO PARAMETERS SPECIFIED.  THE LOOP IS ACTIVE FOR   
!      TEN COUNTS.  THE FINAL VALUE OF THE INTEGER COUNTER SHOULD BE     
!      EQUAL TO TEN (10).  THE FORM OF THE DO STATEMENT USED IN THIS TEST
!      IS SHOWN BELOW -                                                  
!                                                                        
!         DO S, I = E1, E2, E3                                           
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      do 0012, ivon01 = 1, 10, 1                                        
      ivcomp = ivcomp + 1                                               
 0012 continue                                                          
      ivcorr = 10                                                       
40010 if ( ivcomp - 10 )  20010, 10010, 20010                           
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
!      ****  FCVS PROGRAM 256  -  TEST 002  ****                         
!                                                                        
!         TEST 002 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT THE COMMAS
!      THAT ARE OPTIONAL HAVE BEEN DELETED AS A SYNTAX CHECK.            
!                                                                        
!         THE INCREMENTATION PARAMETER IS OPTIONAL AND NOT PRESENT IN    
!      THIS TEST.    ACCORDING TO SECTION 11.10.3,  IF E3 DOES NOT APPEAR
!      THEN M3 HAS A VALUE OF ONE.  THE DO STATEMENT FOR THIS TEST IS OF 
!      THE FORM SHOWN BELOW -                                            
!                                                                        
!         DO S I = E1, E2                                                
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 0                                                        
      do ivon01 = 1, 10                                            
      ivcomp = ivcomp + 1                                               
  end do
      ivcorr = 10                                                       
40020 if ( ivcomp - 10 )  20020, 10020, 20020                           
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
!      ****  FCVS PROGRAM 256  -  TEST 003  ****                         
!                                                                        
!         TEST 003 HAS A DO STATEMENT INSIDE A BLOCKED IF STRUCTURE.     
!      THE LOGICAL EXPRESSION IS TRUE SO THE DO-LOOP SHOULD BE EXECUTED  
!      A TOTAL OF TEN TIMES.                                             
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 0                                                        
      lvon01 = .true.                                                   
      if ( lvon01 )  then                                               
           do 0032, ivon01 = 1, 10, 1                                   
           ivcomp = ivcomp + 1                                          
 0032      continue                                                     
      end if                                                            
      ivcorr = 10                                                       
40030 if ( ivcomp - 10 )  20030, 10030, 20030                           
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
!      ****  FCVS PROGRAM 256  -  TEST 004  ****                         
!                                                                        
!         TEST 004 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT THE DO    
!      STATEMENT IS LOCATED IN AN ELSE IF-BLOCK.  THE DO-LOOP SHOULD BE  
!      EXECUTED FIVE (5) TIMES.                                          
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = 32000                                               
      else if ( lvon02 )  then                                          
           do ivon01 = 1, 5                                        
           ivcomp = ivcomp + 1                                          
  end do
      end if                                                            
      ivcorr = 5                                                        
40040 if ( ivcomp - 5 )  20040, 10040, 20040                            
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
!      ****  FCVS PROGRAM 256  -  TEST 005  ****                         
!                                                                        
!         TEST 005 IS SIMILAR TO THE PREVIOUS TWO TESTS EXCEPT THAT THE  
!      DO STATEMENT IS CONTAINED IN AN ELSE-BLOCK.  THE DO-LOOP SHOULD BE
!      EXECUTED A TOTAL OF 3 TIMES.                                      
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 0                                                        
      lvon01 = .false.                                                  
      lvon02 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = 100                                                 
      else if ( lvon02 )  then                                          
           ivcomp = 1000                                                
      else                                                              
           do 0052, ivon01 = 1, 3                                       
           ivcomp = ivcomp + 1                                          
 0052      continue                                                     
      end if                                                            
      ivcorr = 3                                                        
40050 if ( ivcomp - 3 )  20050, 10050, 20050                            
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
!      ****  FCVS PROGRAM 256  -  TEST 006  ****                         
!                                                                        
!         TEST 006 HAS A BLOCKED IF STRUCTURE INSIDE A DO-LOOP.          
!      THE LOOP IS EXECUTED THREE (3) TIMES.  ALL THREE PARTS OF THE     
!      BLOCK-IF STRUCTURE SHOULD BE EXECUTED.                            
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 1                                                        
      do 0062, ivon01 = 3, 5, 1                                         
      if ( ivon01  <=  3 )  then                                        
           ivcomp = ivcomp * 2                                          
      else if ( ivon01  >  3 .and. ivon01  <  5 )  then               
           ivcomp = ivcomp * 3                                          
      else                                                              
           ivcomp = ivcomp * 5                                          
      end if                                                            
 0062 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 30 = 1 * 2 * 3 * 5   ****
!                                                                        
      ivcorr = 30                                                       
40060 if ( ivcomp - 30 )  20060, 10060, 20060                           
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
!                                                                        
!         THE FOLLOWING SERIES OF TESTS CHECK THE DO-VARIABLE WHEN THE   
!      DO-LOOP BECOMES INACTIVE.  ACCORDING TO SECTION 11.10.2,  WHEN A  
!      DO-LOOP BECOMES INACTIVE, THE DO-VARIABLE OF THE DO-LOOP RETAINS  
!      ITS LAST DEFINED VALUE.                                           
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 256  -  TEST 007  ****                         
!                                                                        
!         TEST 007 CHECKS THAT THE DO-VARIABLE CONTAINS ITS LAST DEFINED 
!      VALUE WHEN THE ITERATION COUNT IS ZERO.                           
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      ivon02 = 0                                                        
      do ivon01 = 100, 105, 2                                      
      ivon02 = ivon02 + 1                                               
  end do
      ivcomp = ivon01                                                   
      ivcorr = 106                                                      
40070 if ( ivcomp - 106 )  20070, 10070, 20070                          
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
!      ****  FCVS PROGRAM 256  -  TEST 008  ****                         
!                                                                        
!         TEST 008 CHECKS THAT THE LOOP COUNTER IN THE PREVIOUS TEST HAD 
!      A VALUE OF THREE TO SHOW THAT THE DO-LOOP WAS EXECUTED THREE TIMES
!      BEFORE TERMINATING ( BECOMMING INACTIVE ).                        
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon02                                                   
      ivcorr = 3                                                        
40080 if ( ivcomp - 3 )  20080, 10080, 20080                            
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
!      ****  FCVS PROGRAM 256  -  TEST 009  ****                         
!                                                                        
!         TEST 009 CHECKS THAT A DO-LOOP BECOMES INACTIVE IF THERE IS A  
!      TRANSFER OF CONTROL OUTSIDE THE RANGE OF THE DO-LOOP.  THE TRANS- 
!      FER MUST BE INSIDE OF THE SAME PROGRAM UNIT - NOT A CALL OR       
!      FUNCTION REFERENCE TO A SUBPROGRAM.                               
!                                                                        
!         THIS IS A SIGNIFICANT DIFFERENCE BETWEEN FORTRAN 66 AND FORTRAN
!      77.  FORTRAN 66 HAD AN EXTENDED RANGE OF THE DO FEATURE WHICH     
!      ALLOWED FOR A TRANSFER OUTSIDE THE RANGE OF A DO-LOOP WITHOUT     
!      MAKING THE DO-LOOP INACTIVE.                                      
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      do ivon01 = 1, 7                                             
      if ( ivon01  >=  3 )  goto 0093                                    !Break
  end do
 0093 ivcomp = ivon01                                                   
      ivcorr = 3                                                        
40090 if ( ivcomp - 3 )  20090, 10090, 20090                            
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
!      ****  FCVS PROGRAM 256  -  TEST 010  ****                         
!                                                                        
!         TEST 010 CHECKS FOR AN INITIAL COUNT EQUAL TO ZERO BECAUSE     
!      M1 IS GREATER THAN M2 AND M3 IS GREATER THAN ZERO - SEE SECTION   
!      11.10.3 FOR CONDITIONS WHICH MAKE THE ITERATION COUNT ZERO.       
!      THE LOOP SHOULD NOT BE EXECUTED AT ALL.                           
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      do 0102, ivon01 = 100, 10, 3                                      
      ivcomp = ivcomp + 1                                               
 0102 continue                                                          
      ivcorr = 0                                                        
40100 if ( ivcomp )  20100, 10100, 20100                                
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
!      ****  FCVS PROGRAM 256  -  TEST 011  ****                         
!                                                                        
!         TEST 011 CHECKS FOR THE PROPER EXECUTION OF THE STEPS AS SHOWN 
!      IN SECTION 11.10.3 - EXECUTING A DO STATEMENT.  THE VARIABLE IVON0
!      SHOULD HAVE BEEN SET TO 100.  THE ITERATION COUNT IS ZERO BY THE  
!      FORMULA IN 11.10.3(3). AS DESCRIBED IN SECTION 11.10.4 - THE      
!      ITERATION COUNT IS TESTED.  IF IT IS NOT ZERO, EXECUTION OF THE   
!      FIRST STATEMENT IN THE RANGE OF THE DO-LOOP BEGINS.  IF THE       
!      ITERATION COUNT IS ZERO, THE DO-LOOP BECOMES INACTIVE.            
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon01                                                   
      ivcorr = 100                                                      
40110 if ( ivcomp - 100 )  20110, 10110, 20110                          
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
!                                                                        
!         THE FOLLOWING TWO TESTS ARE SIMILAR TO THE PREVIOUS TWO TESTS  
!      IN THAT THE PARAMETERS OF THE DO STATEMENT MAKE THE ITERATION     
!      COUNT ZERO WHEN THE DO STATEMENT IS EXECUTED.                     
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 256  -  TEST 012  ****                         
!                                                                        
!         TEST 012 HAS M1 LESS THAN M2, BUT M3 IS NEGATIVE.  THE LOOP    
!      SHOULD NOT BE EXECUTED AT ALL.                                    
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 0                                                        
      do ivon01 = 10, 100, -3                                      
      ivcomp = ivcomp + 1                                               
  end do
      ivcorr = 0                                                        
40120 if ( ivcomp )  20120, 10120, 20120                                
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
!      ****  FCVS PROGRAM 256  -  TEST 013  ****                         
!                                                                        
!         TEST 013 CHECKS THAT THE VALUE RETAINED FOR THE DO-VARIABLE    
!      IN THE PREVIOUS TEST IS EQUAL TO THE INITIAL PARAMETER VALUE - M3.
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon01                                                   
      ivcorr = 10                                                       
40130 if ( ivcomp - 10 )  20130, 10130, 20130                           
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
!      ****  FCVS PROGRAM 256  -  TEST 014  ****                         
!                                                                        
!         TEST 014 CHECKS FOR ONE EXECUTION OF THE RANGE OF A DO-LOOP    
!      ACCORDING TO THE FORMULA SHOWN IN 11.10.3(3) WITH M1 = M2.        
!                                                                        
!         THE DO-LOOPS IN THIS TEST ARE A NEST OF THREE EACH WITH ITS    
!      OWN TERMINAL STATEMENT.                                           
!                                                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 1                                                        
      do ivon01 = 1, 1, 1                                          
           ivcomp = ivcomp * 2                                          
           do ivon02 = 10,10,10                                    
                ivcomp = ivcomp * 3                                     
                do 0142, ivon03 = 100, 100, -2                          
                     ivcomp = ivcomp * 5                                
 0142           continue                                                
  end do
  end do
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 30 = 1 * 2 * 3 * 5    ***
!                                                                        
      ivcorr = 30                                                       
40140 if ( ivcomp - 30 )  20140, 10140, 20140                           
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
!      ****  FCVS PROGRAM 256  -  TEST 015  ****                         
!                                                                        
!         TEST 015 IS A CHECK ON THE FIRST EXAMPLE SHOWN IN SECTION      
!      11.10.7.  THIS IS A TEST OF INCREMENTATION PROCESSING OF TWO NEST 
!      DO-LOOPS HAVING THE SAME TERMINAL STATEMENT.                      
!                                                                        
!         THIS IS A TEST OF A DO-LOOP THAT BECOMES ACTIVE INSIDE AN      
!      ALREADY ACTIVE DO-LOOP.                                           
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 0                                                        
      do ivon02 = 1, 10                                            
      ivon03 = ivon02                                                   
      do ivon04 = 1, 5                                             
      ivon05 = ivon04                                                   
 0152 ivon01 = ivon01 + 1                                               
      end do
      end do
 0153 continue                                                          
      ivcomp = ivon02                                                   
!      THIS IS THE VALUE FOR I IN THE EXAMPLE.                           
      ivcorr = 11                                                       
40150 if ( ivcomp - 11 )  20150, 10150, 20150                           
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
!      ****  FCVS PROGRAM 256  -  TEST 016  ****                         
!                                                                        
!         TEST 016 CHECKS THE VALUE OF J (IVON03) IN THE FIRST EXAMPLE.  
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon03                                                   
      ivcorr = 10                                                       
40160 if ( ivcomp - 10 )  20160, 10160, 20160                           
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
!      ****  FCVS PROGRAM 256  -  TEST 017  ****                         
!                                                                        
!         TEST 017 CHECKS THE VALUE OF K (IVON04) IN THE FIRST EXAMPLE.  
!                                                                        
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon04                                                   
      ivcorr = 6                                                        
40170 if ( ivcomp - 6  )  20170, 10170, 20170                           
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
!      ****  FCVS PROGRAM 256  -  TEST 018  ****                         
!                                                                        
!         TEST 018 CHECKS THE VALUE OF L (IVON05) IN THE FIRST EXAMPLE.  
!                                                                        
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon05                                                   
      ivcorr = 5                                                        
40180 if ( ivcomp - 5  )  20180, 10180, 20180                           
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
!      ****  FCVS PROGRAM 256  -  TEST 019  ****                         
!                                                                        
!         TEST 019 CHECKS THE VALUE OF N (IVON01) IN THE FIRST EXAMPLE.  
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon01                                                   
      ivcorr = 50                                                       
40190 if ( ivcomp - 50 )  20190, 10190, 20190                           
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
!      ****  FCVS PROGRAM 256  -  TEST 020  ****                         
!                                                                        
!         TEST 020 IS A CHECK ON THE SECOND EXAMPLE IN SECTION 11.10.7.  
!      IN THIS EXAMPLE, THE INNER DO-LOOP BECOMES ACTIVE AND THEN        
!      IMMEDIATELY INACTIVE INSIDE AN ALREADY ACTIVE OUTER DO-LOOP.      
!                                                                        
!         ALTHOUGH IN SOME WAYS SIMILAR TO THE FIRST EXAMPLE, THE SECOND 
!      EXAMPLE SHOULD HAVE DIFFERENT FINAL VALUES ON THE INTEGER COUNTERS
!      AND THE VALUE OF L (IVON10) WILL NOT BE TESTED BECAUSE IT IS NOT  
!      DEFINED DURING THE RANGE OF THE DO-LOOP INVOLVED.                 
!                                                                        
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 0                                                        
      ivon06 = 0                                                        
      do ivon07 = 1, 10                                            
      ivon08 = ivon07                                                   
      do ivon09 = 5, 1                                             
      ivon10 = ivon09                                                   
 0202 ivon06 = ivon06 + 1                                               
      end do
      end do
 0203 continue                                                          
      ivcomp = ivon07                                                   
!      THIS IS THE VALUE FOR I IN THE SECOND EXAMPLE.                    
      ivcorr = 11                                                       
40200 if ( ivcomp - 11 )  20200, 10200, 20200                           
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
!      ****  FCVS PROGRAM 256  -  TEST 021  ****                         
!                                                                        
!         TEST 021 CHECKS THE VALUE OF J (IVON08) IN THE SECOND EXAMPLE. 
!                                                                        
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon08                                                   
      ivcorr = 10                                                       
40210 if ( ivcomp - 10 )  20210, 10210, 20210                           
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
!      ****  FCVS PROGRAM 256  -  TEST 022  ****                         
!                                                                        
!         TEST 022 CHECKS THE VALUE OF K (IVON09) IN THE SECOND EXAMPLE. 
!                                                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon09                                                   
      ivcorr = 5                                                        
40220 if ( ivcomp - 5  )  20220, 10220, 20220                           
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
!      ****  FCVS PROGRAM 256  -  TEST 023  ****                         
!                                                                        
!         TEST 023 CHECKS THE VALUE OF N (IVON06) IN THE SECOND EXAMPLE. 
!                                                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon06                                                   
      ivcorr = 0                                                        
40230 if ( ivcomp - 0  )  20230, 10230, 20230                           
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
!      ****  FCVS PROGRAM 256  -  TEST 024  ****                         
!                                                                        
!         TEST 024 IS A CHECK ON USING A LOGICAL IF STATEMENT AS THE     
!      TERMINAL STATEMENT IN THE RANGE OF A DO-LOOP.  THE LOGICAL IF     
!      STATEMENT HAS AN UNCONDITIONAL GO TO STATEMENT AS ITS EXECUTABLE  
!      STATEMENT  AS ALLOWED IN SECTION 11.10.                           
!                                                                        
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivcomp = 0                                                        
      do 0242 ivon01 = 1, 10                                            
      ivcomp = ivcomp + 1                                               
 0242 if ( ivon01  >=  5 )  goto 0243                                    !Break
!                                                                        
!                                                                        
!      IF THE LOGIC DOES NOT BRANCH OUT OF THE RANGE OF THE DO-LOOP WHEN 
!      THE DO-VARIABLE (IVON01) IS EQUAL TO FIVE (5), THEN IVCOMP WILL BE
!      SET BACK TO THE VALUE OF ZERO.                                    
!                                                                        
      ivcomp = 0                                                        
!                                                                        
!                                                                        
 0243 ivcorr = 5                                                        
40240 if ( ivcomp - 5 )  20240, 10240, 20240                            
30240 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10240, 0251, 20240                                    
10240 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0251                                                          !Break
20240 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0251 continue                                                          
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
90001 format (" ",24x,"FM256")                                          
90000 format (" ",20x,"END OF PROGRAM FM256" )                          
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
      end program fm256
