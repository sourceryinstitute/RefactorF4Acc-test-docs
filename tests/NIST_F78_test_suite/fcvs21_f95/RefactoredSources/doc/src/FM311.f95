      program fm311
!                                                                        
!                                                                        
!         THIS ROUTINE TESTS THE USE OF THE FORTRAN IN-LINE STATEMENT    
!      FUNCTION OF TYPES INTEGER, REAL AND LOGICAL.  SPECIFIC FEATURES   
!      TESTED INCLUDE,                                                   
!                                                                        
!         A) REAL STATEMENT FUNCTIONS USING REAL CONSTANTS AND VARIABLES 
!            IN THE EXPRESSION AND AS ACTUAL ARGUMENTS.                  
!                                                                        
!         B) STATEMENT FUNCTIONS WHICH REQUIRE CONVERSION OF THE         
!            EXPRESSION TO REAL AND INTEGER TYPING.                      
!                                                                        
!         C) THE USE OF VARIABLES, ARRAY ELEMENTS, EXTERNAL REFERENCES,  
!            AND INITIALLY DEFINED ENITIIES IN THE EXPRESSION.           
!                                                                        
!         D) VARIOUS DEFINITIONS AND USES OF DUMMY ARGUMENTS.            
!                                                                        
!         E) ACTUAL ARGUMENTS CONSISTING OF EXPRESSIONS, INTRINSIC       
!            FUNCTION REFERENCES, AND EXTERNAL FUNCTION REFERENCES.      
!                                                                        
!         F) CONFIRMING AND OVERRIDING THE TYPING OF STATEMENT FUNCTIONS 
!            AND DUMMY ARGUMENTS.                                        
!                                                                        
!         G) USE OF STATEMENT FUNCTIONS AND DUMMY ARGUMENTS IN THE MAIN  
!            PROGRAM AND IN EXTERNAL FUNCTION AND SUBROUTINE SUBPROGRAMS.
!                                                                        
!      THE SUBSET LEVEL FEATURES OF STATEMENT FUNCTIONS ARE ALSO TESTED  
!      IN ROUTINE FM020.                                                 
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 8.3,    COMMON STATEMENT                               
!         SECTION 8.4,    TYPE-STATEMENT                                 
!         SECTION 8.5,    IMPLICIT STATEMENT                             
!         SECTION 8.7,    EXTERNAL STATEMENT                             
!         SECTION 8.8,    INTRINSIC STATEMENT                            
!         SECTION 9,      DATA STATEMENT                                 
!         SECTION 15.3,   INTRINSIC FUNCTIONS                            
!         SECTION 15.4,   STATEMENT FUNCTION                             
!         SECTION 15.5,   EXTERNAL FUNCTIONS                             
!         SECTION 15.6,   SUBROUTINES                                    
!         SECTION 15.9.1, DUMMY ARGUMENTS                                
!         SECTION 15.9.2, ACTUAL ARGUMENTS                               
!         SECTION 15.9.3, ASSOCIATION OF DUMMY AND ACTUAL ARGUMENTS      
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
      integer :: ivcn01
      real :: rfos01
      real :: rdon01
      real :: rfos02
      real :: rdon02
      real :: rfos03
      real :: rdon03
      integer :: ifos01
      real :: rdon04
      real :: rfos04
      integer :: idon01
      integer :: ifos02
      integer :: idon02
      integer :: ifos03
      integer :: idon03
      integer :: ivon01
      real :: rfos05
      real :: rdon05
      real :: rvon02
      logical  :: lfos01
      logical  :: ldon01
      logical  :: lvon01
      integer :: ifos04
      integer :: idon04
      real :: rfos06
      real :: rdon06
      logical  :: lfos02
      logical  :: ldon02
      real :: rfos07
      integer :: idon05
      integer :: ifos05
      integer :: idon06
      integer :: ifos06
      integer :: idon07
      integer :: ifos07
      integer :: idon08
      integer :: ivond1
      integer :: ifos08
      integer :: idon09
      integer :: ifos09
      integer :: idon10
      integer :: ifos10
      integer :: ivon02
      integer :: ifos11
      integer :: idon11
      integer :: idon13
      integer :: idon12
      integer :: ifos12
      integer :: idon14
      integer :: ifos13
      integer :: ifos14
      integer :: idon15
      real  :: kfos01
      integer :: idon16
      integer  :: afos01
      real :: rdon07
      real :: rfos08
      real  :: mdon01
      real :: rfos09
      integer  :: bdon01
      real :: rfos10
      real :: rfos11
      integer :: ifos15
      integer :: ivon04
      integer :: ifos16
      integer :: idon17
      integer :: ifos17
      integer :: idon18
      integer :: ifos19
      integer :: idon21
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvcomp
      real :: rvcorr
      real :: rvon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon03
      integer :: ivon05
      real :: rvon05
      real :: ndon01
      integer :: edon01
      integer :: ff312! decl of func/sub in program
      integer :: ff314! decl of func/sub in program
      external ff312                                                    
      intrinsic nint                                                    
      real, dimension(1:4) :: radn11
      real, dimension(1:4) :: radn12
      real, dimension(1:4) :: radn13
      integer, dimension(1:4) :: iadn11
      integer, dimension(1:4) :: iadn12
      logical, dimension(1:4) :: ladn11
      data ivond1 / 6 / 
!      TEST 001                                                          
               rfos01(rdon01) = 3.5                                     
!      TEST 002                                                          
               rfos02(rdon02) = rdon02                                  
!      TEST 003                                                          
               rfos03(rdon03) = rdon03 + 1.0                            
!      TEST 004                                                          
               ifos01(rdon04) = rdon04 + 1.0                            
!      TEST 005                                                          
               rfos04(idon01) = idon01 + 1                              
!      TEST 006                                                          
               ifos02(idon02) = idon02 + 1.95                           
!      TEST 007                                                          
               ifos03(idon03) = idon03 + ivon01                         
!      TEST 008                                                          
               rfos05(rdon05) = rdon05 + rvon02                         
!      TEST 009                                                          
               lfos01(ldon01) = ldon01 .or. lvon01                      
!      TEST 010                                                          
               ifos04(idon04) = idon04 + iadn11(1)                      
!      TEST 011                                                          
               rfos06(rdon06) = rdon06 + radn12(3)                      
!      TEST 012                                                          
               lfos02(ldon02) = .not. ldon02 .and. ladn11(2)            
!      TEST 013                                                          
               rfos07(idon05) = radn13(idon05)                          
!      TEST 014                                                          
               ifos05(idon06)  = idon06+ff312(4)
!      TEST 015                                                          
               ifos06(idon07) = (idon07 + 1)                            
!      TEST 016                                                          
               ifos07(idon08) = idon08 + ivond1                         
!      TEST 017                                                          
               ifos08(idon09) = idon09 + 1                              
               ifos09(idon10) = ifos08(idon10) + 1                      
!      TEST 018                                                          
               ifos10() = ivon02                                        
!      TEST 019                                                          
               ifos11(idon11,idon12,idon13) = idon11 + idon12 + idon13  
!      TEST 020                                                          
               ifos12(idon14) = idon14 + 1                              
               ifos13(idon14) = idon14 + 2                              
!      TEST 021,022,023                                                  
               ifos14(idon15) = idon15 + 1                              
!      TEST 024                                                          
               kfos01(idon16) = idon16 + 1.0                            
!      TEST 025                                                          
               afos01(rdon07) = rdon07 + 1.0                            
!      TEST 026                                                          
               rfos08(mdon01) = mdon01 / 5                              
!      TEST 027                                                          
               rfos09(bdon01) = bdon01 / 5                              
!      TEST 028                                                          
               rfos10(ndon01) = ndon01 / 5                              
!      TEST 029                                                          
               rfos11(edon01) = edon01 / 5                              
!      TEST 030                                                          
               ifos15(ivon04) = ivon04 + 1                              
!      TEST 031                                                          
               ifos16(idon17) = idon17 + 1                              
!      TEST 032                                                          
               ifos17(idon18) = idon18 + 1                              
!      TEST 037                                                          
               ifos19(idon21) = idon21 + 1                              
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
!      TEST 001 THROUGH TEST 003 TEST REAL STATEMENT FUNCTIONS WHERE THE 
!      EXPRESSION CONSISTS OF REAL CONSTANTS AND VARIABLES AND THE ACTUAL
!      ARGUMENTS ARE EITHER REAL CONSTANTS OR VARIABLES.                 
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 001  ****                         
!                                                                        
!      EXPRESSION CONSISTS OF REAL CONSTANT (NO DUMMY ARGUMENT).         
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      rvcomp = 0.0                                                      
      rvcomp = rfos01(1.0)                                              
      rvcorr = 3.5                                                      
40010 if (rvcomp - 3.4995) 20010, 10010, 40011                          
40011 if (rvcomp - 3.5005) 10010, 10010, 20010                          
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10010, 0021, 20010                                    
10010 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0021                                                        
20010 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 002  ****                         
!                                                                        
!      DUMMY ARGUMENT USED IN EXPRESSION AND ACTUAL ARGUMENT IS REAL     
!      CONSTANT.                                                         
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvcomp = 0.0                                                      
      rvcomp = rfos02(1.3333)                                           
      rvcorr = 1.3333                                                   
40020 if (rvcomp - 1.3328) 20020, 10020, 40021                          
40021 if (rvcomp - 1.3338) 10020, 10020, 20020                          
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10020, 0031, 20020                                    
10020 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0031                                                        
20020 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0031 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 003  ****                         
!                                                                        
!      DUMMY ARGUMENT USED IN EXPRESSION AND ACTUAL ARGUMENT IS REAL     
!      VARIABLE.                                                         
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = 4.5                                                      
      rvcomp = rfos03(rvon01)                                           
      rvcorr = 5.5                                                      
40030 if (rvcomp - 5.4995) 20030, 10030, 40031                          
40031 if (rvcomp - 5.5005) 10030, 10030, 20030                          
30030 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10030, 0041, 20030                                    
10030 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0041                                                        
20030 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0041 continue                                                          
!                                                                        
!      TEST 004 THROUGH TEST 006 TEST STATEMENT FUNCTIONS WHICH REQUIRE  
!      TYPE CONVERSION OF THE EXPRESSION.                                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 004  ****                         
!                                                                        
!      INTEGER STATEMENT FUNCTION WITH REAL EXPRESSION.                  
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos01(2.3)                                              
      ivcorr = 3                                                        
40040 if (ivcomp - 3) 20040, 10040, 20040                               
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
!      ****  FCVS PROGRAM 311  -  TEST 005  ****                         
!                                                                        
!      REAL STATEMENT FUNCTION WITH INTEGER EXPRESSION                   
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      rvcomp = 0.0                                                      
      rvcomp = rfos04(3)                                                
      rvcorr = 4.0                                                      
40050 if (rvcomp - 3.9995) 20050, 10050, 40051                          
40051 if (rvcomp - 4.0005) 10050, 10050, 20050                          
30050 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10050, 0061, 20050                                    
10050 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0061                                                        
20050 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 006  ****                         
!                                                                        
!      INTEGER STATEMENT FUNCTION WITH EXPRESSION CONSISTING OF INTEGER  
!      AND REAL PRIMARIES.                                               
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos02(2)                                                
      ivcorr = 3                                                        
40060 if (ivcomp - 3) 20060, 10060, 20060                               
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
!      TEST 007 THROUGH TEST 017 TEST THE USAGE OF VARIOUS PRIMARIES     
!      IN THE EXPRESSION OF A STATEMENT FUNCTION.                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 007  ****                         
!                                                                        
!      USE INTEGER VARIABLE AS PRIMARY                                   
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 3                                                        
      ivcomp = ifos03(4)                                                
      ivcorr = 7                                                        
40070 if (ivcomp - 7) 20070, 10070, 20070                               
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
!      ****  FCVS PROGRAM 311  -  TEST 008  ****                         
!                                                                        
!      USE REAL VARIABLE AS PRIMARY.                                     
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      rvcomp = 0.0                                                      
      rvon02 = 1.5                                                      
      radn11(2) = 1.3                                                   
      rvcomp = rfos05(radn11(2))                                        
      rvcorr = 2.8                                                      
40080 if (rvcomp - 2.7995) 20080, 10080, 40081                          
40081 if (rvcomp - 2.8005) 10080, 10080, 20080                          
30080 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10080, 0091, 20080                                    
10080 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0091                                                        
20080 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0091 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 009  ****                         
!                                                                        
!      USE LOGICAL VARIABLE AS PRIMARY.                                  
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      lvon01 = .true.                                                   
      ivcomp = 0                                                        
      if (lfos01(.false.)) ivcomp = 1                                   
      ivcorr = 1                                                        
40090 if (ivcomp - 1) 20090, 10090, 20090                               
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
!      ****  FCVS PROGRAM 311  -  TEST 010  ****                         
!                                                                        
!      USE INTEGER ARRAY ELEMENT NAME AS PRIMARY.                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      iadn11(1) = 7                                                     
      ivcomp = ifos04(-4)                                               
      ivcorr = 3                                                        
40100 if (ivcomp - 3) 20100, 10100, 20100                               
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
!      ****  FCVS PROGRAM 311  -  TEST 011  ****                         
!                                                                        
!      USE REAL ARRAY ELEMENT NAME AS PRIMARY.                           
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      rvcomp = 0.0                                                      
      radn12(3) = 1.23                                                  
      rvcomp = rfos06(3.0)                                              
      rvcorr = 4.23                                                     
40110 if (rvcomp - 4.2295) 20110, 10110, 40111                          
40111 if (rvcomp - 4.2305) 10110, 10110, 20110                          
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
!      ****  FCVS PROGRAM 311  -  TEST 012  ****                         
!                                                                        
!      USE LOGICAL ARRAY ELEMENT NAME AS PRIMARY.                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ladn11(2) = .true.                                                
      ivcomp = 0                                                        
      if (lfos02(.false.)) ivcomp = 1                                   
      ivcorr = 1                                                        
40120 if (ivcomp - 1) 20120, 10120, 20120                               
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
!      ****  FCVS PROGRAM 311  -  TEST 013  ****                         
!                                                                        
!      USE A REAL ARRAY ELEMENT NAME AS PRIMARY WHERE THE SUBSCRIPT      
!      VALUE IS THE DUMMY ARGUMENT NAME.                                 
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvcomp = 0.0                                                      
      radn13(4) = 13.4                                                  
      rvcomp = rfos07(4)                                                
      rvcorr = 13.4                                                     
40130 if (rvcomp - 13.395) 20130, 10130, 40131                          
40131 if (rvcomp - 13.405) 10130, 10130, 20130                          
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
!      ****  FCVS PROGRAM 311  -  TEST 014  ****                         
!                                                                        
!      USE EXTERNAL FUNCTION REFERENCE AS PRIMARY.                       
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos05(6)                                                
      ivcorr = 11                                                       
40140 if (ivcomp - 11) 20140, 10140, 20140                              
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
!      ****  FCVS PROGRAM 311  -  TEST 015  ****                         
!                                                                        
!      USE EXPRESSION ENCLOSED IN PARENTHESES.                           
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos06(4)                                                
      ivcorr = 5                                                        
40150 if (ivcomp - 5) 20150, 10150, 20150                               
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
!      ****  FCVS PROGRAM 311  -  TEST 016  ****                         
!                                                                        
!      USE VARIABLE INITIALLY DEFINED IN DATA STATEMENT AS PRIMARY.      
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos07(3)                                                
      ivcorr = 9                                                        
40160 if (ivcomp - 9) 20160, 10160, 20160                               
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
!      ****  FCVS PROGRAM 311  -  TEST 017  ****                         
!                                                                        
!      USE PREVIOUSLY DEFINED STATEMENT FUNCTION REFERENCE AS PRIMARY.   
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos09(3)                                                
      ivcorr = 5                                                        
40170 if (ivcomp - 5) 20170, 10170, 20170                               
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
!      TEST 018 THROUGH TEST 020 APPLY TO THE DEFINITION OF THE          
!      STATEMENT FUNCTION DUMMY ARGUMENTS.                               
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 018  ****                         
!                                                                        
!      DEFINE STATEMENT FUNCTION WITH NO DUMMY ARGUMENTS.                
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 0                                                        
      ivon02 = 4                                                        
      ivcomp = ifos10()                                                 
      ivcorr = 4                                                        
40180 if (ivcomp - 4) 20180, 10180, 20180                               
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
!      ****  FCVS PROGRAM 311  -  TEST 019  ****                         
!                                                                        
!      DEFINE STATEMENT FUNCTION WITH THREE DUMMY ARGUMENTS.             
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos11(1,2,3)                                            
      ivcorr = 6                                                        
40190 if (ivcomp - 6) 20190, 10190, 20190                               
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
!      ****  FCVS PROGRAM 311  -  TEST 020  ****                         
!                                                                        
!      USE THE SAME DUMMY ARGUMENT NAME IN TWO DIFFERENT                 
!      STATEMENT FUNCTIONS.                                              
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 1                                                        
      if (ifos12(3)  ==  4) ivcomp = ivcomp * 2                         
      if (ifos13(4)  ==  6) ivcomp = ivcomp * 3                         
      ivcorr = 6                                                        
!      6 = 2 * 3                                                         
40200 if (ivcomp - 6) 20200, 10200, 20200                               
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
!      TEST 021 THROUGH TEST 022 TEST THE USAGE OF DIFFERENT TYPES OF    
!      ACTUAL ARGUMENTS IN A STATEMENT FUNCTION REFERENCE.               
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 021  ****                         
!                                                                        
!      USE AN EXPRESSION WITH OPERATORS AS AN ACTUAL ARGUMENT.           
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 0                                                        
      ivon03 = 4                                                        
      ivcomp = ifos14(ivon03 * 4 + 1)                                   
      ivcorr = 18                                                       
40210 if (ivcomp - 18) 20210, 10210, 20210                              
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
!      ****  FCVS PROGRAM 311  -  TEST 022  ****                         
!                                                                        
!      USE AN INTRINSIC FUNCTION REFERENCE AS AN ACTUAL ARGUMENT.        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivcomp = 0                                                        
      rvon01 = 1.75                                                     
      ivcomp = ifos14(nint(rvon01))                                     
      ivcorr = 3                                                        
40220 if (ivcomp - 3) 20220, 10220, 20220                               
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
!      ****  FCVS PROGRAM 311  -  TEST 023  ****                         
!                                                                        
!      USE AN EXTERNAL FUNCTION REFERENCE AS AN ACTUAL ARGUMENT.         
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ifos14(ff312(5))
      ivcorr = 7                                                        
40230 if (ivcomp - 7) 20230, 10230, 20230                               
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
!      TEST 024 THROUGH TEST 029 APPLY TO THE TYPING OF STATEMENT        
!      FUNCTIONS AND THE ASSOCIATED DUMMY ARGUMENT NAMES.                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 024  ****                         
!                                                                        
!      OVERRIDE THE INTEGER DEFAULT TYPING OF A STATEMENT FUNCTION WITH  
!      THE IMPLICIT STATEMENT TYPING OF REAL.                            
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = kfos01(3) / 5                                            
      rvcorr = 0.8                                                      
40240 if (rvcomp - .79995) 20240, 10240, 40241                          
40241 if (rvcomp - .80005) 10240, 10240, 20240                          
30240 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10240, 0251, 20240                                    
10240 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0251                                                        
20240 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0251 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 025  ****                         
!                                                                        
!      OVERRIDE THE REAL DEFAULT TYPING OF A STATEMENT FUNCTION WITH     
!      THE IMPLICIT STATEMENT TYPING OF INTEGER.                         
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = afos01(3.0) / 5                                          
      rvcorr = 0.0                                                      
40250 if (rvcomp + .00005) 20250, 10250, 40251                          
40251 if (rvcomp - .00005) 10250, 10250, 20250                          
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
!      ****  FCVS PROGRAM 311  -  TEST 026  ****                         
!                                                                        
!      OVERRIDE THE INTEGER DEFAULT TYPING OF A STATEMENT FUNCTION       
!      DUMMY ARGUMENT WITH THE IMPLICIT STATEMENT TYPING OF REAL.        
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = rfos08(4.0)                                              
      rvcorr = 0.8                                                      
40260 if (rvcomp - .79995) 20260, 10260, 40261                          
40261 if (rvcomp - .80005) 10260, 10260, 20260                          
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
!      ****  FCVS PROGRAM 311  -  TEST 027  ****                         
!                                                                        
!      OVERRIDE THE REAL DEFAULT TYPING OF A STATEMENT FUNCTION DUMMY    
!      ARGUMENT WITH THE IMPLICIT STATEMENT TYPING OF INTEGER.           
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = rfos09(4)                                                
      rvcorr = 0.0                                                      
40270 if (rvcomp + .00005) 20270, 10270, 40271                          
40271 if (rvcomp - .00005) 10270, 10270, 20270                          
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
!      ****  FCVS PROGRAM 311  -  TEST 028  ****                         
!                                                                        
!      OVERRIDE INTEGER DEFAULT TYPING OF A STATEMENT FUNCTION DUMMY     
!      ARGUMENT WITH TYPE-STATEMENT TYPING OF REAL.                      
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = rfos10(4.0)                                              
      rvcorr = 0.8                                                      
40280 if (rvcomp - .79995) 20280, 10280, 40281                          
40281 if (rvcomp - .80005) 10280, 10280, 20280                          
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
!      ****  FCVS PROGRAM 311  -  TEST 029  ****                         
!                                                                        
!      OVERRIDE THE REAL DEFAULT TYPING OF A STATEMENT FUNCTION DUMMY    
!      ARGUMENT WITH TYPE-STATEMENT TYPING OF INTEGER.                   
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = rfos11(4)                                                
      rvcorr = 0.0                                                      
40290 if (rvcomp + .00005) 20290, 10290, 40291                          
40291 if (rvcomp - .00005) 10290, 10290, 20290                          
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
!      ****  FCVS PROGRAM 311  -  TEST 030  ****                         
!                                                                        
!      TEST 030 TESTS A STATEMENT FUNCTION WHERE THE DUMMY ARGUMENT      
!      NAME IS IDENTICAL TO A VARIABLE NAME WITHIN THE PROGRAM.          
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      ivon04 = 10                                                       
      ivcomp = 1                                                        
      if (ifos15(3)  ==  4) ivcomp = ivcomp * 2                         
      if (ivon04  ==  10) ivcomp = ivcomp * 3                           
      ivcorr = 6                                                        
!      6 = 2 * 3                                                         
40300 if (ivcomp - 6) 20300, 10300, 20300                               
30300 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10300, 0311, 20300                                    
10300 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0311                                                        
20300 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0311 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 031  ****                         
!                                                                        
!      TEST 031 TESTS THE ASSIGNMENT OF A STATEMENT FUNCTION TO AN       
!      ARRAY ELEMENT.                                                    
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      ivcomp = 0                                                        
      iadn12(3) = ifos16(4)                                             
      ivcomp = iadn12(3)                                                
      ivcorr = 5                                                        
40310 if (ivcomp - 5) 20310, 10310, 20310                               
30310 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10310, 0321, 20310                                    
10310 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0321                                                        
20310 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0321 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 032  ****                         
!                                                                        
!      TEST 032 TESTS THE USE OF A STATEMENT FUNCTION REFERENCE          
!      IN AN ARITHMETIC EXPRESSION.                                      
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      ivcomp = 0                                                        
      ivon05 = 12                                                       
      ivcomp = ivon05 + ifos17(4) * 2 - 3                               
      ivcorr = 19                                                       
40320 if (ivcomp - 19) 20320, 10320, 20320                              
30320 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10320, 0331, 20320                                    
10320 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0331                                                        
20320 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0331 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 033  ****                         
!                                                                        
!      TEST 033 TESTS THE USE OF A STATEMENT FUNCTION DEFINITION AND     
!      REFERENCE WITHIN AN EXTERNAL FUNCTION.                            
!                                                                        
      ivtnum =  33                                                      
      if (iczero) 30330, 0330, 30330                                    
 0330 continue                                                          
      rvcomp = 0.0                                                      
      rvcomp  = ff313(1.3)
      rvcorr = 5.8                                                      
40330 if (rvcomp - 5.7995) 20330, 10330, 40331                          
40331 if (rvcomp - 5.8005) 10330, 10330, 20330                          
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
!      ****  FCVS PROGRAM 311  -  TEST 034  ****                         
!                                                                        
!      TEST 034 TESTS THE USE OF A STATEMENT FUNCTION DEFINITION AND     
!      REFERENCE WITHIN A SUBROUTINE.                                    
!                                                                        
      ivtnum =  34                                                      
      if (iczero) 30340, 0340, 30340                                    
 0340 continue                                                          
      rvcomp = 0.0                                                      
      rvon05 = 10.0                                                     
      call fs316(rvon05)

      rvcomp = rvon05                                                   
      rvcorr = 5.5                                                      
40340 if (rvcomp - 5.4995) 20340, 10340, 40341                          
40341 if (rvcomp - 5.5005) 10340, 10340, 20340                          
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
!      ****  FCVS PROGRAM 311  -  TEST 035  ****                         
!                                                                        
!      TEST 035 REFERENCES THE DUMMY ARGUMENT NAME OF AN EXTERNAL        
!      FUNCTION WITHIN THE EXPRESSION OF A STATEMENT FUNCTION DEFINED    
!      IN THAT EXTERNAL FUNCTION.                                        
!                                                                        
      ivtnum =  35                                                      
      if (iczero) 30350, 0350, 30350                                    
 0350 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ff314(4)
      ivcorr = 7                                                        
40350 if (ivcomp - 7) 20350, 10350, 20350                               
30350 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10350, 0361, 20350                                    
10350 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0361                                                        
20350 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0361 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 036  ****                         
!                                                                        
!      TEST 036 TESTS A STATEMENT FUNCTION DEFINED WITHIN AN EXTERNAL    
!      FUNCTION IN WHICH THE STATEMENT FUNCTION DUMMY ARGUMENT NAME IS   
!      IDENTICAL TO THE EXTERNAL FUNCTION DUMMY ARGUMENT NAME.           
!                                                                        
      ivtnum =  36                                                      
      if (iczero) 30360, 0360, 30360                                    
 0360 continue                                                          
      rvcomp = 0.0                                                      
      rvcomp  = ff315(5.5)
      rvcorr = 16.7                                                     
40360 if (rvcomp - 16.695) 20360, 10360, 40361                          
40361 if (rvcomp - 16.705) 10360, 10360, 20360                          
30360 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10360, 0371, 20360                                    
10360 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0371                                                        
20360 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0371 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 311  -  TEST 037  ****                         
!                                                                        
!      TEST 037 TESTS THE USAGE OF THE NAME OF A COMMON BLOCK AS THE     
!      SYMBOLIC NAME OF A STATEMENT FUNCTION.                            
!                                                                        
      ivtnum =  37                                                      
      if (iczero) 30370, 0370, 30370                                    
 0370 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ifos19(4)                                                
      ivcorr = 5                                                        
40370 if (ivcomp - 5) 20370, 10370, 20370                               
30370 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10370, 0381, 20370                                    
10370 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0381                                                        
20370 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0381 continue                                                          
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
90001 format (" ",24x,"FM311")                                          
90000 format (" ",20x,"END OF PROGRAM FM311" )                          
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
      end program fm311
      integer function ff312(idonx1)
      integer :: idonx1
      integer :: idonx2
!      THIS SUBPROGRAM IS USED BY TESTS 014 AND 023 OF THE MAIN PROGRAM  
!      FM311 TO TEST STATEMENT FUNCTION.  IN TEST 014 REFERENCE TO FF312 
!      IS USED IN THE EXPRESSION OF A STATEMENT FUNCTION.  IN TEST 023   
!      REFERENCE TO FF312 IS USED AS AN ACTUAL ARGUMENT IN A STATEMENT   
!      FUNCTION REFERENCE.  THIS ROUTINE MERELY INCREMENTS THE VALUE OF  
!      ACTUAL/DUMMY ARGUMENT BY ONE AND RETURN THE RESULT AS THE         
!      FUNCTION VALUE.                                                   
      idonx2 = idonx1 + 1                                               
      ff312 = idonx2                                                    
      return                                                            
      end function ff312
      real function ff313(rdon08)
      real :: rdon08
      real :: rfos12
      real :: rdon09
      real :: rvon04
!      THIS SUBPROGRAM IS USED BY TEST 033 OF THE MAIN PROGRAM FM311 TO  
!      TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  
!      AN EXTERNAL FUNCTION.                                             
      rfos12(rdon09) = rdon09 + 1.0                                     
      rvon04 = rfos12(3.5)                                              
      ff313 = rdon08 + rvon04                                           
      return                                                            
      end function ff313
      integer function ff314(idon19)
      integer :: idon19
      integer :: ifos18
      integer :: idon20
!      THIS SUBPROGRAM IS USED BY TEST 035 OF THE MAIN PROGRAM FM311 TO  
!      TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  
!      AN EXTERNAL FUNCTION.  IN THIS TEST THE EXTERNAL FUNCTION DUMMY   
!      ARGUMENT IS REFERENCED WITHIN THE EXPRESSION OF THE STATEMENT     
!      FUNCTION.                                                         
      ifos18(idon20) = idon19 + idon20                                  
      ff314 = ifos18(3)                                                 
      return                                                            
      end function ff314
      real function ff315(rdon12)
      real :: rdon12
      real :: rfos14
      real :: rvon06
      real :: rvon07
!      THIS SUBPROGRAM IS USED BY TEST 036 OF THE MAIN PROGRAM FM311 TO  
!      TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  
!      AN EXTERNAL FUNCTION.  IN THIS TEST THE EXTERNAL FUNCTION AND     
!      STATEMENT FUNCTION DUMMY ARGUMENTS NAMES ARE IDENTICAL.           
      rfos14(rdon12) = rdon12 + 1.0                                     
      rvon06 = 10.2                                                     
      rvon07 = rfos14(rvon06)                                           
      ff315 = rdon12 + rvon07                                           
      return                                                            
      end function ff315
      subroutine fs316(rdon10)
      real :: rdon10
      real :: rfos13
      real :: rdon11
!      THIS SUBPROGRAM IS USED BY TEST 034 OF THE MAIN PROGRAM FM311 TO  
!      TEST THE DEFINITION AND REFERENCE OF A STATEMENT FUNCTION WITHIN  
!      A SUBROUTINE.                                                     
      rfos13(rdon11) = rdon11 + 1.0                                     
      rdon10 = rfos13(3.5) + 1.0                                        
      return                                                            
      end subroutine fs316
