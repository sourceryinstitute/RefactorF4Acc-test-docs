      program fm306
!                                                                        
!                                                                        
!           THIS ROUTINE TESTS THE USE OF THE SUBSET LEVEL FEATURES OF   
!      THE IMPLICIT SPECIFICATION STATEMENT.  THE DEFAULT IMPLIED INTEGER
!      AND REAL TYPING IS EITHER CONFIRMED OR OVERRIDDEN TO SPECIFY      
!      INTEGER, REAL AND LOGICAL TYPING.  ALL 26 ALPHABETIC LETTERS ARE  
!      USED TO INDICATE THE IMPLICIT TYPING.  VARIABLE AND ARRAY         
!      ENTITIES ARE USED TO TEST THE ACTUAL TYPING.  THE SUBSET LEVEL    
!      FEATURES OF THE IMPLICIT STATEMENT ARE ALSO TESTED IN ROUTINES    
!      FM201 AND FM251.                                                  
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS.  
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
      integer  :: i01
      integer  :: i02
      integer  :: ivpass
      integer  :: ivfail
      integer  :: ivdele
      integer  :: iczero
      integer  :: ivtnum
      real  :: rvcomp
      integer  :: ivin01
      real  :: rvcorr
      real  :: rvin01
      real  :: jvin01
      logical  :: lvin01
      integer  :: ivcorr
      integer  :: ivcomp
      logical  :: bvin01
      integer  :: dvin01
      integer  :: evin01
      integer  :: fvin01
      real  :: rvcmp1
      real  :: rvcmp2
      real  :: rvcmp3
      real  :: gvin01
      integer  :: kvin01
      integer  :: ovin01
      integer  :: pvin01
      integer  :: qvin01
      real  :: rvcmp4
      real :: svin01
      integer :: tvin01
      integer :: uvin01
      integer :: vvin01
      integer :: wvin01
      real :: xvin01
      logical :: yvin01
      integer :: zvin01
      logical  :: lvcomp
      real :: mvin01
      real :: nvin01
      integer, dimension(1:5) :: aain11
      real, dimension(1:5) :: hain11
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
!      ****  FCVS PROGRAM 306  -  TEST 001  ****                         
!                                                                        
!      TEST 001 IS DESIGNED TO CONFIRM IMPLICIT INTEGER TYPING.          
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      rvcomp = 10.0                                                     
      ivin01 = 4                                                        
      rvcomp = ivin01 / 5                                               
      rvcorr = 0.0                                                      
40010 if (rvcomp) 20010, 10010, 20010                                   
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
!      ****  FCVS PROGRAM 306  -  TEST 002  ****                         
!                                                                        
!      TEST 002 IS DESIGNED TO CONFIRM IMPLICIT REAL TYPING.             
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvcomp = 10.0                                                     
      rvin01 = 4                                                        
      rvcomp = rvin01/5                                                 
      rvcorr = .8                                                       
40020 if (rvcomp - .79995) 20020, 10020, 40021                          
40021 if (rvcomp - .80005) 10020, 10020, 20020                          
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
!      ****  FCVS PROGRAM 306  -  TEST 003  ****                         
!                                                                        
!      TEST 003 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF       
!      INTEGER WITH IMPLICIT TYPING OF REAL.                             
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      rvcomp = 10.0                                                     
      jvin01 = 4                                                        
      rvcomp = jvin01/5                                                 
      rvcorr = .8                                                       
40030 if (rvcomp - .79995) 20030, 10030, 40031                          
40031 if (rvcomp - .80005) 10030, 10030, 20030                          
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
!      ****  FCVS PROGRAM 306  -  TEST 004  ****                         
!                                                                        
!      TEST 004 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF       
!      INTEGER WITH IMPLICIT TYPING OF LOGICAL.                          
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      lvin01 = .true.                                                   
      ivcorr = 1                                                        
      ivcomp = 0                                                        
      if (lvin01) ivcomp = 1                                            
40040 if (ivcomp - 1) 20040, 10040, 20040                               
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
!      ****  FCVS PROGRAM 306  -  TEST 005  ****                         
!                                                                        
!      TEST 005 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF       
!      REAL WITH IMPLICIT TYPING OF INTEGER.                             
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      rvcomp = 10.0                                                     
      aain11(2) = 4                                                     
      rvcomp = aain11(2)/5                                              
      rvcorr = 0.0                                                      
40050 if (rvcomp) 20050, 10050, 20050                                   
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
!      ****  FCVS PROGRAM 306  -  TEST 006  ****                         
!                                                                        
!      TEST 006 IS DESIGNED TO OVERRIDE IMPLICIT DEFAULT TYPING OF REAL  
!      WITH IMPLICIT TYPING OF LOGICAL.                                  
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      bvin01 = .true.                                                   
      ivcorr = 1                                                        
      ivcomp = 0                                                        
      if (bvin01) ivcomp = 1                                            
40060 if (ivcomp - 1) 20060, 10060, 20060                               
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
!      TESTS 007 THROUGH 012 ARE DESIGNED TO TEST VARIOUS SYNTACTICAL    
!      CONSTRUCTS OF THE IMPLICIT STATEMENT.                             
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 306  -  TEST 007  ****                         
!                                                                        
!      TEST 007 TESTS THE SPECIFYING OF MORE THAN ONE ALPHABETIC         
!      CHARACTER IN AN IMPLICIT STATEMENT.                               
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      dvin01 = 4                                                        
      evin01 = 4                                                        
      fvin01 = 4                                                        
      rvcmp1 = dvin01/5                                                 
      rvcmp2 = evin01/5                                                 
      rvcmp3 = fvin01/5                                                 
      ivcomp = 1                                                        
      if (rvcmp1  ==  0.0) ivcomp = ivcomp * 2                          
      if (rvcmp2  ==  0.0) ivcomp = ivcomp * 3                          
      if (rvcmp3  ==  0.0) ivcomp = ivcomp * 5                          
      ivcorr = 30                                                       
!      30 = 2 * 3 * 5                                                    
40070 if (ivcomp -    30) 20070, 10070, 20070                           
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
!      ****  FCVS PROGRAM 306  -  TEST 008  ****                         
!                                                                        
!      TEST 008 TESTS THE SPECIFYING A RANGE OF SINGLE LETTERS IN        
!      ALPHABETIC ORDER IN AN IMPLICIT STATEMENT.                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      gvin01 = 4                                                        
      hain11(4) = 4                                                     
      rvcmp1 = gvin01/5                                                 
      rvcmp2 = hain11(4)/5                                              
      ivcomp = 1                                                        
      if (rvcmp1  >=  .79995 .and. rvcmp1  <=  .80005) ivcomp=ivcomp*2  
      if (rvcmp2  >=  .79995 .and. rvcmp2  <=  .80005) ivcomp=ivcomp*3  
      ivcorr = 6                                                        
!      6 = 2 * 3                                                         
40080 if (ivcomp - 6) 20080, 10080, 20080                               
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
!      ****  FCVS PROGRAM 306  -  TEST 009  ****                         
!                                                                        
!      TEST 009 TESTS THE SPECIFYING A SINGLE LETTER AND A RANGE OF      
!      SINGLE LETTERS IN ALPHABETIC ORDER IN AN IMPLICIT STATEMENT.      
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      kvin01 = 4                                                        
      ovin01 = 4                                                        
      pvin01 = 4                                                        
      qvin01 = 4                                                        
      rvcmp1 = kvin01/5                                                 
      rvcmp2 = ovin01/5                                                 
      rvcmp3 = pvin01/5                                                 
      rvcmp4 = qvin01/5                                                 
      ivcomp = 1                                                        
      if (rvcmp1  ==  0.0) ivcomp = ivcomp * 2                          
      if (rvcmp2  ==  0.0) ivcomp = ivcomp * 3                          
      if (rvcmp3  ==  0.0) ivcomp = ivcomp * 5                          
      if (rvcmp4  ==  0.0) ivcomp = ivcomp * 7                          
      ivcorr = 210                                                      
!      210 = 2 * 3 * 5 * 7                                               
40090 if (ivcomp - 210) 20090, 10090, 20090                             
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
!      ****  FCVS PROGRAM 306  -  TEST 010  ****                         
!                                                                        
!      TEST 010 TESTS THE SPECIFYING OF MORE THAN ONE TYPING IN ONE      
!      IMPLICIT STATEMENT.                                               
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      svin01 = 4                                                        
      tvin01 = 4                                                        
      uvin01 = 4                                                        
      vvin01 = 4                                                        
      rvcmp1 = svin01/5                                                 
      rvcmp2 = tvin01/5                                                 
      rvcmp3 = uvin01/5                                                 
      rvcmp4 = vvin01/5                                                 
      ivcomp = 1                                                        
      if (rvcmp1  >=  .79995 .and. rvcmp1  <=  .80005) ivcomp=ivcomp*2  
      if (rvcmp2  ==  0.0) ivcomp = ivcomp * 3                          
      if (rvcmp3  ==  0.0) ivcomp = ivcomp * 5                          
      if (rvcmp4  ==  0.0) ivcomp = ivcomp * 7                          
      ivcorr = 210                                                      
!      210 = 2 * 3 * 5 * 7                                               
      if (ivcomp - 210) 20100, 10100, 20100                             
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
!      ****  FCVS PROGRAM 306  -  TEST 011  ****                         
!                                                                        
!      TEST 011 TESTS THE SPECIFYING OF INTEGER, REAL, AND LOGICAL       
!      TYPING IN ONE IMPLICIT STATEMENT.  IN THIS TEST INTEGER TYPING    
!      IS REPEATED A SECOND TIME.                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      wvin01 = 4                                                        
      xvin01 = 4                                                        
      yvin01 = .true.                                                   
      zvin01 = 4                                                        
      rvcmp1 = wvin01/5                                                 
      rvcmp2 = xvin01/5                                                 
      lvcomp = yvin01                                                   
      rvcmp3 = zvin01/5                                                 
      ivcomp = 1                                                        
      if (rvcmp1  ==  0.0) ivcomp = ivcomp * 2                          
      if (rvcmp2  >=  .79995 .and. rvcmp2  <=  .80005) ivcomp=ivcomp*3  
      if (lvcomp) ivcomp = ivcomp * 5                                   
      if (rvcmp3  ==  0.0) ivcomp = ivcomp * 7                          
      ivcorr = 210                                                      
!      210 = 2 * 3 * 5 * 7                                               
40110 if (ivcomp - 210) 20110, 10110, 20110                             
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
!      ****  FCVS PROGRAM 306  -  TEST 012  ****                         
!                                                                        
!      TEST 012 TESTS THE SPECIFYING OF REAL TYPING TWICE IN ONE         
!      IMPLICIT STATEMENT.                                               
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      mvin01 = 4                                                        
      nvin01 = 4                                                        
      rvcmp1 = mvin01/5                                                 
      rvcmp2 = nvin01/5                                                 
      ivcomp = 1                                                        
      if (rvcmp1  >=  .79995 .and. rvcmp1  <=  .80005) ivcomp=ivcomp*2  
      if (rvcmp2  >=  .79995 .and. rvcmp2  <=  .80005) ivcomp=ivcomp*3  
      ivcorr = 6                                                        
!      6 = 2 * 3                                                         
      if (ivcomp - 6) 20120, 10120, 20120                               
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
90001 format (" ",24x,"FM306")                                          
90000 format (" ",20x,"END OF PROGRAM FM306" )                          
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
      end program fm306
