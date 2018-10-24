      program fm251
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE TESTS THE IMPLICIT STATEMENT FOR DECLARING        
!      VARIABLES AS TYPE LOGICAL.  THE TYPE OF A VARIABLE ( LOGICAL,     
!      INTEGER, OR REAL ) IS SET BY BOTH IMPLICIT STATEMENTS AND ALSO    
!      BY EXPLICIT TYPE STATEMENTS.  TESTS ARE MADE TO CHECK THAT        
!      EXPLICIT TYPE STATEMENTS OVERIDE THE TYPE SET BY AN IMPLICIT      
!      STATEMENT FOR THE VARIABLES LISTED.                               
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1977                                                 
!         SECTION 4.7,    LOGICAL TYPE                                   
!         SECTION 8.4.1,  LOGICAL TYPE STAEMENT                          
!         SECTION 8.5,    IMPLICIT STATEMENT                             
!         SECTION 11.5,   LOGICAL IF STATEMENT                           
!                                                                        
!                                                                        
!         FM016 - TESTS LOGICAL TYPE STATEMENTS WITH VARIOUS FORMS OF    
!                 LOGICAL CONSTANTS AND VARIABLES.                       
!                                                                        
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
      logical  :: mvin01
      logical  :: lcon01
      logical  :: nvin01
      real :: rvcomp
      real :: rvcorr
      logical  :: lcon02
      integer :: avin01
      real :: ivin01
      logical :: evin01
      logical :: fvin01
      logical :: gvin01
      logical :: hvin01
      logical :: ovin01
      logical :: pvin01
      logical :: qvin01
      logical :: svin01
      logical :: tvin01
      logical :: xvin01
      logical :: yvin01
      integer :: vvin01
      integer :: ivcomp
      integer :: ivpass
      integer :: ivcorr
      integer :: ivtnum
      integer :: ivdele
      integer :: ivfail
      integer :: i01
      integer :: i02
      integer :: iczero
      integer :: mvtn01
      real :: nvtn01
      logical :: mvtn02
      logical :: nvtn02
      logical, dimension(1:3,1:3) :: matn21
      logical :: avtn01
      logical :: ivtn01
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
!      ****  FCVS PROGRAM 251  -  TEST 001  ****                         
!                                                                        
!         TEST 001 ASSIGNS A LOGICAL VALUE OF .TRUE. TO MVIN01 WHICH WAS 
!      SPECIFIED AS TYPE LOGICAL IN AN IMPLICIT STATEMENT.               
!                   IMPLICIT LOGICAL (M,N)                               
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      mvin01 = .true.                                                   
      if ( mvin01 )  ivcomp = 1                                         
      ivcorr = 1                                                        
40010 if ( ivcomp - 1 )  20010, 10010, 20010                            
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
!      ****  FCVS PROGRAM 251  -  TEST 002  ****                         
!                                                                        
!         TEST 002 ASSIGNS A LOGICAL VALUE OF .FALSE. TO NVIN01 WHICH    
!      WAS SPECIFIED AS TYPE LOGICAL IN AN IMPLICIT STATEMENT.           
!                   IMPLICIT LOGICAL (M,N)                               
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 1                                                        
      lcon01 = .false.                                                  
      nvin01 = lcon01                                                   
      if ( nvin01 )  ivcomp = 0                                         
      ivcorr = 1                                                        
40020 if ( ivcomp - 1 )  20020, 10020, 20020                            
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
!      ****  FCVS PROGRAM 251  -  TEST 003  ****                         
!                                                                        
!         TEST 003 ASSIGNS AN INTEGER VALUE OF 4 TO MVTN01 WHICH         
!      WAS SPECIFIED AS TYPE INTEGER EXPLICITLY IN A TYPE STATEMENT.     
!                   INTEGER MVTN01                                       
!      THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT INTEGER TYPE        
!      STATEMENT CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD         
!      SET THE TYPE AS LOGICAL.                                          
!                   IMPLICIT LOGICAL (M,N)                               
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      rvcomp = 10.0                                                     
      mvtn01 = 4                                                        
      rvcomp = mvtn01/5                                                 
      rvcorr = 0.0                                                      
40030 if ( rvcomp )  20030, 10030, 20030                                
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
!      ****  FCVS PROGRAM 251  -  TEST 004  ****                         
!                                                                        
!         TEST 004 ASSIGNS A REAL VALUE OF 4.0 TO NVTN01 WHICH           
!      WAS SPECIFIED AS TYPE REAL EXPLICITLY IN A TYPE STATEMENT.        
!                   REAL NVTN01                                          
!      THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT REAL TYPE           
!      STATEMENT CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD         
!      SET THE TYPE AS LOGICAL.                                          
!                   IMPLICIT LOGICAL (M,N)                               
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      rvcomp = 10.0                                                     
      nvtn01 = 4.0                                                      
      rvcomp = nvtn01/5                                                 
      rvcorr = 0.8                                                      
40040 if ( rvcomp - 0.79995 )  20040, 10040, 40041                      
40041 if ( rvcomp - 0.80005 )  10040, 10040, 20040                      
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10040, 0051, 20040                                    
10040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0051                                                        
20040 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 251  -  TEST 005  ****                         
!                                                                        
!         TEST 005 ASSIGNS A LOGICAL VALUE OF .TRUE. TO MVTN02 WHICH WAS 
!      SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT TYPE STATEMENT AFTER ALSO
!      HAVING ITS FIRST LETTER M SPECIFIED AS TYPE LOGICAL IN AN         
!      IMPLICIT STATEMENT.                                               
!                   IMPLICIT LOGICAL (M,N)                               
!                   LOGICAL MVTN02                                       
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 0                                                        
      lcon02 = .true.                                                   
      mvtn02 = lcon02                                                   
      if ( mvtn02 )  ivcomp = 1                                         
      ivcorr = 1                                                        
40050 if ( ivcomp - 1 )  20050, 10050, 20050                            
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
!      ****  FCVS PROGRAM 251  -  TEST 006  ****                         
!                                                                        
!         TEST 006 ASSIGNS A LOGICAL VALUE OF .FALSE. TO NVTN02 WHICH WAS
!      SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT TYPE STATEMENT AFTER ALSO
!      HAVING ITS FIRST LETTER N SPECIFIED AS TYPE LOGICAL IN AN         
!      IMPLICIT STATEMENT.                                               
!                   IMPLICIT LOGICAL (M,N)                               
!                   LOGICAL NVTN02                                       
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 1                                                        
      nvtn02 = .false.                                                  
      if ( nvtn02 )  ivcomp = 0                                         
      ivcorr = 1                                                        
40060 if ( ivcomp - 1 )  20060, 10060, 20060                            
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
!      ****  FCVS PROGRAM 251  -  TEST 007  ****                         
!                                                                        
!         TEST 007 ASSIGNS A LOGICAL VALUE OF .TRUE. TO THE ARRAY ELEMENT
!      MATN21(1,1) WHICH WAS SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT    
!      TYPE STATEMENT AFTER ALSO HAVING ITS FIRST LETTER M SPECIFIED AS  
!      TYPE LOGICAL IN AN IMPLICIT STATEMENT.                            
!                   IMPLICIT LOGICAL (M,N)                               
!                   LOGICAL MATN21(3,3)                                  
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      matn21(1,1) = .true.                                              
      if ( matn21(1,1) )  ivcomp = 1                                    
      ivcorr = 1                                                        
40070 if ( ivcomp - 1 )  20070, 10070, 20070                            
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
!      ****  FCVS PROGRAM 251  -  TEST 008  ****                         
!                                                                        
!         TEST 008 ASSIGNS AN INTEGER VALUE OF 4 TO AVIN01 WHICH WAS     
!      SPECIFIED AS TYPE INTEGER IN AN IMPLICIT STATEMENT.               
!                   IMPLICIT INTEGER (A,B)                               
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      rvcomp = 10.0                                                     
      avin01 = 4                                                        
      rvcomp = avin01/5                                                 
      rvcorr = 0.0                                                      
40080 if ( rvcomp )  20080, 10080, 20080                                
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
!      ****  FCVS PROGRAM 251  -  TEST 009  ****                         
!                                                                        
!         TEST 009 ASSIGNS A LOGICAL VALUE OF .TRUE. TO AVTN01 WHICH WAS 
!      SPECIFIED AS TYPE LOGICAL EXPLICITLY IN A TYPE STATEMENT.         
!                   LOGICAL AVTN01                                       
!      THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT LOGICAL TYPE        
!      STATEMENT CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD         
!      SET THE TYPE AS INTEGER.                                          
!                   IMPLICIT INTEGER (A,B)                               
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      avtn01 = .true.                                                   
      if ( avtn01 )  ivcomp = 1                                         
      ivcorr = 1                                                        
40090 if ( ivcomp - 1 )  20090, 10090, 20090                            
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
!      ****  FCVS PROGRAM 251  -  TEST 010  ****                         
!                                                                        
!         TEST 010 ASSIGNS A REAL VALUE OF 4.0 TO IVIN01 WHICH WAS       
!      SPECIFIED AS REAL IMPLICITLY IN AN IMPLICIT STATEMENT.            
!                   IMPLICIT REAL (I,J)                                  
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      rvcomp = 10.0                                                     
      ivin01 = 4.0                                                      
      rvcomp = ivin01/5                                                 
      rvcorr = 0.8                                                      
40100 if ( rvcomp - 0.79995 ) 20100, 10100, 40101                       
40101 if ( rvcomp - 0.80005 ) 10100, 10100, 20100                       
30100 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10100, 0111, 20100                                    
10100 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0111                                                        
20100 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0111 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 251  -  TEST 011  ****                         
!                                                                        
!         TEST 011 ASSIGNS A LOGICAL VALUE OF .FALSE. TO IVTN01 WHICH WAS
!      SPECIFIED AS TYPE LOGICAL IN AN EXPLICIT TYPE STATEMENT.          
!                   LOGICAL IVTN01                                       
!      THIS TEST IS TO DETERMINE WHETHER AN EXPLICIT TYPE STATEMENT      
!      CAN OVERRIDE THE IMPLICIT STATEMENT WHICH WOULD SET THE TYPE      
!      AS REAL.                                                          
!                   IMPLICIT REAL (I,J)                                  
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 1                                                        
      ivtn01 = .false.                                                  
      if ( ivtn01 )  ivcomp = 0                                         
      ivcorr = 1                                                        
40110 if ( ivcomp - 1 )  20110, 10110, 20110                            
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
!         THE NEXT TWO TESTS CHECK THE RANGE OF LETTERS THAT             
!      ARE SET BY THE IMPLICIT STATEMENT AS FOLLOWS -                    
!      IMPLICIT LOGICAL ( E-H, O, P-Q,  S-T, X-Y ), INTEGER ( U-W )      
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 251  -  TEST 012  ****                         
!                                                                        
!         TEST 012 ASSIGNS A LOGICAL VALUE OF .TRUE. TO A SERIES OF      
!      VARIABLES THAT BEGIN WITH THE FOLLOWING LETTERS -                 
!                                                                        
!         E  F  G  H  O  P  Q  S  T  X  Y                                
!                                                                        
!      VARIABLES THAT BEGIN WITH THESE LETTERS SHOULD BE IMPLICITLY TYPED
!      LOGICAL BECAUSE OF THE IMPLICIT STATEMENT USING BOTH THE RANGE AND
!      SINGLE LETTER SPECIFICATION FOR TYPE LOGICAL.  THE VARIABLE XVIN01
!      IS FIRST USED IN A LOGICAL IF STATEMENT.  THE TRUE BRANCH SHOULD  
!      BE TAKEN TO SET IVCOMP = 1.  THEN EACH OF THE VARIABLES SET TO    
!      .TRUE. ARE USED IN A SECOND LOGICAL IF STATEMENT WHICH IS ONE     
!      LARGE LOGICAL CONJUNCTION ( VARIABLE .AND. VARIABLE .AND. ... ).  
!      THE TRUE BRANCH SHOULD BE TAKEN TO INCREMENT THE VALUE OF IVCOMP  
!      TO A FINAL VALUE OF THREE (3).                                    
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 3                                                        
      evin01 = .true.                                                   
      fvin01 = .true.                                                   
      gvin01 = .true.                                                   
      hvin01 = .true.                                                   
      ovin01 = .true.                                                   
      pvin01 = .true.                                                   
      qvin01 = .true.                                                   
      svin01 = .true.                                                   
      tvin01 = .true.                                                   
      xvin01 = .true.                                                   
      yvin01 = .true.                                                   
      if ( xvin01 )  ivcomp = 1                                         
      if ( evin01 .and. fvin01 .and. gvin01 .and. hvin01 .and. ovin01   .and. pvin01 .and. qvin01 .and. svin01 .and. tvin01 .and. xvin01  .and. yvin01 )  ivcomp = ivcomp + 2                               
40120 if ( ivcomp - 3 ) 20120, 10120, 20120                             
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
!      ****  FCVS PROGRAM 251  -  TEST 013  ****                         
!                                                                        
!         TEST 013 ASSIGNS AN INTEGER VALUE OF 4 TO VVIN01 WHICH         
!      WAS SPECIFIED AS TYPE INTEGER IMPLICITLY USING THE RANGE OF       
!      LETTERS  U-W  IN THE IMPLICIT INTEGER SPECIFICATION STATEMENT.    
!      DIVISION IS USED TO DETERMINE WHETHER VVIN01 IS TYPE INTEGER.     
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvcomp = 10.0                                                     
      vvin01 = 4                                                        
      rvcomp = vvin01/5                                                 
      rvcorr = 0.0                                                      
40130 if ( rvcomp )  20130, 10130, 20130                                
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
90001 format (" ",24x,"FM251")                                          
90000 format (" ",20x,"END OF PROGRAM FM251" )                          
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
      end program fm251
