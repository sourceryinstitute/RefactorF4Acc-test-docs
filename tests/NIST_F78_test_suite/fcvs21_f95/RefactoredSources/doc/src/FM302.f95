      program fm302
!                                                                        
!                                                                        
!         THIS ROUTINE TESTS THE SUBSET LEVEL FEATURES OF THE COMMON     
!      SPECIFICATION STATEMENT.  INTEGER, REAL AND LOGICAL VARIABLES AND 
!      ARRAYS ARE PASSED BACK-AND-FORTH BETWEEN THE MAIN PROGRAM,EXTERNAL
!      FUNCTIONS AND SUBROUTINES.  BOTH NAMED AND UNNAMED (BLANK) COMMON 
!      ARE TESTED.  SPECIFIC TESTS ARE INCLUDED FOR RENAMING ENTITIES IN 
!      COMMON BETWEEN PROGRAM UNITS, THE PASSING OF DATA THROUGH COMMON  
!      BY EQUIVALENCE ASSOCIATION, AND THE SPECIFYING OF BLANK COMMON OF 
!      DIFFERENT LENGTHS IN DIFFERENT PROGRAM UNITS.  THE SUBSET LEVEL   
!      FEATURES OF THE COMMON STATEMENT ARE ALSO TESTED IN FM022 THROUGH 
!      FM025, FM050 AND FM056.                                           
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 8.2,    EQUIVALENCE STATEMENT                          
!         SECTION 8.3,    COMMON STATEMENT                               
!         SECTION 15.5,   EXTERNAL FUNCTIONS                             
!         SECTION 15.6,   SUBROUTINES                                    
!         SECTION 15.9.4, COMMON BLOCKS                                  
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
!           *** SPECIFICATION STATEMENT FOR TEST 001 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 002 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 003 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 004 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 005 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 006 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 007 ***                 
!                                                                        
      integer, dimension(1:4) :: iacn11
      integer, dimension(1:5) :: iacn1g
      integer, dimension(1:2,1:3) :: iacn21
      integer :: ivce09
      integer :: ivceh1
      integer :: ivcei1
      integer :: ivcn01
      integer :: ivcn02
      integer :: ivcn03
      integer :: ivcn04
      integer :: ivcn05
      integer :: ivcn07
      integer :: ivcn08
      integer :: ivcn12
      integer :: ivcna1
      integer :: ivcnb1
      integer :: ivcnb2
      integer :: ivcnc1
      integer :: ivcnd1
      integer :: ivcnd2
      integer :: ivcnf1
      integer :: ivcnf2
      integer :: ivcnf3
      integer :: ivcng1
      integer :: ivcnh1
      integer :: ivcnj1
      logical  :: lvcn01
      logical  :: lvcnc1
      logical  :: lvcnd1
      real :: rvcn01
      real :: rvcnb1
      real :: rvcnd1
      real :: rvcne1
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivceh2
      integer :: ivcn06
      integer :: ivce10
      integer :: ivon99
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      real, dimension(1:10) :: racn11
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 008 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 009 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 010 ***                 
!                                                                        
      integer, dimension(1:3) :: iacn1f
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 011 ***                 
!                                                                        
      equivalence (ivceh1,ivceh2)                                       
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 012                     
      equivalence (ivce09,ivce10)                                       
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 013                     
!                                                                        
      equivalence (ivcei1,iace1i)                                       
      integer, dimension(1:3) :: iace1i
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 014 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 015 ***                 
!                                                                        
!                                                                        
!           *** SPECIFICATION STATEMENT FOR TEST 016 ***                 
!                                                                        
      character(len=2) :: cvtn01
      character(len=3) :: cvtn02
      character(len=5), dimension(1:3) :: catn11
      integer :: ff304
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
!           THE FOLLOWING ASSIGNMENT STATEMENTS INITIALIZE THE DATA      
!      ENTITIES BEING PASSED THROUGH COMMON TO SUBROUTINE FS303.  ONLY   
!      ONE REFERENCE TO THIS SUBPROGRAM IS MADE FROM THIS PROGRAM.  THE  
!      CONTENTS OF THE DATA ENTITIES BEING RETURNED THROUGH COMMON ARE   
!      THEN CHECKED IN THIS PROGRAM.                                     
!                                                                        
!                                                                        
      ivcn01 = 3                                                        
      ivcn02 = 2                                                        
      lvcn01 = .false.                                                  
      ivcna1 = 25                                                       
      ivcnb1 = 3                                                        
      rvcnb1 = 4.0                                                      
      ivcnb2 = 5                                                        
      lvcnc1 = .true.                                                   
      ivcnc1 = 13                                                       
      racn11(1) = 1.                                                    
      racn11(10) = 10.0                                                 
      iacn21(1,1) = 11                                                  
      iacn21(2,3) = 23                                                  
      ivcnf1 = 41                                                       
      ivcnf3 = 43                                                       
      iacn1f(1) = 141                                                   
      iacn1f(2) = 142                                                   
      ivceh1 = 1                                                        
      ivceh2 = 5                                                        
      cvtn01 = 'AB'                                                     
      cvtn02 = 'CDE'                                                    
      catn11(1) = 'FGHIJ'                                               
      catn11(2) = 'KLMNO'                                               
      catn11(3) = 'PQRST'                                               
      call fs303(catn11,cvtn01,cvtn02,iacn11,iacn1g,iacn21,ivcn01,ivcn02,ivcn03,ivcn04,ivcn05,ivcna1,ivcnb1,ivcnb2,ivcnc1,ivcng1,ivcnh1,lvcn01,lvcnc1,racn11,rvcn01,rvcnb1)

!                                                                        
!           THE FOLLOWING ASSIGNMENT STATEMENTS INITIALIZE THE DATA      
!      ENTITIES BEING PASSED THROUGH COMMON TO EXTERNAL FUNCTION FF304.  
!      ONLY ONE REFERENCE TO THIS SUBPROGRAM IS MADE FROM THIS PROGRAM.  
!      THE CONTENTS OF THE DATA ENTITIES BEING RETURNED THROUGH COMMON   
!      ARE THEN CHECKED IN THIS PROGRAM.                                 
!                                                                        
      rvcn01 = 6.4                                                      
      ivcn03 = 11                                                       
      ivcn03 = ivcn03*2                                                 
      ivcn04 = 16                                                       
      ivcn05 = 16                                                       
      iacn11(1) = 1                                                     
      iacn11(2) = 2                                                     
      iacn11(3) = 3                                                     
      iacn11(4) = 4                                                     
      ivcnd1 = +33                                                      
      ivcnd2 = 10                                                       
      ivcn06 = 6                                                        
      ivcn07 = 7                                                        
      ivcn08 = 8                                                        
      rvcnd1 = 1.3                                                      
      lvcnd1 = .false.                                                  
      rvcne1 = +3.5                                                     
      ivce09 = 9                                                        
      ivce10 = 10                                                       
      ivcei1 = 5                                                        
      iace1i(1) = 10                                                    
      iace1i(2) = 15                                                    
      iace1i(3) = 20                                                    
      ivcnj1 = 1                                                        
      ivon99  = ff304()
!                                                                        
!           TESTS 001 THROUGH 009 ARE DESIGNED TO TEST VARIOUS           
!      SYNTACTICAL CONSTRUCTS OF THE COMMON STATEMENT USING NAMED AND    
!      UNNAMED (BLANK) COMMON IN THE MAIN PROGRAM, A SUBROUTINE AND AN   
!      EXTERNAL FUNCTION.  DATA ENTITIES CONSIST OF INTEGER, REAL AND    
!      LOGICAL VARIABLES AND INTEGER AND REAL ARRAYS.                    
!                                                                        
!      ****  FCVS PROGRAM 302  -  TEST 001  ****                         
!                                                                        
!           TESTS 001 AND 002 TEST THE USE OF UNNAMED COMMON IN A MAIN   
!      PROGRAM AND A SUBROUTINE.                                         
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivcn01                                                   
      ivcorr = 4                                                        
40010 if (ivcomp - 4) 20010, 10010, 20010                               
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
!      ****  FCVS PROGRAM 302  -  TEST 002  ****                         
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 1                                                        
      if (ivcn02  ==  7) ivcomp = ivcomp * 2                            
      if (lvcn01) ivcomp = ivcomp * 3                                   
      ivcorr = 6                                                        
!           6 = 2 * 3                                                    
40020 if (ivcomp - 6) 20020, 10020, 20020                               
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
!      ****  FCVS PROGRAM 302  -  TEST 003  ****                         
!                                                                        
!           TESTS 003 AND 004 TEST THE USE OF UNNAMED COMMON IN A MAIN   
!      PROGRAM AND AN EXTERNAL FUNCTION.                                 
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 1                                                        
      if (rvcn01  >=  4.1995 .and. rvcn01  <=  4.2005) ivcomp=ivcomp*2  
      if (ivcn03  ==   23) ivcomp = ivcomp * 3                          
      ivcorr = 6                                                        
!           6 = 2 * 3                                                    
40030 if (ivcomp - 6) 20030, 10030, 20030                               
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
!      ****  FCVS PROGRAM 302  -  TEST 004  ****                         
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 1                                                        
      if (ivcn04  ==  8) ivcomp = ivcomp * 2                            
      if (ivcn05  ==  16) ivcomp = ivcomp * 3                           
      if (iacn11(1)  ==  5) ivcomp = ivcomp * 5                         
      if (iacn11(2)  ==  5) ivcomp = ivcomp * 7                         
      if (iacn11(3)  ==  5) ivcomp = ivcomp * 11                        
      if (iacn11(4)  ==  5) ivcomp = ivcomp * 13                        
      ivcorr = 30030                                                    
!      30030  = 2 * 3 * 5 * 7 * 11 * 13                                  
40040 if (ivcomp - 30030) 20040, 10040, 20040                           
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
!      ****  FCVS PROGRAM 302  -  TEST 005  ****                         
!                                                                        
!           TESTS 005 THROUGH 007 TEST THE USE OF NAMED COMMON BLOCKS    
!      IN A MAIN PROGRAM AND A SUBROUTINE.                               
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivcna1                                                   
      ivcorr = 5                                                        
40050 if (ivcomp - 5) 20050, 10050, 20050                               
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
!      ****  FCVS PROGRAM 302  -  TEST 006  ****                         
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 1                                                        
      if (ivcnb1  ==  8) ivcomp = ivcomp * 2                            
      if (rvcnb1  >=  3.4995 .and. rvcnb1  <=  3.5005) ivcomp=ivcomp*3  
      if (ivcnb2  ==  5) ivcomp = ivcomp * 5                            
      ivcorr = 30                                                       
!          30 = 2 * 3 * 5                                                
40060 if (ivcomp - 30) 20060, 10060, 20060                              
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
!      ****  FCVS PROGRAM 302  -  TEST 007  ****                         
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvcnc1) ivcomp = ivcomp * 2                             
      if (ivcnc1  ==  12) ivcomp = ivcomp * 3                           
      if (racn11(1) >= 110.95 .and. racn11(1) <= 111.05) ivcomp=ivcomp*5
      if (racn11(10) >= 109.95.and.racn11(10) <= 110.05)ivcomp=ivcomp*7 
      if (iacn21(1,1)  ==  12) ivcomp = ivcomp * 11                     
      if (iacn21 (2,3)  ==  24) ivcomp = ivcomp * 13                    
      ivcorr = 30030                                                    
!      30030  = 2* 3 * 5 * 7 * 11 * 13                                   
40070 if (ivcomp - 30030) 20070, 10070, 20070                           
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
!      ****  FCVS PROGRAM 302  -  TEST 008  ****                         
!                                                                        
!           TESTS 008 AND 009 TEST THE USE OF NAMED COMMON BLOCKS IN A   
!      MAIN PROGRAM AND AN EXTERNAL FUNCTION.                            
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 1                                                        
      if (ivcnd1  ==  34) ivcomp = ivcomp * 2                           
      if (ivcnd2  ==  11) ivcomp = ivcomp * 3                           
      ivcorr = 6                                                        
!           6 = 2 * 3                                                    
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
!      ****  FCVS PROGRAM 302  -  TEST 009  ****                         
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 1                                                        
      if (ivcn06  ==  7) ivcomp = ivcomp * 2                            
      if (rvcnd1  >=  4.4995 .and. rvcnd1  <=  4.5005) ivcomp = ivcomp*3
      if (lvcnd1) ivcomp = ivcomp * 5                                   
      if (ivcn07  ==  -7) ivcomp = ivcomp * 7                           
      if (ivcn08  ==  -3) ivcomp = ivcomp * 11                          
      if (rvcne1 >= -6.7005.and.rvcne1 <= -6.6995) ivcomp=ivcomp*13     
      ivcorr = 30030                                                    
!      30030  = 2 * 3 * 5 * 7 * 11 * 13                                  
40090 if (ivcomp - 30030) 20090, 10090, 20090                           
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
!      ****  FCVS PROGRAM 302  -  TEST 010  ****                         
!                                                                        
!           TEST 010 IS DESIGNED TO TEST THE ABILITY TO RENAME ENTITIES  
!      IN NAMED COMMON BETWEEN A MAIN PROGRAM AND A SUBROUTINE.          
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 1                                                        
      if (ivcnf1  ==  42) ivcomp = ivcomp * 2                           
      if (ivcnf2  ==  43) ivcomp = ivcomp * 3                           
      if (ivcnf3  ==  44) ivcomp = ivcomp * 5                           
      if (iacn1f(1)  ==  142) ivcomp = ivcomp * 7                       
      if (iacn1f(2)  ==  143) ivcomp = ivcomp * 11                      
      if (iacn1f(3)  ==  144) ivcomp = ivcomp * 13                      
      ivcorr = 30030                                                    
!      30030 = 2 * 3 * 5 * 7 * 11 * 13                                   
40100 if (ivcomp - 30030) 20100, 10100, 20100                           
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
!      ****  FCVS PROGRAM 302  -  TEST 011  ****                         
!                                                                        
!           TEST 011 IS DESIGNED TO TEST THE STORAGE OF A VARIABLE  IN   
!      NAMED COMMON BY EQUIVALENCE ASSOCIATION.                          
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivceh2                                                   
      ivcorr = 6                                                        
40110 if (ivcomp - 6) 20110, 10110, 20110                               
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
!      ****  FCVS PROGRAM 302  -  TEST 012  ****                         
!                                                                        
!           TEST 012 IS DESIGNED TO TEST THE STORAGE OF A VARIABLE IN    
!      UNNAMED COMMON BY EQUIVALENCE ASSOCIATION.                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 1                                                        
      if (ivce09  ==  100) ivcomp = ivcomp * 2                          
      if (ivce10  ==  100) ivcomp = ivcomp * 3                          
      ivcorr = 6                                                        
!      6 = 2 * 3                                                         
40120 if (ivcomp - 6) 20120, 10120, 20120                               
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
!      ****  FCVS PROGRAM 302  -  TEST 013  ****                         
!                                                                        
!           TEST 013 IS DESIGNED TO TEST THE EXTENSION OF NAMED COMMON   
!      BLOCK STORAGE BY EQUIVALENCE ASSOCIATION OF A VARIABLE AND AN     
!      ARRAY.                                                            
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 1                                                        
      if (ivcei1  ==  11) ivcomp = ivcomp * 2                           
      if (iace1i(1)  ==  11) ivcomp = ivcomp * 3                        
      if (iace1i(2)  ==  16) ivcomp = ivcomp * 5                        
      if (iace1i(3)  ==  21) ivcomp = ivcomp * 7                        
      ivcorr = 210                                                      
!      210 = 2 * 3 * 5 * 7                                               
40130 if (ivcomp - 210) 20130, 10130, 20130                             
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
!      ****  FCVS PROGRAM 302  -  TEST 014  ****                         
!                                                                        
!           TEST 014 IS DESIGNED TO TEST THE ABILITY OF PASSING DATA     
!      THROUGH UNNAMED COMMON FROM EXTERNAL FUNCTIONS WHICH HAVE MORE    
!      ENTITIES IN UNNAMED COMMON THAN THE MAIN PROGRAM.                 
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivcn12                                                   
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
!      ****  FCVS PROGRAM 302  -  TEST 015  ****                         
!                                                                        
!           TEST 015 IS DESIGNED TO TEST THE ABILITY OF PASSING DATA     
!      THROUGH NAMED COMMON BETWEEN EXTERNAL FUNCTIONS WHERE THE NAMED   
!      COMMON BLOCK IS NOT SPECIFIED IN THE MAIN PROGRAM.                
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivcnj1                                                   
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
!      ****  FCVS PROGRAM 302  -  TEST 016  ****                         
!                                                                        
!           TEST 016 IS DESIGNED TO TEST THE PASSING OF CHARACTER DATA   
!      IN NAMED COMMON BETWEEN THE MAIN PROGRAM AND A SUBROUTINE.        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 1                                                        
      if (cvtn01  ==  'YZ') ivcomp = ivcomp * 2                         
      if (cvtn02  ==  'UVW') ivcomp = ivcomp * 3                        
      if (catn11(1)  ==  'VWXYZ') ivcomp = ivcomp * 5                   
      if (catn11(2)  ==  'KLMNO') ivcomp = ivcomp * 7                   
      if (catn11(3)  ==  'ABCDE') ivcomp = ivcomp * 11                  
      ivcorr = 2310                                                     
!      2310 = 2 * 3 * 5 * 7 * 11                                         
40160 if (ivcomp - 2310) 20160, 10160, 20160                            
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
90001 format (" ",24x,"FM302")                                          
90000 format (" ",20x,"END OF PROGRAM FM302" )                          
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
      end program fm302
      subroutine fs303(catn11,cvtn01,cvtn02,iacn11,iacn1g,iacn21,ivcn01,ivcn02,ivcn03,ivcn04,ivcn05,ivcna1,ivcnb1,ivcnb2,ivcnc1,ivcng1,ivcnh1,lvcn01,lvcnc1,racn11,rvcn01,rvcnb1)
!                                                                        
!         FS303 IS A SUBROUTINE WHICH IS CALLED ONCE FROM PROGRAM FM302. 
!      IT IS USED TO MODIFY VARIABLES AND ARRAY PASSED THROUGH NAMED AND 
!      UNNAMED COMMON FROM FM302.  AFTER THE DATA ENTITIES ARE MODIFIED  
!      CONTROL IS RETURNED TO FM302 WHERE EACH ENTITY IS TESTED.         
!                                                                        
      integer, dimension(1:4) :: iacn11
      integer, dimension(1:5), intent(InOut) :: iacn1g
      integer, dimension(1:2,1:3), intent(InOut) :: iacn21
      integer, intent(InOut) :: ivcn01
      integer, intent(InOut) :: ivcn02
      integer :: ivcn03
      integer :: ivcn04
      integer :: ivcn05
      integer, intent(InOut) :: ivcna1
      integer, intent(InOut) :: ivcnb1
      integer, intent(In) :: ivcnb2
      integer, intent(InOut) :: ivcnc1
      integer, intent(Out) :: ivcng1
      integer, intent(InOut) :: ivcnh1
      logical , intent(InOut) :: lvcn01
      logical , intent(Out) :: lvcnc1
      real :: rvcn01
      real, intent(Out) :: rvcnb1
      real, dimension(1:10), intent(Out) :: racn11
      character(len=2), intent(Out) :: cvtn01
      character(len=3), intent(Out) :: cvtn02
      character(len=5), dimension(1:3), intent(Out) :: catn11
!      TEST 001                                                          
           ivcn01 = ivcn01 + 1                                          
!      TEST 002                                                          
           ivcn02 = ivcn02 + 5                                          
           lvcn01 = .not. lvcn01                                        
!      TEST 005                                                          
           ivcna1 = ivcna1 / 5                                          
!      TEST 006                                                          
           ivcnb1 = ivcnb1 + ivcnb2                                     
           rvcnb1 = 3.5                                                 
!      TEST 007                                                          
           lvcnc1 = .false.                                             
           ivcnc1 = ivcnc1 - 1                                          
           racn11(1) = 111.                                             
           racn11(10) = 110.                                            
           iacn21(1,1) = iacn21(1,1) + 1                                
           iacn21(2,3) = iacn21(2,3) + 1                                
!      TEST 010                                                          
           iacn1g(1) = iacn1g(1) + 1                                    
           iacn1g(2) = 43                                               
           iacn1g(3) = iacn1g(3) + 1                                    
           iacn1g(4) = iacn1g(4) + 1                                    
           iacn1g(5) = iacn1g(5) + 1                                    
           ivcng1 = 144                                                 
!      TEST 011                                                          
           ivcnh1 = ivcnh1 + 1                                          
!      TEST 017                                                          
           cvtn01 = 'YZ'                                                
           cvtn02 = 'UVW'                                               
           catn11(1) = 'VWXYZ'                                          
           catn11(3) = 'ABCDE'                                          
      return                                                            
      end subroutine fs303
      integer function ff304()
!                                                                        
!           FF304 IS AN EXTERNAL FUNCTION WHICH IS REFERENCED ONCE FROM  
!      PROGRAM FM302.  IT IS USED TO MODIFY VARIABLES AND ARRAYS PASSED  
!      THROUGH NAMED AND UNNAMED COMMON FROM FM302.  AFTER THE DATA      
!      ENTITIES ARE MODIFIED CONTROL IS RETURNED TO FM302 WHERE EACH     
!      ENTITY IS TESTED.  A FUNCTION VALUE OF 999 IS RETURNED BUT IT IS  
!      NOT SIGNIFICANT NOR IS IT TESTED BY FM302.                        
!                                                                        
      integer :: ivon99
      integer :: ivcn01
      integer :: ivcn02
      logical  :: lvcn01
      real :: rvcn01
      integer :: ivcn03
      integer :: ivcn04
      integer :: ivcn05
      integer :: ivcnd1
      integer :: ivcnd2
      integer :: ivcn06
      real :: rvcnd1
      logical  :: lvcnd1
      integer :: ivcn07
      integer :: ivcn08
      real :: rvcne1
      integer :: ivcn10
      integer :: ivcni1
      integer :: ivcni2
      integer :: ivcni3
      integer :: ivcn12
      integer :: ivcn13
      integer :: ivcnj1
      integer :: ivcnk1
      integer, dimension(1:4) :: iacn11
      integer :: ff305
!      TEST 003                                                          
           rvcn01 = 4.2                                                 
           ivcn03 = ivcn03 + 1                                          
!      TEST 004                                                          
           ivcn04 = 32                                                  
           ivcn04 = ivcn04 / 4                                          
           ivcn05 = ivcn05                                              
           iacn11(1) = iacn11(1) + 4                                    
           iacn11(2) = iacn11(2) + 3                                    
           iacn11(3) = iacn11(3) + 2                                    
           iacn11(4) = iacn11(4) + 1                                    
!      TEST 008                                                          
           ivcnd1 = ivcnd1 + 1                                          
           ivcnd2 = ivcnd2 + 1                                          
!      TEST 009                                                          
           ivcn06 = ivcn06 + 1                                          
           rvcnd1 = 4.5                                                 
           lvcnd1 = .true.                                              
           ivcn07 = -ivcn07                                             
           ivcn08 = -3                                                  
           rvcne1 = -6.7                                                
!      TEST 012                                                          
           ivcn10 = ivcn10 * ivcn10                                     
!      TEST 013                                                          
           ivcni1 = ivcni1 + 1                                          
           ivcni2 = ivcni2 + 1                                          
           ivcni3 = ivcni3 + 1                                          
!      TEST 014                                                          
           ivcn13 = 5                                                   
!      TEST 015                                                          
           ivcnk1 = 3                                                   
!                                                                        
!      FOR TESTS 014 AND 015 EXTERNAL FUNCTION FF305 IS REFERENCED       
!                                                                        
           ivon99  = ff305()
!      TEST 014                                                          
           ivcn12 = ivcn13                                              
!      TEST 015                                                          
           ivcnj1 = ivcnk1                                              
      ff304 = 999                                                       
      return                                                            
      end function ff304
      integer function ff305()
      integer, dimension(1:15) :: iacn11
      integer :: ivcn12
      integer :: ivcn13
      integer :: ivcn14
      integer :: ivcnj1
      integer :: ivcnk1
!                                                                        
!           FF305 IS AN EXTERNAL FUNCTION WHICH IS USED IN TEST 014 AND  
!      015 OF PROGRAM FM302. THIS SUBPROGRAM IS REFERENCED FROM EXTERNAL 
!      FUNCTION FF304.                                                   
!                                                                        
!      TEST 014                                                          
           ivcn14 = 11                                                  
           ivcn13 = ivcn14                                              
!      TEST 015                                                          
           ivcnk1 = 5                                                   
      ff305 = 999                                                       
      return                                                            
      end function ff305
