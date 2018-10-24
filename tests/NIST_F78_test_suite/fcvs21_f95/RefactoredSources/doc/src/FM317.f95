      program fm317
!                                                                        
!                                                                        
!           THIS ROUTINE TESTS SUBSET LEVEL FEATURES OF EXTERNAL         
!      FUNCTION SUBPROGRAMS.  TESTS ARE DESIGNED TO CHECK THE            
!      ASSOCIATION OF ALL PERMISSIBLE FORMS OF ACTUAL ARGUMENTS WITH     
!      VARIABLE, ARRAY AND PROCEDURE NAME DUMMY ARGUMENTS.  THESE        
!      INCLUDE,                                                          
!                                                                        
!           1) ACTUAL ARGUMENTS ASSOCIATED TO VARIABLE NAME DUMMY        
!              ARGUMENT INCLUDE,                                         
!                                                                        
!              A) CONSTANT                                               
!              B) VARIABLE NAME                                          
!              C) ARRAY ELEMENT NAME                                     
!              D) EXPRESSION INVOLVING OPERATORS                         
!              E) EXPRESSION ENCLOSED IN PARENTHESES                     
!              F) INTRINSIC FUNCTION REFERENCE                           
!              G) EXTERNAL FUNCTION REFERENCE                            
!              H) STATEMENT FUNCTION REFERENCE                           
!              I) ACTUAL ARGUMENT NAME SAME AS DUMMY ARGUMENT NAME       
!                                                                        
!           2) ACTUAL ARGUMENTS ASSOCIATED TO ARRAY NAME DUMMY           
!              ARGUMENT INCLUDE,                                         
!                                                                        
!              A) ARRAY NAME                                             
!              B) ARRAY ELEMENT NAME                                     
!                                                                        
!           3) ACTUAL ARGUMENTS ASSOCIATED TO PROCEDURE NAME DUMMY       
!              ARGUMENT INCLUDE,                                         
!                                                                        
!              A) EXTERNAL FUNCTION NAME                                 
!              B) INTRINSIC FUNCTION NAME                                
!              C) SUBROUTINE NAME                                        
!                                                                        
!      SUBSET LEVEL ROUTINES FM028,FM050 AND FM080 ALSO TEST THE USE OF  
!      EXTERNAL FUNCTIONS.                                               
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 2.8,     DUMMY ARGUMENTS                               
!         SECTION 5.1.2.2, DUMMY ARRAY DECLARATOR                        
!         SECTION 5.5,     DUMMY AND ACTUAL ARRAYS                       
!         SECTION 8.1,     DIMENSION STATEMENT                           
!         SECTION 8.3,     COMMON STATEMENT                              
!         SECTION 8.4,     TYPE-STATEMENT                                
!         SECTION 8.7,     EXTERNAL STATEMENT                            
!         SECTION 8.8,     INTRINSIC STATEMENT                           
!         SECTION 15.2,    REFERENCING A FUNCTION                        
!         SECTION 15.3,    INTRINSIC FUNCTIONS                           
!         SECTION 15.5,    EXTERNAL FUNCTIONS                            
!         SECTION 15.6,    SUBROUTINES                                   
!         SECTION 15.9,    ARGUMENTS AND COMMON BLOCKS                   
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
      integer, dimension(1:6) :: iacn11
      real, dimension(1:10) :: racn11
      integer :: ifos01
      integer :: idon04
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      real :: rvcomp
      real :: rvcorr
      integer :: ivon01
      real :: rvon01
      logical  :: lvon01
      integer :: ivon02
      integer :: ivon03
      real :: rvon02
      real :: rvon03
      logical  :: lvon02
      integer :: idon01
      integer :: ff318! decl of func/sub in program
      integer :: ff321! decl of func/sub in program
      integer :: ff322! decl of func/sub in program
      integer :: ff324! decl of func/sub in program
      integer :: ff325
      logical :: ff320! decl of func/sub in program
      intrinsic  abs, iabs, nint                                        
      external ff318, ff321, ff325, fs327                               
      integer, dimension(1:4) :: iadn11
      integer, dimension(1:4) :: iadn12
      real, dimension(1:4) :: radn11
      real, dimension(1:4) :: radn12
      logical, dimension(1:4) :: ladn11
      integer, dimension(1:2,1:3) :: iatn11
      real, dimension(1:3,1:4) :: ratn11
      ifos01(idon04) = idon04 + 1                                       
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
!      TEST 001 THROUGH TEST 022 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 
!      OF ACTUAL ARGUMENTS TO VARIABLE NAMES USED AS EXTERNAL FUNCTION   
!      DUMMY ARGUMENTS.  INTEGER, REAL AND LOGICAL DUMMY ARGUMENTS ARE   
!      TESTED.                                                           
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 317  -  TEST 001  ****                         
!                                                                        
!      INTEGER CONSTANT AS ACTUAL ARGUMENT                               
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ff318(3)
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
!      ****  FCVS PROGRAM 317  -  TEST 002  ****                         
!                                                                        
!      REAL CONSTANT AS ACTUAL ARGUMENT                                  
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvcomp = 0.0                                                      
      rvcomp  = ff319(3.0)
      rvcorr = 4.0                                                      
40020 if (rvcomp - 3.9995) 20020, 10020, 40021                          
40021 if (rvcomp - 4.0005) 10020, 10020, 20020                          
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
!      ****  FCVS PROGRAM 317  -  TEST 003  ****                         
!                                                                        
!      LOGICAL CONSTANT AS ACTUAL ARGUMENT                               
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 0                                                        
      if (ff320(.false.)) ivcomp  = 1
      ivcorr = 1                                                        
40030 if (ivcomp - 1) 20030, 10030, 20030                               
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
!      ****  FCVS PROGRAM 317  -  TEST 004  ****                         
!                                                                        
!      INTEGER VARIABLE AS ACTUAL ARGUMENT                               
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 7                                                        
      ivcomp  = ff318(ivon01)
      ivcorr = 8                                                        
40040 if (ivcomp - 8) 20040, 10040, 20040                               
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
!      ****  FCVS PROGRAM 317  -  TEST 005  ****                         
!                                                                        
!      REAL VARIABLE AS ACTUAL ARGUMENT                                  
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = 7.0                                                      
      rvcomp  = ff319(rvon01)
      rvcorr = 8.0                                                      
40050 if (rvcomp - 7.9995) 20050, 10050, 40051                          
40051 if (rvcomp - 8.0005) 10050, 10050, 20050                          
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
!      ****  FCVS PROGRAM 317  -  TEST 006  ****                         
!                                                                        
!      LOGICAL VARIABLE AS ACTUAL ARGUMENT                               
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      lvon01 = .true.                                                   
      ivcomp = 0                                                        
      if (.not. ff320(lvon01)) ivcomp  = 1
      ivcorr = 1                                                        
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
!      ****  FCVS PROGRAM 317  -  TEST 007  ****                         
!                                                                        
!      INTEGER ARRAY ELEMENT NAME AS ACTUAL ARGUMENT                     
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      iadn11(2) = 2                                                     
      ivcomp  = ff318(iadn11(2))
      ivcorr = 3                                                        
40070 if (ivcomp - 3) 20070, 10070, 20070                               
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
!      ****  FCVS PROGRAM 317  -  TEST 008  ****                         
!                                                                        
!      REAL ARRAY ELEMENT NAME AS ACTUAL ARGUMENT                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      rvcomp = 0.0                                                      
      radn11(4) = 4.0                                                   
      rvcomp  = ff319(radn11(4))
      rvcorr = 5.0                                                      
40080 if (rvcomp - 4.9995) 20080, 10080, 40081                          
40081 if (rvcomp - 5.0005) 10080, 10080, 20080                          
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
!      ****  FCVS PROGRAM 317  -  TEST 009  ****                         
!                                                                        
!      LOGICAL ARRAY ELEMENT NAME AS ACTUAL ARGUMENT                     
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ladn11(1) = .false.                                               
      ivcomp = 0                                                        
      if (ff320(ladn11(1))) ivcomp  = 1
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
!      ****  FCVS PROGRAM 317  -  TEST 010  ****                         
!                                                                        
!      INTEGER EXPRESSION INVOLVING OPERATORS AS ACTUAL ARGUMENT         
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      ivon02 = 2                                                        
      ivon03 = 3                                                        
      ivcomp  = ff318(ivon02+3*ivon03-7)
      ivcorr = 5                                                        
40100 if (ivcomp - 5) 20100, 10100, 20100                               
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
!      ****  FCVS PROGRAM 317  -  TEST 011  ****                         
!                                                                        
!      REAL EXPRESSION INVOLVING OPERATORS AS ACTUAL ARGUMENT            
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      rvcomp = 0.0                                                      
      rvon02 = 2.                                                       
      rvon03 = 1.2                                                      
      rvcomp  = ff319(rvon02*rvon03*1.0/.6)
      rvcorr = 5.0                                                      
40110 if (rvcomp - 4.9995) 20110, 10110, 40111                          
40111 if (rvcomp - 5.0005) 10110, 10110, 20110                          
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
!      ****  FCVS PROGRAM 317  -  TEST 012  ****                         
!                                                                        
!      REAL EXPRESSION INVOLVING INTEGER AND REAL PRIMARIES AND OPERATORS
!      AS ACTUAL ARGUMENT.                                               
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      rvcomp = 0.0                                                      
      ivon01 = 2                                                        
      radn11(2) = 2.5                                                   
      rvcomp  = ff319(ivon01**3*(radn11(2)-1)+2.0)
      rvcorr = 15.0                                                     
40120 if (rvcomp - 14.995) 20120, 10120, 40121                          
40121 if (rvcomp - 15.005) 10120, 10120, 20120                          
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
!      ****  FCVS PROGRAM 317  -  TEST 013  ****                         
!                                                                        
!      LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.NOT.) AS ACTUAL   
!      ARGUMENT.                                                         
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      lvon01 = .true.                                                   
      ivcomp = 0                                                        
      if (ff320(.not. lvon01)) ivcomp  = 1
      ivcorr = 1                                                        
40130 if (ivcomp - 1) 20130, 10130, 20130                               
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
!      ****  FCVS PROGRAM 317  -  TEST 014  ****                         
!                                                                        
!      LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.OR.) AS ACTIVE    
!      ARGUMENT.                                                         
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      ivcomp = 0                                                        
      if (.not. ff320(lvon01 .or. lvon02)) ivcomp  = 1
      ivcorr = 1                                                        
40140 if (ivcomp - 1) 20140, 10140, 20140                               
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
!      ****  FCVS PROGRAM 317  -  TEST 015  ****                         
!                                                                        
!      LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.AND.) AS ACTUAL   
!      ARGUMENT.                                                         
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      ivcomp = 0                                                        
      if (ff320(lvon01 .and. lvon02)) ivcomp  = 1
      ivcorr = 1                                                        
40150 if (ivcomp - 1) 20150, 10150, 20150                               
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
!      ****  FCVS PROGRAM 317  -  TEST 016  ****                         
!                                                                        
!      EXPRESSION ENCLOSED IN PARENTHESES AS ACTUAL ARGUMENT             
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 6                                                        
      ivcomp  = ff318((ivon01+3))
      ivcorr = 10                                                       
40160 if (ivcomp - 10) 20160, 10160, 20160                              
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
!      ****  FCVS PROGRAM 317  -  TEST 017  ****                         
!                                                                        
!      REAL INTRINSIC FUNCTION REFERENCE AS ACTUAL ARGUMENT.             
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = -5.2                                                     
      rvcomp  = ff319(abs(rvon01))
      rvcorr = 6.2                                                      
40170 if (rvcomp - 6.1995) 20170, 10170, 40171                          
40171 if (rvcomp - 6.2005) 10170, 10170, 20170                          
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
!      ****  FCVS PROGRAM 317  -  TEST 018  ****                         
!                                                                        
!      INTEGER INTRINSIC FUNCTION REFERENCE AS ACTUAL ARGUMENT.          
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 0                                                        
      rvon01 = 4.7                                                      
      ivcomp  = ff318(nint(rvon01))
      ivcorr =  6                                                       
40180 if (ivcomp - 6) 20180, 10180, 20180                               
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
!      ****  FCVS PROGRAM 317  -  TEST 019  ****                         
!                                                                        
!      EXTERNAL FUNCTION REFERENCE AS ACTUAL ARGUMENT.                   
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 4                                                        
      ivcomp  = ff318(ff321(ivon01))
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
!      ****  FCVS PROGRAM 317  -  TEST 020  ****                         
!                                                                        
!      EXTERNAL FUNCTION REFERENCE WHICH USES A REFERENCE TO ITSELF      
!      AS AN ACTUAL ARGUMENT.                                            
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ff318(ff318(4))
      ivcorr = 6                                                        
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
!      ****  FCVS PROGRAM 317  -  TEST 021  ****                         
!                                                                        
!      USE AN ACTUAL ARGUMENT NAME WHICH IS IDENTICAL TO THE DUMMY       
!      ARGUMENT NAME.                                                    
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 0                                                        
      idon01 = 10                                                       
      ivcomp  = ff318(idon01)
      ivcorr = 11                                                       
40210 if (ivcomp - 11) 20210, 10210, 20210                              
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
!      ****  FCVS PROGRAM 317  -  TEST 022  ****                         
!                                                                        
!      USE STATEMENT FUNCTION REFERENCE AS ACTUAL ARGUMENT.              
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ff318(ifos01(4))
      ivcorr = 6                                                        
40220 if (ivcomp - 6) 20220, 10220, 20220                               
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
!      TEST 023 THROUGH TEST 028 ARE DESIGNED TO ASSOCIATE VARIOUS       
!      FORMS OF ACTUAL ARGUMENTS TO ARRAY NAMES USED AS EXTERNAL         
!      FUNCTION DUMMY ARGUMENTS.                                         
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 317  -  TEST 023  ****                         
!                                                                        
!      USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       
!      ARGUMENT ARRAY DECLARATOR IS IDENTICAL TO THE ASSOCIATED DUMMY    
!      ARGUMENT ARRAY DECLARATOR.                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 0                                                        
      iadn12(1) = 1                                                     
      iadn12(2) = 10                                                    
      iadn12(3) = 100                                                   
      iadn12(4) = 1000                                                  
      ivcomp  = ff322(iadn12)
      ivcorr = 1111                                                     
40230 if (ivcomp - 1111) 20230, 10230, 20230                            
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
!      ****  FCVS PROGRAM 317  -  TEST 024  ****                         
!                                                                        
!      USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE OF THE  
!      ACTUAL ARGUMENT ARRAY IS LARGER THAN THE SIZE OF THE ASSOCIATED   
!      DUMMY ARGUMENT ARRAY.                                             
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivcomp = 0                                                        
      iacn11(1) = 1                                                     
      iacn11(2) = 10                                                    
      iacn11(3) = 100                                                   
      iacn11(4) = 1000                                                  
      iacn11(5) = 10000                                                 
      ivcomp  = ff322(iacn11)
      ivcorr = 1111                                                     
40240 if (ivcomp - 1111) 20240, 10240, 20240                            
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
!      ****  FCVS PROGRAM 317  -  TEST 025  ****                         
!                                                                        
!      USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       
!      ARGUMENT ARRAY DECLARATOR IS LARGER AND HAS MORE SUBSCRIPT        
!      EXPRESSIONS THAN THE ASSOCIATED DUMMY ARGUMENT ARRAY DECLARATOR.  
!      THE ASSOCIATED DUMMY ARGUMENT ARRAY DECLARATOR.                   
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      ivcomp = 0                                                        
      iatn11(1,1) = 1                                                   
      iatn11(2,1) = 10                                                  
      iatn11(1,2) = 100                                                 
      iatn11(2,2) = 1000                                                
      iatn11(1,3) = 10000                                               
      ivcomp  = ff322(iatn11)
      ivcorr = 1111                                                     
40250 if (ivcomp - 1111) 20250, 10250, 20250                            
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
!      ****  FCVS PROGRAM 317  -  TEST 026  ****                         
!                                                                        
!      USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE      
!      ASSOCIATED ACTUAL AND DUMMY ARRAY DECLARATORS ARE IDENTICAL.  ALL 
!      ARRAY ELEMENTS OF THE ACTUAL ARRAY SHOULD BE PASSED TO THE        
!      DUMMY ARRAY OF THE EXTERNAL FUNCTION.                             
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      rvcomp = 0.0                                                      
      radn12(1) = 1.                                                    
      radn12(2) = 10.                                                   
      radn12(3) = 100.                                                  
      radn12(4) = 1000.                                                 
      rvcomp  = ff323(radn12(1))
      rvcorr = 1111.                                                    
40260 if (rvcomp - 1110.5) 20260, 10260, 40261                          
40261 if (rvcomp - 1111.5) 10260, 10260, 20260                          
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
!      ****  FCVS PROGRAM 317  -  TEST 027  ****                         
!                                                                        
!      USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 
!      OF THE ACTUAL ARGUMENT ARRAY IS LARGER AND HAS FEWER SUBSCRIPT    
!      EXPRESSIONS THAN THE ASSOCIATED DUMMY ARGUMENT ARRAY.  ONLY ACTUAL
!      ARRAY ELEMENTS WITH SUBSCRIPT VALUES OF 5, 6, 7 AND 8 (OUT OF A   
!      POSSIBLE 10 ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF THE  
!      EXTERNAL FUNCTION.                                                
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      rvcomp = 0.0                                                      
      racn11(4) = 1.                                                    
      racn11(5) = 10.                                                   
      racn11(6) = 100.                                                  
      racn11(7) = 1000.                                                 
      racn11(8) = 10000.                                                
      racn11(9) = 100000.                                               
      rvcorr =  11110.                                                  
      rvcomp  = ff323(racn11(5))
40270 if (rvcomp - 11105.) 20270, 10270, 40271                          
40271 if (rvcomp - 11115.) 10270, 10270, 20270                          
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
!      ****  FCVS PROGRAM 317  -  TEST 028  ****                         
!                                                                        
!      USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 
!      OF THE ACTUAL ARGUMENT ARRAY IS LARGE THAN THE SIZE OF THE        
!      ASSOCIATED DUMMY ARGUMENT ARRAY.  ONLY ACTUAL ARRAY ELEMENTS WITH 
!      SUBSCRIPT VALUES OF 9, 10, 11 AND 12 (OUT OF A POSSIBLE 12        
!      ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF THE EXTERNAL     
!      FUNCTION.                                                         
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      rvcomp = 0.0                                                      
      ratn11(2,3) = 1.                                                  
      ratn11(3,3) = 10.                                                 
      ratn11(1,4) = 100.                                                
      ratn11(2,4) = 1000.                                               
      ratn11(3,4) = 10000.                                              
      rvcomp  = ff323(ratn11(3,3))
      rvcorr = 11110.                                                   
40280 if (rvcomp - 11105.) 20280, 10280, 40281                          
40281 if (rvcomp - 11115.) 10280, 10280, 20280                          
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
!      TEST 029 THROUGH TEST 032 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 
!      OF ACTUAL ARGUMENTS TO PROCEDURES USED AS DUMMY ARGUMENTS.        
!      ACTUAL ARGUMENTS TESTED INCLUDE THE NAMES OF AN EXTERNAL FUNCTION,
!      AN INTRINSIC FUNCTION, AND A SUBROUTINE.                          
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 317  -  TEST 029  ****                         
!                                                                        
!      USE AN EXTERNAL FUNCTION NAME AS AN ACTUAL ARGUMENT.              
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ff324(ff325,5)
      ivcorr = 7                                                        
40290 if (ivcomp - 7) 20290, 10290, 20290                               
30290 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10290, 0301, 20290                                    
10290 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0301                                                        
20290 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0301 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 317  -  TEST 030  ****                         
!                                                                        
!      USE AN INTRINSIC FUNCTION NAME AS AN ACTUAL ARGUMENT.             
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = ff324(iabs,-7)
      ivcorr = 8                                                        
40300 if (ivcomp - 8) 20300, 10300, 20300                               
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
!      ****  FCVS PROGRAM 317  -  TEST 031  ****                         
!                                                                        
!      USE AN EXTERNAL FUNCTION NAME AS AN ACTUAL ARGUMENT.  THE         
!      INTRINSIC FUNCTION NAME (NINT) IS USED AS THE DUMMY PROCEDURE     
!      NAME IN THE EXTERNAL FUNCTION AND THEREFORE CAN NOT BE USED AS    
!      AN INTRINSIC FUNCTION WITHIN THAT PROGRAM UNIT.  HOWEVER IT CAN   
!      BE REFERENCED IN THE MAIN PROGRAM FM317 AND IN THE SUBPROGRAM     
!      FF325.                                                            
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      ivcomp = 0                                                        
      ivcomp  = nint(3.7)+ff324(ff325,2)
      ivcorr = 8                                                        
40310 if (ivcomp - 8) 20310, 10310, 20310                               
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
!      ****  FCVS PROGRAM 317  -  TEST 032  ****                         
!                                                                        
!      USE A SUBROUTINE NAME AS AN ACTUAL ARGUMENT.                      
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = 3.5                                                      
      rvcomp  = ff326(fs327,rvon01)
      rvcorr = 5.5                                                      
40320 if (rvcomp - 5.4995) 20320, 10320, 40321                          
40321 if (rvcomp - 5.5005) 10320, 10320, 20320                          
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
90001 format (" ",24x,"FM317")                                          
90000 format (" ",20x,"END OF PROGRAM FM317" )                          
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
      end program fm317
      integer function ff318(idon01)
      integer :: idon01
!           THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 
!      TO TEST THE ASSOCIATION OF VARIOUS FORMS OF INTEGER ACTUAL        
!      ARGUMENTS TO AN INTEGER VARIABLE NAME USED AS AN EXTERNAL         
!      FUNCTION DUMMY ARGUMENT.  THIS ROUTINE INCREMENTS THE ARGUMENT    
!      VALUE BY ONE AND RETURNS THE RESULT AS THE FUNCTION VALUE.        
      ff318 = idon01 + 1                                                
      return                                                            
      end function ff318
      real function ff319(rdon01)
      real :: rdon01
!           THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 
!      TO TEST THE ASSOCIATION OF VARIOUS FORMS OF REAL ACTUAL           
!      ARGUMENTS TO A REAL VARIABLE NAME USED AS AN EXTERNAL FUNCTION    
!      DUMMY ARGUMENT.  THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY    
!      ONE AND RETURNS THE RESULT AS THE FUNCTION VALUE.                 
      ff319 = rdon01 + 1.0                                              
      return                                                            
      end function ff319
      logical function ff320(ldon01)
!           THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 
!      TO TEST THE ASSOCIATION OF VARIOUS FORMS OF LOGICAL ACTUAL        
!      ARGUMENTS TO A LOGICAL VARIABLE NAME USED AS AN EXTERNAL          
!      FUNCTION DUMMY ARGUMENT.  THIS ROUTINE NEGATES THE ARGUMENT       
!      VALUE AND RETURNS THE RESULT AS THE FUNCTION VALUE.               
      logical :: ldon01
      ff320 = .not. ldon01                                              
      return                                                            
      end function ff320
      integer function ff321(idon02)
      integer :: idon02
!           THIS FUNCTION IS USED IN TEST 019 OF MAIN PROGRAM FM317 AS   
!      THE TEST OF THE USE OF AN EXTERNAL FUNCTION REFERENCE AS AN       
!      ACTUAL ARGUMENT TO A VARIABLE NAME USED AS AN EXTERNAL FUNCTION   
!      DUMMY ARGUMENT.  THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY    
!      ONE AND RETURNS THE RESULT AS THE FUNCTION VALUE.                 
      ff321 = idon02 + 1                                                
      return                                                            
      end function ff321
      integer function ff322(iddn11)
!           THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 
!      TO TEST THE ASSOCIATION OF VARIOUS FORMS OF ARRAY NAMES USED AS   
!      ACTUAL ARGUMENTS TO AN ARRAY NAME USED AS AN EXTERNAL FUNCTION    
!      DUMMY ARGUMENT.  THIS ROUTINE ADDS TOGETHER THE FOUR ELEMENTS IN  
!      THE DUMMY ARRAY AND RETURNS THE SUM AS THE FUNCTION VALUE.        
      integer, dimension(1:4) :: iddn11
      ff322 = iddn11(1) + iddn11(2) + iddn11(3) + iddn11(4)             
      return                                                            
      end function ff322
      real function ff323(rdtn21)
!           THIS FUNCTION IS USED BY VARIOUS TESTS IN MAIN PROGRAM FM317 
!      TO TEST THE ASSOCIATION OF VARIOUS FORMS OF ARRAY ELEMENT NAMES   
!      USED AS ACTUAL ARGUMENTS TO AN ARRAY NAME USED AS AN EXTERNAL     
!      FUNCTION DUMMY ARGUMENT.  THIS ROUTINE ADDS TOGETHER THE FOUR     
!      ELEMENTS IN THE DUMMY ARRAY AND RETURNS THE SUM AS THE FUNCTION   
!      VALUE.                                                            
      real, dimension(1:2,1:2) :: rdtn21
      ff323 = rdtn21(1,1) + rdtn21(2,1) + rdtn21(1,2) + rdtn21(2,2)     
      return                                                            
      end function ff323
      integer function ff324(nint,idon03)
      integer :: nint
      integer :: idon03
!           THIS FUNCTION IS USED BY TESTS 029, 030 AND 031 OF MAIN      
!      PROGRAM FM317 TO TEST THE ASSOCIATION OF EXTERNAL FUNCTION AND    
!      INTRINSIC FUNCTION NAMES USED AS ACTUAL ARGUMENTS TO A PROCEDURE  
!      NAME USED AS A DUMMY ARGUMENT.  THIS FUNCTION REFERENCES THE      
!      EXTERNAL FUNCTION OR INTRINSIC FUNCTION PASSED AS A PROCEDURE     
!      NAME ARGUMENT, INCREMENTING THE RESULT BY ONE BEFORE RETURNING    
!      THE RESULT AS THE FUNCTION VALUE.                                 
      ff324 = nint(idon03) + 1                                          
!           **** THE NAME NINT IS A DUMMY ARGUMENT                       
!                    AND NOT AN INTRINSIC FUNCTION REFERENCE *****       
      return                                                            
      end function ff324
      integer function ff325(idon05)
      integer :: idon05
!           THIS FUNCTION IS USED BY TESTS 029 AND 031 OF MAIN PROGRAM   
!      FM317 TO TEST THE ASSOCIATION OF AN EXTERNAL FUNCTION NAME USED AS
!      AN ACTUAL ARGUMENT TO A PROCEDURE NAME USED AS A DUMMY ARGUMENT.  
!      FF325 IS REFERENCED FROM EXTERNAL FUNCTION FF324 VIA A DUMMY      
!      PROCEDURE NAME REFERENCE.  THIS ROUTINE ADDS THE RESULT OF AN     
!      INTRINSIC FUNCTION REFERENCE (NINT) TO THE ARGUMENT VALUE AND     
!      RETURNS THE SUM AS THE FUNCTION VALUE.                            
      ff325 = idon05 + nint(1.2)                                        
      return                                                            
      end function ff325
      real function ff326(rdon02,rdon03)
!       real :: rdon02! EXTERNAL SUB !       real :: rdon02! EXTERNAL SUB
      real :: rdon03
!           THIS FUNCTION IS USED BY TEST 032 OF MAIN PROGRAM FM317 TO   
!      TEST THE ASSOCIATION OF A SUBROUTINE NAME USED AS AN ACTUAL       
!      ARGUMENT TO A PROCEDURE NAME USED AS A DUMMY ARGUMENT.  THIS      
!      FUNCTION CALLS THE SUBROUTINE (FS327) PASSED AS A PROCEDURE NAME  
!      ARGUMENT.  THE VALUE OF THE ARGUMENT RETURNED FROM THIS           
!      REFERENCE IS THEN INCREMENTED BY ONE BEFORE RETURNING THE SUM AS  
!      THE FUNCTION VALUE.                                               
      call rdon02(rdon03)                                               
      ff326 = rdon03 + 1.0                                              
      return                                                            
      end function ff326
      subroutine fs327(rdon04)
      real :: rdon04
!           THIS SUBROUTINE IS USED BY TEST 032 OF MAIN PROGRAM FM317 TO 
!      TEST THE ASSOCIATION OF A SUBROUTINE NAME USED AS AN ACTUAL       
!      ARGUMENT TO A PROCEDURE NAME USED AS A DUMMY ARGUMENT.  FS327 IS  
!      CALLED FROM EXTERNAL PROGRAM FF326 VIA A DUMMY PROCEDURE NAME     
!      REFERENCE.  THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY ONE.    
      rdon04 = rdon04 + 1.0                                             
      return                                                            
      end subroutine fs327
