      program fm328
!                                                                        
!                                                                        
!           THIS ROUTINE TEST SUBSET LEVEL FEATURES OF                   
!      SUBROUTINE SUBPROGRAMS.  TESTS ARE DESIGNED TO CHECK THE          
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
!      ALL DATA PASSED TO THE REFERENCED SUBPROGRAMS ARE PASSED VIA      
!      ARGUMENT VALUES, WHILE ALL RESULTS RETURNED TO FM328 ARE          
!      RETURNED VIA VARIABLES IN NAMED COMMON.   SUBSET LEVEL ROUTINES   
!      FM026, FM050 AND FM056 ALSO TEST THE USE OF SUBROUTINES.          
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
      integer :: ivcn01
      logical  :: lvcn01
      real, dimension(1:10) :: racn11
      real :: rvcn01
      integer :: ifos01
      integer :: idon04
      real :: rfos01
      real :: rdon04
      logical  :: lfos01
      logical  :: ldon04
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
      real :: rvon01
      logical  :: lvon01
      integer :: ivon02
      integer :: ivon03
      real :: rvon02
      real :: rvon03
      real :: rvcomp
      real :: rvcorr
      logical  :: lvon02
      integer :: idon01
      real :: rdon01
      logical  :: ldon01
      integer, dimension(1:2,1:3) :: iatn11
      real, dimension(1:3,1:4) :: ratn11
      integer :: ff330! decl of func/sub in program
      integer, dimension(1:4) :: iadn11
      integer, dimension(1:4) :: iadn12
      real, dimension(1:4) :: radn11
      real, dimension(1:4) :: radn12
      logical, dimension(1:4) :: ladn11
      external ff330, fs335                                             
      intrinsic  abs, iabs, nint                                        
      ifos01(idon04) = idon04 + 1                                       
      rfos01(rdon04) = rdon04 + 1.0                                     
      lfos01(ldon04) = .not. ldon04                                     
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
!      TEST 001 THROUGH TEST 013 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 
!      OF ACTUAL ARGUMENTS TO VARIABLE NAMES USED AS SUBROUTINE          
!      DUMMY ARGUMENTS.  INTEGER, REAL AND LOGICAL DUMMY ARGUMENTS ARE   
!      TESTED.                                                           
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 328  -  TEST 001  ****                         
!                                                                        
!      USE INTEGER, REAL AND LOGICAL CONSTANTS AS ACTUAL ARGUMENTS.      
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      call fs329(3,3.0,.false.,ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  4) ivcomp = ivcomp * 2                            
      if (rvcn01  >=  3.9995 .and. rvcn01  <=  4.0005) ivcomp = ivcomp*3
      if (lvcn01) ivcomp = ivcomp * 5                                   
      ivcorr = 30                                                       
40010 if (ivcomp - 30) 20010, 10010, 20010                              
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
!      ****  FCVS PROGRAM 328  -  TEST 002  ****                         
!                                                                        
!      USE INTEGER, REAL AND LOGICAL VARIABLES AS ACTUAL ARGUMENTS.      
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivon01 = 7                                                        
      rvon01 = 7.0                                                      
      lvon01 = .true.                                                   
      call fs329(ivon01,rvon01,lvon01,ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  8) ivcomp =ivcomp * 2                             
      if (rvcn01  >=  7.9995 .and. rvcn01  <=  8.0005) ivcomp = ivcomp*3
      if (.not. lvcn01) ivcomp = ivcomp * 5                             
      ivcorr = 30                                                       
40020 if (ivcomp - 30) 20020, 10020, 20020                              
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
!      ****  FCVS PROGRAM 328  -  TEST 003  ****                         
!                                                                        
!      USE INTEGER, REAL AND LOGICAL ARRAY ELEMENT NAMES AS ACTUAL       
!      ARGUMENTS.                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      iadn11(2) = 2                                                     
      radn11(4) = 4.0                                                   
      ladn11(1) = .false.                                               
      call fs329(iadn11(2),radn11(4),ladn11(1),ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  3) ivcomp = ivcomp * 2                            
      if (rvcn01  >=  4.9995 .and. rvcn01  <=  5.0005) ivcomp = ivcomp*3
      if (lvcn01) ivcomp = ivcomp * 5                                   
      ivcorr = 30                                                       
40030 if (ivcomp - 30) 20030, 10030, 20030                              
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
!      ****  FCVS PROGRAM 328  -  TEST 004  ****                         
!                                                                        
!      INTEGER AND REAL EXPRESSIONS INVOLVING OPERATORS AS ACTUAL        
!      ARGUMENTS.                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivon02 = 2                                                        
      ivon03 = 3                                                        
      rvon02 = 2.                                                       
      rvon03 = 1.2                                                      
      call fs329(ivon02+3*ivon03-7,rvon02*rvon03*1.0/.6,.true.,ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  5) ivcomp = ivcomp * 2                            
      if (rvcn01  >=  4.9995 .and. rvcn01  <=  5.0005) ivcomp = ivcomp*3
      ivcorr = 6                                                        
40040 if (ivcomp -  6) 20040, 10040, 20040                              
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
!      ****  FCVS PROGRAM 328  -  TEST 005  ****                         
!                                                                        
!      REAL EXPRESSION INVOLVING INTEGER AND REAL PRIMARIES AND OPERATORS
!      AS ACTUAL ARGUMENT.                                               
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      rvcomp = 0.0                                                      
      ivon01 = 2                                                        
      radn11(2) = 2.5                                                   
      call fs329(1,ivon01**3*(radn11(2)-1)+2.0,.true.,ivcn01,lvcn01,rvcn01)

      rvcomp = rvcn01                                                   
      rvcorr = 15.0                                                     
40050 if (rvcomp - 14.995) 20050, 10050, 40051                          
40051 if (rvcomp - 15.005) 10050, 10050, 20050                          
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
!      ****  FCVS PROGRAM 328  -  TEST 006  ****                         
!                                                                        
!      LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.NOT.) AS ACTUAL   
!      ARGUMENT.                                                         
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      lvon01 = .true.                                                   
      call fs329(1,1.0,.not.lvon01,ivcn01,lvcn01,rvcn01)

      ivcomp = 0                                                        
      if (lvcn01) ivcomp = 1                                            
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
!      ****  FCVS PROGRAM 328  -  TEST 007  ****                         
!                                                                        
!      LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.OR.) AS ACTIVE    
!      ARGUMENT.                                                         
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      call fs329(1,1.0,lvon01.or.lvon02,ivcn01,lvcn01,rvcn01)

      ivcomp = 0                                                        
      if (.not. lvcn01) ivcomp = 1                                      
      ivcorr = 1                                                        
40070 if (ivcomp - 1) 20070, 10070, 20070                               
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
!      ****  FCVS PROGRAM 328  -  TEST 008  ****                         
!                                                                        
!      LOGICAL EXPRESSION INVOLVING LOGICAL OPERATOR (.AND.) AS ACTUAL   
!      ARGUMENT.                                                         
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      call fs329(1,1.0,lvon01.and.lvon02,ivcn01,lvcn01,rvcn01)

      ivcomp = 0                                                        
      if (lvcn01) ivcomp = 1                                            
      ivcorr = 1                                                        
40080 if (ivcomp - 1) 20080, 10080, 20080                               
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
!      ****  FCVS PROGRAM 328  -  TEST 009  ****                         
!                                                                        
!      EXPRESSION ENCLOSED IN PARENTHESES AS ACTUAL ARGUMENT.            
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 6                                                        
      call fs329((ivon01+3),1.0,.true.,ivcn01,lvcn01,rvcn01)

      ivcomp = ivcn01                                                   
      ivcorr = 10                                                       
40090 if (ivcomp - 10) 20090, 10090, 20090                              
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
!      ****  FCVS PROGRAM 328  -  TEST 010  ****                         
!                                                                        
!      INTEGER AND REAL INTRINSIC FUNCTION REFERENCES AS ACTUAL ARGUMENTS
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      rvon01 = 4.7                                                      
      rvon02 = -5.2                                                     
      call fs329(nint(rvon01),abs(rvon02),.true.,ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  6) ivcomp = ivcomp * 2                            
      if (rvcn01  >=  6.1995 .and. rvcn01  <=  6.2005) ivcomp = ivcomp*3
      ivcorr = 6                                                        
40100 if (ivcomp -  6) 20100, 10100, 20100                              
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
!      ****  FCVS PROGRAM 328  -  TEST 011  ****                         
!                                                                        
!      EXTERNAL FUNCTION REFERENCE AS ACTUAL ARGUMENT.                   
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 4                                                        
      call fs329(ff330(ivon01),1.0,.true.,ivcn01,lvcn01,rvcn01)

      call fs329(ff330(ivon01),1.0,.true.)
      ivcomp = ivcn01                                                   
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
!      ****  FCVS PROGRAM 328  -  TEST 012  ****                         
!                                                                        
!      USE ACTUAL ARGUMENT NAMES WHICH ARE IDENTICAL TO THE DUMMY        
!      ARGUMENT NAMES.                                                   
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      idon01 = 10                                                       
      rdon01 = 10.0                                                     
      ldon01 = .false.                                                  
      call fs329(idon01,rdon01,ldon01,ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  11) ivcomp = ivcomp * 2                           
      if (rvcn01  >=  10.995 .and. rvcn01  <=  11.005) ivcomp = ivcomp*3
      if (lvcn01) ivcomp = ivcomp * 5                                   
      ivcorr = 30                                                       
40120 if (ivcomp - 30) 20120, 10120, 20120                              
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
!      ****  FCVS PROGRAM 328  -  TEST 013  ****                         
!                                                                        
!      USE INTEGER, REAL AND LOGICAL STATEMENT FUNCTION REFERENCES AS    
!      ARGUMENT NAMES.                                                   
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvon01 = 5.0                                                      
      call fs329(ifos01(4),rfos01(rvon01),lfos01(.true.),ivcn01,lvcn01,rvcn01)

      ivcomp = 1                                                        
      if (ivcn01  ==  6) ivcomp = ivcomp * 2                            
      if (rvcn01  >=  6.9995 .and. rvcn01  <=  7.0005) ivcomp = ivcomp*3
      if (lvcn01) ivcomp = ivcomp * 5                                   
      ivcorr = 30                                                       
40130 if (ivcomp - 30) 20130, 10130, 20130                              
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
!      TEST 014 THROUGH TEST 019 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 
!      OF ACTUAL ARGUMENTS TO ARRAY NAMES USED AS SUBROUTINE DUMMY       
!      ARGUMENTS.                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 328  -  TEST 014  ****                         
!                                                                        
!      USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       
!      ARGUMENT ARRAY DECLARATOR IS IDENTICAL TO THE ASSOCIATED DUMMY    
!      ARGUMENT ARRAY DECLARATOR.                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 0                                                        
      iadn12(1) = 1                                                     
      iadn12(2) = 10                                                    
      iadn12(3) = 100                                                   
      iadn12(4) = 1000                                                  
      call fs331(iadn12,ivcn01,lvcn01,rvcn01)

      ivcomp = ivcn01                                                   
      ivcorr = 1111                                                     
40140 if (ivcomp - 1111) 20140, 10140, 20140                            
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
!      ****  FCVS PROGRAM 328  -  TEST 015  ****                         
!                                                                        
!      USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE OF THE  
!      ACTUAL ARGUMENT ARRAY IS LARGER THAN THE SIZE OF THE ASSOCIATED   
!      DUMMY ARGUMENT ARRAY.                                             
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 0                                                        
      iacn11(1) = 1                                                     
      iacn11(2) = 10                                                    
      iacn11(3) = 100                                                   
      iacn11(4) = 1000                                                  
      iacn11(5) = 10000                                                 
      call fs331(iacn11,ivcn01,lvcn01,rvcn01)

      ivcomp = ivcn01                                                   
      ivcorr = 1111                                                     
40150 if (ivcomp - 1111) 20150, 10150, 20150                            
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
!      ****  FCVS PROGRAM 328  -  TEST 016  ****                         
!                                                                        
!      USE AN ARRAY NAME AS AN ACTUAL ARGUMENT IN WHICH THE ACTUAL       
!      ARGUMENT ARRAY DECLARATOR IS LARGER AND HAS MORE SUBSCRIPT        
!      EXPRESSIONS THAN THE ASSOCIATED DUMMY ARGUMENT ARRAY DECLARATOR.  
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 0                                                        
      iatn11(1,1) = 1                                                   
      iatn11(2,1) = 10                                                  
      iatn11(1,2) = 100                                                 
      iatn11(2,2) = 1000                                                
      iatn11(1,3) = 10000                                               
      call fs331(iatn11,ivcn01,lvcn01,rvcn01)

      ivcomp = ivcn01                                                   
      ivcorr = 1111                                                     
40160 if (ivcomp - 1111) 20160, 10160, 20160                            
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
!      ****  FCVS PROGRAM 328  -  TEST 017  ****                         
!                                                                        
!      USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE      
!      ASSOCIATED ACTUAL AND DUMMY ARRAY DECLARATORS ARE IDENTICAL.  ALL 
!      ARRAY ELEMENTS OF THE ACTUAL ARRAY SHOULD BE PASSED TO THE        
!      DUMMY ARRAY OF THE SUBROUTINE.                                    
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      rvcomp = 0.0                                                      
      radn12(1) = 1.                                                    
      radn12(2) = 10.                                                   
      radn12(3) = 100.                                                  
      radn12(4) = 1000.                                                 
      call fs332(radn12(1),ivcn01,lvcn01,rvcn01)

      rvcomp = rvcn01                                                   
      rvcorr = 1111.                                                    
40170 if (rvcomp - 1110.5) 20170, 10170, 40171                          
40171 if (rvcomp - 1111.5) 10170, 10170, 20170                          
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
!      ****  FCVS PROGRAM 328  -  TEST 018  ****                         
!                                                                        
!      USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 
!      OF THE ACTUAL ARGUMENT ARRAY IS LARGER AND HAS FEWER SUBSCRIPT    
!      EXPRESSIONS THAN THE ASSOCIATED DUMMY ARRAY.  ONLY ACTUAL ARRAY   
!      ELEMENTS WITH SUBSCRIPT VALUES OF 5, 6, 7 AND 8 ( OUT OF A        
!      POSSIBLE 10 ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF      
!      THE SUBROUTINE.                                                   
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      rvcomp = 0.0                                                      
      racn11(4) = 1.                                                    
      racn11(5) = 10.                                                   
      racn11(6) = 100.                                                  
      racn11(7) = 1000.                                                 
      racn11(8) = 10000.                                                
      racn11(9) = 100000.                                               
      call fs332(racn11(5),ivcn01,lvcn01,rvcn01)

      rvcomp = rvcn01                                                   
      rvcorr =  11110.                                                  
40180 if (rvcomp - 11105.) 20180, 10180, 40181                          
40181 if (rvcomp - 11115.) 10180, 10180, 20180                          
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
!      ****  FCVS PROGRAM 328  -  TEST 019  ****                         
!                                                                        
!      USE AN ARRAY ELEMENT NAME AS AN ACTUAL ARGUMENT IN WHICH THE SIZE 
!      OF THE ACTUAL ARGUMENT ARRAY IS LARGE THAN THE SIZE OF THE        
!      ASSOCIATED DUMMY ARGUMENT ARRAY.  ONLY ACTUAL ARRAY ELEMENTS WITH 
!      SUBSCRIPT VALUES OF 9, 10, 11 AND 12 (OUT OF A POSSIBLE 12        
!      ELEMENTS) SHOULD BE PASSED TO THE DUMMY ARRAY OF THE SUBROUTINE.  
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      rvcomp = 0.0                                                      
      ratn11(2,3) = 1.                                                  
      ratn11(3,3) = 10.                                                 
      ratn11(1,4) = 100.                                                
      ratn11(2,4) = 1000.                                               
      ratn11(3,4) = 10000.                                              
      call fs332(ratn11(3,3),ivcn01,lvcn01,rvcn01)

      rvcomp = rvcn01                                                   
      rvcorr = 11110.                                                   
40190 if (rvcomp - 11105.) 20190, 10190, 40191                          
40191 if (rvcomp - 11115.) 10190, 10190, 20190                          
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
!      TEST 020 THROUGH TEST 022 ARE DESIGNED TO ASSOCIATE VARIOUS FORMS 
!      OF ACTUAL ARGUMENTS TO PROCEDURES USED AS SUBROUTINE DUMMY        
!      ARGUMENTS.  ACTUAL ARGUMENTS TESTED INCLUDE THE NAMES OF AN       
!      EXTERNAL FUNCTION, AN INTRINSIC FUNCTION AND A SUBROUTINE.        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 328  -  TEST 020  ****                         
!                                                                        
!      USE AN EXTERNAL FUNCTION NAME AS AN ACTUAL ARGUMENT.              
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 0                                                        
      call fs333(ff330,5,ivcn01,lvcn01,rvcn01)

      ivcomp = ivcn01                                                   
      ivcorr = 7                                                        
40200 if (ivcomp - 7) 20200, 10200, 20200                               
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
!      ****  FCVS PROGRAM 328  -  TEST 021  ****                         
!                                                                        
!      USE AN INTRINSIC FUNCTION NAME AS AN ACTUAL ARGUMENT.             
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 0                                                        
      call fs333(iabs,-7,ivcn01,lvcn01,rvcn01)

      ivcomp = ivcn01                                                   
      ivcorr = 8                                                        
40210 if (ivcomp - 8) 20210, 10210, 20210                               
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
!      ****  FCVS PROGRAM 328  -  TEST 022  ****                         
!                                                                        
!      USE A SUBROUTINE NAME AS AN ACTUAL ARGUMENT.                      
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      rvcomp = 0.0                                                      
      rvon01 = 3.5                                                      
      call fs334(fs335,rvon01,ivcn01,lvcn01,rvcn01)

      rvcomp = rvcn01                                                   
      rvcorr = 5.5                                                      
40220 if (rvcomp - 5.4995) 20220, 10220, 40221                          
40221 if (rvcomp - 5.5005) 10220, 10220, 20220                          
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
90001 format (" ",24x,"FM328")                                          
90000 format (" ",20x,"END OF PROGRAM FM328" )                          
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
      end program fm328
      subroutine fs329(idon01,rdon01,ldon01,ivcn01,lvcn01,rvcn01)
      integer :: ivcn01
      logical  :: lvcn01
      real :: rvcn01
      integer :: idon01
      real :: rdon01
      logical  :: ldon01
!           THIS SUBROUTINE IS USED BY VARIOUS TESTS IN THE MAIN PROGRAM 
!      FM328 TO TEST THE DIFFERENT FORMS OF INTEGER, REAL AND LOGICAL    
!      ACTUAL ARGUMENTS THAT CAN BE ASSOCIATED WITH INTEGER, REAL AND    
!      LOGICAL DUMMY ARGUMENTS.  THIS ROUTINE INCREMENTS THE INTEGER     
!      AND REAL ARGUMENTS BY ONE AND NEGATES THE LOGICAL ARGUMENT.  ALL  
!      RESULTS ARE THEN RETURNED TO FM328 VIA VARIABLES IN NAMED COMMON. 
      ivcn01 = idon01 + 1                                               
      rvcn01 = rdon01 + 1.0                                             
      lvcn01 = .not. ldon01                                             
      return                                                            
      end subroutine fs329
      integer function ff330(idon02)
      integer :: idon02
!          THIS FUNCTION IS USED BY TEST 011 OF THE MAIN PROGRAM FM328 TO
!      TEST THE USE OF AN EXTERNAL FUNCTION REFERENCE AS AN ACTUAL       
!      ARGUMENT WHEN THE ASSOCIATED DUMMY ARGUMENT IS A VARIABLE NAME.   
!      THIS FUNCTION IS ALSO REFERENCED FROM SUBROUTINE FS333 VIA A      
!      DUMMY PROCEDURE NAME REFERENCE.  THIS FUNCTION INCREMENTS THE     
!      ARGUMENT VALUE BY ONE AND RETURNS THE RESULT AS THE FUNCTION      
!      VALUE.                                                            
      ff330 = idon02 + 1                                                
      return                                                            
      end function ff330
      subroutine fs331(iddn11,ivcn01,lvcn01,rvcn01)
!           THIS SUBROUTINE IS USED BY VARIOUS TESTS IN THE MAIN PROGRAM 
!      FM328 TO TEST THE USE OF AN ARRAY NAME AS AN ACTUAL ARGUMENT WHEN 
!      THE ASSOCIATED DUMMY ARGUMENT IS AN ARRAY NAME.  THIS ROUTINE     
!      ADDS TOGETHER THE FOUR ELEMENTS IN THE DUMMY ARGUMENT ARRAY AND   
!      RETURNS THE RESULTS VIA A VARIABLE IN NAMED COMMON.               
      integer :: ivcn01
      real :: rvcn01
      logical :: lvcn01
      integer, dimension(1:4) :: iddn11
      ivcn01 = iddn11(1) + iddn11(2) + iddn11(3) + iddn11(4)            
      return                                                            
      end subroutine fs331
      subroutine fs332(rdtn21,ivcn01,lvcn01,rvcn01)
!           THIS SUBROUTINE IS USED BY VARIOUS TESTS IN THE MAIN PROGRAM 
!      FM328 TO TEST THE USE OF AN ARRAY ELEMENT NAME AS AN ACTUAL       
!      ARGUMENT WHEN THE ASSOCIATED DUMMY ARGUMENT IS AN ARRAY NAME.     
!      THIS ROUTINE ADDS TOGETHER THE FOUR ELEMENTS IN THE DUMMY         
!      ARGUMENT ARRAY AND RETURNS THE RESULT VIA A VARIABLE IN NAMED     
!      COMMON.                                                           
      integer :: ivcn01
      logical  :: lvcn01
      real :: rvcn01
      real, dimension(1:2,1:2) :: rdtn21
      rvcn01 = rdtn21(1,1) + rdtn21(2,1) + rdtn21(1,2) + rdtn21(2,2)    
      return                                                            
      end subroutine fs332
      subroutine fs333(nint,idon03,ivcn01,lvcn01,rvcn01)
      integer :: ivcn01
      logical  :: lvcn01
      real :: rvcn01
      integer, external :: nint
      integer :: idon03
!           THIS SUBROUTINE IS USED BY TESTS 020 AND 021 OF THE MAIN     
!      PROGRAM FM328 TO TEST THE USE OF EXTERNAL AND INTRINSIC FUNCTION  
!      NAMES AS ACTUAL ARGUMENTS WHEN THE ASSOCIATED DUMMY ARGUMENT IS A 
!      PROCEDURE NAME.  THIS SUBROUTINE REFERENCES THE EXTERNAL FUNCTION 
!      FF330 OR THE INTRINSIC FUNCTION IABS DEPENDING ON THE ACTUAL      
!      ARGUMENT PASSED TO IT.  THE RESULT OF THIS FUNCTION REFERENCE IS  
!      THEN INCREMENTED BY ONE AND THE RESULT IS RETURNED TO FS328 VIA   
!      A VARIABLE IN NAMED COMMON.                                       
      ivcn01 = nint(idon03) + 1                                         
!               **** THE NAME NINT IS A DUMMY ARGUMENT NAME              
!                      AND NOT AN INTRINSIC FUNCTION REFERENCE ****      
      return                                                            
      end subroutine fs333
      subroutine fs334(idon06,rdon03,ivcn01,lvcn01,rvcn01)
      integer :: ivcn01
      logical  :: lvcn01
      real :: rvcn01
!       integer :: idon06! EXTERNAL SUB !       integer :: idon06! EXTERNAL SUB
      real :: rdon03
!           THIS SUBROUTINE IS USED BY TEST 022 OF THE MAIN PROGRAM      
!      FM328 TO TEST THE USE OF A SUBROUTINE NAME AS AN ACTUAL ARGUMENT  
!      WHEN THE ASSOCIATED DUMMY ARGUMENT IS A PROCEDURE NAME.  THIS     
!      SUBROUTINE CALLS THE SUBROUTINE FS335 VIA A DUMMY PROCEDURE NAME  
!      REFERENCE.  THE ARGUMENT VALUE WHICH IS RETURNED FROM THE FS335   
!      REFERENCE IS THEN INCREMENTED BY ONE AND RETURNED TO FM328 VIA    
!      A VARIABLE IN NAMED COMMON.                                       
      call idon06(rdon03)                                               
      rvcn01 = rdon03 + 1.0                                             
      return                                                            
      end subroutine fs334
      subroutine fs335(rdon04)
      real :: rdon04
!           THIS SUBROUITNE IS USED BY TEST 022 OF THE MAIN PROGRAM FM328
!      TO TEST THE USE OF A SUBROUTINE NAME AS AN ACTUAL ARGUMENT WHEN   
!      THE ASSOCIATED DUMMY ARGUMENT IS A PROCEDURE NAME.  FS335 IS      
!      CALLED FROM SUBROUTINE FS334 VIA A DUMMY PROCEDURE NAME REFERENCE.
!      THIS ROUTINE INCREMENTS THE ARGUMENT VALUE BY ONE.                
      rdon04 = rdon04 + 1.0                                             
      return                                                            
      end subroutine fs335
