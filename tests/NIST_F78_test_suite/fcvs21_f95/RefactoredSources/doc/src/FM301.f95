      program fm301
!                                                                        
!                                                                        
!         FM301 TESTS THE USE OF THE TYPE-STATEMENT TO EXPLICITLY        
!      DEFINE THE DATA TYPE FOR VARIABLES, ARRAYS, AND STATEMENT         
!      FUNCTIONS.  ONLY INTEGER, REAL, LOGICAL AND CHARACTER DATA        
!      TYPES ARE TESTED IN THIS ROUTINE.  INTEGER AND REAL VARIABLES     
!      AND ARRAYS ARE TESTED IN A MANNER WHICH BOTH CONFIRMS AND         
!      OVERRIDES THE IMPLICIT TYPING OF THE DATA ENTITIES.               
!                                                                        
!         FM301 DOES NOT ATTEMPT TO TEST ALL OF THE ELEMENTARY SYNTAX    
!      FORMS OF THE TYPE-STATEMENT.  THESE FORMS ARE TESTED ADEQUATELY   
!      WITHIN THE BOILER PLATE AND OTHER AUDIT PROGRAMS.                 
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 4.1, DATA TYPES                                        
!         SECTION 8.4, TYPE-STATEMENT                                    
!         SECTION 8.5, IMPLICIT STATEMENT                                
!         SECTION 15.4, STATEMENT FUNCTION                               
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
!      *** IMPLICIT STATEMENT FOR TEST 006 ***                           
!                                                                        
!                                                                        
!      *** IMPLICIT STATEMENT FOR TEST 017 ***                           
!                                                                        
!                                                                        
!      *** IMPLICIT STATEMENT FOR TEST 018 ***                           
!                                                                        
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 001 ***                     
!                                                                        
      integer :: idon01
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
      character(14 ) :: cvcomp
      character(14 ) :: cvcorr
      integer  :: go
      real :: to
      integer :: ivon01
      integer :: ivon02
      integer :: avtn01
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 002 ***                     
!                                                                        
      real :: kvtn01
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 003 ***                     
!                                                                        
      integer :: kvtn02
      integer :: avtn02
      integer :: kvtn03
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 004 ***                     
!                                                                        
      real :: avtn03
      real :: avtn04
      real :: kvtn04
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 005 ***                     
!                                                                        
      logical :: hvtn01
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 006 ***                     
!         (ALSO SEE THE IMPLICIT STATEMENTS FOR TEST 006)                
!                                                                        
      real :: mvtn01
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 007 ***                     
!                                                                        
      integer, dimension(1:4) :: nvtn11
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 008 ***                     
!                                                                        
      real, dimension(1:2,1:2) :: nvtn22
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TESTS 009 AND 010 ***            
!                                                                        
      integer, dimension(1:3,1:3,1:3) :: nvtn33
      integer, dimension(1:5) :: avtn15
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 011 ***                     
!                                                                        
      integer, dimension(1:5) :: nvtn14
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 012 ***                     
!                                                                        
      integer, dimension(1:4) :: avtn16
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TESTS 013 AND 014 ***            
!                                                                        
      character(len=14) :: cvtn01
      character(len=14), dimension(1:4) :: catn12
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 015 ***                     
!                                                                        
      character(len=14), dimension(1:6) :: cadn13
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 016 ***                     
!                                                                        
      character :: kvtn05
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 017 ***                     
!         (ALSO SEE THE IMPLICIT STATEMENT FOR TEST 017)                 
!                                                                        
      character(len=3) :: gvtn01
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 018 ***                     
!         (ALSO SEE THE IMPLICIT STATEMENT FOR TEST 018)                 
!                                                                        
      character(len=3) :: fvtn01
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 019 ***                     
!                                                                        
      integer :: iftn01
      iftn01(idon01) = idon01 + 1                                       
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
!      ****  FCVS PROGRAM 301  -  TEST 001  ****                         
!                                                                        
!          TEST 001 DEFINES AN INTEGER VARIABLE OVERRIDING THE IMPLICIT  
!      COMPILER DEFAULT TYPE SPECIFYING REAL.                            
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      avtn01 = 100                                                      
      ivcorr = 100                                                      
      ivcomp = avtn01                                                   
40010 if (ivcomp - 100) 20010, 10010, 20010                             
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
!      ****  FCVS PROGRAM 301  -  TEST 002  ****                         
!                                                                        
!          TEST 002 DEFINES A REAL VARIABLE OVERRIDING THE IMPLICIT      
!      COMPILER DEFAULT TYPE SPECIFYING INTEGER.                         
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvcomp = 0.0                                                      
      kvtn01 = 1.004                                                    
      rvcorr = 1.004                                                    
      rvcomp = kvtn01                                                   
40020 if (rvcomp - 1.0035) 20020, 10020, 40021                          
40021 if (rvcomp - 1.0045) 10020, 10020, 20020                          
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
!      ****  FCVS PROGRAM 301  -  TEST 003  ****                         
!                                                                        
!          TEST 003 DEFINES A SERIES OF INTEGER VARIABLES IN ONE TYPE-   
!      STATEMENT.  TWO VARIABLES CONFIRM THE IMPLICIT INTEGER TYPING.    
!      THE OTHER VARIABLE OVERRIDES THE IMPLICIT TYPING.                 
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 0                                                        
      kvtn02 = 20                                                       
      kvtn03 = 30                                                       
      avtn02 = 200                                                      
      ivcorr = 20                                                       
      ivcomp = kvtn02                                                   
40030 if (ivcomp - 20) 20030, 40031, 20030                              
40031 ivcorr = 30                                                       
      ivcomp = kvtn03                                                   
40033 if (ivcomp - 30) 20030, 40034, 20030                              
40034 ivcorr = 200                                                      
      ivcomp = avtn02                                                   
40035 if (ivcomp - 200) 20030, 10030, 20030                             
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
!      ****  FCVS PROGRAM 301  -  TEST 004  ****                         
!                                                                        
!          TEST 004 DEFINES A SERIES OF REAL VARIABLES IN ONE TYPE-      
!      STATEMENT.  TWO VARIABLES CONFIRM THE IMPLICIT REAL TYPING.  THE  
!      THIRD VARIABLE OVERRIDES THE IMPLICIT TYPING.                     
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      rvcomp = 0.0                                                      
      avtn03 = 3.0                                                      
      avtn04 = 4.                                                       
      kvtn04 = .4                                                       
      rvcorr = 3.0                                                      
      rvcomp = avtn03                                                   
40040 if (rvcomp - 2.9995) 20040, 40042, 40041                          
40041 if (rvcomp - 3.0005) 40042, 40042, 20040                          
40042 rvcorr = 4.                                                       
      rvcomp = avtn04                                                   
40043 if (rvcomp - 3.9995) 20040, 40045, 40044                          
40044 if (rvcomp - 4.0005) 40045, 40045, 20040                          
40045 rvcorr = .4                                                       
      rvcomp = kvtn04                                                   
40046 if (rvcomp - .39995) 20040, 10040, 40047                          
40047 if (rvcomp - .40005) 10040, 10040, 20040                          
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
!      ****  FCVS PROGRAM 301  -  TEST 005  ****                         
!                                                                        
!          TEST 005 DEFINES A LOGICAL VARIABLE.                          
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      hvtn01 = .true.                                                   
      ivcorr = 1                                                        
      ivcomp = 0                                                        
      if (hvtn01) ivcomp = 1                                            
40050 if (ivcomp - 1) 20050, 10050, 20050                               
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
!      ****  FCVS PROGRAM 301  -  TEST 006  ****                         
!                                                                        
!          TEST 006 DEFINES A REAL VARIABLE WITH A TYPE-STATEMENT THAT   
!      OVERRIDES THE IMPLICIT STATEMENT TYPING OF THE INTEGER LETTER 'M' 
!      AS LOGICAL.                                                       
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      rvcomp = 0.0                                                      
      mvtn01 = 12.345                                                   
      rvcorr = 12.345                                                   
      rvcomp = mvtn01                                                   
40060 if (rvcomp - 12.340) 20060, 10060, 40061                          
40061 if (rvcomp - 12.350) 10060, 10060, 20060                          
30060 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10060, 0071, 20060                                    
10060 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0071                                                        
20060 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0071 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 007  ****                         
!                                                                        
!          TEST 007 DEFINES A ONE DIMENSIONAL INTEGER ARRAY.             
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      nvtn11(3) = 3                                                     
      ivcorr = 3                                                        
      ivcomp = nvtn11(3)                                                
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
!      ****  FCVS PROGRAM 301  -  TEST 008  ****                         
!                                                                        
!          TEST 008 DEFINES A TWO DIMENSIONAL REAL ARRAY THAT OVERRIDES  
!      THE IMPLICIT TYPING OF INTEGER.                                   
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      rvcomp = 0.0                                                      
      nvtn22(1,2) = 2.12                                                
      rvcorr = 2.12                                                     
      rvcomp = nvtn22(1,2)                                              
40080 if (rvcomp - 2.1195) 20080, 10080, 40081                          
40081 if (rvcomp - 2.1205) 10080, 10080, 20080                          
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
!      ****  FCVS PROGRAM 301  -  TEST 009  ****                         
!                                                                        
!          TEST 009 DEFINES TWO INTEGER ARRAYS WITH ONE TYPE-STATEMENT.  
!      ONE ARRAY IS THREE DIMENSIONAL WHILE THE OTHER ARRAY OVERRIDES    
!      THE IMPLICIT TYPING OF REAL.  ONLY THE THREE DIMENSIONAL ARRAY    
!      IS CHECKED IN THIS TEST.                                          
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      nvtn33(1,2,3) = 123                                               
      ivcorr = 123                                                      
      ivcomp = nvtn33(1,2,3)                                            
40090 if (ivcomp - 123) 20090, 10090, 20090                             
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
!      ****  FCVS PROGRAM 301  -  TEST 010  ****                         
!                                                                        
!          TEST 010 CHECKS THE SECOND ARRAY DESCRIBED IN THE PREVIOUS    
!      TEST.                                                             
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      avtn15(2) = 5                                                     
      ivcorr = 5                                                        
      ivcomp = avtn15(2)                                                
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
!      ****  FCVS PROGRAM 301  -  TEST 011  ****                         
!                                                                        
!          TEST 011 USES THE TYPE-STATEMENT TO EXPLICITLY TYPE AN ARRAY  
!      THAT WAS DEFINED WITH A DIMENSION STATEMENT.                      
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 0                                                        
      nvtn14(5) = 5                                                     
      ivcorr = 5                                                        
      ivcomp = nvtn14(5)                                                
40110 if (ivcomp - 5) 20110, 10110, 20110                               
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
!      ****  FCVS PROGRAM 301  -  TEST 012  ****                         
!                                                                        
!          TEST 012 USES THE TYPE-STATEMENT TO OVERRIDE THE TYPING OF    
!      AN ARRAY THAT WAS DEFINED WITH A DIMENSION STATEMENT.             
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 0                                                        
      avtn16(3) = 163                                                   
      ivcorr = 163                                                      
      ivcomp = avtn16(3)                                                
40120 if (ivcomp - 163) 20120, 10120, 20120                             
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
!      ****  FCVS PROGRAM 301  -  TEST 013  ****                         
!                                                                        
!          TEST 013 USES ONE CHARACTER TYPE-STATEMENT TO SPECIFY BOTH A  
!      VARIABLE AND AN ARRAY DECLARATOR.  ONLY THE VARIABLE IS CHECKED   
!      IN THIS TEST.                                                     
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      cvtn01 = '12345678901234'                                         
      cvcomp = '              '                                         
      cvcorr = '12345678901234'                                         
      cvcomp = cvtn01                                                   
40130 if (cvcomp  ==  '12345678901234') goto 10130                     
40131 go to 20130                                                       
30130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10130, 0141, 20130                                    
10130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0141                                                        
20130 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0141 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 014  ****                         
!                                                                        
!          TEST 014 CHECKS THE ARRAY DECLARATOR FROM THE PREVIOUS TEST.  
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      cvcomp = '              '                                         
      catn12(2) = 'ABCDEFGHIJKLMN'                                      
      cvcorr = 'ABCDEFGHIJKLMN'                                         
      cvcomp = catn12(2)                                                
40140 if (cvcomp  ==  'ABCDEFGHIJKLMN') goto 10140                     
40141 go to 20140                                                       
30140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10140, 0151, 20140                                    
10140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0151                                                        
20140 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0151 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 015  ****                         
!                                                                        
!          TEST 015 USES THE CHARACTER TYPE-STATEMENT TO SPECIFY AN      
!      ARRAY-NAME.  THE ARRAY IS DECLARED IN A DIMENSION STATEMENT.      
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      cvcomp = '              '                                         
      cadn13(3) = '12345678901234'                                      
      cvcorr = '12345678901234'                                         
      cvcomp = cadn13(3)                                                
40150 if (cvcomp  ==  '12345678901234') goto 10150                     
40151 go to 20150                                                       
30150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10150, 0161, 20150                                    
10150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0161                                                        
20150 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0161 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 016  ****                         
!                                                                        
!          TEST 016 USES THE CHARACTER TYPE-STATEMENT TO OVERRIDE THE    
!      IMPLICIT (DEFAULT) TYPING OF INTEGER.                             
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      cvcomp = '   '                                                    
      kvtn05 = 'A'                                                      
      cvcorr = 'A'                                                      
      cvcomp = kvtn05                                                   
40160 if (cvcomp  ==  'A') goto 10160                                  
40161 go to 20160                                                       
30160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10160, 0171, 20160                                    
10160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0171                                                        
20160 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0171 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 017  ****                         
!                                                                        
!          TEST 017 USES THE CHARACTER TYPE-STATEMENT TO OVERRIDE THE    
!      IMPLICIT TYPING OF THE LETTER 'G' AS INTEGER.                     
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      cvcomp = '   '                                                    
      gvtn01 = 'ABC'                                                    
      cvcorr = 'ABC'                                                    
      cvcomp = gvtn01                                                   
40170 if (cvcomp  ==  'ABC') goto 10170                                
40171 go to 20170                                                       
30170 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10170, 0181, 20170                                    
10170 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0181                                                        
20170 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0181 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 018  ****                         
!                                                                        
!          TEST 018 USES THE CHARAACTER TYPE-STATEMENT TO OVERRIDE THE   
!      LENGTH OF A CHARACTER FIELD DEFINED BY AN IMPLICIT STATEMENT.     
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      cvcomp = '   '                                                    
      fvtn01 = 'ABC'                                                    
      cvcorr = 'ABC'                                                    
      cvcomp = fvtn01                                                   
40180 if (cvcomp  ==  'ABC') goto 10180                                
40181 go to 20180                                                       
30180 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10180, 0191, 20180                                    
10180 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0191                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0191 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 301  -  TEST 019  ****                         
!                                                                        
!          TEST 019 USES THE TYPE-STATEMENT TO SPECIFY AN INTEGER        
!      STATEMENT FUNCTION.                                               
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 5                                                        
      ivon02 = iftn01(ivon01)                                           
      ivcorr = 6                                                        
      ivcomp = ivon02                                                   
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
90001 format (" ",24x,"FM301")                                          
90000 format (" ",20x,"END OF PROGRAM FM301" )                          
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
      end program fm301
