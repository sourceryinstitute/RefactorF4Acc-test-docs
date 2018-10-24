      program fm300
!                                                                        
!                                                                        
!         THIS ROUTINE TESTS THE USE OF THE EQUIVALENCE STATEMENT TO     
!      EQUATE STORAGE UNITS OF VARIABLES, ARRAYS AND ARRAY ELEMENTS.     
!      ONLY INTEGER, REAL, LOGICAL AND CHARACTER DATA TYPES ARE TESTED.  
!      NO ATTEMPT IS MADE TO TEST DATA OF DIFFERENT TYPES THAT ARE       
!      EQUATED WITH THE EQUIVALENCE STATEMENT.                           
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 8.1, DIMENSION STATEMENT                               
!         SECTION 8.2, EQUIVALENCE STATEMENT                             
!         SECTION 9, DATA STATEMENT                                      
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
!      *** SPECIFICATION STATEMENTS FOR TEST 001 ***                     
!                                                                        
      equivalence (ivoe01, ivoe02)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 002 ***                     
!                                                                        
      equivalence (rvoe01, rvoe02)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 003 ***                     
!                                                                        
      equivalence (lvoe01, lvoe02)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 004 ***                     
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivoe01
      integer :: ivoe02
      integer :: ivcorr
      real :: rvcomp
      real :: rvoe01
      real :: rvoe02
      real :: rvcorr
      logical  :: lvoe01
      logical  :: lvoe02
      character(14 ) :: cvcorr
      real :: go
      real :: to
      integer :: ivoe03
      integer :: ivoe04
      integer :: ivoe05
      real :: rvoe03
      integer :: ivoe06
      integer :: ivoe07
      integer :: ivoe08
      integer :: ivoe09
      integer :: ivoe10
      integer :: ivoe11
      integer :: ivoe12
      integer :: ivoe13
      integer :: ivoe14
      integer :: ivoe15
      integer :: ivoe16
      integer :: ivoe17
      integer :: ivoe18
      integer :: ivoe20
      character(len=3) :: cvte01
      character(len=3) :: cvte02
      character(len=3) :: cvcomp
      equivalence (cvte01, cvte02)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 005 ***                     
!                                                                        
      equivalence (ivoe03, ivoe04, ivoe05)                              
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 006 ***                     
!                                                                        
      equivalence (ivoe06, ivoe07, rvoe03)                              
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TESTS 007 AND 008 ***            
!                                                                        
      equivalence (ivoe08, ivoe09), (ivoe10, ivoe11)                    
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 009 ***                     
!                                                                        
      equivalence (ivoe12, ivoe13), (ivoe13, ivoe14)                    
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 010 ***                     
!                                                                        
      equivalence (ivoe15, ivoe16)                                      
      equivalence (ivoe16, ivoe17)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TESTS 011 AND 012 ***            
!                                                                        
      integer, dimension(1:2) :: iade11
      integer, dimension(1:3) :: iade12
      equivalence (iade11, iade12)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TESTS 013 AND 014 ***            
!                                                                        
      real, dimension(1:5) :: rade11
      real, dimension(1:5) :: rade12
      equivalence (rade11(4), rade12(2))                                
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 015 ***                     
!                                                                        
      integer, dimension(1:4) :: iade13
      integer, dimension(1:4) :: iade14
      equivalence (iade13, iade14(3))                                   
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 016 ***                     
!                                                                        
      integer, dimension(1:3) :: iade15
      equivalence (iade15(2), ivoe18)                                   
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TESTS 017 AND 018 ***            
!                                                                        
      integer, dimension(1:2,1:2) :: iade21
      integer, dimension(1:4) :: iade16
      equivalence (iade21, iade16)                                      
!                                                                        
!      *** SPECIFICATION STATEMENTS FOR TEST 019 ***                     
!                                                                        
      equivalence (ivoe19, ivoe20)                                      
      data ivoe19 / 19 / 
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
!      ****  FCVS PROGRAM 300  -  TEST 001  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO INTEGER VARIABLES.             
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      ivoe01 = 5                                                        
      ivoe02 = 7                                                        
      ivcorr = 7                                                        
      ivcomp = ivoe01                                                   
40010 if (ivcomp - 7) 20010,10010,20010                                 
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
!      ****  FCVS PROGRAM 300  -  TEST 002  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO REAL VARIABLES.                
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvcomp = 0.0                                                      
      rvoe01 = 4.5                                                      
      rvoe02 = 1.2                                                      
      rvcorr = 1.2                                                      
      rvcomp = rvoe01                                                   
40020 if (rvcomp - 1.1995) 20020,10020,40021                            
40021 if (rvcomp - 1.2005) 10020,10020,20020                            
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
!      ****  FCVS PROGRAM 300  -  TEST 003  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO LOGICAL VARIABLES.             
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      lvoe01 = .true.                                                   
      lvoe02 = .false.                                                  
      ivcorr = 0                                                        
      ivcomp = 0                                                        
      if (lvoe01) ivcomp = 1                                            
40030 if (ivcomp) 20030,10030,20030                                     
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
!      ****  FCVS PROGRAM 300  -  TEST 004  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO CHARACTER VARIABLES.           
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      cvcomp = '   '                                                    
      cvte01 = 'ABC'                                                    
      cvte02 = 'DEF'                                                    
      cvcorr = 'DEF'                                                    
      cvcomp = cvte01                                                   
40040 if (cvcomp  ==  'DEF') goto 10040                                
40041 go to 20040                                                       
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10040, 0051, 20040                                    
10040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0051                                                        
20040 ivfail = ivfail + 1                                               
      write (i02,80018) ivtnum, cvcomp, cvcorr                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 300  -  TEST 005  ****                         
!                                                                        
!      THIS IS A TEST FOR EQUATING THREE INTEGER VARIABLES.              
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 0                                                        
      ivoe03 = 3                                                        
      ivoe04 = 4                                                        
      ivoe05 = 5                                                        
      ivcorr = 5                                                        
      ivcomp = ivoe03                                                   
40050 if (ivcomp - 5) 20050,40051,20050                                 
40051 ivcomp = ivoe04                                                   
40052 if (ivcomp - 5) 20050,10050,20050                                 
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
!      ****  FCVS PROGRAM 300  -  TEST 006  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO INTEGER VARIABLES AND ONE      
!      REAL VARIABLE WITHIN ONE EQUIVALENCE STATEMENT LIST OF NAMES.  THE
!      VALUE OF THE REAL VARIABLE IS NOT TESTED.                         
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 0                                                        
      rvoe03 = 3.445                                                    
      ivoe06 = 6                                                        
      ivoe07 = 7                                                        
      ivcorr = 7                                                        
      ivcomp = ivoe06                                                   
40060 if (ivcomp - 7) 20060,10060,20060                                 
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
!      ****  FCVS PROGRAM 300  -  TEST 007  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING INTEGER VARIABLES USING TWO LISTS  
!      OF NAMES IN ONE EQUIVALENCE STATEMENT.  NAMES SPECIFIED IN THE    
!      FIRST LIST ARE NOT EQUATED TO NAMES IN THE SECOND LIST.  THIS     
!      TEST CHECKS THE EQUIVALINCE OF THE VARIABLES IN THE FIRST LIST.   
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      ivoe08 = 8                                                        
      ivoe09 = 9                                                        
      ivoe10 = 10                                                       
      ivoe11 = 11                                                       
      ivcorr = 9                                                        
      ivcomp = ivoe08                                                   
40070 if (ivcomp - 9) 20070,10070,20070                                 
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
!      ****  FCVS PROGRAM 300  -  TEST 008  ****                         
!                                                                        
!         THIS TEST CHECKS THE EQUIVALENCE OF THE VARIABLES IN THE       
!      SECOND LIST.                                                      
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 11                                                       
      ivcomp = ivoe10                                                   
40080 if (ivcomp - 11) 20080,10080,20080                                
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
!      ****  FCVS PROGRAM 300  -  TEST 009  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING INTEGER VARIABLES IN ONE LIST      
!      WITH INTEGER VARIABLES IN A SECOND LIST OF THE SAME EQUIVALENCE   
!      STATEMENT.  ALL VARIABLES SHOULD BE EQUATED AND SHARE THE SAME    
!      STORAGE UNIT.                                                     
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      ivoe12 = 12                                                       
      ivoe13 = 13                                                       
      ivoe14 = 14                                                       
      ivcorr = 14                                                       
      ivcomp = ivoe13                                                   
40090 if (ivcomp - 14) 20090,40091,20090                                
40091 ivcomp = ivoe12                                                   
40092 if (ivcomp - 14) 20090,10090,20090                                
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
!      ****  FCVS PROGRAM 300  -  TEST 010  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING INTEGER VARIABLES SPECIFIED IN ONE 
!      EQUIVALENCE STATEMENT WITH INTEGER VARIABLES SPECIFIED IN A       
!      SECOND EQUIVALENCE STATEMENT.  ONE VARIABLE IS SPECIFIED IN BOTH  
!      STATEMENTS, THEREFORE ALL VARIABLES SHOULD BE EQUATED AND SHARE   
!      THE SAME STORAGE UNIT.                                            
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      ivoe15 = 15                                                       
      ivoe16 = 16                                                       
      ivoe17 = 17                                                       
      ivcorr = 17                                                       
      ivcomp = ivoe16                                                   
40100 if (ivcomp - 17) 20100,40101,20100                                
40101 ivcomp = ivoe15                                                   
40102 if (ivcomp - 17) 20100,10100,20100                                
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
!      ****  FCVS PROGRAM 300  -  TEST 011  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO INTEGER ARRAYS UNQUALIFIED     
!      BY A SUBSCRIPT IN THE EQUIVALENCE STATEMENT.  ALL ARRAY ELEMENTS  
!      SPECIFIED BY THE SAME SUBSCRIPT VALUE, BEGINNING WITH THE FIRST   
!      ARRAY ELEMENT, SHOULD BE EQUATED AND SHARE THE SAME STORAGE UNIT. 
!      THIS TEST CHECKS THE EQUIVALENCE OF THE FIRST ARRAY ELEMENTS.     
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 0                                                        
      iade11(1) = 111                                                   
      iade11(2) = 112                                                   
      iade12(1) = 121                                                   
      iade12(2) = 122                                                   
      iade12(3) = 123                                                   
      ivcorr = 121                                                      
      ivcomp = iade11(1)                                                
40110 if (ivcomp - 121) 20110,10110,20110                               
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
!      ****  FCVS PROGRAM 300  -  TEST 012  ****                         
!                                                                        
!         THIS TEST CHECKS THE EQUIVALENCE OF THE SECOND ARRAY ELEMENTS. 
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 122                                                      
      ivcomp = iade11(2)                                                
40120 if (ivcomp - 122) 20120,10120,20120                               
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
!      ****  FCVS PROGRAM 300  -  TEST 013  ****                         
!                                                                        
!         THIS IS A TEST FOR EQUATING TWO REAL ARRAY ELEMENTS.  THIS     
!      TEST CHECKS THE EQUIVALENCE OF THE TWO ARRAY ELEMENTS SPECIFIED   
!      IN THE EQUIVALENCE STATEMENT.                                     
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvcomp = 0.0                                                      
      rade11(4) = 11.4                                                  
      rade12(2) = 1.22                                                  
      rvcorr = 1.22                                                     
      rvcomp = rade11(4)                                                
40130 if (rvcomp - 1.2195) 20130,10130,40131                            
40131 if (rvcomp - 1.2205) 10130,10130,20130                            
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
!      ****  FCVS PROGRAM 300  -  TEST 014  ****                         
!                                                                        
!         THIS TEST CHECKS THE EQUIVALENCE OF THE ARRAY ELEMENTS         
!      WITH A SUBSCRIPT VALUE ONE LESS THAN THOSE TESTED IN THE          
!      PREVIOUS TEST.  THESE ELEMENTS SHOULD BE EQUATED AND SHARE THE    
!      SAME STORAGE UNIT DUE TO THE WAY ARRAY ELEMENTS OCCUPY            
!      CONSECUTIVE STORAGE UNITS.                                        
!                                                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      rvcomp = 0.0                                                      
      rade11(3) = .113                                                  
      rade12(1) = 122.                                                  
      rvcorr = 122.                                                     
      rvcomp = rade11(3)                                                
40140 if (rvcomp - 121.95) 20140,10140,40141                            
40141 if (rvcomp - 122.05) 10140,10140,20140                            
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
!      ****  FCVS PROGRAM 300  -  TEST 015  ****                         
!                                                                        
!         THIS IS A TEST TO EQUATE AN ARRAY NAME TO AN ARRAY ELEMENT     
!      NAME.                                                             
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 0                                                        
      iade13(1) = 131                                                   
      iade14(3) = 143                                                   
      ivcorr = 143                                                      
      ivcomp = iade13(1)                                                
40150 if (ivcomp - 143) 20150,10150,20150                               
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
!      ****  FCVS PROGRAM 300  -  TEST 016  ****                         
!                                                                        
!         THIS IS A TEST TO EQUATE AN ARRAY ELEMENT TO AN INTEGER        
!      VARIABLE.                                                         
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 0                                                        
      iade15(2) = 152                                                   
      ivoe18 = 18                                                       
      ivcorr = 18                                                       
      ivcomp = iade15(2)                                                
40160 if (ivcomp - 18) 20160,10160,20160                                
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
!      ****  FCVS PROGRAM 300  -  TEST 017  ****                         
!                                                                        
!         THIS IS A TEST TO EQUATE A ONE DIMENSIONAL ARRAY TO A TWO      
!      DIMENSIONAL ARRAY.  THIS TEST CHECKS THE SECOND ARRAY ELEMENTS.   
!                                                                        
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 0                                                        
      iade21(2,1) = 212                                                 
      iade16(2) = 162                                                   
      ivcorr = 162                                                      
      ivcomp = iade21(2,1)                                              
40170 if (ivcomp - 162) 20170,10170,20170                               
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
!      ****  FCVS PROGRAM 300  -  TEST 018  ****                         
!                                                                        
!         THIS TEST CHECKS THE THIRD ARRAY ELEMENTS FROM THE PREVIOUS    
!      TEST.                                                             
!                                                                        
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 0                                                        
      iade21(1,2) = 2112                                                
      iade16(3) = 163                                                   
      ivcorr = 163                                                      
      ivcomp = iade21(1,2)                                              
40180 if (ivcomp - 163) 20180,10180,20180                               
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
!      ****  FCVS PROGRAM 300  -  TEST 019  ****                         
!                                                                        
!         THIS IS A TEST TO EQUATE TWO INTEGER VARIABLES ONE OF WHICH    
!      IS INITIALIZED IN A DATA STATEMENT.                               
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 19                                                       
      ivcomp = ivoe20                                                   
40190 if (ivcomp - 19) 20190,10190,20190                                
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
90001 format (" ",24x,"FM300")                                          
90000 format (" ",20x,"END OF PROGRAM FM300" )                          
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
      end program fm300
