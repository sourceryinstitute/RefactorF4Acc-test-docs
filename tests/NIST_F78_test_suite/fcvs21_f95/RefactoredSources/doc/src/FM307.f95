      program fm307
!                                                                        
!                                                                        
!           THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE FUNCTION    
!      TYPE IS REAL AND THE ARGUMENTS ARE EITHER INTEGER OR REAL.  THE   
!      FUNCTION NINT IS AN EXCEPTION AND HAS AN INTEGER FUNCTION TYPE.   
!      THE REAL OR INTEGER ARGUMENTS CONSIST OF POSITIVE, NEGATIVE AND   
!      UNSIGNED CONSTANTS, VARIABLES AND ARRAY ELEMENT VALUES.  EACH     
!      INTRINSIC FUNCTION IS TESTED WITH THREE OR FOUR DIFFERENT         
!      COMBINATIONS OF ACTUAL ARGUMENTS DESIGNED TO TEST NOT ONLY THE    
!      VARIOUS COMBINATIONS OF DATA USAGES BUT ALSO TO TEST THE RANGE OF 
!      ARGUMENT AND FUNCTION VALUES, WHERE THAT IS APPROPRIATE.  THE     
!      INTRINSIC FUNCTIONS TESTED IN THIS ROUTINE INCLUDE.               
!                                                                        
!                                         SPECIFIC        TYPE  OF       
!           INTRINSIC FUNCTION            NAME        ARGUMENT   FUNCTION
!           ------------------            ------      --------   --------
!           CONVERSION TO REAL            REAL        INTEGER    REAL    
!           NEAREST WHOLE NUMBER          ANINT       REAL       REAL    
!           NEAREST INTEGER               NINT        REAL       INTEGER 
!           TANGENT                       TAN         REAL       REAL    
!           ARCSINE                       ASIN        REAL       REAL    
!           ARCCOSINE                     ACOS        REAL       REAL    
!           HYPERBOLIC SINE               SINH        REAL       REAL    
!           HYPERBOLIC COSINE             COSH        REAL       REAL    
!                                                                        
!           SUBSET LEVEL ROUTINES FM097 THROUGH FM099 AND FM308 ALSO     
!      TEST THE USE OF INTEGER AND REAL INTRINSIC FUNCTIONS.             
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 15.3,     INTRINSIC FUNCTIONS                          
!         SECTION 15.9.2,   ACTUAL ARGUMENTS                             
!         SECTION 15.9.3,   ASSOCIATION OF DUMMY AND ACTUAL ARGUMENTS    
!         TABLE 5,          INTRINSIC FUNCTIONS (INCLUDING NOTES)        
!         SECTION 15.10.1,  RESTRICTION ON RANGE OF ARGUMENTS AND RESULTS
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
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvcomp
      real :: rvcorr
      integer :: ivon01
      real :: rvon01
      integer :: ivcomp
      integer :: ivcorr
      real :: pi
      integer, dimension(1:4) :: iaon11
      real, dimension(1:4) :: raon11
      data pi / 3.141592654 / 
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
!      TEST 001 THROUGH TEST 004 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      TYPE CONVERSION TO REAL (REAL) WHERE THE FUNCTION IS REAL AND THE 
!      ARGUMENT IS INTEGER.                                              
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 001  ****                         
!                                                                        
!      CONSTANT ARGUMENT                                                 
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = real (6)                                                 
      rvcorr = 6.0                                                      
40010 if (rvcomp - 5.9995) 20010,10010,40011                            
40011 if (rvcomp - 6.0005) 10010,10010,20010                            
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
!      ****  FCVS PROGRAM 307  -  TEST 002  ****                         
!                                                                        
!      VARIABLE ARGUMENT                                                 
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      rvcomp = 10.0                                                     
      ivon01 = 6                                                        
      rvcomp = real (ivon01)                                            
      rvcorr = 6.0                                                      
40020 if (rvcomp - 5.9995) 20020,10020,40021                            
40021 if (rvcomp - 6.0005) 10020, 10020, 20020                          
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
!      ****  FCVS PROGRAM 307  -  TEST 003  ****                         
!                                                                        
!      ARRAY ELEMENT NAME ARGUMENT                                       
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      rvcomp = 10.0                                                     
      iaon11(3) = 6                                                     
      rvcomp = real (iaon11(3))                                         
      rvcorr = 6.0                                                      
40030 if (rvcomp - 5.9995) 20030, 10030, 40031                          
40031 if (rvcomp - 6.0005) 10030, 10030, 20030                          
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
!      ****  FCVS PROGRAM 307  -  TEST 004  ****                         
!                                                                        
!      EXPRESSION AS ARGUMENT                                            
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      rvcomp = 10.0                                                     
      ivon01 = 6                                                        
      rvcomp = real (ivon01 - 6)                                        
      rvcorr = 0.0                                                      
40040 if(rvcomp + .00005) 20040, 10040, 40041                           
40041 if(rvcomp - .00005) 10040, 10040, 20040                           
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
!      TEST 005 THROUGH TEST 008 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE NEAREST WHOLE NUMBER (ANINT) WHERE THE FUNCTION AND   
!      ARGUMENT TYPES ARE BOTH REAL.                                     
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 005  ****                         
!                                                                        
!      CONSTANT ARGUMENT                                                 
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = anint (3.4994)                                           
      rvcorr = 3.0                                                      
40050 if (rvcomp - 2.9995) 20050, 10050, 40051                          
40051 if (rvcomp - 3.0005) 10050, 10050, 20050                          
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
!      ****  FCVS PROGRAM 307  -  TEST 006  ****                         
!                                                                        
!      VARIABLE ARGUMENT                                                 
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = -3.4994                                                  
      rvcomp = anint (rvon01)                                           
      rvcorr = -3.0                                                     
40060 if (rvcomp + 3.0005) 20060, 10060, 40061                          
40061 if (rvcomp + 2.9995) 10060, 10060, 20060                          
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
!      ****  FCVS PROGRAM 307  -  TEST 007  ****                         
!                                                                        
!      ARRAY ELEMENT NAME ARGUMENT                                       
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      rvcomp = 10.0                                                     
      raon11(3) = 3.0000                                                
      rvcomp = anint (raon11(3))                                        
      rvcorr = 3.0                                                      
40070 if (rvcomp - 2.9995) 20070, 10070, 40071                          
40071 if (rvcomp - 3.0005) 10070, 10070, 20070                          
30070 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10070, 0081, 20070                                    
10070 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0081                                                        
20070 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0081 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 008  ****                         
!                                                                        
!      ZERO ARGUMENT                                                     
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = anint (0.0)                                              
      rvcorr = 0.0                                                      
40080 if (rvcomp) 20080, 10080, 20080                                   
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
!      TEST 009 THROUGH TEST 012 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE NEAREST INTEGER (NINT) WHERE THE ARGUMENT IS REAL     
!      AND THE FUNCTION TYPE IS INTEGER.                                 
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 009  ****                         
!                                                                        
!      CONSTANT ARGUMENT                                                 
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 10                                                       
      ivcomp = nint (3.4994)                                            
      ivcorr = 3                                                        
40090 if (ivcomp - 3) 20090, 10090, 20090                               
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
!      ****  FCVS PROGRAM 307  -  TEST 010  ****                         
!                                                                        
!      VARIABLE ARGUMENT                                                 
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 10                                                       
      rvon01 = -3.4994                                                  
      ivcomp = nint (rvon01)                                            
      ivcorr = -3                                                       
40100 if (ivcomp +3) 20100, 10100, 20100                                
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
!      ****  FCVS PROGRAM 307  -  TEST 011  ****                         
!                                                                        
!      ARRAY ELEMENT NAME ARGUMENT                                       
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 10                                                       
      raon11(1) = 3.0000                                                
      ivcomp = nint (raon11(1))                                         
      ivcorr = 3                                                        
40110 if (ivcomp -3) 20110, 10110, 20110                                
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
!      ****  FCVS PROGRAM 307  -  TEST 012  ****                         
!                                                                        
!      ZERO ARGUMENT                                                     
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 10                                                       
      ivcomp = nint (0.0)                                               
      ivcorr = 0                                                        
40120 if (ivcomp) 20120, 10120, 20120                                   
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
!      TEST 013 THROUGH TEST 017 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE TRIGONOMETRIC TANGENT (TAN) WHERE THE FUNCTION AND    
!      ARGUMENT TYPES ARE BOTH REAL.  ALL ARGUMENTS ARE GIVEN IN RADIANS 
!      WHERE ONE RADIAN EQUALS 57.296 DEGREES.                           
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 013  ****                         
!                                                                        
!      FIND THE TANGENT OF 0 DEGREES (0.0 RADIANS)                       
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = tan (0.0)                                                
      rvcorr = 0.0                                                      
40130 if (rvcomp) 20130, 10130, 20130                                   
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
!      ****  FCVS PROGRAM 307  -  TEST 014  ****                         
!                                                                        
!      FIND THE TANGENT OF 135 DEGREES (2.3562 RADIANS)                  
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 3 * pi / 4                                               
      rvcomp = tan (rvon01)                                             
      rvcorr = -1.0                                                     
40140 if (rvcomp + 1.0005) 20140, 10140, 40141                          
40141 if (rvcomp + .9995) 10140, 10140, 20140                           
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
!      ****  FCVS PROGRAM 307  -  TEST 015  ****                         
!                                                                        
!      FIND THE TANGENT OF 540 DEGREES (9.4248 RADIANS)                  
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      rvcomp = 10.0                                                     
      raon11(2) = 3 * pi                                                
      rvcomp = tan (raon11(2))                                          
      rvcorr = 0.0                                                      
40150 if (rvcomp + .00005) 20150, 10150, 40151                          
40151 if (rvcomp - .00005) 10150, 10150, 20150                          
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
!      ****  FCVS PROGRAM 307  -  TEST 016  ****                         
!                                                                        
!      FIND THE TANGENT OF 30 DEGREES (.52360 RADIANS)                   
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = pi/6                                                     
      rvcomp = tan (rvon01)                                             
      rvcorr = .57735                                                   
40160 if (rvcomp - .57730) 20160, 10160, 40161                          
40161 if (rvcomp - .57740) 10160, 10160, 20160                          
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
!      ****  FCVS PROGRAM 307  -  TEST 017  ****                         
!                                                                        
!      FIND THE TANGENT OF 30 DEGREES BY DIVIDING THE SINE OF 30 DEGREES 
!      BY THE COSINE OF 30 DEGREES.                                      
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = pi/6                                                     
      rvcomp = sin(rvon01)/cos(rvon01)                                  
      rvcorr = .57735                                                   
40170 if (rvcomp - .57730) 20170, 10170, 40171                          
40171 if (rvcomp - .57740) 10170, 10170, 20170                          
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
!      TEST 018 THROUGH TEST 021 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE TRIGONOMETRIC ARCSINE (ASIN) WHERE THE FUNCTION AND   
!      ARGUMENT TYPES ARE BOTH REAL.  THE ABSOLUTE VALUES OF ALL         
!      ARGUMENTS ARE LESS THAN OR EQUAL TO ONE.  THE FUNCTION VALUES     
!      ARE EXPRESSED IN RADIANS WHERE ONE RADIAN EQUALS 57.296 DEGREES.  
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 018  ****                         
!                                                                        
!      THE ARCSINE OF +1. IS 90 DEGREES (1.5708 RADIANS)                 
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = asin (+1.0)                                              
      rvcorr = 1.5708                                                   
40180 if (rvcomp - 1.5703) 20180, 10180, 40181                          
40181 if (rvcomp - 1.5713) 10180, 10180, 20180                          
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
!      ****  FCVS PROGRAM 307  -  TEST 019  ****                         
!                                                                        
!      THE ARCSINE OF -1. IS -90 DEGREES (-1.5708 RADIANS)               
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = -1.0                                                     
      rvcomp = asin(rvon01)                                             
      rvcorr = -1.5708                                                  
40190 if (rvcomp + 1.5713) 20190, 10190, 40191                          
40191 if (rvcomp + 1.5703) 10190, 10190, 20190                          
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
!      ****  FCVS PROGRAM 307  -  TEST 020  ****                         
!                                                                        
!      THE ARCSINE OF -.5 TS -30 DEGREES (-.52360 RADIANS)               
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      rvcomp = 10.0                                                     
      raon11(1) = -.5                                                   
      rvcomp = asin (raon11(1))                                         
      rvcorr = -.52360                                                  
40200 if (rvcomp + .52365) 20200, 10200, 40201                          
40201 if (rvcomp + .52355) 10200, 10200, 20200                          
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
!      ****  FCVS PROGRAM 307  -  TEST 021  ****                         
!                                                                        
!      THE ARCSINE OF 0.0 IS 0 DEGREES (0.0 RADIANS)                     
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 0.0                                                      
      rvcomp = asin (rvon01)                                            
      rvcorr = 0.0                                                      
40210 if (rvcomp) 20210, 10210, 20210                                   
30210 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10210, 0221, 20210                                    
10210 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0221                                                        
20210 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0221 continue                                                          
!                                                                        
!      TEST 022 THROUGH TEST 025 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE TRIGONOMETRIC ARCCOSINE (ACOS) WHERE THE FUNCTION     
!      AND ARGUMENT TYPES ARE BOTH REAL.  THE ABSOLUTE VALUES ALL        
!      ARGUMENTS ARE LESS THAN OR EQUAL TO ONE.  THE FUNCTION VALUES     
!      ARE EXPRESSED IN RADIANS WHERE ONE RADIAN EQUALS 57.296 DEGREES.  
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 022  ****                         
!                                                                        
!      THE ARCCOSINE OF +1. IS 0 DEGREES ( 0.0 RADIANS)                  
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = acos(+1.)                                                
      rvcorr = 0.0                                                      
40220 if (rvcomp + .00005) 20220, 10220, 40221                          
40221 if (rvcomp - .00005) 10220, 10220, 20220                          
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
!      ****  FCVS PROGRAM 307  -  TEST 023  ****                         
!                                                                        
!      THE ARCCOSINE OF -1. IS 180 DEGREES (3.1416 RADIANS)              
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = -1.0                                                     
      rvcomp = acos (rvon01)                                            
      rvcorr = 3.1416                                                   
40230 if (rvcomp - 3.1411) 20230, 10230, 40231                          
40231 if (rvcomp - 3.1421) 10230, 10230, 20230                          
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
!      ****  FCVS PROGRAM 307  -  TEST 024  ****                         
!                                                                        
!      THE ARCCOSINE OF -.5 IS 120 DEGREES (2.0944 RADIANS)              
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      rvcomp = 10.0                                                     
      raon11(1) = -.5                                                   
      rvcomp = acos (raon11(1))                                         
      rvcorr = 2.0944                                                   
40240 if (rvcomp - 2.0939) 20240, 10240, 40241                          
40241 if (rvcomp - 2.0949) 10240, 10240, 20240                          
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
!      ****  FCVS PROGRAM 307  -  TEST 025  ****                         
!                                                                        
!      THE ARCCOSINE OF 0.0 IS 90 DEGREES (1.5708 RADIANS)               
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = acos (0.)                                                
      rvcorr = 1.5708                                                   
40250 if (rvcomp - 1.5703) 20250, 10250, 40251                          
40251 if (rvcomp - 1.5713) 10250, 10250, 20250                          
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
!      TEST 026 THROUGH TEST 028 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE HYPERBOLIC SINE (SINH) WHERE THE FUNCTION AND         
!      ARGUMENT TYPES ARE BOTH REAL.  ONLY POSITIVE ARGUMENTS ARE        
!      TESTED.                                                           
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 026  ****                         
!                                                                        
!      CONSTANT ARGUMENT                                                 
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = sinh (0.0)                                               
      rvcorr = 0.0                                                      
40260 if (rvcomp + .00005) 20260, 10260, 40261                          
40261 if (rvcomp - .00005) 10260, 10260, 20260                          
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
!      ****  FCVS PROGRAM 307  -  TEST 027  ****                         
!                                                                        
!      VARIABLE ARGUMENT                                                 
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      rvcomp =10.0                                                      
      rvon01 = 2.0                                                      
      rvcomp = sinh (rvon01)                                            
      rvcorr = 3.6269                                                   
40270 if (rvcomp - 3.6264) 20270, 10270, 40271                          
40271 if (rvcomp - 3.6274) 10270, 10270, 20270                          
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
!      ****  FCVS PROGRAM 307  -  TEST 028  ****                         
!                                                                        
!      ARRAY ELEMENT NAME ARGUMENT                                       
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      rvcomp = 10.0                                                     
      raon11(1) = 6.0                                                   
      rvcomp = sinh (raon11(1))                                         
      rvcorr = 201.71                                                   
40280 if (rvcomp - 201.66) 20280, 10280, 40281                          
40281 if (rvcomp - 201.76) 10280, 10280, 20280                          
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
!      TEST 029 THROUGH TEST 031 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      FINDING THE HYPERBOLIC COSINE (COSH) WHERE THE FUNCTION AND       
!      ARGUMENT TYPES ARE BOTH REAL.  ONLY POSITIVE ARGUMENTS ARE TESTED.
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 029  ****                         
!                                                                        
!      CONSTANT ARGUMENT                                                 
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = cosh (0.0)                                               
      rvcorr = 1.0                                                      
40290 if (rvcomp - .9995) 20290, 10290, 40291                           
40291 if (rvcomp - 1.0005) 10290, 10290, 20290                          
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
!      ****  FCVS PROGRAM 307  -  TEST 030  ****                         
!                                                                        
!      VARIABLE ARGUMENT                                                 
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 2.0                                                      
      rvcomp = cosh (rvon01)                                            
      rvcorr = 3.7622                                                   
40300 if (rvcomp - 3.7617) 20300, 10300, 40301                          
40301 if (rvcomp - 3.7627) 10300, 10300, 20300                          
30300 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10300, 0311, 20300                                    
10300 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0311                                                        
20300 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0311 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 307  -  TEST 031  ****                         
!                                                                        
!      ARRAY ELEMENT NAME ARGUMENT                                       
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      rvcomp = 10.0                                                     
      raon11(2) = 6.0                                                   
      rvcomp = cosh (raon11(2))                                         
      rvcorr = 201.72                                                   
40310 if (rvcomp - 201.67) 20310, 10310, 40311                          
40311 if (rvcomp - 201.77) 10310, 10310, 20310                          
30310 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10310, 0321, 20310                                    
10310 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0321                                                        
20310 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0321 continue                                                          
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
90001 format (" ",24x,"FM307")                                          
90000 format (" ",20x,"END OF PROGRAM FM307" )                          
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
      end program fm307
