      program fm308
!                                                                        
!                                                                        
!           THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE ACTUAL      
!      ARGUMENTS CONSIST OF INTRINSIC FUNCTION REFERENCES, EXTERNAL      
!      FUNCTION REFERENCES, STATEMENT FUNCTION REFERENCES, AND           
!      EXPRESSIONS INVOLVING OPERATORS.  THE ARGUMENT AND FUNCTION       
!      TYPES OF ALL INTRINSIC FUNCTIONS TESTED ARE EITHER INTEGER OR     
!      REAL.  THE INTRINSIC AND EXTERNAL SPECIFICATION STATEMENTS ARE    
!      SPECIFIED IN ORDER TO ALLOW INTRINSIC AND EXTERNAL FUNCTIONS TO   
!      BE USED AS ACTUAL ARGUMENTS.  THE IMPLICIT STATEMENT AND THE      
!      TYPE-STATEMENT ARE TESTED TO ENSURE THAT THEY DO NOT CHANGE THE   
!      TYPE OF AN INTRINSIC FUNCTION.  THE COMMON STATEMENT IS USED TO   
!      PASS DATA ENTITIES TO AN EXTERNAL FUNCTION.  THE DATA STATEMENT   
!      IS USED TO ENSURE THAT INITIALLY DEFINED ENTITIES CAN BE USED AS  
!      ACTUAL ARGUMENTS.  THE EQUIVALENCE STATEMENT IS USED TO EQUATE A  
!      VARIABLE USED AS AN ACTUAL ARGUMENT.  THE INTRINSIC FUNCTIONS     
!      TESTED IN THIS ROUTINE INCLUDE.                                   
!                                                                        
!                                         SPECIFIC        TYPE OF        
!           INTRINSIC FUNCTION            NAME        ARGUMENT   FUNCTION
!           ------------------            --------    --------   --------
!           TYPE CONVERSION               INT         REAL       INTEGER 
!           TYPE CONVERSION               IFIX        REAL       INTEGER 
!           TYPE CONVERSION               FLOAT       INTEGER    REAL    
!           TYPE CONVERSION               REAL        INTEGER    REAL    
!           TRUNCATION                    AINT        REAL       REAL    
!           NEAREST WHOLE NUMBER          ANINT       REAL       REAL    
!           NEAREST INTEGER               NINT        REAL       INTEGER 
!           ABSOLUTE VALUE                IABS        INTEGER    INTEGER 
!           ABSOLUTE VALUE                ABS         REAL       REAL    
!           REMAINDERING                  MOD         INTEGER    INTEGER 
!           REMAINDERING                  AMOD        REAL       REAL    
!           TRANSFER OF SIGN              ISIGN       INTEGER    INTEGER 
!           TRANSFER OF SIGN              SIGN        REAL       REAL    
!           POSITIVE DIFFERENCE           IDIM        INTEGER    INTEGER 
!           POSITIVE DIFFERENCE           DIM         REAL       REAL    
!           CHOOSING LARGEST VALUE        MAX0        INTEGER    INTEGER 
!           CHOOSING LARGEST VALUE        AMAX0       INTEGER    REAL    
!           CHOOSING LARGEST VALUE        MAX1        REAL       INTEGER 
!           CHOOSING SMALLEST VALUE       AMIN1       REAL       REAL    
!           CHOOSING SMALLEST VALUE       MIN1        REAL       INTEGER 
!           SQUARE ROOT                   SQRT        REAL       REAL    
!           EXPONENTIAL                   EXP         REAL       REAL    
!           NATURAL LOGARITHM             ALOG        REAL       REAL    
!           SINE                          SIN         REAL       REAL    
!           COSINE                        COS         REAL       REAL    
!           TANGENT                       TAN         REAL       REAL    
!           ARCSINE                       ASIN        REAL       REAL    
!           ARCCOSINE                     ACOS        REAL      REAL     
!           ARCTANGENT                    ATAN        REAL      REAL     
!           HYPERBOLIC SINE               SINH        REAL      REAL     
!           HYPERBOLIC COSINE             COSH        REAL      REAL     
!           HYPERBOLIC TANGENT            TANH        REAL      REAL     
!                                                                        
!           SUBSET LEVEL ROUTINES FM097, FM098, FM099 AND FM307 TEST THE 
!      USE OF INTEGER AND REAL INTRINSIC FUNCTIONS USING INTEGER AND REAL
!      CONSTANTS, VARIABLES AND ARRAY ELEMENT ENTITIES AS ACTUAL         
!      ARGUMENTS.                                                        
!                                                                        
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 8.2,     EQUIVALENCE STATEMENT                         
!         SECTION 8.3,     COMMON STATEMENT                              
!         SECTION 8.4,     TYPE-STATEMENTS                               
!         SECTION 8.5,     IMPLICIT STATEMENT                            
!         SECTION 8.7,     EXTERNAL STATEMENT                            
!         SECTION 8.8,     INTRINSIC STATEMENT                           
!         SECTION 9,       DATA STATEMENT                                
!         SECTION 15.3,    INTRINSIC FUNCTION                            
!         SECTION 15.4,    STATEMENT FUNCTION                            
!         SECTION 15.5,    EXTERNAL FUNCTION                             
!         SECTION 15.5.2, .REFERENCING AN EXTERNAL FUNCTION              
!         SECTION 15.9.2,  ACTUAL ARGUMENTS                              
!         SECTION 15.9.3,  ASSOCIATION OF DUMMY AND ACTUAL ARGUMENTS     
!         TABLE 5,         INTRINSIC FUNCTIONS (INCLUDING NOTES)         
!         SECTION 15.10.1, RESTRICTIONS ON RANGE OF ARGUMENTS AND RESULTS
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
      real :: rvcn01
      real :: rfos01
      real :: rdon01
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
      integer :: ivcomp
      integer :: ivcorr
      real :: rvon01
      real :: rvon02
      integer :: ivon02
      integer :: ivoe01
      real :: rvon04
      integer :: max1
      real :: sinh
      real, dimension(1:5) :: radn11
      integer, dimension(1:5) :: iadn11
      equivalence (ivoe01,ivoe02)                                       
      external ff309,ff310                                              
      intrinsic abs, aint, iabs, isign, sqrt                            
      data rvon04 / 2.23 / 
      rfos01(rdon01) = rdon01 + 1.0                                     
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
!      TEST 032 THROUGH TEST 040 TEST INTRINSIC FUNCTIONS USING          
!      INTRINSIC FUNCTION REFERENCES AS ACTUAL ARGUMENTS.                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 032  ****                         
!                                                                        
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = anint (abs (-2.78) )                                     
      rvcorr = 3.0                                                      
40320 if (rvcomp - 2.9995) 20320, 10320, 40321                          
40321 if (rvcomp - 3.0005) 10320, 10320, 20320                          
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
!      ****  FCVS PROGRAM 308  -  TEST 033  ****                         
!                                                                        
!                                                                        
      ivtnum =  33                                                      
      if (iczero) 30330, 0330, 30330                                    
 0330 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = atan (aint (1.2) )                                       
      rvcorr = .78540                                                   
40330 if (rvcomp - .78535) 20330, 10330, 40331                          
40331 if (rvcomp - .78545) 10330, 10330, 20330                          
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
!      ****  FCVS PROGRAM 308  -  TEST 034  ****                         
!                                                                        
!                                                                        
      ivtnum =  34                                                      
      if (iczero) 30340, 0340, 30340                                    
 0340 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = cos (abs (-.78540) )                                     
      rvcorr = .70711                                                   
40340 if (rvcomp - .70706) 20340, 10340, 40341                          
40341 if (rvcomp - .70716) 10340, 10340, 20340                          
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
!      ****  FCVS PROGRAM 308  -  TEST 035  ****                         
!                                                                        
!                                                                        
      ivtnum =  35                                                      
      if (iczero) 30350, 0350, 30350                                    
 0350 continue                                                          
      rvcomp = 10.0                                                     
      ivon01 = 6                                                        
      rvcomp = amax0 (1, ivon01, iabs(-7) )                             
      rvcorr = 7.0                                                      
40350 if (rvcomp - 6.9995) 20350, 10350, 40351                          
40351 if (rvcomp - 7.0005) 10350, 10350, 20350                          
30350 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10350, 0361, 20350                                    
10350 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0361                                                        
20350 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0361 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 036  ****                         
!                                                                        
!                                                                        
      ivtnum =  36                                                      
      if (iczero) 30360, 0360, 30360                                    
 0360 continue                                                          
      ivcomp = 10                                                       
      ivcomp = iabs (isign (7, -2))                                     
      ivcorr = 7                                                        
40360 if (ivcomp - 7) 20360, 10360, 20360                               
30360 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10360, 0371, 20360                                    
10360 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0371                                                        
20360 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0371 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 037  ****                         
!                                                                        
!                                                                        
      ivtnum =  37                                                      
      if (iczero) 30370, 0370, 30370                                    
 0370 continue                                                          
      ivcomp = 10                                                       
      ivcomp = mod (5, iabs (-3) )                                      
      ivcorr = 2                                                        
40370 if (ivcomp - 2) 20370, 10370, 20370                               
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
!      ****  FCVS PROGRAM 308  -  TEST 038  ****                         
!                                                                        
!                                                                        
      ivtnum =  38                                                      
      if (iczero) 30380, 0380, 30380                                    
 0380 continue                                                          
      ivcomp = 10                                                       
      ivcomp = isign (-3, iabs (-5) )                                   
      ivcorr = 3                                                        
40380 if (ivcomp - 3) 20380, 10380, 20380                               
30380 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10380, 0391, 20380                                    
10380 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0391                                                        
20380 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0391 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 039  ****                         
!                                                                        
!      REPEAT FUNCTION REFERENCE TWICE IN ONE INTRINSIC FUNCTION         
!      REFERENCE.                                                        
!                                                                        
      ivtnum =  39                                                      
      if (iczero) 30390, 0390, 30390                                    
 0390 continue                                                          
      ivcomp = 10                                                       
      ivcomp = max0 (iabs (-5), iabs (-6) )                             
      ivcorr = 6                                                        
40390 if (ivcomp -6) 20390, 10390, 20390                                
30390 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10390, 0401, 20390                                    
10390 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0401                                                        
20390 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0401 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 040  ****                         
!                                                                        
!      USE INTRINSIC FUNCTION REFERENCE AS ACTUAL ARGUMENT TO ITSELF.    
!                                                                        
      ivtnum =  40                                                      
      if (iczero) 30400, 0400, 30400                                    
 0400 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = sqrt (sqrt (25.) )                                       
      rvcorr = 2.2361                                                   
40400 if (rvcomp - 2.2356) 20400, 10400, 40401                          
40401 if (rvcomp - 2.2366) 10400, 10400, 20400                          
30400 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10400, 0411, 20400                                    
10400 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0411                                                        
20400 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0411 continue                                                          
!                                                                        
!      TEST 041 THROUGH TEST 045 TEST INTRINSIC FUNCTIONS USING EXTERNAL 
!      FUNCTION REFERENCES AS ACTUAL ARGUMENTS.                          
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 041  ****                         
!                                                                        
!                                                                        
      ivtnum =  41                                                      
      if (iczero) 30410, 0410, 30410                                    
 0410 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp  = alog(ff309(29.0))
      rvcorr = 3.4012                                                   
40410 if (rvcomp - 3.4007) 20410, 10410, 40411                          
40411 if (rvcomp - 3.4017) 10410, 10410, 20410                          
30410 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10410, 0421, 20410                                    
10410 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0421                                                        
20410 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0421 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 042  ****                         
!                                                                        
!                                                                        
      ivtnum =  42                                                      
      if (iczero) 30420, 0420, 30420                                    
 0420 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp  = asin(ff309(0.))
      rvcorr = 1.5708                                                   
40420 if (rvcomp - 1.5703) 20420, 10420, 40421                          
40421 if (rvcomp - 1.5713) 10420, 10420, 20420                          
30420 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10420, 0431, 20420                                    
10420 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0431                                                        
20420 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0431 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 043  ****                         
!                                                                        
!                                                                        
      ivtnum =  43                                                      
      if (iczero) 30430, 0430, 30430                                    
 0430 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 1.5                                                      
      rvcomp  = cosh(ff309(rvon01))
      rvcorr = 6.1323                                                   
40430 if (rvcomp - 6.1318) 20430, 10430, 40431                          
40431 if (rvcomp - 6.1328) 10430, 10430, 20430                          
30430 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10430, 0441, 20430                                    
10430 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0441                                                        
20430 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0441 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 044  ****                         
!                                                                        
!                                                                        
      ivtnum =  44                                                      
      if (iczero) 30440, 0440, 30440                                    
 0440 continue                                                          
      ivcomp = 10                                                       
      ivcomp  = ifix(ff309(33.3))
      ivcorr = 34                                                       
40440 if (ivcomp - 34) 20440, 10440, 20440                              
30440 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10440, 0451, 20440                                    
10440 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0451                                                        
20440 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0451 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 045  ****                         
!                                                                        
!                                                                        
      ivtnum =  45                                                      
      if (iczero) 30450, 0450, 30450                                    
 0450 continue                                                          
      rvcomp = 10.0                                                     
      radn11(2) = 2.1416                                                
      rvcomp  = tan(ff309(radn11(2)))
      rvcorr = 0.0                                                      
40450 if (rvcomp + .00005) 20450, 10450, 40451                          
40451 if (rvcomp - .00005) 10450, 10450, 20450                          
30450 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10450, 0461, 20450                                    
10450 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0461                                                        
20450 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0461 continue                                                          
!                                                                        
!      TEST 046 THROUGH TEST 052 TEST INTRINSIC FUNCTIONS USING          
!      EXPRESSIONS INVOLVING OPERATORS AS ACTUAL ARGUMENTS.              
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 046  ****                         
!                                                                        
!                                                                        
      ivtnum =  46                                                      
      if (iczero) 30460, 0460, 30460                                    
 0460 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = abs (3.4 - 8.2)                                          
      rvcorr = 4.8                                                      
40460 if (rvcomp - 4.7995) 20460, 10460, 40461                          
40461 if (rvcomp - 4.8005) 10460, 10460, 20460                          
30460 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10460, 0471, 20460                                    
10460 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0471                                                        
20460 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0471 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 047  ****                         
!                                                                        
!                                                                        
      ivtnum =  47                                                      
      if (iczero) 30470, 0470, 30470                                    
 0470 continue                                                          
      rvcomp = 10.0                                                     
      ivon01 = 2                                                        
      rvon01 = 3.0                                                      
      rvcomp = acos (ivon01 - rvon01 * .5)                              
      rvcorr = 1.0472                                                   
40470 if (rvcomp - 1.0467) 20470, 10470, 40471                          
40471 if (rvcomp - 1.0477) 10470, 10470, 20470                          
30470 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10470, 0481, 20470                                    
10470 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0481                                                        
20470 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0481 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 048  ****                         
!                                                                        
!                                                                        
      ivtnum =  48                                                      
      if (iczero) 30480, 0480, 30480                                    
 0480 continue                                                          
      rvcomp = 10.0                                                     
      ivon01 = 2                                                        
      rvon01 = -4.8                                                     
      rvon02 = 4.5                                                      
      rvcomp = amin1 (rvon01, (ivon01 - 3.2) * rvon02)                  
      rvcorr = -5.4                                                     
40480 if (rvcomp + 5.4005 ) 20480, 10480, 40481                         
40481 if (rvcomp + 5.3995 ) 10480, 10480, 20480                         
30480 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10480, 0491, 20480                                    
10480 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0491                                                        
20480 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0491 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 049  ****                         
!                                                                        
!                                                                        
      ivtnum =  49                                                      
      if (iczero) 30490, 0490, 30490                                    
 0490 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 12.0                                                     
      iadn11(1) = 3                                                     
      radn11(2) = 2.5                                                   
      rvcomp = amod (rvon01 / iadn11(1), 12 / radn11(2))                
      rvcorr = 4.0                                                      
40490 if (rvcomp - 3.9995) 20490, 10490, 40491                          
40491 if (rvcomp - 4.0005) 10490, 10490, 20490                          
30490 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10490, 0501, 20490                                    
10490 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0501                                                        
20490 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0501 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 050  ****                         
!                                                                        
!                                                                        
      ivtnum =  50                                                      
      if (iczero) 30500, 0500, 30500                                    
 0500 continue                                                          
      ivcomp = 10                                                       
      ivon01 = 2                                                        
      ivon02 = 9                                                        
      ivcomp = idim (ivon01 ** 3, ivon02)                               
      ivcorr = 0                                                        
40500 if (ivcomp) 20500, 10500, 20500                                   
30500 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10500, 0511, 20500                                    
10500 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0511                                                        
20500 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0511 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 051  ****                         
!                                                                        
!                                                                        
      ivtnum =  51                                                      
      if (iczero) 30510, 0510, 30510                                    
 0510 continue                                                          
      rvcomp = 10.0                                                     
      ivon01 = 6                                                        
      rvcomp = real (iabs (-3) + ivon01)                                
      rvcorr = 9.0                                                      
40510 if (rvcomp - 8.9995) 20510, 10510, 40511                          
40511 if (rvcomp - 9.0005) 10510, 10510, 20510                          
30510 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10510, 0521, 20510                                    
10510 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0521                                                        
20510 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0521 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 052  ****                         
!                                                                        
!                                                                        
      ivtnum =  52                                                      
      if (iczero) 30520, 0520, 30520                                    
 0520 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 2.3                                                      
      ivon01 = 150                                                      
      iadn11(1) = 3                                                     
      rvcomp  = sign(13+rvon01*iabs(-4)-ivon01*1.0/ff309(1.)**iadn11(1),-1.)
      rvcorr = -3.45                                                    
40520 if (rvcomp + 3.4505) 20520, 10520, 40521                          
40521 if (rvcomp + 3.4495) 10520, 10520, 20520                          
30520 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10520, 0531, 20520                                    
10520 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0531                                                        
20520 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0531 continue                                                          
!                                                                        
!      TEST 053 THROUGH TEST 056 TEST INTRINSIC FUNCTIONS USING          
!      STATEMENT FUNCTION REFERENCES AS ACTUAL ARGUMENTS.                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 053  ****                         
!                                                                        
!                                                                        
      ivtnum =  53                                                      
      if (iczero) 30530, 0530, 30530                                    
 0530 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = dim (rfos01(5.4), 6.0)                                   
      rvcorr = .4                                                       
40530 if (rvcomp - .39995) 20530, 10530, 40531                          
40531 if (rvcomp - .40005) 10530, 10530, 20530                          
30530 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10530, 0541, 20530                                    
10530 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0541                                                        
20530 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0541 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 054  ****                         
!                                                                        
!                                                                        
      ivtnum =  54                                                      
      if (iczero) 30540, 0540, 30540                                    
 0540 continue                                                          
      ivcomp = 10                                                       
      ivcomp = int(rfos01(2.01))                                        
      ivcorr = 3                                                        
40540 if (ivcomp - 3) 20540, 10540, 20540                               
30540 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10540, 0551, 20540                                    
10540 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0551                                                        
20540 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0551 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 055  ****                         
!                                                                        
!                                                                        
      ivtnum =  55                                                      
      if (iczero) 30550, 0550, 30550                                    
 0550 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 0.5708                                                   
      rvcomp = sin (rfos01 (rvon01) / 2)                                
      rvcorr = .70711                                                   
40550 if (rvcomp - .70706) 20550, 10550, 40551                          
40551 if (rvcomp - .70716) 10550, 10550, 20550                          
30550 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10550, 0561, 20550                                    
10550 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0561                                                        
20550 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0561 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 056  ****                         
!                                                                        
!                                                                        
      ivtnum =  56                                                      
      if (iczero) 30560, 0560, 30560                                    
 0560 continue                                                          
      rvcomp = 10.0                                                     
      radn11(2) = 1.5                                                   
      rvcomp = tanh(rfos01(radn11(2)))                                  
      rvcorr = .98661                                                   
40560 if (rvcomp - .98656) 20560, 10560, 40561                          
40561 if (rvcomp - .98666) 10560, 10560, 20560                          
30560 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10560, 0571, 20560                                    
10560 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0571                                                        
20560 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0571 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 057  ****                         
!                                                                        
!      TEST 057 TESTS THE INTRINSIC FUNCTION AINT USING AN EXTERNAL      
!      FUNCTION REFERENCE AS AN ACTUAL ARGUMENT AND THE COMMON           
!      STATEMENT AS A MEANS OF PASSING DATA TO THE EXTERNAL FUNCTION.    
!                                                                        
      ivtnum =  57                                                      
      if (iczero) 30570, 0570, 30570                                    
 0570 continue                                                          
      rvcomp = 10.0                                                     
      rvcn01 = 25.3                                                     
      rvcomp  = aint(ff310(rvcn01))
      rvcorr = 26.0                                                     
40570 if (rvcomp - 25.995) 20570, 10570, 40571                          
40571 if (rvcomp - 26.005) 10570, 10570, 20570                          
30570 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10570, 0581, 20570                                    
10570 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0581                                                        
20570 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0581 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 058  ****                         
!                                                                        
!      TEST 058 TESTS THE INTRINSIC FUNCTION FLOAT BY USING A VARIABLE   
!      EQUATED BY EQUIVALENCE ASSOCIATION AS AN ACTUAL ARGUMENT.         
!                                                                        
      ivtnum =  58                                                      
      if (iczero) 30580, 0580, 30580                                    
 0580 continue                                                          
      rvcomp = 10.0                                                     
      ivoe01 = 5                                                        
      rvcomp = float(ivoe01)                                            
      rvcorr = 5.0                                                      
40580 if (rvcomp - 4.9995) 20580, 10580, 40581                          
40581 if (rvcomp - 5.0005) 10580, 10580, 20580                          
30580 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10580, 0591, 20580                                    
10580 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0591                                                        
20580 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0591 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 059  ****                         
!                                                                        
!      TEST 059 TESTS THE INTRINSIC FUNCTION MIN1 BY USING A VARIABLE    
!      INITIALIZED BY THE DATA STATEMENT AS AN ACTUAL ARGUMENT.          
!                                                                        
      ivtnum =  59                                                      
      if (iczero) 30590, 0590, 30590                                    
 0590 continue                                                          
      ivcomp = 10                                                       
      ivcomp = min1(6., rvon04, 7.3)                                    
      ivcorr = 2                                                        
40590 if (ivcomp - 2) 20590, 10590, 20590                               
30590 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10590, 0601, 20590                                    
10590 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0601                                                        
20590 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0601 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 060  ****                         
!                                                                        
!      TEST 060 ATTEMPTS TO OVERRIDE THE TYPING OF REAL FOR THE          
!      INTRINSIC FUNCTION EXP WITH IMPLICIT INTEGER TYPING.              
!                                                                        
      ivtnum =  60                                                      
      if (iczero) 30600, 0600, 30600                                    
 0600 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 2.05                                                     
      rvcomp = exp(rvon01)                                              
      rvcorr = 7.7679                                                   
40600 if (rvcomp - 7.7674) 20600, 10600, 40601                          
40601 if (rvcomp - 7.7684) 10600, 10600, 20600                          
30600 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10600, 0611, 20600                                    
10600 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0611                                                        
20600 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0611 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 061  ****                         
!                                                                        
!      TEST 061 ATTEMPTS TO OVERRIDE THE TYPING OF INTEGER FOR THE       
!      INTRINSIC FUNCTION NINT WITH IMPLICIT REAL TYPING.                
!                                                                        
      ivtnum =  61                                                      
      if (iczero) 30610, 0610, 30610                                    
 0610 continue                                                          
      rvcomp = 10.0                                                     
      rvon01 = 3.78                                                     
      rvcomp = nint(rvon01) / 5                                         
      rvcorr = 0.0                                                      
40610 if (rvcomp + .00005) 20610, 10610, 40611                          
40611 if (rvcomp - .00005) 10610, 10610, 20610                          
30610 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10610, 0621, 20610                                    
10610 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0621                                                        
20610 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0621 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 062  ****                         
!                                                                        
!      TEST 062 ATTEMPTS TO OVERRIDE THE TYPING OF REAL FOR THE          
!      INTRINSIC FUNCTION SINH WITH TYPE-STATEMENT TYPING OF INTEGER.    
!                                                                        
      ivtnum =  62                                                      
      if (iczero) 30620, 0620, 30620                                    
 0620 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = sinh(2.0)                                                
      rvcorr = 3.6269                                                   
40620 if (rvcomp - 3.6264) 20620, 10620, 40621                          
40621 if (rvcomp - 3.6274) 10620, 10620, 20620                          
30620 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10620, 0631, 20620                                    
10620 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0631                                                        
20620 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0631 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 308  -  TEST 063  ****                         
!                                                                        
!      TEST 063 ATTEMPTS TO OVERRIDE THE TYPING OF INTEGER FOR THE       
!      INTRINSIC FUNCTION MAX1 WITH TYPE-STATEMENT TYPING OF REAL.       
!                                                                        
      ivtnum =  63                                                      
      if (iczero) 30630, 0630, 30630                                    
 0630 continue                                                          
      rvcomp = 10.0                                                     
      rvcomp = max1(2.3, 3.1, 4.4) / 5                                  
      rvcorr = 0.0                                                      
40630 if (rvcomp + .00005) 20630, 10630, 40631                          
40631 if (rvcomp - .00005) 10630, 10630, 20630                          
30630 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10630, 0641, 20630                                    
10630 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0641                                                        
20630 ivfail = ivfail + 1                                               
      write (i02,80012) ivtnum, rvcomp, rvcorr                          
 0641 continue                                                          
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
90001 format (" ",24x,"FM308")                                          
90000 format (" ",20x,"END OF PROGRAM FM308" )                          
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
      end program fm308
      real function ff309(rdon01)
      real :: rdon01
!           THIS FUNCTION IS USED TO INCREMENT THE ARGUMENT VALUE BY     
!      ONE AND RETURN THE RESULT AS THE FUNCTION VALUE.                  
      ff309 = rdon01 + 1.0                                              
      return                                                            
      end function ff309
      real function ff310(rvcn01)
      real :: rvcn01
!           THIS FUNCTION IS USED TO INCREMENT BY ONE A VALUE PASSED     
!      TO THE FUNCTION THROUGH COMMON.                                   
      ff310 = rvcn01 + 1.0                                              
      return                                                            
      end function ff310
