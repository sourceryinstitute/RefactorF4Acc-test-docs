      program fm099
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvon01
      real :: rvcomp
      real :: rvcorr
      real :: rvon02
!      COMMENT SECTION                                                   
!                                                                        
!      FM099                                                             
!                                                                        
!      THIS ROUTINE TESTS VARIOUS MATHEMATICAL FUNCTIONS WHERE BOTH THE  
!      FUNCTION TYPE AND ARGUMENTS ARE REAL.  THE REAL VARIABLES AND     
!      CONSTANTS CONTAIN BOTH POSITIVE AND NEGATIVE VALUES.  THE         
!      FUNCTIONS TESTED IN FM099 INCLUDE                                 
!                                                                        
!                                                      TYPE OF           
!        FUNCTION                    NAME       ARGUMENT     FUNCTION    
!        ----------------            ----        --------    --------    
!          EXPONENTIAL               EXP        REAL         REAL        
!          NATURAL LOGARITHM         ALOG       REAL         REAL        
!          COMMON LOGARITHM          ALOG10     REAL         REAL        
!          SQUARE ROOT               SQRT       REAL         REAL        
!          TRIGONOMETRIC SINE        SIN        REAL         REAL        
!          TRIGONOMETRIC COSINE      COS        REAL         REAL        
!          HYPERBOLIC TANGENT        TANH       REAL         REAL        
!          ARCTANGENT                ATAN       REAL         REAL        
!                                    ATAN2      REAL         REAL        
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8.7, EXTERNAL STATEMENT                                
!         SECTION 15.5.2, FUNCTION REFERENCE                             
!                                                                        
!                                                                        
!       **********************************************************       
!                                                                        
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  
!      PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 
!      FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     
!      VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED
!      DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   
!      PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  
!      LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 
!      OF EXECUTING THESE TESTS.                                         
!                                                                        
!          THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 
!      FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 
!                                                                        
!          SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             
!                                                                        
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!       **********************************************************       
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION                                            
!                                                                        
!      INITIALIZE CONSTANTS                                              
!       **************                                                   
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD. 
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD. 
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass=0                                                          
      ivfail=0                                                          
      ivdele=0                                                          
      iczero=0                                                          
!                                                                        
!      WRITE PAGE HEADERS                                                
      write (i02,90000)                                                 
      write (i02,90001)                                                 
      write (i02,90002)                                                 
      write (i02, 90002)                                                
      write (i02,90003)                                                 
      write (i02,90002)                                                 
      write (i02,90004)                                                 
      write (i02,90002)                                                 
      write (i02,90011)                                                 
      write (i02,90002)                                                 
      write (i02,90002)                                                 
      write (i02,90005)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!      TEST 939 THROUGH TEST 942 CONTAIN FUNCTION TESTS FOR EXPONENTIAL  
!      FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL                
!                                                                        
      ivtnum = 939                                                      
!                                                                        
!       ****  TEST 939  ****                                             
!                                                                        
      if (iczero) 39390, 9390, 39390                                    
 9390 continue                                                          
      rvon01 = 0.0                                                      
      rvcomp = exp (rvon01)                                             
      goto 49390                                                       
39390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49390, 9401, 49390                                    
49390 if (rvcomp - 0.95) 29390,19390,49391                              
49391 if (rvcomp - 1.05) 19390,19390,29390                              
19390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9401                                                        
29390 ivfail = ivfail + 1                                               
      rvcorr = 1.00                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9401 continue                                                          
      ivtnum = 940                                                      
!                                                                        
!       ****  TEST 940  ****                                             
!                                                                        
      if (iczero) 39400, 9400, 39400                                    
 9400 continue                                                          
      rvcomp = exp (0.5)                                                
      goto 49400                                                       
39400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49400, 9411, 49400                                    
49400 if (rvcomp - 1.60) 29400,19400,49401                              
49401 if (rvcomp - 1.70) 19400,19400,29400                              
19400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9411                                                        
29400 ivfail = ivfail + 1                                               
      rvcorr = 1.65                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9411 continue                                                          
      ivtnum = 941                                                      
!                                                                        
!       ****  TEST 941  ****                                             
!                                                                        
      if (iczero) 39410, 9410, 39410                                    
 9410 continue                                                          
      rvon01 = .1e1                                                     
      rvcomp = exp (rvon01)                                             
      goto 49410                                                       
39410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49410, 9421, 49410                                    
49410 if (rvcomp - 2.67) 29410,19410,49411                              
49411 if (rvcomp - 2.77) 19410,19410,29410                              
19410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9421                                                        
29410 ivfail = ivfail + 1                                               
      rvcorr = 2.72                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9421 continue                                                          
      ivtnum = 942                                                      
!                                                                        
!       ****  TEST 942  ****                                             
!                                                                        
      if (iczero) 39420, 9420, 39420                                    
 9420 continue                                                          
      rvon01 = -1.0                                                     
      rvcomp = exp (rvon01)                                             
      goto 49420                                                       
39420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49420, 9431, 49420                                    
49420 if (rvcomp - 0.363) 29420,19420,49421                             
49421 if (rvcomp - 0.373) 19420,19420,29420                             
19420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9431                                                        
29420 ivfail = ivfail + 1                                               
      rvcorr = 0.368                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9431 continue                                                          
!                                                                        
!      TEST 943 THROUGH TEST 945 CONTAIN FUNCTION TESTS FOR NATURAL      
!      LOGARITHM FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL      
!                                                                        
      ivtnum = 943                                                      
!                                                                        
!       ****  TEST 943  ****                                             
!                                                                        
      if (iczero) 39430, 9430, 39430                                    
 9430 continue                                                          
      rvon01 = 5e1                                                      
      rvcomp = alog (rvon01)                                            
      goto 49430                                                       
39430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49430, 9441, 49430                                    
49430 if (rvcomp - 3.9115) 29430,19430,49431                            
49431 if (rvcomp - 3.9125) 19430,19430,29430                            
19430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9441                                                        
29430 ivfail = ivfail + 1                                               
      rvcorr = 3.9120                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9441 continue                                                          
      ivtnum = 944                                                      
!                                                                        
!       ****  TEST 944  ****                                             
!                                                                        
      if (iczero) 39440, 9440, 39440                                    
 9440 continue                                                          
      rvon01 = 1.0                                                      
      rvcomp = alog (rvon01)                                            
      goto 49440                                                       
39440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49440, 9451, 49440                                    
49440 if (rvcomp + .00005) 29440,19440,49441                            
49441 if (rvcomp - .00005) 19440,19440,29440                            
19440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9451                                                        
29440 ivfail = ivfail + 1                                               
      rvcorr = 0.00000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9451 continue                                                          
      ivtnum = 945                                                      
!                                                                        
!       ****  TEST 945  ****                                             
!                                                                        
      if (iczero) 39450, 9450, 39450                                    
 9450 continue                                                          
      rvcomp = alog (2.0)                                               
      goto 49450                                                       
39450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49450, 9461, 49450                                    
49450 if (rvcomp - 0.688) 29450,19450,49451                             
49451 if (rvcomp - 0.698) 19450,19450,29450                             
19450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9461                                                        
29450 ivfail = ivfail + 1                                               
      rvcorr = 0.693                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9461 continue                                                          
!                                                                        
!      TEST 946 THROUGH TEST 948 CONTAIN FUNCTION TESTS FOR COMMON       
!      LOGARITHM FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL      
!                                                                        
      ivtnum = 946                                                      
!                                                                        
!       ****  TEST 946  ****                                             
!                                                                        
      if (iczero) 39460, 9460, 39460                                    
 9460 continue                                                          
      rvon01 = 2e2                                                      
      rvcomp = alog10 (rvon01)                                          
      goto 49460                                                       
39460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49460, 9471, 49460                                    
49460 if (rvcomp - 2.296) 29460,19460,49461                             
49461 if (rvcomp - 2.306) 19460,19460,29460                             
19460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9471                                                        
29460 ivfail = ivfail + 1                                               
      rvcorr = 2.301                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9471 continue                                                          
      ivtnum = 947                                                      
!                                                                        
!       ****  TEST 947  ****                                             
!                                                                        
      if (iczero) 39470, 9470, 39470                                    
 9470 continue                                                          
      rvon01 = .3e+3                                                    
      rvcomp = alog10 (rvon01)                                          
      goto 49470                                                       
39470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49470, 9481, 49470                                    
49470 if (rvcomp - 2.472) 29470,19470,49471                             
49471 if (rvcomp - 2.482) 19470,19470,29470                             
19470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9481                                                        
29470 ivfail = ivfail + 1                                               
      rvcorr = 2.477                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9481 continue                                                          
      ivtnum = 948                                                      
!                                                                        
!       ****  TEST 948  ****                                             
!                                                                        
      if (iczero) 39480, 9480, 39480                                    
 9480 continue                                                          
      rvon01 = 1350.0                                                   
      rvcomp = alog10 (rvon01)                                          
      goto 49480                                                       
39480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49480, 9491, 49480                                    
49480 if (rvcomp - 3.125) 29480,19480,49481                             
49481 if (rvcomp - 3.135) 19480,19480,29480                             
19480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9491                                                        
29480 ivfail = ivfail + 1                                               
      rvcorr = 3.130                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9491 continue                                                          
!                                                                        
!      TEST 949 THROUGH TEST 951 CONTAIN FUNCTION TESTS FOR SQUARE ROOT  
!      FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL                
!                                                                        
      ivtnum = 949                                                      
!                                                                        
!       ****  TEST 949  ****                                             
!                                                                        
      if (iczero) 39490, 9490, 39490                                    
 9490 continue                                                          
      rvon01 = 1.0                                                      
      rvcomp = sqrt (rvon01)                                            
      goto 49490                                                       
39490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49490, 9501, 49490                                    
49490 if (rvcomp - 0.95) 29490,19490,49491                              
49491 if (rvcomp - 1.05) 19490,19490,29490                              
19490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9501                                                        
29490 ivfail = ivfail + 1                                               
      rvcorr = 1.00                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9501 continue                                                          
      ivtnum = 950                                                      
!                                                                        
!       ****  TEST 950  ****                                             
!                                                                        
      if (iczero) 39500, 9500, 39500                                    
 9500 continue                                                          
      rvcomp = sqrt (2.0)                                               
      goto 49500                                                       
39500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49500, 9511, 49500                                    
49500 if (rvcomp - 1.36) 29500,19500,49501                              
49501 if (rvcomp - 1.46) 19500,19500,29500                              
19500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9511                                                        
29500 ivfail = ivfail + 1                                               
      rvcorr = 1.41                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9511 continue                                                          
      ivtnum = 951                                                      
!                                                                        
!       ****  TEST 951  ****                                             
!                                                                        
      if (iczero) 39510, 9510, 39510                                    
 9510 continue                                                          
      rvon01 = .229e1                                                   
      rvcomp = sqrt (rvon01)                                            
      goto 49510                                                       
39510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49510, 9521, 49510                                    
49510 if (rvcomp - 1.46) 29510,19510,49511                              
49511 if (rvcomp - 1.56) 19510,19510,29510                              
19510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9521                                                        
29510 ivfail = ivfail + 1                                               
      rvcorr = 1.51                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9521 continue                                                          
!                                                                        
!      TEST 952 THROUGH TEST 953 CONTAIN FUNCTION TESTS FOR TRIGONOMETRIC
!      SINE FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL           
!                                                                        
      ivtnum = 952                                                      
!                                                                        
!       ****  TEST 952  ****                                             
!                                                                        
      if (iczero) 39520, 9520, 39520                                    
 9520 continue                                                          
      rvon01 = 0.00000                                                  
      rvcomp = sin (rvon01)                                             
      goto 49520                                                       
39520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49520, 9531, 49520                                    
49520 if (rvcomp + .00005) 29520,19520,49521                            
49521 if (rvcomp - .00005) 19520,19520,29520                            
19520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9531                                                        
29520 ivfail = ivfail + 1                                               
      rvcorr = 0.00000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9531 continue                                                          
      ivtnum = 953                                                      
!                                                                        
!       ****  TEST 953  ****                                             
!                                                                        
      if (iczero) 39530, 9530, 39530                                    
 9530 continue                                                          
      rvon01 = 0.5                                                      
      rvcomp = sin (rvon01)                                             
      goto 49530                                                       
39530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49530, 9541, 49530                                    
49530 if (rvcomp - .474) 29530,19530,49531                              
49531 if (rvcomp - .484) 19530,19530,29530                              
19530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9541                                                        
29530 ivfail = ivfail + 1                                               
      rvcorr = .479                                                     
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9541 continue                                                          
      ivtnum = 954                                                      
!                                                                        
!       ****  TEST 954  ****                                             
!                                                                        
      if (iczero) 39540, 9540, 39540                                    
 9540 continue                                                          
      rvon01 = 4e0                                                      
      rvcomp = sin (rvon01)                                             
      goto 49540                                                       
39540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49540, 9551, 49540                                    
49540 if (rvcomp + .762) 29540,19540,49541                              
49541 if (rvcomp + .752) 19540,19540,29540                              
19540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9551                                                        
29540 ivfail = ivfail + 1                                               
      rvcorr = -.757                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9551 continue                                                          
!                                                                        
!      TEST 955 THROUGH TEST 957 CONTAIN FUNCTION TESTS FOR TRIGONOMETRIC
!      COSINE FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL         
!                                                                        
      ivtnum = 955                                                      
!                                                                        
!       ****  TEST 955  ****                                             
!                                                                        
      if (iczero) 39550, 9550, 39550                                    
 9550 continue                                                          
      rvon01 = 0.00000                                                  
      rvcomp = cos (rvon01)                                             
      goto 49550                                                       
39550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49550, 9561, 49550                                    
49550 if (rvcomp - .995) 29550,19550,49551                              
49551 if (rvcomp - 1.005) 19550,19550,29550                             
19550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9561                                                        
29550 ivfail = ivfail + 1                                               
      rvcorr = 1.000                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9561 continue                                                          
      ivtnum = 956                                                      
!                                                                        
!       ****  TEST 956  ****                                             
!                                                                        
      if (iczero) 39560, 9560, 39560                                    
 9560 continue                                                          
      rvon01 = 1.0e0                                                    
      rvcomp = cos (rvon01)                                             
      goto 49560                                                       
39560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49560, 9571, 49560                                    
49560 if (rvcomp - .535) 29560,19560,49561                              
49561 if (rvcomp - .545) 19560,19560,29560                              
19560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9571                                                        
29560 ivfail = ivfail + 1                                               
      rvcorr = 0.540                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9571 continue                                                          
      ivtnum = 957                                                      
!                                                                        
!       ****  TEST 957  ****                                             
!                                                                        
      if (iczero) 39570, 9570, 39570                                    
 9570 continue                                                          
      rvcomp = cos (4.0)                                                
      goto 49570                                                       
39570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49570, 9581, 49570                                    
49570 if (rvcomp + .659) 29570,19570,49571                              
49571 if (rvcomp + .649) 19570,19570,29570                              
19570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9581                                                        
29570 ivfail = ivfail + 1                                               
      rvcorr = -0.654                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9581 continue                                                          
!                                                                        
!      TEST 958 THROUGH TEST 960 CONTAIN FUNCTION TESTS FOR HYPERBOLIC   
!      TANGENT FUNCTIONS WHERE THE ARGUMENT AND FUNCTION ARE REAL        
!                                                                        
      ivtnum = 958                                                      
!                                                                        
!       ****  TEST 958  ****                                             
!                                                                        
      if (iczero) 39580, 9580, 39580                                    
 9580 continue                                                          
      rvcomp = tanh (0.0)                                               
      goto 49580                                                       
39580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49580, 9591, 49580                                    
49580 if (rvcomp + .00005) 29580,19580,49581                            
49581 if (rvcomp - .00005) 19580,19580,29580                            
19580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9591                                                        
29580 ivfail = ivfail + 1                                               
      rvcorr = 0.00000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9591 continue                                                          
      ivtnum = 959                                                      
!                                                                        
!       ****  TEST 959  ****                                             
!                                                                        
      if (iczero) 39590, 9590, 39590                                    
 9590 continue                                                          
      rvon01 = .5e0                                                     
      rvcomp = tanh (rvon01)                                            
      goto 49590                                                       
39590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49590, 9601, 49590                                    
49590 if (rvcomp - .457) 29590,19590,49591                              
49591 if (rvcomp - .467) 19590,19590,29590                              
19590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9601                                                        
29590 ivfail = ivfail + 1                                               
      rvcorr = 0.462                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9601 continue                                                          
      ivtnum = 960                                                      
!                                                                        
!       ****  TEST 960  ****                                             
!                                                                        
      if (iczero) 39600, 9600, 39600                                    
 9600 continue                                                          
      rvon01 = .25                                                      
      rvcomp = tanh (rvon01)                                            
      goto 49600                                                       
39600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49600, 9611, 49600                                    
49600 if (rvcomp - .240) 29600,19600,49601                              
49601 if (rvcomp - .250) 19600,19600,29600                              
19600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9611                                                        
29600 ivfail = ivfail + 1                                               
      rvcorr = 0.245                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9611 continue                                                          
!                                                                        
!      TESTS 961 AND 962 CONTAIN TESTS FOR ARCTANGENT OF THE FORM        
!      ATAN (A) WHERE THE ARGUMENT AND FUNCTION ARE REAL                 
!                                                                        
      ivtnum = 961                                                      
!                                                                        
!       ****  TEST 961  ****                                             
!                                                                        
      if (iczero) 39610, 9610, 39610                                    
 9610 continue                                                          
      rvcomp = atan (0.0)                                               
      goto 49610                                                       
39610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49610, 9621, 49610                                    
49610 if (rvcomp + .00005) 29610,19610,49611                            
49611 if (rvcomp - .00005) 19610,19610,29610                            
19610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9621                                                        
29610 ivfail = ivfail + 1                                               
      rvcorr = 0.00000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9621 continue                                                          
      ivtnum = 962                                                      
!                                                                        
!       ****  TEST 962  ****                                             
!                                                                        
      if (iczero) 39620, 9620, 39620                                    
 9620 continue                                                          
      rvon01 = 5e-1                                                     
      rvcomp = atan (rvon01)                                            
      goto 49620                                                       
39620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49620, 9631, 49620                                    
49620 if (rvcomp - .459) 29620,19620,49621                              
49621 if (rvcomp - .469) 19620,19620,29620                              
19620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9631                                                        
29620 ivfail = ivfail + 1                                               
      rvcorr = 0.464                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9631 continue                                                          
!                                                                        
!      TESTS 963 AND 964 CONTAIN TESTS FOR ARCTANGENT OF THE FORM        
!      ATAN2 (A1,A2) WHERE THE ARGUMENTS AND FUNCTION ARE REAL           
!                                                                        
      ivtnum = 963                                                      
!                                                                        
!       ****  TEST 963  ****                                             
!                                                                        
      if (iczero) 39630, 9630, 39630                                    
 9630 continue                                                          
      rvon01 = 0.0                                                      
      rvon02 = 1e0                                                      
      rvcomp = atan2 (rvon01,rvon02)                                    
      goto 49630                                                       
39630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49630, 9641, 49630                                    
49630 if (rvcomp + .00005) 29630,19630,49631                            
49631 if (rvcomp - .00005) 19630,19630,29630                            
19630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9641                                                        
29630 ivfail = ivfail + 1                                               
      rvcorr = 0.00000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9641 continue                                                          
      ivtnum = 964                                                      
!                                                                        
!       ****  TEST 964  ****                                             
!                                                                        
      if (iczero) 39640, 9640, 39640                                    
 9640 continue                                                          
      rvon01 = 2e1                                                      
      rvcomp = atan2 (-1.0,rvon01)                                      
      goto 49640                                                       
39640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49640, 9651, 49640                                    
49640 if (rvcomp + .05001) 29640,19640,49641                            
49641 if (rvcomp + .04991) 19640,19640,29640                            
19640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9651                                                        
29640 ivfail = ivfail + 1                                               
      rvcorr = -.04996                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 9651 continue                                                          
!                                                                        
!      WRITE PAGE FOOTINGS AND RUN SUMMARIES                             
99999 continue                                                          
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
      write (i02,90002)                                                 
      write (i02,90007)                                                 
      write (i02,90002)                                                 
      write (i02,90008)  ivfail                                         
      write (i02,90009) ivpass                                          
      write (i02,90010) ivdele                                          
!                                                                        
!                                                                        
!      TERMINATE ROUTINE EXECUTION                                       
      stop                                                              
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
90000 format ("1")                                                      
90002 format (" ")                                                      
90001 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90003 format (" ",21x,"VERSION 2.1" )                                   
90004 format (" ",10x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )        
90005 format (" ",5x,"TEST",5x,"PASS/FAIL", 5x,"COMPUTED",8x,"CORRECT") 
90006 format (" ",5x,"----------------------------------------------" ) 
90011 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARIES                               
90008 format (" ",15x,i5," ERRORS ENCOUNTERED" )                        
90009 format (" ",15x,i5," TESTS PASSED" )                              
90010 format (" ",15x,i5," TESTS DELETED" )                             
!                                                                        
!      FORMAT STATEMENTS FOR TEST RESULTS                                
80001 format (" ",4x,i5,7x,"PASS")                                      
80002 format (" ",4x,i5,7x,"FAIL")                                      
80003 format (" ",4x,i5,7x,"DELETED")                                   
80004 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80005 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
!                                                                        
90007 format (" ",20x,"END OF PROGRAM FM099" )                          
      end program fm099
