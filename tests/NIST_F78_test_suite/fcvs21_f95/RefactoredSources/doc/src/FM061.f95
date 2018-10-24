      program fm061
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
      real :: rvcomp
      real :: rvcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM061                                                             
!                                                                        
!           THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE   
!      FORM                                                              
!                    INTEGER VARIABLE = REAL CONSTANT                    
!                    INTEGER VARIABLE = REAL VARIABLE                    
!                    REAL VARIABLE = INTEGER VARIABLE                    
!                    REAL VARIABLE = INTEGER CONSTANT                    
!                                                                        
!      THE CONSTANTS AND VARIABLES CONTAIN BOTH POSITIVE AND NEGATIVE    
!      VALUES.                                                           
!                                                                        
!            A REAL DATUM IS A PROCESSOR APPROXIMATION TO THE VALUE OF A 
!      REAL NUMBER.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES.   
!                                                                        
!           A BASIC REAL CONSTANT IS WRITTEN AS AN INTEGER PART, A       
!      DECIMAL POINT, AND A DECIMAL FRACTION PART IN THAT ORDER.  BOTH   
!      THE INTEGER PART AND THE DECIMAL PART ARE STRINGS OF DIGITS;      
!      EITHER ONE OF THESE STRINGS MAY BE EMPTY BUT NOT BOTH.  THE       
!      CONSTANT IS AN APPROXIMATION TO THE DIGIT STRING INTERPRETED AS A 
!      DECIMAL NUMERAL.                                                  
!                                                                        
!          A DECIMAL EXPONENT IS WRITTEN AS THE LETTER E, FOLLOWED BY AN 
!      OPTIONALLY SIGNED INTEGER CONSTANT.                               
!                                                                        
!          A REAL CONSTANT IS INDICATED BY WRITING A BASIC REAL CONSTANT,
!      A BASIC REAL CONSTANT FOLLOWED BY A DECIMAL EXPONENT, OR AN       
!      INTEGER CONSTANT FOLLOWED BY A DECIMAL EXPONENT.                  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.4, REAL TYPE                                         
!         SECTION 4.4.1, REAL CONSTANT                                   
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 6.6, EVALUATION OF EXPRESSIONS                         
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  
!         SECTION 11.4, ARITHMETIC IF STATEMENT                          
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
!      TEST 32 THROUGH TEST 42 CONTAIN ARITHMETIC ASSIGNMENT             
!      STATEMENTS OF THE FORM                                            
!                                                                        
!                    INTEGER VARIABLE = REAL VARIABLE                    
!                                                                        
      ivtnum =  32                                                      
!                                                                        
!       ****  TEST  32  ****                                             
!                                                                        
      if (iczero) 30320,  320, 30320                                    
  320 continue                                                          
      rvon01 = 44.5                                                     
      ivcomp = rvon01                                                   
      goto 40320                                                       
30320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40320,  331, 40320                                    
40320 if (ivcomp - 44) 20320,10320,20320                                
10320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  331                                                        
20320 ivfail = ivfail + 1                                               
      ivcorr = 44                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  331 continue                                                          
      ivtnum =  33                                                      
!                                                                        
!       ****  TEST  33  ****                                             
!                                                                        
      if (iczero) 30330,  330, 30330                                    
  330 continue                                                          
      rvon01 = -2.0005                                                  
      ivcomp = rvon01                                                   
      goto 40330                                                       
30330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40330,  341, 40330                                    
40330 if (ivcomp + 2) 20330,10330,20330                                 
10330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  341                                                        
20330 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  341 continue                                                          
      ivtnum =  34                                                      
!                                                                        
!       ****  TEST  34  ****                                             
!                                                                        
      if (iczero) 30340,  340, 30340                                    
  340 continue                                                          
      rvon01 = .32767                                                   
      ivcomp = rvon01                                                   
      goto 40340                                                       
30340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40340,  351, 40340                                    
40340 if (ivcomp) 20340,10340,20340                                     
10340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  351                                                        
20340 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  351 continue                                                          
      ivtnum =  35                                                      
!                                                                        
!       ****  TEST  35  ****                                             
!                                                                        
      if (iczero) 30350,  350, 30350                                    
  350 continue                                                          
      rvon01 = 1.999                                                    
      ivcomp = rvon01                                                   
      goto 40350                                                       
30350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40350,  361, 40350                                    
40350 if (ivcomp - 1) 20350,10350,20350                                 
10350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  361                                                        
20350 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  361 continue                                                          
      ivtnum =  36                                                      
!                                                                        
!       ****  TEST  36  ****                                             
!                                                                        
      if (iczero) 30360,  360, 30360                                    
  360 continue                                                          
      rvon01 = .25e+1                                                   
      ivcomp = rvon01                                                   
      goto 40360                                                       
30360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40360,  371, 40360                                    
40360 if (ivcomp - 2) 20360,10360,20360                                 
10360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  371                                                        
20360 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  371 continue                                                          
      ivtnum =  37                                                      
!                                                                        
!       ****  TEST  37  ****                                             
!                                                                        
      if (iczero) 30370,  370, 30370                                    
  370 continue                                                          
      rvon01 = 445.0e-01                                                
      ivcomp = rvon01                                                   
      goto 40370                                                       
30370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40370,  381, 40370                                    
40370 if (ivcomp - 44) 20370,10370,20370                                
10370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  381                                                        
20370 ivfail = ivfail + 1                                               
      ivcorr = 44                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  381 continue                                                          
      ivtnum =  38                                                      
!                                                                        
!       ****  TEST  38  ****                                             
!                                                                        
      if (iczero) 30380,  380, 30380                                    
  380 continue                                                          
      rvon01 = -651.1e-0                                                
      ivcomp = rvon01                                                   
      goto 40380                                                       
30380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40380,  391, 40380                                    
40380 if (ivcomp + 651) 20380,10380,20380                               
10380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  391                                                        
20380 ivfail = ivfail + 1                                               
      ivcorr = -651                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  391 continue                                                          
      ivtnum =  39                                                      
!                                                                        
!       ****  TEST  39  ****                                             
!                                                                        
      if (iczero) 30390,  390, 30390                                    
  390 continue                                                          
      rvon01 = .3266e4                                                  
      ivcomp = rvon01                                                   
      goto 40390                                                       
30390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40390,  401, 40390                                    
40390 if (ivcomp - 3266) 20390,10390,20390                              
10390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  401                                                        
20390 ivfail = ivfail + 1                                               
      ivcorr = 3266                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  401 continue                                                          
      ivtnum =  40                                                      
!                                                                        
!       ****  TEST  40  ****                                             
!                                                                        
      if (iczero) 30400,  400, 30400                                    
  400 continue                                                          
      rvon01 = 35.43e-01                                                
      ivcomp = rvon01                                                   
      goto 40400                                                       
30400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40400,  411, 40400                                    
40400 if (ivcomp - 3) 20400,10400,20400                                 
10400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  411                                                        
20400 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  411 continue                                                          
      ivtnum =  41                                                      
!                                                                        
!       ****  TEST  41  ****                                             
!                                                                        
      if (iczero) 30410,  410, 30410                                    
  410 continue                                                          
      rvon01 = -7.001e2                                                 
      ivcomp = rvon01                                                   
      goto 40410                                                       
30410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40410,  421, 40410                                    
40410 if (ivcomp + 700) 20410,10410,20410                               
10410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  421                                                        
20410 ivfail = ivfail + 1                                               
      ivcorr = -700                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  421 continue                                                          
      ivtnum =  42                                                      
!                                                                        
!       ****  TEST  42  ****                                             
!                                                                        
      if (iczero) 30420,  420, 30420                                    
  420 continue                                                          
      rvon01 = 4.45e-02                                                 
      ivcomp = rvon01                                                   
      goto 40420                                                       
30420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40420,  431, 40420                                    
40420 if (ivcomp) 20420,10420,20420                                     
10420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  431                                                        
20420 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!      TEST 43 THROUGH TEST 48 CONTAIN ARITHMETIC ASSIGNMENT             
!      STATEMENTS OF THE FORM                                            
!                                                                        
!                    REAL VARIABLE = INTEGER VARIABLE                    
!                                                                        
  431 continue                                                          
      ivtnum =  43                                                      
!                                                                        
!       ****  TEST  43  ****                                             
!                                                                        
      if (iczero) 30430,  430, 30430                                    
  430 continue                                                          
      ivon01 = 2                                                        
      rvcomp = ivon01                                                   
      goto 40430                                                       
30430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40430,  441, 40430                                    
40430 if (rvcomp - 1.9995) 20430,10430,40431                            
40431 if (rvcomp - 2.0005) 10430,10430,20430                            
10430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  441                                                        
20430 ivfail = ivfail + 1                                               
      rvcorr = 2.0000                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  441 continue                                                          
      ivtnum =  44                                                      
!                                                                        
!       ****  TEST  44  ****                                             
!                                                                        
      if (iczero) 30440,  440, 30440                                    
  440 continue                                                          
      ivon01 = 25                                                       
      rvcomp = ivon01                                                   
      goto 40440                                                       
30440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40440,  451, 40440                                    
40440 if (rvcomp - 24.995) 20440,10440,40441                            
40441 if (rvcomp - 25.005) 10440,10440,20440                            
10440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  451                                                        
20440 ivfail = ivfail + 1                                               
      rvcorr = 25.000                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  451 continue                                                          
      ivtnum =  45                                                      
!                                                                        
!       ****  TEST  45  ****                                             
!                                                                        
      if (iczero) 30450,  450, 30450                                    
  450 continue                                                          
      ivon01 = 357                                                      
      rvcomp = ivon01                                                   
      goto 40450                                                       
30450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40450,  461, 40450                                    
40450 if (rvcomp - 356.95) 20450,10450,40451                            
40451 if (rvcomp - 357.05) 10450,10450,20450                            
10450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  461                                                        
20450 ivfail = ivfail + 1                                               
      rvcorr = 357.00                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  461 continue                                                          
      ivtnum =  46                                                      
!                                                                        
!       ****  TEST  46  ****                                             
!                                                                        
      if (iczero) 30460,  460, 30460                                    
  460 continue                                                          
      ivon01 = 4968                                                     
      rvcomp = ivon01                                                   
      goto 40460                                                       
30460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40460,  471, 40460                                    
40460 if (rvcomp - 4967.5) 20460,10460,40461                            
40461 if (rvcomp - 4968.5) 10460,10460,20460                            
10460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  471                                                        
20460 ivfail = ivfail + 1                                               
      rvcorr = 4968.0                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  471 continue                                                          
      ivtnum =  47                                                      
!                                                                        
!       ****  TEST  47  ****                                             
!                                                                        
      if (iczero) 30470,  470, 30470                                    
  470 continue                                                          
      ivon01 = 32767                                                    
      rvcomp = ivon01                                                   
      goto 40470                                                       
30470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40470,  481, 40470                                    
40470 if (rvcomp - 32762.) 20470,10470,40471                            
40471 if (rvcomp - 32772.) 10470,10470,20470                            
10470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  481                                                        
20470 ivfail = ivfail + 1                                               
      rvcorr = 32767.                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  481 continue                                                          
      ivtnum =  48                                                      
!                                                                        
!       ****  TEST  48  ****                                             
!                                                                        
      if (iczero) 30480,  480, 30480                                    
  480 continue                                                          
      ivon01 = -2                                                       
      rvcomp = ivon01                                                   
      goto 40480                                                       
30480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40480,  491, 40480                                    
40480 if (rvcomp + 2.0005) 20480,10480,40481                            
40481 if (rvcomp + 1.9995) 10480,10480,20450                            
10480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  491                                                        
20480 ivfail = ivfail + 1                                               
      rvcorr = -2.0000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
!                                                                        
!      TEST 49 THROUGH TEST 51 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS  
!      OF THE FORM                                                       
!                    INTEGER VARIABLE = REAL CONSTANT                    
!      WHERE CONSTANT IS BASIC REAL CONSTANT                             
!                                                                        
  491 continue                                                          
      ivtnum =  49                                                      
!                                                                        
!       ****  TEST  49  ****                                             
!                                                                        
      if (iczero) 30490,  490, 30490                                    
  490 continue                                                          
      ivcomp = 44.5                                                     
      goto 40490                                                       
30490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40490,  501, 40490                                    
40490 if (ivcomp - 44) 20490,10490,20490                                
10490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  501                                                        
20490 ivfail = ivfail + 1                                               
      ivcorr = 44                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  501 continue                                                          
      ivtnum =  50                                                      
!                                                                        
!       ****  TEST  50  ****                                             
!                                                                        
      if (iczero) 30500,  500, 30500                                    
  500 continue                                                          
      ivcomp = 6500.1                                                   
      goto 40500                                                       
30500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40500,  511, 40500                                    
40500 if (ivcomp - 6500)  20500,10500,20500                             
10500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  511                                                        
20500 ivfail = ivfail + 1                                               
      ivcorr = 6500                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  511 continue                                                          
      ivtnum =  51                                                      
!                                                                        
!       ****  TEST  51  ****                                             
!                                                                        
      if (iczero) 30510,  510, 30510                                    
  510 continue                                                          
      ivcomp = -.33333                                                  
      goto 40510                                                       
30510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40510,  521, 40510                                    
40510 if (ivcomp) 20510,10510,20510                                     
10510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  521                                                        
20510 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 52 THROUGH TEST 55 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS  
!      OF THE FORM                                                       
!                    INTEGER VARIABLE = REAL CONSTANT                    
!                                                                        
!      WHERE CONSTANT IS BASIC REAL CONSTANT FOLLOWED BY DECIMAL EXPONENT
!                                                                        
  521 continue                                                          
      ivtnum =  52                                                      
!                                                                        
!       ****  TEST  52  ****                                             
!                                                                        
      if (iczero) 30520,  520, 30520                                    
  520 continue                                                          
      ivcomp = .21e+1                                                   
      goto 40520                                                       
30520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40520,  531, 40520                                    
40520 if (ivcomp - 2) 20520,10520,20520                                 
10520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  531                                                        
20520 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  531 continue                                                          
      ivtnum =  53                                                      
!                                                                        
!       ****  TEST  53  ****                                             
!                                                                        
      if (iczero) 30530,  530, 30530                                    
  530 continue                                                          
      ivcomp = 445.0e-01                                                
      goto 40530                                                       
30530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40530,  541, 40530                                    
40530 if (ivcomp - 44) 20530,10530,20530                                
10530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  541                                                        
20530 ivfail = ivfail + 1                                               
      ivcorr = 44                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  541 continue                                                          
      ivtnum =  54                                                      
!                                                                        
!       ****  TEST  54  ****                                             
!                                                                        
      if (iczero) 30540,  540, 30540                                    
  540 continue                                                          
      ivcomp = 4.450e1                                                  
      goto 40540                                                       
30540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40540,  551, 40540                                    
40540 if (ivcomp - 44) 20540,10540,20540                                
10540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  551                                                        
20540 ivfail = ivfail + 1                                               
      ivcorr = 44                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  551 continue                                                          
      ivtnum =  55                                                      
!                                                                        
!       ****  TEST  55  ****                                             
!                                                                        
      if (iczero) 30550,  550, 30550                                    
  550 continue                                                          
      ivcomp = -4.45e0                                                  
      goto 40550                                                       
30550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40550,  561, 40550                                    
40550 if (ivcomp + 4) 20550,10550,20550                                 
10550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  561                                                        
20550 ivfail = ivfail + 1                                               
      ivcorr = -4                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 56 AND 57 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM          INTEGER VARIABLE = REAL CONSTANT                    
!      WHERE CONSTANT IS INTEGER CONSTANT FOLLOWED BY DECIMAL EXPONENT   
!                                                                        
  561 continue                                                          
      ivtnum =  56                                                      
!                                                                        
!       ****  TEST  56  ****                                             
!                                                                        
      if (iczero) 30560,  560, 30560                                    
  560 continue                                                          
      ivcomp = 445e-02                                                  
      goto 40560                                                       
30560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40560,  571, 40560                                    
40560 if (ivcomp - 4) 20560,10560,20560                                 
10560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  571                                                        
20560 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
  571 continue                                                          
      ivtnum =  57                                                      
!                                                                        
!       ****  TEST  57  ****                                             
!                                                                        
      if (iczero) 30570,  570, 30570                                    
  570 continue                                                          
      ivcomp = -701e-1                                                  
      goto 40570                                                       
30570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40570,  581, 40570                                    
40570 if (ivcomp + 70) 20570,10570,20570                                
10570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  581                                                        
20570 ivfail = ivfail + 1                                               
      ivcorr = -70                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 58 THROUGH TEST 62 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS  
!      OF THE FORM   REAL VARIABLE = INTEGER CONSTANT                    
!                                                                        
  581 continue                                                          
      ivtnum =  58                                                      
!                                                                        
!       ****  TEST  58  ****                                             
!                                                                        
      if (iczero) 30580,  580, 30580                                    
  580 continue                                                          
      rvcomp = 23                                                       
      goto 40580                                                       
30580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40580,  591, 40580                                    
40580 if (rvcomp - 22.995) 20580,10580,40581                            
40581 if (rvcomp - 23.005) 10580,10580,20580                            
10580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  591                                                        
20580 ivfail = ivfail + 1                                               
      rvcorr = 23.000                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  591 continue                                                          
      ivtnum =  59                                                      
!                                                                        
!       ****  TEST  59  ****                                             
!                                                                        
      if (iczero) 30590,  590, 30590                                    
  590 continue                                                          
      rvcomp = 32645                                                    
      goto 40590                                                       
30590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40590,  601, 40590                                    
40590 if (rvcomp - 32640.) 20590,10590,40591                            
40591 if (rvcomp - 32650.) 10590,10590,20590                            
10590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  601                                                        
20590 ivfail = ivfail + 1                                               
      rvcorr = 32645.                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  601 continue                                                          
      ivtnum =  60                                                      
!                                                                        
!       ****  TEST  60  ****                                             
!                                                                        
      if (iczero) 30600,  600, 30600                                    
  600 continue                                                          
      rvcomp = 0                                                        
      goto 40600                                                       
30600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40600,  611, 40600                                    
40600 if (rvcomp) 20600,10600,20600                                     
10600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  611                                                        
20600 ivfail = ivfail + 1                                               
      rvcorr = 00000.                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  611 continue                                                          
      ivtnum =  61                                                      
!                                                                        
!       ****  TEST  61  ****                                             
!                                                                        
      if (iczero) 30610,  610, 30610                                    
  610 continue                                                          
      rvcomp = -15                                                      
      goto 40610                                                       
30610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40610,  621, 40610                                    
40610 if (rvcomp -14.995) 40611,10610,20610                             
40611 if (rvcomp + 15.005) 20610,10610,10610                            
10610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  621                                                        
20610 ivfail = ivfail + 1                                               
      rvcorr = -15.000                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  621 continue                                                          
!                                                                        
!       ****    END OF TESTS    ****                                     
!                                                                        
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
90007 format (" ",20x,"END OF PROGRAM FM061" )                          
      end program fm061
