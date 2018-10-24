      program fm035
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivon02
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon03
!      COMMENT SECTION                                                   
!                                                                        
!      FM035                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM                                                              
!               INTEGER VARIABLE = ARITHMETIC EXPRESSION                 
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR *, INTEGER VARIABLES AND INTEGER CONSTANT.  SOME OF THE  
!      TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE EXPRESSION AND TO  
!      ALLOW THE USE OF NEGATIVE CONSTANTS FOLLOWING THE * OPERATOR.     
!      THE INTEGER VARIABLES CONTAIN POSITIVE AND NEGATIVE VALUES.       
!                                                                        
!      THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS          
!          (1)  INTEGER VARIABLE * INTEGER VARIABLE                      
!          (2)  INTEGER VARIABLE * INTEGER VARIABLE * INTEGER CONSTANT   
!               INTEGER VARIABLE * INTEGER CONSTANT * INTEGER VARIABLE   
!               INTEGER CONSTANT * INTEGER VARIABLE * INTEGER VARIABLE   
!          (3)  SAME AS (2) BUT WITH PARENTHESES TO GROUP ELEMENTS.      
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  
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
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!      TEST 430 THROUGH TEST 441 CONTAIN TWO INTEGER VARIABLES AND       
!      OPERATOR * IN AN ARITHMETIC EXPRESSION.                           
!          THE FORM IS   IV = IV * IV                                    
!                                                                        
!      TEST 430 THROUGH TEST 433  -  TWO POSITIVE VARIABLES              
!                                                                        
 4301 continue                                                          
      ivtnum = 430                                                      
!                                                                        
!       ****  TEST 430  ****                                             
!                                                                        
      if (iczero) 34300, 4300, 34300                                    
 4300 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 * ivon02                                          
      goto 44300                                                       
34300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44300, 4311, 44300                                    
44300 if (ivcomp - 6) 24300,14300,24300                                 
14300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4311                                                        
24300 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4311 continue                                                          
      ivtnum = 431                                                      
!                                                                        
!       ****  TEST 431  ****                                             
!                                                                        
      if (iczero) 34310, 4310, 34310                                    
 4310 continue                                                          
      ivon01 = 13                                                       
      ivon02 = 11                                                       
      ivcomp = ivon01 * ivon02                                          
      goto 44310                                                       
34310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44310, 4321, 44310                                    
44310 if (ivcomp - 143) 24310,14310,24310                               
14310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4321                                                        
24310 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4321 continue                                                          
      ivtnum = 432                                                      
!                                                                        
!       ****  TEST 432  ****                                             
!                                                                        
      if (iczero) 34320, 4320, 34320                                    
 4320 continue                                                          
      ivon01 = 223                                                      
      ivon02 = 99                                                       
      ivcomp = ivon01 * ivon02                                          
      goto 44320                                                       
34320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44320, 4331, 44320                                    
44320 if (ivcomp - 22077) 24320,14320,24320                             
14320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4331                                                        
24320 ivfail = ivfail + 1                                               
      ivcorr = 22077                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4331 continue                                                          
      ivtnum = 433                                                      
!                                                                        
!       ****  TEST 433  ****                                             
!                                                                        
      if (iczero) 34330, 4330, 34330                                    
 4330 continue                                                          
      ivon01 = 11235                                                    
      ivon02 = 2                                                        
      ivcomp = ivon01*ivon02                                            
      goto 44330                                                       
34330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44330, 4341, 44330                                    
44330 if (ivcomp - 22470) 24330,14330,24330                             
14330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4341                                                        
24330 ivfail = ivfail + 1                                               
      ivcorr = 22470                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 434 THROUGH TEST 437                                         
!           ONE NEGATIVE VARIABLE, ONE POSITIVE VARIABLE                 
!                                                                        
 4341 continue                                                          
      ivtnum = 434                                                      
!                                                                        
!       ****  TEST 434  ****                                             
!                                                                        
      if (iczero) 34340, 4340, 34340                                    
 4340 continue                                                          
      ivon01 = -2                                                       
      ivon02 = 3                                                        
      ivcomp = ivon01 * ivon02                                          
      goto 44340                                                       
34340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44340, 4351, 44340                                    
44340 if (ivcomp +6) 24340,14340,24340                                  
14340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4351                                                        
24340 ivfail = ivfail + 1                                               
      ivcorr = -6                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4351 continue                                                          
      ivtnum = 435                                                      
!                                                                        
!       ****  TEST 435  ****                                             
!                                                                        
      if (iczero) 34350, 4350, 34350                                    
 4350 continue                                                          
      ivon01 = -13                                                      
      ivon02 = +11                                                      
      ivcomp = ivon01*ivon02                                            
      goto 44350                                                       
34350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44350, 4361, 44350                                    
44350 if (ivcomp + 143) 24350,14350,24350                               
14350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4361                                                        
24350 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4361 continue                                                          
      ivtnum = 436                                                      
!                                                                        
!       ****  TEST 436  ****                                             
!                                                                        
      if (iczero) 34360, 4360, 34360                                    
 4360 continue                                                          
      ivon01 = -223                                                     
      ivon02 = 99                                                       
      ivcomp = ivon01 * ivon02                                          
      goto 44360                                                       
34360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44360, 4371, 44360                                    
44360 if (ivcomp + 22077) 24360,14360,24360                             
14360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4371                                                        
24360 ivfail = ivfail + 1                                               
      ivcorr = -22077                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4371 continue                                                          
      ivtnum = 437                                                      
!                                                                        
!       ****  TEST 437  ****                                             
!                                                                        
      if (iczero) 34370, 4370, 34370                                    
 4370 continue                                                          
      ivon01 = -11235                                                   
      ivon02 =  2                                                       
      ivcomp = ivon01 * ivon02                                          
      goto 44370                                                       
34370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44370, 4381, 44370                                    
44370 if (ivcomp + 22470) 24370,14370,24370                             
14370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4381                                                        
24370 ivfail = ivfail + 1                                               
      ivcorr = -22470                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 438 THROUGH TEST 441  -  TWO NEGATIVE VARIABLES              
 4381 continue                                                          
      ivtnum = 438                                                      
!                                                                        
!       ****  TEST 438  ****                                             
!                                                                        
      if (iczero) 34380, 4380, 34380                                    
 4380 continue                                                          
      ivon01 = -2                                                       
      ivon02 = -3                                                       
      ivcomp = ivon01 * ivon02                                          
      goto 44380                                                       
34380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44380, 4391, 44380                                    
44380 if (ivcomp - 6) 24380,14380,24380                                 
14380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4391                                                        
24380 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4391 continue                                                          
      ivtnum = 439                                                      
!                                                                        
!       ****  TEST 439  ****                                             
!                                                                        
      if (iczero) 34390, 4390, 34390                                    
 4390 continue                                                          
      ivon01 = -13                                                      
      ivon02 = -11                                                      
      ivcomp = ivon01 * ivon02                                          
      goto 44390                                                       
34390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44390, 4401, 44390                                    
44390 if (ivcomp - 143) 24390,14390,24390                               
14390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4401                                                        
24390 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4401 continue                                                          
      ivtnum = 440                                                      
!                                                                        
!       ****  TEST 440  ****                                             
!                                                                        
      if (iczero) 34400, 4400, 34400                                    
 4400 continue                                                          
      ivon01 = -223                                                     
      ivon02 = -99                                                      
      ivcomp = ivon01*ivon02                                            
      goto 44400                                                       
34400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44400, 4411, 44400                                    
44400 if (ivcomp - 22077) 24400,14400,24400                             
14400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4411                                                        
24400 ivfail = ivfail + 1                                               
      ivcorr = 22077                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4411 continue                                                          
      ivtnum = 441                                                      
!                                                                        
!       ****  TEST 441  ****                                             
!                                                                        
      if (iczero) 34410, 4410, 34410                                    
 4410 continue                                                          
      ivon01 = -5461                                                    
      ivon02 = -6                                                       
      ivcomp = ivon01 * ivon02                                          
      goto 44410                                                       
34410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44410, 4421, 44410                                    
44410 if (ivcomp - 32766) 24410, 14410, 24410                           
14410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4421                                                        
24410 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 442 THROUGH TEST 445 CONTAIN SIGNED INTEGER VARIABLES AND    
!      OPERATOR * IN AN ARITHMETIC EXPRESSION.                           
 4421 continue                                                          
      ivtnum = 442                                                      
!                                                                        
!       ****  TEST 442  ****                                             
!         FORM IS  IV = -IV*IV                                           
!                                                                        
      if (iczero) 34420, 4420, 34420                                    
 4420 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = -ivon01 * ivon02                                         
      goto 44420                                                       
34420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44420, 4431, 44420                                    
44420 if (ivcomp + 6) 24420,14420,24420                                 
14420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4431                                                        
24420 ivfail = ivfail + 1                                               
      ivcorr = -6                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4431 continue                                                          
      ivtnum = 443                                                      
!                                                                        
!       ****  TEST 443  ****                                             
!         FORM IS  IV = IV*(-IV)                                         
!                                                                        
      if (iczero) 34430, 4430, 34430                                    
 4430 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 * (-ivon02)                                       
      goto 44430                                                       
34430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44430, 4441, 44430                                    
44430 if (ivcomp +6) 24430,14430,24430                                  
14430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4441                                                        
24430 ivfail = ivfail + 1                                               
      ivcorr = -6                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4441 continue                                                          
      ivtnum = 444                                                      
!                                                                        
!       ****  TEST 444  ****                                             
!         FORM IS  IV = (-IV)*(-IV)                                      
!                                                                        
      if (iczero) 34440, 4440, 34440                                    
 4440 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = (-ivon01) * (-ivon02)                                    
      goto 44440                                                       
34440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44440, 4451, 44440                                    
44440 if (ivcomp - 6) 24440,14440,24440                                 
14440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4451                                                        
24440 ivfail = ivfail + 1                                               
      ivcorr =  6                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4451 continue                                                          
      ivtnum = 445                                                      
!                                                                        
!       ****  TEST 445  ****                                             
!         FORM IS   IV = -IV * IV                                        
!                                                                        
      if (iczero) 34450, 4450, 34450                                    
 4450 continue                                                          
      ivon01 = -11235                                                   
      ivon02 =  -2                                                      
      ivcomp = -ivon01 * ivon02                                         
      goto 44450                                                       
34450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44450, 4461, 44450                                    
44450 if (ivcomp + 22470) 24450,14450,24450                             
14450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4461                                                        
24450 ivfail = ivfail + 1                                               
      ivcorr = -22470                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 446 THROUGH TEST 452 CONTAIN TWO INTEGER VARIABLES, AN       
!      INTEGER CONSTANT AND OPERATOR * IN AN ARITHMETIC EXPRESSION.      
!                                                                        
 4461 continue                                                          
      ivtnum = 446                                                      
!                                                                        
!       ****  TEST 446  ****                                             
!                                                                        
      if (iczero) 34460, 4460, 34460                                    
 4460 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 * ivon02 * 4                                      
      goto 44460                                                       
34460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44460, 4471, 44460                                    
44460 if (ivcomp -24) 24460,14460,24460                                 
14460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4471                                                        
24460 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4471 continue                                                          
      ivtnum = 447                                                      
!                                                                        
!       ****  TEST 447  ****                                             
!                                                                        
      if (iczero) 34470, 4470, 34470                                    
 4470 continue                                                          
      ivon01 = -2                                                       
      ivon02 = 3                                                        
      ivcomp = ivon01 * ivon02 * 4                                      
      goto 44470                                                       
34470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44470, 4481, 44470                                    
44470 if (ivcomp +24) 24470,14470,24470                                 
14470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4481                                                        
24470 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4481 continue                                                          
      ivtnum = 448                                                      
!                                                                        
!       ****  TEST 448  ****                                             
!                                                                        
      if (iczero) 34480, 4480, 34480                                    
 4480 continue                                                          
      ivon01 = -2                                                       
      ivon02 = 3                                                        
      ivcomp = ivon01 * ivon02 * (-4)                                   
      goto 44480                                                       
34480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44480, 4491, 44480                                    
44480 if (ivcomp -24) 24480,14480,24480                                 
14480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4491                                                        
24480 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4491 continue                                                          
      ivtnum = 449                                                      
!                                                                        
!       ****  TEST 449  ****                                             
!                                                                        
      if (iczero) 34490, 4490, 34490                                    
 4490 continue                                                          
      ivon01 = 51                                                       
      ivon03 = 13                                                       
      ivcomp = ivon01 * 23 * ivon03                                     
      goto 44490                                                       
34490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44490, 4501, 44490                                    
44490 if (ivcomp - 15249) 24490,14490,24490                             
14490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4501                                                        
24490 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4501 continue                                                          
      ivtnum = 450                                                      
!                                                                        
!       ****  TEST 450  ****                                             
!                                                                        
      if (iczero) 34500, 4500, 34500                                    
 4500 continue                                                          
      ivon02 = 2                                                        
      ivon03 = 5461                                                     
      ivcomp = 3 * ivon02 * ivon03                                      
      goto 44500                                                       
34500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44500, 4511, 44500                                    
44500 if (ivcomp -32766) 24500,14500,24500                              
14500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4511                                                        
24500 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4511 continue                                                          
      ivtnum = 451                                                      
!                                                                        
!       ****  TEST 451  ****                                             
!                                                                        
      if (iczero) 34510, 4510, 34510                                    
 4510 continue                                                          
      ivon01 = -51                                                      
      ivon03 = 13                                                       
      ivcomp = ivon01 * 23 * (-ivon03)                                  
      goto 44510                                                       
34510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44510, 4521, 44510                                    
44510 if (ivcomp - 15249) 24510,14510,24510                             
14510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4521                                                        
24510 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4521 continue                                                          
      ivtnum = 452                                                      
!                                                                        
!       ****  TEST 452  ****                                             
!                                                                        
      if (iczero) 34520, 4520, 34520                                    
 4520 continue                                                          
      ivon01 = -5461                                                    
      ivon03 = 2                                                        
      ivcomp = ivon01 * (-3) * ivon03                                   
      goto 44520                                                       
34520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44520, 4531, 44520                                    
44520 if (ivcomp - 32766) 24520,14520,24520                             
14520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4531                                                        
24520 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 453 THROUGH TEST 461 CONTAIN TWO INTEGER VARIABLES AND ONE   
!      INTEGER CONSTANT IN AN ARITHMETIC EXPRESSION.  PARENTHESES ARE    
!      USED TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSIONS IN THESE     
!      TESTS.                                                            
!                                                                        
 4531 continue                                                          
      ivtnum = 453                                                      
!                                                                        
!       ****  TEST 453  ****                                             
!                                                                        
      if (iczero) 34530, 4530, 34530                                    
 4530 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 * (ivon02 * 4)                                    
      goto 44530                                                       
34530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44530, 4541, 44530                                    
44530 if (ivcomp - 24) 24530,14530,24530                                
14530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4541                                                        
24530 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4541 continue                                                          
      ivtnum = 454                                                      
!                                                                        
!       ****  TEST 454  ****                                             
!                                                                        
      if (iczero) 34540, 4540, 34540                                    
 4540 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = (ivon01 * ivon02) * 4                                    
      goto 44540                                                       
34540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44540, 4551, 44540                                    
44540 if (ivcomp -24) 24540,14540,24540                                 
14540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4551                                                        
24540 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4551 continue                                                          
      ivtnum = 455                                                      
!                                                                        
!       ****  TEST 455  ****                                             
!                                                                        
      if (iczero) 34550, 4550, 34550                                    
 4550 continue                                                          
      ivon01 = -2                                                       
      ivon02 = 3                                                        
      ivcomp = ivon01 *(ivon02 * (-4))                                  
      goto 44550                                                       
34550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44550, 4561, 44550                                    
44550 if (ivcomp - 24) 24550,14550,24550                                
14550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4561                                                        
24550 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4561 continue                                                          
      ivtnum = 456                                                      
!                                                                        
!       ****  TEST 456  ****                                             
!                                                                        
      if (iczero) 34560, 4560, 34560                                    
 4560 continue                                                          
      ivon01 = -2                                                       
      ivon02 = -3                                                       
      ivcomp = ivon01 * (ivon02 * 4)                                    
      goto 44560                                                       
34560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44560, 4571, 44560                                    
44560 if (ivcomp -24) 24560,14560,24560                                 
14560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4571                                                        
24560 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4571 continue                                                          
      ivtnum = 457                                                      
!                                                                        
!       ****  TEST 457  ****                                             
!                                                                        
      if (iczero) 34570, 4570, 34570                                    
 4570 continue                                                          
      ivon01 = -2                                                       
      ivon02 = -3                                                       
      ivcomp = (ivon01*ivon02) * (-4)                                   
      goto 44570                                                       
34570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44570, 4581, 44570                                    
44570 if (ivcomp +24) 24570,14570,24570                                 
14570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4581                                                        
24570 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4581 continue                                                          
      ivtnum = 458                                                      
!                                                                        
!       ****  TEST 458  ****                                             
!                                                                        
      if (iczero) 34580, 4580, 34580                                    
 4580 continue                                                          
      ivon01 = 23                                                       
      ivon03 = 13                                                       
      ivcomp = ivon01 * (51 * ivon03)                                   
      goto 44580                                                       
34580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44580, 4591, 44580                                    
44580 if (ivcomp -15249) 24580,14580,24580                              
14580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4591                                                        
24580 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4591 continue                                                          
      ivtnum = 459                                                      
!                                                                        
!       ****  TEST 459  ****                                             
!                                                                        
      if (iczero) 34590, 4590, 34590                                    
 4590 continue                                                          
      ivon02 = 51                                                       
      ivon03 = 13                                                       
      ivcomp = (23 * ivon02) * ivon03                                   
      goto 44590                                                       
34590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44590, 4601, 44590                                    
44590 if (ivcomp - 15249) 24590,14590,24590                             
14590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4601                                                        
24590 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4601 continue                                                          
      ivtnum = 460                                                      
!                                                                        
!       ****  TEST 460  ****                                             
!                                                                        
      if (iczero) 34600, 4600, 34600                                    
 4600 continue                                                          
      ivon01 = -23                                                      
      ivon03 = 13                                                       
      ivcomp = (ivon01 * (-51)) * (-ivon03)                             
      goto 44600                                                       
34600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44600, 4611, 44600                                    
44600 if (ivcomp + 15249) 24600,14600,24600                             
14600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4611                                                        
24600 ivfail = ivfail + 1                                               
      ivcorr = -15249                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4611 continue                                                          
      ivtnum = 461                                                      
!                                                                        
!       ****  TEST 461  ****                                             
!                                                                        
      if (iczero) 34610, 4610, 34610                                    
 4610 continue                                                          
      ivon02 = 51                                                       
      ivon03 = 13                                                       
      ivcomp = -23 * (ivon02*ivon03)                                    
      goto 44610                                                       
34610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44610, 4621, 44610                                    
44610 if (ivcomp + 15249) 24610,14610,24610                             
14610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4621                                                        
24610 ivfail = ivfail + 1                                               
      ivcorr = -15249                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!       ****    END OF TESTS    ****                                     
 4621 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM035" )                          
      end program fm035
