      program fm009
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
!      COMMENT SECTION.                                                  
!                                                                        
!      FM009                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM                                                              
!              INTEGER VARIABLE = ARITHMETIC EXPRESSION                  
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR +, INTEGER CONSTANTS AND POSITIVE INTEGER VARIABLES.     
!      SOME OF THE TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE        
!      ARITHMETIC EXPRESSION.                                            
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!             (1)  TWO INTEGER VARIABLES,                                
!             (2)  TWO INTEGER VARIABLES AND ONE INTEGER CONSTANT,       
!             (3)  TWO INTEGER VARIABLES AND ONE INTEGER CONSTANT WITH   
!                    PARENTHESES TO GROUP ELEMENTS.                      
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENTS                 
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
!      TEST 235 THROUGH TEST 243 CONTAIN TWO POSITIVE INTEGER VARIABLES  
!      AND OPERATOR + IN ARITHMETIC EXPRESSION.                          
!                                                                        
 2351 continue                                                          
      ivtnum = 235                                                      
!                                                                        
!       ****  TEST 235  ****                                             
!                                                                        
      if (iczero) 32350, 2350, 32350                                    
 2350 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 + ivon02                                          
      goto 42350                                                       
32350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42350, 2361, 42350                                    
42350 if (ivcomp - 5) 22350,12350,22350                                 
12350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2361                                                        
22350 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2361 continue                                                          
      ivtnum = 236                                                      
!                                                                        
!       ****  TEST 236  ****                                             
!                                                                        
      if (iczero) 32360, 2360, 32360                                    
 2360 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon02 + ivon01                                          
      goto 42360                                                       
32360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42360, 2371, 42360                                    
42360 if (ivcomp - 5) 22360, 12360, 22360                               
12360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2371                                                        
22360 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2371 continue                                                          
      ivtnum = 237                                                      
!                                                                        
!       ****  TEST 237  ****                                             
!                                                                        
      if (iczero) 32370, 2370, 32370                                    
 2370 continue                                                          
      ivon01 = 51                                                       
      ivon02 = 52                                                       
      ivcomp = ivon01 + ivon02                                          
      goto 42370                                                       
32370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42370, 2381, 42370                                    
42370 if (ivcomp - 103) 22370, 12370, 22370                             
12370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2381                                                        
22370 ivfail = ivfail + 1                                               
      ivcorr = 103                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2381 continue                                                          
      ivtnum = 238                                                      
!                                                                        
!       ****  TEST  238 ****                                             
!                                                                        
      if (iczero) 32380, 2380, 32380                                    
 2380 continue                                                          
      ivon01 = 189                                                      
      ivon02 = 676                                                      
      ivcomp = ivon01 + ivon02                                          
      goto 42380                                                       
32380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42380, 2391, 42380                                    
42380 if (ivcomp - 865) 22380, 12380, 22380                             
12380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2391                                                        
22380 ivfail = ivfail + 1                                               
      ivcorr = 865                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2391 continue                                                          
      ivtnum = 239                                                      
!                                                                        
!       ****  TEST 239  ****                                             
!                                                                        
      if (iczero) 32390, 2390, 32390                                    
 2390 continue                                                          
      ivon01 = 1358                                                     
      ivon02 = 8001                                                     
      ivcomp = ivon01 + ivon02                                          
      goto 42390                                                       
32390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42390, 2401, 42390                                    
42390 if (ivcomp - 9359) 22390, 12390, 22390                            
12390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2401                                                        
22390 ivfail = ivfail + 1                                               
      ivcorr = 9359                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2401 continue                                                          
      ivtnum = 240                                                      
!                                                                        
!       ****  TEST 240  ****                                             
!                                                                        
      if (iczero) 32400, 2400, 32400                                    
 2400 continue                                                          
      ivon01 = 1358                                                     
      ivon02 = 8001                                                     
      ivcomp = ivon02 + ivon01                                          
      goto 42400                                                       
32400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42400, 2411, 42400                                    
42400 if (ivcomp - 9359) 22400, 12400, 22400                            
12400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2411                                                        
22400 ivfail = ivfail + 1                                               
      ivcorr = 9359                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2411 continue                                                          
      ivtnum = 241                                                      
!                                                                        
!       ****  TEST 241  ****                                             
!                                                                        
      if (iczero) 32410, 2410, 32410                                    
 2410 continue                                                          
      ivon01 = 11112                                                    
      ivon02 = 10001                                                    
      ivcomp = ivon01 + ivon02                                          
      goto 42410                                                       
32410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42410, 2421, 42410                                    
42410 if (ivcomp - 21113) 22410, 12410, 22410                           
12410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2421                                                        
22410 ivfail = ivfail + 1                                               
      ivcorr = 21113                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2421 continue                                                          
      ivtnum = 242                                                      
!                                                                        
!       **** TEST 242  ****                                              
!                                                                        
      if (iczero) 32420, 2420, 32420                                    
 2420 continue                                                          
      ivon01 = 189                                                      
      ivon02 = 9876                                                     
      ivcomp = ivon01 + ivon02                                          
      goto 42420                                                       
32420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42420, 2431, 42420                                    
42420 if (ivcomp - 10065) 22420, 12420, 22420                           
12420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2431                                                        
22420 ivfail = ivfail + 1                                               
      ivcorr = 10065                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2431 continue                                                          
      ivtnum = 243                                                      
!                                                                        
!       **** TEST 243  ****                                              
!          REQUIRES 32767                                                
!                                                                        
      if (iczero) 32430, 2430, 32430                                    
 2430 continue                                                          
      ivon01 = 16383                                                    
      ivon02 = 16384                                                    
      ivcomp = ivon01 + ivon02                                          
      goto 42430                                                       
32430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42430, 2441, 42430                                    
42430 if (ivcomp - 32767) 22430, 12430, 22430                           
12430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2441                                                        
22430 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 244 THROUGH TEST 250 CONTAIN TWO POSITIVE INTEGER VARIABLES, 
!      ONE INTEGER CONSTANT, AND OPERATOR + IN ARITHMETIC EXPRESSION.    
!                                                                        
 2441 continue                                                          
      ivtnum = 244                                                      
!                                                                        
!       ****  TEST 244  ****                                             
!                                                                        
      if (iczero) 32440, 2440, 32440                                    
 2440 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 + ivon02 + 4                                      
      goto 42440                                                       
32440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42440, 2451, 42440                                    
42440 if (ivcomp - 9) 22440, 12440, 22440                               
12440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2451                                                        
22440 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2451 continue                                                          
      ivtnum = 245                                                      
!                                                                        
!       ****  TEST 245  ****                                             
!                                                                        
      if (iczero) 32450, 2450, 32450                                    
 2450 continue                                                          
      ivon01 = 2                                                        
      ivon03 = 4                                                        
      ivcomp = ivon01 +3 + ivon03                                       
      goto 42450                                                       
32450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42450, 2461, 42450                                    
42450 if (ivcomp - 9) 22450, 12450, 22450                               
12450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2461                                                        
22450 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2461 continue                                                          
      ivtnum = 246                                                      
!                                                                        
!       ****  TEST 246  ****                                             
!                                                                        
      if (iczero) 32460, 2460, 32460                                    
 2460 continue                                                          
      ivon02 = 3                                                        
      ivon03 = 4                                                        
      ivcomp = 2 + ivon02 + ivon03                                      
      goto 42460                                                       
32460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42460, 2471, 42460                                    
42460 if (ivcomp - 9) 22460, 12460, 22460                               
12460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2471                                                        
22460 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2471 continue                                                          
      ivtnum = 247                                                      
!                                                                        
!       ****  TEST 247  ****                                             
!                                                                        
      if (iczero) 32470, 2470, 32470                                    
 2470 continue                                                          
      ivon01 = 51                                                       
      ivon03 = 53                                                       
      ivcomp = ivon01 +52 + ivon03                                      
      goto 42470                                                       
32470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42470, 2481, 42470                                    
42470 if (ivcomp - 156) 22470, 12470, 22470                             
12470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2481                                                        
22470 ivfail = ivfail + 1                                               
      ivcorr = 156                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2481 continue                                                          
      ivtnum = 248                                                      
!                                                                        
!       ****  TEST 248  ****                                             
!                                                                        
      if (iczero) 32480, 2480, 32480                                    
 2480 continue                                                          
      ivon02 = 676                                                      
      ivon03 = 101                                                      
      ivcomp = 189 + ivon02 + ivon03                                    
      goto 42480                                                       
32480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42480, 2491, 42480                                    
42480 if (ivcomp - 966) 22480, 12480, 22480                             
12480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2491                                                        
22480 ivfail = ivfail + 1                                               
      ivcorr = 966                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2491 continue                                                          
      ivtnum = 249                                                      
!                                                                        
!       ****  TEST 249  ****                                             
!                                                                        
      if (iczero) 32490, 2490, 32490                                    
 2490 continue                                                          
      ivon01 = 1358                                                     
      ivon02 = 8001                                                     
      ivcomp = ivon01 + ivon02 + 2189                                   
      goto 42490                                                       
32490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42490, 2501, 42490                                    
42490 if (ivcomp - 11548) 22490, 12490, 22490                           
12490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2501                                                        
22490 ivfail = ivfail + 1                                               
      ivcorr = 11548                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2501 continue                                                          
      ivtnum = 250                                                      
!                                                                        
!       ****  TEST 250  ****                                             
!          REQUIRES 32767                                                
!                                                                        
      if (iczero) 32500, 2500, 32500                                    
 2500 continue                                                          
      ivon01 = 16383                                                    
      ivon03 = 4                                                        
      ivcomp = ivon01 + 16380 + ivon03                                  
      goto 42500                                                       
32500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42500, 2511, 42500                                    
42500 if (ivcomp - 32767) 22500,12500,22500                             
12500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2511                                                        
22500 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 251 THROUGH TEST 264 CONTAIN TWO POSITIVE INTEGER VARIABLES, 
!      ONE INTEGER CONSTANT, AND OPERATOR + IN ARITHMETIC EXPRESSION.    
!      PARENTHESES ARE USED TO GROUP ELEMENTS.  THE RESULTS ARE THE SAME 
!      AS TESTS 244 THROUGH 250.                                         
!                                                                        
 2511 continue                                                          
      ivtnum = 251                                                      
!                                                                        
!       ****  TEST 251  ****                                             
!                                                                        
      if (iczero) 32510, 2510, 32510                                    
 2510 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = (ivon01 + ivon02) + 4                                    
      goto 42510                                                       
32510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42510, 2521, 42510                                    
42510 if (ivcomp - 9) 22510,12510,22510                                 
12510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2521                                                        
22510 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2521 continue                                                          
      ivtnum = 252                                                      
!                                                                        
!       ****  TEST 252  ****                                             
!                                                                        
      if (iczero) 32520, 2520, 32520                                    
 2520 continue                                                          
      ivon02 = 3                                                        
      ivon03 = 4                                                        
      ivcomp = 2 + (ivon02 + ivon03)                                    
      goto 42520                                                       
32520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42520, 2531, 42520                                    
42520 if (ivcomp - 9) 22520,12520,22520                                 
12520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2531                                                        
22520 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2531 continue                                                          
      ivtnum = 253                                                      
!                                                                        
!       **** TEST 253  ****                                              
!                                                                        
      if (iczero) 32530, 2530, 32530                                    
 2530 continue                                                          
      ivon02 =3                                                         
      ivon03 =4                                                         
      ivcomp = (2+ivon02)+ivon03                                        
      goto 42530                                                       
32530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42530, 2541, 42530                                    
42530 if (ivcomp -9) 22530,12530,22530                                  
12530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2541                                                        
22530 ivfail = ivfail + 1                                               
      ivcorr =9                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2541 continue                                                          
      ivtnum = 254                                                      
!                                                                        
!       ****  TEST 254  ****                                             
!                                                                        
      if (iczero) 32540, 2540, 32540                                    
 2540 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 + (ivon02 + 4)                                    
      goto 42540                                                       
32540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42540, 2551, 42540                                    
42540 if (ivcomp-9)22540,12540,22540                                    
12540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2551                                                        
22540 ivfail = ivfail + 1                                               
      ivcorr=9                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2551 continue                                                          
      ivtnum = 255                                                      
!                                                                        
!       ****  TEST 255  ****                                             
!                                                                        
      if (iczero) 32550, 2550, 32550                                    
 2550 continue                                                          
      ivon01 = 2                                                        
      ivon03 = 4                                                        
      ivcomp = ivon01 +(3+ivon03)                                       
      goto 42550                                                       
32550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42550, 2561, 42550                                    
42550 if (ivcomp-9)22550,12550,22550                                    
12550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2561                                                        
22550 ivfail = ivfail + 1                                               
      ivcorr =9                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2561 continue                                                          
      ivtnum = 256                                                      
!                                                                        
!       ****  TEST 256  ****                                             
!                                                                        
      if (iczero) 32560, 2560, 32560                                    
 2560 continue                                                          
      ivon01 = 2                                                        
      ivon03 = 4                                                        
      ivcomp =(ivon01+3)+ivon03                                         
      goto 42560                                                       
32560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42560, 2571, 42560                                    
42560 if (ivcomp-9) 22560,12560,22560                                   
12560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2571                                                        
22560 ivfail = ivfail + 1                                               
      ivcorr =9                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2571 continue                                                          
      ivtnum = 257                                                      
!                                                                        
!       ****  TEST 257  ****                                             
!                                                                        
      if (iczero) 32570, 2570, 32570                                    
 2570 continue                                                          
      ivon01 = 51                                                       
      ivon03 = 53                                                       
      ivcomp=ivon01+(52+ivon03)                                         
      goto 42570                                                       
32570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42570, 2581, 42570                                    
42570 if (ivcomp -156) 22570,12570,22570                                
12570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2581                                                        
22570 ivfail = ivfail + 1                                               
      ivcorr = 156                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2581 continue                                                          
      ivtnum = 258                                                      
!                                                                        
!       ****  TEST 258  ****                                             
!                                                                        
      if (iczero) 32580, 2580, 32580                                    
 2580 continue                                                          
      ivon01 = 51                                                       
      ivon03 = 53                                                       
      ivcomp =(ivon01+52)+ivon03                                        
      goto 42580                                                       
32580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42580, 2591, 42580                                    
42580 if (ivcomp-156) 22580,12580,22580                                 
12580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2591                                                        
22580 ivfail = ivfail + 1                                               
      ivcorr = 156                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2591 continue                                                          
      ivtnum = 259                                                      
!                                                                        
!       ****  TEST 259  ****                                             
!                                                                        
      if (iczero) 32590, 2590, 32590                                    
 2590 continue                                                          
      ivon02 = 676                                                      
      ivon03 = 101                                                      
      ivcomp = 189+(ivon02+ivon03)                                      
      goto 42590                                                       
32590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42590, 2601, 42590                                    
42590 if (ivcomp -966) 22590,12590,22590                                
12590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2601                                                        
22590 ivfail = ivfail + 1                                               
      ivcorr =966                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2601 continue                                                          
      ivtnum = 260                                                      
!                                                                        
!       ****  TEST 260  ****                                             
!                                                                        
      if (iczero) 32600, 2600, 32600                                    
 2600 continue                                                          
      ivon02 = 676                                                      
      ivon03 = 101                                                      
      ivcomp = (189 + ivon02) + ivon03                                  
      goto 42600                                                       
32600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42600, 2611, 42600                                    
42600 if (ivcomp-966) 22600,12600,22600                                 
12600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2611                                                        
22600 ivfail = ivfail + 1                                               
      ivcorr=966                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2611 continue                                                          
      ivtnum = 261                                                      
!                                                                        
!       ****  TEST 261  ****                                             
!                                                                        
      if (iczero) 32610, 2610, 32610                                    
 2610 continue                                                          
      ivon01 = 1358                                                     
      ivon02 = 8001                                                     
      ivcomp = ivon01 + (ivon02 + 2189)                                 
      goto 42610                                                       
32610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42610, 2621, 42610                                    
42610 if (ivcomp-11548) 22610,12610,22610                               
12610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2621                                                        
22610 ivfail = ivfail + 1                                               
      ivcorr=11548                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2621 continue                                                          
      ivtnum = 262                                                      
!                                                                        
!       ****  TEST 262  ****                                             
!                                                                        
      if (iczero) 32620, 2620, 32620                                    
 2620 continue                                                          
      ivon01 = 1358                                                     
      ivon02 = 8001                                                     
      ivcomp =(ivon01+ivon02)+2189                                      
      goto 42620                                                       
32620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42620, 2631, 42620                                    
42620 if (ivcomp-11548) 22620,12620,22620                               
12620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2631                                                        
22620 ivfail = ivfail + 1                                               
      ivcorr=11548                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2631 continue                                                          
      ivtnum = 263                                                      
!                                                                        
!       ****  TEST 263  ****                                             
!          REQUIRES 32767                                                
!                                                                        
      if (iczero) 32630, 2630, 32630                                    
 2630 continue                                                          
      ivon01 = 16383                                                    
      ivon03 = 16380                                                    
      ivcomp = ivon01 + (4+ivon03)                                      
      goto 42630                                                       
32630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42630, 2641, 42630                                    
42630 if (ivcomp-32767) 22630,12630,22630                               
12630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2641                                                        
22630 ivfail = ivfail + 1                                               
      ivcorr =32767                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2641 continue                                                          
      ivtnum = 264                                                      
!                                                                        
!       ****  TEST 264  ****                                             
!          REQUIRES 32767                                                
!                                                                        
      if (iczero) 32640, 2640, 32640                                    
 2640 continue                                                          
      ivon01 = 16383                                                    
      ivon02 = 16380                                                    
      ivcomp = (ivon01+ivon02) +4                                       
      goto 42640                                                       
32640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42640, 2651, 42640                                    
42640 if (ivcomp - 32767) 22640,12640,22640                             
12640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2651                                                        
22640 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2651 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM009" )                          
      end program fm009
