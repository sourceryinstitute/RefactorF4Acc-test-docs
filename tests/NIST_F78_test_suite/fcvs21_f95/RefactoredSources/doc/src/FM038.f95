      program fm038
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
      integer :: ivon02
!      COMMENT SECTION                                                   
!                                                                        
!      FM038                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM          INTEGER VARIABLE = ARITHMETIC EXPRESSION            
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR /, INTEGER CONSTANTS AND AN INTEGER VARIABLE.  BOTH      
!      POSITIVE AND NEGATIVE VALUES ARE USED FOR THE INTEGER CONSTANTS   
!      AND THE INTEGER VARIABLE.                                         
!                                                                        
!          THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     
!      AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  
!      IN THE RESULTANT INTEGER VARIABLE.  SOME OF THE TESTS USE PARENS  
!      TO GROUP ELEMENTS IN THE ARITHMETIC EXPRESSION.                   
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!              (1) (INTEGER CONSTANT/INTEGER CONSTANT)/INTEGER CONSTANT  
!              (2) INTEGER CONSTANT/(INTEGER CONSTANT/INTEGER CONSTANT)  
!              (3) INTEGER VARIABLE/INTEGER CONSTANT                     
!              (4) INTEGER CONSTANT/INTEGER VARIABLE                     
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 6.6, EVALUATION OF EXPRESSIONS                         
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
!      TEST 520 THROUGH TEST 525 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM       INTEGER VARIABLE = (INT.CON./INT.CON.)/INT.CON. 
!      NO TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND       
!      NEGATIVE CONSTANTS ARE INCLUDED.                                  
!                                                                        
 5201 continue                                                          
      ivtnum = 520                                                      
!                                                                        
!       ****  TEST 520  ****                                             
!                                                                        
      if (iczero) 35200, 5200, 35200                                    
 5200 continue                                                          
      ivcomp = (24/3)/4                                                 
      goto 45200                                                       
35200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45200, 5211, 45200                                    
45200 if (ivcomp - 2) 25200,15200,25200                                 
15200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5211                                                        
25200 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5211 continue                                                          
      ivtnum = 521                                                      
!                                                                        
!       ****  TEST 521  ****                                             
!                                                                        
      if (iczero) 35210, 5210, 35210                                    
 5210 continue                                                          
      ivcomp = (7150/2)/25                                              
      goto 45210                                                       
35210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45210, 5221, 45210                                    
45210 if (ivcomp - 143) 25210,15210,25210                               
15210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5221                                                        
25210 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5221 continue                                                          
      ivtnum = 522                                                      
!                                                                        
!       ****  TEST 522  ****                                             
!                                                                        
      if (iczero) 35220, 5220, 35220                                    
 5220 continue                                                          
      ivcomp = (-24/3)/4                                                
      goto 45220                                                       
35220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45220, 5231, 45220                                    
45220 if (ivcomp + 2) 25220,15220,25220                                 
15220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5231                                                        
25220 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5231 continue                                                          
      ivtnum = 523                                                      
!                                                                        
!       ****  TEST 523  ****                                             
!                                                                        
      if (iczero) 35230, 5230, 35230                                    
 5230 continue                                                          
      ivcomp = (330/(-3))/2                                             
      goto 45230                                                       
35230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45230, 5241, 45230                                    
45230 if (ivcomp + 55) 25230,15230,25230                                
15230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5241                                                        
25230 ivfail = ivfail + 1                                               
      ivcorr = -55                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5241 continue                                                          
      ivtnum = 524                                                      
!                                                                        
!       ****  TEST 524  ****                                             
!                                                                        
      if (iczero) 35240, 5240, 35240                                    
 5240 continue                                                          
      ivcomp = ((-7150)/(-2))/(-25)                                     
      goto 45240                                                       
35240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45240, 5251, 45240                                    
45240 if (ivcomp + 143) 25240,15240,25240                               
15240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5251                                                        
25240 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5251 continue                                                          
      ivtnum = 525                                                      
!                                                                        
!       ****  TEST 525  ****                                             
!                                                                        
      if (iczero) 35250, 5250, 35250                                    
 5250 continue                                                          
      ivcomp = (15249/(-13))/(-51)                                      
      goto 45250                                                       
35250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45250, 5261, 45250                                    
45250 if (ivcomp - 23) 25250,15250,25250                                
15250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5261                                                        
25250 ivfail = ivfail + 1                                               
      ivcorr = 23                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 526 THROUGH TEST 531 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM   IV = (IC/IC)/IC.                                    
!      TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND          
!      NEGATIVE CONSTANTS ARE INCLUDED.                                  
!                                                                        
 5261 continue                                                          
      ivtnum = 526                                                      
!                                                                        
!       ****  TEST 526  ****                                             
!                                                                        
      if (iczero) 35260, 5260, 35260                                    
 5260 continue                                                          
      ivcomp = (24/3)/3                                                 
      goto 45260                                                       
35260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45260, 5271, 45260                                    
45260 if (ivcomp - 2) 25260,15260,25260                                 
15260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5271                                                        
25260 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5271 continue                                                          
      ivtnum = 527                                                      
!                                                                        
!       ****  TEST 527  ****                                             
!                                                                        
      if (iczero) 35270, 5270, 35270                                    
 5270 continue                                                          
      ivcomp = (7151/3)/10                                              
      goto 45270                                                       
35270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45270, 5281, 45270                                    
45270 if (ivcomp - 238) 25270,15270,25270                               
15270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5281                                                        
25270 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5281 continue                                                          
      ivtnum = 528                                                      
!                                                                        
!       ****  TEST 528  ****                                             
!                                                                        
      if (iczero) 35280, 5280, 35280                                    
 5280 continue                                                          
      ivcomp = (-24/3)/3                                                
      goto 45280                                                       
35280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45280, 5291, 45280                                    
45280 if (ivcomp + 2) 25280,15280,25280                                 
15280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5291                                                        
25280 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5291 continue                                                          
      ivtnum = 529                                                      
!                                                                        
!       ****  TEST 529  ****                                             
!                                                                        
      if (iczero) 35290, 5290, 35290                                    
 5290 continue                                                          
      ivcomp = (7151/(-3))/10                                           
      goto 45290                                                       
35290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45290, 5301, 45290                                    
45290 if (ivcomp + 238) 25290,15290,25290                               
15290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5301                                                        
25290 ivfail = ivfail + 1                                               
      ivcorr = -238                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5301 continue                                                          
      ivtnum = 530                                                      
!                                                                        
!       ****  TEST 530  ****                                             
!                                                                        
      if (iczero) 35300, 5300, 35300                                    
 5300 continue                                                          
      ivcomp = (15248/(-51))/(-23)                                      
      goto 45300                                                       
35300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45300, 5311, 45300                                    
45300 if (ivcomp - 12) 25300,15300,25300                                
15300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5311                                                        
25300 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5311 continue                                                          
      ivtnum = 531                                                      
!                                                                        
!       ****  TEST 531  ****                                             
!                                                                        
      if (iczero) 35310, 5310, 35310                                    
 5310 continue                                                          
      ivcomp = ((-27342)/(-4))/(-3)                                     
      goto 45310                                                       
35310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45310, 5321, 45310                                    
45310 if (ivcomp + 2278) 25310,15310,25310                              
15310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5321                                                        
25310 ivfail = ivfail + 1                                               
      ivcorr = -2278                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 532 THROUGH TEST 537 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM   IV = IC/(IC/IC).                                    
!      NO TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND       
!      NEGATIVE CONSTANTS ARE INCLUDED.                                  
!                                                                        
 5321 continue                                                          
      ivtnum = 532                                                      
!                                                                        
!       ****  TEST 532  ****                                             
!                                                                        
      if (iczero) 35320, 5320, 35320                                    
 5320 continue                                                          
      ivcomp = 24/(8/4)                                                 
      goto 45320                                                       
35320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45320, 5331, 45320                                    
45320 if (ivcomp - 12) 25320,15320,25320                                
15320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5331                                                        
25320 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5331 continue                                                          
      ivtnum = 533                                                      
!                                                                        
!       ****  TEST 533  ****                                             
!                                                                        
      if (iczero) 35330, 5330, 35330                                    
 5330 continue                                                          
      ivcomp = 7150/(25/5)                                              
      goto 45330                                                       
35330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45330, 5341, 45330                                    
45330 if (ivcomp - 1430) 25330,15330,25330                              
15330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5341                                                        
25330 ivfail = ivfail + 1                                               
      ivcorr = 1430                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5341 continue                                                          
      ivtnum = 534                                                      
!                                                                        
!       ****  TEST 534  ****                                             
!                                                                        
      if (iczero) 35340, 5340, 35340                                    
 5340 continue                                                          
      ivcomp = -24/(8/4)                                                
      goto 45340                                                       
35340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45340, 5351, 45340                                    
45340 if (ivcomp + 12) 25340,15340,25340                                
15340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5351                                                        
25340 ivfail = ivfail + 1                                               
      ivcorr = -12                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5351 continue                                                          
      ivtnum = 535                                                      
!                                                                        
!       ****  TEST 535  ****                                             
!                                                                        
      if (iczero) 35350, 5350, 35350                                    
 5350 continue                                                          
      ivcomp = 24/((-8)/4)                                              
      goto 45350                                                       
35350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45350, 5361, 45350                                    
45350 if (ivcomp + 12) 25350,15350,25350                                
15350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5361                                                        
25350 ivfail = ivfail + 1                                               
      ivcorr = -12                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5361 continue                                                          
      ivtnum = 536                                                      
!                                                                        
!       ****  TEST 536  ****                                             
!                                                                        
      if (iczero) 35360, 5360, 35360                                    
 5360 continue                                                          
      ivcomp = (-7150)/((-25)/(-5))                                     
      goto 45360                                                       
35360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45360, 5371, 45360                                    
45360 if (ivcomp + 1430) 25360,15360,25360                              
15360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5371                                                        
25360 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5371 continue                                                          
      ivtnum = 537                                                      
!                                                                        
!       ****  TEST 537  ****                                             
!                                                                        
      if (iczero) 35370, 5370, 35370                                    
 5370 continue                                                          
      ivcomp = -7150/(25/(-5))                                          
      goto 45370                                                       
35370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45370, 5381, 45370                                    
45370 if (ivcomp - 1430) 25370,15370,25370                              
15370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5381                                                        
25370 ivfail = ivfail + 1                                               
      ivcorr = 1430                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 538 THROUGH TEST 543 CONTAIN ARITHMETIC ASSIGMMENT STATEMENTS
!      OF THE FORM   IV = IC/(IC/IC).                                    
!      TRUNCATION OF THE RESULT IS REQUIRED.  BOTH POSITIVE AND          
!      NEGATIVE CONSTANTS ARE INCLUDED.                                  
!                                                                        
 5381 continue                                                          
      ivtnum = 538                                                      
!                                                                        
!       ****  TEST 538  ****                                             
!                                                                        
      if (iczero) 35380, 5380, 35380                                    
 5380 continue                                                          
      ivcomp = 29/(5/2)                                                 
      goto 45380                                                       
35380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45380, 5391, 45380                                    
45380 if (ivcomp - 14) 25380,15380,25380                                
15380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5391                                                        
25380 ivfail = ivfail + 1                                               
      ivcorr = 14                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5391 continue                                                          
      ivtnum = 539                                                      
!                                                                        
!       ****  TEST 539  ****                                             
!                                                                        
      if (iczero) 35390, 5390, 35390                                    
 5390 continue                                                          
      ivcomp = 7154/(26/5)                                              
      goto 45390                                                       
35390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45390, 5401, 45390                                    
45390 if (ivcomp - 1430) 25390,15390,25390                              
15390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5401                                                        
25390 ivfail = ivfail + 1                                               
      ivcorr = 1430                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5401 continue                                                          
      ivtnum = 540                                                      
!                                                                        
!       ****  TEST 540  ****                                             
!                                                                        
      if (iczero) 35400, 5400, 35400                                    
 5400 continue                                                          
      ivcomp = -7154/(26/5)                                             
      goto 45400                                                       
35400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45400, 5411, 45400                                    
45400 if (ivcomp + 1430) 25400,15400,25400                              
15400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5411                                                        
25400 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5411 continue                                                          
      ivtnum = 541                                                      
!                                                                        
!       ****  TEST 541  ****                                             
!                                                                        
      if (iczero) 35410, 5410, 35410                                    
 5410 continue                                                          
      ivcomp = (-7154)/((-26)/5)                                        
      goto 45410                                                       
35410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45410, 5421, 45410                                    
45410 if (ivcomp - 1430) 25410,15410,25410                              
15410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5421                                                        
25410 ivfail = ivfail + 1                                               
      ivcorr = 1430                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5421 continue                                                          
      ivtnum = 542                                                      
!                                                                        
!       ****  TEST 542  ****                                             
!                                                                        
      if (iczero) 35420, 5420, 35420                                    
 5420 continue                                                          
      ivcomp = 7154/((-26)/(-5))                                        
      goto 45420                                                       
35420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45420, 5431, 45420                                    
45420 if (ivcomp - 1430) 25420,15420,25420                              
15420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5431                                                        
25420 ivfail = ivfail + 1                                               
      ivcorr = 1430                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5431 continue                                                          
      ivtnum = 543                                                      
!                                                                        
!       ****  TEST 543  ****                                             
!                                                                        
      if (iczero) 35430, 5430, 35430                                    
 5430 continue                                                          
      ivcomp = (-7154)/((-26)/(-5))                                     
      goto 45430                                                       
35430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45430, 5441, 45430                                    
45430 if (ivcomp + 1430) 25430,15430,25430                              
15430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5441                                                        
25430 ivfail = ivfail + 1                                               
      ivcorr = -1430                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 544 THROUGH TEST 547 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM   INTEGER VARIABLE = INTEGER VARIABLE/INTEGER CONSTANT
!                                                                        
 5441 continue                                                          
      ivtnum = 544                                                      
!                                                                        
!       ****  TEST 544  ****                                             
!                                                                        
      if (iczero) 35440, 5440, 35440                                    
 5440 continue                                                          
      ivon01 = 75                                                       
      ivcomp = ivon01/25                                                
      goto 45440                                                       
35440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45440, 5451, 45440                                    
45440 if (ivcomp - 3) 25440,15440,25440                                 
15440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5451                                                        
25440 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5451 continue                                                          
      ivtnum = 545                                                      
!                                                                        
!       ****  TEST 545  ****                                             
!                                                                        
      if (iczero) 35450, 5450, 35450                                    
 5450 continue                                                          
      ivon01 = -3575                                                    
      ivcomp = ivon01/25                                                
      goto 45450                                                       
35450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45450, 5461, 45450                                    
45450 if (ivcomp + 143) 25450,15450,25450                               
15450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5461                                                        
25450 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5461 continue                                                          
      ivtnum = 546                                                      
!                                                                        
!       ****  TEST 546  ****                                             
!                                                                        
      if (iczero) 35460, 5460, 35460                                    
 5460 continue                                                          
      ivon01 = 3575                                                     
      ivcomp = ivon01/(-143)                                            
      goto 45460                                                       
35460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45460, 5471, 45460                                    
45460 if (ivcomp + 25) 25460,15460,25460                                
15460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5471                                                        
25460 ivfail = ivfail + 1                                               
      ivcorr = -25                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5471 continue                                                          
      ivtnum = 547                                                      
!                                                                        
!       ****  TEST 547  ****                                             
!                                                                        
      if (iczero) 35470, 5470, 35470                                    
 5470 continue                                                          
      ivon01 = 959                                                      
      ivcomp = ivon01/120                                               
      goto 45470                                                       
35470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45470, 5481, 45470                                    
45470 if (ivcomp -7)  25470,15470,25470                                 
15470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5481                                                        
25470 ivfail = ivfail + 1                                               
      ivcorr = 7                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 548 THROUGH TEST 551 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM   INTEGER VARIABLE =INTEGER CONSTANT/INTEGER VARIABLE.
!                                                                        
 5481 continue                                                          
      ivtnum = 548                                                      
!                                                                        
!       ****  TEST 548  ****                                             
!                                                                        
      if (iczero) 35480, 5480, 35480                                    
 5480 continue                                                          
      ivon02 = 25                                                       
      ivcomp = 75/ivon02                                                
      goto 45480                                                       
35480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45480, 5491, 45480                                    
45480 if (ivcomp - 3) 25480,15480,25480                                 
15480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5491                                                        
25480 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5491 continue                                                          
      ivtnum = 549                                                      
!                                                                        
!       ****  TEST 549  ****                                             
!                                                                        
      if (iczero) 35490, 5490, 35490                                    
 5490 continue                                                          
      ivon02 = -25                                                      
      ivcomp = 3579/ivon02                                              
      goto 45490                                                       
35490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45490, 5501, 45490                                    
45490 if (ivcomp + 143) 25490,15490,25490                               
15490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5501                                                        
25490 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5501 continue                                                          
      ivtnum = 550                                                      
!                                                                        
!       ****  TEST 550  ****                                             
!                                                                        
      if (iczero) 35500, 5500, 35500                                    
 5500 continue                                                          
      ivon02 = -143                                                     
      ivcomp = (-3575)/ivon02                                           
      goto 45500                                                       
35500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45500, 5511, 45500                                    
45500 if (ivcomp - 25) 25500,15500,25500                                
15500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5511                                                        
25500 ivfail = ivfail + 1                                               
      ivcorr = 25                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5511 continue                                                          
      ivtnum = 551                                                      
!                                                                        
!       ****  TEST 551  ****                                             
!                                                                        
      if (iczero) 35510, 5510, 35510                                    
 5510 continue                                                          
      ivon02 = 120                                                      
      ivcomp = -959/ivon02                                              
      goto 45510                                                       
35510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45510, 5521, 45510                                    
45510 if (ivcomp + 7) 25510,15510,25510                                 
15510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5521                                                        
25510 ivfail = ivfail + 1                                               
      ivcorr = -7                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!       ****    END OF TESTS    ****                                     
 5521 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM038" )                          
      end program fm038
