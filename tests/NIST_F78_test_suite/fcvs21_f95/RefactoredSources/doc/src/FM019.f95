      program fm019
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM019                                                             
!                                                                        
!            THIS ROUTINE CONTINUES TESTS OF THE FORTRAN LOGICAL IF STATE
!      BY TESTING VARIOUS FORMS OF RELATIONAL EXPRESSIONS WITH ARITHMETIC
!      EXPRESSIONS .  POSITIVE AND NEGATIVE SIGNS ARE USED IN CONJUNCTION
!      WITH PARENTHESES. COMBINATIONS OF LOGICAL  .AND.    .OR.          
!      .NOT. ARE USED TO TEST THE MORE COMPLEX EXPRESSIONS.              
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.7.1, LOGICAL CONSTANT                                
!         SECTION 6, EXPRESSIONS                                         
!         SECTION 11.5, LOGICAL IF STATEMENT                             
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      logical :: lctnt1
      logical :: lctnt2
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
      ivtnum = 530                                                      
!                                                                        
!       ****  TEST 530  ****                                             
!      TEST 530  - TEST OF POSITIVELY SIGNED TERM   +(IC) (RO) -(IC)     
!            .LT.  FALSE PATH                                            
!                                                                        
      if (iczero) 35300, 5300, 35300                                    
 5300 continue                                                          
      ivon01 = 1                                                        
      if ( +3  <  -3)  ivon01 = 0                                      
      goto 45300                                                       
35300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45300, 5311, 45300                                    
45300 if ( ivon01 - 1 )  25300, 15300, 25300                            
15300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5311                                                        
25300 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5311 continue                                                          
      ivtnum = 531                                                      
!                                                                        
!       ****  TEST 531  ****                                             
!      TEST 531  -  TEST OF SIGNED ZERO     .LT.  FALSE PATH             
!                                                                        
!                                                                        
      if (iczero) 35310, 5310, 35310                                    
 5310 continue                                                          
      ivon01 = 1                                                        
      if ( +0  <  -0 )  ivon01 = 0                                     
      goto 45310                                                       
35310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45310, 5321, 45310                                    
45310 if ( ivon01 - 1 )  25310, 15310, 25310                            
15310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5321                                                        
25310 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5321 continue                                                          
      ivtnum = 532                                                      
!                                                                        
!       ****  TEST 532  ****                                             
!      TEST 532  -  TEST OF SIGNED ZERO  .LE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35320, 5320, 35320                                    
 5320 continue                                                          
      ivon01 = 0                                                        
      if ( +0  <=  -0 )  ivon01 = 1                                     
      goto 45320                                                       
35320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45320, 5331, 45320                                    
45320 if ( ivon01 - 1 )  25320, 15320, 25320                            
15320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5331                                                        
25320 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5331 continue                                                          
      ivtnum = 533                                                      
!                                                                        
!       ****  TEST 533  ****                                             
!      TEST 533  -  TEST OF SIGNED ZERO  .EQ.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35330, 5330, 35330                                    
 5330 continue                                                          
      ivon01 = 0                                                        
      if ( +0  ==  -0 )  ivon01 = 1                                     
      goto 45330                                                       
35330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45330, 5341, 45330                                    
45330 if ( ivon01 - 1 )  25330, 15330, 25330                            
15330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5341                                                        
25330 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5341 continue                                                          
      ivtnum = 534                                                      
!                                                                        
!       ****  TEST 534  ****                                             
!      TEST 534  -  TEST OF SIGNED ZERO  .NE.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35340, 5340, 35340                                    
 5340 continue                                                          
      ivon01 = 1                                                        
      if ( +0  /=  -0 )  ivon01 = 0                                     
      goto 45340                                                       
35340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45340, 5351, 45340                                    
45340 if ( ivon01 - 1 )  25340, 15340, 25340                            
15340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5351                                                        
25340 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5351 continue                                                          
      ivtnum = 535                                                      
!                                                                        
!       ****  TEST 535  ****                                             
!      TEST 535  -  TEST OF SIGNED ZERO  .GE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35350, 5350, 35350                                    
 5350 continue                                                          
      ivon01 = 0                                                        
      if ( +0  >=  -0 )  ivon01 = 1                                     
      goto 45350                                                       
35350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45350, 5361, 45350                                    
45350 if ( ivon01 - 1 )  25350, 15350, 25350                            
15350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5361                                                        
25350 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5361 continue                                                          
      ivtnum = 536                                                      
!                                                                        
!       ****  TEST 536  ****                                             
!      TEST 536  -  TEST OF SIGNED ZERO  .GT.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35360, 5360, 35360                                    
 5360 continue                                                          
      ivon01 = 1                                                        
      if ( +0  >  -0 )  ivon01 = 0                                     
      goto 45360                                                       
35360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45360, 5371, 45360                                    
45360 if ( ivon01 - 1 )  25360, 15360, 25360                            
15360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5371                                                        
25360 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5371 continue                                                          
      ivtnum = 537                                                      
!                                                                        
!       ****  TEST 537  ****                                             
!      TEST 537  -  TEST OF +32767 .EQ. -32766  FALSE PATH               
!                                                                        
!                                                                        
      if (iczero) 35370, 5370, 35370                                    
 5370 continue                                                          
      ivon01 = 1                                                        
      if ( +32767  ==  -32766 )  ivon01 = 0                             
      goto 45370                                                       
35370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45370, 5381, 45370                                    
45370 if ( ivon01 - 1 )  25370, 15370, 25370                            
15370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5381                                                        
25370 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5381 continue                                                          
      ivtnum = 538                                                      
!                                                                        
!       ****  TEST 538  ****                                             
!      TEST 538  -  TESTS MINUS SIGN WITH INTEGER VARIABLES              
!            RELATIONAL EXPRESSION USES  .LE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35380, 5380, 35380                                    
 5380 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      if ( -ivon02  <=  -ivon02 )  ivon01 = 1                           
      goto 45380                                                       
35380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45380, 5391, 45380                                    
45380 if ( ivon01 - 1 )  25380, 15380, 25380                            
15380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5391                                                        
25380 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5391 continue                                                          
      ivtnum = 539                                                      
!                                                                        
!       ****  TEST 539  ****                                             
!      TEST 539  -  TEST IS LIKE TEST 538   USES  .GE.  TRUE PATH        
!                                                                        
!                                                                        
      if (iczero) 35390, 5390, 35390                                    
 5390 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 32766                                                    
      if ( -ivon02  >=  -ivon02 )  ivon01 = 1                           
      goto 45390                                                       
35390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45390, 5401, 45390                                    
45390 if ( ivon01 - 1 )  25390, 15390, 25390                            
15390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5401                                                        
25390 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5401 continue                                                          
      ivtnum = 540                                                      
!                                                                        
!       ****  TEST 540  ****                                             
!      TEST 540  -  INTEGER EXPONIENTIATION AND MINUS SIGN  USES .NE.    
!            FALSE PATH                                                  
!                                                                        
!                                                                        
      if (iczero) 35400, 5400, 35400                                    
 5400 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 3                                                        
      if ( -ivon02 ** 3  /=  -27 )  ivon01 = 0                          
      goto 45400                                                       
35400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45400, 5411, 45400                                    
45400 if ( ivon01 - 1 )  25400, 15400, 25400                            
15400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5411                                                        
25400 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5411 continue                                                          
      ivtnum = 541                                                      
!                                                                        
!       ****  TEST 541  ****                                             
!      TEST 541  -  LIKE TEST 540  USES  .LE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35410, 5410, 35410                                    
 5410 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      if ( -3 ** ivon02   <=  -27 )  ivon01 = 1                         
      goto 45410                                                       
35410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45410, 5421, 45410                                    
45410 if ( ivon01 - 1 )  25410, 15410, 25410                            
15410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5421                                                        
25410 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5421 continue                                                          
      ivtnum = 542                                                      
!                                                                        
!       ****  TEST 542  ****                                             
!      TEST 542  -  INTEGER EXPONIENTIATION AND MULTIPLICATION           
!            USES  .EQ.  TRUE PATH                                       
!                                                                        
!                                                                        
      if (iczero) 35420, 5420, 35420                                    
 5420 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      ivon03 = 27                                                       
      if ( -ivon02 ** 2 * ivon02  ==  -ivon03 )  ivon01 = 1             
      goto 45420                                                       
35420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45420, 5431, 45420                                    
45420 if ( ivon01 - 1 )  25420, 15420, 25420                            
15420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5431                                                        
25420 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5431 continue                                                          
      ivtnum = 543                                                      
!                                                                        
!       ****  TEST 543  ****                                             
!      TEST 543  -  INTEGER EXPONIENTIATION AND DIVISION                 
!            USES  .LT.  TRUE PATH                                       
!                                                                        
!                                                                        
      if (iczero) 35430, 5430, 35430                                    
 5430 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 587                                                      
      ivon03 = 3                                                        
      ivon04 = 3                                                        
      if ( -ivon02/ivon04 ** 3  <  -3 ** ivon03/ivon02 )  ivon01 = 1   
      goto 45430                                                       
35430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45430, 5441, 45430                                    
45430 if ( ivon01 - 1 )  25430, 15430, 25430                            
15430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5441                                                        
25430 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5441 continue                                                          
      ivtnum = 544                                                      
!                                                                        
!       ****  TEST 544  ****                                             
!      TEST 544  -  INTEGER ADDITION AND SUBTRACTION                     
!            USES  .EQ.  TRUE PATH                                       
!                                                                        
!                                                                        
      if (iczero) 35440, 5440, 35440                                    
 5440 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      ivon03 = 587                                                      
      if ( ivon02 - ivon03  ==  -ivon03 + ivon02 )  ivon01 = 1          
      goto 45440                                                       
35440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45440, 5451, 45440                                    
45440 if ( ivon01 - 1 )  25440, 15440, 25440                            
15440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5451                                                        
25440 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5451 continue                                                          
      ivtnum = 545                                                      
!                                                                        
!       ****  TEST 545  ****                                             
!      TEST 545  -  INTEGER ADDITION AND SUBTRACTION WITH PARENTHESES    
!            USES  .EQ.  TRUE PATH  LIKE TEST 544                        
!                                                                        
!                                                                        
      if (iczero) 35450, 5450, 35450                                    
 5450 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      ivon03 = 587                                                      
      if ( (ivon02 - ivon03)  ==  (-ivon03 + ivon02) )  ivon01 = 1      
      goto 45450                                                       
35450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45450, 5461, 45450                                    
45450 if ( ivon01 - 1 ) 25450, 15450, 25450                             
15450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5461                                                        
25450 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5461 continue                                                          
      ivtnum = 546                                                      
!                                                                        
!       ****  TEST 546  ****                                             
!      TEST 546  -  INTEGER EXPONIENTIATION AND DIVISION WITH PARENS     
!            USES  .LT.  TRUE PATH                                       
!                                                                        
!                                                                        
      if (iczero) 35460, 5460, 35460                                    
 5460 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 587                                                      
      ivon03 = 3                                                        
      ivon04 = 3                                                        
      if ((-ivon02/(ivon04**3)) < ((-3**ivon03)/ivon02))ivon01=1       
      goto 45460                                                       
35460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45460, 5471, 45460                                    
45460 if ( ivon01 - 1 )  25460, 15460, 25460                            
15460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5471                                                        
25460 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5471 continue                                                          
      ivtnum = 547                                                      
!                                                                        
!       ****  TEST 547  ****                                             
!      TEST 547  -  INTEGER MULTIPLICATION WITH PARENTHESES  .LT.  FALSE 
!                                                                        
!                                                                        
      if (iczero) 35470, 5470, 35470                                    
 5470 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 587                                                      
      if ((-3)*(-3) < (-ivon02))ivon01=0                               
      goto 45470                                                       
35470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45470, 5481, 45470                                    
45470 if ( ivon01 - 1 )  25470, 15470, 25470                            
15470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5481                                                        
25470 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5481 continue                                                          
      ivtnum = 548                                                      
!                                                                        
!       ****  TEST 548  ****                                             
!      TEST 548  -  INTEGER EXPONIENTIATION, MINUS SIGNS, AND PARENTHESES
!            USES  .LE.  TRUE PATH                                       
!                                                                        
!                                                                        
      if (iczero) 35480, 5480, 35480                                    
 5480 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      ivon03 = 27                                                       
      if ( ((-ivon02) ** ivon02  <=  (-ivon03)))  ivon01 = 1            
      goto 45480                                                       
35480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45480, 5491, 45480                                    
45480 if ( ivon01 - 1 )  25480, 15480, 25480                            
15480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5491                                                        
25480 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5491 continue                                                          
      ivtnum = 549                                                      
!                                                                        
!       ****  TEST 549  ****                                             
!      TEST 549  -  TEST THE ORDER OF INTEGER ARITHMETIC OPERATIONS      
!            USES INTEGER EXPONIENTIATION, ADDITION, MULTIPLICATION,     
!            AND PARENTHESES.  ALSO USES  .EQ.  TRUE PATH                
!            SEE SECTION 6.1, ARITHMETIC EXPRESSIONS.                    
!                                                                        
!                                                                        
      if (iczero) 35490, 5490, 35490                                    
 5490 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      if(ivon02 * ivon02/(ivon02+ivon02)**ivon02+ivon02  ==  3) ivon01=1
      goto 45490                                                       
35490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45490, 5501, 45490                                    
45490 if ( ivon01 - 1 )  25490, 15490, 25490                            
15490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5501                                                        
25490 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5501 continue                                                          
      ivtnum = 550                                                      
!                                                                        
!       ****  TEST 550  ****                                             
!      TEST 550  -  COMBINATION OF LOGICAL  .NOT. AND  .AND.             
!            .NOT. (LP) .AND. .NOT. (LP)                                 
!            TRUE PATH                                                   
!                                                                        
!                                                                        
      if (iczero) 35500, 5500, 35500                                    
 5500 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .false.                                                  
      if ( .not. .false. .and. .not. lctnt1 )  ivon01 = 1               
      goto 45500                                                       
35500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45500, 5511, 45500                                    
45500 if ( ivon01 - 1 )  25500, 15500, 25500                            
15500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5511                                                        
25500 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5511 continue                                                          
      ivtnum = 551                                                      
!                                                                        
!       ****  TEST 551  ****                                             
!      TEST 551  -  COMBINATION OF LOGICAL .OR. AND .NOT.                
!            .NOT. (LP) .OR. .NOT. (LP)                                  
!            TRUE PATH                                                   
!                                                                        
!                                                                        
      if (iczero) 35510, 5510, 35510                                    
 5510 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .false.                                                  
      if ( .not. lctnt1 .or. .not. lctnt2 )  ivon01 = 1                 
      goto 45510                                                       
35510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45510, 5521, 45510                                    
45510 if ( ivon01 - 1 )  25510, 15510, 25510                            
15510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5521                                                        
25510 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5521 continue                                                          
      ivtnum = 552                                                      
!                                                                        
!       ****  TEST 552  ****                                             
!      TEST 552  -  COMBINATION OF LOGICAL .AND.  .OR.  AND  .NOT.       
!            .NOT. ( (LE) .OR. (LT) ) .AND. .NOT. ( (LT) .AND. (LF) )    
!            .NOT. IS APPLIED TO A LOGICAL EXPRESSION INCLOSED IN PARENS 
!            FALSE PATH                                                  
!                                                                        
      if (iczero) 35520, 5520, 35520                                    
 5520 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .true.                                                   
      if(.not.(lctnt1.or.lctnt2).and..not.(lctnt1.and.lctnt2))ivon01 = 0
      goto 45520                                                       
35520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45520, 5531, 45520                                    
45520 if ( ivon01 - 1 )  25520, 15520, 25520                            
15520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5531                                                        
25520 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5531 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM019" )                          
      end program fm019