      program fm041
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
!      COMMENT SECTION                                                   
!                                                                        
!      FM041                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE              
!      FORM      INTEGER VARIABLE =  PRIMARY ** PRIMARY                  
!      WHERE THE FIRST OF TWO PRIMARIES IS AN INTEGER VARIABLE OR AN     
!      INTEGER CONSTANT AND THE SECOND PRIMARY IS AN INTEGER CONSTANT.   
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
!      TEST 615 THROUGH TEST 631 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM    INTEGER VARIABLE = INTEGER CONSTANT ** INTEGER CON.
!                                                                        
!      TEST 632 THROUGH TEST 648 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM    INTEGER VARIABLE = INTEGER VARIABLE ** INTEGER CON.
!                                                                        
!                                                                        
      ivtnum = 615                                                      
!                                                                        
!       ****  TEST 615  ****                                             
!      TEST 615  - SMALL NUMBER BASE; ZERO EXPONENT                      
!                                                                        
      if (iczero) 36150, 6150, 36150                                    
 6150 continue                                                          
      ivcomp = 1 ** 0                                                   
      goto 46150                                                       
36150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46150, 6161, 46150                                    
46150 if (ivcomp - 1) 26150,16150,26150                                 
16150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6161                                                        
26150 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6161 continue                                                          
      ivtnum = 616                                                      
!                                                                        
!       ****  TEST 616  ****                                             
!      TEST 616  - ZERO BASE TO FIRST POWER                              
!                                                                        
      if (iczero) 36160, 6160, 36160                                    
 6160 continue                                                          
      ivcomp = 0 ** 1                                                   
      goto 46160                                                       
36160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46160, 6171, 46160                                    
46160 if (ivcomp) 26160,16160,26160                                     
16160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6171                                                        
26160 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6171 continue                                                          
      ivtnum = 617                                                      
!                                                                        
!       ****  TEST 617  ****                                             
!      TEST 617  - BASE =1; EXPONENT = 1                                 
!                                                                        
      if (iczero) 36170, 6170, 36170                                    
 6170 continue                                                          
      ivcomp = 1 ** 1                                                   
      goto 46170                                                       
36170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46170, 6181, 46170                                    
46170 if (ivcomp - 1) 26170,16170,26170                                 
16170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6181                                                        
26170 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6181 continue                                                          
      ivtnum = 618                                                      
!                                                                        
!       ****  TEST 618  ****                                             
!      TEST 618  - LARGE NUMBER BASE; EXPONENT = 1                       
!                                                                        
      if (iczero) 36180, 6180, 36180                                    
 6180 continue                                                          
      ivcomp = 32767 ** 1                                               
      goto 46180                                                       
36180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46180, 6191, 46180                                    
46180 if (ivcomp - 32767) 26180,16180,26180                             
16180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6191                                                        
26180 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6191 continue                                                          
      ivtnum = 619                                                      
!                                                                        
!       ****  TEST 619  ****                                             
!      TEST 619  - LARGE EXPONENT                                        
!                                                                        
      if (iczero) 36190, 6190, 36190                                    
 6190 continue                                                          
      ivcomp = 1 ** 32767                                               
      goto 46190                                                       
36190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46190, 6201, 46190                                    
46190 if (ivcomp - 1) 26190,16190,26190                                 
16190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6201                                                        
26190 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6201 continue                                                          
      ivtnum = 620                                                      
!                                                                        
!       ****  TEST 620  ****                                             
!      TEST 620  - ZERO BASE; LARGE NUMBER EXPONENT                      
!                                                                        
      if (iczero) 36200, 6200, 36200                                    
 6200 continue                                                          
      ivcomp = 0 ** 32767                                               
      goto 46200                                                       
36200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46200, 6211, 46200                                    
46200 if (ivcomp) 26200,16200,26200                                     
16200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6211                                                        
26200 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6211 continue                                                          
      ivtnum = 621                                                      
!                                                                        
!       ****  TEST 621  ****                                             
!      TEST 621  -LARGE NUMBER BASE; ZERO EXPONENT                       
!                                                                        
      if (iczero) 36210, 6210, 36210                                    
 6210 continue                                                          
      ivcomp = 32767 ** 0                                               
      goto 46210                                                       
36210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46210, 6221, 46210                                    
46210 if (ivcomp - 1) 26210,16210,26210                                 
16210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6221                                                        
26210 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6221 continue                                                          
      ivtnum = 622                                                      
!                                                                        
!       ****  TEST 622  ****                                             
!      TEST 622  -EXPONENT IS POWER OF TWO                               
!                                                                        
      if (iczero) 36220, 6220, 36220                                    
 6220 continue                                                          
      ivcomp = 181 ** 2                                                 
      goto 46220                                                       
36220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46220, 6231, 46220                                    
46220 if (ivcomp - 32761) 26220,16220,26220                             
16220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6231                                                        
26220 ivfail = ivfail + 1                                               
      ivcorr = 32761                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6231 continue                                                          
      ivtnum = 623                                                      
!                                                                        
!       ****  TEST 623  ****                                             
!      TEST 623  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              
!                                                                        
      if (iczero) 36230, 6230, 36230                                    
 6230 continue                                                          
      ivcomp = 2 ** 8                                                   
      goto 46230                                                       
36230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46230, 6241, 46230                                    
46230 if (ivcomp - 256) 26230,16230,26230                               
16230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6241                                                        
26230 ivfail = ivfail + 1                                               
      ivcorr = 256                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6241 continue                                                          
!                                                                        
!      TESTS 624 AND 625 TEST TO ENSURE EXPONENTIATION OPERATOR IS       
!                        NOT COMMUTATIVE                                 
!                                                                        
      ivtnum = 624                                                      
!                                                                        
!       ****  TEST 624  ****                                             
!                                                                        
      if (iczero) 36240, 6240, 36240                                    
 6240 continue                                                          
      ivcomp = 3 ** 9                                                   
      goto 46240                                                       
36240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46240, 6251, 46240                                    
46240 if (ivcomp - 19683) 26240,16240,26240                             
16240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6251                                                        
26240 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6251 continue                                                          
      ivtnum = 625                                                      
!                                                                        
!       ****  TEST 625  ****                                             
!                                                                        
      if (iczero) 36250, 6250, 36250                                    
 6250 continue                                                          
      ivcomp = 9 ** 3                                                   
      goto 46250                                                       
36250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46250, 6261, 46250                                    
46250 if (ivcomp - 729) 26250,16250,26250                               
16250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6261                                                        
26250 ivfail = ivfail + 1                                               
      ivcorr = 729                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6261 continue                                                          
!                                                                        
!      TESTS 626 THROUGH 631 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE
!                            ODD AND EVEN NUMBER POWERS CHECKING THE SIGN
!                            OF THE RESULTS                              
!                                                                        
      ivtnum = 626                                                      
!                                                                        
!       ****  TEST 626  ****                                             
!                                                                        
      if (iczero) 36260, 6260, 36260                                    
 6260 continue                                                          
      ivcomp = 1 ** 2                                                   
      goto 46260                                                       
36260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46260, 6271, 46260                                    
46260 if (ivcomp - 1) 26260,16260,26260                                 
16260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6271                                                        
26260 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6271 continue                                                          
      ivtnum = 627                                                      
!                                                                        
!       ****  TEST 627  ****                                             
!                                                                        
      if (iczero) 36270, 6270, 36270                                    
 6270 continue                                                          
      ivcomp= (-1) ** 2                                                 
      goto 46270                                                       
36270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46270, 6281, 46270                                    
46270 if (ivcomp - 1) 26270,16270,26270                                 
16270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6281                                                        
26270 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6281 continue                                                          
      ivtnum = 628                                                      
!                                                                        
!       ****  TEST 628  ****                                             
!                                                                        
      if (iczero) 36280, 6280, 36280                                    
 6280 continue                                                          
      ivcomp = 7 ** 3                                                   
      goto 46280                                                       
36280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46280, 6291, 46280                                    
46280 if (ivcomp - 343) 26280,16280,26280                               
16280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6291                                                        
26280 ivfail = ivfail + 1                                               
      ivcorr = 343                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6291 continue                                                          
      ivtnum = 629                                                      
!                                                                        
!       ****  TEST 629  ****                                             
!                                                                        
      if (iczero) 36290, 6290, 36290                                    
 6290 continue                                                          
      ivcomp = (-7) ** 3                                                
      goto 46290                                                       
36290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46290, 6301, 46290                                    
46290 if (ivcomp + 343) 26290,16290,26290                               
16290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6301                                                        
26290 ivfail = ivfail + 1                                               
      ivcorr = -343                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6301 continue                                                          
      ivtnum = 630                                                      
!                                                                        
!       ****  TEST 630  ****                                             
!                                                                        
      if (iczero) 36300, 6300, 36300                                    
 6300 continue                                                          
      ivcomp = 7 ** 4                                                   
      goto 46300                                                       
36300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46300, 6311, 46300                                    
46300 if (ivcomp - 2401) 26300,16300,26300                              
16300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6311                                                        
26300 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6311 continue                                                          
      ivtnum = 631                                                      
!                                                                        
!       ****  TEST 631  ****                                             
!                                                                        
      if (iczero) 36310, 6310, 36310                                    
 6310 continue                                                          
      ivcomp = (-7) ** 4                                                
      goto 46310                                                       
36310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46310, 6321, 46310                                    
46310 if (ivcomp - 2401) 26310,16310,26310                              
16310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6321                                                        
26310 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6321 continue                                                          
      ivtnum = 632                                                      
!                                                                        
!       ****  TEST 632  ****                                             
!      TEST 632  - SMALL NUMBER BASE; ZERO EXPONENT                      
!                                                                        
      if (iczero) 36320, 6320, 36320                                    
 6320 continue                                                          
      ivon01 = 1                                                        
      ivcomp = ivon01 ** 1                                              
      goto 46320                                                       
36320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46320, 6331, 46320                                    
46320 if (ivcomp - 1) 26320,16320,26320                                 
16320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6331                                                        
26320 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6331 continue                                                          
      ivtnum = 633                                                      
!                                                                        
!       ****  TEST 633  ****                                             
!      TEST 633  - ZERO BASE TO FIRST POWER                              
!                                                                        
      if (iczero) 36330, 6330, 36330                                    
 6330 continue                                                          
      ivon01 = 0                                                        
      ivcomp = ivon01 ** 1                                              
      goto 46330                                                       
36330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46330, 6341, 46330                                    
46330 if (ivcomp) 26330,16330,26330                                     
16330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6341                                                        
26330 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6341 continue                                                          
      ivtnum = 634                                                      
!                                                                        
!       ****  TEST 634  ****                                             
!      TEST 634  - BASE =1; EXPONENT = 1                                 
!                                                                        
      if (iczero) 36340, 6340, 36340                                    
 6340 continue                                                          
      ivon01 = 1                                                        
      ivcomp = ivon01 ** 1                                              
      goto 46340                                                       
36340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46340, 6351, 46340                                    
46340 if (ivcomp - 1) 26340,16340,26340                                 
16340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6351                                                        
26340 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6351 continue                                                          
      ivtnum = 635                                                      
!                                                                        
!       ****  TEST 635  ****                                             
!      TEST 635  - LARGE EXPONENT                                        
!                                                                        
      if (iczero) 36350, 6350, 36350                                    
 6350 continue                                                          
      ivon01 = 1                                                        
      ivcomp = ivon01 ** 32767                                          
      goto 46350                                                       
36350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46350, 6361, 46350                                    
46350 if (ivcomp - 1) 26350,16350,26350                                 
16350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6361                                                        
26350 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6361 continue                                                          
      ivtnum = 636                                                      
!                                                                        
!       ****  TEST 636  ****                                             
!      TEST 636  - LARGE NUMBER BASE; EXPONENT = 1                       
!                                                                        
      if (iczero) 36360, 6360, 36360                                    
 6360 continue                                                          
      ivon01 = 32767                                                    
      ivcomp = ivon01 ** 1                                              
      goto 46360                                                       
36360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46360, 6371, 46360                                    
46360 if (ivcomp - 32767) 26360,16360,26360                             
16360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6371                                                        
26360 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6371 continue                                                          
      ivtnum = 637                                                      
!                                                                        
!       ****  TEST 637  ****                                             
!      TEST 637  - ZERO BASE; LARGE NUMBER EXPONENT                      
!                                                                        
      if (iczero) 36370, 6370, 36370                                    
 6370 continue                                                          
      ivon01 = 0                                                        
      ivcomp = ivon01 ** 32767                                          
      goto 46370                                                       
36370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46370, 6381, 46370                                    
46370 if (ivcomp) 26370,16370,26370                                     
16370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6381                                                        
26370 ivfail = ivfail +1                                                
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6381 continue                                                          
      ivtnum = 638                                                      
!                                                                        
!       ****  TEST 638  ****                                             
!      TEST 638  -LARGE NUMBER BASE; ZERO EXPONENT                       
!                                                                        
      if (iczero) 36380, 6380, 36380                                    
 6380 continue                                                          
      ivon01 = 32767                                                    
      ivcomp = ivon01 ** 0                                              
      goto 46380                                                       
36380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46380, 6391, 46380                                    
46380 if (ivcomp - 1) 26380,16380,26380                                 
16380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6391                                                        
26380 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6391 continue                                                          
      ivtnum = 639                                                      
!                                                                        
!       ****  TEST 639  ****                                             
!      TEST 639  -EXPONENT IS POWER OF TWO                               
!                                                                        
      if (iczero) 36390, 6390, 36390                                    
 6390 continue                                                          
      ivon01 = 181                                                      
      ivcomp = ivon01 ** 2                                              
      goto 46390                                                       
36390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46390, 6401, 46390                                    
46390 if (ivcomp - 32761) 26390,16390,26390                             
16390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6401                                                        
26390 ivfail = ivfail + 1                                               
      ivcorr = 32761                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6401 continue                                                          
      ivtnum = 640                                                      
!                                                                        
!       ****  TEST 640  ****                                             
!      TEST 640  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              
!                                                                        
      if (iczero) 36400, 6400, 36400                                    
 6400 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 ** 8                                              
      goto 46400                                                       
36400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46400, 6411, 46400                                    
46400 if (ivcomp - 256) 26400,16400,26400                               
16400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6411                                                        
26400 ivfail = ivfail + 1                                               
      ivcorr = 256                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6411 continue                                                          
!                                                                        
!      TESTS 641 AND 642 TEST TO ENSURE EXPONENTIATION OPERATOR IS       
!                        NOT COMMUTATIVE                                 
!                                                                        
      ivtnum = 641                                                      
!                                                                        
!       ****  TEST 641  ****                                             
!                                                                        
      if (iczero) 36410, 6410, 36410                                    
 6410 continue                                                          
      ivon01 = 3                                                        
      ivcomp = ivon01 ** 9                                              
      goto 46410                                                       
36410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46410, 6421, 46410                                    
46410 if (ivcomp - 19683) 26410,16410,26410                             
16410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6421                                                        
26410 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6421 continue                                                          
      ivtnum = 642                                                      
!                                                                        
!       ****  TEST 642  ****                                             
!                                                                        
      if (iczero) 36420, 6420, 36420                                    
 6420 continue                                                          
      ivon01 = 9                                                        
      ivcomp = ivon01 ** 3                                              
      goto 46420                                                       
36420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46420, 6431, 46420                                    
46420 if (ivcomp - 729) 26420,16420,26420                               
16420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6431                                                        
26420 ivfail = ivfail + 1                                               
      ivcorr = 729                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6431 continue                                                          
!                                                                        
!      TESTS 643 THROUGH 648 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE
!                            ODD AND EVEN NUMBER POWERS CHECKING THE SIGN
!                            OF THE RESULTS                              
!                                                                        
      ivtnum = 643                                                      
!                                                                        
!       ****  TEST 643  ****                                             
!                                                                        
      if (iczero) 36430, 6430, 36430                                    
 6430 continue                                                          
      ivon01 = 1                                                        
      ivcomp = ivon01 ** 2                                              
      goto 46430                                                       
36430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46430, 6441, 46430                                    
46430 if (ivcomp - 1) 26430,16430,26430                                 
16430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6441                                                        
26430 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6441 continue                                                          
      ivtnum = 644                                                      
!                                                                        
!       ****  TEST 644  ****                                             
!                                                                        
      if (iczero) 36440, 6440, 36440                                    
 6440 continue                                                          
      ivon01 = -1                                                       
      ivcomp = ivon01 ** 2                                              
      goto 46440                                                       
36440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46440, 6451, 46440                                    
46440 if (ivcomp - 1) 26440,16440,26440                                 
16440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6451                                                        
26440 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6451 continue                                                          
      ivtnum = 645                                                      
!                                                                        
!       ****  TEST 645  ****                                             
!                                                                        
      if (iczero) 36450, 6450, 36450                                    
 6450 continue                                                          
      ivon01 = 7                                                        
      ivcomp = ivon01 ** 3                                              
      goto 46450                                                       
36450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46450, 6461, 46450                                    
46450 if (ivcomp - 343) 26450,16450,26450                               
16450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6461                                                        
26450 ivfail = ivfail + 1                                               
      ivcorr = 343                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6461 continue                                                          
      ivtnum = 646                                                      
!                                                                        
!       ****  TEST 646  ****                                             
!                                                                        
      if (iczero) 36460, 6460, 36460                                    
 6460 continue                                                          
      ivon01 = -7                                                       
      ivcomp = ivon01 ** 3                                              
      goto 46460                                                       
36460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46460, 6471, 46460                                    
46460 if (ivcomp + 343) 26460,16460,26460                               
16460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6471                                                        
26460 ivfail = ivfail + 1                                               
      ivcorr = -343                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6471 continue                                                          
      ivtnum = 647                                                      
!                                                                        
!       ****  TEST 647  ****                                             
!                                                                        
      if (iczero) 36470, 6470, 36470                                    
 6470 continue                                                          
      ivon01 = 7                                                        
      ivcomp = ivon01 ** 4                                              
      goto 46470                                                       
36470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46470, 6481, 46470                                    
46470 if (ivcomp - 2401) 26470,16470,26470                              
16470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6481                                                        
26470 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6481 continue                                                          
      ivtnum = 648                                                      
!                                                                        
!       ****  TEST 648  ****                                             
!                                                                        
      if (iczero) 36480, 6480, 36480                                    
 6480 continue                                                          
      ivon01 = -7                                                       
      ivcomp = ivon01 ** 4                                              
      goto 46480                                                       
36480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46480, 6491, 46480                                    
46480 if (ivcomp - 2401) 26480,16480,26480                              
16480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6491                                                        
26480 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6491 continue                                                          
!       ***    END OF TESTS    ***                                       
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
90007 format (" ",20x,"END OF PROGRAM FM041" )                          
      end program fm041
