      program fm004
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
!      COMMENT SECTION                                                   
!                                                                        
!      FM004                                                             
!                                                                        
!          THIS ROUTINE CONTAINS BASIC ARITHMETIC IF STATEMENT TESTS.    
!      THE STATEMENT FORMAT IS                                           
!                 IF  (E)  K1, K2, K3                                    
!      WHERE E IS A SIMPLE INTEGER EXPRESSION OF FORM                    
!                 VARIABLE - CONSTANT                                    
!                 VARIABLE + CONSTANT                                    
!      AND K1, K2 AND K3 ARE STATEMENT LABELS.  ONLY THE STATEMENTS IN   
!      THE BASIC ASSUMPTIONS ARE INCLUDED IN THESE TESTS.                
!          EXECUTION OF AN IF STATEMENT CAUSES EVALUATION OF THE         
!      EXPRESSION E FOLLOWING WHICH THE STATEMENT LABEL K1, K2 OR K3     
!      IS EXECUTED NEXT AS THE VALUE OF E IS LESS THAN ZERO, ZERO, OR    
!      GREATER THAN ZERO, RESPECTIVELY.                                  
!                                                                        
!          THE BASIC UNCONDITIONAL GO TO STATEMENT IS TESTED IN THIS     
!      ROUTINE. THE STATEMENT IS OF THE FORM                             
!                GO TO K                                                 
!      WHERE K IS A STATEMENT LABEL.                                     
!          EXECUTION OF AN UNCONDITIONAL GO TO STATEMENT CAUSES THE      
!      STATEMENT IDENTIFIED BY STATEMENT LABEL K TO BE EXECUTED NEXT.    
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 3.6, NORMAL EXECUTION SEQUENCE AND TRANSFER OF CONTROL 
!         SECTION 11.1, GO TO STATEMENT                                  
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
!           TEST SECTION                                                 
!                                                                        
!          TESTS 21, 22, AND 23 CONTAIN THE SAME IF STATEMENT BUT THE    
!      EXPECTED BRANCH IS TO THE FIRST, SECOND OR THIRD STATEMENT LABEL  
!      AS THE INTEGER EXPRESSION IS LESS THAN ZERO, EQUAL TO ZERO, OR    
!      GREATER THAN ZERO RESPECTIVELY.                                   
!                                                                        
  211 continue                                                          
      ivtnum =  21                                                      
!                                                                        
!       ****  TEST 021  ****                                             
!      TEST 21 - ARITHMETIC IF STATEMENT TEST                            
!          LESS THAN ZERO BRANCH EXPECTED.                               
!                                                                        
      if (iczero) 30210,  210, 30210                                    
  210 continue                                                          
      ivon01=2                                                          
      if (ivon01 - 3) 212,213,214                                       
  212 ivon02 = -1                                                       
      goto 40210                                                       
  213 ivon02 = 0                                                        
      goto 40210                                                       
  214 ivon02 = 1                                                        
      goto 40210                                                       
30210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40210,  221, 40210                                    
40210 if (ivon02) 10210, 20210, 20210                                   
10210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  221                                                        
20210 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr=-1                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  221 continue                                                          
      ivtnum =  22                                                      
!                                                                        
!       ****  TEST 022  ****                                             
!      TEST 22 - ARITHMETIC IF STATEMENT TEST                            
!          EQUAL TO ZERO BRANCH EXPECTED                                 
!                                                                        
      if (iczero) 30220,  220, 30220                                    
  220 continue                                                          
      ivon01 = 3                                                        
      if (ivon01 - 3) 222,223,224                                       
  222 ivon02 = -1                                                       
      goto 40220                                                       
  223 ivon02 = 0                                                        
      goto 40220                                                       
  224 ivon02 = 1                                                        
      goto 40220                                                       
30220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40220,  231, 40220                                    
40220 if (ivon02) 20220, 10220, 20220                                   
10220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  231                                                        
20220 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr= 0                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  231 continue                                                          
      ivtnum =  23                                                      
!                                                                        
!       ****  TEST 023  ****                                             
!      TEST 23 - ARITHMETIC IF STATEMENT TEST                            
!          GREATER THAN ZERO BRANCH EXPECTED                             
!                                                                        
      if (iczero) 30230,  230, 30230                                    
  230 continue                                                          
      ivon01 = 4                                                        
      if (ivon01 - 3) 232,233,234                                       
  232 ivon02 = -1                                                       
      goto 40230                                                       
  233 ivon02 = 0                                                        
      goto 40230                                                       
  234 ivon02 = 1                                                        
      goto 40230                                                       
30230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40230,  241, 40230                                    
40230 if (ivon02) 20230, 20230, 10230                                   
10230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  241                                                        
20230 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TESTS 24 THROUGH 29 CONTAIN AN IF STATEMENT WITH TWO OF THE   
!      THREE BRANCH STATEMENT LABELS EQUAL.                              
!                                                                        
  241 continue                                                          
      ivtnum =  24                                                      
!                                                                        
!       ****  TEST 024  ****                                             
!      TEST 24 - ARITHMETIC IF STATEMENT TEST                            
!          LESS THAN ZERO BRANCH EXPECTED                                
!                                                                        
      if (iczero) 30240,  240, 30240                                    
  240 continue                                                          
      ivon01=2                                                          
      if (ivon01 - 3) 242,243,242                                       
  242 ivon02=-1                                                         
      goto 40240                                                       
  243 ivon02=0                                                          
      goto 40240                                                       
30240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40240,  251, 40240                                    
40240 if (ivon02) 10240, 20240, 20240                                   
10240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  251                                                        
20240 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr=-1                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  251 continue                                                          
      ivtnum =  25                                                      
!                                                                        
!       ****  TEST 025  ****                                             
!      TEST 25 - ARITHMETIC IF STATEMENT TEST                            
!          EQUAL TO ZERO BRANCH EXPECTED                                 
!                                                                        
      if (iczero) 30250,  250, 30250                                    
  250 continue                                                          
      ivon01=3                                                          
      if (ivon01 - 3) 252,253,252                                       
  252 ivon02= -1                                                        
      goto 40250                                                       
  253 ivon02 = 0                                                        
      goto 40250                                                       
30250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40250,  261, 40250                                    
40250 if (ivon02) 20250,10250,20250                                     
10250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  261                                                        
20250 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr=0                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  261 continue                                                          
      ivtnum =  26                                                      
!                                                                        
!       ****  TEST 026  ****                                             
!      TEST 26 - ARITHMETIC IF STATEMENT TEST                            
!          GREATER THAN ZERO BRANCH EXPECTED                             
!                                                                        
      if (iczero) 30260,  260, 30260                                    
  260 continue                                                          
      ivon01=4                                                          
      if (ivon01-3) 262, 263, 262                                       
  262 ivon02= 1                                                         
      goto 40260                                                       
  263 ivon02 = 0                                                        
      goto 40260                                                       
30260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40260,  271, 40260                                    
40260 if (ivon02) 20260, 20260, 10260                                   
10260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  271                                                        
20260 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  271 continue                                                          
      ivtnum =  27                                                      
!                                                                        
!       ****  TEST 027  ****                                             
!      TEST 27 - ARITHMETIC IF STATEMENT TEST                            
!          LESS THAN ZERO BRANCH EXPECTED                                
!                                                                        
      if (iczero) 30270,  270, 30270                                    
  270 continue                                                          
      ivon01 = -4                                                       
      if (ivon01 + 3) 272, 272, 273                                     
  272 ivon02= -1                                                        
      goto 40270                                                       
  273 ivon02 = 1                                                        
      goto 40270                                                       
30270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40270,  281, 40270                                    
40270 if (ivon02) 10270, 20270, 20270                                   
10270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  281                                                        
20270 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr= -1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  281 continue                                                          
      ivtnum =  28                                                      
!                                                                        
!       ****  TEST 028  ****                                             
!      TEST 28 - ARITHMETIC IF STATEMENT TEST                            
!          EQUAL TO ZERO BRANCH EXPECTED                                 
!                                                                        
      if (iczero) 30280,  280, 30280                                    
  280 continue                                                          
      ivon01 = -3                                                       
      if (ivon01 + 3) 282, 282, 283                                     
  282 ivon02 = 0                                                        
      goto 40280                                                       
  283 ivon02 = 1                                                        
      goto 40280                                                       
30280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40280,  291, 40280                                    
40280 if (ivon02) 20280, 10280, 20280                                   
10280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  291                                                        
20280 ivfail = ivfail + 1                                               
      ivcomp=ivon02                                                     
      ivcorr= 0                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  291 continue                                                          
      ivtnum =  29                                                      
!                                                                        
!       ****  TEST 029  ****                                             
!      TEST 29 - ARITHMETIC IF STATEMENT TEST                            
!          GREATER THAN ZERO BRANCH EXPECTED                             
!                                                                        
      if (iczero) 30290,  290, 30290                                    
  290 continue                                                          
      ivon01 = -2                                                       
      if (ivon01 + 3) 292,292,293                                       
  292 ivon02 = -1                                                       
      goto 40290                                                       
  293 ivon02 = 1                                                        
      goto 40290                                                       
30290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40290,  301, 40290                                    
40290 if (ivon02) 20290, 20290, 10290                                   
10290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  301                                                        
20290 ivfail = ivfail + 1                                               
      ivcomp= ivon02                                                    
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TESTS 30 AND 31 CONTAIN THE BASIC GO TO STATEMENT TESTS.      
!                                                                        
  301 continue                                                          
      ivtnum =  30                                                      
!                                                                        
!       ****  TEST 030  ****                                             
!      TEST 30 - UNCONDITIONAL GO TO STATEMENT TEST                      
!                                                                        
      if (iczero) 30300,  300, 30300                                    
  300 continue                                                          
      ivon01 = 1                                                        
      goto 302                                                         
  303 ivon01 = 2                                                        
      goto 304                                                         
  302 ivon01 = 3                                                        
      goto 303                                                         
  304 goto 40300                                                       
30300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40300,  311, 40300                                    
40300 if (ivon01 - 2) 20300,10300,20300                                 
10300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  311                                                        
20300 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  311 continue                                                          
      ivtnum =  31                                                      
!                                                                        
!       ****  TEST 031  ****                                             
!      TEST 31 - UNCONDITIONAL GO TO STATEMENT TEST                      
!                                                                        
      if (iczero) 30310,  310, 30310                                    
  310 continue                                                          
      ivon01 = 1                                                        
      goto 316                                                         
  313 goto 317                                                         
  314 ivon01 = 3                                                        
      goto 40310                                                       
  315 goto 313                                                         
  316 goto 315                                                         
  317 goto 314                                                         
30310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40310,  321, 40310                                    
40310 if (ivon01 - 3) 20310, 10310, 20310                               
10310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  321                                                        
20310 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  321 continue                                                          
      ivtnum =  32                                                      
!                                                                        
!       ****  TEST 032  ****                                             
!          TEST 32 - ARITHMETIC IF STATEMENT AND UNCONDITIONAL GO TO     
!                    STATEMENT                                           
!      THIS TEST COMBINES THE BASIC ARITHMETIC IF STATEMENTS AND         
!      UNCONDITIONAL GO TO STATEMENTS IN ONE TEST.                       
!                                                                        
      if (iczero) 30320,  320, 30320                                    
  320 continue                                                          
      ivon01 = 1                                                        
      goto 322                                                         
  324 ivon01 = 2                                                        
      if (ivon01 -1) 323, 323, 325                                      
  327 ivon01 = 5                                                        
      goto 328                                                         
  326 ivon01 = -4                                                       
      if (ivon01 + 4) 323, 327, 323                                     
  322 if (ivon01 - 1) 323, 324, 323                                     
  323 goto 20320                                                       
  325 ivon01 = 3                                                        
      if (ivon01 -4) 326,323,323                                        
  328 goto 40320                                                       
30320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40320,  331, 40320                                    
40320 if (ivon01 - 5) 20320, 10320, 20320                               
10320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  331                                                        
20320 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=5                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  331 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM004" )                          
      end program fm004
