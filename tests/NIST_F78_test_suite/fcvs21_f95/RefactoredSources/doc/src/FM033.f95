      program fm033
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM033                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM                                                              
!              INTEGER VARIABLE = ARITHMETIC EXPRESSION                  
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR * AND INTEGER CONSTANTS.  SOME OF THE TESTS USE PARENS   
!      TO GROUP ELEMENTS IN THE EXPRESSION AND TO ALLOW THE USE OF       
!      NEGATIVE CONSTANTS FOLLOWING THE * OPERATOR.                      
!                                                                        
!      THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS          
!          (1)  INTEGER CONSTANT * INTEGER CONSTANT                      
!          (2)  INTEGER CONSTANT * INTEGER CONSTANT * INTEGER CONSTANT   
!          (3)  SAME AS (2) BUT WITH PARENS TO GROUP ELEMENTS            
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
!      TEST 360 THROUGH TEST 376 CONTAIN TWO INTEGER CONSTANTS AND       
!      OPERATOR * IN AN ARITHMETIC EXPRESSION.                           
!               IV = IC * IC                                             
!                                                                        
!      TEST 360 THROUGH TEST 365  - INTEGER CONSTANTS ARE POSITIVE       
!                                                                        
 3601 continue                                                          
      ivtnum = 360                                                      
!                                                                        
!        ****  TEST 360  ****                                            
!                                                                        
      if (iczero) 33600, 3600, 33600                                    
 3600 continue                                                          
      ivcomp = 2 * 3                                                    
      goto 43600                                                       
33600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43600, 3611, 43600                                    
43600 if (ivcomp - 6) 23600,13600,23600                                 
13600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3611                                                        
23600 ivfail = ivfail + 1                                               
      ivcorr=6                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3611 continue                                                          
      ivtnum = 361                                                      
!                                                                        
!       ****  TEST 361  ****                                             
!                                                                        
      if (iczero) 33610, 3610, 33610                                    
 3610 continue                                                          
      ivcomp = 3*2                                                      
      goto 43610                                                       
33610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43610, 3621, 43610                                    
43610 if (ivcomp-6) 23610,13610,23610                                   
13610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3621                                                        
23610 ivfail = ivfail + 1                                               
      ivcorr=6                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3621 continue                                                          
      ivtnum = 362                                                      
!                                                                        
!       ****  TEST 362  ****                                             
!                                                                        
      if (iczero) 33620, 3620, 33620                                    
 3620 continue                                                          
      ivcomp=13*11                                                      
      goto 43620                                                       
33620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43620, 3631, 43620                                    
43620 if (ivcomp-143) 23620,13620,23620                                 
13620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3631                                                        
23620 ivfail = ivfail + 1                                               
      ivcorr=143                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3631 continue                                                          
      ivtnum = 363                                                      
!                                                                        
!       ****  TEST 363  ****                                             
!                                                                        
      if (iczero) 33630, 3630, 33630                                    
 3630 continue                                                          
      ivcomp = 223*99                                                   
      goto 43630                                                       
33630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43630, 3641, 43630                                    
43630 if (ivcomp-22077) 23630,13630,23630                               
13630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3641                                                        
23630 ivfail = ivfail + 1                                               
      ivcorr=22077                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3641 continue                                                          
      ivtnum = 364                                                      
!                                                                        
!       ****  TEST 364  ****                                             
!                                                                        
      if (iczero) 33640, 3640, 33640                                    
 3640 continue                                                          
      ivcomp=11235*2                                                    
      goto 43640                                                       
33640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43640, 3651, 43640                                    
43640 if (ivcomp-22470) 23640,13640,23640                               
13640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3651                                                        
23640 ivfail = ivfail + 1                                               
      ivcorr=22470                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3651 continue                                                          
      ivtnum = 365                                                      
!                                                                        
!       ****  TEST 365  ****                                             
!                                                                        
      if (iczero) 33650, 3650, 33650                                    
 3650 continue                                                          
      ivcomp = 2*16383                                                  
      goto 43650                                                       
33650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43650, 3661, 43650                                    
43650 if (ivcomp-32766) 23650,13650,23650                               
13650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3661                                                        
23650 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 366 THROUGH TEST 371                                         
!          ONE POSITIVE AND ONE NEGATIVE CONSTANT                        
!                                                                        
 3661 continue                                                          
      ivtnum = 366                                                      
!                                                                        
!       ****  TEST 366  ****                                             
!                                                                        
      if (iczero) 33660, 3660, 33660                                    
 3660 continue                                                          
      ivcomp =2*(-3)                                                    
      goto 43660                                                       
33660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43660, 3671, 43660                                    
43660 if (ivcomp+6) 23660,13660,23660                                   
13660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3671                                                        
23660 ivfail = ivfail + 1                                               
      ivcorr = -6                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3671 continue                                                          
      ivtnum = 367                                                      
!                                                                        
!       ****  TEST 367  ****                                             
!                                                                        
      if (iczero) 33670, 3670, 33670                                    
 3670 continue                                                          
      ivcomp=(-2)*3                                                     
      goto 43670                                                       
33670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43670, 3681, 43670                                    
43670 if (ivcomp+6)23670,13670,23670                                    
13670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3681                                                        
23670 ivfail = ivfail + 1                                               
      ivcorr =-6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3681 continue                                                          
      ivtnum = 368                                                      
!                                                                        
!       ****  TEST 368  ****                                             
!                                                                        
      if (iczero) 33680, 3680, 33680                                    
 3680 continue                                                          
      ivcomp= -2*3                                                      
      goto 43680                                                       
33680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43680, 3691, 43680                                    
43680 if (ivcomp +6) 23680,13680,23680                                  
13680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3691                                                        
23680 ivfail = ivfail + 1                                               
      ivcorr=-6                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3691 continue                                                          
      ivtnum = 369                                                      
!                                                                        
!       ****  TEST 369  ****                                             
!                                                                        
      if (iczero) 33690, 3690, 33690                                    
 3690 continue                                                          
      ivcomp = (-13)*11                                                 
      goto 43690                                                       
33690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43690, 3701, 43690                                    
43690 if (ivcomp+143) 23690,13690,23690                                 
13690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3701                                                        
23690 ivfail = ivfail + 1                                               
      ivcorr=-143                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3701 continue                                                          
      ivtnum = 370                                                      
!                                                                        
!       ****  TEST 370  ****                                             
!                                                                        
      if (iczero) 33700, 3700, 33700                                    
 3700 continue                                                          
      ivcomp = 223 * (-99)                                              
      goto 43700                                                       
33700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43700, 3711, 43700                                    
43700 if (ivcomp + 22077) 23700,13700,23700                             
13700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3711                                                        
23700 ivfail = ivfail + 1                                               
      ivcorr =-22077                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3711 continue                                                          
      ivtnum = 371                                                      
!                                                                        
!       ****  TEST 371  ****                                             
!                                                                        
      if (iczero) 33710, 3710, 33710                                    
 3710 continue                                                          
      ivcomp= -2 * 16383                                                
      goto 43710                                                       
33710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43710, 3721, 43710                                    
43710 if (ivcomp+32766) 23710,13710,23710                               
13710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3721                                                        
23710 ivfail = ivfail + 1                                               
      ivcorr= -32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 372 THROUGH TEST 376 - TWO NEGATIVE CONSTANTS                
!                                                                        
 3721 continue                                                          
      ivtnum = 372                                                      
!                                                                        
!       ****  TEST 372  ****                                             
!                                                                        
      if (iczero) 33720, 3720, 33720                                    
 3720 continue                                                          
      ivcomp=(-2)*(-3)                                                  
      goto 43720                                                       
33720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43720, 3731, 43720                                    
43720 if (ivcomp-6) 23720,13720,23720                                   
13720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3731                                                        
23720 ivfail = ivfail + 1                                               
      ivcorr=6                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3731 continue                                                          
      ivtnum = 373                                                      
!                                                                        
!       ****  TEST 373  ****                                             
!                                                                        
      if (iczero) 33730, 3730, 33730                                    
 3730 continue                                                          
      ivcomp = -2*(-3)                                                  
      goto 43730                                                       
33730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43730, 3741, 43730                                    
43730 if (ivcomp-6) 23730,13730,23730                                   
13730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3741                                                        
23730 ivfail = ivfail + 1                                               
      ivcorr=6                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3741 continue                                                          
      ivtnum = 374                                                      
!                                                                        
!       ****  TEST 374  ****                                             
!                                                                        
      if (iczero) 33740, 3740, 33740                                    
 3740 continue                                                          
      ivcomp=(-13)*(-11)                                                
      goto 43740                                                       
33740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43740, 3751, 43740                                    
43740 if (ivcomp-143) 23740,13740,23740                                 
13740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3751                                                        
23740 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3751 continue                                                          
      ivtnum = 375                                                      
!                                                                        
!       ****  TEST 375  ****                                             
!                                                                        
      if (iczero) 33750, 3750, 33750                                    
 3750 continue                                                          
      ivcomp= -223 *(-99)                                               
      goto 43750                                                       
33750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43750, 3761, 43750                                    
43750 if (ivcomp - 22077) 23750,13750,23750                             
13750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3761                                                        
23750 ivfail = ivfail + 1                                               
      ivcorr = 22077                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3761 continue                                                          
      ivtnum = 376                                                      
!                                                                        
!       ****  TEST 376  ****                                             
!                                                                        
      if (iczero) 33760, 3760, 33760                                    
 3760 continue                                                          
      ivcomp = (-16383)*(-2)                                            
      goto 43760                                                       
33760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43760, 3771, 43760                                    
43760 if (ivcomp - 32766) 23760,13760,23760                             
13760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3771                                                        
23760 ivfail = ivfail + 1                                               
      ivcorr =32766                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 377 THROUGH TEST 394 CONTAIN THREE INTEGER CONSTANTS AND     
!      OPERATOR * IN AN ARITHMETIC EXPRESSION.                           
!                IV = IC * IC * IC                                       
!                                                                        
!      TEST 377 THROUGH TEST 382   - CONSTANTS ARE POSITIVE              
!                                                                        
 3771 continue                                                          
      ivtnum = 377                                                      
!                                                                        
!       ****  TEST 377  ****                                             
!                                                                        
      if (iczero) 33770, 3770, 33770                                    
 3770 continue                                                          
      ivcomp =2*3*4                                                     
      goto 43770                                                       
33770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43770, 3781, 43770                                    
43770 if (ivcomp-24) 23770,13770,23770                                  
13770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3781                                                        
23770 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3781 continue                                                          
      ivtnum = 378                                                      
!                                                                        
!       ****  TEST 378  ****                                             
!                                                                        
      if (iczero) 33780, 3780, 33780                                    
 3780 continue                                                          
      ivcomp = 2*3*55                                                   
      goto 43780                                                       
33780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43780, 3791, 43780                                    
43780 if (ivcomp-330) 23780,13780,23780                                 
13780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3791                                                        
23780 ivfail = ivfail + 1                                               
      ivcorr = 330                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3791 continue                                                          
      ivtnum = 379                                                      
!                                                                        
!       ****  TEST 379  ****                                             
!                                                                        
      if (iczero) 33790, 3790, 33790                                    
 3790 continue                                                          
      ivcomp = 23*51*13                                                 
      goto 43790                                                       
33790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43790, 3801, 43790                                    
43790 if (ivcomp-15249) 23790,13790,23790                               
13790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3801                                                        
23790 ivfail = ivfail + 1                                               
      ivcorr = 15249                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3801 continue                                                          
      ivtnum = 380                                                      
!                                                                        
!       ****  TEST 380  ****                                             
!                                                                        
      if (iczero) 33800, 3800, 33800                                    
 3800 continue                                                          
      ivcomp = 3* 5461* 2                                               
      goto 43800                                                       
33800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43800, 3811, 43800                                    
43800 if (ivcomp - 32766) 23800,13800,23800                             
13800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3811                                                        
23800 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3811 continue                                                          
      ivtnum = 381                                                      
!                                                                        
!       ****  TEST 381  ****                                             
!                                                                        
      if (iczero) 33810, 3810, 33810                                    
 3810 continue                                                          
      ivcomp = 16383*2*1                                                
      goto 43810                                                       
33810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43810, 3821, 43810                                    
43810 if (ivcomp-32766) 23810,13810,23810                               
13810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3821                                                        
23810 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3821 continue                                                          
      ivtnum = 382                                                      
!                                                                        
!       ****  TEST 382  ****                                             
!                                                                        
      if (iczero) 33820, 3820, 33820                                    
 3820 continue                                                          
      ivcomp = 3*53*157                                                 
      goto 43820                                                       
33820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43820, 3831, 43820                                    
43820 if (ivcomp-24963) 23820,13820,23820                               
13820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3831                                                        
23820 ivfail = ivfail + 1                                               
      ivcorr = 24963                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 383 THROUGH TEST 386                                         
!          THREE POSITIVE INTEGER CONSTANTS GROUPED WITH PARENS.         
!                                                                        
 3831 continue                                                          
      ivtnum = 383                                                      
!                                                                        
!       ****  TEST 383  ****                                             
!                                                                        
      if (iczero) 33830, 3830, 33830                                    
 3830 continue                                                          
      ivcomp = (2*3)*4                                                  
      goto 43830                                                       
33830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43830, 3841, 43830                                    
43830 if (ivcomp-24) 23830,13830,23830                                  
13830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3841                                                        
23830 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3841 continue                                                          
      ivtnum = 384                                                      
!                                                                        
!       ****  TEST 384  ****                                             
!                                                                        
      if (iczero) 33840, 3840, 33840                                    
 3840 continue                                                          
      ivcomp = 2*(3*4)                                                  
      goto 43840                                                       
33840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43840, 3851, 43840                                    
43840 if (ivcomp-24) 23840,13840,23840                                  
13840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3851                                                        
23840 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3851 continue                                                          
      ivtnum = 385                                                      
!                                                                        
!       ****  TEST 385 ****                                              
!                                                                        
      if (iczero) 33850, 3850, 33850                                    
 3850 continue                                                          
      ivcomp = (3*(+53)) * (+157)                                       
      goto 43850                                                       
33850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43850, 3861, 43850                                    
43850 if (ivcomp-24963) 23850,13850,23850                               
13850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3861                                                        
23850 ivfail = ivfail + 1                                               
      ivcorr = 24963                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3861 continue                                                          
      ivtnum = 386                                                      
!                                                                        
!       ****  TEST 386  ****                                             
!                                                                        
      if (iczero) 33860, 3860, 33860                                    
 3860 continue                                                          
      ivcomp = 3 *((+53)*157)                                           
      goto 43860                                                       
33860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43860, 3871, 43860                                    
43860 if (ivcomp-24963) 23860,13860,23860                               
13860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3871                                                        
23860 ivfail = ivfail + 1                                               
      ivcorr=24963                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 387 THROUGH TEST 391                                         
!          BOTH POSITIVE AND NEGATIVE CONSTANTS IN ARITHMETIC EXPRESSION.
!                                                                        
 3871 continue                                                          
      ivtnum = 387                                                      
!                                                                        
!       ****  TEST 387  ****                                             
!                                                                        
      if (iczero) 33870, 3870, 33870                                    
 3870 continue                                                          
      ivcomp = 2*3*(-4)                                                 
      goto 43870                                                       
33870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43870, 3881, 43870                                    
43870 if (ivcomp + 24) 23870,13870,23870                                
13870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3881                                                        
23870 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3881 continue                                                          
      ivtnum = 388                                                      
!                                                                        
!       ****  TEST 388  ****                                             
!                                                                        
      if (iczero) 33880, 3880, 33880                                    
 3880 continue                                                          
      ivcomp = 2*(-3)*(+4)                                              
      goto 43880                                                       
33880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43880, 3891, 43880                                    
43880 if (ivcomp + 24) 23880,13880,23880                                
13880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3891                                                        
23880 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3891 continue                                                          
      ivtnum = 389                                                      
!                                                                        
!       ****  TEST 389  ****                                             
!                                                                        
      if (iczero) 33890, 3890, 33890                                    
 3890 continue                                                          
      ivcomp = (-2)*3*4                                                 
      goto 43890                                                       
33890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43890, 3901, 43890                                    
43890 if (ivcomp+24) 23890,13890,23890                                  
13890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3901                                                        
23890 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3901 continue                                                          
      ivtnum = 390                                                      
!                                                                        
!       ****  TEST 390  ****                                             
!                                                                        
      if (iczero) 33900, 3900, 33900                                    
 3900 continue                                                          
      ivcomp = -2*3*4                                                   
      goto 43900                                                       
33900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43900, 3911, 43900                                    
43900 if (ivcomp+24) 23900,13900,23900                                  
13900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3911                                                        
23900 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3911 continue                                                          
      ivtnum = 391                                                      
!                                                                        
!       ****  TEST 391  ****                                             
!                                                                        
      if (iczero) 33910, 3910, 33910                                    
 3910 continue                                                          
      ivcomp = +2 * (-3) * (-4)                                         
      goto 43910                                                       
33910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43910, 3921, 43910                                    
43910 if (ivcomp - 24) 23910,13910,23910                                
13910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3921                                                        
23910 ivfail = ivfail + 1                                               
      ivcorr = 24                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 392 THROUGH TEST 394                                         
!          ALL CONSTANTS ARE NEGATIVE.                                   
!                                                                        
 3921 continue                                                          
      ivtnum = 392                                                      
!                                                                        
!       ****  TEST 392  ****                                             
!                                                                        
      if (iczero) 33920, 3920, 33920                                    
 3920 continue                                                          
      ivcomp = (-2)*(-3)*(-4)                                           
      goto 43920                                                       
33920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43920, 3931, 43920                                    
43920 if (ivcomp+24) 23920,13920,23920                                  
13920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3931                                                        
23920 ivfail = ivfail + 1                                               
      ivcorr = -24                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3931 continue                                                          
      ivtnum = 393                                                      
!                                                                        
!       ****  TEST 393  ****                                             
!                                                                        
      if (iczero) 33930, 3930, 33930                                    
 3930 continue                                                          
      ivcomp = (-23)*(-51)*(-13)                                        
      goto 43930                                                       
33930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43930, 3941, 43930                                    
43930 if (ivcomp + 15249) 23930,13930,23930                             
13930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3941                                                        
23930 ivfail = ivfail + 1                                               
      ivcorr = -15249                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3941 continue                                                          
      ivtnum = 394                                                      
!                                                                        
!       ****  TEST 394  ****                                             
!                                                                        
      if (iczero) 33940, 3940, 33940                                    
 3940 continue                                                          
      ivcomp = -3 * (-53)*( -157)                                       
      goto 43940                                                       
33940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 43940, 3951, 43940                                    
43940 if (ivcomp +24963) 23940,13940,23940                              
13940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3951                                                        
23940 ivfail = ivfail + 1                                               
      ivcorr = -24963                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!       ****   END OF TESTS   ****                                       
 3951 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM033" )                          
      end program fm033
