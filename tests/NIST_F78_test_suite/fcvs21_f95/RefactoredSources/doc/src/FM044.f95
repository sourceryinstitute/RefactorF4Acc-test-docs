      program fm044
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivcomp
      integer :: ivcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM044                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE FORM         
!      INTEGER VAR. = INTEGER VAR. <OP1> INTEGER VAR. <OP2> INTEGER VAR. 
!                                                                        
!      WHERE <OP1> AND <OP2> ARE ARITHMETIC OPERATORS.                   
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
!                   ARITHMETIC ASSIGNMENT STATEMENT                      
!                                                                        
!      TESTS 719 THROUGH 730 TEST STATEMENTS WHERE <OP1> IS '/' AND      
!      <OP2> VARIES.                                                     
!                                                                        
!      TESTS 731 THROUGH 746 TEST STATEMENTS WHERE <OP1> IS '**' AND     
!      <OP2> VARIES.                                                     
!                                                                        
!                                                                        
!      TEST 719 THROUGH 721 TEST '/' FOLLOWED BY '+'.                    
!                                                                        
      ivtnum = 719                                                      
!                                                                        
!       ****  TEST 719  ****                                             
!                                                                        
      if (iczero) 37190, 7190, 37190                                    
 7190 continue                                                          
      ivon01 = 108                                                      
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 / ivon02 + ivon03                                 
      goto 47190                                                       
37190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47190, 7201, 47190                                    
47190 if (ivcomp - 15) 27190,17190,27190                                
17190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7201                                                        
27190 ivfail = ivfail + 1                                               
      ivcorr = 15                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7201 continue                                                          
      ivtnum = 720                                                      
!                                                                        
!       ****  TEST 720  ****                                             
!                                                                        
      if (iczero) 37200, 7200, 37200                                    
 7200 continue                                                          
      ivon01 = 108                                                      
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 / ivon02) + ivon03                               
      goto 47200                                                       
37200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47200, 7211, 47200                                    
47200 if (ivcomp - 15) 27200,17200,27200                                
17200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7211                                                        
27200 ivfail = ivfail + 1                                               
      ivcorr = 15                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7211 continue                                                          
      ivtnum = 721                                                      
!                                                                        
!       ****  TEST 721  ****                                             
!                                                                        
      if (iczero) 37210, 7210, 37210                                    
 7210 continue                                                          
      ivon01 = 108                                                      
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 / (ivon02 + ivon03)                               
      goto 47210                                                       
37210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47210, 7221, 47210                                    
47210 if (ivcomp - 9) 27210,17210,27210                                 
17210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7221                                                        
27210 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7221 continue                                                          
!                                                                        
!      TEST 722 THROUGH 724 TEST '/' FOLLOWED BY '-'.                    
!                                                                        
      ivtnum = 722                                                      
!                                                                        
!       ****  TEST 722  ****                                             
!                                                                        
      if (iczero) 37220, 7220, 37220                                    
 7220 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = ivon01 / ivon02 - ivon03                                 
      goto 47220                                                       
37220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47220, 7231, 47220                                    
47220 if (ivcomp - 9) 27220,17220,27220                                 
17220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7231                                                        
27220 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7231 continue                                                          
      ivtnum = 723                                                      
!                                                                        
!       ****  TEST 723  ****                                             
!                                                                        
      if (iczero) 37230, 7230, 37230                                    
 7230 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = (ivon01 / ivon02) - ivon03                               
      goto 47230                                                       
37230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47230, 7241, 47230                                    
47230 if (ivcomp - 9) 27230,17230,27230                                 
17230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7241                                                        
27230 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7241 continue                                                          
      ivtnum = 724                                                      
!                                                                        
!       ****  TEST 724  ****                                             
!                                                                        
      if (iczero) 37240, 7240, 37240                                    
 7240 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = ivon01 / (ivon02 - ivon03)                               
      goto 47240                                                       
37240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47240, 7251, 47240                                    
47240 if (ivcomp - 18) 27240,17240,27240                                
17240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7251                                                        
27240 ivfail = ivfail + 1                                               
      ivcorr = 18                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7251 continue                                                          
!                                                                        
!      TEST 725 THROUGH 727 TEST '/' FOLLOWED BY '*'.                    
!                                                                        
      ivtnum = 725                                                      
!                                                                        
!       ****  TEST 725  ****                                             
!                                                                        
      if (iczero) 37250, 7250, 37250                                    
 7250 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = ivon01 / ivon02 * ivon03                                 
      goto 47250                                                       
37250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47250, 7261, 47250                                    
47250 if (ivcomp - 36) 27250,17250,27250                                
17250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7261                                                        
27250 ivfail = ivfail + 1                                               
      ivcorr = 36                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7261 continue                                                          
      ivtnum = 726                                                      
!                                                                        
!       ****  TEST 726  ****                                             
!                                                                        
      if (iczero) 37260, 7260, 37260                                    
 7260 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = (ivon01 / ivon02) * ivon03                               
      goto 47260                                                       
37260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47260, 7271, 47260                                    
47260 if (ivcomp - 36) 27260,17260,27260                                
17260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7271                                                        
27260 ivfail = ivfail + 1                                               
      ivcorr = 36                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7271 continue                                                          
      ivtnum = 727                                                      
!                                                                        
!       ****  TEST 727  ****                                             
!                                                                        
      if (iczero) 37270, 7270, 37270                                    
 7270 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = ivon01 / (ivon02 * ivon03)                               
      goto 47270                                                       
37270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47270, 7281, 47270                                    
47270 if (ivcomp - 4) 27270,17270,27270                                 
17270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7281                                                        
27270 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7281 continue                                                          
!                                                                        
!      TEST 728 THROUGH 730 TEST '/' FOLLOWED BY '**'.                   
!                                                                        
      ivtnum = 728                                                      
!                                                                        
!       ****  TEST 728  ****                                             
!                                                                        
      if (iczero) 37280, 7280, 37280                                    
 7280 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   3                                                      
      ivon03 =   2                                                      
      ivcomp = ivon01 / ivon02 ** ivon03                                
      goto 47280                                                       
37280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47280, 7291, 47280                                    
47280 if (ivcomp - 12) 27280,17280,27280                                
17280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7291                                                        
27280 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7291 continue                                                          
      ivtnum = 729                                                      
!                                                                        
!       ****  TEST 729  ****                                             
!                                                                        
      if (iczero) 37290, 7290, 37290                                    
 7290 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   3                                                      
      ivon03 =   2                                                      
      ivcomp = (ivon01 / ivon02) ** ivon03                              
      goto 47290                                                       
37290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47290, 7301, 47290                                    
47290 if (ivcomp - 1296) 27290,17290,27290                              
17290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7301                                                        
27290 ivfail = ivfail + 1                                               
      ivcorr = 1296                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7301 continue                                                          
      ivtnum = 730                                                      
!                                                                        
!       ****  TEST 730  ****                                             
!                                                                        
      if (iczero) 37300, 7300, 37300                                    
 7300 continue                                                          
      ivon01 = 108                                                      
      ivon02 =   3                                                      
      ivon03 =   2                                                      
      ivcomp = ivon01 / (ivon02 ** ivon03)                              
      goto 47300                                                       
37300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47300, 7311, 47300                                    
47300 if (ivcomp - 12) 27300,17300,27300                                
17300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7311                                                        
27300 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7311 continue                                                          
!                                                                        
!      TEST 731 THROUGH 733 TEST '**' FOLLOWED BY '+'.                   
!                                                                        
      ivtnum = 731                                                      
!                                                                        
!       ****  TEST 731  ****                                             
!                                                                        
      if (iczero) 37310, 7310, 37310                                    
 7310 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 5                                                        
      ivon03 = 4                                                        
      ivcomp = ivon01 ** ivon02 + ivon03                                
      goto 47310                                                       
37310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47310, 7321, 47310                                    
47310 if (ivcomp - 247) 27310,17310,27310                               
17310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7321                                                        
27310 ivfail = ivfail + 1                                               
      ivcorr = 247                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7321 continue                                                          
      ivtnum = 732                                                      
!                                                                        
!       ****  TEST 732  ****                                             
!                                                                        
      if (iczero) 37320, 7320, 37320                                    
 7320 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 5                                                        
      ivon03 = 4                                                        
      ivcomp = (ivon01 ** ivon02) + ivon03                              
      goto 47320                                                       
37320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47320, 7331, 47320                                    
47320 if (ivcomp - 247) 27320,17320,27320                               
17320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7331                                                        
27320 ivfail = ivfail + 1                                               
      ivcorr = 247                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7331 continue                                                          
      ivtnum = 733                                                      
!                                                                        
!       ****  TEST 733  ****                                             
!                                                                        
      if (iczero) 37330, 7330, 37330                                    
 7330 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 5                                                        
      ivon03 = 4                                                        
      ivcomp = ivon01 ** (ivon02 + ivon03)                              
      goto 47330                                                       
37330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47330, 7341, 47330                                    
47330 if (ivcomp - 19683) 27330,17330,27330                             
17330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7341                                                        
27330 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7341 continue                                                          
!                                                                        
!      TEST 734 THROUGH 736 TEST '**' FOLLOWED BY '-'.                   
!                                                                        
      ivtnum = 734                                                      
!                                                                        
!       ****  TEST 734  ****                                             
!                                                                        
      if (iczero) 37340, 7340, 37340                                    
 7340 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 7                                                        
      ivon03 = 4                                                        
      ivcomp = ivon01 ** ivon02 - ivon03                                
      goto 47340                                                       
37340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47340, 7351, 47340                                    
47340 if (ivcomp - 2183) 27340,17340,27340                              
17340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7351                                                        
27340 ivfail = ivfail + 1                                               
      ivcorr = 2183                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7351 continue                                                          
      ivtnum = 735                                                      
!                                                                        
!       ****  TEST 735  ****                                             
!                                                                        
      if (iczero) 37350, 7350, 37350                                    
 7350 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 7                                                        
      ivon03 = 4                                                        
      ivcomp = (ivon01 ** ivon02) - ivon03                              
      goto 47350                                                       
37350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47350, 7361, 47350                                    
47350 if (ivcomp - 2183) 27350,17350,27350                              
17350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7361                                                        
27350 ivfail = ivfail + 1                                               
      ivcorr = 2183                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7361 continue                                                          
      ivtnum = 736                                                      
!                                                                        
!       ****  TEST 736  ****                                             
!                                                                        
      if (iczero) 37360, 7360, 37360                                    
 7360 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 7                                                        
      ivon03 = 4                                                        
      ivcomp = ivon01 ** (ivon02 - ivon03)                              
      goto 47360                                                       
37360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47360, 7371, 47360                                    
47360 if (ivcomp - 27) 27360,17360,27360                                
17360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7371                                                        
27360 ivfail = ivfail + 1                                               
      ivcorr = 27                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7371 continue                                                          
!                                                                        
!      TEST 737 THROUGH 739 TEST '**' FOLLOWED BY '*'.                   
!                                                                        
      ivtnum = 737                                                      
!                                                                        
!       ****  TEST 737  ****                                             
!                                                                        
      if (iczero) 37370, 7370, 37370                                    
 7370 continue                                                          
      ivon01 =  3                                                       
      ivon02 =  3                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 ** ivon02 * ivon03                                
      goto 47370                                                       
37370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47370, 7381, 47370                                    
47370 if (ivcomp - 81) 27370,17370,27370                                
17370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7381                                                        
27370 ivfail = ivfail + 1                                               
      ivcorr = 81                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7381 continue                                                          
      ivtnum = 738                                                      
!                                                                        
!       ****  TEST 738  ****                                             
!                                                                        
      if (iczero) 37380, 7380, 37380                                    
 7380 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 3                                                        
      ivon03 = 3                                                        
      ivcomp = (ivon01 ** ivon02) * ivon03                              
      goto 47380                                                       
37380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47380, 7391, 47380                                    
47380 if (ivcomp - 81) 27380,17380,27380                                
17380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7391                                                        
27380 ivfail = ivfail + 1                                               
      ivcorr = 81                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7391 continue                                                          
      ivtnum = 739                                                      
!                                                                        
!       ****  TEST 739  ****                                             
!                                                                        
      if (iczero) 37390, 7390, 37390                                    
 7390 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 3                                                        
      ivon03 = 3                                                        
      ivcomp = ivon01 ** (ivon02 * ivon03)                              
      goto 47390                                                       
37390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47390, 7401, 47390                                    
47390 if (ivcomp - 19683) 27390,17390,27390                             
17390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7401                                                        
27390 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7401 continue                                                          
!                                                                        
!      TEST 740 THROUGH 742 TEST '**' FOLLOWED BY '/'.                   
!                                                                        
      ivtnum = 740                                                      
!                                                                        
!       ****  TEST 740  ****                                             
!                                                                        
      if (iczero) 37400, 7400, 37400                                    
 7400 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 9                                                        
      ivon03 = 3                                                        
      ivcomp = ivon01 ** ivon02 / ivon03                                
      goto 47400                                                       
37400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47400, 7411, 47400                                    
47400 if (ivcomp - 6561) 27400,17400,27400                              
17400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7411                                                        
27400 ivfail = ivfail + 1                                               
      ivcorr = 6561                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7411 continue                                                          
      ivtnum = 741                                                      
!                                                                        
!       ****  TEST 741  ****                                             
!                                                                        
      if (iczero) 37410, 7410, 37410                                    
 7410 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 9                                                        
      ivon03 = 3                                                        
      ivcomp = (ivon01 ** ivon02) / ivon03                              
      goto 47410                                                       
37410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47410, 7421, 47410                                    
47410 if (ivcomp - 6561) 27410,17410,27410                              
17410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7421                                                        
27410 ivfail = ivfail + 1                                               
      ivcorr = 6561                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7421 continue                                                          
      ivtnum = 742                                                      
!       ****  TEST 742  ****                                             
!                                                                        
      if (iczero) 37420, 7420, 37420                                    
 7420 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 9                                                        
      ivon03 = 3                                                        
      ivcomp = ivon01 ** (ivon02 / ivon03)                              
      goto 47420                                                       
37420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47420, 7431, 47420                                    
47420 if (ivcomp - 27) 27420,17420,27420                                
17420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7431                                                        
27420 ivfail = ivfail + 1                                               
      ivcorr = 27                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7431 continue                                                          
!                                                                        
!      TEST 743 THROUGH 746 TEST '**' FOLLOWED BY '**'.                  
!                                                                        
      ivtnum = 743                                                      
!                                                                        
!       ****  TEST 743  ****                                             
!                                                                        
      if (iczero) 37430, 7430, 37430                                    
 7430 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 3                                                        
      ivon03 = 2                                                        
      ivcomp = (ivon01 ** ivon02) ** ivon03                             
      goto 47430                                                       
37430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47430, 7441, 47430                                    
47430 if (ivcomp - 729) 27430,17430,27430                               
17430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7441                                                        
27430 ivfail = ivfail + 1                                               
      ivcorr = 729                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7441 continue                                                          
      ivtnum = 744                                                      
!                                                                        
!       ****  TEST 744  ****                                             
!                                                                        
      if (iczero) 37440, 7440, 37440                                    
 7440 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 3                                                        
      ivon03 = 2                                                        
      ivcomp = ivon01 ** (ivon02 ** ivon03)                             
      goto 47440                                                       
37440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47440, 7451, 47440                                    
47440 if (ivcomp - 19683) 27440,17440,27440                             
17440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7451                                                        
27440 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7451 continue                                                          
      ivtnum = 745                                                      
!                                                                        
!       ****  TEST 745  ****                                             
!                                                                        
      if (iczero) 37450, 7450, 37450                                    
 7450 continue                                                          
      ivon01 = -3                                                       
      ivon02 = 3                                                        
      ivon03 = 2                                                        
      ivcomp = (ivon01 ** ivon02) ** ivon03                             
      goto 47450                                                       
37450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47450, 7461, 47450                                    
47450 if (ivcomp - 729) 27450,17450,27450                               
17450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7461                                                        
27450 ivfail = ivfail + 1                                               
      ivcorr = 729                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7461 continue                                                          
      ivtnum = 746                                                      
!                                                                        
!       ****  TEST 746  ****                                             
!                                                                        
      if (iczero) 37460, 7460, 37460                                    
 7460 continue                                                          
      ivon01 = -3                                                       
      ivon02 =  3                                                       
      ivon03 =  2                                                       
      ivcomp = ivon01 ** (ivon02 ** ivon03)                             
      goto 47460                                                       
37460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47460, 7471, 47460                                    
47460 if (ivcomp + 19683) 27460,17460,27460                             
17460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7471                                                        
27460 ivfail = ivfail + 1                                               
      ivcorr = -19683                                                   
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7471 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM044" )                          
      end program fm044
