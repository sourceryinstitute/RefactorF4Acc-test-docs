      program fm030
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
!      COMMENT SECTION.                                                  
!                                                                        
!      FM030                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM                                                              
!                INTEGER VARIABLE = ARITHMETIC EXPRESSION                
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR -, INTEGER CONSTANTS AND INTEGER VARIABLES.              
!      SOME OF THE TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE        
!      ARITHMETIC EXPRESSION.                                            
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!             (1)  INTEGER CONSTANT - INTEGER CONSTANT                   
!             (2)  INTEGER CONSTANT - INTEGER CONSTANT - INTEGER CONSTANT
!             (3)  SAME AS (2) BUT WITH PARENTHESES TO GROUP ELEMENTS    
!             (4)  INTEGER VARIABLE - INTEGER CONSTANT                   
!                  INTEGER CONSTANT - INTEGER VARIABLE                   
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
!      TEST SECTION                                                      
!                                                                        
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!          TEST 265 THROUGH TEST 270 CONTAIN TWO INTEGER CONSTANTS AND   
!      OPERATOR - IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS       
!           INTEGER VARIABLE = INTEGER CONSTANT - INTEGER CONSTANT       
!                                                                        
 2651 continue                                                          
      ivtnum = 265                                                      
!                                                                        
!       ****  TEST 265  ****                                             
!                                                                        
      if (iczero) 32650, 2650, 32650                                    
 2650 continue                                                          
      ivcomp = 3-2                                                      
      goto 42650                                                       
32650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42650, 2661, 42650                                    
42650 if (ivcomp - 1) 22650,12650,22650                                 
12650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2661                                                        
22650 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2661 continue                                                          
      ivtnum = 266                                                      
!                                                                        
!       ****  TEST 266  ****                                             
!                                                                        
      if (iczero) 32660, 2660, 32660                                    
 2660 continue                                                          
      ivcomp = 51 - 52                                                  
      goto 42660                                                       
32660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42660, 2671, 42660                                    
42660 if (ivcomp +1) 22660,12660,22660                                  
12660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2671                                                        
22660 ivfail = ivfail + 1                                               
      ivcorr = -1                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2671 continue                                                          
      ivtnum = 267                                                      
!                                                                        
!       ****  TEST 267  ***                                              
!                                                                        
      if (iczero) 32670, 2670, 32670                                    
 2670 continue                                                          
      ivcomp = 865 - 189                                                
      goto 42670                                                       
32670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42670, 2681, 42670                                    
42670 if (ivcomp -676) 22670,12670,22670                                
12670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2681                                                        
22670 ivfail = ivfail + 1                                               
      ivcorr = 676                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2681 continue                                                          
      ivtnum = 268                                                      
!                                                                        
!       ****  TEST 268  ****                                             
!                                                                        
      if (iczero) 32680, 2680, 32680                                    
 2680 continue                                                          
      ivcomp =1358-9359                                                 
      goto 42680                                                       
32680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42680, 2691, 42680                                    
42680 if (ivcomp+8001) 22680,12680,22680                                
12680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2691                                                        
22680 ivfail = ivfail + 1                                               
      ivcorr = -8001                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2691 continue                                                          
      ivtnum = 269                                                      
!                                                                        
!       ****  TEST 269  ****                                             
!                                                                        
      if (iczero) 32690, 2690, 32690                                    
 2690 continue                                                          
      ivcomp =21113-10001                                               
      goto 42690                                                       
32690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42690, 2701, 42690                                    
42690 if (ivcomp-11112) 22690,12690,22690                               
12690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2701                                                        
22690 ivfail = ivfail + 1                                               
      ivcorr=11112                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2701 continue                                                          
      ivtnum = 270                                                      
!                                                                        
!       ****  TEST 270  ****                                             
!                                                                        
      if (iczero) 32700, 2700, 32700                                    
 2700 continue                                                          
      ivcomp = 32767-1                                                  
      goto 42700                                                       
32700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42700, 2711, 42700                                    
42700 if (ivcomp -32766) 22700,12700,22700                              
12700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2711                                                        
22700 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TEST 271 THROUGH TEST 274 CONTAIN THREE INTEGER CONSTANTS     
!      AND OPERATOR - IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS   
!                        IV = IC - IC - IC                               
!                                                                        
 2711 continue                                                          
      ivtnum = 271                                                      
!                                                                        
!       ****  TEST 271  ****                                             
!                                                                        
      if (iczero) 32710, 2710, 32710                                    
 2710 continue                                                          
      ivcomp=9-4-3                                                      
      goto 42710                                                       
32710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42710, 2721, 42710                                    
42710 if (ivcomp -2) 22710,12710,22710                                  
12710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2721                                                        
22710 ivfail = ivfail + 1                                               
      ivcorr =2                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2721 continue                                                          
      ivtnum = 272                                                      
!                                                                        
!       ****  TEST 272 ****                                              
!                                                                        
      if (iczero) 32720, 2720, 32720                                    
 2720 continue                                                          
      ivcomp = 51-52-53                                                 
      goto 42720                                                       
32720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42720, 2731, 42720                                    
42720 if (ivcomp +54) 22720,12720,22720                                 
12720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2731                                                        
22720 ivfail = ivfail + 1                                               
      ivcorr = -54                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2731 continue                                                          
      ivtnum = 273                                                      
!                                                                        
!       ****  TEST 273  ****                                             
!                                                                        
      if (iczero) 32730, 2730, 32730                                    
 2730 continue                                                          
      ivcomp = 966 -676 -189                                            
      goto 42730                                                       
32730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42730, 2741, 42730                                    
42730 if (ivcomp -101) 22730,12730,22730                                
12730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2741                                                        
22730 ivfail = ivfail + 1                                               
      ivcorr = 101                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2741 continue                                                          
      ivtnum = 274                                                      
!                                                                        
!       ****  TEST 274  ****                                             
!                                                                        
      if (iczero) 32740, 2740, 32740                                    
 2740 continue                                                          
      ivcomp = 1358-8001-2188                                           
      goto 42740                                                       
32740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42740, 2751, 42740                                    
42740 if (ivcomp + 8831) 22740,12740,22740                              
12740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2751                                                        
22740 ivfail = ivfail + 1                                               
      ivcorr = -8831                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 275 THROUGH TEST 282 ARE THE SAME AS TESTS 271-274 EXCEPT    
!      PARENTHESES ARE USED TO GROUP THE CONSTANTS.                      
!                                                                        
 2751 continue                                                          
      ivtnum = 275                                                      
!                                                                        
!       ****  TEST 275  ****                                             
!                                                                        
      if (iczero) 32750, 2750, 32750                                    
 2750 continue                                                          
      ivcomp =(9-4)-3                                                   
      goto 42750                                                       
32750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42750, 2761, 42750                                    
42750 if (ivcomp -2) 22750,12750,22750                                  
12750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2761                                                        
22750 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2761 continue                                                          
      ivtnum = 276                                                      
!                                                                        
!       ****  TEST 276  ****                                             
!                                                                        
      if (iczero) 32760, 2760, 32760                                    
 2760 continue                                                          
      ivcomp =9-(4-3)                                                   
      goto 42760                                                       
32760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42760, 2771, 42760                                    
42760 if (ivcomp -8) 22760,12760,22760                                  
12760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2771                                                        
22760 ivfail = ivfail + 1                                               
      ivcorr =8                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2771 continue                                                          
      ivtnum = 277                                                      
!                                                                        
!       ****  TEST 277  ****                                             
!                                                                        
      if (iczero) 32770, 2770, 32770                                    
 2770 continue                                                          
      ivcomp =(51-52)-53                                                
      goto 42770                                                       
32770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42770, 2781, 42770                                    
42770 if (ivcomp +54) 22770,12770,22770                                 
12770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2781                                                        
22770 ivfail = ivfail + 1                                               
      ivcorr = -54                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2781 continue                                                          
      ivtnum = 278                                                      
!                                                                        
!       ****  TEST 278  ****                                             
!                                                                        
      if (iczero) 32780, 2780, 32780                                    
 2780 continue                                                          
      ivcomp=51-(52-53)                                                 
      goto 42780                                                       
32780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42780, 2791, 42780                                    
42780 if (ivcomp-52) 22780,12780,22780                                  
12780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2791                                                        
22780 ivfail = ivfail + 1                                               
      ivcorr = 52                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2791 continue                                                          
      ivtnum = 279                                                      
!                                                                        
!       ****  TEST 279  ****                                             
!                                                                        
      if (iczero) 32790, 2790, 32790                                    
 2790 continue                                                          
      ivcomp =(966-676)-189                                             
      goto 42790                                                       
32790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42790, 2801, 42790                                    
42790 if (ivcomp - 101) 22790,12790,22790                               
12790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2801                                                        
22790 ivfail = ivfail + 1                                               
      ivcorr = 101                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2801 continue                                                          
      ivtnum = 280                                                      
!                                                                        
!       ****  TEST 280  ****                                             
!                                                                        
      if (iczero) 32800, 2800, 32800                                    
 2800 continue                                                          
      ivcomp =966-(676-189)                                             
      goto 42800                                                       
32800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42800, 2811, 42800                                    
42800 if (ivcomp - 479) 22800,12800,22800                               
12800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2811                                                        
22800 ivfail = ivfail + 1                                               
      ivcorr = 479                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2811 continue                                                          
      ivtnum = 281                                                      
!                                                                        
!       ****  TEST 281  ****                                             
!                                                                        
      if (iczero) 32810, 2810, 32810                                    
 2810 continue                                                          
      ivcomp = (1358-8001)-2188                                         
      goto 42810                                                       
32810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42810, 2821, 42810                                    
42810 if (ivcomp + 8831) 22810,12810,22810                              
12810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2821                                                        
22810 ivfail = ivfail + 1                                               
      ivcorr = -8831                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2821 continue                                                          
      ivtnum = 282                                                      
!                                                                        
!       ****  TEST 282  ****                                             
!                                                                        
      if (iczero) 32820, 2820, 32820                                    
 2820 continue                                                          
      ivcomp = 1358-(8001-2188)                                         
      goto 42820                                                       
32820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42820, 2831, 42820                                    
42820 if (ivcomp + 4455) 22820,12820,22820                              
12820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2831                                                        
22820 ivfail = ivfail + 1                                               
      ivcorr = -4455                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 283 THROUGH TEST 299 CONTAIN INTEGER VARIABLE, INTEGER       
!      CONSTANT AND OPERATOR - IN ARITHMETIC EXPRESSION. THE INTEGER     
!      VARIABLE CONTAINS BOTH POSITIVE AND NEGATIVE VALUES.              
!      THE FORMS TESTED ARE                                              
!              INTEGER VARIABLE = INTEGER VARIABLE - INTEGER CONSTANT    
!              INTEGER VARIABLE = INTEGER CONSTANT - INTEGER VARIABLE    
!                                                                        
 2831 continue                                                          
      ivtnum = 283                                                      
!                                                                        
!       ****  TEST 283  ****                                             
!                                                                        
      if (iczero) 32830, 2830, 32830                                    
 2830 continue                                                          
      ivon01 = 3                                                        
      ivcomp = ivon01 - 2                                               
      goto 42830                                                       
32830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42830, 2841, 42830                                    
42830 if (ivcomp - 1) 22830,12830,22830                                 
12830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2841                                                        
22830 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2841 continue                                                          
      ivtnum = 284                                                      
!                                                                        
!       ****  TEST 284  ****                                             
!                                                                        
      if (iczero) 32840, 2840, 32840                                    
 2840 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 -3                                                
      goto 42840                                                       
32840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42840, 2851, 42840                                    
42840 if (ivcomp +1) 22840,12840,22840                                  
12840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2851                                                        
22840 ivfail = ivfail + 1                                               
      ivcorr = -1                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2851 continue                                                          
      ivtnum = 285                                                      
!                                                                        
!       ****  TEST 285  ****                                             
!                                                                        
      if (iczero) 32850, 2850, 32850                                    
 2850 continue                                                          
      ivon01 =-3                                                        
      ivcomp = ivon01 -2                                                
      goto 42850                                                       
32850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42850, 2861, 42850                                    
42850 if (ivcomp +5) 22850,12850,22850                                  
12850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2861                                                        
22850 ivfail = ivfail + 1                                               
      ivcorr =-5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2861 continue                                                          
      ivtnum = 286                                                      
!                                                                        
!       ****  TEST 286  ****                                             
!                                                                        
      if (iczero) 32860, 2860, 32860                                    
 2860 continue                                                          
      ivon02 =2                                                         
      ivcomp = 3 - ivon02                                               
      goto 42860                                                       
32860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42860, 2871, 42860                                    
42860 if (ivcomp -1) 22860,12860,22860                                  
12860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2871                                                        
22860 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2871 continue                                                          
      ivtnum = 287                                                      
!                                                                        
!       ****  TEST 287  ****                                             
!                                                                        
      if (iczero) 32870, 2870, 32870                                    
 2870 continue                                                          
      ivon02 =3                                                         
      ivcomp = 2 -ivon02                                                
      goto 42870                                                       
32870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42870, 2881, 42870                                    
42870 if (ivcomp +1) 22870,12870,22870                                  
12870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2881                                                        
22870 ivfail = ivfail + 1                                               
      ivcorr =-1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2881 continue                                                          
      ivtnum = 288                                                      
!                                                                        
!       ****  TEST 288  ****                                             
!                                                                        
      if (iczero) 32880, 2880, 32880                                    
 2880 continue                                                          
      ivon02 = -2                                                       
      ivcomp = 3 - ivon02                                               
      goto 42880                                                       
32880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42880, 2891, 42880                                    
42880 if (ivcomp -5) 22880,12880,22880                                  
12880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2891                                                        
22880 ivfail = ivfail + 1                                               
      ivcorr =5                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2891 continue                                                          
      ivtnum = 289                                                      
!                                                                        
!       ****  TEST 289  ****                                             
!                                                                        
      if (iczero) 32890, 2890, 32890                                    
 2890 continue                                                          
      ivon01 =51                                                        
      ivcomp = ivon01 - 52                                              
      goto 42890                                                       
32890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42890, 2901, 42890                                    
42890 if (ivcomp + 1) 22890,12890,22890                                 
12890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2901                                                        
22890 ivfail = ivfail + 1                                               
      ivcorr = -1                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2901 continue                                                          
      ivtnum = 290                                                      
!                                                                        
!       ****  TEST 290  ****                                             
!                                                                        
      if (iczero) 32900, 2900, 32900                                    
 2900 continue                                                          
      ivon01 =51                                                        
      ivcomp = ivon01 -51                                               
      goto 42900                                                       
32900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42900, 2911, 42900                                    
42900 if (ivcomp) 22900,12900,22900                                     
12900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2911                                                        
22900 ivfail = ivfail + 1                                               
      ivcorr =0                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2911 continue                                                          
      ivtnum = 291                                                      
!                                                                        
!       ****  TEST 291  ****                                             
!                                                                        
      if (iczero) 32910, 2910, 32910                                    
 2910 continue                                                          
      ivon01 =53                                                        
      ivcomp =ivon01 -52                                                
      goto 42910                                                       
32910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42910, 2921, 42910                                    
42910 if (ivcomp -1) 22910,12910,22910                                  
12910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2921                                                        
22910 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2921 continue                                                          
      ivtnum = 292                                                      
!                                                                        
!       ****  TEST 292  ****                                             
!                                                                        
      if (iczero) 32920, 2920, 32920                                    
 2920 continue                                                          
      ivon02 = 676                                                      
      ivcomp = 189 - ivon02                                             
      goto 42920                                                       
32920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42920, 2931, 42920                                    
42920 if (ivcomp + 487) 22920,12920,22920                               
12920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2931                                                        
22920 ivfail = ivfail + 1                                               
      ivcorr = -487                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2931 continue                                                          
      ivtnum = 293                                                      
!                                                                        
!       ****  TEST 293  ****                                             
!                                                                        
      if (iczero) 32930, 2930, 32930                                    
 2930 continue                                                          
      ivon02 = -676                                                     
      ivcomp = 189 - ivon02                                             
      goto 42930                                                       
32930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42930, 2941, 42930                                    
42930 if (ivcomp - 865) 22930,12930,22930                               
12930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2941                                                        
22930 ivfail = ivfail + 1                                               
      ivcorr = 865                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2941 continue                                                          
      ivtnum = 294                                                      
!                                                                        
!       ****  TEST 294  ****                                             
!                                                                        
      if (iczero) 32940, 2940, 32940                                    
 2940 continue                                                          
      ivon01 = 1358                                                     
      ivcomp = ivon01 - 8001                                            
      goto 42940                                                       
32940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42940, 2951, 42940                                    
42940 if (ivcomp + 6643) 22940,12940,22940                              
12940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2951                                                        
22940 ivfail = ivfail + 1                                               
      ivcorr = -6643                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2951 continue                                                          
      ivtnum = 295                                                      
!                                                                        
!       ****  TEST 295  ****                                             
!                                                                        
      if (iczero) 32950, 2950, 32950                                    
 2950 continue                                                          
      ivon01 = -1358                                                    
      ivcomp = ivon01 - 8001                                            
      goto 42950                                                       
32950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42950, 2961, 42950                                    
42950 if (ivcomp + 9359) 22950,12950,22950                              
12950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2961                                                        
22950 ivfail = ivfail + 1                                               
      ivcorr = -9359                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2961 continue                                                          
      ivtnum = 296                                                      
!                                                                        
!       ****  TEST 296  ****                                             
!                                                                        
      if (iczero) 32960, 2960, 32960                                    
 2960 continue                                                          
      ivon01 = 15                                                       
      ivcomp = ivon01 - 32752                                           
      goto 42960                                                       
32960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42960, 2971, 42960                                    
42960 if (ivcomp + 32737) 22960,12960,22960                             
12960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2971                                                        
22960 ivfail = ivfail + 1                                               
      ivcorr = -32737                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2971 continue                                                          
      ivtnum = 297                                                      
!                                                                        
!       ****  TEST 297  ****                                             
!                                                                        
      if (iczero) 32970, 2970, 32970                                    
 2970 continue                                                          
      ivon01 =-32751                                                    
      ivcomp = ivon01 - 15                                              
      goto 42970                                                       
32970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42970, 2981, 42970                                    
42970 if (ivcomp + 32766) 22970,12970,22970                             
12970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2981                                                        
22970 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2981 continue                                                          
      ivtnum = 298                                                      
!                                                                        
!       ****  TEST 298  ****                                             
!                                                                        
      if (iczero) 32980, 2980, 32980                                    
 2980 continue                                                          
      ivon02 = -32752                                                   
      ivcomp = 15 - ivon02                                              
      goto 42980                                                       
32980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42980, 2991, 42980                                    
42980 if (ivcomp - 32767) 22980,12980,22980                             
12980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2991                                                        
22980 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2991 continue                                                          
      ivtnum = 299                                                      
!                                                                        
!       ****  TEST 299  ****                                             
!                                                                        
      if (iczero) 32990, 2990, 32990                                    
 2990 continue                                                          
      ivon02 = 15                                                       
      ivcomp = 32752 - ivon02                                           
      goto 42990                                                       
32990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42990, 3001, 42990                                    
42990 if (ivcomp - 32737) 22990,12990,22990                             
12990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 3001                                                        
22990 ivfail = ivfail + 1                                               
      ivcorr = 32737                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 3001  continue                                                         
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
90007 format (" ",20x,"END OF PROGRAM FM030" )                          
      end program fm030
