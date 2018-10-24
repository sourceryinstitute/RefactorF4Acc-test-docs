      program fm006
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
!         COMMENT SECTION                                                
!                                                                        
!      FM006                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF        
!      THE FORM                                                          
!                    INTEGER VARIABLE = INTEGER CONSTANT                 
!                    INTEGER VARIABLE = INTEGER VARIABLE                 
!          THE INTEGER CONSTANT MAY BE UNSIGNED, POSITIVE OR NEGATIVE.   
!                                                                        
!          AN INTEGER DATUM IS ALWAYS AN EXACT REPRESENTATION OF AN      
!      INTEGER VALUE.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES. 
!      IT MAY ONLY ASSUME INTEGRAL VALUES.                               
!                                                                        
!          AN INTEGER CONSTANT IS WRITTEN AS A NONEMPTY STRING OF DIGITS.
!      THE CONSTANT IS THE DIGIT STRING INTERPRETED AS A DECIMAL NUMBER. 
!                                                                        
!          THIS ROUTINE ALSO CONTAINS TESTS WHICH CHECK ON THE USE OF    
!      AT LEAST 16 BITS FOR REPRESENTING INTEGER DATA VALUES.  THE       
!      CONSTANT VALUES 32767 AND -32766 ARE USED IN THESE TESTS.         
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENTS                 
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
!             ARITHMETIC ASSIGNMENT STATEMENT                            
!                                                                        
!      TEST 50 THROUGH TEST 61 CONTAIN STATEMENT OF FORM                 
!               INTEGER VARIABLE = INTEGER CONSTANT                      
!                                                                        
!      TESTS 50 THROUGH 53 CONTAIN UNSIGNED INTEGER CONSTANT.            
!                                                                        
  501 continue                                                          
      ivtnum =  50                                                      
!                                                                        
!       ****  TEST 50  ****                                              
!                                                                        
      if (iczero) 30500,  500, 30500                                    
  500 continue                                                          
      ivcomp=3                                                          
      goto 40500                                                       
30500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40500,  511, 40500                                    
40500 if (ivcomp - 3) 20500, 10500, 20500                               
10500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  511                                                        
20500 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  511 continue                                                          
      ivtnum =  51                                                      
!                                                                        
!       ****  TEST 51  ****                                              
!                                                                        
      if (iczero) 30510,  510, 30510                                    
  510 continue                                                          
      ivcomp = 76                                                       
      goto 40510                                                       
30510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40510,  521, 40510                                    
40510 if (ivcomp - 76) 20510, 10510, 20510                              
10510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  521                                                        
20510 ivfail = ivfail + 1                                               
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  521 continue                                                          
      ivtnum =  52                                                      
!                                                                        
!       ****  TEST 52  ****                                              
!                                                                        
      if (iczero) 30520,  520, 30520                                    
  520 continue                                                          
      ivcomp = 587                                                      
      goto 40520                                                       
30520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40520,  531, 40520                                    
40520 if (ivcomp - 587) 20520, 10520, 20520                             
10520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  531                                                        
20520 ivfail = ivfail + 1                                               
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  531 continue                                                          
      ivtnum =  53                                                      
!                                                                        
!       ****  TEST 53  ****                                              
!                                                                        
      if (iczero) 30530,  530, 30530                                    
  530 continue                                                          
      ivcomp = 9999                                                     
      goto 40530                                                       
30530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40530,  541, 40530                                    
40530 if (ivcomp - 9999) 20530, 10530, 20530                            
10530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  541                                                        
20530 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TESTS 54 THROUGH 57 CONTAIN POSITIVE SIGNED INTEGERS          
!                                                                        
  541 continue                                                          
      ivtnum =  54                                                      
!                                                                        
!       ****  TEST 54  ****                                              
!                                                                        
      if (iczero) 30540,  540, 30540                                    
  540 continue                                                          
      ivcomp = +3                                                       
      goto 40540                                                       
30540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40540,  551, 40540                                    
40540 if (ivcomp - 3) 20540, 10540, 20540                               
10540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  551                                                        
20540 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  551 continue                                                          
      ivtnum =  55                                                      
!                                                                        
!       ****  TEST 55  ****                                              
!                                                                        
      if (iczero) 30550,  550, 30550                                    
  550 continue                                                          
      ivcomp = +76                                                      
      goto 40550                                                       
30550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40550,  561, 40550                                    
40550 if (ivcomp - 76) 20550, 10550, 20550                              
10550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  561                                                        
20550 ivfail = ivfail + 1                                               
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  561 continue                                                          
      ivtnum =  56                                                      
!                                                                        
!       ****  TEST 56  ****                                              
!                                                                        
      if (iczero) 30560,  560, 30560                                    
  560 continue                                                          
      ivcomp = +587                                                     
      goto 40560                                                       
30560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40560,  571, 40560                                    
40560 if (ivcomp - 587) 20560, 10560, 20560                             
10560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  571                                                        
20560 ivfail = ivfail + 1                                               
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  571 continue                                                          
      ivtnum =  57                                                      
!                                                                        
!       ****  TEST 57  ****                                              
!                                                                        
      if (iczero) 30570,  570, 30570                                    
  570 continue                                                          
      ivcomp = +9999                                                    
      goto 40570                                                       
30570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40570,  581, 40570                                    
40570 if (ivcomp - 9999) 20570, 10570, 20570                            
10570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  581                                                        
20570 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TESTS 58 THROUGH 61 CONTAIN SIGNED NEGATIVE INTEGERS          
!                                                                        
  581 continue                                                          
      ivtnum =  58                                                      
!                                                                        
!       ****  TEST 58  ****                                              
!                                                                        
      if (iczero) 30580,  580, 30580                                    
  580 continue                                                          
      ivcomp = -3                                                       
      goto 40580                                                       
30580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40580,  591, 40580                                    
40580 if (ivcomp + 3) 20580, 10580, 20580                               
10580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  591                                                        
20580 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  591 continue                                                          
      ivtnum =  59                                                      
!                                                                        
!       ****  TEST 59  ****                                              
!                                                                        
      if (iczero) 30590,  590, 30590                                    
  590 continue                                                          
      ivcomp = -76                                                      
      goto 40590                                                       
30590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40590,  601, 40590                                    
40590 if (ivcomp + 76) 20590, 10590, 20590                              
10590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  601                                                        
20590 ivfail = ivfail + 1                                               
      ivcorr = -76                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  601 continue                                                          
      ivtnum =  60                                                      
!                                                                        
!       ****  TEST 60  ****                                              
!                                                                        
      if (iczero) 30600,  600, 30600                                    
  600 continue                                                          
      ivcomp = -587                                                     
      goto 40600                                                       
30600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40600,  611, 40600                                    
40600 if (ivcomp + 587) 20600,10600,20600                               
10600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  611                                                        
20600 ivfail = ivfail + 1                                               
      ivcorr = -587                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  611 continue                                                          
      ivtnum =  61                                                      
!                                                                        
!       ****  TEST 61  ****                                              
!                                                                        
      if (iczero) 30610,  610, 30610                                    
  610 continue                                                          
      ivcomp = -9999                                                    
      goto 40610                                                       
30610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40610,  621, 40610                                    
40610 if (ivcomp + 9999) 20610, 10610, 20610                            
10610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  621                                                        
20610 ivfail = ivfail + 1                                               
      ivcorr = -9999                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 62 THROUGH TEST 73 CONTAIN STATEMENT OF FORM                 
!          INTEGER VARIABLE = INTEGER VARIABLE                           
!                                                                        
!      TESTS 62 THROUGH 65 CONTAIN UNSIGNED VALUES.                      
!                                                                        
  621 continue                                                          
      ivtnum =  62                                                      
!                                                                        
!       ****  TEST 62  ****                                              
!                                                                        
      if (iczero) 30620,  620, 30620                                    
  620 continue                                                          
      ivon01 = 3                                                        
      ivcomp = ivon01                                                   
      goto 40620                                                       
30620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40620,  631, 40620                                    
40620 if (ivcomp - 3) 20620, 10620, 20620                               
10620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  631                                                        
20620 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  631 continue                                                          
      ivtnum =  63                                                      
!                                                                        
!       ****  TEST 63  ****                                              
!                                                                        
      if (iczero) 30630,  630, 30630                                    
  630 continue                                                          
      ivon01 = 76                                                       
      ivcomp = ivon01                                                   
      goto 40630                                                       
30630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40630,  641, 40630                                    
40630 if (ivcomp - 76) 20630, 10630, 20630                              
10630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  641                                                        
20630 ivfail = ivfail + 1                                               
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  641 continue                                                          
      ivtnum =  64                                                      
!                                                                        
!       ****  TEST 64  ****                                              
!                                                                        
      if (iczero) 30640,  640, 30640                                    
  640 continue                                                          
      ivon01 = 587                                                      
      ivcomp = ivon01                                                   
      goto 40640                                                       
30640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40640,  651, 40640                                    
40640 if (ivcomp - 587) 20640, 10640, 20640                             
10640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  651                                                        
20640 ivfail = ivfail + 1                                               
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  651 continue                                                          
      ivtnum =  65                                                      
!                                                                        
!       ****  TEST 65  ****                                              
!                                                                        
      if (iczero) 30650,  650, 30650                                    
  650 continue                                                          
      ivon01 = 9999                                                     
      ivcomp = ivon01                                                   
      goto 40650                                                       
30650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40650,  661, 40650                                    
40650 if (ivcomp - 9999)  20650, 10650, 20650                           
10650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  661                                                        
20650 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TESTS 66 THROUGH 69 CONTAIN POSITIVE VALUES.                      
!                                                                        
  661 continue                                                          
      ivtnum =  66                                                      
!                                                                        
!       ****  TEST 66  ****                                              
!                                                                        
      if (iczero) 30660,  660, 30660                                    
  660 continue                                                          
      ivon01 = +3                                                       
      ivcomp = ivon01                                                   
      goto 40660                                                       
30660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40660,  671, 40660                                    
40660 if (ivcomp - 3) 20660,10660,20660                                 
10660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  671                                                        
20660 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  671 continue                                                          
      ivtnum =  67                                                      
!                                                                        
!       ****  TEST 67  ****                                              
!                                                                        
      if (iczero) 30670,  670, 30670                                    
  670 continue                                                          
      ivon01 = +76                                                      
      ivcomp = ivon01                                                   
      goto 40670                                                       
30670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40670,  681, 40670                                    
40670 if (ivcomp - 76) 20670, 10670, 20670                              
10670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  681                                                        
20670 ivfail = ivfail + 1                                               
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  681 continue                                                          
      ivtnum =  68                                                      
!                                                                        
!       ****  TEST 68  ****                                              
!                                                                        
      if (iczero) 30680,  680, 30680                                    
  680 continue                                                          
      ivon01 = +587                                                     
      ivcomp = ivon01                                                   
      goto 40680                                                       
30680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40680,  691, 40680                                    
40680 if (ivcomp - 587) 20680, 10680, 20680                             
10680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  691                                                        
20680 ivfail = ivfail + 1                                               
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  691 continue                                                          
      ivtnum =  69                                                      
!                                                                        
!       ****  TEST 69  ****                                              
!                                                                        
      if (iczero) 30690,  690, 30690                                    
  690 continue                                                          
      ivon01 = +9999                                                    
      ivcomp = ivon01                                                   
      goto 40690                                                       
30690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40690,  701, 40690                                    
40690 if (ivcomp - 9999) 20690, 10690, 20690                            
10690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  701                                                        
20690 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TESTS 70 THROUGH 73 CONTAIN NEGATIVE VALUES.                      
!                                                                        
  701 continue                                                          
      ivtnum =  70                                                      
!                                                                        
!       ****  TEST 70  ****                                              
!                                                                        
      if (iczero) 30700,  700, 30700                                    
  700 continue                                                          
      ivon01 = -3                                                       
      ivcomp = ivon01                                                   
      goto 40700                                                       
30700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40700,  711, 40700                                    
40700 if (ivcomp + 3) 20700, 10700, 20700                               
10700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  711                                                        
20700 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  711 continue                                                          
      ivtnum =  71                                                      
!                                                                        
!       ****  TEST 71  ****                                              
!                                                                        
      if (iczero) 30710,  710, 30710                                    
  710 continue                                                          
      ivon01 = -76                                                      
      ivcomp = ivon01                                                   
      goto 40710                                                       
30710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40710,  721, 40710                                    
40710 if (ivcomp + 76) 20710, 10710, 20710                              
10710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  721                                                        
20710 ivfail = ivfail + 1                                               
      ivcorr = -76                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  721 continue                                                          
      ivtnum =  72                                                      
!                                                                        
!       ****  TEST 72  ****                                              
!                                                                        
      if (iczero) 30720,  720, 30720                                    
  720 continue                                                          
      ivon01 = -587                                                     
      ivcomp = ivon01                                                   
      goto 40720                                                       
30720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40720,  731, 40720                                    
40720 if (ivcomp + 587) 20720, 10720, 20720                             
10720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  731                                                        
20720 ivfail = ivfail + 1                                               
      ivcorr = -587                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  731 continue                                                          
      ivtnum =  73                                                      
!                                                                        
!       ****  TEST 73  ****                                              
!                                                                        
      if (iczero) 30730,  730, 30730                                    
  730 continue                                                          
      ivon01 = -9999                                                    
      ivcomp = ivon01                                                   
      goto 40730                                                       
30730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40730,  741, 40730                                    
40730 if (ivcomp + 9999) 20730, 10730, 20730                            
10730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  741                                                        
20730 ivfail = ivfail + 1                                               
      ivcorr = -9999                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TESTS 74 THROUGH 79 CHECK THAT AT LEAST 16 BITS ARE USED IN THE   
!      INTERNAL REPRESENTATION OF AN INTEGER DATUM.  THIS INCLUDES ONE   
!      BIT FOR THE SIGN.  THE LARGEST INTEGER USED IS 32767 =2**15 - 1,  
!      AND THE SMALLEST INTEGER USED IS -32766.                          
!                                                                        
  741 continue                                                          
      ivtnum =  74                                                      
!                                                                        
!       ****  TEST 74  ****                                              
!              UNSIGNED CONSTANT 32767                                   
!                                                                        
      if (iczero) 30740,  740, 30740                                    
  740 continue                                                          
      ivcomp = 32767                                                    
      goto 40740                                                       
30740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40740,  751, 40740                                    
40740 if (ivcomp - 32767) 20740, 10740, 20740                           
10740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  751                                                        
20740 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  751 continue                                                          
      ivtnum =  75                                                      
!                                                                        
!       ****  TEST 75  ****                                              
!              SIGNED POSITIVE CONSTANT +32767                           
!                                                                        
      if (iczero) 30750,  750, 30750                                    
  750 continue                                                          
      ivcomp = +32767                                                   
      goto 40750                                                       
30750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40750,  761, 40750                                    
40750 if (ivcomp - 32767) 20750, 10750, 20750                           
10750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  761                                                        
20750 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  761 continue                                                          
      ivtnum =  76                                                      
!                                                                        
!       ****  TEST 76  ****                                              
!              SIGNED NEGATIVE CONSTANT -32766                           
!                                                                        
      if (iczero) 30760,  760, 30760                                    
  760 continue                                                          
      ivcomp = - 32766                                                  
      goto 40760                                                       
30760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40760,  771, 40760                                    
40760 if (ivcomp + 32766) 20760, 10760, 20760                           
10760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  771                                                        
20760 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  771 continue                                                          
      ivtnum =  77                                                      
!                                                                        
!       ****  TEST 77  ****                                              
!                                                                        
      if (iczero) 30770,  770, 30770                                    
  770 continue                                                          
      ivon01 = 32767                                                    
      ivcomp = ivon01                                                   
      goto 40770                                                       
30770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40770,  781, 40770                                    
40770 if (ivcomp - 32767) 20770, 10770, 20770                           
10770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  781                                                        
20770 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  781 continue                                                          
      ivtnum =  78                                                      
!                                                                        
!       ****  TEST 78  ****                                              
!                                                                        
      if (iczero) 30780,  780, 30780                                    
  780 continue                                                          
      ivon01 = +32767                                                   
      ivcomp = ivon01                                                   
      goto 40780                                                       
30780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40780,  791, 40780                                    
40780 if (ivcomp - 32767) 20780, 10780, 20780                           
10780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  791                                                        
20780 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  791 continue                                                          
      ivtnum =  79                                                      
!                                                                        
!       ****  TEST 79  ****                                              
!                                                                        
      if (iczero) 30790,  790, 30790                                    
  790 continue                                                          
      ivon01 = -32766                                                   
      ivcomp=ivon01                                                     
      goto 40790                                                       
30790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40790,  801, 40790                                    
40790 if (ivcomp + 32766) 20790, 10790, 20790                           
10790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  801                                                        
20790 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  801 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM006" )                          
      end program fm006
