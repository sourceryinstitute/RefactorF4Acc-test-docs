      program fm109
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
      integer :: ivon04
      integer :: ivon05
      real :: rvon01
!      COMMENT SECTION                                                   
!                                                                        
!      FM109                                                             
!                                                                        
!          THIS ROUTINE TESTS THE BASIC OPTIONS     REGARDING THE SIMPLE 
!      FORMATTED WRITE STATEMENT OF FORM                                 
!             WRITE (U,F)     OR                                         
!             WRITE (U,F) L                                              
!      WHERE      U IS A LOGICAL UNIT NUMBER                             
!                 F IS A FORMAT STATEMENT LABEL, AND                     
!                 L IS A LIST OF INTEGER VARIABLES.                      
!      THE FORMAT STATEMENT F CONTAINS NH HOLLERITH FIELD DESCRIPTORS,   
!      NX BLANK FIELD DESCRIPTORS AND IW NUMERIC FIELD DESCRIPTORS.      
!                                                                        
!          THIS ROUTINE TESTS WHETHER THE FIRST CHARACTER OF A FORMAT    
!      RECORD FOR PRINTER OUTPUT DETERMINES VERTICAL SPACING AS FOLLOWS  
!                  1    -  ADVANCE TO FIRST LINE OF NEXT PAGE            
!                BLANK  -  ONE LINE                                      
!                  0    -  ADVANCE TWO LINES BEFORE PRINTING             
!                  +    -  DO NOT ADVANCE BEFORE PRINTING  -  ADVANCE 0  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8, SPECIFICATION STATEMENTS                            
!         SECTION 9, DATA STATEMENT                                      
!         SECTION 11.10, DO STATEMENT                                    
!         SECTION 12, INPUT/OUTPUT STATEMENTS                            
!         SECTION 12.8.2, INPUT/OUTPUT LIST                              
!         SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      
!         SECTION 13, FORMAT STATEMENT                                   
!         SECTION 13.2.1, EDIT DESCRIPTORS                               
!                                                                        
!          ALL OF THE RESULTS OF THIS ROUTINE MUST BE VISUALLY CHECKED   
!      ON THE OUTPUT REPORT.  THE USUAL TEST CODE FOR PASS, FAIL, OR     
!      DELETE DOES NOT APPLY TO THIS ROUTINE.  IF ANY TEST IS TO BE      
!      DELETED, CHANGE THE OFFENDING WRITE OR FORMAT STATEMENT TO A      
!      COMMENT.  THE PERSON RESPONSIBLE FOR CHECKING THE OUTPUT MUST ALSO
!      CHECK THE COMPILER LISTING TO SEE IF ANY STATEMENTS HAVE BEEN     
!      CHANGED TO COMMENTS.                                              
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
      write (i02,90006)                                                 
      write (i02,90002)                                                 
!                                                                        
      ivtnum = 156                                                      
!       ****  TEST 156  ****                                             
!      TEST 156    - VERTICAL SPACING TEST                               
!              1 IN FIRST CHARACTER OF FORMATTED PRINT RECORD MEANS      
!              RECORD IS FIRST LINE AT TOP OF NEXT PAGE.                 
!                                                                        
      if (iczero) 31560, 1560, 31560                                    
 1560 continue                                                          
      write (i02,80001) ivtnum                                          
      write (i02,80331)                                                 
80331 format (5x,"LAST LINE ON THIS PAGE" )                             
      write (i02,80330)                                                 
80330 format ("1","     THIS IS FIRST LINE ON PAGE" )                   
      goto 1571                                                        
31560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1571 continue                                                          
      ivtnum = 157                                                      
!                                                                        
!       ****  TEST 157  ****                                             
!      TEST  157  -  VERTICAL SPACING TEST                               
!          PRINT BLANK LINES                                             
!                                                                        
!                                                                        
      if (iczero) 31570, 1570, 31570                                    
 1570 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80340)                                                 
80340 format (" ", 10x)                                                 
      write (i02,80341)                                                 
80341 format (" THERE IS ONE BLANK LINE BEFORE THIS LINE" )             
      write (i02,80342)                                                 
      write (i02,80342)                                                 
80342 format ("           " )                                           
      write (i02,80343)                                                 
80343 format (" THERE ARE TWO BLANK LINES BEFORE THIS LINE" )           
      write (i02,80344)                                                 
      write (i02,80344)                                                 
      write (i02,80344)                                                 
80344 format (11x)                                                      
      write (i02,80345)                                                 
80345 format (" THERE ARE THREE BLANK LINES BEFORE THIS LINE" )         
      goto 1581                                                        
31570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1581 continue                                                          
      ivtnum = 158                                                      
!                                                                        
!       ****  TEST 158  ****                                             
!      TEST  158  -  PRINT 54 CHARACTERS                                 
!                                                                        
!                                                                        
      if (iczero) 31580, 1580, 31580                                    
 1580 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001)ivtnum                                           
      write (i02,80351)                                                 
80351 format (" NEXT LINE CONTAINS 54 CHARACTERS" )                     
      write (i02,80350)                                                 
80350 format(" 123456789012345678901234567890123456789012345678901234" )
      goto 1591                                                        
31580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1591 continue                                                          
      ivtnum = 159                                                      
!                                                                        
!       ****  TEST 159  ****                                             
!      TEST  159  -  NUMERIC FIELD DESCRIPTOR I1                         
!                                                                        
      if (iczero) 31590, 1590, 31590                                    
 1590 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80361)                                                 
80361 format (" ",10x,"THIS TEST PRINTS 3 UNDER I1 DESCRIPTOR" )        
      ivon01 = 3                                                        
      write (i02,80360) ivon01                                          
80360 format (" ",10x,i1)                                               
      goto 1601                                                        
31590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1601 continue                                                          
      ivtnum = 160                                                      
!                                                                        
!       ****  TEST 160  ****                                             
!      TEST  160  -  NUMERIC FIELD DESCRIPTOR I2                         
!                                                                        
      if (iczero) 31600, 1600, 31600                                    
 1600 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80371)                                                 
80371 format (11x,"THIS TEST PRINTS 15 UNDER I2 DESCRIPTOR" )           
      ivon01 = 15                                                       
      write (i02,80370) ivon01                                          
80370 format (" ",10x,i2)                                               
      goto 1611                                                        
31600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1611 continue                                                          
      ivtnum = 161                                                      
!                                                                        
!       ****  TEST 161  ****                                             
!      TEST  161  -  NUMERIC FIELD DESCRIPTOR I3                         
!                                                                        
      if (iczero) 31610, 1610, 31610                                    
 1610 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80381)                                                 
80381 format (11x,"THIS TEST PRINTS 291 UNDER I3 DESCRIPTOR" )          
      ivon01 = 291                                                      
      write (i02,80380) ivon01                                          
80380 format (11x,i3)                                                   
      goto 1621                                                        
31610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1621 continue                                                          
      ivtnum = 162                                                      
!                                                                        
!       ****  TEST 162  ****                                             
!      TEST  162  -  NUMERIC FIELD DESCRIPTOR I4                         
!                                                                        
      if (iczero) 31620, 1620, 31620                                    
 1620 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80391)                                                 
80391 format (11x,"THIS TEST PRINTS 4321 UNDER I4 DESCRIPTOR" )         
      ivon01 = 4321                                                     
      write (i02,80390) ivon01                                          
80390 format (11x,i4)                                                   
      goto 1631                                                        
31620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1631 continue                                                          
      ivtnum = 163                                                      
!                                                                        
!       ****  TEST 163  ****                                             
!      TEST  163  -  NUMERIC FIELD DESCRIPTOR I5                         
!                                                                        
      if (iczero) 31630, 1630, 31630                                    
 1630 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80401)                                                 
80401 format (" ",10x,"THIS TEST PRINTS 12345 UNDER I5 DESCRIPTOR" )    
      ivon01 = 12345                                                    
      write (i02,80400) ivon01                                          
80400 format (" ",10x,i5)                                               
      goto 1641                                                        
31630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1641 continue                                                          
      ivtnum = 164                                                      
!                                                                        
!       ****  TEST 164  ****                                             
!      TEST  164  -  NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       
!                                                                        
      if (iczero) 31640, 1640, 31640                                    
 1640 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 22                                                       
      ivon03 = 333                                                      
      ivon04 = 4444                                                     
      ivon05 = 25555                                                    
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80411)                                                 
80411 format (3x,"THIS TEST PRINTS 1, 22, 333, 4444, AND 25555 UNDER" ) 
      write (i02,80412)                                                 
80412 format (10x,"(10X,I1,3X,I2,3X,I3,3X,I4,3X,I5)" )                  
      write (i02,80410) ivon01, ivon02, ivon03, ivon04, ivon05          
80410 format (10x,i1,3x,i2,3x,i3,3x,i4,3x,i5)                           
      goto 1651                                                        
31640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1651 continue                                                          
      ivtnum = 165                                                      
!                                                                        
!       ****  TEST 165  ****                                             
!      TEST  165   - HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS          
!             COMBINE HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS IN      
!             ONE FORMAT STATEMENT                                       
!                                                                        
      if (iczero) 31650, 1650, 31650                                    
 1650 continue                                                          
      ivon01=113                                                        
      ivon02=8                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80421)                                                 
80421 format (10x,"NEXT TWO LINES ARE IDENTICAL" )                      
      write (i02,80422)                                                 
80422 format ("      IVON01 =  113   IVON02 =    8" )                   
      write (i02,80420) ivon01, ivon02                                  
80420 format (6x,"IVON01 =",i5,3x,"IVON02 =",i5)                        
      goto 1661                                                        
31650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1661 continue                                                          
      ivtnum = 166                                                      
!                                                                        
!       ****  TEST 166  ****                                             
!      TEST  166   - NUMERIC FIELD DESCRIPTOR I2                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      if (iczero) 31660, 1660, 31660                                    
 1660 continue                                                          
      ivon01 = -1                                                       
      write (i02,90002)                                                 
      write (i02,80001)  ivtnum                                         
      write (i02,80431)                                                 
80431 format (11x,"THIS TEST PRINTS -1 UNDER I2 DESCRIPTOR" )           
      write (i02,80430) ivon01                                          
80430 format (11x,i2)                                                   
      goto 1671                                                        
31660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1671 continue                                                          
      ivtnum = 167                                                      
!                                                                        
!       ****  TEST 167  ****                                             
!      TEST  167 -   NUMERIC FIELD DESCRIPTOR I3                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      if (iczero) 31670, 1670, 31670                                    
 1670 continue                                                          
      ivon01 = -22                                                      
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80441)                                                 
80441 format (11x,"THIS TEST PRINTS -22 UNDER I3 DESCRIPTOR" )          
      write (i02,80440) ivon01                                          
80440 format (11x,i3)                                                   
      goto 1681                                                        
31670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1681 continue                                                          
      ivtnum = 168                                                      
!                                                                        
!       ****  TEST 168  ****                                             
!      TEST  168 -   NUMERIC FIELD DESCRIPTOR I4                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      if (iczero) 31680, 1680, 31680                                    
 1680 continue                                                          
      ivon01 = -333                                                     
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80451)                                                 
80451 format (11x,"THIS TEST PRINTS -333 UNDER I4 DESCRIPTOR" )         
      write (i02,80450) ivon01                                          
80450 format (11x,i4)                                                   
      goto 1691                                                        
31680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1691 continue                                                          
      ivtnum = 169                                                      
!                                                                        
!       ****  TEST 169  ****                                             
!      TEST  169 -   NUMERIC FIELD DESCRIPTOR I5                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      if (iczero) 31690, 1690, 31690                                    
 1690 continue                                                          
      ivon01 = -4444                                                    
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80461)                                                 
80461 format (11x,"THIS TEST PRINTS -4444 UNDER I5 DESCRIPTOR" )        
      write (i02,80460) ivon01                                          
80460 format (11x,i5)                                                   
      goto 1701                                                        
31690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1701 continue                                                          
      ivtnum = 170                                                      
!                                                                        
!       ****  TEST 170  ****                                             
!      TEST  170 -   NUMERIC FIELD DESCRIPTOR I6                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      if (iczero) 31700, 1700, 31700                                    
 1700 continue                                                          
      ivon01 = -15555                                                   
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80471)                                                 
80471 format (11x,"THIS TEST PRINTS -15555 UNDER DESCRIPTOR I6" )       
      write (i02,80470) ivon01                                          
80470 format (11x,i6)                                                   
      goto 1711                                                        
31700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1711 continue                                                          
      ivtnum = 171                                                      
!                                                                        
!       ****  TEST 171  ****                                             
!      TEST  171 -   NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       
!            PRINT NEGATIVE INTEGERS                                     
!                                                                        
      if (iczero) 31710, 1710, 31710                                    
 1710 continue                                                          
      ivon01 = -9                                                       
      ivon02 = -88                                                      
      ivon03 = -777                                                     
      ivon04 = -6666                                                    
      ivon05 = -25555                                                   
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80481)                                                 
80481 format (8x,"THIS TEST PRINTS -9, -88, -777, -6666, AND -25555" )  
      write (i02,80482)                                                 
80482 format (11x,"UNDER FORMAT 10X,I2,3X,I3,3X,I4,3X,I5,3X,I6" )       
      write (i02,80480) ivon01,ivon02,ivon03,ivon04,ivon05              
80480 format (10x,i2,3x,i3,3x,i4,3x,i5,3x,i6)                           
      goto 1721                                                        
31710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1721 continue                                                          
      ivtnum = 172                                                      
!                                                                        
!       ****  TEST 172  ****                                             
!      TEST  172 -   NUMERIC FIELD DESCRIPTOR I5                         
!             MIX POSITIVE AND NEGATIVE INTEGER OUTPUT IN ONE FORMAT     
!          STATEMENT ALL UNDER I5 DESCRIPTOR                             
!                                                                        
      if (iczero) 31720, 1720, 31720                                    
 1720 continue                                                          
      ivon01 =5                                                         
      ivon02 = -54                                                      
      ivon03 = 543                                                      
      ivon04 = -5432                                                    
      ivon05=32000                                                      
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80491)                                                 
80491 format (18x,"THIS TEST PRINTS 5, -54, 543, -5432, AND 32000" )    
      write (i02,80492)                                                 
80492 format (11x,"UNDER I5 NUMERIC FIELD DESCRIPTOR" )                 
      write (i02,80490) ivon01,ivon02,ivon03,ivon04,ivon05              
80490 format (11x,i5,3x,i5,3x,i5,3x,i5,3x,i5)                           
      goto 1731                                                        
31720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1731 continue                                                          
      ivtnum = 173                                                      
!                                                                        
!       ****  TEST 173  ****                                             
!      TEST 173  -  VERTICAL SPACING TEST USING THE 1H0 AS A DOUBLE      
!      SPACE BEFORE PRINT ( ADVANCE TWO LINES BEFORE WRITING ).  THE 0   
!      AS A CARRIAGE CONTROL CHARACTER IS USED WITH THE BLANK CHARACTER  
!      TO GET AN ODD NUMBER OF LINES TO ADVANCE BEFORE WRITING.          
!                                                                        
      if (iczero) 31730, 1730, 31730                                    
 1730 continue                                                          
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,81730)                                                 
81730 format (" ", 10x)                                                 
      write (i02,81731)                                                 
81731 format (" THERE IS ONE BLANK LINE BEFORE THIS LINE" )             
      write ( i02, 81732 )                                              
81732 format ( "0",10x)                                                 
      write ( i02, 81733 )                                              
81733 format (" THERE ARE TWO BLANK LINES BEFORE THIS LINE" )           
      write ( i02, 81730 )                                              
      write ( i02, 81732 )                                              
      write ( i02, 81735 )                                              
81735 format (" THERE ARE THREE BLANK LINES BEFORE THIS LINE" )         
      write ( i02, 81732 )                                              
      write ( i02, 81732 )                                              
      write ( i02, 81736 )                                              
81736 format (" THERE ARE FOUR  BLANK LINES BEFORE THIS LINE" )         
      goto 1741                                                        
31730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1741 continue                                                          
      ivtnum = 174                                                      
!                                                                        
!       ****  TEST 174  ****                                             
!      TEST 174  -  VERTICAL SPACING TEST USING THE + CHARACTER TO       
!      SUPPRESS ADVANCING BEFORE THE PRINT AND THIS SHOULD CAUSE TWO AND 
!      THEN THREE SUCCESSIVE LINES TO OVERPRINT                          
!                                                                        
      if (iczero) 31740, 1740, 31740                                    
 1740 continue                                                          
      write ( i02, 90002 )                                              
      write ( i02, 80001 ) ivtnum                                       
      write ( i02, 81740 )                                              
81740 format ( " " )                                                    
      write ( i02, 81741 )                                              
81741 format ( " ",10x, "1ST LINE - AABBCCDD" )                         
      write ( i02, 81742 )                                              
81742 format ( "+", 25x, "WWXXYYZZ OVERPRINTS - 2ND LINE" )             
      write ( i02, 81743 )                                              
81743 format ( /////" ")                                                
!      SKIP DOWN A FEW LINES TO GET SET  -  OK AWAY WE GO..              
      write ( i02, 81740 )                                              
      write ( i02, 81744 )                                              
81744 format ( " ", 10x, "11    44     1ST         LINE" )              
      write ( i02, 81745 )                                              
81745 format ( "+", 10x, "  22    55       2ND" )                       
      write ( i02, 81746 )                                              
81746 format ( "+", 10x, "    33    66         3RD" )                   
      goto 1751                                                        
31740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1751 continue                                                          
      ivtnum = 175                                                      
!                                                                        
!       ****  TEST 175  ****                                             
!      TEST 175  -  NUMERIC FIELD DESCRIPTOR F3.0                        
!                                                                        
      if (iczero) 31750, 1750, 31750                                    
 1750 continue                                                          
      write ( i02, 90002 )                                              
      write ( i02, 80001 ) ivtnum                                       
      write ( i02, 81751 )                                              
81751 format (" ",10x,"THIS TESTS PRINTS 3. UNDER F3.0 DESCRIPTOR" )    
      rvon01 = 3.                                                       
      write ( i02, 81752 )  rvon01                                      
81752 format ( " ",10x, f3.0 )                                          
      goto 1761                                                        
31750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1761 continue                                                          
      ivtnum = 176                                                      
!                                                                        
!       ****  TEST 176  ****                                             
!      TEST 176  -  SIGNED NUMERIC FIELD DESCRIPTOR F4.0                 
!                                                                        
      if (iczero) 31760, 1760, 31760                                    
 1760 continue                                                          
      write ( i02, 90002 )                                              
      write ( i02, 80001 ) ivtnum                                       
      write ( i02, 81761 )                                              
81761 format ( " ",10x,"THIS TEST  PRINTS -15. WITH F4.0 DESCRIPTOR" )  
      rvon01 = -15.                                                     
      write ( i02, 81762 )  rvon01                                      
81762 format ( " ",10x, f4.0)                                           
      goto 1771                                                        
31760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1771 continue                                                          
      ivtnum = 177                                                      
!                                                                        
!       ****  TEST 177  ****                                             
!      TEST 177  -  SIGNED NUMERIC FIELD DESCRIPTOR E12.5                
!                                                                        
      if (iczero) 31770, 1770, 31770                                    
 1770 continue                                                          
      write ( i02, 90002 )                                              
      write ( i02, 80001 )  ivtnum                                      
      write ( i02, 81771 )                                              
81771 format ( " ", 10x,"THIS TEST PRINTS -0.12345E+03 USING E12.5" )   
      rvon01 = -123.45                                                  
      write ( i02, 81772 )  rvon01                                      
81772 format ( " ", 10x, e12.5 )                                        
      goto 1781                                                        
31770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1781 continue                                                          
!                                                                        
!      WRITE PAGE FOOTINGS                                               
99999 continue                                                          
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
      write (i02,90007)                                                 
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
!      FORMAT STATEMENTS FOR THIS ROUTINE                                
80001 format (10x,"TEST ",i5)                                           
80003 format ( " ",4x,i5,7x,"DELETED")                                  
90007 format (" ",20x,"END OF PROGRAM FM109" )                          
      end program fm109
