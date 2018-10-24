      program fm005
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
!      COMMENT SECTION                                                   
!                                                                        
!      FM005                                                             
!                                                                        
!          THIS ROUTINE TESTS THE BASIC ASSUMPTIONS REGARDING THE SIMPLE 
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
!                BLANK  -  ONE LINE                                      
!                  1    -  ADVANCE TO FIRST LINE OF NEXT PAGE            
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 12.8.2, INPUT/OUTPUT LISTS                             
!         SECTION 12.9.5.2, READ, WRITE, AND PRINT STATEMENT             
!         SECTION 12.9.5.2.3, PRINTING OF FORMATTED RECORDS              
!         SECTION 13.5.2, H EDITING                                      
!         SECTION 13.5.3.2, X EDITING                                    
!         SECTION 13.5.9.1, NUMERIC EDITING                              
!                                                                        
!          ALL OF THE RESULTS OF THIS ROUTINE MUST BE VISUALLY CHECKED   
!      ON THE OUTPUT REPORT.  THE USUAL TEST CODE FOR PASS, FAIL, OR     
!      DELETE DOES NOT APPLY TO THIS ROUTINE.  IF ANY TEST IS TO BE      
!      DELETED, CHANGE THE OFFENDING WRITE OR FORMAT STATEMENT TO A      
!      COMMENT.  THE PERSON RESPONSIBLE FOR CHECKING THE OUTPUT MUST ALSO
!      CHECK THE COMPILER LISTING TO SEE IF ANY STATEMENTS HAVE BEEN     
!      CHANGED TO COMMENTS.                                              
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
  331 continue                                                          
      ivtnum = 33                                                       
!                                                                        
!       ****  TEST 033  ****                                             
!          TEST 33 - VERTICAL SPACING TEST                               
!              1 IN FIRST CHARACTER OF FORMATTED PRINT RECORD MEANS      
!              RECORD IS FIRST LINE AT TOP OF NEXT PAGE.                 
!                                                                        
      write (i02,80001) ivtnum                                          
      write (i02,80331)                                                 
80331 format (5x,"LAST LINE ON THIS PAGE" )                             
      write (i02,80330)                                                 
80330 format ("1","     THIS IS FIRST LINE ON PAGE" )                   
  341 continue                                                          
      ivtnum = 34                                                       
!                                                                        
!       ****  TEST 034  ****                                             
!          TEST 34 - VERTICAL SPACING TEST                               
!          PRINT BLANK LINES                                             
!                                                                        
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
  351 continue                                                          
      ivtnum = 35                                                       
!                                                                        
!       ****  TEST 035  ****                                             
!          TEST 35 - PRINT 54 CHARACTERS                                 
!                                                                        
      write (i02,90002)                                                 
      write (i02,80001)ivtnum                                           
      write (i02,80351)                                                 
80351 format (" NEXT LINE CONTAINS 54 CHARACTERS" )                     
      write (i02,80350)                                                 
80350 format(" 123456789012345678901234567890123456789012345678901234" )
  361 continue                                                          
      ivtnum = 36                                                       
!                                                                        
!       ****  TEST 036  ****                                             
!          TEST 36 - NUMERIC FIELD DESCRIPTOR I1                         
!                                                                        
      write (i02,90000)                                                 
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80361)                                                 
80361 format (" ",10x,"THIS TEST PRINTS 3 UNDER I1 DESCRIPTOR" )        
      ivon01 = 3                                                        
      write (i02,80360) ivon01                                          
80360 format (" ",10x,i1)                                               
  371 continue                                                          
      ivtnum = 37                                                       
!                                                                        
!       ****  TEST 037  ****                                             
!          TEST 37 - NUMERIC FIELD DESCRIPTOR I2                         
!                                                                        
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80371)                                                 
80371 format (11x,"THIS TEST PRINTS 15 UNDER I2 DESCRIPTOR" )           
      ivon01 = 15                                                       
      write (i02,80370) ivon01                                          
80370 format (" ",10x,i2)                                               
  381 continue                                                          
      ivtnum = 38                                                       
!                                                                        
!       ****  TEST 038  ****                                             
!          TEST 38 - NUMERIC FIELD DESCRIPTOR I3                         
!                                                                        
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80381)                                                 
80381 format (11x,"THIS TEST PRINTS 291 UNDER I3 DESCRIPTOR" )          
      ivon01 = 291                                                      
      write (i02,80380) ivon01                                          
80380 format (11x,i3)                                                   
  391 continue                                                          
      ivtnum = 39                                                       
!                                                                        
!       ****  TEST 039  ****                                             
!          TEST 39 - NUMERIC FIELD DESCRIPTOR I4                         
!                                                                        
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80391)                                                 
80391 format (11x,"THIS TEST PRINTS 4321 UNDER I4 DESCRIPTOR" )         
      ivon01 = 4321                                                     
      write (i02,80390) ivon01                                          
80390 format (11x,i4)                                                   
  401 continue                                                          
      ivtnum = 40                                                       
!                                                                        
!       ****  TEST 040  ****                                             
!          TEST 40 - NUMERIC FIELD DESCRIPTOR I5                         
!                                                                        
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80401)                                                 
80401 format (" ",10x,"THIS TEST PRINTS 12345 UNDER I5 DESCRIPTOR" )    
      ivon01 = 12345                                                    
      write (i02,80400) ivon01                                          
80400 format (" ",10x,i5)                                               
  411 continue                                                          
      ivtnum = 41                                                       
!                                                                        
!       ****  TEST 041  ****                                             
!          TEST 41 - NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       
!                                                                        
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
  421 continue                                                          
      ivtnum = 42                                                       
!                                                                        
!       ****  TEST 042  ****                                             
!          TEST 42 - HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS          
!             COMBINE HOLLERITH, NUMERIC AND X FIELD DESCRIPTORS IN      
!             ONE FORMAT STATEMENT                                       
!                                                                        
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
  431 continue                                                          
      ivtnum=43                                                         
!                                                                        
!       ****  TEST 043  ****                                             
!          TEST 43 - NUMERIC FIELD DESCRIPTOR I2                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      ivon01 = -1                                                       
      write (i02,90000)                                                 
      write (i02,90002)                                                 
      write (i02,80001)  ivtnum                                         
      write (i02,80431)                                                 
80431 format (11x,"THIS TEST PRINTS -1 UNDER I2 DESCRIPTOR" )           
      write (i02,80430) ivon01                                          
80430 format (11x,i2)                                                   
  441 continue                                                          
      ivtnum = 44                                                       
!                                                                        
!       ****  TEST 044  ****                                             
!          TEST 44 - NUMERIC FIELD DESCRIPTOR I3                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      ivon01 = -22                                                      
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80441)                                                 
80441 format (11x,"THIS TEST PRINTS -22 UNDER I3 DESCRIPTOR" )          
      write (i02,80440) ivon01                                          
80440 format (11x,i3)                                                   
  451 continue                                                          
      ivtnum = 45                                                       
!                                                                        
!       ****  TEST 045  ****                                             
!          TEST 45 - NUMERIC FIELD DESCRIPTOR I4                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      ivon01 = -333                                                     
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80451)                                                 
80451 format (11x,"THIS TEST PRINTS -333 UNDER I4 DESCRIPTOR" )         
      write (i02,80450) ivon01                                          
80450 format (11x,i4)                                                   
  461 continue                                                          
      ivtnum = 46                                                       
!                                                                        
!       ****  TEST 046  ****                                             
!          TEST 46 - NUMERIC FIELD DESCRIPTOR I5                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      ivon01 = -4444                                                    
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80461)                                                 
80461 format (11x,"THIS TEST PRINTS -4444 UNDER I5 DESCRIPTOR" )        
      write (i02,80460) ivon01                                          
80460 format (11x,i5)                                                   
  471 continue                                                          
      ivtnum = 47                                                       
!                                                                        
!       ****  TEST 047  ****                                             
!          TEST 47 - NUMERIC FIELD DESCRIPTOR I6                         
!            PRINT NEGATIVE INTEGER                                      
!                                                                        
      ivon01 = -15555                                                   
      write (i02,90002)                                                 
      write (i02,80001) ivtnum                                          
      write (i02,80471)                                                 
80471 format (11x,"THIS TEST PRINTS -15555 UNDER DESCRIPTOR I6" )       
      write (i02,80470) ivon01                                          
80470 format (11x,i6)                                                   
  481 continue                                                          
      ivtnum = 48                                                       
!                                                                        
!       ****  TEST 048  ****                                             
!          TEST 48 - NUMERIC FIELD DESCRIPTORS, INTEGER CONVERSION       
!            PRINT NEGATIVE INTEGERS                                     
!                                                                        
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
  491 continue                                                          
      ivtnum = 49                                                       
!                                                                        
!       ****  TEST 049  ****                                             
!          TEST 49 - NUMERIC FIELD DESCRIPTOR I5                         
!             MIX POSITIVE AND NEGATIVE INTEGER OUTPUT IN ONE FORMAT     
!          STATEMENT ALL UNDER I5 DESCRIPTOR                             
!                                                                        
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
80001 format (10x,"TEST ",i2)                                           
90007 format (" ",20x,"END OF PROGRAM FM005" )                          
      end program fm005
