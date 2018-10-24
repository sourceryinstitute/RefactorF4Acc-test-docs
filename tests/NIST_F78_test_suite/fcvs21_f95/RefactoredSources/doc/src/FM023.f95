      program fm023
!      COMMENT SECTION.                                                  
!                                                                        
!      FM023                                                             
!                                                                        
!                   TWO DIMENSIONED ARRAYS ARE USED IN THIS ROUTINE.     
!          THIS ROUTINE TESTS ARRAYS WITH FIXED DIMENSION AND SIZE LIMITS
!      SET EITHER IN A BLANK COMMON OR DIMENSION STATEMENT.  THE VALUES  
!      OF THE ARRAY ELEMENTS ARE SET IN VARIOUS WAYS SUCH AS SIMPLE      
!      ASSIGNMENT STATEMENTS, SET TO THE VALUES OF OTHER ARRAY ELEMENTS  
!      (EITHER POSITIVE OR NEGATIVE), SET BY INTEGER TO REAL OR REAL TO  
!      INTEGER CONVERSION, SET BY ARITHMETIC EXPRESSIONS, OR SET BY      
!      USE OF THE  EQUIVALENCE  STATEMENT.                               
!                                                                        
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8, SPECIFICATION STATEMENTS                            
!         SECTION 8.1, DIMENSION STATEMENT                               
!         SECTION 8.2, EQUIVALENCE STATEMENT                             
!         SECTION 8.3, COMMON STATEMENT                                  
!         SECTION 8.4, TYPE-STATEMENTS                                   
!         SECTION 9, DATA STATEMENT                                      
!                                                                        
      integer, dimension(1:2,1:2) :: iadn22
      integer :: icoe01
      real, dimension(1:2,1:2) :: radn22
      real :: rcoe01
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: icon01
      integer :: icoe03
      integer :: icoe04
      integer :: icoe02
      real :: rcoe02
      real :: rcoe03
      integer, dimension(1:2,1:2) :: iadn21
      real, dimension(1:2,1:2) :: radn21
      integer, dimension(1:2,1:2) :: iade23
      integer, dimension(1:2,1:2) :: iade24
      real, dimension(1:2,1:2) :: rade23
      real, dimension(1:2,1:2) :: rade24
      equivalence (iade23(2,2),iadn22(2,2),iade24(2,2))                 
      equivalence (rade23(2,2),radn22(2,2),rade24(2,2))                 
      equivalence (icoe01,icoe02,icoe03,icoe04), (rcoe01,rcoe02,rcoe03) 
      integer, dimension(1:2) :: radn11
      integer, dimension(1:2,1:2) :: radn25
      logical, dimension(1:2,1:2) :: ladn21
      data radn21(2,2) / -512. / 
      data ladn21 / 4*.true. / 
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
      ivtnum = 632                                                      
!                                                                        
!       ****  TEST 632  ****                                             
!      TEST 632  -  TESTS SETTING AN INTEGER ARRAY ELEMENT BY A          
!      SIMPLE ASSIGNMENT STATEMENT TO THE VALUE 9999.                    
!                                                                        
      if (iczero) 36320, 6320, 36320                                    
 6320 continue                                                          
      iadn21(1,1) = 9999                                                
      ivcomp = iadn21(1,1)                                              
      goto 46320                                                       
36320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46320, 6331, 46320                                    
46320 if ( ivcomp - 9999 )  26320, 16320, 26320                         
16320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6331                                                        
26320 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6331 continue                                                          
      ivtnum = 633                                                      
!                                                                        
!       ****  TEST 633  ****                                             
!      TEST 633  -  TESTS SETTING A REAL ARRAY ELEMENT BY A SIMPLE       
!      ASSIGNMENT STATEMENT TO THE VALUE -32766.                         
!                                                                        
      if (iczero) 36330, 6330, 36330                                    
 6330 continue                                                          
      radn21(1,2) = -32766.                                             
      ivcomp = radn21(1,2)                                              
      goto 46330                                                       
36330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46330, 6341, 46330                                    
46330 if ( ivcomp + 32766 )  26330, 16330, 26330                        
16330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6341                                                        
26330 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6341 continue                                                          
      ivtnum = 634                                                      
!                                                                        
!       ****  TEST 634  ****                                             
!      TEST 634  -  TEST OF THE DATA INITIALIZATION STATEMENT AND SETTING
!      AN INTEGER ARRAY ELEMENT EQUAL TO THE VALUE OF A REAL ARRAY       
!      ELEMENT.  THE VALUE USED IS -512.                                 
!                                                                        
      if (iczero) 36340, 6340, 36340                                    
 6340 continue                                                          
      iadn21(2,2) = radn21(2,2)                                         
      ivcomp = iadn21(2,2)                                              
      goto 46340                                                       
36340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46340, 6351, 46340                                    
46340 if ( ivcomp + 512 )  26340, 16340, 26340                          
16340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6351                                                        
26340 ivfail = ivfail + 1                                               
      ivcorr = -512                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6351 continue                                                          
      ivtnum = 635                                                      
!                                                                        
!       ****  TEST 635  ****                                             
!      TEST 635  -  TEST OF SETTING A TWO DIMENSIONED ARRAY ELEMENT      
!      EQUAL TO THE VALUE OF A ONE DIMENSIONED ARRAY ELEMENT.            
!      BOTH ARRAYS ARE SET INTEGER BY THE TYPE STATEMENT AND THE TWO     
!      DIMENSIONED ARRAY ELEMENT IS MINUS THE VALUE OF THE ONE DIMENSION 
!      ELEMENT.  THE VALUE USED IS 3.                                    
!                                                                        
      if (iczero) 36350, 6350, 36350                                    
 6350 continue                                                          
      radn11(1) = 3                                                     
      radn25(2,2) = - radn11(1)                                         
      ivcomp = radn25(2,2)                                              
      goto 46350                                                       
36350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46350, 6361, 46350                                    
46350 if ( ivcomp + 3 )  26350, 16350, 26350                            
16350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6361                                                        
26350 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6361 continue                                                          
      ivtnum = 636                                                      
!                                                                        
!       ****  TEST 636  ****                                             
!      TEST 636  -  TEST OF LOGICAL ARRAY ELEMENTS SET BY DATA STATEMENTS
!                                                                        
      if (iczero) 36360, 6360, 36360                                    
 6360 continue                                                          
      icon01 = 0                                                        
      if ( ladn21(2,1) )  icon01 = 1                                    
      goto 46360                                                       
36360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46360, 6371, 46360                                    
46360 if ( icon01 - 1 )  26360, 16360, 26360                            
16360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6371                                                        
26360 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6371 continue                                                          
      ivtnum = 637                                                      
!                                                                        
!       ****  TEST 637  ****                                             
!      TEST 637  -  TEST OF REAL TO INTEGER CONVERSION AND SETTING       
!      INTEGER ARRAY ELEMENTS TO THE VALUE OBTAINED IN AN ARITHMETIC     
!      EXPRESSION USING REAL ARRAY ELEMENTS.   .5  +  .5  =  1           
!                                                                        
      if (iczero) 36370, 6370, 36370                                    
 6370 continue                                                          
      radn21(1,2) = 00000.5                                             
      radn21(2,1) = .500000                                             
      iadn21(2,1) = radn21(1,2) + radn21(2,1)                           
      ivcomp = iadn21(2,1)                                              
      goto 46370                                                       
36370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46370, 6381, 46370                                    
46370 if ( ivcomp - 1 )  26370, 16370, 26370                            
16370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6381                                                        
26370 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6381 continue                                                          
      ivtnum = 638                                                      
!                                                                        
!       ****  TEST 638  ****                                             
!      TEST 638  -  TEST OF EQUIVALENCE OF THREE INTEGER ARRAYS ONE OF   
!      WHICH IS IN COMMON.                                               
!                                                                        
      if (iczero) 36380, 6380, 36380                                    
 6380 continue                                                          
      iadn22(2,1) = -9999                                               
      ivcomp = iade23(2,1)                                              
      goto 46380                                                       
36380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46380, 6391, 46380                                    
46380 if ( ivcomp + 9999 )  26380, 16380, 26380                         
16380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6391                                                        
26380 ivfail = ivfail + 1                                               
      ivcorr = -9999                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6391 continue                                                          
      ivtnum = 639                                                      
!                                                                        
!       ****  TEST 639  ****                                             
!      TEST 639  -  LIKE TEST 638 ONLY THE OTHER EQUIVALENCED ARRAY IS   
!      TESTED FOR THE VALUE -9999.                                       
!                                                                        
      if (iczero) 36390, 6390, 36390                                    
 6390 continue                                                          
      iade23(2,1) = -9999                                               
      ivcomp = iade24(2,1)                                              
      goto 46390                                                       
36390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46390, 6401, 46390                                    
46390 if ( ivcomp + 9999 )  26390, 16390, 26390                         
16390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6401                                                        
26390 ivfail = ivfail + 1                                               
      ivcorr = -9999                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6401 continue                                                          
      ivtnum = 640                                                      
!                                                                        
!       ****  TEST 640  ****                                             
!      TEST 640  -  TEST OF THREE REAL ARRAYS THAT ARE EQUIVALENCED.     
!      ONE OF THE ARRAYS IS IN COMMON.  THE VALUE 512 IS SET INTO ONE OF 
!      THE DIMENSIONED ARRAY ELEMENTS BY AN INTEGER TO REAL CONVERSION   
!      ASSIGNMENT STATEMENT.                                             
!                                                                        
      if (iczero) 36400, 6400, 36400                                    
 6400 continue                                                          
      rade24(2,2) = 512                                                 
      ivcomp = radn22(2,2)                                              
      goto 46400                                                       
36400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46400, 6411, 46400                                    
46400 if ( ivcomp - 512 )  26400, 16400, 26400                          
16400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6411                                                        
26400 ivfail = ivfail + 1                                               
      ivcorr = 512                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6411 continue                                                          
      ivtnum = 641                                                      
!                                                                        
!       ****  TEST 641  ****                                             
!      TEST 641  -  LIKE TEST 640 ONLY THE OTHER EQUIVALENCED ARRAY IS   
!      TESTED FOR THE VALUE 512.                                         
!                                                                        
      if (iczero) 36410, 6410, 36410                                    
 6410 continue                                                          
      radn22(2,2) = 512                                                 
      ivcomp = rade23(2,2)                                              
      goto 46410                                                       
36410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46410, 6421, 46410                                    
46410 if ( ivcomp - 512 )  26410, 16410, 26410                          
16410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6421                                                        
26410 ivfail = ivfail + 1                                               
      ivcorr = 512                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6421 continue                                                          
      ivtnum = 642                                                      
!                                                                        
!       ****  TEST 642  ****                                             
!      TEST 642  -  TEST OF FOUR INTEGER VARIABLES THAT ARE EQUIVALENCED.
!      ONE OF THE INTEGER VARIABLES IS IN BLANK COMMON.  THE VALUE USED  
!      IS 3 SET  BY AN ASSIGNMENT STATEMENT.                             
!                                                                        
      if (iczero) 36420, 6420, 36420                                    
 6420 continue                                                          
      icoe03 = 3                                                        
      ivcomp = icoe01                                                   
      goto 46420                                                       
36420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46420, 6431, 46420                                    
46420 if ( ivcomp - 3 )  26420, 16420, 26420                            
16420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6431                                                        
26420 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6431 continue                                                          
      ivtnum = 643                                                      
!                                                                        
!       ****  TEST 643  ****                                             
!      TEST 643  -  LIKE TEST 642 BUT ANOTHER OF THE ELEMENTS IS TESTED  
!      BY AN ARITHMETIC EXPRESSION USING THE EQUIVALENCED  ELEMENTS.     
!      THE VALUE OF ALL OF THE ELEMENTS SHOULD INITITIALLY BE 3 SINCE    
!      THEY ALL SHOULD SHARE THE SAME STORAGE LOCATION. ICOE04 = 3+3+3+3 
!      ICOE04 = 12  THEN THE ELEMENT ICOE02 IS TESTED FOR THE VALUE 12.  
!                                                                        
      if (iczero) 36430, 6430, 36430                                    
 6430 continue                                                          
      icoe01 = 3                                                        
      icoe04 = icoe01 + icoe02 + icoe03 + icoe04                        
      ivcomp = icoe02                                                   
      goto 46430                                                       
36430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46430, 6441, 46430                                    
46430 if ( ivcomp - 12 )  26430, 16430, 26430                           
16430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6441                                                        
26430 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6441 continue                                                          
      ivtnum = 644                                                      
!                                                                        
!       ****  TEST 644  ****                                             
!      TEST 644  -  TEST OF EQUIVALENCE WITH THREE REAL VARIABLES ONE    
!      OF WHICH IS IN BLANK COMMON.  THE ELEMENTS ARE SET INITIALLY TO .5
!      THEN ALL OF THE ELEMENTS ARE USED IN AN ARITHMETIC EXPRESSION     
!      RCOE01 =(.5 + .5 + .5) * 2.   SO RCOE01 = 3.   ELEMENT RCOE02     
!      IS TESTED FOR THE VALUE 3.                                        
!                                                                        
      if (iczero) 36440, 6440, 36440                                    
 6440 continue                                                          
      rcoe02 = 0.5                                                      
      rcoe01 = ( rcoe01 + rcoe02 + rcoe03 ) * 2.                        
      ivcomp = rcoe02                                                   
      goto 46440                                                       
36440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46440, 6451, 46440                                    
46440 if ( ivcomp - 3 )  26440, 16440, 26440                            
16440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6451                                                        
26440 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6451 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM023" )                          
      end program fm023
