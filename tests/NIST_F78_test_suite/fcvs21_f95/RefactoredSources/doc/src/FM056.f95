      program fm056
      integer, dimension(1:12) :: iacn11
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivon01
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
!                                                                        
!      COMMENT SECTION                                                   
!                                                                        
!      FM056                                                             
!                                                                        
!           FM056 IS A MAIN WHICH TESTS THE ARGUMENT PASSING LINKAGE OF  
!      A 2 LEVEL NESTED SUBROUTINE AND AN EXTERNAL FUNCTION REFERENCE.   
!      THE MAIN PROGRAM FM056 CALLS SUBROUTINE FS057 PASSING ONE         
!      ARGUMENT.  SUBROUTINE FS057 CALLS SUBROUTINE FS058 PASSING TWO    
!      ARGUMENTS.  SUBROUTINE FS058 REFERENCES EXTERNAL FUNCTION FF059   
!      PASSING 3 ARGUMENTS.  FUNCTION FF059 ADDS THE VALUES OF THE 3     
!      ARGUMENTS TOGETHER.  SUBROUTINE FS057 AND FS058 THEN MERELY       
!      RETURN THE RESULT TO FM056 IN THE FIRST ARGUMENT.                 
!                                                                        
!           THE VALUES OF THE ARGUMENTS THAT ARE PASSED TO EACH          
!      SUBPROGRAM AND FUNCTION, AND RETURNED TO THE CALLING OR           
!      REFERENCING PROGRAM ARE SAVED IN AN INTEGER ARRAY.  FM056 THEN    
!      USES THESE VALUES TO TEST THE COMPILER'S ARGUMENT PASSING         
!      CAPABILITIES.                                                     
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.6.2, SUBROUTINE REFERENCE                           
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
!          SUBROUTINE SUBPROGRAM                                         
!                                                                        
      ivon01 = 5                                                        
      call fs057(ivon01,iacn11)

      iacn11 (12) = ivon01                                              
      ivtnum = 430                                                      
!                                                                        
!       ****  TEST 430  ****                                             
!                                                                        
!      TEST 430 TESTS THE VALUE OF THE ARGUMENT RECEIVED BY FS057 FROM   
!      A FM056 CALL TO FS057                                             
!                                                                        
      if (iczero) 34300, 4300, 34300                                    
 4300 continue                                                          
      ivcomp = iacn11 (1)                                               
      goto 44300                                                       
34300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44300, 4311, 44300                                    
44300 if (ivcomp - 5) 24300,14300,24300                                 
14300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4311                                                        
24300 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4311 continue                                                          
      ivtnum = 431                                                      
!                                                                        
!       ****  TEST 431  ****                                             
!                                                                        
!      TEST 431 TESTS THE VALUE OF THE SECOND ARGUMENT THAT WAS PASSED   
!      FROM A FS057 CALL TO FS058                                        
!                                                                        
!                                                                        
      if (iczero) 34310, 4310, 34310                                    
 4310 continue                                                          
      ivcomp = iacn11 (2)                                               
      goto 44310                                                       
34310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44310, 4321, 44310                                    
44310 if (ivcomp - 4) 24310,14310,24310                                 
14310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4321                                                        
24310 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4321 continue                                                          
      ivtnum = 432                                                      
!                                                                        
!       ****  TEST 432  ****                                             
!                                                                        
!      TEST 432 TESTS THE VALUE OF THE FIRST ARGUMENT RECEIVED BY FS058  
!      FROM A FS057 CALL TO FS058                                        
!                                                                        
!                                                                        
      if (iczero) 34320, 4320, 34320                                    
 4320 continue                                                          
      ivcomp = iacn11 (3)                                               
      goto 44320                                                       
34320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44320, 4331, 44320                                    
44320 if (ivcomp - 5) 24320,14320,24320                                 
14320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4331                                                        
24320 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4331 continue                                                          
      ivtnum = 433                                                      
!                                                                        
!       ****  TEST 433  ****                                             
!                                                                        
!      TEST 433 TESTS THE VALUE OF THE SECOND ARGUMENT RECEIVED BY FS058 
!      FROM A FS057 CALL TO FS058                                        
!                                                                        
!                                                                        
      if (iczero) 34330, 4330, 34330                                    
 4330 continue                                                          
      ivcomp = iacn11 (4)                                               
      goto 44330                                                       
34330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44330, 4341, 44330                                    
44330 if (ivcomp - 4) 24330,14330,24330                                 
14330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4341                                                        
24330 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4341 continue                                                          
      ivtnum = 434                                                      
!                                                                        
!       ****  TEST 434  ****                                             
!                                                                        
!      TEST 434 TESTS THE VALUE OF THE THIRD ARGUMENT THAT WAS PASSED    
!      FROM A FS058 REFERENCE OF FUNCTION FF059                          
!                                                                        
!                                                                        
      if (iczero) 34340, 4340, 34340                                    
 4340 continue                                                          
      ivcomp = iacn11 (5)                                               
      goto 44340                                                       
34340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44340, 4351, 44340                                    
44340 if (ivcomp - 3) 24340,14340,24340                                 
14340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4351                                                        
24340 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4351 continue                                                          
      ivtnum = 435                                                      
!                                                                        
!       ****  TEST 435  ****                                             
!                                                                        
!      TEST 435 TESTS THE VALUE OF THE FIRST ARGUMENT RECEIVED BY FF059  
!      FROM A FS058 REFERENCE OF FUNCTION FF059                          
!                                                                        
!                                                                        
      if (iczero) 34350, 4350, 34350                                    
 4350 continue                                                          
      ivcomp = iacn11 (6)                                               
      goto 44350                                                       
34350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44350, 4361, 44350                                    
44350 if (ivcomp - 5) 24350,14350,24350                                 
14350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4361                                                        
24350 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4361 continue                                                          
      ivtnum = 436                                                      
!                                                                        
!       ****  TEST 436  ****                                             
!                                                                        
!      TEST 436 TESTS THE VALUE OF THE SECOND ARGUMENT RECEIVED BY FF059 
!      FROM A FS058 REFERENCE OF FUNCTION FF059                          
!                                                                        
!                                                                        
      if (iczero) 34360, 4360, 34360                                    
 4360 continue                                                          
      ivcomp = iacn11 (7)                                               
      goto 44360                                                       
34360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44360, 4371, 44360                                    
44360 if (ivcomp - 4) 24360,14360,24360                                 
14360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4371                                                        
24360 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4371 continue                                                          
      ivtnum = 437                                                      
!                                                                        
!       ****  TEST 437  ****                                             
!                                                                        
!      TEST 437 TESTS THE VALUE OF THE THIRD ARGUMENT RECEIVED BY FF059  
!      FROM A FS058 REFERENCE OF FUNCTION FF059                          
!                                                                        
!                                                                        
      if (iczero) 34370, 4370, 34370                                    
 4370 continue                                                          
      ivcomp = iacn11 (8)                                               
      goto 44370                                                       
34370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44370, 4381, 44370                                    
44370 if (ivcomp - 3) 24370,14370,24370                                 
14370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4381                                                        
24370 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4381 continue                                                          
      ivtnum = 438                                                      
!                                                                        
!       ****  TEST 438  ****                                             
!                                                                        
!      TEST 438 TESTS THE VALUE OF THE FUNCTION DETERMINED BY FF059      
!                                                                        
!                                                                        
      if (iczero) 34380, 4380, 34380                                    
 4380 continue                                                          
      ivcomp = iacn11 (9)                                               
      goto 44380                                                       
34380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44380, 4391, 44380                                    
44380 if (ivcomp - 12) 24380,14380,24380                                
14380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4391                                                        
24380 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4391 continue                                                          
      ivtnum = 439                                                      
!                                                                        
!       ****  TEST 439  ****                                             
!                                                                        
!      TEST 439 TESTS THE VALUE OF THE FUNCTION RETURNED TO FS058 BY     
!      FF059                                                             
!                                                                        
!                                                                        
      if (iczero) 34390, 4390, 34390                                    
 4390 continue                                                          
      ivcomp = iacn11 (10)                                              
      goto 44390                                                       
34390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44390, 4401, 44390                                    
44390 if (ivcomp - 12) 24390,14390,24390                                
14390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4401                                                        
24390 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4401 continue                                                          
      ivtnum = 440                                                      
!                                                                        
!       ****  TEST 440  ****                                             
!                                                                        
!      TEST 440 TESTS THE VALUE OF THE FIRST ARGUMENT RETURNED TO FS057  
!      BY FS058                                                          
!                                                                        
      if (iczero) 34400, 4400, 34400                                    
 4400 continue                                                          
      ivcomp = iacn11 (11)                                              
      goto 44400                                                       
34400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44400, 4411, 44400                                    
44400 if (ivcomp - 12) 24400,14400,24400                                
14400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4411                                                        
24400 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4411 continue                                                          
      ivtnum = 441                                                      
!                                                                        
!       ****  TEST 441  ****                                             
!                                                                        
!      TEST 441 TESTS THE VALUE OF THE FIRST ARGUMENT RETURNED TO FM056  
!      BY FS057                                                          
!                                                                        
!                                                                        
      if (iczero) 34410, 4410, 34410                                    
 4410 continue                                                          
      ivcomp = iacn11 (12)                                              
      goto 44410                                                       
34410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44410, 4421, 44410                                    
44410 if (ivcomp - 12) 24410,14410,24410                                
14410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4421                                                        
24410 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4421 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM056" )                          
      end program fm056
      subroutine fs057(ivon01,iacn11)
      integer, dimension(1:12) :: iacn11
      integer :: ivon01
      integer :: ivon02
      iacn11 (1) = ivon01                                               
      ivon02 = 4                                                        
      iacn11 (2) = ivon02                                               
      call fs058(ivon01,ivon02,iacn11)

      iacn11 (11) = ivon01                                              
      return                                                            
      end subroutine fs057
      subroutine fs058(ivon01,ivon02,iacn11)
      integer, dimension(1:12) :: iacn11
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ff059
      ivon03 = 3                                                        
      iacn11 (3) = ivon01                                               
      iacn11 (4) = ivon02                                               
      iacn11 (5) = ivon03                                               
      ivon01  = ff059(ivon01,ivon02,ivon03)
      iacn11 (10) = ivon01                                              
      return                                                            
      end subroutine fs058
      integer function ff059(ivon01,ivon02,ivon03)
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer, dimension(1:12) :: iacn11
      iacn11 (6) = ivon01                                               
      iacn11 (7) = ivon02                                               
      iacn11 (8) = ivon03                                               
      ff059 = ivon01 + ivon02 + ivon03                                  
      iacn11 (9) = ivon01 + ivon02 + ivon03                             
      return                                                            
      end function ff059
