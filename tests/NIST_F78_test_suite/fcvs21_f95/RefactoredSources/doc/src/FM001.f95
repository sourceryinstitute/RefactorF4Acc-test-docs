      program fm001
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
!      FM001                                                             
!                                                                        
!          THIS ROUTINE CONTAINS THE BOILERPLATE SOURCE CODING WHICH     
!      IS USED TO PRINT THE REPORT HEADINGS AND RUN SUMMARIES FOR EACH   
!      OF THE ELEMENTARY ROUTINES.                                       
!                                                                        
!          THREE TESTS ARE INCLUDED WHICH CONTAIN THE PROCEDURES FOR     
!          TESTING THE LANGUAGE FEATURES AND DELETING TESTS.             
!                                                                        
!          TEST 1 CHECKS THE PASS PROCEDURE                              
!          TEST 2 CHECKS THE FAIL PROCEDURE                              
!          TEST 3 CHECKS THE DELETE PROCEDURE                            
!                                                                        
!          IF THIS ROUTINE DOES NOT EXECUTE CORRECTLY, THEN NO OTHER     
!      ROUTINES WILL BE RUN.  THERE IS NO USE IN TRYING TO VALIDATE A    
!      FORTRAN COMPILER WHICH CANNOT HANDLE SUCH BASIC STATEMENTS.       
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
   11 continue                                                          
!                                                                        
!       ****  TEST 001  ****                                             
!      TEST 001  -  BASIC PROCEDURE FOR CODING TESTS                     
!            ALSO CHECKS CONTINUE STATEMENT WHICH SHOULD NOT HAVE        
!            ANY AFFECT ON EXECUTION SEQUENCE                            
!                                                                        
      if (iczero) 30010, 10, 30010                                      
   10 continue                                                          
      ivtnum=1                                                          
      goto 40010                                                       
30010 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40010, 21, 40010                                      
40010 if (ivtnum - 1) 20010, 10010, 20010                               
10010 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 21                                                          
20010 ivfail=ivfail+1                                                   
      ivcomp=ivtnum                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
   21 continue                                                          
!                                                                        
!       ****  TEST 002  ****                                             
!      TEST - 002    FORCE FAIL CODE TO BE EXECUTED                      
!                                                                        
      if (iczero) 30020,20,30020                                        
   20 continue                                                          
      ivtnum=2                                                          
      goto 40020                                                       
30020 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40020,31,40020                                        
40020 if (ivtnum-1) 20020, 10020, 20020                                 
10020 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 31                                                          
20020 ivfail=ivfail+1                                                   
      ivcomp=ivtnum                                                     
      ivcorr=2                                                          
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
   31 continue                                                          
!                                                                        
!       ****  TEST 003  ****                                             
!      TEST 003 - DELETE PROCEDURE TESTED                                
!                                                                        
      if (iczero) 30030,30,30030                                        
   30 continue                                                          
!      IVTNUM=5000                                                       
!      GO TO 40030                                                       
30030 ivdele=ivdele+1                                                   
      ivtnum=3                                                          
      write (i02,80003) ivtnum                                          
      if (iczero) 40030,99999,40030                                     
40030 if (ivtnum - 5000) 20030,10030,20030                              
10030 ivpass=ivpass +1                                                  
      write (i02,80001) ivtnum                                          
      goto 99999                                                       
20030 ivfail=ivfail+1                                                   
      ivcomp=ivtnum                                                     
      ivcorr=5000                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
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
!      SPECIAL OUTPUT STATEMENTS FOR THIS ROUTINE                        
      write (i02,90000)                                                 
      write (i02,90002)                                                 
      write (i02,80031)                                                 
      write (i02,90002)                                                 
      write (i02,80010)                                                 
      write (i02,80020)                                                 
      write (i02,80030)                                                 
      write (i02,80032)                                                 
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
!      FORMATS FOR CURRENT ROUTINE                                       
80031 format (" ",10x,"THE PROGRAM FM001 EXECUTED CORRECTLY IF" )       
80010 format (" ",15x,"TEST 1 PASSED" )                                 
80020 format (" ",15x,"TEST 2 FAILED WITH COMPUTED AND CORRECT =2" )    
80030 format (" ",15x,"TEST 3 WAS DELETED" )                            
80032 format (" ",15x,"THE RUN SUMMARY TOTALS ALL EQUAL 1" )            
90007 format (" ",20x,"END OF PROGRAM FM001" )                          
      end program fm001
