      program fm026
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
!      COMMENT SECTION                                                   
!                                                                        
!      FM026                                                             
!                                                                        
!          THIS ROUTINE CONTAINS THE BASIC SUBROUTINE REFERENCE TESTS.   
!      THE SUBROUTINE FS027 IS CALLED BY THIS PROGRAM.  THE SUBROUTINE   
!      FS027 INCREMENTS THE CALLING ARGUMENT BY 1 AND RETURNS TO THE     
!      CALLING PROGRAM.                                                  
!                                                                        
!          EXECUTION OF A SUBROUTINE REFERENCE RESULTS IN AN ASSOCIATION 
!      OF ACTUAL ARGUMENTS WITH ALL APPEARANCES OF DUMMY ARGUMENTS IN    
!      THE DEFINING SUBPROGRAM.  FOLLOWING THESE ASSOCIATIONS, EXECUTION 
!      OF THE FIRST EXECUTABLE STATEMENT OF THE DEFINING SUBPROGRAM      
!      IS UNDERTAKEN.                                                    
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
!          SUBROUTINE REFERENCE - CALL                                   
!                                                                        
      ivtnum = 666                                                      
!                                                                        
!       ****  TEST 666  ****                                             
!      SUBROUTINE CALL - ARGUMENT NAME SAME AS SUBROUTINE ARGUMENT NAME. 
!                                                                        
      if (iczero) 36660, 6660, 36660                                    
 6660 continue                                                          
      ivon01 = 0                                                        
      call fs027(ivon01)

      ivcomp = ivon01                                                   
      goto 46660                                                       
36660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46660, 6671, 46660                                    
46660 if (ivcomp - 1) 26660,16660,26660                                 
16660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6671                                                        
26660 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6671 continue                                                          
      ivtnum = 667                                                      
!                                                                        
!       ****  TEST 667  ****                                             
!      SUBROUTINE CALL - ARGUMENT NAME SAME AS INTERNAL VARIABLE IN      
!          SUBROUTINE.                                                   
!                                                                        
      if (iczero) 36670, 6670, 36670                                    
 6670 continue                                                          
      ivon02 = 2                                                        
      call fs027(ivon02)

      ivcomp = ivon02                                                   
      goto 46670                                                       
36670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46670, 6681, 46670                                    
46670 if (ivcomp - 3) 26670,16670,26670                                 
16670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6681                                                        
26670 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6681 continue                                                          
      ivtnum = 668                                                      
!                                                                        
!       ****  TEST 668  ****                                             
!      SUBROUTINE CALL - ARGUMENT NAME DIFFERENT FROM SUBROUTINE ARGUMENT
!          AND INTERNAL VARIABLE.                                        
!                                                                        
      if (iczero) 36680, 6680, 36680                                    
 6680 continue                                                          
      ivon01 = 7                                                        
      ivon03 = -12                                                      
      call fs027(ivon03)

      ivcomp = ivon03                                                   
      goto 46680                                                       
36680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46680, 6691, 46680                                    
46680 if (ivcomp + 11 ) 26680,16680,26680                               
16680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6691                                                        
26680 ivfail = ivfail + 1                                               
      ivcorr = -11                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6691 continue                                                          
      ivtnum = 669                                                      
!                                                                        
!       ****  TEST 669  ****                                             
!      REPEATED SUBROUTINE CALLS IN A DO LOOP.                           
!                                                                        
      if (iczero) 36690, 6690, 36690                                    
 6690 continue                                                          
      ivcomp = 0                                                        
      do ivon04 = 1,5                                              
      call fs027(ivcomp)

  end do
      goto 46690                                                       
36690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46690, 6701, 46690                                    
46690 if (ivcomp - 5) 26690,16690,26690                                 
16690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6701                                                        
26690 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!       ****     END OF TESTS   ****                                     
 6701 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM026" )                          
      end program fm026
      subroutine fs027(ivon01)
      integer :: ivon01
      integer :: ivon02
!      COMMENT SECTION                                                   
!                                                                        
!      FS027                                                             
!                                                                        
!          THIS SUBROUTINE IS CALLED BY THE MAIN PROGRAM FM026.  THE     
!      SUBROUTINE ARGUMENT IS INCREMENTED BY 1 AND CONTROL RETURNED      
!      TO THE CALLING PROGRAM.                                           
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.6, SUBROUTINES                                      
!         SECTION 15.8, RETURN STATEMENT                                 
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!          SUBROUTINE SUBPROGRAM                                         
!                                                                        
!      INCREMENT ARGUMENT BY 1 AND RETURN TO CALLING PROGRAM.            
!                                                                        
      ivon02 = ivon01                                                   
      ivon02 = ivon02 + 1                                               
      ivon01 = ivon02                                                   
      ivon02 = 300                                                      
      return                                                            
      end subroutine fs027
