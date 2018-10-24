      program fm028
!      COMMENT SECTION                                                   
!                                                                        
!      FM028                                                             
!                                                                        
!          THIS ROUTINE CONTAINS THE EXTERNAL FUNCTION REFERENCE TESTS.  
!      THE FUNCTION SUBPROGRAM FF029 IS CALLED BY THIS PROGRAM. THE      
!      FUNCTION SUBPROGRAM FF029 INCREMENTS THE CALLING ARGUMENT BY 1    
!      AND RETURNS TO THE CALLING PROGRAM.                               
!                                                                        
!          EXECUTION OF AN EXTERNAL FUNCTION REFERENCE RESULTS IN AN     
!      ASSOCIATION OF ACTUAL ARGUMENTS WITH ALL APPEARANCES OF DUMMY     
!      ARGUMENTS IN THE DEFINING SUBPROGRAM.  FOLLOWING THESE            
!      ASSOCIATIONS, EXECUTION OF THE FIRST EXECUTABLE STATEMENT OF THE  
!      DEFINING SUBPROGRAM IS UNDERTAKEN.                                
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.5.2, REFERENCING AN EXTERNAL FUNCTION               
!                                                                        
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
      integer :: ff029! decl of func/sub in program
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
!      EXTERNAL FUNCTION REFERENCE                                       
!                                                                        
!      EXTERNAL FUNCTION REFERENCE - ARGUMENT NAME SAME AS SUBPROGRAM    
!               ARGUMENT NAME.                                           
 6701 continue                                                          
      ivtnum = 670                                                      
!                                                                        
!      **** TEST 670 ****                                                
!                                                                        
      if (iczero) 36700,6700,36700                                      
 6700 continue                                                          
      ivon01 = 0                                                        
      ivcomp  = ff029(ivon01)
      goto 46700                                                       
36700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46700,6711,46700                                      
46700 if (ivcomp - 1) 26700,16700,26700                                 
16700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6711                                                        
26700 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6711 continue                                                          
      ivtnum = 671                                                      
!                                                                        
!       ****  TEST 671  ****                                             
!                                                                        
!      EXTERNAL FUNCTION REFERENCE - ARGUMENT NAME SAME AS INTERNAL      
!            VARIABLE IN FUNCTION SUBPROGRAM.                            
!                                                                        
      if (iczero) 36710,6710,36710                                      
 6710 continue                                                          
      ivon02 = 2                                                        
      ivon01 = 5                                                        
      ivcomp  = ff029(ivon02)
      goto 46710                                                       
36710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46710,6721,46710                                      
46710 if (ivcomp - 3) 26710,16710,26710                                 
16710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6721                                                        
26710 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6721 continue                                                          
      ivtnum = 672                                                      
!                                                                        
!      ****  TEST 672  ****                                              
!                                                                        
!      EXTERNAL FUNCTION REFERENCE - ARGUMENT NAME DIFFERENT FROM        
!            FUNCTION SUBPROGRAM ARGUMENT AND INTERNAL VARIABLE.         
!                                                                        
      if  (iczero) 36720,6720,36720                                     
 6720 continue                                                          
      ivon01 = 7                                                        
      ivon03 = -12                                                      
      ivcomp  = ff029(ivon03)
      goto 46720                                                       
36720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46720,6731,46720                                      
46720 if (ivcomp + 11) 26720,16720,26720                                
16720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6731                                                        
26720 ivfail = ivfail + 1                                               
      ivcorr = -11                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6731 continue                                                          
      ivtnum = 673                                                      
!                                                                        
!       **** TEST 673  ****                                              
!                                                                        
!      REPEATED EXTERNAL FUNCTION REFERENCE IN A DO LOOP.                
!                                                                        
      if (iczero) 36730,6730,36730                                      
 6730 continue                                                          
      ivon01 = -7                                                       
      ivcomp = 0                                                        
      do ivon04 = 1,5                                              
      ivcomp  = ff029(ivcomp)
  end do
      goto 46730                                                       
36730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46730,6741,46730                                      
46730 if (ivcomp - 5) 26730,16730,26730                                 
16730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6741                                                        
26730 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6741 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM028" )                          
      end program fm028
      integer function ff029(ivon01)
      integer :: ivon01
      integer :: ivon02
!                                                                        
!      COMMENT SECTION                                                   
!      FF029                                                             
!                                                                        
!          THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM028. 
!      THE FUNCTION ARGUMENT IS INCREMENTED BY 1 AND CONTROL RETURNED    
!      TO THE CALLING PROGRAM.                                           
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.5.1, DEFINING FUNCTION SUBPROGRAMS AND FUNCTION     
!                         STATEMENTS                                     
!         SECTION 15.8, RETURN STATEMENT                                 
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!           FUNCTION SUBPROGRAM                                          
!                                                                        
!      INCREMENT ARGUMENT BY 1 AND RETURN TO CALLING PROGRAM.            
!                                                                        
      ivon02 = ivon01                                                   
      ff029  = ivon02 + 1                                               
      ivon02 = 500                                                      
      return                                                            
      end function ff029
