      program fm050
!                                                                        
!      COMMENT SECTION                                                   
!                                                                        
!      FM050                                                             
!                                                                        
!           THIS ROUTINE CONTAINS BASIC SUBROUTINE AND FUNCTION REFERENCE
!      TESTS.  FOUR SUBROUTINES AND ONE FUNCTION ARE CALLED OR           
!      REFERENCED.  FS051 IS CALLED TO TEST THE CALLING AND PASSING OF   
!      ARGUMENTS THROUGH UNLABELED COMMON.  NO ARGUMENTS ARE SPECIFIED   
!      IN THE CALL LINE.  FS052 IS IDENTICAL TO FS051 EXCEPT THAT SEVERAL
!      RETURNS ARE USED.  FS053 UTILIZES MANY ARGUMENTS ON THE CALL      
!      STATEMENT AND MANY RETURN STATEMENTS IN THE SUBROUTINE BODY.      
!      FF054 IS A FUNCTION SUBROUTINE IN WHICH MANY ARGUMENTS AND RETURN 
!      STATEMENTS ARE USED.  AND FINALLY FS055 PASSES A ONE DIMENIONAL   
!      ARRAY BACK TO FM050.                                              
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.5.2, REFERENCING AN EXTERNAL FUNCTION               
!         SECTION 15.6.2, SUBROUTINE REFERENCE                           
!                                                                        
      integer, dimension(1:20) :: iacn11
      integer :: ivcn01
      integer :: ivcn02
      real :: rvcn01
      real :: rvdn01
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvcomp
      real :: rvcorr
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon04
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivon05
      integer :: i
      integer :: ff054
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
!          SUBROUTINE AND FUNCTION SUBPROGRAMS                           
!                                                                        
 4001 continue                                                          
      ivtnum = 400                                                      
!                                                                        
!       ****  TEST 400  ****                                             
!      TEST 400 TESTS THE CALL TO A SUBROUTINE CONTAINING NO ARGUMENTS.  
!      ALL PARAMETERS ARE PASSED THROUGH UNLABELED COMMON.               
!                                                                        
      if (iczero) 34000, 4000, 34000                                    
 4000 continue                                                          
      rvcn01 = 2.1654                                                   
      call fs051(rvcn01)

      rvcomp = rvcn01                                                   
      goto 44000                                                       
34000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44000, 4011, 44000                                    
44000 if (rvcomp - 3.1649) 24000,14000,44001                            
44001 if (rvcomp - 3.1659) 14000,14000,24000                            
14000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4011                                                        
24000 ivfail = ivfail + 1                                               
      rvcorr = 3.1654                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 4011 continue                                                          
!                                                                        
!      TEST 401 THROUGH TEST 403 TEST THE CALL TO SUBROUTINE FS052 WHICH 
!      CONTAINS NO ARGUMENTS.  ALL PARAMETERS ARE PASSED THROUGH         
!      UNLABELED COMMON.  SUBROUTINE FS052 CONTAIN SEVERAL RETURN        
!      STATEMENTS.                                                       
!                                                                        
      ivtnum = 401                                                      
!                                                                        
!       ****  TEST 401  ****                                             
!                                                                        
      if (iczero) 34010, 4010, 34010                                    
 4010 continue                                                          
      ivcn01 = 5                                                        
      ivcn02 = 1                                                        
      call fs052(ivcn01,ivcn02,rvdn01)

      ivcomp = ivcn01                                                   
      goto 44010                                                       
34010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44010, 4021, 44010                                    
44010 if (ivcomp - 6) 24010,14010,24010                                 
14010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4021                                                        
24010 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4021 continue                                                          
      ivtnum = 402                                                      
!                                                                        
!       ****  TEST 402  ****                                             
!                                                                        
      if (iczero) 34020, 4020, 34020                                    
 4020 continue                                                          
      ivcn01 = 10                                                       
      ivcn02 =  5                                                       
      call fs052(ivcn01,ivcn02,rvdn01)

      ivcomp = ivcn01                                                   
      goto 44020                                                       
34020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44020, 4031, 44020                                    
44020 if (ivcomp - 15) 24020,14020,24020                                
14020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4031                                                        
24020 ivfail = ivfail + 1                                               
      ivcorr = 15                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4031 continue                                                          
      ivtnum = 403                                                      
!                                                                        
!       ****  TEST 403  ****                                             
!                                                                        
      if (iczero) 34030, 4030, 34030                                    
 4030 continue                                                          
      ivcn01 = 30                                                       
      ivcn02 = 3                                                        
      call fs052(ivcn01,ivcn02,rvdn01)

      ivcomp = ivcn01                                                   
      goto 44030                                                       
34030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44030, 4041, 44030                                    
44030 if (ivcomp - 33) 24030,14030,24030                                
14030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4041                                                        
24030 ivfail = ivfail + 1                                               
      ivcorr = 33                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4041 continue                                                          
!                                                                        
!      TEST 404 THROUGH TEST 406 TEST THE CALL TO SUBROUTINE FS053 WHICH 
!      CONTAINS SEVERAL ARGUMENTS AND SEVERAL RETURN STATEMENTS.         
!                                                                        
      ivtnum = 404                                                      
!                                                                        
!       ****  TEST 404  ****                                             
!                                                                        
      if (iczero) 34040, 4040, 34040                                    
 4040 continue                                                          
      call fs053(6,10,11,ivon04,1)

      ivcomp = ivon04                                                   
      goto 44040                                                       
34040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44040, 4051, 44040                                    
44040 if (ivcomp - 6) 24040,14040,24040                                 
14040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4051                                                        
24040 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4051 continue                                                          
      ivtnum = 405                                                      
!                                                                        
!       ****  TEST 405  ****                                             
!                                                                        
      if (iczero) 34050, 4050, 34050                                    
 4050 continue                                                          
      ivcn01 = 10                                                       
      call fs053(6,ivcn01,11,ivon04,2)

      ivcomp = ivon04                                                   
      goto 44050                                                       
34050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44050, 4061, 44050                                    
44050 if (ivcomp - 16) 24050,14050,24050                                
14050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4061                                                        
24050 ivfail = ivfail + 1                                               
      ivcorr = 16                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4061 continue                                                          
      ivtnum = 406                                                      
!                                                                        
!       ****  TEST 406  ****                                             
!                                                                        
      if (iczero) 34060, 4060, 34060                                    
 4060 continue                                                          
      ivon01 = 6                                                        
      ivon02 = 10                                                       
      ivon03 = 11                                                       
      ivon05 = 3                                                        
      call fs053(ivon01,ivon02,ivon03,ivon04,ivon05)

      ivcomp = ivon04                                                   
      goto 44060                                                       
34060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44060, 4071, 44060                                    
44060 if (ivcomp - 27) 24060,14060,24060                                
14060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4071                                                        
24060 ivfail = ivfail + 1                                               
      ivcorr = 27                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4071 continue                                                          
!                                                                        
!      TEST 407 THROUGH 409 TEST THE REFERENCE TO FUNCTION FF054 WHICH   
!      CONTAINS SEVERAL ARGUMENTS AND SEVERAL RETURN STATEMENTS          
!                                                                        
      ivtnum = 407                                                      
!                                                                        
!       ****  TEST 407  ****                                             
!                                                                        
      if (iczero) 34070, 4070, 34070                                    
 4070 continue                                                          
      ivcomp  = ff054(300,1,21,1)
      goto 44070                                                       
34070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44070, 4081, 44070                                    
44070 if (ivcomp - 300) 24070,14070,24070                               
14070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4081                                                        
24070 ivfail = ivfail + 1                                               
      ivcorr = 300                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4081 continue                                                          
      ivtnum = 408                                                      
!                                                                        
!       ****  TEST 408  ****                                             
!                                                                        
      if (iczero) 34080, 4080, 34080                                    
 4080 continue                                                          
      ivon01 = 300                                                      
      ivon04 = 2                                                        
      ivcomp  = ff054(ivon01,77,5,ivon04)
      goto 44080                                                       
34080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44080, 4091, 44080                                    
44080 if (ivcomp - 377) 24080,14080,24080                               
14080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4091                                                        
24080 ivfail = ivfail + 1                                               
      ivcorr = 377                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4091 continue                                                          
      ivtnum = 409                                                      
!                                                                        
!       ****  TEST 409  ****                                             
!                                                                        
      if (iczero) 34090, 4090, 34090                                    
 4090 continue                                                          
      ivon01 = 71                                                       
      ivon02 = 21                                                       
      ivon03 = 17                                                       
      ivon04 = 3                                                        
      ivcomp  = ff054(ivon01,ivon02,ivon03,ivon04)
      goto 44090                                                       
34090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44090, 4101, 44090                                    
44090 if (ivcomp - 109) 24090,14090,24090                               
14090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4101                                                        
24090 ivfail = ivfail + 1                                               
      ivcorr = 109                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4101 continue                                                          
!                                                                        
!      TEST 410 THROUGH 429 TEST THE CALL TO SUBROUTINE FS055 WHICH      
!      CONTAINS NO ARGUMENTS.  THE PARAMETERS ARE PASSED THROUGH AN      
!      INTEGER ARRAY VARIABLE IN UNLABELED COMMON.                       
!                                                                        
      call fs055(iacn11,ivcn01,ivcn02,rvcn01)

      do i = 1,20                                                    
      if (iczero) 34100, 4100, 34100                                    
 4100 continue                                                          
      ivtnum = 409 + i                                                  
      ivcomp = iacn11(i)                                                
      goto 44100                                                         !Break
34100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44100, 4111, 44100                                    
44100 if (ivcomp - i) 24100,14100,24100                                 
14100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4111                                                          !Break
24100 ivfail = ivfail + 1                                               
      ivcorr = i                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4111 continue                                                          
  end do
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
90007 format (" ",20x,"END OF PROGRAM FM050" )                          
      end program fm050
      subroutine fs051(rvcn01)
      real, intent(InOut) :: rvcn01
      rvcn01 = rvcn01 + 1.0                                             
      return                                                            
      end subroutine fs051
      subroutine fs052(ivcn01,ivcn02,rvdn01)
      integer, intent(InOut) :: ivcn01
      integer, intent(In) :: ivcn02
      real :: rvdn01
      go to (10,20,30,40,50),ivcn02                                     
10    ivcn01 = ivcn01 + 1                                               
      return                                                            
20    ivcn01 = ivcn01 + 2                                               
      return                                                            
30    ivcn01 = ivcn01 + 3                                               
      return                                                            
40    ivcn01 = ivcn01 + 4                                               
      return                                                            
50    ivcn01 = ivcn01 + 5                                               
      return                                                            
      end subroutine fs052
      subroutine fs053(ivon01,ivon02,ivon03,ivon04,ivon05)
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      go to (10,20,30),ivon05                                           
10    ivon04 = ivon01                                                   
      return                                                            
20    ivon04 = ivon01 + ivon02                                          
      return                                                            
30    ivon04 = ivon01 + ivon02 + ivon03                                 
      return                                                            
      end subroutine fs053
      integer function ff054(ivon01,ivon02,ivon03,ivon04)
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      go to (10,20,30),ivon04                                           
10    ff054 = ivon01                                                    
      return                                                            
20    ff054 = ivon01 + ivon02                                           
      return                                                            
30    ff054 = ivon01 + ivon02 + ivon03                                  
      return                                                            
      end function ff054
      subroutine fs055(iacn11,ivcn01,ivcn02,rvcn01)
      integer :: ivcn01
      integer :: ivcn02
      real :: rvcn01
      integer :: i
      integer, dimension(1:20), intent(Out) :: iacn11
      do i = 1,20                                                    
      iacn11(i) = i                                                     
       end do
      return                                                            
      end subroutine fs055
