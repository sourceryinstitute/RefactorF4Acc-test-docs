      program fm014
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: icon01
      integer :: i
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
      integer :: j
      integer :: k
      integer :: l
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM014                                                             
!                                                                        
!              THIS ROUTINE TESTS THE FORTRAN   COMPUTED GO TO STATEMENT.
!      BECAUSE THE FORM OF THE COMPUTED GO TO IS SO STRAIGHTFORWARD, THE 
!      TESTS MAINLY RELATE TO THE RANGE OF POSSIBLE STATEMENT NUMBERS    
!      WHICH ARE USED.                                                   
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 11.2, COMPUTED GO TO STATEMENT                         
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
      ivtnum = 131                                                      
!                                                                        
!      TEST 131  -  TEST OF THE SIMPLIST FORM OF THE COMPUTED GO TO      
!            STATEMENT WITH THREE POSSIBLE BRANCHES.                     
!                                                                        
!                                                                        
      if (iczero) 31310, 1310, 31310                                    
 1310 continue                                                          
      icon01=0                                                          
      i=3                                                               
      go to ( 1312, 1313, 1314 ), i                                     
 1312 icon01 = 1312                                                     
      goto 1315                                                        
 1313 icon01 = 1313                                                     
      goto 1315                                                        
 1314 icon01 = 1314                                                     
 1315 continue                                                          
      goto 41310                                                       
31310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41310, 1321, 41310                                    
41310 if ( icon01 - 1314 )  21310, 11310, 21310                         
11310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1321                                                        
21310 ivfail = ivfail + 1                                               
      ivcomp=icon01                                                     
      ivcorr = 1314                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1321 continue                                                          
      ivtnum = 132                                                      
!                                                                        
!      TEST 132  -  THIS TESTS THE COMPUTED GO TO IN CONJUNCTION WITH THE
!            THE UNCONDITIONAL GO TO STATEMENT.  THIS TEST IS NOT        
!            INTENDED TO BE AN EXAMPLE OF GOOD STRUCTURED PROGRAMMING.   
!                                                                        
!                                                                        
      if (iczero) 31320, 1320, 31320                                    
 1320 continue                                                          
      ivon01=0                                                          
      j=1                                                               
      goto 1326                                                        
 1322 j = 2                                                             
      ivon01=ivon01+2                                                   
      goto 1326                                                        
 1323 j = 3                                                             
      ivon01=ivon01 * 10 + 3                                            
      goto 1326                                                        
 1324 j = 4                                                             
      ivon01=ivon01 * 100 + 4                                           
      goto 1326                                                        
 1325 ivon01 = ivon01 + 1                                               
      goto 1327                                                        
 1326 go to ( 1322, 1323, 1324, 1325, 1326 ), j                         
 1327 continue                                                          
      goto 41320                                                       
31320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41320, 1331, 41320                                    
41320 if ( ivon01 - 2305 )  21320, 11320, 21320                         
11320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1331                                                        
21320 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=2305                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1331 continue                                                          
      ivtnum = 133                                                      
!                                                                        
!      TEST 133  -  THIS IS A TEST OF THE COMPUTED GO TO STATEMENT WITH  
!            A SINGLE STATEMENT LABEL AS THE LIST OF POSSIBLE BRANCHES.  
!                                                                        
!                                                                        
      if (iczero) 31330, 1330, 31330                                    
 1330 continue                                                          
      ivon01=0                                                          
      k=1                                                               
      go to ( 1332 ), k                                                 
 1332 ivon01 = 1                                                        
      goto 41330                                                       
31330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41330, 1341, 41330                                    
41330 if ( ivon01 - 1 )  21330, 11330, 21330                            
11330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1341                                                        
21330 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1341 continue                                                          
      ivtnum = 134                                                      
!                                                                        
!      TEST 134  -  THIS IS A TEST OF FIVE (5) DIGIT STATEMENT NUMBERS   
!            WHICH EXCEED THE INTEGER 32767 USED IN THE COMPUTED GO TO   
!            STATEMENT WITH THREE POSSIBLE BRANCHES.                     
!                                                                        
!                                                                        
      if (iczero) 31340, 1340, 31340                                    
 1340 continue                                                          
      ivon01=0                                                          
      l=2                                                               
      go to ( 99991, 99992, 99993 ), l                                  
99991 ivon01=1                                                          
      goto 1342                                                        
99992 ivon01=2                                                          
      goto 1342                                                        
99993 ivon01=3                                                          
 1342 continue                                                          
      goto 41340                                                       
31340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41340, 1351, 41340                                    
41340 if ( ivon01 - 2 )  21340, 11340, 21340                            
11340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1351                                                        
21340 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=2                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1351 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM014" )                          
      end program fm014
