      program fm003
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivon02
      integer :: ivcomp
      integer :: ivcorr
      integer :: icone
      integer :: icon02
!      COMMENT SECTION                                                   
!                                                                        
!      FM003                                                             
!                                                                        
!          THIS ROUTINE CONTAINS THE BASIC CONTINUE TESTS.  THESE TESTS  
!      ENSURE THAT EXECUTION OF A CONTINUE STATEMENT CAUSES CONTINUATION 
!      OF THE NORMAL PROGRAM EXECUTION SEQUENCE.  ONLY THE STATEMENTS IN 
!      THE BASIC ASSUMPTIONS ARE INCLUDED IN THESE TESTS.  OTHER CONTINUE
!      TESTS ARE CONTAINED IN OTHER ROUTINES AS PART OF THE TESTS FOR    
!      OTHER LANGUAGE FEATURES SUCH AS THE DO STATEMENTS TESTS.          
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 3.6, NORMAL EXECUTION SEQUENCE AND TRANSFER OF CONTROL 
!         SECTION 11.11, CONTINUE STATEMENT                              
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
  131 continue                                                          
      ivtnum =  13                                                      
!                                                                        
!       ****  TEST 013  ****                                             
!          TEST 13 - CONTINUE TEST                                       
!                CONTINUE STATEMENT FOLLOWING INTEGER ASSIGNMENT         
!                STATEMENTS.                                             
!                                                                        
      if (iczero) 30130,  130, 30130                                    
  130 continue                                                          
      ivon01=5                                                          
      ivon02=6                                                          
      continue                                                          
      goto 40130                                                       
30130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40130,  141, 40130                                    
40130 if (ivon01-5) 20131,40131,20131                                   
40131 if (ivon02-6) 20132,10130,20132                                   
10130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  141                                                        
20131 ivcomp=ivon01                                                     
      ivcorr=5                                                          
      goto 20130                                                       
20132 ivcomp=ivon02                                                     
      ivcorr=6                                                          
20130 ivfail = ivfail + 1                                               
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  141 continue                                                          
      ivtnum =  14                                                      
!                                                                        
!       ****  TEST 014  ****                                             
!          TEST 14 - CONTINUE TEST                                       
!                CONTINUE STATEMENT BETWEEN INTEGER ASSIGNMENT           
!                STATEMENTS                                              
!                                                                        
      if (iczero) 30140,  140, 30140                                    
  140 continue                                                          
      ivon01=14                                                         
      continue                                                          
      ivon02=15                                                         
      goto 40140                                                       
30140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40140,  151, 40140                                    
40140 if (ivon01 - 14) 20141,40141,20141                                
40141 if (ivon02 - 15) 20142, 10140, 20142                              
10140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  151                                                        
20141 ivcomp=ivon01                                                     
      ivcorr=14                                                         
      goto 20140                                                       
20142 ivcomp=ivon02                                                     
      ivcorr=15                                                         
20140 ivfail = ivfail + 1                                               
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  151 continue                                                          
      ivtnum =  15                                                      
!                                                                        
!       ****  TEST 015  ****                                             
!          TEST 15 - CONTINUE TEST                                       
!                TWO CONSECUTIVE CONTINUE STATEMENTS                     
!                                                                        
      if (iczero) 30150,  150, 30150                                    
  150 continue                                                          
      continue                                                          
      ivon01=19                                                         
      ivon02=20                                                         
      goto 40150                                                       
30150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40150,  161, 40150                                    
40150 if (ivon01 - 19) 20151,40151,20151                                
40151 if (ivon02 -20) 20152,10150,20152                                 
10150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  161                                                        
20151 ivcomp=ivon01                                                     
      ivcorr=19                                                         
      goto 20150                                                       
20152 ivcomp=ivon02                                                     
      ivcorr=20                                                         
20150 ivfail = ivfail + 1                                               
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  161 continue                                                          
      ivtnum =  16                                                      
!                                                                        
!       ****  TEST 016  ****                                             
!          TEST 16 - CONTINUE TEST                                       
!                BRANCH TO CONTINUE STATEMENT FROM IF STATEMENT          
!                                                                        
      if (iczero) 30160,  160, 30160                                    
  160 continue                                                          
      ivon01=16                                                         
      if (ivon01 - 16) 162,163,162                                      
  162 ivcorr=16                                                         
      goto 20160                                                       
  163 continue                                                          
      ivon01=160                                                        
      goto 40160                                                       
30160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40160,  171, 40160                                    
40160 if (ivon01-160) 20161,10160,20161                                 
10160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  171                                                        
20161 ivcorr=160                                                        
20160 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  171 continue                                                          
      ivtnum =  17                                                      
!                                                                        
!       ****  TEST 017  ****                                             
!          TEST 17 - CONTINUE TEST                                       
!                TWO OF THE BRANCHES OF AN IF STATEMENT ARE TO THE SAME  
!                CONTINUE STATEMENT.  THE THIRD BRANCH ALSO IS MADE TO   
!                A CONTINUE STATEMENT.                                   
!                                                                        
      if (iczero) 30170,  170, 30170                                    
  170 continue                                                          
      ivon01=17                                                         
      if (ivon01-19) 173,172,172                                        
  172 continue                                                          
      ivcorr=17                                                         
      goto 20170                                                       
  173 continue                                                          
      ivon01=170                                                        
      goto 40170                                                       
30170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40170,  181, 40170                                    
40170 if (ivon01 - 170) 20171,10170,20171                               
10170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  181                                                        
20171 ivcorr=170                                                        
20170 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  181 continue                                                          
      ivtnum =  18                                                      
!                                                                        
!       ****  TEST 018  ****                                             
!          TEST 18 - CONTINUE TEST                                       
!                BRANCH TO CONTINUE STATEMENT FROM GO TO STATEMENT       
!                                                                        
      if (iczero) 30180,  180, 30180                                    
  180 continue                                                          
      if (iczero) 184,182,184                                           
  182 ivon01=18                                                         
      goto 183                                                         
  184 ivon01=20                                                         
  183 continue                                                          
      ivon02=180                                                        
      goto 40180                                                       
30180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40180,  191, 40180                                    
40180 if (ivon01 - 18) 20181,40181,20181                                
40181 if (ivon02 -180) 20182,10180,20182                                
10180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  191                                                        
20181 ivcorr=18                                                         
      ivcomp=ivon01                                                     
      goto 20180                                                       
20182 ivcomp=ivon02                                                     
      ivcorr=180                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  191 continue                                                          
      ivtnum =  19                                                      
!                                                                        
!       ****  TEST 019  ****                                             
!          TEST 19 - CONTINUE TEST                                       
!              BRANCH TO THREE  CONTINUE STATEMENTS  FROM IF STATEMENT.  
!                CONTINUE STATEMENTS FOLLOW EACH OTHER.                  
!                                                                        
      if (iczero) 30190,  190, 30190                                    
  190 continue                                                          
      icone = 1                                                         
      if (icone) 194,192,193                                            
  193 continue                                                          
  192 continue                                                          
  194 continue                                                          
      ivon01=19                                                         
      goto 40190                                                       
30190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40190,  201, 40190                                    
40190 if (ivon01 - 19) 20190,10190,20190                                
10190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  201                                                        
20190 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=19                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  201 continue                                                          
      ivtnum =  20                                                      
!                                                                        
!       ****  TEST 020  ****                                             
!          TEST 20 - CONTINUE TEST                                       
!                THREE SEPARATE BRANCHES OF AN IF STATEMENT ARE TO       
!                CONTINUE STATEMENTS.                                    
!                                                                        
      if (iczero) 30200,  200, 30200                                    
  200 continue                                                          
      icon02=-2                                                         
      if  (icon02) 204,202,203                                          
  203 continue                                                          
      ivon01=203                                                        
      goto 40200                                                       
  204 continue                                                          
      ivon01 = 204                                                      
      goto 40200                                                       
  202 continue                                                          
      ivon01=202                                                        
      goto 40200                                                       
30200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40200,  211, 40200                                    
40200 if (ivon01 - 204) 20200,10200,20200                               
10200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  211                                                        
20200 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=204                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  211 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM003" )                          
      end program fm003
