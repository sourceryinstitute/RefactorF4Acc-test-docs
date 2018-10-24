      program fm002
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
!      COMMENT SECTION                                                   
!                                                                        
!      FM002                                                             
!                                                                        
!          THIS ROUTINE CHECKS THAT COMMENT LINES WHICH HAVE VALID       
!      FORTRAN STATEMENTS DO NOT AFFECT THE EXECUTION OF THE PROGRAM     
!      IN ANY WAY.                                                       
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!                    SECTION 3.2.1, COMMENT LINE                         
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
   41 continue                                                          
      ivtnum=4                                                          
!                                                                        
!       ****  TEST 004  ****                                             
!      TEST 004  -  BLANK COMMENT LINE                                   
!                                                                        
      if (iczero) 30040,40,30040                                        
   40 continue                                                          
      ivon01=4                                                          
!                                                                        
      goto 40040                                                       
30040 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40040, 51, 40040                                      
40040 if (ivon01 - 4) 20040, 10040, 20040                               
10040 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 51                                                          
20040 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=4                                                          
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
   51 continue                                                          
      ivtnum=5                                                          
!                                                                        
!       ****  TEST 005  ****                                             
!      TEST 005  - GO TO IN COMMENT LINE                                 
!                                                                        
      if (iczero) 30050, 50, 30050                                      
   50 continue                                                          
      ivon01 = 3                                                        
!      GO TO 20050                                                       
      ivon01=5                                                          
      goto 40050                                                       
30050 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40050, 61, 40050                                      
40050 if (ivon01 - 5) 20050,10050,20050                                 
10050 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 61                                                          
20050 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=5                                                          
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
   61 continue                                                          
      ivtnum=6                                                          
!                                                                        
!       ****  TEST 006  ****                                             
!      TEST 006 - INTEGER ASSIGNMENT STATEMENT IN COMMENT LINE           
!                                                                        
      if (iczero) 30060,60,30060                                        
   60 continue                                                          
      ivon01=6                                                          
!      IVON01=1                                                          
      goto 40060                                                       
30060 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40060,71,40060                                        
40060 if (ivon01-6) 20060,10060,20060                                   
10060 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 71                                                          
20060 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=6                                                          
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
   71 continue                                                          
      ivtnum=7                                                          
!                                                                        
!       ****  TEST  007  ****                                            
!      TEST 007 - INTEGER ASSIGNMENT STATEMENT IN COMMENT LINE           
!                 INTEGER EXPRESSION TO RIGHT OF =                       
!                                                                        
      if (iczero) 30070,70,30070                                        
   70 continue                                                          
      ivon02=6                                                          
      ivon01=7                                                          
!      IVON01= 3*IVON02                                                  
      goto 40070                                                       
30070 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40070,81,40070                                        
40070 if (ivon01-7) 20070,10070,20070                                   
10070 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 81                                                          
20070 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=7                                                          
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
   81 continue                                                          
      ivtnum=8                                                          
!                                                                        
!       ****  TEST 008  ****                                             
!      TEST 008 - IF STATEMENT IN COMMENT LINE                           
!                                                                        
      if (iczero) 30080,80,30080                                        
   80 continue                                                          
      ivon01=300                                                        
!      IF (IVON01) 20080,20080,20080                                     
      ivon01=8                                                          
      goto 40080                                                       
30080 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40080,91,40080                                        
40080 if (ivon01-8) 20080,10080,20080                                   
10080 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 91                                                          
20080 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=8                                                          
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
   91 continue                                                          
      ivtnum=9                                                          
!                                                                        
!       ****  TEST 009  ****                                             
!      TEST 009 - WRITE STATEMENT IN A COMMENT LINE                      
!                                                                        
      if (iczero) 30090,90,30090                                        
   90 continue                                                          
      ivon01=200                                                        
!   92 WRITE (I02,80002)  IVTNUM                                         
      ivon01=9                                                          
      goto 40090                                                       
30090 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40090,101,40090                                       
40090 if (ivon01-9) 20090,10090,20090                                   
10090 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 101                                                         
20090 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=9                                                          
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
  101 ivtnum=10                                                         
!                                                                        
!       ****  TEST 010  ****                                             
!      TEST 010 - STATEMENT LABEL IN COMMENT LINE                        
!                                                                        
      if (iczero) 30100,100,30100                                       
  100 continue                                                          
      goto 102                                                         
!  102 WRITE (I02,80002)                                                 
!      GO TO 111                                                         
  102 ivon01=10                                                         
      goto 40100                                                       
30100 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40100,111,40100                                       
40100 if (ivon01-10) 20100,10100,20100                                  
10100 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 111                                                         
20100 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=10                                                         
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
  111 continue                                                          
      ivtnum=11                                                         
!                                                                        
!       ****  TEST 011  ****                                             
!      TEST 011 - CONTINUE IN COMMENT LINE                               
!                 FOLLOWED BY INTEGER ASSIGNMENT STATEMENT IN COMMENT    
!                                                                        
      if (iczero) 30110,110,30110                                       
  110 ivon01=11                                                         
!      CONTINUE                                                          
!      IVON01=7000                                                       
      goto 40110                                                       
30110 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40110,121,40110                                       
40110 if (ivon01 -11) 20110,10110,20110                                 
10110 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 121                                                         
20110 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=11                                                         
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
  121 continue                                                          
      ivtnum=12                                                         
!                                                                        
!       ****  TEST 012  ****                                             
!      TEST 012 - INTEGER ASSIGNMENT STATEMENT IN COMMENT LINE           
!                                                                        
      if (iczero) 30120,120,30120                                       
  120 continue                                                          
      ivon01=12                                                         
!      IVON01=IVON01+1                                                   
      goto 40120                                                       
30120 ivdele=ivdele+1                                                   
      write (i02,80003) ivtnum                                          
      if (iczero) 40120,99999,40120                                     
40120 if (ivon01 - 12) 20120,10120,20120                                
10120 ivpass=ivpass+1                                                   
      write (i02,80001) ivtnum                                          
      goto 99999                                                       
20120 ivfail=ivfail+1                                                   
      ivcomp=ivon01                                                     
      ivcorr=12                                                         
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
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
90007 format (" ",20x,"END OF PROGRAM FM002" )                          
!      COMMENT LINE BEFORE END STATEMENT                                 
      end program fm002
