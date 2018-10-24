      program fm013
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: icon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
      integer :: l
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM013                                                             
!                                                                        
!              THIS ROUTINE TESTS THE FORTRAN  ASSIGNED GO TO STATEMENT  
!      AS DESCRIBED IN SECTION 11.3 (ASSIGNED GO TO STATEMENT). FIRST A  
!      STATEMENT LABEL IS ASSIGNED TO AN INTEGER VARIABLE IN THE ASSIGN  
!      STATEMENT.  SECONDLY A BRANCH IS MADE IN AN ASSIGNED GO TO        
!      STATEMENT USING THE INTEGER VARIABLE AS THE BRANCH CONTROLLER     
!      IN A LIST OF POSSIBLE STATEMENT NUMBERS TO BE BRANCHED TO.        
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 10.3, STATEMENT LABEL ASSIGNMENT (ASSIGN) STATEMENT    
!         SECTION 11.3, ASSIGNED GO TO STATEMENT                         
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
      ivtnum = 126                                                      
!                                                                        
!      TEST 126  -  THIS TESTS THE SIMPLE ASSIGN STATEMENT IN PREPARATION
!            FOR THE ASSIGNED GO TO TEST TO FOLLOW.                      
!            THE ASSIGNED GO TO IS THE SIMPLIST FORM OF THE STATEMENT.   
!                                                                        
!                                                                        
      if (iczero) 31260, 1260, 31260                                    
 1260 continue                                                          
      assign 1263 to i                                                  
      go to i, (1262,1263,1264)                                         
 1262 icon01 = 1262                                                     
      goto 1265                                                        
 1263 icon01 = 1263                                                     
      goto 1265                                                        
 1264 icon01 = 1264                                                     
 1265 continue                                                          
      goto 41260                                                       
31260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41260, 1271, 41260                                    
41260 if ( icon01 - 1263 )  21260, 11260, 21260                         
11260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1271                                                        
21260 ivfail = ivfail + 1                                               
      ivcomp=icon01                                                     
      ivcorr = 1263                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1271 continue                                                          
      ivtnum = 127                                                      
!                                                                        
!      TEST 127  -  THIS IS A TEST OF MORE COMPLEX BRANCHING USING       
!            THE ASSIGN AND ASSIGNED GO TO STATEMENTS.  THIS TEST IS NOT 
!            INTENDED TO BE AN EXAMPLE OF STRUCTURED PROGRAMMING.        
!                                                                        
!                                                                        
      if (iczero) 31270, 1270, 31270                                    
 1270 continue                                                          
      ivon01=0                                                          
 1272 assign 1273 to j                                                  
      ivon01=ivon01+1                                                   
      goto 1276                                                        
 1273 assign 1274 to j                                                  
      ivon01=ivon01 * 10 + 2                                            
      goto 1276                                                        
 1274 assign 1275 to j                                                  
      ivon01=ivon01 * 100 + 3                                           
      goto 1276                                                        
 1275 goto 1277                                                        
 1276 go to j, ( 1272, 1273, 1274, 1275 )                               
 1277 continue                                                          
      goto 41270                                                       
31270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41270, 1281, 41270                                    
41270 if ( ivon01 - 1203 )  21270, 11270, 21270                         
11270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1281                                                        
21270 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1203                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1281 continue                                                          
      ivtnum = 128                                                      
!                                                                        
!      TEST 128  -  TEST OF THE ASSIGNED GO TO WITH ALL OF THE           
!            STATEMENT NUMBERS IN THE ASSIGNED GO TO LIST THE SAME       
!            VALUE EXCEPT FOR ONE.                                       
!                                                                        
!                                                                        
      if (iczero) 31280, 1280, 31280                                    
 1280 continue                                                          
      icon01=0                                                          
      assign 1283 to k                                                  
      go to k, ( 1282, 1282, 1282, 1282, 1282, 1282, 1283 )             
 1282 icon01 = 0                                                        
      goto 1284                                                        
 1283 icon01 = 1                                                        
 1284 continue                                                          
      goto 41280                                                       
31280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41280, 1291, 41280                                    
41280 if ( icon01 - 1 )  21280, 11280, 21280                            
11280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1291                                                        
21280 ivfail = ivfail + 1                                               
      ivcomp=icon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1291 continue                                                          
      ivtnum = 129                                                      
!                                                                        
!      TEST 129  -  THIS TESTS THE ASSIGN STATEMENT IN CONJUNCTION       
!            WITH THE NORMAL ARITHMETIC ASSIGN STATEMENT.  THE VALUE     
!            OF THE INDEX FOR THE ASSIGNED GO TO STATEMENT IS CHANGED BY 
!            THE COMBINATION OF STATEMENTS.                              
!                                                                        
!                                                                        
      if (iczero) 31290, 1290, 31290                                    
 1290 continue                                                          
      icon01=0                                                          
      assign 1292 to l                                                  
      l = 1293                                                          
      assign 1294 to l                                                  
      go to l, ( 1294, 1293, 1292 )                                     
 1292 icon01 = 0                                                        
      goto 1295                                                        
 1293 icon01 = 0                                                        
      goto 1295                                                        
 1294 icon01 = 1                                                        
 1295 continue                                                          
      goto 41290                                                       
31290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41290, 1301, 41290                                    
41290 if ( icon01 - 1 )  21290, 11290, 21290                            
11290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1301                                                        
21290 ivfail = ivfail + 1                                               
      ivcomp=icon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1301 continue                                                          
      ivtnum = 130                                                      
!                                                                        
!      TEST 130  -  THIS IS A TEST OF A LOOP USING A COMBINATION OF THE  
!            ASSIGNED GO TO STATEMENT AND THE ARITHMETIC IF STATEMENT.   
!            THE LOOP SHOULD BE EXECUTED ELEVEN (11) TIMES THEN CONTROL  
!            SHOULD PASS TO THE CHECK OF THE VALUE FOR IVON01.           
!                                                                        
!                                                                        
      if (iczero) 31300, 1300, 31300                                    
 1300 continue                                                          
      ivon01=0                                                          
 1302 assign 1302 to m                                                  
      ivon01=ivon01+1                                                   
      if ( ivon01 - 10 )  1303, 1303, 1304                              
 1303 goto 1305                                                        
 1304 assign 1306 to m                                                  
 1305 go to m, ( 1302, 1306 )                                           
 1306 continue                                                          
      goto 41300                                                       
31300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41300, 1311, 41300                                    
41300 if ( ivon01 - 11 )  21300, 11300, 21300                           
11300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1311                                                        
21300 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=11                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1311 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM013" )                          
      end program fm013
