      program fm091
!       COMMENT SECTION.                                                 
!                                                                        
!       FM091  WV MODIFIED FROM FM011, remove space to make sane         
!                                                                        
!      THIS ROUTINE IS A TEST OF BLANK CHARACTERS (SECTION 3.1.6)        
!          WHICH SHOULD HAVE NO MEANING WHEN EMBEDDED IN FORTRAN RESERVED
!          WORDS.                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 3.1.6, BLANK CHARACTER                                 
      integer, dimension(1:3) :: iace11
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
      real :: rvon01
      integer :: ivon02
      real :: rvon03
      integer :: ivon03
      integer :: ivon04
      integer, dimension(1:3) :: iadn11
      integer, dimension(1:3) :: iadn12
      integer :: rvtni1
      real :: ivtnr1
      logical :: lvtnl1
      logical :: lvtnl2
      equivalence  (iace11(1),iadn11(1))                                
      data iadn12 / 3*3 / 
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
      ivtnum = 103                                                      
!                                                                        
!       ****  TEST  103  ****                                            
!      TEST 103  -  THIS TEST HAS BLANKS EMBEDDED IN A DIMENSION         
!            STATEMENT.  ALSO THE DO STATEMENT WITH AN EMBEDDED BLANK    
!            WILL BE TESTED TO INITIALIZE VALUES IN AN ARRAY.  THE       
!            CONTINUE AND IF STATEMENTS HAVE EMBEDDED BLANKS AS WELL.    
!                                                                        
      if (iczero) 31030, 1030, 31030                                    
 1030 continue                                                          
      do ivon01 =1 , 3 ,  1                                         
      iadn11(ivon01) = ivon01                                           
     end do
      goto 41030                                                       
31030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41030, 1041, 41030                                    
41030 if  (iadn11(2) - 2)  21030,11030,21030                            
11030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1041                                                        
21030 ivfail = ivfail + 1                                               
      ivcomp = iadn11(2)                                                
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1041 continue                                                          
      ivtnum = 104                                                      
!                                                                        
!       ****  TEST  104  ****                                            
!      TEST 104  -  THIS TESTS EMBEDDED BLANKS IN AN INTEGER TYPE        
!            STATEMENT.  FRACTION 1/2 SHOULD BECOME 0 AS AN INTEGER.     
!            INTEGER TO REAL * 2. BACK TO INTEGER CONVERSION SHOULD BE 0.
!                                                                        
      if (iczero) 31040, 1040, 31040                                    
 1040 continue                                                          
      rvtni1 = 2                                                        
      rvon01 = 1/rvtni1                                                 
      ivon02 = rvon01 * 2.                                              
      goto 41040                                                       
31040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41040, 1051, 41040                                    
41040 if( ivon02 - 0 ) 21040,11040,21040                                
11040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1051                                                        
21040 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1051 continue                                                          
      ivtnum = 105                                                      
!                                                                        
!       ****  TEST  105  ****                                            
!      TEST 105  -  TEST OF EMBEDDED BLANKS IN A REAL TYPE STATEMENT.    
!            REAL TO REAL*2. TO INTEGER CONVERSION IS PERFORMED.  RESULT 
!            IS 1 IF THE TYPE OF THE TEST VARIABLE(IVTNR1) WAS REAL.     
!                                                                        
      if (iczero) 31050, 1050, 31050                                    
 1050 continue                                                          
      ivtnr1 = .5                                                       
      rvon03 = ivtnr1*2.                                                
      ivon03 = rvon03 +.3                                               
      goto 41050                                                       
31050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41050, 1061, 41050                                    
41050 if(ivon03 - 1) 21050,  11050, 21050                               
11050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1061                                                        
21050 ivfail = ivfail + 1                                               
      ivcomp = ivon03                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1061 continue                                                          
      ivtnum = 106                                                      
!                                                                        
!       ****  TEST  106  ****                                            
!      TEST 106  -  TEST THE LOGICAL TYPE WITH EMBEDDED BLANKS BY A      
!            LOGIC ASSIGNMENT (V = .TRUE.) SECTION 4.7.1 AND 10.2        
!                                                                        
      if (iczero) 31060, 1060, 31060                                    
 1060 continue                                                          
      lvtnl1 = .true.                                                   
      goto 41060                                                       
31060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41060, 1071, 41060                                    
41060 if(iczero) 21060,11060,21060                                      
11060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1071                                                        
21060 ivfail = ivfail + 1                                               
      write (i02,80002) ivtnum, ivcomp ,ivcorr                          
 1071 continue                                                          
      ivtnum = 107                                                      
!                                                                        
!       ****  TEST  107  ****                                            
!      TEST 107  -  A SECOND TEST OF THE LOGICAL TYPE STATEMENT WITH     
!            EMBEDDED BLANKS.  THE TEST IS AGAIN MADE BY A LOGICAL       
!            ASSIGNMENT (SECTION 4.7.1 AND 10.2).                        
!                                                                        
      if (iczero) 31070, 1070, 31070                                    
 1070 continue                                                          
      lvtnl2 = .false.                                                  
      goto 41070                                                       
31070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41070, 1081, 41070                                    
41070 if(iczero) 21070,11070,21070                                      
11070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1081                                                        
21070 ivfail = ivfail + 1                                               
      write (i02,80002) ivtnum, ivcomp ,ivcorr                          
 1081 continue                                                          
      ivtnum = 108                                                      
!                                                                        
!       ****  TEST  108  ****                                            
!      TEST 108  -  THIS IS A TEST OF BLANKS EMBEDDED IN THE COMMON,     
!            DIMENSION AND EQUIVALENCE STATEMENTS (SECTION 8.1,          
!            8.3. AND 8.2.).                                             
!                                                                        
      if (iczero) 31080, 1080, 31080                                    
 1080 continue                                                          
      iadn11(3) = 4                                                     
      goto 41080                                                       
31080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41080, 1091, 41080                                    
41080 if(iace11(3) - 4)  21080,11080,21080                              
11080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1091                                                        
21080 ivfail = ivfail + 1                                               
      ivcomp = iace11(3)                                                
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1091 continue                                                          
      ivtnum = 109                                                      
!                                                                        
!       ****  TEST  109  ****                                            
!      TEST 109  -  THIS TESTS THE EFFECT OF BLANKS EMBEDDED IN THE      
!            DATA STATEMENT BY CHECKING THE INITIALIZATION OF ARRAY      
!            ELEMENT VALUES (SECTION 9).                                 
!                                                                        
      if (iczero) 31090, 1090, 31090                                    
 1090 continue                                                          
      ivon04    = iadn12(1) + iadn12(2) + iadn12(3)                     
      goto 41090                                                       
31090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41090, 1101, 41090                                    
41090 if(ivon04 - 9) 21090,11090,21090                                  
11090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1101                                                        
21090 ivfail = ivfail + 1                                               
      ivcomp = ivon04                                                   
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1101 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM091" )                          
      end program fm091
