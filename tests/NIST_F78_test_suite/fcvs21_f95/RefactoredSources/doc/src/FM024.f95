      program fm024
!      COMMENT SECTION.                                                  
!                                                                        
!      FM024                                                             
!                                                                        
!                   THREE DIMENSIONED ARRAYS ARE USED IN THIS ROUTINE.   
!          THIS ROUTINE TESTS ARRAYS WITH FIXED DIMENSION AND SIZE LIMITS
!      SET EITHER IN A BLANK COMMON OR DIMENSION STATEMENT.  THE VALUES  
!      OF THE ARRAY ELEMENTS ARE SET IN VARIOUS WAYS SUCH AS SIMPLE      
!      ASSIGNMENT STATEMENTS, SET TO THE VALUES OF OTHER ARRAY ELEMENTS  
!      (EITHER POSITIVE OR NEGATIVE), SET BY INTEGER TO REAL OR REAL TO  
!      INTEGER CONVERSION, SET BY ARITHMETIC EXPRESSIONS, OR SET BY      
!      USE OF THE  EQUIVALENCE  STATEMENT.                               
!                                                                        
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8, SPECIFICATION STATEMENTS                            
!         SECTION 8.1, DIMENSION STATEMENT                               
!         SECTION 8.2, EQUIVALENCE STATEMENT                             
!         SECTION 8.3, COMMON STATEMENT                                  
!         SECTION 8.4, TYPE-STATEMENTS                                   
!         SECTION 9, DATA STATEMENT                                      
!                                                                        
!                                                                        
      integer, dimension(1:3,1:3,1:3) :: iade31
      integer, dimension(1:2,1:2,1:2) :: iadn31
      integer :: icoe01
      real, dimension(1:3,1:3,1:3) :: rade31
      real, dimension(1:2,1:2,1:2) :: radn31
      real :: rcoe01
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: icon01
      integer, dimension(1:3,1:3,1:3) :: iade32
      real, dimension(1:3,1:3,1:3) :: rade32
      logical, dimension(1:3,1:3,1:3) :: lade32
      integer, dimension(1:2,1:2,1:2) :: iadn32
      integer, dimension(1:2,1:2) :: iadn21
      integer, dimension(1:2) :: iadn11
      integer, dimension(1:2,1:2) :: iade21
      integer, dimension(1:4) :: iade11
!                                                                        
      equivalence (iade31(1,1,1), iade32(1,1,1) )                       
      equivalence ( rade31(1,1,1), rade32(1,1,1) )                      
      equivalence ( lade31(1,1,1), lade32(1,1,1) )                      
      equivalence ( iade31(1,1,1), iade21(1,1), iade11(1) )             
      equivalence ( icoe01, icoe02, icoe03 )                            
!                                                                        
      logical, dimension(1:3,1:3,1:3) :: lade31
      logical, dimension(1:2,1:2,1:2) :: ladn31
      logical :: lcoe01
      integer, dimension(1:2,1:2,1:2) :: radn33
      integer, dimension(1:2,1:4) :: radn21
      integer, dimension(1:8) :: radn11
      real, dimension(1:2,1:2,1:2) :: iadn33
      real, dimension(1:2,1:4) :: iadn22
      real, dimension(1:8) :: iadn12
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
      ivtnum = 645                                                      
!                                                                        
!       ****  TEST 645  ****                                             
!      TEST 645  -  TESTS SETTING A THREE DIMENSION INTEGER ARRAY ELEMENT
!      BY A SIMPLE INTEGER ASSIGNMENT STATEMENT.                         
!                                                                        
      if (iczero) 36450, 6450, 36450                                    
 6450 continue                                                          
      iadn31(2,2,2) = -9999                                             
      ivcomp = iadn31(2,2,2)                                            
      goto 46450                                                       
36450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46450, 6461, 46450                                    
46450 if ( ivcomp + 9999 )  26450, 16450, 26450                         
16450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6461                                                        
26450 ivfail = ivfail + 1                                               
      ivcorr = -9999                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6461 continue                                                          
      ivtnum = 646                                                      
!                                                                        
!       ****  TEST 646  ****                                             
!      TEST 646  -  TESTS SETTING A THREE DIMENSION REAL ARRAY ELEMENT   
!      BY A SIMPLE REAL ASSIGNMENT STATEMENT.                            
!                                                                        
      if (iczero) 36460, 6460, 36460                                    
 6460 continue                                                          
      radn31(1,2,1) = 512.                                              
      ivcomp = radn31(1,2,1)                                            
      goto 46460                                                       
36460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46460, 6471, 46460                                    
46460 if ( ivcomp - 512 )  26460, 16460, 26460                          
16460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6471                                                        
26460 ivfail = ivfail + 1                                               
      ivcorr = 512                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6471 continue                                                          
      ivtnum = 647                                                      
!                                                                        
!       ****  TEST 647  ****                                             
!      TEST 647  -  TESTS SETTING A THREE DIMENSION LOGICAL ARRAY ELEMENT
!      BY A SIMPLE LOGICAL ASSIGNMENT STATEMENT.                         
!                                                                        
      if (iczero) 36470, 6470, 36470                                    
 6470 continue                                                          
      ladn31(1,2,2) = .true.                                            
      icon01 = 0                                                        
      if ( ladn31(1,2,2) )  icon01 = 1                                  
      goto 46470                                                       
36470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46470, 6481, 46470                                    
46470 if ( icon01 - 1 )  26470, 16470, 26470                            
16470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6481                                                        
26470 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6481 continue                                                          
      ivtnum = 648                                                      
!                                                                        
!       ****  TEST 648  ****                                             
!      TEST 648  -  TESTS SETTING A ONE, TWO, AND THREE DIMENSION ARRAY  
!      ELEMENT TO A VALUE IN ARITHMETIC ASSIGNMENT STATEMENTS.  ALL THREE
!      ELEMENTS ARE INTEGERS.  THE INTEGER ARRAY ELEMENTS ARE THEN USED  
!      IN AN ARITHMETIC STATEMENT AND THE RESULT IS STORED BY INTEGER    
!      TO REAL CONVERSION INTO A THREE DIMENSION REAL ARRAY ELEMENT.     
!                                                                        
      if (iczero) 36480, 6480, 36480                                    
 6480 continue                                                          
      iadn11(2) = 1                                                     
      iadn21(2,2) = 2                                                   
      iadn32(2,2,2) = 3                                                 
      radn31(2,2,1) = iadn11(2) + iadn21(2,2) + iadn32(2,2,2)           
      ivcomp = radn31(2,2,1)                                            
      goto 46480                                                       
36480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46480, 6491, 46480                                    
46480 if ( ivcomp - 6) 26480, 16480, 26480                              
16480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6491                                                        
26480 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6491 continue                                                          
      ivtnum = 649                                                      
!                                                                        
!       ****  TEST 649  ****                                             
!      TEST 649  -  TESTS OF ONE, TWO, AND THREE DIMENSION ARRAY ELEMENTS
!      SET EXPLICITLY INTEGER BY THE INTEGER TYPE STATEMENT.  ALL ELEMENT
!      VALUES SHOULD BE ZERO FROM REAL TO INTEGER TRUNCATION FROM A VALUE
!      OF 0.5.  ALL THREE ELEMENTS ARE USED IN AN ARITHMETIC EXPRESSION. 
!      THE VALUE OF THE SUM OF THE ELEMENTS SHOULD BE ZERO.              
!                                                                        
      if (iczero) 36490, 6490, 36490                                    
 6490 continue                                                          
      radn11(8) = 0000.50000                                            
      radn21(2,4) = .50000                                              
      radn33(2,2,2) = 00000.5                                           
      radn11(1) = radn11(8) + radn21(2,4) + radn33(2,2,2)               
      ivcomp = radn11(1)                                                
      goto 46490                                                       
36490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46490, 6501, 46490                                    
46490 if ( ivcomp - 0 )  26490, 16490, 26490                            
16490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6501                                                        
26490 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6501 continue                                                          
      ivtnum = 650                                                      
!                                                                        
!       ****  TEST 650  ****                                             
!      TEST 650  -  TEST OF THE EQUIVALENCE STATEMENT.  A REAL ARRAY     
!      ELEMENT IS SET BY AN ASSIGNMENT STATEMENT.  ITS EQUIVALENT ELEMENT
!      IN COMMON IS USED TO SET THE VALUE OF AN INTEGER ARRAY ELEMENT    
!      ALSO IN COMMON.  FINALLY THE DIMENSIONED EQUIVALENT INTEGER       
!      ARRAY ELEMENT IS TESTED FOR THE VALUE USED THROUGHOUT  32767.     
!                                                                        
      if (iczero) 36500, 6500, 36500                                    
 6500 continue                                                          
      rade32(2,2,2) = 32767.                                            
      iade31(2,2,2) = rade31(2,2,2)                                     
      ivcomp = iade32(2,2,2)                                            
      goto 46500                                                       
36500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46500, 6511, 46500                                    
46500 if ( ivcomp - 32767 )  26500, 16500, 26500                        
16500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6511                                                        
26500 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6511 continue                                                          
      ivtnum = 651                                                      
!                                                                        
!       ****  TEST 651  ****                                             
!      TEST 651  -  THIS IS A TEST OF COMMON AND DIMENSION AS WELL AS A  
!      TEST OF THE EQUIVALENCE STATEMENT USING LOGICAL ARRAY ELEMENTS    
!      BOTH IN COMMON AND DIMENSIONED.  A LOGICAL VARIABLE IN COMMON IS  
!      SET TO A VALUE OF .NOT. THE VALUE USED IN THE EQUIVALENCED ARRAY  
!      ELEMENTS WHICH WERE SET IN A LOGICAL ASSIGNMENT STATEMENT.        
!                                                                        
      if (iczero) 36510, 6510, 36510                                    
 6510 continue                                                          
      lade31(1,2,3) = .false.                                           
      lcoe01 = .not. lade32(1,2,3)                                      
      icon01 = 0                                                        
      if ( lcoe01 )  icon01 = 1                                         
      goto 46510                                                       
36510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46510, 6521, 46510                                    
46510 if ( icon01 - 1 )  26510, 16510, 26510                            
16510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6521                                                        
26510 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6521 continue                                                          
      ivtnum = 652                                                      
!                                                                        
!       ****  TEST 652  ****                                             
!      TEST 652  -  TESTS OF ONE, TWO, AND THREE DIMENSION ARRAY ELEMENTS
!      SET EXPLICITLY REAL BY THE REAL TYPE STATEMENT.  ALL ELEMENT      
!      VALUES SHOULD BE 0.5 FROM THE REAL ASSIGNMENT STATEMENT.  THE     
!      ARRAY ELEMENTS ARE SUMMED AND THEN THE SUM MULTIPLIED BY 2.       
!      FINALLY 0.2 IS ADDED TO THE RESULT AND THE FINAL RESULT CONVERTED 
!      TO AN INTEGER  ( ( .5 + .5 + .5 ) * 2. ) + 0.2                    
!                                                                        
      if (iczero) 36520, 6520, 36520                                    
 6520 continue                                                          
      iadn12(5) = 0.5                                                   
      iadn22(1,3) = 0.5                                                 
      iadn33(1,2,2) = 0.5                                               
      ivcomp = ( ( iadn12(5) + iadn22(1,3) + iadn33(1,2,2) ) * 2. ) + .2
      goto 46520                                                       
36520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46520, 6531, 46520                                    
46520 if ( ivcomp - 3 )  26520, 16520, 26520                            
16520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6531                                                        
26520 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6531 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM024" )                          
      end program fm024
