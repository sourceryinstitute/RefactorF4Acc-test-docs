      program fm025
!      COMMENT SECTION.                                                  
!                                                                        
!      FM025                                                             
!                                                                        
!          THIS ROUTINE TESTS ARRAYS WITH IF STATEMENTS, DO LOOPS,       
!      ASSIGNED AND COMPUTED GO TO STATEMENTS IN CONJUNCTION WITH ARRAY  
!      ELEMENTS   IN COMMON OR DIMENSIONED.  ONE, TWO, AND THREE         
!      DIMENSIONED ARRAYS ARE USED.  THE SUBSCRIPTS ARE INTEGER CONSTANTS
!      OR SOMETIMES INTEGER VARIABLES WHEN THE ELEMENTS ARE IN LOOPS     
!      AND ALL ARRAYS HAVE FIXED SIZE LIMITS.  INTEGER, REAL, AND LOGICAL
!      ARRAYS ARE USED WITH THE TYPE SOMETIMES SPECIFIED WITH THE        
!      EXPLICIT TYPE STATEMENT.                                          
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8, SPECIFICATION STATEMENTS                            
!         SECTION 8.1, DIMENSION STATEMENT                               
!         SECTION 8.3, COMMON STATEMENT                                  
!         SECTION 8.4, TYPE-STATEMENTS                                   
!         SECTION 9, DATA STATEMENT                                      
!         SECTION 11.2, COMPUTED GO TO STATEMENT                         
!         SECTION 11.3, ASSIGNED GO TO STATEMENT                         
!         SECTION 11.10, DO STATEMENT                                    
!                                                                        
!                                                                        
      integer, dimension(1:2,1:2,1:2) :: iadn31
      real, dimension(1:2,1:2,1:2) :: radn31
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: i
      integer :: ivcomp
      integer :: ivcorr
      integer :: j
      integer :: icon01
      integer :: k
      integer, dimension(1:2,1:2,1:2) :: iadn32
      integer, dimension(1:2,1:2) :: iadn21
      integer, dimension(1:2) :: iadn11
!                                                                        
      logical, dimension(1:2,1:2,1:2) :: ladn31
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
      ivtnum = 653                                                      
!                                                                        
!       ****  TEST 653  ****                                             
!      TEST 653  -  TEST OF SETTING ALL VALUES OF AN INTEGER ARRAY       
!      BY THE INTEGER INDEX OF A DO  LOOP.  THE ARRAY HAS ONE DIMENSION. 
!                                                                        
      if (iczero) 36530, 6530, 36530                                    
 6530 continue                                                          
      do i = 1,2,1                                                 
      iadn11(i) = i                                                     
  end do
      ivcomp = iadn11(1)                                                
      goto 46530                                                       
36530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46530, 6541, 46530                                    
46530 if ( ivcomp - 1 )  26530, 16530, 26530                            
16530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6541                                                        
26530 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6541 continue                                                          
      ivtnum = 654                                                      
!                                                                        
!       ****  TEST 654  ****                                             
!      TEST 654  -  SEE TEST 653.  THIS TEST CHECKS THE SECOND ELEMENT OF
!      THE INTEGER ARRAY IADN11(2).                                      
!                                                                        
      if (iczero) 36540, 6540, 36540                                    
 6540 continue                                                          
      ivcomp = iadn11(2)                                                
      goto 46540                                                       
36540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46540, 6551, 46540                                    
46540 if ( ivcomp - 2 )  26540, 16540, 26540                            
16540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6551                                                        
26540 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6551 continue                                                          
      ivtnum = 655                                                      
!                                                                        
!       ****  TEST 655  ****                                             
!      TEST 655  -  TEST OF SETTING THE VALUES OF THE COLUMN OF A TWO    
!      DIMENSION INTEGER ARRAY BY A DO LOOP.  THE VALUES FOR THE ELEMENTS
!      IN A COLUMN IS THE NUMBER OF THE COLUMN AS SET BY THE DO LOOP     
!      INDEX.  ROW NUMBERS ARE INTEGER CONSTANTS.                        
!      THE VALUES FOR THE ELEMENTS ARE AS FOLLOWS                        
!      1    2                                                            
!      1    2                                                            
!                                                                        
      if (iczero) 36550, 6550, 36550                                    
 6550 continue                                                          
      do j = 1, 2                                                  
      iadn21(1,j) = j                                                   
      iadn21(2,j) = j                                                   
  end do
      ivcomp = iadn21(1,1)                                              
      goto 46550                                                       
36550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46550, 6561, 46550                                    
46550 if ( ivcomp - 1 )  26550, 16550, 26550                            
16550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6561                                                        
26550 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6561 continue                                                          
      ivtnum = 656                                                      
!                                                                        
!       ****  TEST 656  ****                                             
!      TEST 656  -  SEE TEST 655.  THIS TEST CHECKS THE VALUE OF THE     
!      INTEGER ARRAY  IADN21(2,2)                                        
!                                                                        
      if (iczero) 36560, 6560, 36560                                    
 6560 continue                                                          
      ivcomp = iadn21(2,2)                                              
      goto 46560                                                       
36560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46560, 6571, 46560                                    
46560 if ( ivcomp - 2 )  26560, 16560, 26560                            
16560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6571                                                        
26560 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6571 continue                                                          
      ivtnum = 657                                                      
!                                                                        
!       ****  TEST 657  ****                                             
!      TEST 657  -  THIS TESTS SETTING BOTH THE ROW AND COLUMN SUBSCRIPTS
!      IN A TWO DIMENSION INTEGER ARRAY WITH A DOUBLE NESTED DO LOOP.    
!      THE ELEMENT VALUES ARE SET BY AN INTEGER COUNTER.  ELEMENT VALUES 
!      ARE AS FOLLOWS         1   2                                      
!                             3   4                                      
!                                                                        
      if (iczero) 36570, 6570, 36570                                    
 6570 continue                                                          
      icon01 = 0                                                        
      do i = 1, 2                                                  
      do j = 1, 2                                                  
      icon01 = icon01 + 1                                               
      iadn21(i,j) = icon01                                              
  end do
  end do
      ivcomp = iadn21(1,2)                                              
      goto 46570                                                       
36570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46570, 6581, 46570                                    
46570 if ( ivcomp - 2 )  26570, 16570, 26570                            
16570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6581                                                        
26570 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6581 continue                                                          
      ivtnum = 658                                                      
!                                                                        
!       ****  TEST 658  ****                                             
!      TEST 658  -  SEE TEST 657.  THIS TEST CHECKS THE VALUE OF ARRAY   
!      ELEMENT IADN21(2,1) = 3                                           
!                                                                        
      if (iczero) 36580, 6580, 36580                                    
 6580 continue                                                          
      ivcomp = iadn21(2,1)                                              
      goto 46580                                                       
36580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46580, 6591, 46580                                    
46580 if ( ivcomp - 3 )  26580, 16580, 26580                            
16580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6591                                                        
26580 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6591 continue                                                          
      ivtnum = 659                                                      
!                                                                        
!       ****  TEST 659  ****                                             
!      TEST 659  -  THIS TEST USES A TRIPLE NESTED DO LOOP TO SET THE    
!      ELEMENTS IN ALL THREE DIMENSIONS OF AN INTEGER ARRAY THAT IS      
!      DIMENSIONED.  THE VALUES FOR THE ELEMENTS ARE AS FOLLOWS          
!      FOR ELEMENT (I,J,K) = I + J + K                                   
!      SO FOR ELEMENT (1,1,2) = 1 + 1 + 2 = 4                            
!                                                                        
      if (iczero) 36590, 6590, 36590                                    
 6590 continue                                                          
      do i = 1, 2                                                  
      do j = 1, 2                                                  
      do k = 1, 2                                                  
      iadn32( i, j, k ) = i + j + k                                     
  end do
  end do
  end do
      ivcomp = iadn32(1,1,2)                                            
      goto 46590                                                       
36590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46590, 6601, 46590                                    
46590 if ( ivcomp - 4 )  26590, 16590, 26590                            
16590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6601                                                        
26590 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6601 continue                                                          
      ivtnum = 660                                                      
!                                                                        
!       ****  TEST 660  ****                                             
!      TEST 660  -  SEE TEST 659.  THIS CHECKS FOR IADN32(2,2,2) = 6     
!                                                                        
      if (iczero) 36600, 6600, 36600                                    
 6600 continue                                                          
      ivcomp = iadn32(2,2,2)                                            
      goto 46600                                                       
36600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46600, 6611, 46600                                    
46600 if ( ivcomp - 6 )  26600, 16600, 26600                            
16600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6611                                                        
26600 ivfail = ivfail + 1                                               
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6611 continue                                                          
      ivtnum = 661                                                      
!                                                                        
!       ****  TEST 661  ****                                             
!      TEST 661  -  THIS TEST SETS THE ELEMENTS OF AN INTEGER ARRAY IN   
!      COMMON TO MINUS THE VALUE OF THE INTEGER ARRAY SET IN TEST 659.   
!      ELEMENT IADN32(1,1,2) = 4  SO ELEMENT IADN31(1,1,2) = -4          
!      THE SAME INTEGER ASSIGNMENT STATEMENT IS USED AS THE TERMINATING  
!      STATEMENT FOR ALL THREE DO LOOPS USED TO SET THE ARRAY VALUES     
!      OF INTEGER ARRAY IADN31.                                          
!      IF TEST 659 FAILS, THEN THIS TEST SHOULD ALSO FAIL.  HOWEVER, THE 
!      COMPUTED VALUES SHOULD RELATE IN THAT THE COMPUTED VALUE FOR      
!      TEST 661 SHOULD BE MINUS THE COMPUTED VALUE FOR TEST 659.         
!                                                                        
      if (iczero) 36610, 6610, 36610                                    
 6610 continue                                                          
      do i = 1, 2                                                  
      do j = 1, 2                                                  
      do k = 1, 2                                                  
 6612 iadn31(i,j,k) = - iadn32 ( i, j, k )                              
      end do
      end do
      end do
      ivcomp = iadn31(1,1,2)                                            
      goto 46610                                                       
36610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46610, 6621, 46610                                    
46610 if ( ivcomp + 4 )  26610, 16610, 26610                            
16610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6621                                                        
26610 ivfail = ivfail + 1                                               
      ivcorr = -4                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6621 continue                                                          
      ivtnum = 662                                                      
!                                                                        
!       ****  TEST 662  ****                                             
!      TEST 662  -  THIS IS A TEST OF A TRIPLE NESTED DO LOOP USED TO    
!      SET THE VALUES OF A LOGICAL ARRAY LADN31.  UNLIKE THE OTHER TESTS 
!      THE THIRD DIMENSION IS SET LAST, THE FIRST DIMENSION IS SET SECOND
!      AND THE SECOND DIMENSION IS SET FIRST.  ALL ARRAY ELEMENTS ARE SET
!      TO THE LOGICAL CONSTANT .FALSE.                                   
!                                                                        
      if (iczero) 36620, 6620, 36620                                    
 6620 continue                                                          
      do k = 1, 2                                                  
      do i = 1, 2                                                  
      do j = 1, 2                                                  
      ladn31( i, j, k ) = .false.                                       
  end do
      end do
      end do
      icon01 = 1                                                        
      if ( ladn31(2,1,2) )  icon01 = 0                                  
      goto 46620                                                       
36620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46620, 6631, 46620                                    
46620 if ( icon01 - 1 )  26620, 16620, 26620                            
16620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6631                                                        
26620 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6631 continue                                                          
      ivtnum = 665                                                      
!                                                                        
!       ****  TEST 665  ****                                             
!      TEST 665  -  ARRAY ELEMENTS SET TO TYPE REAL BY THE EXPLICIT      
!      REAL STATEMENT ARE SET TO THE VALUE 0.5 AND USED TO SET THE VALUE 
!      OF AN ARRAY ELEMENT SET TO TYPE INTEGER BY THE INTEGER STATEMENT. 
!      THIS LAST INTEGER ELEMENT IS USED IN A LOGICAL IF STATEMENT       
!      THAT SHOULD COMPARE TRUE.  ( .5 + .5 + .5 ) * 2. .EQ. 3           
!                                                                        
      if (iczero) 36650, 6650, 36650                                    
 6650 continue                                                          
      iadn33(2,2,2) = 0.5                                               
      iadn22(2,4) = 0.5                                                 
      iadn12(8) = 0.5                                                   
      radn11(8) = ( iadn33(2,2,2) + iadn22(2,4) + iadn12(8) ) * 2.      
      icon01 = 0                                                        
      if ( radn11(8)  ==  3 )  icon01 = 1                               
      goto 46650                                                       
36650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46650, 6661, 46650                                    
46650 if ( icon01 - 1 )  26650, 16650, 26650                            
16650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6661                                                        
26650 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6661 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM025" )                          
      end program fm025
