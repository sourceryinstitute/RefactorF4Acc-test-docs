      program fm020
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM020                                                             
!                                                                        
!              THIS ROUTINE TESTS THE FORTRAN IN-LINE STATEMENT FUNCTION 
!      OF TYPE LOGICAL AND INTEGER.  INTEGER CONSTANTS, LOGICAL CONSTANTS
!      INTEGER VARIABLES, LOGICAL VARIABLES, INTEGER ARITHMETIC EXPRESS- 
!      IONS ARE ALL USED TO TEST THE STATEMENT FUNCTION DEFINITION AND   
!      THE VALUE RETURNED FOR THE STATEMENT FUNCTION WHEN IT IS USED     
!      IN THE MAIN BODY OF THE PROGRAM.                                  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8.4.1, INTEGER, REAL, DOUBLE PRECISION, COMPLEX, AND   
!                        LOGICAL TYPE-STATEMENTS                         
!         SECTION 15.3.2, INTRINSIC FUNCTION REFERENCES                  
!         SECTION 15.4, STATEMENT FUNCTIONS                              
!         SECTION 15.4.1, FORMS OF A FUNCTION STATEMENT                  
!         SECTION 15.4.2, REFERENCING A STATEMENT FUNCTION               
!         SECTION 15.5.2, EXTERNAL FUNCTION REFERENCES                   
!                                                                        
      integer :: ifon01
      integer :: idon01
      integer :: ifon02
      integer :: idon02
      integer :: ifon03
      integer :: idon03
      integer :: ifon04
      integer :: idon04
      integer :: ifon05
      integer :: idon05
      integer :: idon06
      integer :: ifon06
      integer :: idon07
      integer :: idon08
      integer :: ifon07
      integer :: idon09
      integer :: ifon08
      integer :: i
      integer :: j
      integer :: ifon09
      integer :: k
      integer :: l
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
      integer :: icon01
      integer :: icon04
      integer :: icon05
      integer :: icon06
      integer :: icon07
      integer :: icon08
      integer :: icon09
      integer :: icon10
      logical :: lftn01
      logical :: ldtn01
      logical :: lftn02
      logical :: ldtn02
      logical :: lftn03
      logical :: ldtn03
      logical :: lctn03
      logical :: lftn04
      logical :: ldtn04
      logical :: lctn04
      integer, dimension(1:2) :: iadn11
!                                                                        
! ..... TEST 553                                                         
      ifon01(idon01) = 32767                                            
!                                                                        
! ..... TEST 554                                                         
      lftn01(ldtn01) = .true.                                           
!                                                                        
! ..... TEST 555                                                         
      ifon02 ( idon02 ) = idon02                                        
!                                                                        
! ..... TEST 556                                                         
      lftn02( ldtn02 ) = ldtn02                                         
!                                                                        
! ..... TEST 557                                                         
      ifon03 (idon03 )= idon03                                          
!                                                                        
! ..... TEST 558                                                         
      lftn03(ldtn03) = ldtn03                                           
!                                                                        
! ..... TEST 559                                                         
      lftn04(ldtn04) = .not. ldtn04                                     
!                                                                        
! ..... TEST 560                                                         
      ifon04(idon04) = idon04 ** 2                                      
!                                                                        
! ..... TEST 561                                                         
      ifon05(idon05, idon06) = idon05 + idon06                          
!                                                                        
! ..... TEST 562                                                         
      ifon06(idon07, idon08) = sqrt(float(idon07**2)+float(idon08**2))  
!                                                                        
! ..... TEST 563                                                         
      ifon07(idon09) = idon09 ** 2                                      
      ifon08(i,j)=sqrt(float(ifon07(i))+float(ifon07(j)))               
!                                                                        
! ..... TEST 564                                                         
      ifon09(k,l) = k / l + k ** l - k * l                              
!                                                                        
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
      ivtnum = 553                                                      
!                                                                        
!       ****  TEST 553  ****                                             
!      TEST 553  -  THE VALUE OF THE INTEGER FUNCTION IS SET TO A        
!          CONSTANT OF 32767 REGARDLESS OF THE VALUE OF THE ARGUEMENT    
!      SUPPLIED TO THE DUMMY ARGUEMENT.  TEST OF POSITIVE INTEGER        
!      CONSTANTS FOR A STATEMENT FUNCTION.                               
!                                                                        
!                                                                        
      if (iczero) 35530, 5530, 35530                                    
 5530 continue                                                          
      ivcomp = ifon01(3)                                                
      goto 45530                                                       
35530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45530, 5541, 45530                                    
45530 if ( ivcomp - 32767 )  25530, 15530, 25530                        
15530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5541                                                        
25530 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5541 continue                                                          
      ivtnum = 554                                                      
!                                                                        
!       ****  TEST 554  ****                                             
!      TEST 554  -  TEST OF THE STATEMENT FUNCTION OF TYPE LOGICAL       
!          SET TO THE LOGICAL CONSTANT .TRUE. REGARDLESS OF THE          
!      ARGUEMENT SUPPLIED TO THE DUMMY ARGUEMENT.                        
!      A LOGICAL    IF STATEMENT IS USED IN CONJUNCTION WITH THE LOGICAL 
!      STATEMENT FUNCTION.  THE TRUE PATH IS TESTED.                     
!                                                                        
!                                                                        
      if (iczero) 35540, 5540, 35540                                    
 5540 continue                                                          
      ivon01 = 0                                                        
      if ( lftn01(.false.) )  ivon01 = 1                                
      goto 45540                                                       
35540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45540, 5551, 45540                                    
45540 if ( ivon01 - 1 )  25540, 15540, 25540                            
15540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5551                                                        
25540 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5551 continue                                                          
      ivtnum = 555                                                      
!                                                                        
!       ****  TEST 555  ****                                             
!      TEST 555  -  THE INTEGER STATEMENT FUNCTION IS SET TO THE VALUE   
!          OF THE ARGEUMENT SUPPLIED.                                    
!                                                                        
!                                                                        
      if (iczero) 35550, 5550, 35550                                    
 5550 continue                                                          
      ivcomp = ifon02 ( 32767 )                                         
      goto 45550                                                       
35550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45550, 5561, 45550                                    
45550 if ( ivcomp - 32767 )  25550, 15550, 25550                        
15550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5561                                                        
25550 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5561 continue                                                          
      ivtnum = 556                                                      
!                                                                        
!       ****  TEST 556  ****                                             
!      TEST 556  -  TEST OF A LOGICAL STATEMENT FUNCTION SET TO THE      
!          VALUE OF THE ARGUEMENT SUPPLIED.  THE FALSE PATH OF A LOGICAL 
!             IF STATEMENT IS USED IN CONJUNCTION WITH THE LOGICAL       
!          STATEMENT FUNCTION.                                           
!                                                                        
!                                                                        
      if (iczero) 35560, 5560, 35560                                    
 5560 continue                                                          
      ivon01 = 1                                                        
      if ( lftn02(.false.) )  ivon01 = 0                                
      goto 45560                                                       
35560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45560, 5571, 45560                                    
45560 if ( ivon01 - 1 )  25560, 15560, 25560                            
15560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5571                                                        
25560 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5571 continue                                                          
      ivtnum = 557                                                      
!                                                                        
!       ****  TEST 557  ****                                             
!      TEST 557  -  THE VALUE OF AN INTEGER FUNCTION IS SET EQUAL TO     
!          VALUE OF THE ARGUEMENT SUPPLIED.  THIS VALUE IS AN INTEGER    
!          VARIABLE SET TO 32767.                                        
!                                                                        
!                                                                        
      if (iczero) 35570, 5570, 35570                                    
 5570 continue                                                          
      icon01 = 32767                                                    
      ivcomp = ifon03 ( icon01 )                                        
      goto 45570                                                       
35570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45570, 5581, 45570                                    
45570 if ( ivcomp - 32767 )  25570, 15570, 25570                        
15570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5581                                                        
25570 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5581 continue                                                          
      ivtnum = 558                                                      
!                                                                        
!       ****  TEST 558  ****                                             
!      TEST 558 -  A LOGICAL STATEMENT FUNCTION IS SET EQUAL TO THE      
!          VALUE OF THE ARGUEMENT SUPPLIED.  THIS VALUE IS A LOGICAL     
!      VARIABLE SET TO .TRUE.  THE TRUE PATH OF A LOGICAL IF             
!          STATEMENT IS USED IN CONJUNCTION WITH THE LOGICAL STATEMENT   
!          FUNCTION.                                                     
!                                                                        
!                                                                        
      if (iczero) 35580, 5580, 35580                                    
 5580 continue                                                          
      ivon01 = 0                                                        
      lctn03 = .true.                                                   
      if ( lftn03(lctn03) )  ivon01 = 1                                 
      goto 45580                                                       
35580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45580, 5591, 45580                                    
45580 if ( ivon01 - 1 )  25580, 15580, 25580                            
15580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5591                                                        
25580 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5591 continue                                                          
      ivtnum = 559                                                      
!                                                                        
!       ****  TEST 559  ****                                             
!      TEST 559  -  LIKE TEST 558 ONLY THE LOGICAL  .NOT.  IS USED       
!          IN THE LOGICAL STATEMENT FUNCTION DEFINITION  THE FALSE PATH  
!          OF A LOGICAL IF STATEMENT IS USED IN CONJUNCTION WITH THE     
!          LOGICAL STATEMENT FUNCTION.                                   
!                                                                        
!                                                                        
      if (iczero) 35590, 5590, 35590                                    
 5590 continue                                                          
      ivon01 = 1                                                        
      lctn04 = .true.                                                   
      if ( lftn04(lctn04) )  ivon01 = 0                                 
      goto 45590                                                       
35590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45590, 5601, 45590                                    
45590 if ( ivon01 - 1 )  25590, 15590, 25590                            
15590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5601                                                        
25590 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5601 continue                                                          
      ivtnum = 560                                                      
!                                                                        
!       ****  TEST 560  ****                                             
!      TEST 560  -  INTEGER EXPONIENTIATION USED IN AN INTEGER           
!          STATEMENT FUNCTION.                                           
!                                                                        
!                                                                        
      if (iczero) 35600, 5600, 35600                                    
 5600 continue                                                          
      icon04 = 3                                                        
      ivcomp = ifon04(icon04)                                           
      goto 45600                                                       
35600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45600, 5611, 45600                                    
45600 if ( ivcomp - 9 )  25600, 15600, 25600                            
15600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5611                                                        
25600 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5611 continue                                                          
      ivtnum = 561                                                      
!                                                                        
!       ****  TEST 561  ****                                             
!      TEST 561  -  TEST OF INTEGER ADDITION USING TWO (2) DUMMY         
!          ARGUEMENTS.                                                   
!                                                                        
!                                                                        
      if (iczero) 35610, 5610, 35610                                    
 5610 continue                                                          
      icon05 = 9                                                        
      icon06 = 16                                                       
      ivcomp = ifon05(icon05, icon06)                                   
      goto 45610                                                       
35610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45610, 5621, 45610                                    
45610 if ( ivcomp - 25 )  25610, 15610, 25610                           
15610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5621                                                        
25610 ivfail = ivfail + 1                                               
      ivcorr = 25                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5621 continue                                                          
      ivtnum = 562                                                      
!                                                                        
!       ****  TEST 562  ****                                             
!      TEST 562  -  THIS TEST IS THE SOLUTION OF A RIGHT TRIANGLE        
!          USING INTEGER STATEMENT FUNCTIONS WHICH REFERENCE THE         
!          INTRINSIC FUNCTIONS  SQRT  AND  FLOAT.  THIS IS A 3-4-5       
!          RIGHT TRIANGLE.                                               
!                                                                        
!                                                                        
      if (iczero) 35620, 5620, 35620                                    
 5620 continue                                                          
      icon07 = 3                                                        
      icon08 = 4                                                        
      ivcomp = ifon06(icon07, icon08)                                   
      goto 45620                                                       
35620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45620, 5631, 45620                                    
45620 if ( ivcomp - 5 )  5622, 15620, 5622                              
 5622 if ( ivcomp - 4 ) 25620, 15620, 25620                             
15620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5631                                                        
25620 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5631 continue                                                          
      ivtnum = 563                                                      
!                                                                        
!       ****  TEST 563  ****                                             
!      TEST 563  -  SOLUTION OF A 3-4-5 RIGHT TRIANGLE LIKE TEST 562     
!          EXCEPT THAT BOTH INTRINSIC AND PREVIOUSLY DEFINED STATEMENT   
!          FUNCTIONS ARE USED.                                           
!                                                                        
!                                                                        
      if (iczero) 35630, 5630, 35630                                    
 5630 continue                                                          
      icon09 = 3                                                        
      icon10 = 4                                                        
      ivcomp = ifon08(icon09, icon10)                                   
      goto 45630                                                       
35630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45630, 5641, 45630                                    
45630 if ( ivcomp - 5 )   5632, 15630, 5632                             
 5632 if ( ivcomp - 4 )  25630, 15630, 25630                            
15630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5641                                                        
25630 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5641 continue                                                          
      ivtnum = 564                                                      
!                                                                        
!       ****  TEST 564  ****                                             
!      TEST 564  -  USE  OF ARRAY ELEMENTS IN AN INTEGER STATEMENT       
!          FUNCTION WHICH USES THE OPERATIONS OF + - * /  .              
!                                                                        
!                                                                        
      if (iczero) 35640, 5640, 35640                                    
 5640 continue                                                          
      iadn11(1) = 2                                                     
      iadn11(2) = 2                                                     
      ivcomp = ifon09( iadn11(1), iadn11(2) )                           
      goto 45640                                                       
35640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45640, 5651, 45640                                    
45640 if ( ivcomp - 1 )  25640, 15640, 25640                            
15640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5651                                                        
25640 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5651 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM020" )                          
      end program fm020
