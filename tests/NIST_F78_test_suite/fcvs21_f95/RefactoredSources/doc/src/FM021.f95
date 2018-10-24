      program fm021
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM021                                                             
!                                                                        
!            THIS ROUTINE TESTS THE FORTRAN  DATA INITIALIZATION         
!      STATEMENT.  INTEGER, REAL, AND LOGICAL DATA TYPES ARE TESTED      
!      USING UNSIGNED CONSTANTS, SIGNED CONSTANTS, AND LOGICAL           
!      CONSTANTS..   INTEGER, REAL, LOGICAL, AND MIXED TYPE ARRAYS       
!      ARE ALSO TESTED.                                                  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.1.3, DATA TYPE PREPARATION                           
!         SECTION 4.4.3, REAL CONSTANT                                   
!         SECTION 9, DATA STATEMENT                                      
!                                                                        
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
      integer :: icon02
      integer :: icon03
      integer :: icon04
      integer :: icon05
      integer :: icon06
      integer :: icon07
      integer :: icon08
      integer :: icon09
      integer :: icon10
      integer :: ivon01
      real :: rcon01
      real :: rcon02
      real :: rcon03
      real :: rcon04
      real :: rcon05
      real :: rcon06
      real :: rcon07
      real :: rcon08
      integer, dimension(1:3) :: ratn11
      logical :: lctn01
      logical :: lctn02
      logical, dimension(1:3) :: latn11
      logical, dimension(1:6) :: ladn11
      real, dimension(1:3) :: iatn11
      integer, dimension(1:3) :: iadn11
      real, dimension(1:4) :: radn11
      real, dimension(1:4) :: radn13
      integer, dimension(1:4) :: iadn12
      integer, dimension(1:4) :: iadn13
!                                                                        
      data icon01 / 0 / 
      data icon02 / 3 / 
      data icon03 / 76 / 
      data icon04 / 587 / 
      data icon05 / 9999 / 
      data icon06 / 32767 / 
      data icon07 / -0 / 
      data icon08 / -32766 / 
      data icon09 / 00003 / 
      data icon10 / 32767 / 
      data lctn01 / .true. / 
      data lctn02 / .false. / 
      data rcon01 / 0. / 
      data rcon02 / .0 / 
      data rcon03 / 0.0 / 
      data rcon04 / 32767. / 
      data rcon05 / -32766. / 
      data rcon06 / -000587. / 
      data rcon07 / 99.99 / 
      data rcon08 / -03.2766 / 
      data iadn11(1) / 3 / ,iadn11(3) / -587 / ,iadn11(2) / 32767 / 
      data iadn12 / 4*9999 / 
      data iadn13 / 0,2*-32766,-587 / 
      data ladn11 / .true.,.false.,2*.true.,2*.false. / 
      data radn11 / 32767.,-32.766,2*587. / 
      data latn11 / .true.,2*.false. / ,iatn11 / 2*32767.,-32766. / 
      data ratn11 / 3*-32766 / 
      data radn13 / 32.767e03,-3.2766e-01,.587e+03,9e1 / 
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
      ivtnum = 565                                                      
!                                                                        
!       ****  TEST 565  ****                                             
!      TEST 565  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT ZERO.                                                
!                                                                        
!                                                                        
      if (iczero) 35650, 5650, 35650                                    
 5650 continue                                                          
      goto 45650                                                       
35650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45650, 5661, 45650                                    
45650 if ( icon01 - 0 )  25650, 15650, 25650                            
15650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5661                                                        
25650 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5661 continue                                                          
      ivtnum = 566                                                      
!                                                                        
!       ****  TEST 566  ****                                             
!      TEST 566  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT 3.                                                   
!                                                                        
!                                                                        
      if (iczero) 35660, 5660, 35660                                    
 5660 continue                                                          
      goto 45660                                                       
35660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45660, 5671, 45660                                    
45660 if ( icon02 - 3 )  25660, 15660, 25660                            
15660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5671                                                        
25660 ivfail = ivfail + 1                                               
      ivcomp = icon02                                                   
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5671 continue                                                          
      ivtnum = 567                                                      
!                                                                        
!       ****  TEST 567  ****                                             
!      TEST 567  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT 76.                                                  
!                                                                        
!                                                                        
      if (iczero) 35670, 5670, 35670                                    
 5670 continue                                                          
      goto 45670                                                       
35670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45670, 5681, 45670                                    
45670 if ( icon03 - 76 )  25670, 15670, 25670                           
15670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5681                                                        
25670 ivfail = ivfail + 1                                               
      ivcomp = icon03                                                   
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5681 continue                                                          
      ivtnum = 568                                                      
!                                                                        
!       ****  TEST 568  ****                                             
!      TEST 568  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT  587.                                                
!                                                                        
!                                                                        
      if (iczero) 35680, 5680, 35680                                    
 5680 continue                                                          
      goto 45680                                                       
35680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45680, 5691, 45680                                    
45680 if ( icon04 - 587 )  25680, 15680, 25680                          
15680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5691                                                        
25680 ivfail = ivfail + 1                                               
      ivcomp = icon04                                                   
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5691 continue                                                          
      ivtnum = 569                                                      
!                                                                        
!       ****  TEST 569  ****                                             
!      TEST 569  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT  9999.                                               
!                                                                        
!                                                                        
      if (iczero) 35690, 5690, 35690                                    
 5690 continue                                                          
      goto 45690                                                       
35690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45690, 5701, 45690                                    
45690 if ( icon05 - 9999 )  25690, 15690, 25690                         
15690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5701                                                        
25690 ivfail = ivfail + 1                                               
      ivcomp = icon05                                                   
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5701 continue                                                          
      ivtnum = 570                                                      
!                                                                        
!       ****  TEST 570  ****                                             
!      TEST 570  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT  32767.                                              
!                                                                        
!                                                                        
      if (iczero) 35700, 5700, 35700                                    
 5700 continue                                                          
      goto 45700                                                       
35700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45700, 5711, 45700                                    
45700 if ( icon06 - 32767 )  25700, 15700, 25700                        
15700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5711                                                        
25700 ivfail = ivfail + 1                                               
      ivcomp = icon06                                                   
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5711 continue                                                          
      ivtnum = 571                                                      
!                                                                        
!       ****  TEST 571  ****                                             
!      TEST 571  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT -0.  NOTE THAT SIGNED ZERO AND UNSIGNED ZERO         
!          SHOULD BE EQUAL FOR ANY INTEGER OPERATION.                    
!                                                                        
!                                                                        
      if (iczero) 35710, 5710, 35710                                    
 5710 continue                                                          
      goto 45710                                                       
35710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45710, 5721, 45710                                    
45710 if ( icon07 - 0 )  25710, 15710, 25710                            
15710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5721                                                        
25710 ivfail = ivfail + 1                                               
      ivcomp = icon07                                                   
      ivcorr = -0                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5721 continue                                                          
      ivtnum = 572                                                      
!                                                                        
!       ****  TEST 572  ****                                             
!      TEST 572  -  TEST OF AN INTEGER VARIABLE SET TO THE INTEGER       
!          CONSTANT  (SIGNED)  -32766.                                   
!                                                                        
!                                                                        
      if (iczero) 35720, 5720, 35720                                    
 5720 continue                                                          
      goto 45720                                                       
35720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45720, 5731, 45720                                    
45720 if ( icon08 + 32766 )  25720, 15720, 25720                        
15720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5731                                                        
25720 ivfail = ivfail + 1                                               
      ivcomp = icon08                                                   
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5731 continue                                                          
      ivtnum = 573                                                      
!                                                                        
!       ****  TEST 573  ****                                             
!      TEST 573  -  TEST THE EFFECT OF LEADING ZERO ON AN INTEGER        
!          CONSTANT  00003.                                              
!                                                                        
!                                                                        
      if (iczero) 35730, 5730, 35730                                    
 5730 continue                                                          
      goto 45730                                                       
35730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45730, 5741, 45730                                    
45730 if ( icon09 - 3 )  25730, 15730, 25730                            
15730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5741                                                        
25730 ivfail = ivfail + 1                                               
      ivcomp = icon09                                                   
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5741 continue                                                          
      ivtnum = 574                                                      
!                                                                        
!       ****  TEST 574  ****                                             
!      TEST 574  -  TEST OF BLANKS IMBEDDED IN AN INTEGER CONSTANT       
!          WHICH WAS / 3 2 7 6 7/ IN THE DATA INITIALIZATION STATEMENT.  
!                                                                        
!                                                                        
      if (iczero) 35740, 5740, 35740                                    
 5740 continue                                                          
      goto 45740                                                       
35740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45740, 5751, 45740                                    
45740 if ( icon10 - 32767 )  25740, 15740, 25740                        
15740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5751                                                        
25740 ivfail = ivfail + 1                                               
      ivcomp = icon10                                                   
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5751 continue                                                          
      ivtnum = 575                                                      
!                                                                        
!       ****  TEST 575  ****                                             
!      TEST 575  -  TEST OF A LOGICAL VARIABLE SET TO THE LOGICAL        
!          CONSTANT  .TRUE.                                              
!          TRUE PATH OF A LOGICAL IF STATEMENT IS USED IN THE TEST.      
!                                                                        
!                                                                        
      if (iczero) 35750, 5750, 35750                                    
 5750 continue                                                          
      ivon01 = 0                                                        
      if ( lctn01 )  ivon01 = 1                                         
      goto 45750                                                       
35750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45750, 5761, 45750                                    
45750 if ( ivon01 - 1 )  25750, 15750, 25750                            
15750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5761                                                        
25750 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5761 continue                                                          
      ivtnum = 576                                                      
!                                                                        
!       ****  TEST 576  ****                                             
!      TEST 576  -  TEST OF A LOGICAL VARIABLE SET TO THE LOGICAL        
!          CONSTANT .FALSE.  THE FALSE PATH OF A LOGICAL IF STATEMENT    
!          IS ALSO USED IN THE TEST.                                     
!                                                                        
!                                                                        
      if (iczero) 35760, 5760, 35760                                    
 5760 continue                                                          
      ivon01 = 1                                                        
      if ( lctn02 )  ivon01 = 0                                         
      goto 45760                                                       
35760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45760, 5771, 45760                                    
45760 if ( ivon01 - 1 )  25760, 15760, 25760                            
15760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5771                                                        
25760 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5771 continue                                                          
      ivtnum = 577                                                      
!                                                                        
!       ****  TEST 577  ****                                             
!      TEST 577  -  REAL VARIABLE SET TO 0.                              
!                                                                        
!                                                                        
      if (iczero) 35770, 5770, 35770                                    
 5770 continue                                                          
      goto 45770                                                       
35770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45770, 5781, 45770                                    
45770 if ( rcon01 - 0. )  25770, 15770, 25770                           
15770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5781                                                        
25770 ivfail = ivfail + 1                                               
      ivcomp = rcon01                                                   
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5781 continue                                                          
      ivtnum = 578                                                      
!                                                                        
!       ****  TEST 578  ****                                             
!      TEST 578  -  REAL VARIABLE SET TO  .0                             
!                                                                        
!                                                                        
      if (iczero) 35780, 5780, 35780                                    
 5780 continue                                                          
      goto 45780                                                       
35780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45780, 5791, 45780                                    
45780 if ( rcon02 - .0 )  25780, 15780, 25780                           
15780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5791                                                        
25780 ivfail = ivfail + 1                                               
      ivcomp = rcon02                                                   
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5791 continue                                                          
      ivtnum = 579                                                      
!                                                                        
!       ****  TEST 579  ****                                             
!      TEST 579  -  REAL VARIABLE SET TO 0.0                             
!                                                                        
!                                                                        
      if (iczero) 35790, 5790, 35790                                    
 5790 continue                                                          
      goto 45790                                                       
35790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45790, 5801, 45790                                    
45790 if ( rcon03 - 0.0 )  25790, 15790, 25790                          
15790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5801                                                        
25790 ivfail = ivfail + 1                                               
      ivcomp = rcon03                                                   
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5801 continue                                                          
      ivtnum = 580                                                      
!                                                                        
!       ****  TEST 580  ****                                             
!      TEST 580  -  REAL VARIABLE SET TO 32767.                          
!                                                                        
!                                                                        
      if (iczero) 35800, 5800, 35800                                    
 5800 continue                                                          
      goto 45800                                                       
35800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45800, 5811, 45800                                    
45800 if ( rcon04 - 32767. )  25800, 15800, 25800                       
15800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5811                                                        
25800 ivfail = ivfail + 1                                               
      ivcomp = rcon04                                                   
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5811 continue                                                          
      ivtnum = 581                                                      
!                                                                        
!       ****  TEST 581  ****                                             
!      TEST 581  -  REAL VARIABLE SET TO -32766.                         
!                                                                        
!                                                                        
      if (iczero) 35810, 5810, 35810                                    
 5810 continue                                                          
      goto 45810                                                       
35810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45810, 5821, 45810                                    
45810 if ( rcon05 + 32766 )  25810, 15810, 25810                        
15810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5821                                                        
25810 ivfail = ivfail + 1                                               
      ivcomp = rcon05                                                   
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5821 continue                                                          
      ivtnum = 582                                                      
!                                                                        
!       ****  TEST 582  ****                                             
!      TEST 582  -  REAL VARIABLE SET TO -000587.  TEST OF LEADING SIGN  
!          AND LEADING ZEROS ON A REAL CONSTANT.                         
!                                                                        
!                                                                        
      if (iczero) 35820, 5820, 35820                                    
 5820 continue                                                          
      goto 45820                                                       
35820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45820, 5831, 45820                                    
45820 if ( rcon06 + 587. )  25820, 15820, 25820                         
15820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5831                                                        
25820 ivfail = ivfail + 1                                               
      ivcomp = rcon06                                                   
      ivcorr = -587                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5831 continue                                                          
      ivtnum = 583                                                      
!                                                                        
!       ****  TEST 583  ****                                             
!      TEST 583  -  REAL VARIABLE SET TO 99.99                           
!                                                                        
!                                                                        
      if (iczero) 35830, 5830, 35830                                    
 5830 continue                                                          
      goto 45830                                                       
35830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45830, 5841, 45830                                    
45830 if ( rcon07 - 99.99 )  25830, 15830, 25830                        
15830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5841                                                        
25830 ivfail = ivfail + 1                                               
      ivcomp = rcon07                                                   
      ivcorr = 99                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5841 continue                                                          
      ivtnum = 584                                                      
!                                                                        
!       ****  TEST 584  ****                                             
!      TEST 584  -  REAL VARIABLE SET TO /-03. 2  7 6   6/ TO TEST       
!          THE EFFECT OF BLANKS IMBEDDED IN A REAL CONSTANT.             
!                                                                        
!                                                                        
      if (iczero) 35840, 5840, 35840                                    
 5840 continue                                                          
      goto 45840                                                       
35840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45840, 5851, 45840                                    
45840 if ( rcon08 + 3.2766 )  25840, 15840, 25840                       
15840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5851                                                        
25840 ivfail = ivfail + 1                                               
      ivcomp = rcon08                                                   
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5851 continue                                                          
      ivtnum = 585                                                      
!                                                                        
!       ****  TEST 585  ****                                             
!      TEST 585  -  INTEGER ARRAY ELEMENT SET TO 3                       
!                                                                        
!                                                                        
      if (iczero) 35850, 5850, 35850                                    
 5850 continue                                                          
      goto 45850                                                       
35850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45850, 5861, 45850                                    
45850 if ( iadn11(1) - 3 )  25850, 15850, 25850                         
15850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5861                                                        
25850 ivfail = ivfail + 1                                               
      ivcomp = iadn11(1)                                                
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5861 continue                                                          
      ivtnum = 586                                                      
!                                                                        
!       ****  TEST 586  ****                                             
!      TEST 586  -  INTEGER ARRAY ELEMENT SET TO  32767                  
!                                                                        
!                                                                        
      if (iczero) 35860, 5860, 35860                                    
 5860 continue                                                          
      goto 45860                                                       
35860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45860, 5871, 45860                                    
45860 if ( iadn11(2) - 32767 )  25860, 15860, 25860                     
15860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5871                                                        
25860 ivfail = ivfail + 1                                               
      ivcomp = iadn11(2)                                                
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5871 continue                                                          
      ivtnum = 587                                                      
!                                                                        
!       ****  TEST 587  ****                                             
!      TEST 587  -  INTEGER ARRAY ELEMENT SET TO -587                    
!                                                                        
!                                                                        
      if (iczero) 35870, 5870, 35870                                    
 5870 continue                                                          
      goto 45870                                                       
35870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45870, 5881, 45870                                    
45870 if ( iadn11(3) + 587 )  25870, 15870, 25870                      
15870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5881                                                        
25870 ivfail = ivfail + 1                                               
      ivcomp = iadn11(3)                                                
      ivcorr = -587                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5881 continue                                                          
      ivtnum = 588                                                      
!                                                                        
!       ****  TEST 588  ****                                             
!      TEST 588  -  TEST OF THE REPEAT FIELD  /4*999/ IN A DATA STATE.   
!                                                                        
!                                                                        
      if (iczero) 35880, 5880, 35880                                    
 5880 continue                                                          
      goto 45880                                                       
35880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45880, 5891, 45880                                    
45880 if ( iadn12(3) - 9999 )  25880, 15880, 25880                      
15880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5891                                                        
25880 ivfail = ivfail + 1                                               
      ivcomp = iadn12(3)                                                
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5891 continue                                                          
      ivtnum = 589                                                      
!                                                                        
!       ****  TEST 589  ****                                             
!      TEST 589  -  TEST OF SETTING THE WHOLE INTEGER ARRAY ELEMENTS     
!          IN ONE DATA INITIALIZATION STATEMENT.  THE FIRST ELEMENT      
!          IS SET TO 0                                                   
!                                                                        
!                                                                        
      if (iczero) 35890, 5890, 35890                                    
 5890 continue                                                          
      goto 45890                                                       
35890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45890, 5901, 45890                                    
45890 if ( iadn13(1) - 0 )  25890, 15890, 25890                         
15890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5901                                                        
25890 ivfail = ivfail + 1                                               
      ivcomp = iadn13(1)                                                
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5901 continue                                                          
      ivtnum = 590                                                      
!                                                                        
!       ****  TEST 590  ****                                             
!      TEST 590  -  SEE TEST 589.  THE SECOND ELEMENT WAS SET TO -32766  
!                                                                        
!                                                                        
      if (iczero) 35900, 5900, 35900                                    
 5900 continue                                                          
      goto 45900                                                       
35900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45900, 5911, 45900                                    
45900 if ( iadn13(2) + 32766 )  25900, 15900, 25900                     
15900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5911                                                        
25900 ivfail = ivfail + 1                                               
      ivcomp = iadn13(2)                                                
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5911 continue                                                          
      ivtnum = 591                                                      
!                                                                        
!       ****  TEST 591  ****                                             
!      TEST 591  -  SEE TEST 589.  THE THIRD ELEMENT WAS SET TO -32766   
!                                                                        
!                                                                        
      if (iczero) 35910, 5910, 35910                                    
 5910 continue                                                          
      goto 45910                                                       
35910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45910, 5921, 45910                                    
45910 if ( iadn13(3) + 32766 )  25910, 15910, 25910                     
15910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5921                                                        
25910 ivfail = ivfail + 1                                               
      ivcomp = iadn13(3)                                                
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5921 continue                                                          
      ivtnum = 592                                                      
!                                                                        
!       ****  TEST 592  ****                                             
!      TEST 592  -  SEE TEST 589.  THE FOURTH ELEMENT WAS SET TO -587    
!                                                                        
!                                                                        
      if (iczero) 35920, 5920, 35920                                    
 5920 continue                                                          
      goto 45920                                                       
35920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45920, 5931, 45920                                    
45920 if ( iadn13(4) + 587 )  25920, 15920, 25920                       
15920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5931                                                        
25920 ivfail = ivfail + 1                                               
      ivcomp = iadn13(4)                                                
      ivcorr = -587                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5931 continue                                                          
      ivtnum = 593                                                      
!                                                                        
!       ****  TEST 593  ****                                             
!      TEST 593  -  TEST OF SETTING THE WHOLE LOGICAL ARRAY IN ONE       
!          DATA INITIALIZATION STATEMENT.  THE FIRST ELEMENT IS .TRUE.   
!          THE SECOND AND THIRD ELEMENTS ARE .FALSE.                     
!          THE FALSE PATH OF A LOGICAL IF STATEMENT IS USED  TESTING 2.  
!                                                                        
!                                                                        
      if (iczero) 35930, 5930, 35930                                    
 5930 continue                                                          
      ivon01 = 1                                                        
      if ( ladn11(2) )  ivon01 = 0                                      
      goto 45930                                                       
35930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45930, 5941, 45930                                    
45930 if ( ivon01 - 1 )  25930, 15930, 25930                            
15930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5941                                                        
25930 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5941 continue                                                          
      ivtnum = 594                                                      
!                                                                        
!       ****  TEST 594  ****                                             
!      TEST 594  -  SEE TEST 593.  THE FOURTH ELEMENT IS TESTED          
!          WITH THE TRUE PATH OF THE LOGICAL IF STATEMENT.               
!                                                                        
!                                                                        
      if (iczero) 35940, 5940, 35940                                    
 5940 continue                                                          
      ivon01 = 0                                                        
      if ( ladn11(4) )  ivon01 = 1                                      
      goto 45940                                                       
35940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45940, 5951, 45940                                    
45940 if ( ivon01 - 1 )  25940, 15940, 25940                            
15940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5951                                                        
25940 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5951 continue                                                          
      ivtnum = 595                                                      
!                                                                        
!       ****  TEST 595  ****                                             
!      TEST 595  -  A WHOLE REAL ARRAY IS SET IN ONE DATA INITIALIZATION 
!          STATEMENT.  THE SECOND ELEMENT IS -32.766                     
!                                                                        
!                                                                        
      if (iczero) 35950, 5950, 35950                                    
 5950 continue                                                          
      goto 45950                                                       
35950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45950, 5961, 45950                                    
45950 if ( radn11(2) + 32.766 )  25950, 15950, 25950                    
15950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5961                                                        
25950 ivfail = ivfail + 1                                               
      ivcomp = radn11(2)                                                
      ivcorr = -32                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5961 continue                                                          
      ivtnum = 596                                                      
!                                                                        
!       ****  TEST 596  ****                                             
!      TEST 596  -  SEE TEST 595.  THE FOURTH ELEMENT IS SET TO 587      
!          BY A REPEAT FIELD.                                            
!                                                                        
!                                                                        
      if (iczero) 35960, 5960, 35960                                    
 5960 continue                                                          
      goto 45960                                                       
35960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45960, 5971, 45960                                    
45960 if ( radn11(4) - 587 )  25960, 15960, 25960                       
15960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5971                                                        
25960 ivfail = ivfail + 1                                               
      ivcomp = radn11(4)                                                
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5971 continue                                                          
      ivtnum = 597                                                      
!                                                                        
!       ****  TEST 597  ****                                             
!      TEST 597  -  TEST OF MIXED ARRAY ELEMENT TYPES IN A SINGLE DATA   
!          INITIALIZATION STATEMENT.  THE TYPE LOGICAL STATEMENT CONTAINS
!          THE ARRAY DECLARATIONS.  THE FALSE PATH OF A LOGICAL          
!          IF STATEMENT TESTS THE LOGICAL RESULTS.                       
!                                                                        
!                                                                        
      if (iczero) 35970, 5970, 35970                                    
 5970 continue                                                          
      ivon01 = 1                                                        
      if ( latn11(2) )  ivon01 = 0                                      
      goto 45970                                                       
35970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45970, 5981, 45970                                    
45970 if ( ivon01 - 1 )  25970, 15970, 25970                            
15970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5981                                                        
25970 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5981 continue                                                          
      ivtnum = 598                                                      
!                                                                        
!       ****  TEST 598  ****                                             
!      TEST 598  -  TYPE OF THE DATA WAS SET EXPLICITLY REAL IN  THE     
!          DECLARATIVE FOR THE ARRAY.  DATA SHOULD BE SET TO 32767.      
!                                                                        
!                                                                        
      if (iczero) 35980, 5980, 35980                                    
 5980 continue                                                          
      goto 45980                                                       
35980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45980, 5991, 45980                                    
45980 if ( iatn11(2) - 32767. )  25980, 15980, 25980                    
15980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5991                                                        
25980 ivfail = ivfail + 1                                               
      ivcomp = iatn11(2)                                                
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5991 continue                                                          
      ivtnum = 599                                                      
!                                                                        
!       ****  TEST 599  ****                                             
!      TEST 599  -  TYPE OF THE DATA WAS SET EXPLICITLY INTEGER IN THE   
!          DECLARATIVE FOR THE ARRAY.  DATA SHOULD BE SET TO -32766      
!                                                                        
!                                                                        
      if (iczero) 35990, 5990, 35990                                    
 5990 continue                                                          
      goto 45990                                                       
35990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45990, 6001, 45990                                    
45990 if ( ratn11(2) + 32766 )  25990, 15990, 25990                     
15990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6001                                                        
25990 ivfail = ivfail + 1                                               
      ivcomp = ratn11(2)                                                
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6001 continue                                                          
      ivtnum = 600                                                      
!                                                                        
!       ****  TEST 600  ****                                             
!      TEST 600  -  TEST OF REAL DECIMAL CONSTANTS USING E-NOTATION.     
!          SEE SECTION 4.4.2.  THE VALUE OF THE ELEMENT SHOULD           
!          BE SET TO 32767.                                              
!                                                                        
!                                                                        
      if (iczero) 36000, 6000, 36000                                    
 6000 continue                                                          
      goto 46000                                                       
36000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46000, 6011, 46000                                    
46000 if ( radn13(1) - 32767. )  26000, 16000, 26000                    
16000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6011                                                        
26000 ivfail = ivfail + 1                                               
      ivcomp = radn13(1)                                                
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6011 continue                                                          
      ivtnum = 601                                                      
!                                                                        
!       ****  TEST 601  ****                                             
!      TEST 601  -  LIKE TEST 600.  REAL DECIMAL CONSTANT VALUE -.32766  
!                                                                        
!                                                                        
      if (iczero) 36010, 6010, 36010                                    
 6010 continue                                                          
      goto 46010                                                       
36010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46010, 6021, 46010                                    
46010 if ( radn13(2) + .32766 )  26010, 16010, 26010                    
16010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6021                                                        
26010 ivfail = ivfail + 1                                               
      ivcomp = radn13(2)                                                
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6021 continue                                                          
      ivtnum = 602                                                      
!                                                                        
!       ****  TEST 602  ****                                             
!      TEST 602  -  LIKE TEST 600.  REAL DECIMAL CONSTANT VALUE  587.    
!                                                                        
!                                                                        
      if (iczero) 36020, 6020, 36020                                    
 6020 continue                                                          
      goto 46020                                                       
36020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46020, 6031, 46020                                    
46020 if ( radn13(3) - 587 )  26020, 16020, 26020                       
16020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6031                                                        
26020 ivfail = ivfail + 1                                               
      ivcomp = radn13(3)                                                
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6031 continue                                                          
      ivtnum = 603                                                      
!                                                                        
!       ****  TEST 603  ****                                             
!      TEST 603  -  LIKE TEST 600.  REAL DECIMAL CONSTANT VALUE 90.      
!                                                                        
!                                                                        
      if (iczero) 36030, 6030, 36030                                    
 6030 continue                                                          
      goto 46030                                                       
36030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46030, 6041, 46030                                    
46030 if ( radn13(4) - 90. )  26030, 16030, 26030                       
16030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6041                                                        
26030 ivfail = ivfail + 1                                               
      ivcomp = radn13(4)                                                
      ivcorr = 90                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6041 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM021" )                          
      end program fm021
