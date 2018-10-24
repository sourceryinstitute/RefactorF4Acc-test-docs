      program fm043
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivcomp
      integer :: ivcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM043                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE FORM         
!                                                                        
!      INTEGER VAR. = INTEGER VAR. <OP1> INTEGER VAR. <OP2> INTEGER VAR. 
!                                                                        
!      WHERE <OP1> AND <OP2> ARE ARITHMETIC OPERATORS, BUT <OP1> IS      
!      NOT THE SAME AS <OP2>.                                            
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 6.6, EVALUATION OF EXPRESSIONS                         
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENT                  
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
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!      TESTS 683 THROUGH 694 TEST STATEMENTS WHERE <OP1> IS '+' AND      
!      <OP2> VARIES.                                                     
!                                                                        
!      TEST 695 THROUGH 706 TEST STATEMENTS WHERE <OP1> IS '-' AND       
!      <OP2> VARIES.                                                     
!                                                                        
!      TESTS 707 THROUGH 718 TEST STATEMENTS WHERE <OP1> IS '*' AND      
!      <OP2> VARIES.                                                     
!                                                                        
!                                                                        
!                                                                        
!      TESTS 683 THROUGH  685 TEST '+' FOLLOWED BY '-'.                  
!                                                                        
      ivtnum = 683                                                      
!                                                                        
!       ****  TEST 683  ****                                             
!                                                                        
      if (iczero) 36830, 6830, 36830                                    
 6830 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 + ivon02 - ivon03                                 
      goto 46830                                                       
36830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46830, 6841, 46830                                    
46830 if (ivcomp - 51) 26830,16830,26830                                
16830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6841                                                        
26830 ivfail = ivfail + 1                                               
      ivcorr = 51                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6841 continue                                                          
      ivtnum = 684                                                      
!                                                                        
!       ****  TEST 684  ****                                             
!                                                                        
      if (iczero) 36840, 6840, 36840                                    
 6840 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 + ivon02) - ivon03                               
      goto 46840                                                       
36840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46840, 6851, 46840                                    
46840 if (ivcomp - 51) 26840,16840,26840                                
16840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6851                                                        
26840 ivfail = ivfail + 1                                               
      ivcorr = 51                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6851 continue                                                          
      ivtnum = 685                                                      
!                                                                        
!       ****  TEST 685  ****                                             
!                                                                        
      if (iczero) 36850, 6850, 36850                                    
 6850 continue                                                          
      ivon01 = 45                                                       
      ivon02 = 9                                                        
      ivon03 = 3                                                        
      ivcomp = ivon01 + (ivon02 - ivon03)                               
      goto 46850                                                       
36850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46850, 6861, 46850                                    
46850 if (ivcomp - 51) 26850,16850,26850                                
16850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6861                                                        
26850 ivfail = ivfail + 1                                               
      ivcorr = 51                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6861 continue                                                          
!                                                                        
!      TESTS 686 THROUGH 688 TEST '+' FOLLOWED BY '*'.                   
!                                                                        
      ivtnum = 686                                                      
!                                                                        
!       ****  TEST 686  ****                                             
!                                                                        
      if (iczero) 36860, 6860, 36860                                    
 6860 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp =  ivon01 + ivon02 * ivon03                                
      goto 46860                                                       
36860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46860, 6871, 46860                                    
46860 if (ivcomp - 72) 26860,16860,26860                                
16860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6871                                                        
26860 ivfail = ivfail + 1                                               
      ivcorr = 72                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6871 continue                                                          
      ivtnum = 687                                                      
!                                                                        
!       ****  TEST 687  ****                                             
!                                                                        
      if (iczero) 36870, 6870, 36870                                    
 6870 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 + ivon02) * ivon03                               
      goto 46870                                                       
36870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46870, 6881, 46870                                    
46870 if (ivcomp - 162) 26870,16870,26870                               
16870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6881                                                        
26870 ivfail = ivfail + 1                                               
      ivcorr = 162                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6881 continue                                                          
      ivtnum = 688                                                      
!                                                                        
!       ****  TEST 688  ****                                             
!                                                                        
      if (iczero) 36880, 6880, 36880                                    
 6880 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 = 3                                                        
      ivcomp = ivon01 + (ivon02 * ivon03)                               
      goto 46880                                                       
36880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46880, 6891, 46880                                    
46880 if (ivcomp - 72) 26880,16880,26880                                
16880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6891                                                        
26880 ivfail = ivfail + 1                                               
      ivcorr = 72                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6891 continue                                                          
!                                                                        
!      TESTS 689 THROUGH 691 TEST '+' FOLLOWED BY '/'.                   
!                                                                        
      ivtnum = 689                                                      
!                                                                        
!       ****  TEST 689  ****                                             
!                                                                        
      if (iczero) 36890, 6890, 36890                                    
 6890 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 = 3                                                        
      ivcomp = ivon01 + ivon02 / ivon03                                 
      goto 46890                                                       
36890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46890, 6901, 46890                                    
46890 if (ivcomp - 48) 26890,16890,26890                                
16890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6901                                                        
26890 ivfail = ivfail + 1                                               
      ivcorr = 48                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6901 continue                                                          
      ivtnum = 690                                                      
!                                                                        
!       ****  TEST 690  ****                                             
!                                                                        
      if (iczero) 36900, 6900, 36900                                    
 6900 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 + ivon02) / ivon03                               
      goto 46900                                                       
36900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46900, 6911, 46900                                    
46900 if (ivcomp - 18) 26900,16900,26900                                
16900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6911                                                        
26900 ivfail = ivfail + 1                                               
      ivcorr = 18                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6911 continue                                                          
      ivtnum = 691                                                      
!                                                                        
!       ****  TEST 691  ****                                             
!                                                                        
      if (iczero) 36910, 6910, 36910                                    
 6910 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 + (ivon02 / ivon03)                               
      goto 46910                                                       
36910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46910, 6921, 46910                                    
46910 if (ivcomp - 48) 26910,16910,26910                                
16910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6921                                                        
26910 ivfail = ivfail + 1                                               
      ivcorr = 48                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6921 continue                                                          
!                                                                        
!      TESTS 692 THROUGH 694 TEST '+' FOLLOWED BY '**'.                  
!                                                                        
      ivtnum = 692                                                      
!                                                                        
!       ****  TEST 692  ****                                             
!                                                                        
      if (iczero) 36920, 6920, 36920                                    
 6920 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 + ivon02 ** ivon03                                
      goto 46920                                                       
36920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46920, 6931, 46920                                    
46920 if (ivcomp - 744) 26920,16920,26920                               
16920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6931                                                        
26920 ivfail = ivfail + 1                                               
      ivcorr = 744                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6931 continue                                                          
      ivtnum = 693                                                      
!                                                                        
!       ****  TEST 693  ****                                             
!                                                                        
      if (iczero) 36930, 6930, 36930                                    
 6930 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 + ivon02) ** ivon03                              
      goto 46930                                                       
36930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46930, 6941, 46930                                    
46930 if (ivcomp - 13824) 26930,16930,26930                             
16930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6941                                                        
26930 ivfail = ivfail + 1                                               
      ivcorr = 13824                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6941 continue                                                          
      ivtnum = 694                                                      
!                                                                        
!       ****  TEST 694  ****                                             
!                                                                        
      if (iczero) 36940, 6940, 36940                                    
 6940 continue                                                          
      ivon01 = 15                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 + (ivon02 ** ivon03)                              
      goto 46940                                                       
36940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46940, 6951, 46940                                    
46940 if (ivcomp - 744) 26940,16940,26940                               
16940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6951                                                        
26940 ivfail = ivfail + 1                                               
      ivcorr = 744                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6951 continue                                                          
!                                                                        
!      TESTS 695 THROUGH 697 TEST '-' FOLLOWED BY '+'.                   
!                                                                        
      ivtnum = 695                                                      
!                                                                        
!       ****  TEST 695  ****                                             
!                                                                        
      if (iczero) 36950, 6950, 36950                                    
 6950 continue                                                          
      ivon01 =  45                                                      
      ivon02 =   9                                                      
      ivon03 =   3                                                      
      ivcomp = ivon01 - ivon02 + ivon03                                 
      goto 46950                                                       
36950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46950, 6961, 46950                                    
46950 if (ivcomp - 39) 26950,16950,26950                                
16950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6961                                                        
26950 ivfail = ivfail + 1                                               
      ivcorr = 39                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6961 continue                                                          
      ivtnum = 696                                                      
!                                                                        
!       ****  TEST 696  ****                                             
!                                                                        
      if (iczero) 36960, 6960, 36960                                    
 6960 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 - ivon02) + ivon03                               
      goto 46960                                                       
36960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46960, 6971, 46960                                    
46960 if (ivcomp - 39) 26960,16960,26960                                
16960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6971                                                        
26960 ivfail = ivfail + 1                                               
      ivcorr = 39                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6971 continue                                                          
      ivtnum = 697                                                      
!                                                                        
!       ****  TEST 697  ****                                             
!                                                                        
      if (iczero) 36970, 6970, 36970                                    
 6970 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 - (ivon02 + ivon03)                               
      goto 46970                                                       
36970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46970, 6981, 46970                                    
46970 if (ivcomp - 33) 26970,16970,26970                                
16970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6981                                                        
26970 ivfail = ivfail + 1                                               
      ivcorr = 33                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6981 continue                                                          
!                                                                        
!      TESTS 698 THROUGH 700 TEST '-' FOLLOWED BY '*'.                   
!                                                                        
      ivtnum = 698                                                      
!                                                                        
!       ****  TEST 698  ****                                             
!                                                                        
      if (iczero) 36980, 6980, 36980                                    
 6980 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp =  ivon01 - ivon02 * ivon03                                
      goto 46980                                                       
36980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46980, 6991, 46980                                    
46980 if (ivcomp - 18) 26980,16980,26980                                
16980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6991                                                        
26980 ivfail = ivfail + 1                                               
      ivcorr = 18                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6991 continue                                                          
      ivtnum = 699                                                      
!                                                                        
!       ****  TEST 699  ****                                             
!                                                                        
      if (iczero) 36990, 6990, 36990                                    
 6990 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 - ivon02) * ivon03                               
      goto 46990                                                       
36990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46990, 7001, 46990                                    
46990 if (ivcomp - 108) 26990,16990,26990                               
16990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7001                                                        
26990 ivfail = ivfail + 1                                               
      ivcorr = 108                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7001 continue                                                          
      ivtnum = 700                                                      
!                                                                        
!       ****  TEST 700  ****                                             
!                                                                        
      if (iczero) 37000, 7000, 37000                                    
 7000 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 - (ivon02 * ivon03)                               
      goto 47000                                                       
37000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47000, 7011, 47000                                    
47000 if (ivcomp - 18) 27000,17000,27000                                
17000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7011                                                        
27000 ivfail = ivfail + 1                                               
      ivcorr = 18                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7011 continue                                                          
!                                                                        
!      TESTS 701 THROUGH 703 TEST '-' FOLLOWED BY '/'.                   
!                                                                        
      ivtnum = 701                                                      
!                                                                        
!       ****  TEST 701  ****                                             
!                                                                        
      if (iczero) 37010, 7010, 37010                                    
 7010 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 - ivon02 / ivon03                                 
      goto 47010                                                       
37010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47010, 7021, 47010                                    
47010 if (ivcomp - 42) 27010,17010,27010                                
17010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7021                                                        
27010 ivfail = ivfail + 1                                               
      ivcorr = 42                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7021 continue                                                          
      ivtnum = 702                                                      
!                                                                        
!       ****  TEST 702  ****                                             
!                                                                        
      if (iczero) 37020, 7020, 37020                                    
 7020 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 - ivon02) / ivon03                               
      goto 47020                                                       
37020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47020, 7031, 47020                                    
47020 if (ivcomp - 12) 27020,17020,27020                                
17020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7031                                                        
27020 ivfail = ivfail + 1                                               
      ivcorr = 12                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7031 continue                                                          
      ivtnum = 703                                                      
!                                                                        
!       ****  TEST 703  ****                                             
!                                                                        
      if (iczero) 37030, 7030, 37030                                    
 7030 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 - (ivon02 / ivon03)                               
      goto 47030                                                       
37030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47030, 7041, 47030                                    
47030 if (ivcomp - 42) 27030,17030,27030                                
17030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7041                                                        
27030 ivfail = ivfail + 1                                               
      ivcorr = 42                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7041 continue                                                          
!                                                                        
!      TESTS 704 THROUGH 706 TEST '-' FOLLOWED BY '**'.                  
!                                                                        
      ivtnum = 704                                                      
!                                                                        
!       ****  TEST 704  ****                                             
!                                                                        
      if (iczero) 37040, 7040, 37040                                    
 7040 continue                                                          
      ivon01 = 35                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 - ivon02 ** ivon03                                
      goto 47040                                                       
37040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47040, 7051, 47040                                    
47040 if (ivcomp + 694) 27040,17040,27040                               
17040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7051                                                        
27040 ivfail = ivfail + 1                                               
      ivcorr = -694                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7051 continue                                                          
      ivtnum = 705                                                      
!                                                                        
!       ****  TEST 705  ****                                             
!                                                                        
      if (iczero) 37050, 7050, 37050                                    
 7050 continue                                                          
      ivon01 = 35                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 - ivon02) ** ivon03                              
      goto 47050                                                       
37050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47050, 7061, 47050                                    
47050 if (ivcomp - 17576) 27050,17050,27050                             
17050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7061                                                        
27050 ivfail = ivfail + 1                                               
      ivcorr = 17576                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7061 continue                                                          
      ivtnum = 706                                                      
!                                                                        
!       ****  TEST 706  ****                                             
!                                                                        
      if (iczero) 37060, 7060, 37060                                    
 7060 continue                                                          
      ivon01 = 35                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 - (ivon02 ** ivon03)                              
      goto 47060                                                       
37060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47060, 7071, 47060                                    
47060 if (ivcomp + 694) 27060,17060,27060                               
17060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7071                                                        
27060 ivfail = ivfail + 1                                               
      ivcorr = -694                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7071 continue                                                          
!                                                                        
!      TESTS 707 THROUGH 709 TEST '*' FOLLOWED BY '+'.                   
!                                                                        
      ivtnum = 707                                                      
!                                                                        
!       ****  TEST 707  ****                                             
!                                                                        
      if (iczero) 37070, 7070, 37070                                    
 7070 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp =  ivon01 * ivon02 + ivon03                                
      goto 47070                                                       
37070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47070, 7081, 47070                                    
47070 if (ivcomp - 408) 27070,17070,27070                               
17070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7081                                                        
27070 ivfail = ivfail + 1                                               
      ivcorr = 408                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7081 continue                                                          
      ivtnum = 708                                                      
!                                                                        
!       ****  TEST 708  ****                                             
!                                                                        
      if (iczero) 37080, 7080, 37080                                    
 7080 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 * ivon02) + ivon03                               
      goto 47080                                                       
37080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47080, 7091, 47080                                    
47080 if (ivcomp - 408) 27080,17080,27080                               
17080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7091                                                        
27080 ivfail = ivfail + 1                                               
      ivcorr = 408                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7091 continue                                                          
      ivtnum = 709                                                      
!                                                                        
!       ****  TEST 709  ****                                             
!                                                                        
      if (iczero) 37090, 7090, 37090                                    
 7090 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 * (ivon02 + ivon03)                               
      goto 47090                                                       
37090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47090, 7101, 47090                                    
47090 if (ivcomp - 540) 27090,17090,27090                               
17090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7101                                                        
27090 ivfail = ivfail + 1                                               
      ivcorr = 540                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7101 continue                                                          
!                                                                        
!      TESTS 710 THROUGH 712 TEST '*' FOLLOWED BY '-'.                   
!                                                                        
      ivtnum = 710                                                      
!                                                                        
!       ****  TEST 710  ****                                             
!                                                                        
      if (iczero) 37100, 7100, 37100                                    
 7100 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 * ivon02 - ivon03                                 
      goto 47100                                                       
37100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47100, 7111, 47100                                    
47100 if (ivcomp - 402) 27100,17100,27100                               
17100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7111                                                        
27100 ivfail = ivfail + 1                                               
      ivcorr = 402                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7111 continue                                                          
      ivtnum = 711                                                      
!                                                                        
!       ****  TEST 711  ****                                             
!                                                                        
      if (iczero) 37110, 7110, 37110                                    
 7110 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 * ivon02) - ivon03                               
      goto 47110                                                       
37110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47110, 7121, 47110                                    
47110 if (ivcomp - 402) 27110,17110,27110                               
17110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7121                                                        
27110 ivfail = ivfail + 1                                               
      ivcorr = 402                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7121 continue                                                          
      ivtnum = 712                                                      
!                                                                        
!       ****  TEST 712  ****                                             
!                                                                        
      if (iczero) 37120, 7120, 37120                                    
 7120 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 * (ivon02 - ivon03)                               
      goto 47120                                                       
37120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47120, 7131, 47120                                    
47120 if (ivcomp - 270) 27120,17120,27120                               
17120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7131                                                        
27120 ivfail = ivfail + 1                                               
      ivcorr = 270                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7131 continue                                                          
!                                                                        
!      TESTS 713 THROUGH 715 TEST '*' FOLLOWED BY '/'.                   
!                                                                        
      ivtnum = 713                                                      
!                                                                        
!       ****  TEST 713  ****                                             
!                                                                        
      if (iczero) 37130, 7130, 37130                                    
 7130 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 * ivon02 / ivon03                                 
      goto 47130                                                       
37130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47130, 7141, 47130                                    
47130 if (ivcomp - 135) 27130,17130,27130                               
17130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7141                                                        
27130 ivfail = ivfail + 1                                               
      ivcorr = 135                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7141 continue                                                          
      ivtnum = 714                                                      
!                                                                        
!       ****  TEST 714  ****                                             
!                                                                        
      if (iczero) 37140, 7140, 37140                                    
 7140 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = (ivon01 * ivon02) / ivon03                               
      goto 47140                                                       
37140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47140, 7151, 47140                                    
47140 if (ivcomp - 135) 27140,17140,27140                               
17140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7151                                                        
27140 ivfail = ivfail + 1                                               
      ivcorr = 135                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7151 continue                                                          
      ivtnum = 715                                                      
!                                                                        
!       ****  TEST 715  ****                                             
!                                                                        
      if (iczero) 37150, 7150, 37150                                    
 7150 continue                                                          
      ivon01 = 45                                                       
      ivon02 =  9                                                       
      ivon03 =  3                                                       
      ivcomp = ivon01 * (ivon02 / ivon03)                               
      goto 47150                                                       
37150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47150, 7161, 47150                                    
47150 if (ivcomp - 135) 27150,17150,27150                               
17150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7161                                                        
27150 ivfail = ivfail + 1                                               
      ivcorr = 135                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7161 continue                                                          
!                                                                        
!      TESTS 716 THROUGH 718 TEST '*' FOLLOWED BY '**'.                  
!                                                                        
      ivtnum = 716                                                      
!                                                                        
!       ****  TEST 716  ****                                             
!                                                                        
      if (iczero) 37160, 7160, 37160                                    
 7160 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivon03 = 3                                                        
      ivcomp = ivon01 * ivon02  ** ivon03                               
      goto 47160                                                       
37160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47160, 7171, 47160                                    
47160 if (ivcomp - 189) 27160,17160,27160                               
17160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7171                                                        
27160 ivfail = ivfail + 1                                               
      ivcorr = 189                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7171 continue                                                          
      ivtnum = 717                                                      
!                                                                        
!       ****  TEST 717  ****                                             
!                                                                        
      if (iczero) 37170, 7170, 37170                                    
 7170 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivon03 = 3                                                        
      ivcomp = (ivon01 * ivon02) ** ivon03                              
      goto 47170                                                       
37170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47170, 7181, 47170                                    
47170 if (ivcomp - 9261) 27170,17170,27170                              
17170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7181                                                        
27170 ivfail = ivfail + 1                                               
      ivcorr = 9261                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7181 continue                                                          
      ivtnum = 718                                                      
!                                                                        
!       ****  TEST 718  ****                                             
!                                                                        
      if (iczero) 37180, 7180, 37180                                    
 7180 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivon03 = 3                                                        
      ivcomp = ivon01 * (ivon02 ** ivon03)                              
      goto 47180                                                       
37180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 47180, 7191, 47180                                    
47180 if (ivcomp - 189) 27180,17180,27180                               
17180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 7191                                                        
27180 ivfail = ivfail + 1                                               
      ivcorr = 189                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 7191 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM043" )                          
      end program fm043
