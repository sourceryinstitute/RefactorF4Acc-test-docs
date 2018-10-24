      program fm042
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
!      FM042                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENTS OF THE              
!      FORM      INTEGER VARIABLE =  PRIMARY ** PRIMARY                  
!      WHERE THE FIRST OF TWO PRIMARIES IS AN INTEGER VARIABLE OR AN     
!      INTEGER CONSTANT AND THE SECOND PRIMARY IS AN INTEGER VARIABLE.   
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
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
!      TEST 649 THROUGH TEST 665 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM    INTEGER VARIABLE = INTEGER CONST. ** INTEGER VAR.  
!                                                                        
!      TEST 666 THROUGH TEST 682 CONTAIN ARITHMETIC ASSIGNMENT STATEMENTS
!      OF THE FORM    INTEGER VARIABLE = INTEGER VAR. ** INTEGER VAR.    
!                                                                        
!                                                                        
      ivtnum = 649                                                      
!                                                                        
!       ****  TEST 649  ****                                             
!      TEST 649  - SMALL NUMBER BASE; ZERO EXPONENT                      
!                                                                        
      if (iczero) 36490, 6490, 36490                                    
 6490 continue                                                          
      ivon01 = 0                                                        
      ivcomp = 1 ** ivon01                                              
      goto 46490                                                       
36490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46490, 6501, 46490                                    
46490 if (ivcomp - 1) 26490,16490,26490                                 
16490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6501                                                        
26490 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6501 continue                                                          
      ivtnum = 650                                                      
!                                                                        
!       ****  TEST 650  ****                                             
!      TEST 650  - ZERO BASE TO FIRST POWER                              
!                                                                        
      if (iczero) 36500, 6500, 36500                                    
 6500 continue                                                          
      ivon01 = 1                                                        
      ivcomp = 0 ** ivon01                                              
      goto 46500                                                       
36500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46500, 6511, 46500                                    
46500 if (ivcomp) 26500,16500,26500                                     
16500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6511                                                        
26500 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6511 continue                                                          
      ivtnum = 651                                                      
!                                                                        
!       ****  TEST 651  ****                                             
!      TEST 651  - BASE =1; EXPONENT = 1                                 
!                                                                        
      if (iczero) 36510, 6510, 36510                                    
 6510 continue                                                          
      ivon01 = 1                                                        
      ivcomp = 1 ** ivon01                                              
      goto 46510                                                       
36510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46510, 6521, 46510                                    
46510 if (ivcomp - 1) 26510,16510,26510                                 
16510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6521                                                        
26510 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6521 continue                                                          
      ivtnum = 652                                                      
!                                                                        
!       ****  TEST 652  ****                                             
!      TEST 652  - LARGE EXPONENT                                        
!                                                                        
      if (iczero) 36520, 6520, 36520                                    
 6520 continue                                                          
      ivon01 = 32767                                                    
      ivcomp = 1 ** ivon01                                              
      goto 46520                                                       
36520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46520, 6531, 46520                                    
46520 if (ivcomp - 1) 26520,16520,26520                                 
16520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6531                                                        
26520 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6531 continue                                                          
      ivtnum = 653                                                      
!                                                                        
!       ****  TEST 653  ****                                             
!      TEST 653  - LARGE NUMBER BASE; EXPONENT = 1                       
!                                                                        
      if (iczero) 36530, 6530, 36530                                    
 6530 continue                                                          
      ivon01 = 1                                                        
      ivcomp = 32767 ** ivon01                                          
      goto 46530                                                       
36530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46530, 6541, 46530                                    
46530 if (ivcomp - 32767) 26530,16530,26530                             
16530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6541                                                        
26530 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6541 continue                                                          
      ivtnum = 654                                                      
!                                                                        
!       ****  TEST 654  ****                                             
!      TEST 654  - ZERO BASE; LARGE NUMBER EXPONENT                      
!                                                                        
      if (iczero) 36540, 6540, 36540                                    
 6540 continue                                                          
      ivon01 = 32767                                                    
      ivcomp = 0 ** ivon01                                              
      goto 46540                                                       
36540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46540, 6551, 46540                                    
46540 if (ivcomp) 26540,16540,26540                                     
16540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6551                                                        
26540 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6551 continue                                                          
      ivtnum = 655                                                      
!                                                                        
!       ****  TEST 655  ****                                             
!      TEST 655  -LARGE NUMBER BASE; ZERO EXPONENT                       
!                                                                        
      if (iczero) 36550, 6550, 36550                                    
 6550 continue                                                          
      ivon01 = 0                                                        
      ivcomp = 32767 ** ivon01                                          
      goto 46550                                                       
36550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46550, 6561, 46550                                    
46550 if (ivcomp -1) 26550,16550,26550                                  
16550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6561                                                        
26550 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6561 continue                                                          
      ivtnum = 656                                                      
!                                                                        
!       ****  TEST 656  ****                                             
!      TEST 656  -EXPONENT IS POWER OF TWO                               
!                                                                        
      if (iczero) 36560, 6560, 36560                                    
 6560 continue                                                          
      ivon01 = 2                                                        
      ivcomp = 181 ** ivon01                                            
      goto 46560                                                       
36560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46560, 6571, 46560                                    
46560 if (ivcomp - 32761) 26560,16560,26560                             
16560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6571                                                        
26560 ivfail = ivfail + 1                                               
      ivcorr = 32761                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6571 continue                                                          
      ivtnum = 657                                                      
!                                                                        
!       ****  TEST 657  ****                                             
!      TEST 657  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              
!                                                                        
      if (iczero) 36570, 6570, 36570                                    
 6570 continue                                                          
      ivon01 = 8                                                        
      ivcomp = 2 ** ivon01                                              
      goto 46570                                                       
36570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46570, 6581, 46570                                    
46570 if (ivcomp - 256) 26570,16570,26560                               
16570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6581                                                        
26570 ivfail = ivfail + 1                                               
      ivcorr = 256                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6581 continue                                                          
!                                                                        
!      TESTS 658 AND 659 TEST TO ENSURE EXPONENTIATION OPERATOR IS       
!                        NOT COMMUTATIVE                                 
!                                                                        
      ivtnum = 658                                                      
!                                                                        
!       ****  TEST 658  ****                                             
!                                                                        
      if (iczero) 36580, 6580, 36580                                    
 6580 continue                                                          
      ivon01 = 9                                                        
      ivcomp = 3 ** ivon01                                              
      goto 46580                                                       
36580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46580, 6591, 46580                                    
46580 if (ivcomp - 19683) 26580,16580,26580                             
16580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6591                                                        
26580 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6591 continue                                                          
      ivtnum = 659                                                      
!                                                                        
!       ****  TEST 659  ****                                             
!                                                                        
      if (iczero) 36590, 6590, 36590                                    
 6590 continue                                                          
      ivon01 = 3                                                        
      ivcomp = 9 ** ivon01                                              
      goto 46590                                                       
36590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46590, 6601, 46590                                    
46590 if (ivcomp - 729) 26590,16590,26590                               
16590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6601                                                        
26590 ivfail = ivfail + 1                                               
      ivcorr = 729                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6601 continue                                                          
!                                                                        
!      TESTS 660 THROUGH 665 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE
!                            ODD AND EVEN NUMBER POWERS CHECKING THE SIGN
!                            OF THE RESULTS                              
!                                                                        
      ivtnum = 660                                                      
!                                                                        
!       ****  TEST 660  ****                                             
!                                                                        
      if (iczero) 36600, 6600, 36600                                    
 6600 continue                                                          
      ivon01 = 2                                                        
      ivcomp = 1 ** ivon01                                              
      goto 46600                                                       
36600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46600, 6611, 46600                                    
46600 if (ivcomp - 1) 26600,16600,26600                                 
16600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6611                                                        
26600 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6611 continue                                                          
      ivtnum = 661                                                      
!                                                                        
!       ****  TEST 661  ****                                             
!                                                                        
      if (iczero) 36610, 6610, 36610                                    
 6610 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ( -1) ** ivon01                                          
      goto 46610                                                       
36610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46610, 6621, 46610                                    
46610 if (ivcomp - 1) 26610,16610,26610                                 
16610 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6621                                                        
26610 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6621 continue                                                          
      ivtnum = 662                                                      
!                                                                        
!       ****  TEST 662  ****                                             
!                                                                        
      if (iczero) 36620, 6620, 36620                                    
 6620 continue                                                          
      ivon01 = 3                                                        
      ivcomp = 7 ** ivon01                                              
      goto 46620                                                       
36620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46620, 6631, 46620                                    
46620 if (ivcomp - 343) 26620,16620,26620                               
16620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6631                                                        
26620 ivfail = ivfail + 1                                               
      ivcorr = 343                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6631 continue                                                          
      ivtnum = 663                                                      
!                                                                        
!       ****  TEST 663  ****                                             
!                                                                        
      if (iczero) 36630, 6630, 36630                                    
 6630 continue                                                          
      ivon01 = 3                                                        
      ivcomp = (-7) **ivon01                                            
      goto 46630                                                       
36630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46630, 6641, 46630                                    
46630 if (ivcomp + 343) 26630,16630,26630                               
16630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6641                                                        
26630 ivfail = ivfail + 1                                               
      ivcorr = -343                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6641 continue                                                          
      ivtnum = 664                                                      
!                                                                        
!       ****  TEST 664  ****                                             
!                                                                        
      if (iczero) 36640, 6640, 36640                                    
 6640 continue                                                          
      ivon01 = 4                                                        
      ivcomp = 7 ** ivon01                                              
      goto 46640                                                       
36640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46640, 6651, 46640                                    
46640 if (ivcomp - 2401) 26640,16640,26640                              
16640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6651                                                        
26640 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6651 continue                                                          
      ivtnum = 665                                                      
!                                                                        
!       ****  TEST 665  ****                                             
!                                                                        
      if (iczero) 36650, 6650, 36650                                    
 6650 continue                                                          
      ivon01 = 4                                                        
      ivcomp = (-7) ** ivon01                                           
      goto 46650                                                       
36650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46650, 6661, 46650                                    
46650 if (ivcomp - 2401) 26650,16650,26650                              
16650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6661                                                        
26650 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6661 continue                                                          
      ivtnum = 666                                                      
!                                                                        
!       ****  TEST 666  ****                                             
!      TEST 666  - SMALL NUMBER BASE; ZERO EXPONENT                      
!                                                                        
      if (iczero) 36660, 6660, 36660                                    
 6660 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 0                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46660                                                       
36660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46660, 6671, 46660                                    
46660 if (ivcomp - 1) 26660,16660,26660                                 
16660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6671                                                        
26660 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6671 continue                                                          
      ivtnum = 667                                                      
!                                                                        
!       ****  TEST 667  ****                                             
!      TEST 667  - ZERO BASE TO FIRST POWER                              
!                                                                        
      if (iczero) 36670, 6670, 36670                                    
 6670 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 1                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46670                                                       
36670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46670, 6681, 46670                                    
46670 if (ivcomp) 26670,16670,26670                                     
16670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6681                                                        
26670 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6681 continue                                                          
      ivtnum = 668                                                      
!                                                                        
!       ****  TEST 668  ****                                             
!      TEST 668  - BASE =1; EXPONENT = 1                                 
!                                                                        
      if (iczero) 36680, 6680, 36680                                    
 6680 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 1                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46680                                                       
36680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46680, 6691, 46680                                    
46680 if (ivcomp - 1) 26680,16680,26680                                 
16680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6691                                                        
26680 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6691 continue                                                          
      ivtnum = 669                                                      
!                                                                        
!       ****  TEST 669  ****                                             
!      TEST 669  - LARGE EXPONENT                                        
!                                                                        
      if (iczero) 36690, 6690, 36690                                    
 6690 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 32767                                                    
      ivcomp = ivon01 ** ivon02                                         
      goto 46690                                                       
36690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46690, 6701, 46690                                    
46690 if (ivcomp - 1) 26690,16690,26690                                 
16690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6701                                                        
26690 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6701 continue                                                          
      ivtnum = 670                                                      
!                                                                        
!       ****  TEST 670  ****                                             
!      TEST 670  - LARGE NUMBER BASE; EXPONENT = 1                       
!                                                                        
      if (iczero) 36700, 6700, 36700                                    
 6700 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 1                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46700                                                       
36700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46700, 6711, 46700                                    
46700 if (ivcomp - 32767) 26700,16700,26700                             
16700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6711                                                        
26700 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6711 continue                                                          
      ivtnum = 671                                                      
!                                                                        
!       ****  TEST 671  ****                                             
!      TEST 671  - ZERO BASE; LARGE NUMBER EXPONENT                      
!                                                                        
      if (iczero) 36710, 6710, 36710                                    
 6710 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 32767                                                    
      ivcomp = ivon01 ** ivon02                                         
      goto 46710                                                       
36710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46710, 6721, 46710                                    
46710 if (ivcomp) 26710,16710,26710                                     
16710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6721                                                        
26710 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6721 continue                                                          
      ivtnum = 672                                                      
!                                                                        
!       ****  TEST 672  ****                                             
!      TEST 672  -LARGE NUMBER BASE; ZERO EXPONENT                       
!                                                                        
      if (iczero) 36720, 6720, 36720                                    
 6720 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 0                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46720                                                       
36720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46720, 6731, 46720                                    
46720 if (ivcomp -1) 26720,16720,26720                                  
16720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6731                                                        
26720 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6731 continue                                                          
      ivtnum = 673                                                      
!                                                                        
!       ****  TEST 673  ****                                             
!      TEST 673  -EXPONENT IS POWER OF TWO                               
!                                                                        
      if (iczero) 36730, 6730, 36730                                    
 6730 continue                                                          
      ivon01 = 181                                                      
      ivon02 = 2                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46730                                                       
36730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46730, 6741, 46730                                    
46730 if (ivcomp - 32761) 26730,16730,26730                             
16730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6741                                                        
26730 ivfail = ivfail + 1                                               
      ivcorr = 32761                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6741 continue                                                          
      ivtnum = 674                                                      
!                                                                        
!       ****  TEST 674  ****                                             
!      TEST 674  - BASE AND EXPONENT ARE BOTH POWERS OF TWO              
!                                                                        
      if (iczero) 36740, 6740, 36740                                    
 6740 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 8                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46740                                                       
36740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46740, 6751, 46740                                    
46740 if (ivcomp - 256) 26740,16740,26740                               
16740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6751                                                        
26740 ivfail = ivfail + 1                                               
      ivcorr = 256                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6751 continue                                                          
!                                                                        
!      TESTS 675 AND 676 TEST TO ENSURE EXPONENTIATION OPERATOR IS       
!                        NOT COMMUTATIVE                                 
!                                                                        
      ivtnum = 675                                                      
!                                                                        
!       ****  TEST 675  ****                                             
!                                                                        
      if (iczero) 36750, 6750, 36750                                    
 6750 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 9                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46750                                                       
36750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46750, 6761, 46750                                    
46750 if (ivcomp - 19683) 26750,16750,26750                             
16750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6761                                                        
26750 ivfail = ivfail + 1                                               
      ivcorr = 19683                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6761 continue                                                          
      ivtnum = 676                                                      
!                                                                        
!       ****  TEST 676  ****                                             
!                                                                        
      if (iczero) 36760, 6760, 36760                                    
 6760 continue                                                          
      ivon01 = 9                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46760                                                       
36760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46760, 6771, 46760                                    
46760 if (ivcomp - 729) 26760,16760,26760                               
16760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6771                                                        
26760 ivfail = ivfail + 1                                               
      ivcorr = 729                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6771 continue                                                          
!                                                                        
!      TESTS 677 THROUGH 682 TEST POSITIVE AND NEGATIVE BASES TO POSITIVE
!                            ODD AND EVEN NUMBER POWERS CHECKING THE SIGN
!                            OF THE RESULTS                              
!                                                                        
      ivtnum = 677                                                      
!                                                                        
!       ****  TEST 677  ****                                             
!                                                                        
      if (iczero) 36770, 6770, 36770                                    
 6770 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 2                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46770                                                       
36770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46770, 6781, 46770                                    
46770 if (ivcomp - 1) 26770,16770,26770                                 
16770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6781                                                        
26770 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6781 continue                                                          
      ivtnum = 678                                                      
!                                                                        
!       ****  TEST 678  ****                                             
!                                                                        
      if (iczero) 36780, 6780, 36780                                    
 6780 continue                                                          
      ivon01 = -1                                                       
      ivon02 = 2                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46780                                                       
36780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46780, 6791, 46780                                    
46780 if (ivcomp - 1) 26780,16780,26780                                 
16780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6791                                                        
26780 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6791 continue                                                          
      ivtnum = 679                                                      
!                                                                        
!       ****  TEST 679  ****                                             
!                                                                        
      if (iczero) 36790, 6790, 36790                                    
 6790 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 3                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46790                                                       
36790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46790, 6801, 46790                                    
46790 if (ivcomp - 343) 26790,16790,26790                               
16790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6801                                                        
26790 ivfail = ivfail + 1                                               
      ivcorr = 343                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6801 continue                                                          
      ivtnum = 680                                                      
!                                                                        
!       ****  TEST 680  ****                                             
!                                                                        
      if (iczero) 36800, 6800, 36800                                    
 6800 continue                                                          
      ivon01 = -7                                                       
      ivon02 = 3                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46800                                                       
36800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46800, 6811, 46800                                    
46800 if (ivcomp + 343) 26800,16800,26800                               
16800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6811                                                        
26800 ivfail = ivfail + 1                                               
      ivcorr = -343                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6811 continue                                                          
      ivtnum = 681                                                      
!                                                                        
!       ****  TEST 681  ****                                             
!                                                                        
      if (iczero) 36810, 6810, 36810                                    
 6810 continue                                                          
      ivon01 = 7                                                        
      ivon02 = 4                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46810                                                       
36810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46810, 6821, 46810                                    
46810 if (ivcomp - 2401) 26810,16810,26810                              
16810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6821                                                        
26810 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6821 continue                                                          
      ivtnum = 682                                                      
!                                                                        
!       ****  TEST 682  ****                                             
!                                                                        
      if (iczero) 36820, 6820, 36820                                    
 6820 continue                                                          
      ivon01 = -7                                                       
      ivon02 = 4                                                        
      ivcomp = ivon01 ** ivon02                                         
      goto 46820                                                       
36820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46820, 6831, 46820                                    
46820 if (ivcomp - 2401) 26820,16820,26820                              
16820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6831                                                        
26820 ivfail = ivfail + 1                                               
      ivcorr = 2401                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6831 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM042" )                          
      end program fm042
