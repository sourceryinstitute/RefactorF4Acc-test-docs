      program fm098
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
      real :: rvon01
      integer :: ivon02
      integer :: ivon03
      real :: rvon02
      real :: rvon03
      real :: rvon04
!      COMMENT SECTION                                                   
!                                                                        
!      FM098                                                             
!                                                                        
!      THIS ROUTINE TESTS INTRINSIC FUNCTIONS WHERE THE FUNCTION TYPE IS 
!      INTEGER AND THE ARGUMENTS ARE EITHER INTEGER OR REAL.  THE REAL   
!      AND INTEGER VARIABLES AND THE REAL AND INTEGER CONSTANTS CONTAIN  
!      BOTH POSITIVE AND NEGATIVE VALUES.  THE INTRINSIC FUNCTIONS TESTED
!      BY FM098 INCLUDE                                                  
!                                                      TYPE OF           
!        INTRINSIC FUNCTION          NAME       ARGUMENT     FUNCTION    
!        ------------------          ----       --------     --------    
!          ABSOLUTE VALUE            IABS       INTEGER      INTEGER     
!          TRUNCATION                INT        REAL         INTEGER     
!          REMAINDERING              MOD        INTEGER      INTEGER     
!          CHOOSING LARGEST VALUE    MAX0       INTEGER      INTEGER     
!                                    MAX1       REAL         INTEGER     
!          CHOOSING SMALLEST VALUE   MIN0       INTEGER      INTEGER     
!                                    MIN1       REAL         INTEGER     
!          FIX                       IFIX      REAL          INTEGER     
!          TRANSFER OF SIGN          ISIGN     INTEGER       INTEGER     
!          POSITIVE DIFFERENCE       IDIM      INTEGER       INTEGER     
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS   
!         SECTION 15.3, INTRINSIC FUNCTION                               
!         SECTION 15.3.2, INTRINSIC FUNCTIONS AND THEIR REFERENCE        
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
!      TEST 907 THROUGH TEST 909 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      ABSOLUTE VALUE WHERE ARGUMENT AND FUNCTION ARE INTEGER            
!                                                                        
 9071 continue                                                          
      ivtnum = 907                                                      
!                                                                        
!       ****  TEST 907  ****                                             
!                                                                        
      if (iczero) 39070, 9070, 39070                                    
 9070 continue                                                          
      ivcomp = iabs (-382)                                              
      goto 49070                                                       
39070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49070, 9081, 49070                                    
49070 if (ivcomp - 382) 29070,19070,29070                               
19070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9081                                                        
29070 ivfail = ivfail + 1                                               
      ivcorr = 382                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9081 continue                                                          
      ivtnum = 908                                                      
!                                                                        
!       ****  TEST 908  ****                                             
!                                                                        
      if (iczero) 39080, 9080, 39080                                    
 9080 continue                                                          
      ivon01 = 445                                                      
      ivcomp = iabs (ivon01)                                            
      goto 49080                                                       
39080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49080, 9091, 49080                                    
49080 if (ivcomp - 445) 29080,19080,29080                               
19080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9091                                                        
29080 ivfail = ivfail + 1                                               
      ivcorr = 445                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9091 continue                                                          
      ivtnum = 909                                                      
!                                                                        
!       ****  TEST 909  ****                                             
!                                                                        
      if (iczero) 39090, 9090, 39090                                    
 9090 continue                                                          
      ivon01 = -32176                                                   
      ivcomp = iabs (ivon01)                                            
      goto 49090                                                       
39090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49090, 9101, 49090                                    
49090 if (ivcomp - 32176) 29090,19090,29090                             
19090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9101                                                        
29090 ivfail = ivfail + 1                                               
      ivcorr = 32176                                                    
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 910 THROUGH TEST 913 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      TRUNCATION WHERE ARGUMENT IS REAL AND FUNCTION IS INTEGER         
!                                                                        
 9101 continue                                                          
      ivtnum = 910                                                      
!                                                                        
!       ****  TEST 910  ****                                             
!                                                                        
      if (iczero) 39100, 9100, 39100                                    
 9100 continue                                                          
      ivcomp = int (38.2)                                               
      goto 49100                                                       
39100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49100, 9111, 49100                                    
49100 if (ivcomp - 38) 29100,19100,29100                                
19100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9111                                                        
29100 ivfail = ivfail + 1                                               
      ivcorr = 38                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9111 continue                                                          
      ivtnum = 911                                                      
!                                                                        
!       ****  TEST 911  ****                                             
!                                                                        
      if (iczero) 39110, 9110, 39110                                    
 9110 continue                                                          
      rvon01 = -445.95                                                  
      ivcomp = int (rvon01)                                             
      goto 49110                                                       
39110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49110, 9121, 49110                                    
49110 if (ivcomp + 445) 29110,19110,29110                               
19110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9121                                                        
29110 ivfail = ivfail + 1                                               
      ivcorr = -445                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9121 continue                                                          
      ivtnum = 912                                                      
!                                                                        
!       ****  TEST 912  ****                                             
!                                                                        
      if (iczero) 39120, 9120, 39120                                    
 9120 continue                                                          
      rvon01 = 466.01                                                   
      ivcomp = int (rvon01)                                             
      goto 49120                                                       
39120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49120, 9131, 49120                                    
49120 if (ivcomp - 466) 29120,19120,29120                               
19120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9131                                                        
29120 ivfail = ivfail + 1                                               
      ivcorr = 466                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9131 continue                                                          
      ivtnum = 913                                                      
!                                                                        
!       ****  TEST 913  ****                                             
!                                                                        
      if (iczero) 39130, 9130, 39130                                    
 9130 continue                                                          
      rvon01 = 382e-1                                                   
      ivcomp = int (rvon01)                                             
      goto 49130                                                       
39130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49130, 9141, 49130                                    
49130 if (ivcomp - 38) 29130,19130,29130                                
19130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9141                                                        
29130 ivfail = ivfail + 1                                               
      ivcorr = 38                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 914 THROUGH TEST 917 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      REMAINDERING WHERE ARGUMENTS AND FUNCTION ARE INTEGERS            
!                                                                        
 9141 continue                                                          
      ivtnum = 914                                                      
!                                                                        
!       ****  TEST 914  ****                                             
!                                                                        
      if (iczero) 39140, 9140, 39140                                    
 9140 continue                                                          
      ivcomp = mod (42,19)                                              
      goto 49140                                                       
39140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49140, 9151, 49140                                    
49140 if (ivcomp - 4) 29140,19140,29140                                 
19140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9151                                                        
29140 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9151 continue                                                          
      ivtnum = 915                                                      
!                                                                        
!       ****  TEST 915  ****                                             
!                                                                        
      if (iczero) 39150, 9150, 39150                                    
 9150 continue                                                          
      ivon01 = 6667                                                     
      ivon02 = 2                                                        
      ivcomp = mod (ivon01,ivon02)                                      
      goto 49150                                                       
39150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49150, 9161, 49150                                    
49150 if (ivcomp - 1) 29150,19150,29150                                 
19150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9161                                                        
29150 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9161 continue                                                          
      ivtnum = 916                                                      
!                                                                        
!       ****  TEST 916  ****                                             
!                                                                        
      if (iczero) 39160, 9160, 39160                                    
 9160 continue                                                          
      ivon01 = 225                                                      
      ivon02 = 50                                                       
      ivcomp = mod (ivon01,ivon02)                                      
      goto 49160                                                       
39160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49160, 9171, 49160                                    
49160 if (ivcomp - 25) 29160,19160,29160                                
19160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9171                                                        
29160 ivfail = ivfail + 1                                               
      ivcorr = 25                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9171 continue                                                          
      ivtnum = 917                                                      
!                                                                        
!       ****  TEST 917  ****                                             
!                                                                        
      if (iczero) 39170, 9170, 39170                                    
 9170 continue                                                          
      ivon01 = -39                                                      
      ivon02 = 500                                                      
      ivcomp = mod (ivon01,ivon02)                                      
      goto 49170                                                       
39170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49170, 9181, 49170                                    
49170 if (ivcomp + 39) 29170,19170,29170                                
19170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9181                                                        
29170 ivfail = ivfail + 1                                               
      ivcorr = -39                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 918 AND 919 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    
!      LARGEST VALUE WHERE ARGUMENTS AND FUNCTION ARE INTEGER            
!                                                                        
 9181 continue                                                          
      ivtnum = 918                                                      
!                                                                        
!       ****  TEST 918  ****                                             
!                                                                        
      if (iczero) 39180, 9180, 39180                                    
 9180 continue                                                          
      ivon01 = 317                                                      
      ivon02 = -99                                                      
      ivon03 = 1                                                        
      ivcomp = max0 (263,ivon01,ivon02,ivon03)                          
      goto 49180                                                       
39180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49180, 9191, 49180                                    
49180 if (ivcomp - 317) 29180,19180,29180                               
19180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9191                                                        
29180 ivfail = ivfail + 1                                               
      ivcorr = 317                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9191 continue                                                          
      ivtnum = 919                                                      
!                                                                        
!       ****  TEST 919  ****                                             
!                                                                        
      if (iczero) 39190, 9190, 39190                                    
 9190 continue                                                          
      ivon01 = 2572                                                     
      ivon02 = 2570                                                     
      ivcomp = max0 (ivon01,ivon02)                                     
      goto 49190                                                       
39190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49190, 9201, 49190                                    
49190 if (ivcomp - 2572) 29190,19190,29190                              
19190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9201                                                        
29190 ivfail = ivfail + 1                                               
      ivcorr = 2572                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 920 AND 921 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    
!      LARGEST VALUE WHERE ARGUMENTS ARE REAL AND FUNCTION IS INTEGER    
!                                                                        
 9201 continue                                                          
      ivtnum = 920                                                      
!                                                                        
!       ****  TEST 920  ****                                             
!                                                                        
      if (iczero) 39200, 9200, 39200                                    
 9200 continue                                                          
      rvon01 = .326e+2                                                  
      rvon02 = 22.075                                                   
      rvon03 = 76e-1                                                    
      ivcomp = max1 (rvon01,rvon02,rvon03)                              
      goto 49200                                                       
39200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49200, 9211, 49200                                    
49200 if (ivcomp - 32) 29200,19200,29200                                
19200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9211                                                        
29200 ivfail = ivfail + 1                                               
      ivcorr = 32                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9211 continue                                                          
      ivtnum = 921                                                      
!                                                                        
!       ****  TEST 921  ****                                             
!                                                                        
      if (iczero) 39210, 9210, 39210                                    
 9210 continue                                                          
      rvon01 = -6.3e2                                                   
      rvon02 = -21.0                                                    
      ivcomp = max1 (-463.3,rvon01,rvon02)                              
      goto 49210                                                       
39210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49210, 9221, 49210                                    
49210 if (ivcomp + 21) 29210,19210,29210                                
19210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9221                                                        
29210 ivfail = ivfail + 1                                               
      ivcorr = -21                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 922 AND 923 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    
!      SMALLEST VALUE WHERE ARGUMENTS AND FUNCTION ARE INTEGER           
!                                                                        
 9221 continue                                                          
      ivtnum = 922                                                      
!                                                                        
!       ****  TEST 922  ****                                             
!                                                                        
      if (iczero) 39220, 9220, 39220                                    
 9220 continue                                                          
      ivon01 = -75                                                      
      ivon02 = -243                                                     
      ivcomp = min0 (ivon01,ivon02)                                     
      goto 49220                                                       
39220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49220, 9231, 49220                                    
49220 if (ivcomp + 243) 29220,19220,29220                               
19220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9231                                                        
29220 ivfail = ivfail + 1                                               
      ivcorr = -243                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9231 continue                                                          
      ivtnum = 923                                                      
!                                                                        
!       ****  TEST 923  ****                                             
!                                                                        
      if (iczero) 39230, 9230, 39230                                    
 9230 continue                                                          
      ivon01 = -11                                                      
      ivon02 = 11                                                       
      ivcomp = min0 (0,ivon01,ivon02)                                   
      goto 49230                                                       
39230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49230, 9241, 49230                                    
49230 if (ivcomp + 11) 29230,19230,29230                                
19230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9241                                                        
29230 ivfail = ivfail + 1                                               
      ivcorr = -11                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 924 AND 925 CONTAIN INTRINSIC FUNCTION TESTS FOR CHOOSING    
!      SMALLEST VALUE WHERE ARGUMENTS ARE REAL AND FUNCTION IS INTEGER   
!                                                                        
 9241 continue                                                          
      ivtnum = 924                                                      
!                                                                        
!       ****  TEST 924  ****                                             
!                                                                        
      if (iczero) 39240, 9240, 39240                                    
 9240 continue                                                          
      rvon01 = 1.1111                                                   
      rvon02 = 22.222                                                   
      rvon03 = 333.33                                                   
      ivcomp = min1 (rvon01,rvon02,rvon03)                              
      goto 49240                                                       
39240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49240, 9251, 49240                                    
49240 if (ivcomp - 1) 29240,19240,29240                                 
19240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9251                                                        
29240 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9251 continue                                                          
      ivtnum = 925                                                      
!                                                                        
!       ****  TEST 925  ****                                             
!                                                                        
      if (iczero) 39250, 9250, 39250                                    
 9250 continue                                                          
      rvon01 = 28.8                                                     
      rvon02 = 2.88e1                                                   
      rvon03 = 288e-1                                                   
      rvon04 = 35.0                                                     
      ivcomp = min1 (rvon01,rvon02,rvon03,rvon04)                       
      goto 49250                                                       
39250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49250, 9261, 49250                                    
49250 if (ivcomp - 28) 29250,19250,29250                                
19250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9261                                                        
29250 ivfail = ivfail + 1                                               
      ivcorr = 28                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 926 THROUGH TEST 929 CONTAIN THE INTRINSIC FUNCTION FIX      
!      WHICH CONVERTS REAL ARGUMENTS TO INTEGER FUNCTION RESULTS         
!                                                                        
 9261 continue                                                          
      ivtnum = 926                                                      
!                                                                        
!       ****  TEST 926  ****                                             
!                                                                        
      if (iczero) 39260, 9260, 39260                                    
 9260 continue                                                          
      ivcomp = ifix (-6.06)                                             
      goto 49260                                                       
39260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49260, 9271, 49260                                    
49260 if (ivcomp + 6) 29260,19260,29260                                 
19260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9271                                                        
29260 ivfail = ivfail + 1                                               
      ivcorr = -6                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9271 continue                                                          
      ivtnum = 927                                                      
!                                                                        
!       ****  TEST 927  ****                                             
!                                                                        
      if (iczero) 39270, 9270, 39270                                    
 9270 continue                                                          
      rvon01 = 71.01                                                    
      ivcomp = ifix (rvon01)                                            
      goto 49270                                                       
39270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49270, 9281, 49270                                    
49270 if (ivcomp - 71) 29270,19270,29270                                
19270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9281                                                        
29270 ivfail = ivfail + 1                                               
      ivcorr = 71                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9281 continue                                                          
      ivtnum = 928                                                      
!                                                                        
!       ****  TEST 928  ****                                             
!                                                                        
      if (iczero) 39280, 9280, 39280                                    
 9280 continue                                                          
      rvon01 = 3.211e2                                                  
      ivcomp = ifix (rvon01)                                            
      goto 49280                                                       
39280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49280, 9291, 49280                                    
49280 if (ivcomp - 321) 29280,19280,29280                               
19280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9291                                                        
29280 ivfail = ivfail + 1                                               
      ivcorr = 321                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9291 continue                                                          
      ivtnum = 929                                                      
!                                                                        
!       ****  TEST 929  ****                                             
!                                                                        
      if (iczero) 39290, 9290, 39290                                    
 9290 continue                                                          
      rvon01 = 777e-1                                                   
      ivcomp = ifix (rvon01)                                            
      goto 49290                                                       
39290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49290, 9301, 49290                                    
49290 if (ivcomp - 77) 29290,19290,29290                                
19290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9301                                                        
29290 ivfail = ivfail + 1                                               
      ivcorr = 77                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 930 THROUGH TEST 932 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      TRANSFER OF SIGN WHERE ARGUMENTS AND FUNCTION ARE INTEGER         
!                                                                        
 9301 continue                                                          
      ivtnum = 930                                                      
!                                                                        
!       ****  TEST 930  ****                                             
!                                                                        
      if (iczero) 39300, 9300, 39300                                    
 9300 continue                                                          
      ivon01 = 643                                                      
      ivcomp = isign (ivon01,-1)                                        
      goto 49300                                                       
39300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49300, 9311, 49300                                    
49300 if (ivcomp + 643) 29300,19300,29300                               
19300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9311                                                        
29300 ivfail = ivfail + 1                                               
      ivcorr = -643                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9311 continue                                                          
      ivtnum = 931                                                      
!                                                                        
!       ****  TEST 931  ****                                             
!                                                                        
      if (iczero) 39310, 9310, 39310                                    
 9310 continue                                                          
      ivon01 = -22                                                      
      ivon02 = 723                                                      
      ivcomp = isign (ivon01,ivon02)                                    
      goto 49310                                                       
39310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49310, 9321, 49310                                    
49310 if (ivcomp - 22) 29310,19310,29310                                
19310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9321                                                        
29310 ivfail = ivfail + 1                                               
      ivcorr = 22                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9321 continue                                                          
      ivtnum = 932                                                      
!                                                                        
!       ****  TEST 932  ****                                             
!                                                                        
      if (iczero) 39320, 9320, 39320                                    
 9320 continue                                                          
      ivon01 = 3532                                                     
      ivon02 = 1                                                        
      ivcomp = isign (ivon01,ivon02)                                    
      goto 49320                                                       
39320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49320, 9331, 49320                                    
49320 if (ivcomp - 3532) 29320,19320,29320                              
19320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9331                                                        
29320 ivfail = ivfail + 1                                               
      ivcorr = 3532                                                     
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TEST 933 THROUGH TEST 936 CONTAIN INTRINSIC FUNCTION TESTS FOR    
!      POSITIVE DIFFERENCE WHERE ARGUMENTS AND FUNCTION ARE INTEGERS     
!                                                                        
 9331 continue                                                          
      ivtnum = 933                                                      
!                                                                        
!       ****  TEST 933  ****                                             
!                                                                        
      if (iczero) 39330, 9330, 39330                                    
 9330 continue                                                          
      ivon01 = 222                                                      
      ivcomp = idim (ivon01,1)                                          
      goto 49330                                                       
39330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49330, 9341, 49330                                    
49330 if (ivcomp - 221) 29330,19330,29330                               
19330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9341                                                        
29330 ivfail = ivfail + 1                                               
      ivcorr = 221                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9341 continue                                                          
      ivtnum = 934                                                      
!                                                                        
!       ****  TEST 934  ****                                             
!                                                                        
      if (iczero) 39340, 9340, 39340                                    
 9340 continue                                                          
      ivon01 = 45                                                       
      ivon02 = 41                                                       
      ivcomp = idim (ivon01,ivon02)                                     
      goto 49340                                                       
39340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49340, 9351, 49340                                    
49340 if (ivcomp - 4) 29340,19340,29340                                 
19340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9351                                                        
29340 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9351 continue                                                          
      ivtnum = 935                                                      
!                                                                        
!       ****  TEST 935  ****                                             
!                                                                        
      if (iczero) 39350, 9350, 39350                                    
 9350 continue                                                          
      ivon01 = 2                                                        
      ivon02 = 10                                                       
      ivcomp = idim (ivon01,ivon02)                                     
      goto 49350                                                       
39350 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49350, 9361, 49350                                    
49350 if (ivcomp) 29350,19350,29350                                     
19350 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9361                                                        
29350 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9361 continue                                                          
      ivtnum = 936                                                      
!                                                                        
!       ****  TEST 936  ****                                             
!                                                                        
      if (iczero) 39360, 9360, 39360                                    
 9360 continue                                                          
      ivon01 = 165                                                      
      ivon02 = -2                                                       
      ivcomp = idim (ivon01,ivon02)                                     
      goto 49360                                                       
39360 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49360, 9371, 49360                                    
49360 if (ivcomp - 167) 29360,19360,29360                               
19360 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9371                                                        
29360 ivfail = ivfail + 1                                               
      ivcorr = 167                                                      
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
!                                                                        
!      TESTS 937 AND 938 CONTAIN EXPRESSIONS CONTAINING MORE THAN ONE    
!      INTRINSIC FUNCTION - THE FUNCTIONS ARE INTEGER AND THE ARGUMENTS  
!      ARE REAL AND INTEGER                                              
!                                                                        
 9371 continue                                                          
      ivtnum = 937                                                      
!                                                                        
!       ****  TEST 937  ****                                             
!                                                                        
      if (iczero) 39370, 9370, 39370                                    
 9370 continue                                                          
      rvon01 = 33.3                                                     
      ivon01 = -12                                                      
      ivcomp = int (rvon01) + iabs (ivon01)                             
      goto 49370                                                       
39370 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49370, 9381, 49370                                    
49370 if (ivcomp -  45) 29370,19370,29370                               
19370 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9381                                                        
29370 ivfail = ivfail + 1                                               
      ivcorr = 45                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9381 continue                                                          
      ivtnum = 938                                                      
!                                                                        
!       ****  TEST 938  ****                                             
!                                                                        
      if (iczero) 39380, 9380, 39380                                    
 9380 continue                                                          
      ivon01 = 76                                                       
      ivon02 = 21                                                       
      ivon03 = 30                                                       
      ivcomp = max0 (ivon01,ivon02,ivon03) - min0 (ivon01,ivon02,ivon03)
      goto 49380                                                       
39380 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 49380, 9391, 49380                                    
49380 if (ivcomp - 55) 29380,19380,29380                                
19380 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 9391                                                        
29380 ivfail = ivfail + 1                                               
      ivcorr = 55                                                       
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 9391 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM098" )                          
      end program fm098
