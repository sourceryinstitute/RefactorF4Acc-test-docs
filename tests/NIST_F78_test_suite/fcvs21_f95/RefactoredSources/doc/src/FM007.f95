      program fm007
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivon01
      integer :: ivcorr
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      integer :: ivon06
      integer :: ivon07
      integer :: ivon08
      integer :: ivon09
      integer :: ivon10
      integer :: ivon11
      integer :: ivon12
      integer :: ivon13
      integer :: ivon14
      integer :: ivon15
      integer :: ivon16
      integer :: ivon17
      integer :: ivon18
      integer :: ivon19
      integer :: ivon20
!         COMMENT SECTION                                                
!                                                                        
!      FM007                                                             
!                                                                        
!          THIS ROUTINE TESTS THE USE OF DATA INITIALIZATION STATEMENTS. 
!      DATA INITIALIZATION STATEMENTS ARE USED TO DEFINE INITIAL VALUES  
!      OF INTEGER VARIABLES.  THE DATA STATEMENTS CONTAIN UNSIGNED,      
!      POSITIVE SIGNED AND NEGATIVE SIGNED INTEGER CONSTANTS.  THE LAST  
!      DATA STATEMENT CONTAINS THE FORM                                  
!                    J*INTEGER CONSTANT                                  
!      WHICH INDICATES THE CONSTANT IS TO BE SPECIFIED J TIMES.          
!                                                                        
!       THE TESTS IN THIS ROUTINE CHECK THE INTEGER VARIABLES IN THE     
!      DATA STATEMENT FOR THE ASSIGNED INITIAL VALUES.                   
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 9, DATA STATEMENT                                      
!                                                                        
!                                                                        
!          DATA INITIALIZATION STATEMENTS                                
!                                                                        
      data ivon01,ivon02,ivon03,ivon04,ivon05 / 3,76,587,9999,21111 / 
      data ivon06,ivon07,ivon08,ivon09,ivon10 / +3,+76,+587,+9999,+21111 / 
      data ivon11,ivon12,ivon13,ivon14,ivon15 / -3,-76,-587,-9999,-21111 / 
      data ivon16,ivon17,ivon18,ivon19,ivon20 / 2*119,2*7,-427 / 
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
!      TEST SECTION                                                      
!                                                                        
!      TESTS 80 THROUGH 84 CHECK THE VALUES INITIALIZED BY THE DATA      
!      STATEMENT CONTAINING IVON01,..., IVON05.                          
!                                                                        
  801 continue                                                          
      ivtnum =  80                                                      
!                                                                        
!       ****  TEST 80  ****                                              
!                                                                        
      if (iczero) 30800,  800, 30800                                    
  800 continue                                                          
      ivcomp = ivon01                                                   
      goto 40800                                                       
30800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40800,  811, 40800                                    
40800 if (ivcomp - 3) 20800, 10800,20800                                
10800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  811                                                        
20800 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  811 continue                                                          
      ivtnum =  81                                                      
!                                                                        
!       ****  TEST 81  ****                                              
!                                                                        
      if (iczero) 30810,  810, 30810                                    
  810 continue                                                          
      ivcomp = ivon02                                                   
      goto 40810                                                       
30810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40810,  821, 40810                                    
40810 if (ivcomp - 76) 20810, 10810, 20810                              
10810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  821                                                        
20810 ivfail = ivfail + 1                                               
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  821 continue                                                          
      ivtnum =  82                                                      
!                                                                        
!       ****  TEST 82  ****                                              
!                                                                        
      if (iczero) 30820,  820, 30820                                    
  820 continue                                                          
      ivcomp = ivon03                                                   
      goto 40820                                                       
30820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40820,  831, 40820                                    
40820 if (ivcomp - 587) 20820, 10820, 20820                             
10820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  831                                                        
20820 ivfail = ivfail + 1                                               
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  831 continue                                                          
      ivtnum =  83                                                      
!                                                                        
!       ****  TEST 83  ****                                              
!                                                                        
      if (iczero) 30830,  830, 30830                                    
  830 continue                                                          
      ivcomp =ivon04                                                    
      goto 40830                                                       
30830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40830,  841, 40830                                    
40830 if (ivcomp - 9999)  20830, 10830, 20830                           
10830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  841                                                        
20830 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  841 continue                                                          
      ivtnum =  84                                                      
!                                                                        
!       ****  TEST 84  ****                                              
!                                                                        
      if (iczero) 30840,  840, 30840                                    
  840 continue                                                          
      ivcomp = ivon05                                                   
      goto 40840                                                       
30840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40840,  851, 40840                                    
40840 if (ivcomp - 21111) 20840, 10840, 20840                           
10840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  851                                                        
20840 ivfail = ivfail + 1                                               
      ivcorr = 21111                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!         TESTS 85 THROUGH 89 CHECK THE VALUES INITIALIZED BY THE DATA   
!      STATEMENT CONTAINING IVON06,...,IVON10.                           
!                                                                        
  851 continue                                                          
      ivtnum =  85                                                      
!                                                                        
!       ****  TEST 85  ****                                              
!                                                                        
      if (iczero) 30850,  850, 30850                                    
  850 continue                                                          
      ivcomp=ivon06                                                     
      goto 40850                                                       
30850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40850,  861, 40850                                    
40850 if (ivcomp - 3) 20850, 10850, 20850                               
10850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  861                                                        
20850 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  861 continue                                                          
      ivtnum =  86                                                      
!                                                                        
!       ****  TEST 86  ****                                              
!                                                                        
      if (iczero) 30860,  860, 30860                                    
  860 continue                                                          
      ivcomp = ivon07                                                   
      goto 40860                                                       
30860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40860,  871, 40860                                    
40860 if (ivcomp - 76) 20860, 10860, 20860                              
10860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  871                                                        
20860 ivfail = ivfail + 1                                               
      ivcorr = 76                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  871 continue                                                          
      ivtnum =  87                                                      
!                                                                        
!       ****  TEST 87  ****                                              
!                                                                        
      if (iczero) 30870,  870, 30870                                    
  870 continue                                                          
      ivcomp = ivon08                                                   
      goto 40870                                                       
30870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40870,  881, 40870                                    
40870 if (ivcomp - 587) 20870, 10870, 20870                             
10870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  881                                                        
20870 ivfail = ivfail + 1                                               
      ivcorr = 587                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  881 continue                                                          
      ivtnum =  88                                                      
!                                                                        
!       ****  TEST 88  ****                                              
!                                                                        
      if (iczero) 30880,  880, 30880                                    
  880 continue                                                          
      ivcomp = ivon09                                                   
      goto 40880                                                       
30880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40880,  891, 40880                                    
40880 if (ivcomp - 9999) 20880, 10880, 20880                            
10880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  891                                                        
20880 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  891 continue                                                          
      ivtnum =  89                                                      
!                                                                        
!       ****  TEST 89  ****                                              
!                                                                        
      if (iczero) 30890,  890, 30890                                    
  890 continue                                                          
      ivcomp = ivon10                                                   
      goto 40890                                                       
30890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40890,  901, 40890                                    
40890 if (ivcomp - 21111)  20890, 10890, 20890                          
10890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  901                                                        
20890 ivfail = ivfail + 1                                               
      ivcorr= 21111                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TESTS 90 THROUGH 94 CHECK THE VALUES INITIALIZED BY THE DATA  
!      STATEMENT CONTAINING IVON11,...,IVON15.                           
!                                                                        
  901 continue                                                          
      ivtnum =  90                                                      
!                                                                        
!       ****  TEST 90  ****                                              
!                                                                        
      if (iczero) 30900,  900, 30900                                    
  900 continue                                                          
      ivcomp = ivon11                                                   
      goto 40900                                                       
30900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40900,  911, 40900                                    
40900 if (ivcomp + 3) 20900, 10900, 20900                               
10900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  911                                                        
20900 ivfail = ivfail + 1                                               
      ivcorr = -3                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  911 continue                                                          
      ivtnum =  91                                                      
!                                                                        
!       ****  TEST 91  ****                                              
!                                                                        
      if (iczero) 30910,  910, 30910                                    
  910 continue                                                          
      ivcomp = ivon12                                                   
      goto 40910                                                       
30910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40910,  921, 40910                                    
40910 if (ivcomp + 76) 20910, 10910, 20910                              
10910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  921                                                        
20910 ivfail = ivfail + 1                                               
      ivcorr = -76                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  921 continue                                                          
      ivtnum =  92                                                      
!                                                                        
!       ****  TEST 92  ****                                              
!                                                                        
      if (iczero) 30920,  920, 30920                                    
  920 continue                                                          
      ivcomp= ivon13                                                    
      goto 40920                                                       
30920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40920,  931, 40920                                    
40920 if (ivcomp + 587) 20920, 10920, 20920                             
10920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  931                                                        
20920 ivfail = ivfail + 1                                               
      ivcorr = -587                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  931 continue                                                          
      ivtnum =  93                                                      
!                                                                        
!       ****  TEST 93  ****                                              
!                                                                        
      if (iczero) 30930,  930, 30930                                    
  930 continue                                                          
      ivcomp = ivon14                                                   
      goto 40930                                                       
30930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40930,  941, 40930                                    
40930 if (ivcomp + 9999) 20930, 10930, 20930                            
10930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  941                                                        
20930 ivfail = ivfail + 1                                               
      ivcorr = -9999                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  941 continue                                                          
      ivtnum =  94                                                      
!                                                                        
!       ****  TEST 94  ****                                              
!                                                                        
      if (iczero) 30940,  940, 30940                                    
  940 continue                                                          
      ivcomp = ivon15                                                   
      goto 40940                                                       
30940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40940,  951, 40940                                    
40940 if (ivcomp + 21111) 20940, 10940, 20940                           
10940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  951                                                        
20940 ivfail = ivfail + 1                                               
      ivcorr = -21111                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!          TESTS 95 THROUGH 99 CHECK THE VALUES INITIALIZED BY THE DATA  
!      STATEMENT CONTAINING IVON16,...,IVON20.                           
!                                                                        
  951 continue                                                          
      ivtnum =  95                                                      
!                                                                        
!       ****  TEST 95  ****                                              
!                                                                        
      if (iczero) 30950,  950, 30950                                    
  950 continue                                                          
      ivcomp =ivon16                                                    
      goto 40950                                                       
30950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40950,  961, 40950                                    
40950 if (ivcomp - 119) 20950, 10950, 20950                             
10950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  961                                                        
20950 ivfail = ivfail + 1                                               
      ivcorr = 119                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  961 continue                                                          
      ivtnum =  96                                                      
!                                                                        
!       ****  TEST 96  ****                                              
!                                                                        
      if (iczero) 30960,  960, 30960                                    
  960 continue                                                          
      ivcomp=ivon17                                                     
      goto 40960                                                       
30960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40960,  971, 40960                                    
40960 if (ivcomp - 119) 20960, 10960, 20960                             
10960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  971                                                        
20960 ivfail = ivfail + 1                                               
      ivcorr = 119                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  971 continue                                                          
      ivtnum =  97                                                      
!                                                                        
!       ****  TEST 97  ****                                              
!                                                                        
      if (iczero) 30970,  970, 30970                                    
  970 continue                                                          
      ivcomp = ivon18                                                   
      goto 40970                                                       
30970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40970,  981, 40970                                    
40970 if (ivcomp - 7) 20970, 10970, 20970                               
10970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  981                                                        
20970 ivfail = ivfail + 1                                               
      ivcorr = 7                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  981 continue                                                          
      ivtnum =  98                                                      
!                                                                        
!       ****  TEST 98  ****                                              
!                                                                        
      if (iczero) 30980,  980, 30980                                    
  980 continue                                                          
      ivcomp = ivon19                                                   
      goto 40980                                                       
30980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40980,  991, 40980                                    
40980 if (ivcomp - 7) 20980, 10980, 20980                               
10980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  991                                                        
20980 ivfail = ivfail + 1                                               
      ivcorr = 7                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  991 continue                                                          
      ivtnum =  99                                                      
!                                                                        
!       ****  TEST 99  ****                                              
!                                                                        
      if (iczero) 30990,  990, 30990                                    
  990 continue                                                          
      ivcomp = ivon20                                                   
      goto 40990                                                       
30990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40990, 1001, 40990                                    
40990 if (ivcomp + 427)  20990,10990,20990                              
10990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1001                                                        
20990 ivfail = ivfail + 1                                               
      ivcorr = -427                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1001 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM007" )                          
      end program fm007
