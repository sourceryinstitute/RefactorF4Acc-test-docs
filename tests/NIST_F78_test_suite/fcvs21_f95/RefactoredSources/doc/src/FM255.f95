      program fm255
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      real :: true
      integer :: ivcorr
      logical  :: lvon01
      logical  :: lvon02
      logical  :: lvon03
      logical  :: lvon04
      logical  :: lvon05
      logical  :: lvon06
      logical  :: lvon07
      real :: go
      real :: to
      integer :: icon01
      integer :: i
      real :: false
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE IS A TEST OF THE ELSE STATEMENT.  TESTS WITHIN    
!      THIS ROUTINE ARE FOR THE SYNTAX OF THE BASIC ELSE STATEMENT AND   
!      ELSE BLOCK STRUCTURES.  THE END IF STATEMENT IS USED IN ALL BLOCK 
!      IF STRUCTURES FOR THE ROUTINES FM253, FM254, AND FM255.  FOR EACH 
!      BLOCK IF STATEMENT, THERE MUST BE A CORRESPONDING END IF STATEMENT
!      IN THE SAME PROGRAM UNIT.                                         
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1977                                                 
!         SECTION 11.8,       ELSE STATEMENT                             
!         SECTION 11.8.1,     ELSE BLOCK                                 
!         SECTION 11.8.2,     EXECUTION OF AN ELSE STATEMENT             
!         SECTION 11.9,       END IF STATEMENT                           
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!      ******************************************************************
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   
!      X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 
!      FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       
!      ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT
!      ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   
!      OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING
!      THE RESULT OF EXECUTING THESE TESTS.                              
!                                                                        
!      THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      
!      FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        
!                                                                        
!            SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!      ******************************************************************
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION.                                           
!                                                                        
!      INITIALIZE CONSTANTS                                              
!      ********************                                              
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010     THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD.
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD.
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass = 0                                                        
      ivfail = 0                                                        
      ivdele = 0                                                        
      iczero = 0                                                        
!                                                                        
!      WRITE OUT PAGE HEADERS                                            
!                                                                        
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90008)                                                 
      write (i02,90004)                                                 
      write (i02,90010)                                                 
      write (i02,90004)                                                 
      write (i02,90016)                                                 
      write (i02,90001)                                                 
      write (i02,90004)                                                 
      write (i02,90012)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
!                                                                        
!                                                                        
!                                                                        
!         THE SYNTAX OF THE ELSE STATEMENTS IN THE TESTS TO FOLLOW IS    
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK                                                  
!         ELSE                                                           
!              ELSE-BLOCK                                                
!         END IF                                                         
!                                                                        
!         THE NEXT TWO TESTS WILL USE THE FOLLOWING COMBINATIONS OF TRUE 
!      AND FALSE FOR E1 AS SHOWN BELOW -                                 
!         TEST NUMBER    1    2                                          
!              E1        T    F                                          
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 001  ****                         
!                                                                        
!         TEST 001 USES A VALUE OF .TRUE. FOR E1.  THE IF-BLOCK SHOULD BE
!      EXECUTED.  THE ELSE-BLOCK SHOULD NOT BE EXECUTED.                 
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 1                                                        
      if ( .true. )  then                                               
           ivcomp = ivcomp * 2                                          
      else                                                              
           ivcomp = ivcomp * 3                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****
!                                                                        
      ivcorr = 2                                                        
40010 if ( ivcomp - 2 )  20010, 10010, 20010                            
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10010, 0021, 20010                                    
10010 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0021                                                        
20010 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 002  ****                         
!                                                                        
!         TEST 002 HAS E1 AS FALSE.  THE IF-BLOCK SHOULD NOT BE EXECUTED.
!      THE ELSE-BLOCK SHOULD EXECUTE.                                    
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else                                                              
           ivcomp = ivcomp * 3                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****
!                                                                        
      ivcorr = 3                                                        
40020 if ( ivcomp - 3 )  20020, 10020, 20020                            
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10020, 0031, 20020                                    
10020 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0031                                                        
20020 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0031 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 003  ****                         
!                                                                        
!         TEST 003 HAS AN EMPTY ELSE-BLOCK.  SECTION 11.8.1 STATES THAT  
!      AN ELSE-BLOCK MAY BE EMPTY.  IN THIS TEST THE VALUE OF E1 IS TRUE.
!      THE IF-BLOCK SHOULD BE EXECUTED.                                  
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else                                                              
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****
!                                                                        
      ivcorr = 2                                                        
40030 if ( ivcomp - 2 )  20030, 10030, 20030                            
30030 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10030, 0041, 20030                                    
10030 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0041                                                        
20030 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0041 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 004  ****                         
!                                                                        
!         TEST 004 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT THE VALUE 
!      OF E1 IS FALSE.  THE IF-BLOCK SHOULD NOT BE EXECUTED.             
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else                                                              
      end if                                                            
      ivcorr = 1                                                        
40040 if ( ivcomp - 1 )  20040, 10040, 20040                            
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10040, 0051, 20040                                    
10040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0051                                                        
20040 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 005  ****                         
!                                                                        
!         TEST 005 USES AN ELSE STATEMENT IN AN IF-LEVEL OF 2.  THE      
!      SYNTAX FOR THIS STRUCTURE IS SHOWN BELOW -                        
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK 1                                                
!              IF ( E2 )  THEN                                           
!                   IF-BLOCK 2                                           
!              ELSE                                                      
!                   ELSE-BLOCK 1                                         
!              END IF                                                    
!         ELSE                                                           
!              ELSE-BLOCK 2                                              
!         END IF                                                         
!                                                                        
!         IN THIS TEST THE VALUES FOR E1 AND E2 ARE BOTH TRUE.  IF-BLOCK 
!      1 AND IF-BLOCK 2 SHOULD BE EXECUTED.  ELSE-BLOCKS 1 AND 2 SHOULD  
!      NOT BE EXECUTED.                                                  
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
           else                                                         
                ivcomp = ivcomp * 5                                     
           end if                                                       
      else                                                              
      ivcomp = ivcomp * 7                                               
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 6 = 1 * 2 * 3            
!                                                                        
      ivcorr = 6                                                        
40050 if ( ivcomp - 6 )  20050, 10050, 20050                            
30050 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10050, 0061, 20050                                    
10050 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0061                                                        
20050 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 006  ****                         
!                                                                        
!         TEST 006 IS SIMILAR TO THE PREVIOUS TEST EXCEPT THAT E1 IS TRUE
!      AND E2 IS FALSE.  IF-BLOCK 1 AND ELSE-BLOCK 1 SHOULD BE EXECUTED. 
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
           else                                                         
                ivcomp = ivcomp * 5                                     
           end if                                                       
      else                                                              
           ivcomp = ivcomp * 7                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 10 = 1 * 2 * 5       ****
!                                                                        
      ivcorr = 10                                                       
40060 if ( ivcomp - 10 )  20060, 10060, 20060                           
30060 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10060, 0071, 20060                                    
10060 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0071                                                        
20060 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0071 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 007  ****                         
!                                                                        
!         TEST 007 AGAIN USES THE SAME STRUCTURE AS THE PREVIOUS TWO     
!      TESTS.  IN THIS TEST E1 IS FALSE AND E2 IS TRUE.  ONLY ELSE-BLOCK 
!      2 SHOULD BE EXECUTED.                                             
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
           else                                                         
                ivcomp = ivcomp * 5                                     
           end if                                                       
      else                                                              
           ivcomp = ivcomp * 7                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 7 = 1 * 7            ****
!                                                                        
      ivcorr = 7                                                        
40070 if ( ivcomp - 7 )  20070, 10070, 20070                            
30070 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10070, 0081, 20070                                    
10070 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0081                                                        
20070 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0081 continue                                                          
!                                                                        
!                                                                        
!         THE FOLLOWING TESTS USE A BLOCK IF STRUCTURE OF IF-LEVEL 3.    
!      THE STRUCTURE AS SHOWN BELOW CONTAINS THE IF-THEN, ELSE IF-THEN,  
!      AND ELSE STRUCTURES.                                              
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK 1                                                
!              IF ( E2 )  THEN                                           
!                   IF-BLOCK 2                                           
!                   IF ( E3 )  THEN                                      
!                        IF-BLOCK 3                                      
!                   ELSE IF ( E4 )  THEN                                 
!                        ELSE IF-BLOCK 1                                 
!                   ELSE IF ( E5 )  THEN                                 
!                        ELSE IF-BLOCK 2                                 
!                   ELSE                                                 
!                        ELSE-BLOCK 1                                    
!                   END IF                                               
!                   MORE IF-BLOCK 2                                      
!              ELSE IF ( E6 )  THEN                                      
!                   ELSE IF-BLOCK 3                                      
!              ELSE                                                      
!                   ELSE-BLOCK 2                                         
!              END IF                                                    
!              MORE IF-BLOCK 1                                           
!         ELSE IF ( E7 )  THEN                                           
!              ELSE IF-BLOCK 4                                           
!         ELSE                                                           
!              ELSE-BLOCK 3                                              
!         END IF                                                         
!                                                                        
!         THE TRUE AND FALSE VALUES FOR THE VARIOUS LOGICAL EXPRESSIONS  
!      USED IN THE TESTS TO FOLLOW ARE SHOWN BELOW -                     
!                                                                        
!         TEST NUMBER    8    9   10   11   12   13   14   15            
!              E1        T    T    T    T    T    T    F    F            
!              E2        T    T    T    T    F    F    F    F            
!              E3        T    F    F    F    F    F    F    F            
!              E4        T    F    F    T    F    F    F    F            
!              E5        T    F    T    F    F    F    F    F            
!              E6        T    F    F    F    T    F    F    F            
!              E7        T    F    F    F    F    F    T    F            
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 008  ****                         
!                                                                        
!         TEST 008 SHOULD EXECUTE IF-BLOCKS 1, 2, AND 3.  IT SHOULD ALSO 
!      EXECUTE MORE IF-BLOCKS 2 AND 1.                                   
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .true.                                                   
      lvon03 = .true.                                                   
      lvon04 = .true.                                                   
      lvon05 = .true.                                                   
      lvon06 = .true.                                                   
      lvon07 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           goto 0082                                                   
      end if                                                            
      goto 0083                                                        
 0082 ivcomp = ivcomp * 37                                              
 0083 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 14790 = 1 * 2 * 3 * 5 *  
!                                                       17 * 29      ****
!                                                                        
      ivcorr = 14790                                                    
40080 if ( ivcomp - 14790 )  20080, 10080, 20080                        
30080 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10080, 0091, 20080                                    
10080 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0091                                                        
20080 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0091 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 009  ****                         
!                                                                        
!         TEST 009 SHOULD EXECUTE IF-BLOCKS 1 AND 2.  IT SHOULD ALSO     
!      EXECUTE ELSE-BLOCK 1, PLUS MORE IF-BLOCKS 2 AND 1.                
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .true.                                                   
      lvon03 = .false.                                                  
      lvon04 = .false.                                                  
      lvon05 = .false.                                                  
      lvon06 = .false.                                                  
      lvon07 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 13                               
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 17                               
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 5                                
                end if                                                  
                ivcomp = ivcomp * 7                                     
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           if ( .true. )  goto 0092                                    
      end if                                                            
      goto 0093                                                        
 0092 ivcomp = ivcomp * 37                                              
 0093 continue                                                          
!                                                                        
!         **** THE ORDER OF THE PRIME INTEGER MULTIPLIERS HAS BEEN       
!      CHANGED TO KEEP THE IVCOMP RESULT SMALLER THAN 32767          ****
!                                                                        
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 6090 = 1 * 2 * 3 * 5 * 7 
!                                                      * 29          ****
!                                                                        
      ivcorr = 6090                                                     
40090 if ( ivcomp - 6090 )  20090, 10090, 20090                         
30090 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10090, 0101, 20090                                    
10090 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0101                                                        
20090 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0101 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 010  ****                         
!                                                                        
!         TEST 010 SHOULD EXECUTE IF-BLOCKS 1 AND 2.  IT SHOULD ALSO     
!      EXECUTE ELSE IF-BLOCK 2, PLUS MORE IF-BLOCKS 2 AND 1.             
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .true.                                                   
      lvon03 = .false.                                                  
      lvon04 = .false.                                                  
      lvon05 = .true.                                                   
      lvon06 = .false.                                                  
      lvon07 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           icon01 = 1                                                   
           if ( icon01 )  0103, 0102, 0103                              
      end if                                                            
      goto 0103                                                        
 0102 ivcomp = ivcomp * 37                                              
 0103 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 32538 = 1 * 2 * 3 * 11 * 
!                                                       17 * 29      ****
!                                                                        
      ivcorr = 32538                                                    
40100 if ( ivcomp - 32538 )  20100, 10100, 20100                        
30100 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10100, 0111, 20100                                    
10100 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0111                                                        
20100 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0111 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 011  ****                         
!                                                                        
!         TEST 011 SHOULD EXECUTE IF-BLOCKS 1 AND 2.  IT SHOULD ALSO     
!      EXECUTE ELSE IF-BLOCK 1, PLUS MORE IF-BLOCKS 2 AND 1.             
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .true.                                                   
      lvon03 = .false.                                                  
      lvon04 = .true.                                                   
      lvon05 = .false.                                                  
      lvon06 = .false.                                                  
      lvon07 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           assign 0112 to i                                             
           go to i, ( 0113, 0112)                                       
      end if                                                            
      goto 0113                                                        
 0112 ivcomp = ivcomp * 37                                              
 0113 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 20706 = 1 * 2 * 3 * 7 *  
!                                                       17 * 29      ****
!                                                                        
      ivcorr = 20706                                                    
40110 if ( ivcomp - 20706 )  20110, 10110, 20110                        
30110 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10110, 0121, 20110                                    
10110 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0121                                                        
20110 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0121 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 012  ****                         
!                                                                        
!         TEST 012 SHOULD EXECUTE IF-BLOCK 1, ELSE IF-BLOCK 3, AND MORE  
!      IF-BLOCK 1.                                                       
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
      lvon04 = .false.                                                  
      lvon05 = .false.                                                  
      lvon06 = .true.                                                   
      lvon07 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           i = 2                                                        
           go to ( 0123, 0122 ), i                                      
      end if                                                            
      goto 0123                                                        
 0122 ivcomp = ivcomp * 37                                              
 0123 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 1102 = 1 * 2 * 19 * 29   
!                                                                        
      ivcorr = 1102                                                     
40120 if ( ivcomp - 1102 )  20120, 10120, 20120                         
30120 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10120, 0131, 20120                                    
10120 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0131                                                        
20120 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0131 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 013  ****                         
!                                                                        
!         TEST 013 SHOULD EXECUTE IF-BLOCK 1, ELSE-BLOCK 2, AND MORE     
!      IF-BLOCK 1.                                                       
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
      lvon04 = .false.                                                  
      lvon05 = .false.                                                  
      lvon06 = .false.                                                  
      lvon07 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           goto 0132                                                   
      end if                                                            
      goto 0133                                                        
 0132 ivcomp = ivcomp * 37                                              
 0133 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 1334 = 1 * 2 * 23 * 29   
!                                                                        
      ivcorr = 1334                                                     
40130 if ( ivcomp - 1334 )  20130, 10130, 20130                         
30130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10130, 0141, 20130                                    
10130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0141                                                        
20130 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0141 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 014  ****                         
!                                                                        
!         TEST 014 SHOULD ONLY EXECUTE ELSE IF-BLOCK 4.                  
!                                                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
      lvon04 = .false.                                                  
      lvon05 = .false.                                                  
      lvon06 = .false.                                                  
      lvon07 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           if ( .not. .false. )  goto 0142                             
      end if                                                            
      goto 0143                                                        
 0142 ivcomp = ivcomp * 37                                              
 0143 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 31 = 1 * 31          ****
!                                                                        
      ivcorr = 31                                                       
40140 if ( ivcomp - 31 )  20140, 10140, 20140                           
30140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10140, 0151, 20140                                    
10140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0151                                                        
20140 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0151 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 015  ****                         
!                                                                        
!         TEST 015 SHOULD ONLY EXECUTE THE LOGIC IN ELSE-BLOCK 3.  THIS  
!      LOGIC CONSISTS OF AN ARITHMETIC IF STATEMENT WHICH SHOULD TAKE    
!      THE EXPRESSION EQUAL TO ZERO BRANCH TO STATEMENT 0152.            
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
      lvon04 = .false.                                                  
      lvon05 = .false.                                                  
      lvon06 = .false.                                                  
      lvon07 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                else if ( lvon04 )  then                                
                     ivcomp = ivcomp * 7                                
                else if ( lvon05 )  then                                
                     ivcomp = ivcomp * 11                               
                else                                                    
                     ivcomp = ivcomp * 13                               
                end if                                                  
                ivcomp = ivcomp * 17                                    
           else if ( lvon06 )  then                                     
                ivcomp = ivcomp * 19                                    
           else                                                         
                ivcomp = ivcomp * 23                                    
           end if                                                       
           ivcomp = ivcomp * 29                                         
      else if ( lvon07 )  then                                          
           ivcomp = ivcomp * 31                                         
      else                                                              
           icon01 = 1                                                   
           if ( icon01 - 1 )  0153, 0152, 0153                          
      end if                                                            
      goto 0153                                                        
 0152 ivcomp = ivcomp * 37                                              
 0153 continue                                                          
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 37 = 1 * 37          ****
!                                                                        
      ivcorr = 37                                                       
40150 if ( ivcomp - 37 )  20150, 10150, 20150                           
30150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10150, 0161, 20150                                    
10150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0161                                                        
20150 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0161 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 255  -  TEST 016  ****                         
!                                                                        
!         TEST 016 IS A TEST OF THE END IF STATEMENT.   SECTION 11.9     
!      PERMITS TRANSFER OF CONTROL FROM ANYWHERE TO AN END IF STATEMENT. 
!      ALSO ACCORDING TO SECTION 11.9 - EXECUTION OF AN END IF STATEMENT 
!      HAS NO EFFECT.                                                    
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      if ( iczero )  0163, 0162, 0163                                   
 0162 goto 0164                                                        
 0163 if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else                                                              
           ivcomp = ivcomp * 3                                          
 0164 end if                                                            
!                                                                        
!         **** IVCOMP SHOULD REMAIN SET TO ONE (1).                  ****
!                                                                        
      ivcorr = 1                                                        
40160 if ( ivcomp - 1 )  20160, 10160, 20160                            
30160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10160, 0171, 20160                                    
10160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0171                                                        
20160 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0171 continue                                                          
!                                                                        
!                                                                        
!      WRITE OUT TEST SUMMARY                                            
!                                                                        
      write (i02,90004)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
      write (i02,90000)                                                 
      write (i02,90004)                                                 
      write (i02,90020) ivfail                                          
      write (i02,90022) ivpass                                          
      write (i02,90024) ivdele                                          
      stop                                                              
90001 format (" ",24x,"FM255")                                          
90000 format (" ",20x,"END OF PROGRAM FM255" )                          
!                                                                        
!      FORMATS FOR TEST DETAIL LINES                                     
!                                                                        
80000 format (" ",4x,i5,6x,"DELETED")                                   
80002 format (" ",4x,i5,7x,"PASS")                                      
80010 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80012 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
80018 format (" ",4x,i5,7x,"FAIL",2x,a14,1x,a14)                        
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
!                                                                        
90002 format ("1")                                                      
90004 format (" ")                                                      
90006 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90008 format (" ",21x,"VERSION 2.1" )                                   
90010 format (" ",8x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )         
90012 format (" ",5x,"TEST",5x,"PASS/FAIL",5x,"COMPUTED",8x,"CORRECT")  
90014 format (" ",5x,"----------------------------------------------" ) 
90016 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARY                                 
!                                                                        
90020 format (" ",19x,i5," TESTS FAILED" )                              
90022 format (" ",19x,i5," TESTS PASSED" )                              
90024 format (" ",19x,i5," TESTS DELETED" )                             
      end program fm255
