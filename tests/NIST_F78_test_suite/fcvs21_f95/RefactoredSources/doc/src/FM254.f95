      program fm254
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE IS A TEST OF THE ELSE IF-BLOCK.  TESTS WITHIN THIS
!      ROUTINE ARE FOR THE SYNTAX OF THE BASIC ELSE IF STATEMENT AND     
!      ELSE IF-BLOCK STRUCTURE.                                          
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!             X3.9-1977                                                  
!         SECTION 11.7,       ELSE IF STATEMENT                          
!         SECTION 11.7.1,     ELSE IF-BLOCK                              
!         SECTION 11.7.2,     EXECUTION OF THE ELSE IF STATEMENT         
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
      logical  :: lfis01
      logical  :: l
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      real :: false
      real :: true
      integer :: ivcorr
      logical  :: lvon01
      logical  :: lvon02
      logical  :: lvon03
      logical  :: lvon04
      logical  :: lvon05
      logical  :: lvon06
      logical, dimension(1:2) :: ladn11
      logical :: lvtn01
      logical :: lvtn02
      logical, dimension(1:2) :: latn11
      data ladn11 / .true.,.false. / 
!                                                                        
!                                                                        
!      **** LOGICAL STATEMENT FUNCTION REFERENCED IN TEST 4   ****       
!                                                                        
      lfis01 ( l ) = l .and. l                                          
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
!         THE SYNTAX OF THE ELSE IF STATEMENTS IN THE TESTS TO FOLLOW IS 
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK                                                  
!         ELSE IF ( E2 )  THEN                                           
!              ELSE IF-BLOCK                                             
!         END IF                                                         
!                                                                        
!      THE NEXT FOUR TESTS WILL USE THE FOLLOWING COMBINATIONS OF TRUE   
!      AND FALSE FOR E1 AND E2 AS SHOWN BELOW -                          
!         TEST NUMBER    1    2    3    4                                
!              E1        F    F    T    T                                
!              E2        T    F    T    F                                
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 254  -  TEST 001  ****                         
!                                                                        
!         TEST 001 USES A VERY SIMPLE ELSE IF STATEMENT.  THE EXPRESSION 
!      WITHIN THE PARENTHESES IS THE LOGICAL CONSTANT  .TRUE. AND THE    
!      EXECUTABLE STATEMENT WITHIN THE ELSE IF-BLOCK OF LEVEL ONE IS AN  
!      INTEGER ARITHMETIC ASSIGNMENT STATEMENT.  IN THIS TEST THE LOGICAL
!      EXPRESSION E1 IS .FALSE. SO THE IF-BLOCK SHOULD NOT BE EXECUTED.  
!      THE LOGICAL EXPRESSION E2 IS .TRUE. SO THE ELSE IF-BLOCK SHOULD   
!      BE EXECUTED.                                                      
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 1                                                        
      if ( .false. )  then                                              
           ivcomp = ivcomp * 2                                          
      else if ( .true. )  then                                          
           ivcomp = ivcomp * 3                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3                
!                                                                        
      ivcorr = 3                                                        
40010 if ( ivcomp - 3 )  20010, 10010, 20010                            
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
!      ****  FCVS PROGRAM 254  -  TEST 002  ****                         
!                                                                        
!         TEST 002 HAS E1 .FALSE. AND E2 .FALSE..  NEITHER THE IF-BLOCK  
!      NOR THE ELSE IF-BLOCK SHOULD BE EXECUTED.                         
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lvon02 )  then                                          
           ivcomp = ivcomp * 3                                          
      end if                                                            
      ivcorr = 1                                                        
40020 if ( ivcomp - 1 )  20020, 10020, 20020                            
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
!      ****  FCVS PROGRAM 254  -  TEST 003  ****                         
!                                                                        
!         TEST 003 HAS E1 AS .TRUE. AND E2 AS .TRUE..  ONLY THE IF-BLOCK 
!      SHOULD BE EXECUTED. THE ELSE IF-BLOCK SHOULD NOT BE EXECUTED.     
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .true.                                                   
      lvtn01 = lvon01                                                   
      lvtn02 = lvon02                                                   
      if ( lvtn01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lvtn02 )  then                                          
           ivcomp = ivcomp * 3                                          
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
!      ****  FCVS PROGRAM 254  -  TEST 004  ****                         
!                                                                        
!         TEST 004 HAS E1 AS .TRUE. AND E2 AS .FALSE..  ONLY THE IF-BLOCK
!      SHOULD BE EXECUTED.  THE ELSE IF-BLOCK SHOULD NOT BE EXECUTED.    
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvtn01 = lfis01 ( lvon01 )                                        
      lvon02 = .false.                                                  
      if ( lvtn01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lfis01 ( lvon02 ) )  then                               
           ivcomp = ivcomp * 3                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****
!                                                                        
      ivcorr = 2                                                        
40040 if ( ivcomp - 2 )  20040, 10040, 20040                            
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
!                                                                        
!         THE SYNTAX OF THE ELSE IF STATEMENTS IN THE TESTS TO FOLLOW IS 
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK 1                                                
!         ELSE IF ( E2 )  THEN                                           
!              ELSE IF-BLOCK 1                                           
!         ELSE IF ( E3 )  THEN                                           
!              ELSE IF-BLOCK 2                                           
!         END IF                                                         
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 254  -  TEST 005  ****                         
!                                                                        
!         TEST 005 HAS E1 AS TRUE.  E2 AND E3 ARE FALSE.  ONLY IF-BLOCK 1
!      SHOULD BE EXECUTED.  ELSE IF-BLOCKS 1 AND 2 SHOULD NOT EXECUTE.   
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 1                                                        
!      LADN11(1) IS SET TO .TRUE. IN A DATA STATEMENT.                   
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
      if ( ladn11(1) )  then                                            
           ivcomp = ivcomp * 2                                          
      else if ( lvon02 )  then                                          
           ivcomp = ivcomp * 3                                          
      else if ( lvon03 )  then                                          
           ivcomp = ivcomp * 5                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2            ****
!                                                                        
      ivcorr = 2                                                        
40050 if ( ivcomp - 2 )  20050, 10050, 20050                            
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
!      ****  FCVS PROGRAM 254  -  TEST 006  ****                         
!                                                                        
!         TEST 006 HAS E1 AS FALSE, E2 AS TRUE, AND E3 AS FALSE.  ONLY   
!      ELSE IF-BLOCK 1 SHOULD EXECUTE.  IF-BLOCK 1 AND ELSE IF-BLOCK 2   
!      SHOULD NOT EXECUTE.                                               
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      latn11(2) = .true.                                                
      lvon03 = .false.                                                  
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( latn11(2) )  then                                       
           ivcomp = ivcomp * 3                                          
      else if ( lvon03 )  then                                          
           ivcomp = ivcomp * 5                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****
!                                                                        
      ivcorr = 3                                                        
40060 if ( ivcomp - 3 )  20060, 10060, 20060                            
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
!      ****  FCVS PROGRAM 254  -  TEST 007  ****                         
!                                                                        
!         TEST 007 HAS E1 AS FALSE, E2 AS FALSE, AND E3 AS TRUE.  ONLY   
!      ELSE IF-BLOCK 2 SHOULD BE EXECUTED.  IF-BLOCK 1 AND ELSE IF-BLOCK 
!      1 SHOULD NOT EXECUTE.                                             
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .false.                                                  
      lvon03 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lvon02 )  then                                          
           ivcomp = ivcomp * 3                                          
      else if ( lvon03 )  then                                          
           ivcomp = ivcomp * 5                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 5 = 1 * 5            ****
!                                                                        
      ivcorr = 5                                                        
40070 if ( ivcomp - 5 )  20070, 10070, 20070                            
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
!      ****  FCVS PROGRAM 254  -  TEST 008  ****                         
!                                                                        
!         TEST 008 HAS E1 AS FALSE.  BOTH E2 AND E3 ARE TRUE.  ONLY ELSE 
!      IF-BLOCK 1 SHOULD EXECUTE.  IF-BLOCK 1 AND ELSE IF-BLOCK 2 SHOULD 
!      NOT EXECUTE.  THIS IS A TEST OF THE LOGIC FLOW WHEN ONE OF THE    
!      EXPRESSIONS IN A STRING OF ELSE IF BLOCK STRUCTURES IS TRUE.  ONLY
!      THAT PARTICULAR ELSE IF-BLOCK SHOULD BE EXECUTED.  THE REST OF THE
!      STRING SHOULD BE SKIPPED.                                         
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      lvon03 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lvon02 )  then                                          
           ivcomp = ivcomp * 3                                          
      else if ( lvon03 )  then                                          
           ivcomp = ivcomp * 5                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****
!                                                                        
      ivcorr = 3                                                        
40080 if ( ivcomp - 3 )  20080, 10080, 20080                            
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
!                                                                        
!         THE FOLLOWING TWO TESTS ARE TO CHECK THE EXECUTION OF AN ELSE  
!      IF STATEMENT WITH AN EMPTY ELSE IF-BLOCK.  THE SYNTAX FOR THE TWO 
!      TESTS IS AS FOLLOWS -                                             
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK 1                                                
!         ELSE IF ( E2 )  THEN                                           
!         ELSE IF ( E3 )  THEN                                           
!              ELSE IF-BLOCK 1                                           
!         END IF                                                         
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 254  -  TEST 009  ****                         
!                                                                        
!         TEST 009 HAS E1 FALSE, E2 TRUE, AND E3 AS TRUE.  THE STRUCTURE 
!         ELSE IF ( E2 )  THEN                                           
!      IS FOLLOWED BY AN EMPTY ELSE IF-BLOCK ALLOWED IN SECTION 11.7.1.  
!      IN SECTION 11.7.2,  IF THE VALUE OF THE EXPRESSION IS TRUE AND THE
!      ELSE IF-BLOCK IS EMPTY, CONTROL IS TRANSFERRED TO THE NEXT END IF 
!      STATEMENT THAT HAS THE SAME IF-LEVEL AS THE ELSE IF STATEMENT.    
!      NEITHER IF-BLOCK 1 NOR ELSE IF-BLOCK 1 SHOULD BE EXECUTED.        
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      lvon03 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lvon02 )  then                                          
      else if ( lvon03 )  then                                          
           ivcomp = ivcomp * 3                                          
      end if                                                            
      ivcorr = 1                                                        
40090 if ( ivcomp - 1 )  20090, 10090, 20090                            
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
!      ****  FCVS PROGRAM 254  -  TEST 010  ****                         
!                                                                        
!         TEST 010 ALSO HAS AN EMPTY ELSE IF-BLOCK.  E1 AND E2 ARE FALSE.
!      E3 IS TRUE.  ONLY ELSE IF-BLOCK 1 SHOULD BE EXECUTED.  IF-BLOCK 1 
!      SHOULD NOT BE EXECUTED.  IN SECTION 11.7.2,  IF THE VALUE OF THE  
!      EXPRESSION IS FALSE, CONTROL IS TRANSFERRED TO THE NEXT ELSE IF,  
!      ELSE, OR END IF STATEMENT THAT HAS THE SAME IF-LEVEL AS THE ELSE  
!      IF STATEMENT.                                                     
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .false.                                                  
      lvon03 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
      else if ( lvon02 )  then                                          
      else if ( lvon03 )  then                                          
           ivcomp = ivcomp * 3                                          
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 3 = 1 * 3            ****
!                                                                        
      ivcorr = 3                                                        
40100 if ( ivcomp - 3 )  20100, 10100, 20100                            
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
!                                                                        
!         THE NEXT TWO TESTS USE THE ELSE IF STRUCTURE INSIDE A BLOCKED  
!      IF STRUCTURE OF LEVEL 2 AS FOLLOWS -                              
!                                                                        
!         IF ( E1 )  THEN                                                
!              IF-BLOCK 1                                                
!              IF ( E2 )  THEN                                           
!                   IF-BLOCK 2                                           
!              ELSE IF ( E3 )  THEN                                      
!                   ELSE IF-BLOCK 1                                      
!              ELSE IF ( E4 )  THEN                                      
!                   ELSE IF-BLOCK 2                                      
!              END IF                                                    
!         ELSE IF ( E5 )  THEN                                           
!              ELSE IF-BLOCK 3                                           
!         ELSE IF ( E6 )  THEN                                           
!              ELSE IF-BLOCK 4                                           
!         END IF                                                         
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 254  -  TEST 011  ****                         
!                                                                        
!         TEST 011 HAS E1 TRUE, E2 AND E3 AS FALSE, E4, E5, AND ALSO     
!      E6 AS TRUE.  IF-BLOCK 1, AND ELSE IF-BLOCK 2 SHOULD BE EXECUTED.  
!      IF-BLOCK 2, ELSE IF-BLOCK 1, 3, AND 4 SHOULD NOT BE EXECUTED.     
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
      lvon04 = .true.                                                   
      lvon05 = .true.                                                   
      lvon06 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
           else if ( lvon03 )  then                                     
                ivcomp = ivcomp * 5                                     
           else if ( lvon04 )  then                                     
                ivcomp = ivcomp * 7                                     
           end if                                                       
      else if ( lvon05 )  then                                          
           ivcomp = ivcomp * 11                                         
      else if ( lvon06 )  then                                          
           ivcomp = ivcomp * 13                                         
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 14 = 1 * 2 * 7       ****
!                                                                        
      ivcorr = 14                                                       
40110 if ( ivcomp - 14 )  20110, 10110, 20110                           
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
!      ****  FCVS PROGRAM 254  -  TEST 012  ****                         
!                                                                        
!         TEST 012 HAS E1 AS FALSE, E2, E3, AND E4 ARE TRUE, E5 AS FALSE,
!      AND E6 IS TRUE.  ONLY ELSE IF-BLOCK 4 SHOULD BE EXECUTED.  NO     
!      OTHER IF-BLOCK OR ELSE IF-BLOCK SHOULD BE EXECUTED.               
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      lvon03 = .true.                                                   
      lvon04 = .true.                                                   
      lvon05 = .false.                                                  
      lvon06 = .true.                                                   
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
           else if ( lvon03 )  then                                     
                ivcomp = ivcomp * 5                                     
           else if ( lvon04 )  then                                     
                ivcomp = ivcomp * 7                                     
           end if                                                       
      else if ( lvon05 )  then                                          
           ivcomp = ivcomp * 11                                         
      else if ( lvon06 )  then                                          
           ivcomp = ivcomp * 13                                         
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 13 = 1 * 13          ****
!                                                                        
      ivcorr = 13                                                       
40120 if ( ivcomp - 13 )  20120, 10120, 20120                           
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
90001 format (" ",24x,"FM254")                                          
90000 format (" ",20x,"END OF PROGRAM FM254" )                          
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
      end program fm254
