      program fm252
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
      integer :: k
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!         THIS PROGRAM TESTS REDEFINITION OF STATEMENT LABELS WITH THE   
!      ASSIGN STATEMENT IN CONJUNCTION WITH THE ASSIGNED GO TO STATEMENT.
!      THE OPTIONAL COMMA IN THE SYNTAX OF THE ASSIGNED GO TO IS TESTED. 
!      THE RANGE OF STATEMENT LABELS ( FROM 00001 TO 99999 ) IS TESTED   
!      USING THE ASSIGN STATEMENT AND THE ASSIGNED GO TO STATEMENT.      
!      IT ALSO TESTS THE OPTIONAL COMMA IN THE SYNTAX OF THE COMPUTED    
!      GO TO STATEMENT AND HAS TESTS ON THE RANGE OF THE INDEX IN THE    
!      COMPUTED GO TO.                                                   
!                                                                        
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1978                                                 
!         SECTION 10.3,       STATEMENT LABEL ASSIGNMENT (ASSIGN)        
!         SECTION 11.2,       COMPUTED GO TO STATEMENT                   
!         SECTION 11.3,       ASSIGNED GO TO STATEMENT                   
!                                                                        
!                                                                        
!         FM013 - SUBSET LEVEL TESTS OF THE ASSIGN STATEMENT AND THE     
!                 ASSIGNED GO TO STATEMENT.                              
!                                                                        
!         FM014, FM052, AND FM053 - SUBSET LEVEL TESTS OF THE COMPUTED   
!                 GO TO STATEMENT.                                       
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
!      ****  FCVS PROGRAM 252  -  TEST 001  ****                         
!                                                                        
!         TEST 001 IS AN ASSIGN STATEMENT IN WHICH THE STATEMENT         
!      LABEL IS ACTUALLY FOR A FORMAT STATEMENT.  IN 10.3 - THE STATEMENT
!      LABEL MUST BE THE LABEL OF AN EXECUTABLE STATEMENT OR A FORMAT    
!      STATEMENT.  THE ASSIGN STATEMENT IS FOLLOWED BY A SIMPLE WRITE    
!      TO THE PRINTER.                                                   
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      assign 0012 to i                                                  
 0012 format (" **** ASSIGN FORMAT NUMBER TO INTEGER VARIABLE ****" )   
      write (i02, i)                                                    
!         ***** VISUALLY CHECK THE OUTPUT PRINTER LISTING *****          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40010 if ( ivcomp )  20010, 10010, 20010                                
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
!      ****  FCVS PROGRAM 252  -  TEST 002  ****                         
!                                                                        
!         TEST 002 IS A TEST OF THE ASSIGNED GO TO STATEMENT WITH THE    
!      OPTIONAL COMMA INTENTIONALLY DELETED FROM THE SYNTAX.             
!                   GO TO I (S1, S2, S3)                                 
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      assign 0023 to j                                                  
      goto 0025                                                        
 0022 ivcomp = 0                                                        
      goto 40020                                                       
 0023 ivcomp = 1                                                        
      goto 40020                                                       
 0024 ivcomp = 0                                                        
      goto 40020                                                       
 0025 go to j (0022, 0023, 0024)                                        
!         NOTE THAT THE OPTIONAL COMMA IS NOT PRESENT AFTER THE J IN     
!      PREVIOUS ASSIGNED GO TO STATEMENT.                                
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
!      ****  FCVS PROGRAM 252  -  TEST 003  ****                         
!                                                                        
!         TEST 003 USES A SERIES OF ASSIGN STATEMENTS TO TEST THAT THE   
!      SAME STATEMENT LABEL AND INTEGER VARIABLE CAN BE USED IN A        
!      MULTIPLE REDEFINITION TO THE SAME VALUES.  A SIMPLE ASSIGNED      
!      GO TO IS USED TO TEST THE VALUE OF THE INTEGER VARIABLE M.        
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      assign 0033 to m                                                  
      assign 0033 to m                                                  
      assign 0033 to m                                                  
      goto 0035                                                        
 0032 ivcomp = 0                                                        
      goto 40030                                                       
 0033 ivcomp = 1                                                        
      goto 40030                                                       
 0034 ivcomp = 0                                                        
      goto 40030                                                       
 0035 go to m, (0032, 0033, 0034)                                       
40030 if ( ivcomp - 1 )  20030, 10030, 20030                            
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
!      ****  FCVS PROGRAM 252  -  TEST 004  ****                         
!                                                                        
!         TEST 004 USES A SERIES OF ASSIGN STATEMENTS TO SET THE INTEGER 
!      VARIABLE K TO A STATEMENT LABEL IN THE PARENTHESIZED LIST OF      
!      STATEMENT LABELS FOR THE ASSIGNED GO TO STATEMENT, THEN TO A      
!      STATEMENT LABEL NOT IN THE LIST AND FINALLY BACK TO A PROPER      
!      STATEMENT LABEL WITHIN THE PARENTHESIZED LIST.  SECTION 11.3      
!      REQUIRES - IF THE PARENTHESIZED LIST IS PRESENT, THE STATEMENT    
!      LABEL ASSIGNED TO  I  MUST BE ONE OF THE STATEMENT LABELS IN      
!      THE LIST.  AN ASSIGNED GO TO STATEMENT IS USED TO TEST THE FINAL  
!      ASSIGNMENT OF STATEMENT LABELS TO THE INTEGER VARIABLE K.         
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      assign 0043 to k                                                  
      assign 0042 to k                                                  
 0042 assign 0043 to k                                                  
      goto 0045                                                        
 0043 ivcomp = 1                                                        
      goto 40040                                                       
 0044 ivcomp = 0                                                        
      goto 40040                                                       
 0045 go to k, (0044, 0043)                                             
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
!                                                                        
!         THE FOLLOWING TWO TESTS CHECK THE POSSIBLE RANGE OF STATEMENT  
!      LABELS ( FROM 00001 TO 99999 ) BY USING THEM IN ASSIGN STATEMENTS 
!      AND ASSIGNED GO TO STATEMENTS.                                    
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 252  -  TEST 005  ****                         
!                                                                        
!         TEST 005 USES A STATEMENT LABEL OF 00001 WHICH IS THE SMALLEST 
!      ALLOWABLE STATEMENT LABEL.                                        
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      assign 00001 to i                                                 
      goto 0054                                                        
 0052 ivcomp = 0                                                        
      goto 40050                                                       
00001 ivcomp = 1                                                        
      goto 40050                                                       
 0053 ivcomp = 0                                                        
      goto 40050                                                       
 0054 go to i, ( 0052, 00001, 0053 )                                    
40050 if ( ivcomp - 1 )  20050, 10050, 20050                            
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
!      ****  FCVS PROGRAM 252  -  TEST 006  ****                         
!                                                                        
!         TEST 006 USES A STATEMENT LABEL OF 99999 WHICH IS THE LARGEST  
!      ALLOWABLE STATEMENT LABEL.                                        
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      assign 99999 to j                                                 
      goto 0064                                                        
 0062 ivcomp = 0                                                        
      goto 40060                                                       
99999 ivcomp = 1                                                        
      goto 40060                                                       
 0063 ivcomp = 0                                                        
      goto 40060                                                       
 0064 go to j, ( 0062, 99999, 0063 )                                    
40060 if ( ivcomp - 1 )  20060, 10060, 20060                            
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
!      ****  FCVS PROGRAM 252  -  TEST 007  ****                         
!                                                                        
!         TEST 007 IS A SYNTAX CHECK ON THE OPTIONAL COMMA IN THE        
!      COMPUTED GO TO STATEMENT.  THE COMMA FOLLOWING THE PARENTHESIZED  
!      LIST OF STATEMENT LABELS IS INTENTIONALLY OMITTED.                
!                   GO TO ( S1, S2, S3 )  I                              
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      i = 3                                                             
      goto 0075                                                        
 0072 ivcomp = 0                                                        
      i = 1                                                             
      goto 0075                                                        
 0073 ivcomp = 1                                                        
      goto 40070                                                       
 0074 ivcomp = 0                                                        
      i = 2                                                             
      goto 0075                                                        
 0075 go to ( 0074, 0073, 0072 )  i                                     
40070 if ( i - 2 )  20070, 40071, 20070                                 
40071 if ( ivcomp - 1 )  20070, 10070, 20070                            
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
!      ****  FCVS PROGRAM 252  -  TEST 008  ****                         
!                                                                        
!         TEST 008 USES THE COMPUTED GO TO WITHOUT THE OPTIONAL COMMA    
!      AND HAS A SINGLE STATEMENT LABEL IN THE PARENTHESIZED LIST OF     
!      STATEMENT LABELS.                                                 
!                   GO TO ( S1 ) I                                       
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      j = 1                                                             
      goto 0083                                                        
 0082 ivcomp = 1                                                        
      goto 40080                                                       
 0083 go to ( 0082 ) j                                                  
40080 if ( ivcomp - 1 )  20080, 10080, 20080                            
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
!         THE NEXT THREE TESTS OF THE COMPUTED GO TO TEST THE RANGE OF   
!      THE INDEX.                                                        
!                                                                        
!         FORTRAN 77 HAS THE REQUIREMENT IN SECTION 11.2 - IF THE INDEX  
!      IS LESS THAN ONE OR GREATER THAN THE NUMBER OF STATEMENT LABELS IN
!      THE PARENTHESIZED LIST, THE EXECUTION SEQUENCE CONTINUES AS THOUGH
!      A  CONTINUE  STATEMENT WERE EXECUTED.                             
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 252  -  TEST 009  ****                         
!                                                                        
!                                                                        
!         TEST 009 USES A VALUE OF THE INDEX OF THE COMPUTED GO TO       
!      STATEMENT GREATER THAN THE NUMBER OF STATEMENT LABELS IN THE      
!      PARENTHESIZED LIST.                                               
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      k = 3                                                             
      goto 0094                                                        
 0092 ivcomp = 0                                                        
      goto 40090                                                       
 0093 ivcomp = 0                                                        
      goto 40090                                                       
 0094 go to ( 0092, 0093 )  k                                           
!                                                                        
!         TO REACH THIS STATEMENT THE COMPUTED GO TO WILL HAVE TO BE     
!      EXECUTED AS IF IT WERE A CONTINUE STATEMENT.                      
!                                                                        
      ivcomp = 1                                                        
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
!      ****  FCVS PROGRAM 252  -  TEST 010  ****                         
!                                                                        
!         TEST 010 USES A VALUE OF THE INDEX OF THE COMPUTED GO TO       
!      STATEMENT EQUAL TO ZERO.                                          
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      i = 0                                                             
      goto 0104                                                        
 0102 ivcomp = 0                                                        
      goto 40100                                                       
 0103 ivcomp = 0                                                        
      goto 40100                                                       
 0104 go to ( 0103, 0102 ), i                                           
!                                                                        
!         THIS STATEMENT CAN ONLY BE REACHED IF THE COMPUTED GO TO       
!      IS EXECUTED AS IF IT WERE A CONTINUE STATEMENT.                   
!                                                                        
      ivcomp = 1                                                        
40100 if ( ivcomp - 1 )  20100, 10100, 20100                            
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
!      ****  FCVS PROGRAM 252  -  TEST 011  ****                         
!                                                                        
!         TEST 011 USES A VALUE OF THE INDEX OF THE COMPUTED GO TO       
!      EQUAL TO -1.                                                      
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      j = -1                                                            
      goto 0114                                                        
 0112 ivcomp = 0                                                        
      goto 40110                                                       
 0113 ivcomp = 0                                                        
      goto 40110                                                       
 0114 go to  (0112,0113),j                                              
!                                                                        
!         THIS STATEMENT CAN ONLY BE REACHED IF THE COMPUTED GO TO       
!      IS EXECUTED AS IF IT WERE A CONTINUE STATEMENT.                   
!                                                                        
      ivcomp = 1                                                        
40110 if ( ivcomp - 1 )  20110, 10110, 20110                            
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
90001 format (" ",24x,"FM252")                                          
90000 format (" ",20x,"END OF PROGRAM FM252" )                          
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
      end program fm252
