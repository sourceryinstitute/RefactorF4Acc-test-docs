      program fm202
!                                                                        
!                                                                        
!          THIS ROUTINE IS THE FIRST ROUTINE TO TEST CHARACTER DATA      
!      TYPES.  CHARACTER TYPE-STATEMENTS SPECIFY CHARACTER VARIABLES OF  
!      LENGTH ONE AND LENGTH TWO.  THE TESTS IN THIS ROUTINE DETERMINE   
!      THAT THE FOLLOWING LANGUAGE FEATURES FUNCTION CORRECTLY.          
!                                                                        
!          (1) CHARACTER ASSIGNMENT STATEMENTS OF THE FORM               
!                                                                        
!              CHARACTER VARIABLE = CHARACTER CONSTANT                   
!              CHARACTER VARIABLE = CHARACTER VARIABLE                   
!                                                                        
!          WHERE THE VARIABLES AND CONSTANTS ARE THE SAME LENGTH.        
!                                                                        
!          (2)  THE REPRESENTATION OF AN APOSTROPHE IN A CHARACTER       
!          CONSTANT IS TWO CONSECUTIVE APOSTROPHES WITH NO INTERVENING   
!          BLANKS.                                                       
!                                                                        
!          (3)  CHARACTER RELATIONAL EXPRESSION OF THE FORM              
!                                                                        
!               CHARACTER VARIABLE  RELOP  CHARACTER CONSTANT            
!               CHARACTER CONSTANT  RELOP  CHARACTER VARIABLE            
!               CHARACTER VARIABLE  RELOP  CHARACTER VARIABLE            
!                                                                        
!          WHERE THE CHARACTER ENTITIES ARE THE SAME LENGTH.             
!                                                                        
!          (4)  CHARACTER RELATIONAL EXPRESSIONS OF THE FORM             
!                                                                        
!               CHARACTER VARIABLE .EQ. CHARACTER CONSTANT               
!                                                                        
!          ARE USED IN THIS ROUTINE TO VERIFY THE CHARACTER ASSIGNMENT   
!          STATEMENTS.                                                   
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!          SECTION 4.8,   CHARACTER TYPE                                 
!          SECTION 4.8.1, CHARACTER CONSTANT                             
!          SECTION 6.2,   CHARACTER EXPRESSIONS                          
!          SECTION 6.3.4, CHARACTER RELATIONAL EXPRESSION                
!          SECTION 6.3.5, INTERPRETATION OF CHARACTER RELATIONAL         
!                           EXPRESSIONS                                  
!          SECTION 8.4.2, CHARACTER TYPE-STATEMENT                       
!          SECTION 10.4,  CHARACTER ASSIGNMENT STATEMENT                 
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
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      character(len=1) :: cvtn01
      character(len=1) :: cvtn02
      character(len=2) :: cvtn03
      character(len=2) :: cvtn04
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
!          TEST 1 THROUGH TEST 6 VERIFY THAT THE CHARACTER ASSIGNMENT    
!      STATEMENT                                                         
!                                                                        
!         CHARACTER VARIABLE (LEN 1) = CHARACTER CONSTANT (LEN 1)        
!                                                                        
!      IS CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                  
!                                                                        
!         CHARACTER VARIABLE (LEN 1) RELOP CHARACTER CONSTANT (LEN 1)    
!                                                                        
!      IS USED TO VERIFY THE ASSIGNMENT STATEMENT.  BOTH OF THE ABOVE    
!      STATEMENTS MUST MEET THE LANGUAGE SPECIFICATIONS FOR THESE TESTS  
!      TO PASS.                                                          
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 001  ****                         
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      cvtn01 = ' '                                                      
      ivcorr = 1                                                        
      if (cvtn01  ==  ' ') ivcomp = 1                                   
40010 if (ivcomp - 1) 20010,10010,20010                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 002  ****                         
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 0                                                        
      cvtn01 = 'M'                                                      
      ivcorr = 1                                                        
      if (cvtn01  ==  'M') ivcomp = 1                                   
40020 if (ivcomp - 1) 20020,10020,20020                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 003  ****                         
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '4'                                                      
      if (cvtn01  ==  '4') ivcomp = 1                                   
40030 if (ivcomp - 1) 20030,10030,20030                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 004  ****                         
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '='                                                      
      if (cvtn01  ==  '=') ivcomp = 1                                   
40040 if (ivcomp - 1) 20040,10040,20040                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 005  ****                         
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '/'                                                      
      if (cvtn01  ==  '/') ivcomp = 1                                   
40050 if (ivcomp - 1) 20050,10050,20050                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 006  ****                         
!                                                                        
!          AN APOSTROPHE IN A CHARACTER CONSTANT IS REPRESENTED BY TWO   
!      CONSECUTIVE APOSTROPHES WITH NO INTERVENING BLANKS.               
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = ''''                                                     
      if (cvtn01  ==  '''') ivcomp = 1                                  
40060 if (ivcomp - 1) 20060,10060,20060                                 
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
!          TEST 7 THROUGH TEST 12 VERIFY THAT THE CHARACTER ASSIGNMENT   
!      STATEMENTS                                                        
!                                                                        
!          CHARACTER VARIABLE (LEN 1) = CHARACTER CONSTANT (LEN 1)       
!          CHARACTER VARIABLE (LEN 1) = CHARACTER VARIABLE (LEN 1)       
!                                                                        
!      ARE CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                 
!                                                                        
!          CHARACTER VARIABLE (LEN 1) .EQ. CHARACTER CONSTANT (LEN 1)    
!                                                                        
!      IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENTS.        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 007  ****                         
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = ' '                                                      
      cvtn02 = cvtn01                                                   
      if (cvtn02  ==  ' ') ivcomp = 1                                   
40070 if (ivcomp - 1) 20070, 10070, 20070                               
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
!      ****  FCVS PROGRAM 202  -  TEST 008  ****                         
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = 'M'                                                      
      cvtn02 = cvtn01                                                   
      if (cvtn02  ==  'M') ivcomp = 1                                   
40080 if (ivcomp - 1) 20080,10080,20080                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 009  ****                         
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '4'                                                      
      cvtn02 = cvtn01                                                   
      if (cvtn02  ==  '4') ivcomp = 1                                   
40090 if (ivcomp - 1) 20090,10090,20090                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 010  ****                         
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '='                                                      
      cvtn02 = cvtn01                                                   
      if (cvtn02  ==  '=') ivcomp = 1                                   
40100 if (ivcomp - 1) 20100,10100,20100                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 011  ****                         
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp =0                                                         
      ivcorr = 1                                                        
      cvtn01 = '/'                                                      
      cvtn02 = cvtn01                                                   
      if (cvtn02  ==  '/') ivcomp = 1                                   
40110 if (ivcomp - 1) 20110,10110,20110                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 012  ****                         
!                                                                        
!          AN APOSTROPHE IN A CHARACTER CONSTANT IS REPRESENTED BY TWO   
!      CONSECUTIVE APOSTROPHES WITH NO INTERVENING BLANKS.               
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = ''''                                                     
      cvtn02 = cvtn01                                                   
      if (cvtn02  ==  '''') ivcomp = 1                                  
40120 if (ivcomp - 1) 20120,10120,20120                                 
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
!          TEST 13 THROUGH TEST 18 VERIFY THE RESULTS OF THE CHARACTER   
!      RELATIONAL EXPRESSION USING EACH OF THE SIX RELATIONAL OPERATORS  
!      IN THE STATEMENT FORM                                             
!                                                                        
!          CHARACTER VARIABLE (LEN 1) RELOP CHARACTER CONSTANT (LEN 1).  
!                                                                        
!      THE VARIABLE AND CONSTANT CONTAIN THE CHARACTER DATUM C.          
!                                                                        
      cvtn01 = 'C'                                                      
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 013  ****                         
!                                                                        
!          RELATIONAL OPERATOR .EQ.                                      
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn01  ==  'C') ivcomp = 1                                   
40130 if (ivcomp - 1) 20130,10130,20130                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 014  ****                         
!                                                                        
!          RELATIONAL OPERATOR .NE.                                      
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
      if (cvtn01  /=  'C') ivcomp = 1                                   
40140 if (ivcomp) 20140,10140,20140                                     
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
!      ****  FCVS PROGRAM 202  -  TEST 015  ****                         
!                                                                        
!          RELATIONAL OPERATOR .LE.                                      
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn01  <=  'C') ivcomp = 1                                   
      if (ivcomp - 1) 20150,10150,20150                                 
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
!      ****  FCVS PROGRAM 202  -  TEST 016  ****                         
!                                                                        
!          RELATIONAL OPERATOR .LT.                                      
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp=0                                                          
      ivcorr=0                                                          
      if (cvtn01  <  'C') ivcomp = 1                                   
      if (ivcomp) 20160,10160,20160                                     
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
!      ****  FCVS PROGRAM 202  -  TEST 017  ****                         
!                                                                        
!          RELATIONAL OPERATOR .GE.                                      
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn01  >=  'C') ivcomp = 1                                   
40170 if (ivcomp - 1) 20170,10170,20170                                 
30170 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10170, 0181, 20170                                    
10170 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0181                                                        
20170 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0181 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 018  ****                         
!                                                                        
!          RELATIONAL OPERATOR .GT.                                      
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
      if (cvtn01  >  'C') ivcomp = 1                                   
40180 if (ivcomp) 20180,10180,20180                                     
30180 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10180, 0191, 20180                                    
10180 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0191                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0191 continue                                                          
!                                                                        
!          TEST 19 THROUGH TEST 21 VERIFY THAT THE CHARACTER ASSIGNMENT  
!      STATEMENT                                                         
!                                                                        
!          CHARACTER VARIABLE (LEN 2) = CHARACTER CONSTANT (LEN 2)       
!                                                                        
!      OPERATES CORRECTLY.  THE CHARACTER RELATIONAL EXPRESSION          
!                                                                        
!          CHARACTER VARIABLE (LEN 2) .EQ. CHARACTER CONSTANT (LEN 2)    
!                                                                        
!      IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENT.         
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 019  ****                         
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp =0                                                         
      ivcorr =1                                                         
      cvtn03 = 'AZ'                                                     
      if (cvtn03  ==  'AZ') ivcomp = 1                                  
40190 if (ivcomp - 1) 20190,10190,20190                                 
30190 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10190, 0201, 20190                                    
10190 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0201                                                        
20190 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0201 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 020  ****                         
!                                                                        
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = 'B'''                                                    
      if (cvtn03  ==  'B''') ivcomp = 1                                 
40200 if (ivcomp - 1) 20200,10200,20200                                 
30200 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10200, 0211, 20200                                    
10200 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0211                                                        
20200 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0211 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 021  ****                         
!                                                                        
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = '//'                                                     
      if (cvtn03  ==  '//') ivcomp = 1                                  
40210 if (ivcomp - 1) 20210,10210,20210                                 
30210 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10210, 0221, 20210                                    
10210 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0221                                                        
20210 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0221 continue                                                          
!                                                                        
!          TEST 22 THROUGH TEST 24 VERIFY THAT THE CHARACTER ASSIGNMENT  
!      STATEMENTS                                                        
!                                                                        
!          CHARACTER VARIABLE (LEN 2) = CHARACTER CONSTANT (LEN 2)       
!          CHARACTER VARIABLE (LEN 2) = CHARACTER VARIABLE (LEN 2)       
!                                                                        
!      OPERATE CORRECTLY.                                                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 022  ****                         
!                                                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = 'AZ'                                                     
      cvtn04 = cvtn03                                                   
      if (cvtn04  ==  'AZ') ivcomp=1                                    
40220 if (ivcomp - 1) 20220,10220,20220                                 
30220 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10220, 0231, 20220                                    
10220 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0231                                                        
20220 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0231 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 023  ****                         
!                                                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = 'B'''                                                    
      cvtn04 = cvtn03                                                   
      if (cvtn04  ==  'B''') ivcomp = 1                                 
40230 if (ivcomp - 1) 20230,10230,20230                                 
30230 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10230, 0241, 20230                                    
10230 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0241                                                        
20230 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0241 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 024  ****                         
!                                                                        
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = '//'                                                     
      cvtn04 = cvtn03                                                   
      if (cvtn04  ==  '//') ivcomp = 1                                  
40240 if (ivcomp - 1) 20240,10240,20240                                 
30240 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10240, 0251, 20240                                    
10240 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0251                                                        
20240 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0251 continue                                                          
!                                                                        
!          TEST 25 THROUGH TEST 30 VERIFY THE RESULTS OF THE CHARACTER   
!      RELATIONAL EXPRESSION USING EACH OF THE SIX RELATIONAL OPERATORS  
!      IN THE EXPRESSION FORM                                            
!                                                                        
!          CHARACTER VARIABLE (LEN 2) RELOP CHARACTER VARIABLE (LEN 2)   
!                                                                        
!      THE VARIABLES CONTAIN THE CHARACTER DATUM CC.                     
!                                                                        
      cvtn03 = 'CC'                                                     
      cvtn04 = 'CC'                                                     
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 025  ****                         
!                                                                        
!          RELATIONAL OPERATOR .EQ.                                      
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn03  ==  cvtn04) ivcomp = 1                                
40250 if (ivcomp - 1) 20250,10250,20250                                 
30250 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10250, 0261, 20250                                    
10250 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0261                                                        
20250 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0261 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 026  ****                         
!                                                                        
!          RELATIONAL OPERATOR .NE.                                      
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
      if (cvtn03  /=  cvtn04) ivcomp = 1                                
40260 if (ivcomp) 20260,10260,20260                                     
30260 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10260, 0271, 20260                                    
10260 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0271                                                        
20260 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0271 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 027  ****                         
!                                                                        
!          RELATIONAL OPERATOR .LE.                                      
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn03  <=  cvtn04) ivcomp = 1                                
40270 if (ivcomp - 1) 20270,10270,20270                                 
30270 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10270, 0281, 20270                                    
10270 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0281                                                        
20270 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0281 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 028  ****                         
!                                                                        
!          RELATIONAL OPERATOR .LT.                                      
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
      if (cvtn03  <  cvtn04) ivcomp=1                                  
40280 if (ivcomp) 20280,10280,20280                                     
30280 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10280, 0291, 20280                                    
10280 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0291                                                        
20280 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0291 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 029  ****                         
!                                                                        
!          RELATIONAL OPERATOR .GE.                                      
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn03  >=  cvtn04) ivcomp = 1                                
40290 if (ivcomp - 1) 20290,10290,20290                                 
30290 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10290, 0301, 20290                                    
10290 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0301                                                        
20290 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0301 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 202  -  TEST 030  ****                         
!                                                                        
!          RELATIONAL OPERATOR .GT.                                      
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 0                                                        
      if (cvtn03  >  cvtn04) ivcomp = 1                                
40300 if (ivcomp) 20300,10300,20300                                     
30300 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10300, 0311, 20300                                    
10300 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0311                                                        
20300 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0311 continue                                                          
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
90001 format (" ",24x,"FM202")                                          
90000 format (" ",20x,"END OF PROGRAM FM202" )                          
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
      end program fm202
