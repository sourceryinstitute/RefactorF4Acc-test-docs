      program fm199
!     WV: SANE VERSION OF FM200, removed spaces in tests 9 and 11        
!                                                                        
!         THIS ROUTINE IS THE FIRST AUDIT PROGRAM TO CONTAIN A PROGRAM   
!      STATEMENT.  THE FOLLOWING FEATURES FROM CHAPTER 3., CHARACTERS,   
!      LINES AND EXECUTION SEQUENCE ARE TESTED.                          
!                                                                        
!         (1)  ASTERISK (*) IN COLUMN 1 TO DESIGNATE A COMMENT LINE.     
!         (2)  USE OF NON-FORTRAN CHARACTERS WITHIN A COMMENT LINE.      
!         (3)  STATEMENT LABELS ON NONEXECUTABLE STATEMENTS.             
!         (4)  DIGIT 0 IN COLUMN 6 OF AN INITIAL LINE.                   
!         (5)  CONTINUATION LINES - MAXIMUM OF NINE CONTINUATION LINES   
!              (660 CHARACTERS).                                         
!         (6)  BLANK CHARACTERS WITHIN STATEMENTS.                       
!         (7)  BLANK COMMENT LINE, BLANK CHARACTERS IN COLUMNS 1-72.     
!                                                                        
!      THE BASIC FEATURES OF SUBSET FORTRAN WHICH ARE TESTED BY THIS     
!      PROGRAM ARE USED THROUGHOUT THE REST OF THE SUBSET ROUTINES.      
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 3.1.6, BLANK CHARACTER                                 
!         SECTION 3.2.1, COMMENT LINE                                    
!         SECTION 3.2.2, INITIAL LINE                                    
!         SECTION 3.2.3, CONTINUATION LINE                               
!         SECTION 3.3, STATEMENTS                                        
!         SECTION 3.4, STATEMENT LABEL                                   
!         SECTION 14.1, PROGRAM STATEMENT                                
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
      integer :: ivon02
      integer :: ivon01
   integer :: xvtn01
   22 data ivon02 / 5 / 
!         THE PRECEDING STATEMENTS ARE NONEXECUTABLE STATEMENTS WHICH    
!      CONTAIN STATEMENT LABELS.  THEY ARE REFERENCED IN TESTS 1 AND 2.  
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
!         TEST 1 AND TEST 2 REFERENCE VARIABLES DEFINED IN NONEXECUTABLE 
!      STATEMENTS WHICH CONTAIN STATEMENT LABELS.  THE NONEXECUTABLE     
!      STATEMENTS WHICH APPEAR AT THE BEGINNING OF THE PROGRAM ARE       
!             12 INTEGER XVTN01                                          
!             22 DATA IVON02/5/                                          
!                                                                        
!      REFERENCE   X3.9-1977, SECTION 3.4, STATEMENT LABELS              
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 200  -  TEST 001  ****                         
!                                                                        
!         TEST 001 ASSIGNS AN INTEGER VALUE TO XVTN01 WHICH WAS SPECIFIED
!      AS TYPE INTEGER IN AN INTEGER STATEMENT CONTAINING A STATEMENT    
!      LABEL.                                                            
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
       xvtn01 = 1                                                       
      ivcomp = xvtn01                                                   
      ivcorr = 1                                                        
40010 if (ivcomp - 1) 20010, 10010, 20010                               
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
!      ****  FCVS PROGRAM 200  -  TEST 002  ****                         
!                                                                        
!         TEST 002 CHECKS THE VALUE WHICH WAS ASSIGNED TO IVON02 BY A    
!      DATA STATEMENT WITH A STATEMENT LABEL.                            
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 0                                                        
      ivcomp = ivon02                                                   
      ivcorr = 5                                                        
40020 if (ivcomp - 5) 20020, 10020, 20020                               
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
!         TEST 3 THROUGH TEST 5 USE AN ASTERISK (*) IN COLUMN 1 TO       
!      DENOTE A COMMENT LINE.                                            
!                                                                        
!      REFERENCE   X3.9-1977, SECTION 3.2.1, COMMENT LINE                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 200  -  TEST 003  ****                         
!                                                                        
!         GO TO STATEMENT IN ASTERISK COMMENT LINE.                      
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 1                                                        
!           GO TO 20030                                                  
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40030 if (ivcomp) 20030, 10030, 20030                                   
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
!      ****  FCVS PROGRAM 200  -  TEST 004  ****                         
!                                                                        
!         SEVERAL * COMMENT LINES INTERMIXED WITH EXECUTABLE STATEMENTS. 
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
!      THE * COMMENT LINE IS THE SAME AS A C COMMENT LINE.               
      ivcomp = 1                                                        
!      THE * COMMENT LINES HAVE NO EFFECT ON THE PROGRAM EXECUTION.      
!      THEIR USE IS STRICTLY FOR DOCUMENTATION PURPOSES.                 
      ivcomp = 2                                                        
!      IVCOMP = 3                                                        
!   40 ANY STATEMENT LABELS ON COMMENT LINES ARE IGNORED.                
      ivcorr = 2                                                        
40040 if (ivcomp - 2) 20040, 10040, 20040                               
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
!      ****  FCVS PROGRAM 200  -  TEST 005  ****                         
!                                                                        
!         NONFORTRAN CHARACTERS WITHIN C AND * COMMENT LINES.            
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 1                                                        
!           <>%?   NONFORTRAN CHARACTER                                  
!           <>%?   NONFORTRAN CHARACTER                                  
      ivcomp = 0                                                        
      ivcorr = 0                                                        
40050 if (ivcomp) 20050, 10050, 20050                                   
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
!      ****  FCVS PROGRAM 200  -  TEST 006  ****                         
!                                                                        
!         LINES CONTAINING ONLY BLANK CHARACTERS IN COLUMNS 1 THROUGH    
!      72 ARE COMMENT LINES.                                             
!                                                                        
!      REFERENCE   X3.9-1977, SECTION 3.2.1, COMMENT LINE                
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 3                                                        
      ivcomp = 9                                                        
!         ASTERISK COMMENT LINE FOLLOWED BY BLANK COMMENT LINE.          
!         ASTERISK COMMENT LINE.                                         
      ivcomp = 3                                                        
40060 if (ivcomp - 3) 20060, 10060, 20060                               
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
!         TEST 7 AND TEST 8 CONTAIN THE DIGIT 0 IN COLUMN 6 OF INITIAL   
!      LINES.                                                            
!                                                                        
!      REFERENCE   X3.9-1977, SECTION 3.2.2, INITIAL LINE                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 200  -  TEST 007  ****                         
!                                                                        
!         TEST 007 USES THE DIGIT 0 IN COLUMN 6 OF TWO SUCCESSIVE        
!      INITIAL LINES.                                                    
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 5                                                        
      ivon02 = 6                                                        
      ivcomp = ivon01 + ivon02                                          
      ivcorr = 11                                                       
40070 if (ivcomp - 11) 20070, 10070, 20070                              
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
!      ****  FCVS PROGRAM 200  -  TEST 008  ****                         
!                                                                        
!         TEST 008 MIXES STATEMENTS WITH DIGIT 0 IN COLUMN 6 OF INITIAL  
!      LINE AND COMMENT LINES WITH * IN COLUMN 1.                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 0                                                        
!         FIRST INITIAL LINE FOLLOWS.                                    
      ivon01 = 5                                                        
!         TWO SUCCESSIVE COMMENT LINES,                                  
!         FOLLOWED BY TWO INITIAL LINES.                                 
      ivon02=4                                                          
      ivcomp=ivon01+ivon02                                              
!           FALL THROUGH TO VERIFICATION CODE                            
      ivcorr = 9                                                        
40080 if (ivcomp - 9) 20080, 10080, 20080                               
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
!         TEST 9 THROUGH TEST 13 VERIFY THAT CONTINUATION LINES ARE      
!      PERMITTED.                                                        
!                                                                        
!      REFERENCE   X3.9-1977, SECTION 3.2.3, CONTINUATION LINE           
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 200  -  TEST 009  ****                         
!                                                                        
!         STATEMENT WITH TWO CONTINUATION LINES.                         
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivon01 = 0                                                        
      ivon01                                                            = 2                                                        
      ivcomp = ivon01                                                   
      ivcorr = 2                                                        
40090 if (ivcomp - 2) 20090, 10090, 20090                               
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
!      ****  FCVS PROGRAM 200  -  TEST 010  ****                         
!                                                                        
!         STATEMENT WITH NINE CONTINUATION LINES.                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivon01 = 0                                                        
      ivon01 =                                                                   1                                                        +1                                                        +1                                                  +1                                                            +1                                                          +1                                                          +1                              +1          +1                                                                
      ivcomp = ivon01                                                   
      ivcorr = 9                                                        
40100 if (ivcomp - 9) 20100, 10100, 20100                               
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
!      ****  FCVS PROGRAM 200  -  TEST 011  ****                         
!                                                                        
!         TEST 011 CONTAINS THE MAXIMUM NUMBER OF CONTINUATION LINES     
!      PERMITTED IN THE SUBSET LANGUAGE AND EACH OF THE 660 CHARACTERS   
!      IN THE STATEMENT ARE NONBLANK.                                    
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivon01 = 1                                                        
      ivcomp = 0                                                        
      ivcomp=ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+ivon01+12
      ivcorr = 105                                                      
40110 if (ivcomp - 105) 20110, 10110, 20110                             
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
!      ****  FCVS PROGRAM 200  -  TEST 012  ****                         
!                                                                        
!         TEST 012 SPLITS A STATEMENT ACROSS 8 CONTINUATION LINES.       
!      THERE IS A STATEMENT LABEL IN COLUMNS 1-5 AND 0 IN COLUMN 6       
!      OF THE INITIAL LINE.                                              
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivon01 = 0                                                        
      goto 0122                                                        
 0122    ivon01                                                         =                                                                  89                                                      
!     8           9                                                      
      ivcomp = ivon01                                                   
      ivcorr = 89                                                       
40120 if (ivcomp - 89) 20120, 10120, 20120                              
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
!      ****  FCVS PROGRAM 200  -  TEST 013  ****                         
!                                                                        
!         TEST 013 CONSISTS OF AN INITIAL LINE WHICH CONTAINS ONLY A     
!      STATEMENT LABEL AND A CONTINUATION LINE WHICH CONTAINS THE        
!      EXECUTABLE STATEMENT.                                             
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 0                                                        
 0132                                                                   ivcomp = 4                                                        
      ivcorr = 4                                                        
40130 if (ivcomp - 4) 20130, 10130, 20130                               
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
90001 format (" ",24x,"FM199")                                          
90000 format (" ",20x,"END OF PROGRAM FM199" )                          
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
      end program fm199
