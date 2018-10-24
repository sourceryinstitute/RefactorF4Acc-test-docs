      program fm037
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
!      COMMENT SECTION                                                   
!                                                                        
!      FM037                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM                                                              
!               INTEGER VARIABLE = ARITHMETIC EXPRESSION                 
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THREE INTEGER      
!      CONSTANTS AND THE ARITHMETIC OPERATOR /.  BOTH POSITIVE AND NEGA- 
!      TIVE CONSTANTS ARE USED IN THE ARITHMETIC EXPRESSION.             
!                                                                        
!          THERE ARE TESTS WHICH REQUIRE NO TRUNCATION OF THE RESULT     
!      AND TESTS WHERE THE RESULT MUST BE TRUNCATED BEFORE BEING STORED  
!      IN THE RESULTANT INTEGER VARIABLE.  THE STANDARD STATES 'THE VALUE
!      OF AN INTEGER FACTOR OR TERM IS THE NEAREST INTEGER WHOSE MAGNI-  
!      TUDE DOES NOT EXCEED THE MAGNITUDE OF THE MATHEMATICAL VALUE      
!      REPRESENTED BY THAT FACTOR OR TERM.  THE ASSOCIATIVE AND COMMUTA- 
!      TIVE LAWS DO NOT APPLY IN THE EVALUATION OF INTEGER TERMS CON-    
!      TAINING DIVISION, HENCE THE EVALUATION OF SUCH TERMS MUST EFFEC-  
!      TIVELY PROCEED FROM LEFT TO RIGHT.'                               
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!              (1)  INTEGER CONSTANT/INTEGER CONSTANT/INTEGER CONSTANT   
!                       NO TRUNCATION REQUIRED                           
!              (2)  INTEGER CONSTANT/INTEGER CONSTANT/INTEGER CONSTANT   
!                       TRUNCATION REQUIRED                              
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
!      TEST 491 THROUGH TEST 519 CONTAIN THREE INTEGER CONSTANTS AND     
!      OPERATOR / IN AN ARITHMETIC EXPRESSION.  THE FORM TESTED IS       
!          INTEGER VARIABLE = INTEGER CONSTANT/INTEGER CONSTANT/INT.CON. 
!                                                                        
!                                                                        
!      TEST 491 THROUGH TEST 496 - POSITIVE INTEGER CONSTANTS            
!                        NO TRUNCATION REQUIRED                          
!                                                                        
 4911 continue                                                          
      ivtnum = 491                                                      
!                                                                        
!       ****  TEST 491  ****                                             
!                                                                        
      if (iczero) 34910, 4910, 34910                                    
 4910 continue                                                          
      ivcomp = 24/3/4                                                   
      goto 44910                                                       
34910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44910, 4921, 44910                                    
44910 if (ivcomp - 2) 24910,14910,24910                                 
14910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4921                                                        
24910 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4921 continue                                                          
      ivtnum = 492                                                      
!                                                                        
!       ****  TEST 492  ****                                             
!                                                                        
      if (iczero) 34920, 4920, 34920                                    
 4920 continue                                                          
      ivcomp = 330/3/2                                                  
      goto 44920                                                       
34920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44920, 4931, 44920                                    
44920 if (ivcomp - 55) 24920,14920,24920                                
14920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4931                                                        
24920 ivfail = ivfail + 1                                               
      ivcorr = 55                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4931 continue                                                          
      ivtnum = 493                                                      
!                                                                        
!       ****  TEST 493  ****                                             
!                                                                        
      if (iczero) 34930, 4930, 34930                                    
 4930 continue                                                          
      ivcomp = 15249/13/51                                              
      goto 44930                                                       
34930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44930, 4941, 44930                                    
44930 if (ivcomp - 23) 24930,14930,24930                                
14930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4941                                                        
24930 ivfail = ivfail + 1                                               
      ivcorr = 23                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4941 continue                                                          
      ivtnum = 494                                                      
!                                                                        
!       ****  TEST 494  ****                                             
!                                                                        
      if (iczero) 34940, 4940, 34940                                    
 4940 continue                                                          
      ivcomp = 7150/2/25                                                
      goto 44940                                                       
34940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44940, 4951, 44940                                    
44940 if (ivcomp - 143) 24940,14940,24940                               
14940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4951                                                        
24940 ivfail = ivfail + 1                                               
      ivcorr = 143                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4951 continue                                                          
      ivtnum = 495                                                      
!                                                                        
!       ****  TEST 495  ****                                             
!                                                                        
      if (iczero) 34950, 4950, 34950                                    
 4950 continue                                                          
      ivcomp = 32766/2/3                                                
      goto 44950                                                       
34950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44950, 4961, 44950                                    
44950 if (ivcomp - 5461) 24950,14950,24950                              
14950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4961                                                        
24950 ivfail = ivfail + 1                                               
      ivcorr = 5461                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4961 continue                                                          
      ivtnum = 496                                                      
!                                                                        
!       ****  TEST 496  ****                                             
!                                                                        
      if (iczero) 34960, 4960, 34960                                    
 4960 continue                                                          
      ivcomp = 32766/1/1                                                
      goto 44960                                                       
34960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44960, 4971, 44960                                    
44960 if (ivcomp - 32766) 24960,14960,24960                             
14960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4971                                                        
24960 ivfail = ivfail + 1                                               
      ivcorr = 32766                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 497 THROUGH TEST 502 - POSITIVE INTEGER CONSTANTS            
!                   TRUNCATION REQUIRED                                  
!                                                                        
 4971 continue                                                          
      ivtnum = 497                                                      
!                                                                        
!       ****  TEST 497  ****                                             
!                                                                        
      if (iczero) 34970, 4970, 34970                                    
 4970 continue                                                          
      ivcomp = 24/3/3                                                   
      goto 44970                                                       
34970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44970, 4981, 44970                                    
44970 if (ivcomp -2) 24970,14970,24970                                  
14970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4981                                                        
24970 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4981 continue                                                          
      ivtnum = 498                                                      
!                                                                        
!       ****  TEST 498  ****                                             
!                                                                        
      if (iczero) 34980, 4980, 34980                                    
 4980 continue                                                          
      ivcomp = 230/2/3                                                  
      goto 44980                                                       
34980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44980, 4991, 44980                                    
44980 if (ivcomp - 38) 24980,14980,24980                                
14980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 4991                                                        
24980 ivfail = ivfail + 1                                               
      ivcorr = 38                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 4991 continue                                                          
      ivtnum = 499                                                      
!                                                                        
!       ****  TEST 499  ****                                             
!                                                                        
      if (iczero) 34990, 4990, 34990                                    
 4990 continue                                                          
      ivcomp = 7151/3/10                                                
      goto 44990                                                       
34990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 44990, 5001, 44990                                    
44990 if (ivcomp - 238) 24990,14990,24990                               
14990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5001                                                        
24990 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5001 continue                                                          
      ivtnum = 500                                                      
!                                                                        
!       ****  TEST 500  ****                                             
!                                                                        
      if (iczero) 35000, 5000, 35000                                    
 5000 continue                                                          
      ivcomp = 15248/51/13                                              
      goto 45000                                                       
35000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45000, 5011, 45000                                    
45000 if (ivcomp - 22) 25000,15000,25000                                
15000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5011                                                        
25000 ivfail = ivfail + 1                                               
      ivcorr = 22                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5011 continue                                                          
      ivtnum = 501                                                      
!                                                                        
!       ****  TEST 501  ****                                             
!                                                                        
      if (iczero) 35010, 5010, 35010                                    
 5010 continue                                                          
      ivcomp = 27342/4/3                                                
      goto 45010                                                       
35010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45010, 5021, 45010                                    
45010 if (ivcomp - 2278) 25010,15010,25010                              
15010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5021                                                        
25010 ivfail = ivfail + 1                                               
      ivcorr = 2278                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5021 continue                                                          
      ivtnum = 502                                                      
!                                                                        
!       ****  TEST 502  ****                                             
!                                                                        
      if (iczero) 35020, 5020, 35020                                    
 5020 continue                                                          
      ivcomp = 32767/2/1                                                
      goto 45020                                                       
35020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45020, 5031, 45020                                    
45020 if (ivcomp - 16383) 25020,15020,25020                             
15020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5031                                                        
25020 ivfail = ivfail + 1                                               
      ivcorr = 16383                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 503 THROUGH TEST 507 - NEGATIVE INTEGER CONSTANTS INCLUDED   
!                   NO TRUNCATION REQUIRED                               
!                                                                        
 5031 continue                                                          
      ivtnum = 503                                                      
!                                                                        
!       ****  TEST 503  ****                                             
!                                                                        
      if (iczero) 35030, 5030, 35030                                    
 5030 continue                                                          
      ivcomp = -24/3/4                                                  
      goto 45030                                                       
35030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45030, 5041, 45030                                    
45030 if (ivcomp +2) 25030,15030,25030                                  
15030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5041                                                        
25030 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5041 continue                                                          
      ivtnum = 504                                                      
!                                                                        
!       ****  TEST 504  ****                                             
!                                                                        
      if (iczero) 35040, 5040, 35040                                    
 5040 continue                                                          
      ivcomp = 330/(-3)/2                                               
      goto 45040                                                       
35040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45040, 5051, 45040                                    
45040 if (ivcomp + 55) 25040,15040,25040                                
15040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5051                                                        
25040 ivfail = ivfail + 1                                               
      ivcorr = -55                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5051 continue                                                          
      ivtnum = 505                                                      
!                                                                        
!       ****  TEST 505  ****                                             
!                                                                        
      if (iczero) 35050, 5050, 35050                                    
 5050 continue                                                          
      ivcomp = 15249/(-13)/(-51)                                        
      goto 45050                                                       
35050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45050, 5061, 45050                                    
45050 if (ivcomp - 23) 25050,15050,25050                                
15050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5061                                                        
25050 ivfail = ivfail + 1                                               
      ivcorr = 23                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5061 continue                                                          
      ivtnum = 506                                                      
!                                                                        
!       ****  TEST 506  ****                                             
!                                                                        
      if (iczero) 35060, 5060, 35060                                    
 5060 continue                                                          
      ivcomp = -7150/(-2)/(-25)                                         
      goto 45060                                                       
35060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45060, 5071, 45060                                    
45060 if (ivcomp + 143) 25060,15060,25060                               
15060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5071                                                        
25060 ivfail = ivfail + 1                                               
      ivcorr = -143                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5071 continue                                                          
      ivtnum = 507                                                      
!                                                                        
!       ****  TEST 507  ****                                             
!                                                                        
      if (iczero) 35070, 5070, 35070                                    
 5070 continue                                                          
      ivcomp = (-32766)/(-2)/(-3)                                       
      goto 45070                                                       
35070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45070, 5081, 45070                                    
45070 if (ivcomp + 5461) 25070,15070,25070                              
15070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5081                                                        
25070 ivfail = ivfail + 1                                               
      ivcorr = -5461                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 508 THROUGH TEST 513 - NEGATIVE INTEGER CONSTANTS INCLUDED   
!                        TRUNCATION REQUIRED                             
!                                                                        
 5081 continue                                                          
      ivtnum = 508                                                      
!                                                                        
!       ****  TEST 508  ****                                             
!                                                                        
      if (iczero) 35080, 5080, 35080                                    
 5080 continue                                                          
      ivcomp = -24/3/3                                                  
      goto 45080                                                       
35080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45080, 5091, 45080                                    
45080 if (ivcomp + 2) 25080,15080,25080                                 
15080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5091                                                        
25080 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5091 continue                                                          
      ivtnum = 509                                                      
!                                                                        
!       ****  TEST 509  ****                                             
!                                                                        
      if (iczero) 35090, 5090, 35090                                    
 5090 continue                                                          
      ivcomp = 230/(-2)/3                                               
      goto 45090                                                       
35090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45090, 5101, 45090                                    
45090 if (ivcomp + 38) 25090,15090,25090                                
15090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5101                                                        
25090 ivfail = ivfail + 1                                               
      ivcorr = -38                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5101 continue                                                          
      ivtnum = 510                                                      
!                                                                        
!       ****  TEST 510  ****                                             
!                                                                        
      if (iczero) 35100, 5100, 35100                                    
 5100 continue                                                          
      ivcomp = 7151/(-3)/(-10)                                          
      goto 45100                                                       
35100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45100, 5111, 45100                                    
45100 if (ivcomp - 238) 25100,15100,25100                               
15100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5111                                                        
25100 ivfail = ivfail + 1                                               
      ivcorr = 238                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5111 continue                                                          
      ivtnum = 511                                                      
!                                                                        
!       ****  TEST 511  ****                                             
!                                                                        
      if (iczero) 35110, 5110, 35110                                    
 5110 continue                                                          
      ivcomp = -15248/(-51)/(-13)                                       
      goto 45110                                                       
35110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45110, 5121, 45110                                    
45110 if (ivcomp + 22) 25110,15110,25110                                
15110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5121                                                        
25110 ivfail = ivfail + 1                                               
      ivcorr = -22                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5121 continue                                                          
      ivtnum = 512                                                      
!                                                                        
!       ****  TEST 512  ****                                             
!                                                                        
      if (iczero) 35120, 5120, 35120                                    
 5120 continue                                                          
      ivcomp = (-27342)/(-4)/(-3)                                       
      goto 45120                                                       
35120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45120, 5131, 45120                                    
45120 if (ivcomp + 2278) 25120,15120,25120                              
15120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5131                                                        
25120 ivfail = ivfail + 1                                               
      ivcorr = -2278                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5131 continue                                                          
      ivtnum = 513                                                      
!                                                                        
!       ****  TEST 513  ****                                             
!                                                                        
      if (iczero) 35130, 5130, 35130                                    
 5130 continue                                                          
      ivcomp = 32767/2/(-1)                                             
      goto 45130                                                       
35130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45130, 5141, 45130                                    
45130 if (ivcomp + 16383) 25130,15130,25130                             
15130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5141                                                        
25130 ivfail = ivfail + 1                                               
      ivcorr = -16383                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 514 THROUGH TEST 519 - POSITIVE AND NEGATIVE SIGNED INTEGER  
!            CONSTANTS IN ARITHMETIC EXPRESSION.                         
!                                                                        
 5141 continue                                                          
      ivtnum = 514                                                      
!                                                                        
!       ****  TEST 514  ****                                             
!                                                                        
      if (iczero) 35140, 5140, 35140                                    
 5140 continue                                                          
      ivcomp = +24/(-3)/4                                               
      goto 45140                                                       
35140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45140, 5151, 45140                                    
45140 if (ivcomp +2) 25140,15140,25140                                  
15140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5151                                                        
25140 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5151 continue                                                          
      ivtnum = 515                                                      
!                                                                        
!       ****  TEST 515  ****                                             
!                                                                        
      if (iczero) 35150, 5150, 35150                                    
 5150 continue                                                          
      ivcomp = 24/(+3)/(-4)                                             
      goto 45150                                                       
35150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45150, 5161, 45150                                    
45150 if (ivcomp +2) 25150,15150,25150                                  
15150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5161                                                        
25150 ivfail = ivfail + 1                                               
      ivcorr = -2                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5161 continue                                                          
      ivtnum = 516                                                      
!                                                                        
!       ****  TEST 516  ****                                             
!                                                                        
      if (iczero) 35160, 5160, 35160                                    
 5160 continue                                                          
      ivcomp = -24/(-3)/(+4)                                            
      goto 45160                                                       
35160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45160, 5171, 45160                                    
45160 if (ivcomp -2) 25160,15160,25160                                  
15160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5171                                                        
25160 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5171 continue                                                          
      ivtnum = 517                                                      
!                                                                        
!       ****  TEST 517  ****                                             
!                                                                        
      if (iczero) 35170, 5170, 35170                                    
 5170 continue                                                          
      ivcomp = -16811/(-16812)/(+1)                                     
      goto 45170                                                       
35170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45170, 5181, 45170                                    
45170 if (ivcomp - 0) 25170,15170,25170                                 
15170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5181                                                        
25170 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5181 continue                                                          
      ivtnum = 518                                                      
!                                                                        
!       ****  TEST 518  ****                                             
!                                                                        
      if (iczero) 35180, 5180, 35180                                    
 5180 continue                                                          
      ivcomp = (-16811) / (+16811) / (+1)                               
      goto 45180                                                       
35180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45180, 5191, 45180                                    
45180 if (ivcomp +1) 25180,15180,25180                                  
15180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5191                                                        
25180 ivfail = ivfail + 1                                               
      ivcorr = -1                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5191 continue                                                          
      ivtnum = 519                                                      
!                                                                        
!       ****  TEST 519  ****                                             
!                                                                        
      if (iczero) 35190, 5190, 35190                                    
 5190 continue                                                          
      ivcomp = (-335)/(+168)/(+1)                                       
      goto 45190                                                       
35190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45190, 5201, 45190                                    
45190 if (ivcomp + 1) 25190,15190,25190                                 
15190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5201                                                        
25190 ivfail = ivfail + 1                                               
      ivcorr = -1                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!       ****    END OF TESTS    ****                                     
 5201 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM037" )                          
      end program fm037
