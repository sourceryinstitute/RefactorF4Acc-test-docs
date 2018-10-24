      program fm008
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
!      COMMENT SECTION.                                                  
!                                                                        
!      FM008                                                             
!                                                                        
!          THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS OF THE    
!      FORM          INTEGER VARIABLE = ARITHMETIC EXPRESSION            
!      WHERE THE ARITHMETIC EXPRESSION IS FORMED WITH THE ARITHMETIC     
!      OPERATOR + INTEGER CONSTANTS AND POSITIVE INTEGER VARIABLES.      
!      SOME OF THE TESTS USE PARENTHESES TO GROUP ELEMENTS IN THE        
!      ARITHMETIC EXPRESSION.                                            
!                                                                        
!          THERE ARE TESTS WHERE THE ARITHMETIC EXPRESSION CONTAINS      
!             (1) TWO INTEGER CONSTANTS,                                 
!             (2) THREE INTEGER CONSTANTS,                               
!             (3) THREE INTEGER CONSTANTS WITH PARENTHESES TO GROUP      
!                    ELEMENTS,                                           
!             (4) ONE INTEGER VARIABLE AND ONE INTEGER CONSTANT,         
!             (5) ONE INTEGER VARIABLE AND TWO INTEGER CONSTANTS,        
!             (6) ONE INTEGER VARIABLE AND TWO INTEGER CONSTANTS WITH    
!                    PARENTHESES TO GROUP ELEMENTS.                      
!                                                                        
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.3, INTEGER TYPE                                      
!         SECTION 4.3.1, INTEGER CONSTANT                                
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 10.1, ARITHMETIC ASSIGNMENT STATEMENTS                 
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
!          ARITHMETIC ASSIGNMENT STATEMENT                               
!                                                                        
!      TEST 200 THROUGH TEST 214 CONTAIN INTEGER CONSTANTS AND OPERATOR +
!      IN ARITHMETIC EXPRESSION.                                         
!                                                                        
!      TEST 200 THROUGH TEST 206 - TWO INTEGER CONSTANTS                 
!                                                                        
 2001 continue                                                          
      ivtnum = 200                                                      
!                                                                        
!       ****  TEST 200  ****                                             
!                                                                        
      if (iczero) 32000, 2000, 32000                                    
 2000 continue                                                          
      ivcomp = 2+3                                                      
      goto 42000                                                       
32000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42000, 2011, 42000                                    
42000 if (ivcomp - 5) 22000,12000,22000                                 
12000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2011                                                        
22000 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2011 continue                                                          
      ivtnum = 201                                                      
!                                                                        
!       ****  TEST 201  ****                                             
!                                                                        
      if (iczero) 32010, 2010, 32010                                    
 2010 continue                                                          
      ivcomp = 51 + 52                                                  
      goto 42010                                                       
32010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42010, 2021, 42010                                    
42010 if (ivcomp - 103) 22010,12010,22010                               
12010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2021                                                        
22010 ivfail = ivfail + 1                                               
      ivcorr = 103                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2021 continue                                                          
      ivtnum = 202                                                      
!                                                                        
!       ****  TEST 202  ****                                             
!                                                                        
      if (iczero) 32020, 2020, 32020                                    
 2020 continue                                                          
      ivcomp = 189 + 676                                                
      goto 42020                                                       
32020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42020, 2031, 42020                                    
42020 if (ivcomp - 865) 22020,12020,22020                               
12020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2031                                                        
22020 ivfail = ivfail + 1                                               
      ivcorr = 865                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2031 continue                                                          
      ivtnum = 203                                                      
!                                                                        
!       ****  TEST 203  ****                                             
!                                                                        
      if (iczero) 32030, 2030, 32030                                    
 2030 continue                                                          
      ivcomp = 1358 + 8001                                              
      goto 42030                                                       
32030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42030, 2041, 42030                                    
42030 if (ivcomp - 9359) 22030, 12030, 22030                            
12030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2041                                                        
22030 ivfail = ivfail + 1                                               
      ivcorr = 9359                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2041 continue                                                          
      ivtnum = 204                                                      
!                                                                        
!       ****  TEST 204  ****                                             
!                                                                        
      if (iczero) 32040, 2040, 32040                                    
 2040 continue                                                          
      ivcomp = 11112 + 10001                                            
      goto 42040                                                       
32040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42040, 2051, 42040                                    
42040 if (ivcomp - 21113) 22040, 12040, 22040                           
12040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2051                                                        
22040 ivfail = ivfail + 1                                               
      ivcorr=21113                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2051 continue                                                          
      ivtnum = 205                                                      
!                                                                        
!       ****  TEST 205  ****                                             
!                                                                        
      if (iczero) 32050, 2050, 32050                                    
 2050 continue                                                          
      ivcomp = 189 + 9876                                               
      goto 42050                                                       
32050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42050, 2061, 42050                                    
42050 if (ivcomp - 10065) 22050,12050,22050                             
12050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2061                                                        
22050 ivfail = ivfail + 1                                               
      ivcorr = 10065                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2061 continue                                                          
      ivtnum = 206                                                      
!                                                                        
!       ****  TEST 206  ****                                             
!           REQUIRES 32767                                               
!                                                                        
      if (iczero) 32060, 2060, 32060                                    
 2060 continue                                                          
      ivcomp = 32752 + 15                                               
      goto 42060                                                       
32060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42060, 2071, 42060                                    
42060 if (ivcomp - 32767) 22060,12060,22060                             
12060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2071                                                        
22060 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 207 THROUGH TEST 210 - THREE INTEGER CONSTANTS               
!                                                                        
 2071 continue                                                          
      ivtnum = 207                                                      
!                                                                        
!       ****  TEST 207  ****                                             
!                                                                        
      if (iczero) 32070, 2070, 32070                                    
 2070 continue                                                          
      ivcomp = 2+3+4                                                    
      goto 42070                                                       
32070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42070, 2081, 42070                                    
42070 if (ivcomp - 9) 22070,12070,22070                                 
12070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2081                                                        
22070 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2081 continue                                                          
      ivtnum = 208                                                      
!                                                                        
!       ****  TEST 208  ****                                             
!                                                                        
      if (iczero) 32080, 2080, 32080                                    
 2080 continue                                                          
      ivcomp = 51 + 52 + 53                                             
      goto 42080                                                       
32080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42080, 2091, 42080                                    
42080 if (ivcomp - 156) 22080,12080,22080                               
12080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2091                                                        
22080 ivfail = ivfail + 1                                               
      ivcorr = 156                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2091 continue                                                          
      ivtnum = 209                                                      
!                                                                        
!       ****  TEST 209  ****                                             
!                                                                        
      if (iczero) 32090, 2090, 32090                                    
 2090 continue                                                          
      ivcomp = 189 +676+101                                             
      goto 42090                                                       
32090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42090, 2101, 42090                                    
42090 if (ivcomp - 966) 22090,12090,22090                               
12090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2101                                                        
22090 ivfail = ivfail + 1                                               
      ivcorr = 966                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2101 continue                                                          
      ivtnum = 210                                                      
!                                                                        
!       ****  TEST 210  ****                                             
!                                                                        
      if (iczero) 32100, 2100, 32100                                    
 2100 continue                                                          
      ivcomp = 1358 + 8001 + 2189                                       
      goto 42100                                                       
32100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42100, 2111, 42100                                    
42100 if (ivcomp - 11548) 22100,12100,22100                             
12100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2111                                                        
22100 ivfail = ivfail + 1                                               
      ivcorr = 11548                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TESTS 211 THROUGH 214 ARE THE SAME AS 207 THROUGH 210 EXCEPT      
!      PARENTHESES ARE USED TO GROUP THE CONSTANTS.                      
!                                                                        
 2111 continue                                                          
      ivtnum = 211                                                      
!                                                                        
!       ****  TEST 211  ****                                             
!                                                                        
      if (iczero) 32110, 2110, 32110                                    
 2110 continue                                                          
      ivcomp = (2+3)+4                                                  
      goto 42110                                                       
32110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42110, 2121, 42110                                    
42110 if (ivcomp -9) 22110,12110,22110                                  
12110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2121                                                        
22110 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2121 continue                                                          
      ivtnum = 212                                                      
!                                                                        
!       ****  TEST 212  ****                                             
!                                                                        
      if (iczero) 32120, 2120, 32120                                    
 2120 continue                                                          
      ivcomp = 51+(52+53)                                               
      goto 42120                                                       
32120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42120, 2131, 42120                                    
42120 if (ivcomp - 156) 22120,12120,22120                               
12120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2131                                                        
22120 ivfail = ivfail + 1                                               
      ivcorr = 156                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2131 continue                                                          
      ivtnum = 213                                                      
!                                                                        
!       ****  TEST 213  ****                                             
!                                                                        
      if (iczero) 32130, 2130, 32130                                    
 2130 continue                                                          
      ivcomp = 189 +(676+101)                                           
      goto 42130                                                       
32130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42130, 2141, 42130                                    
42130 if (ivcomp - 966) 22130,12130,22130                               
12130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2141                                                        
22130 ivfail = ivfail + 1                                               
      ivcorr = 966                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2141 continue                                                          
      ivtnum = 214                                                      
!                                                                        
!       ****  TEST 214  ****                                             
!                                                                        
      if (iczero) 32140, 2140, 32140                                    
 2140 continue                                                          
      ivcomp = (1358+2189) + 8001                                       
      goto 42140                                                       
32140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42140, 2151, 42140                                    
42140 if (ivcomp - 11548) 22140,12140,22140                             
12140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2151                                                        
22140 ivfail = ivfail + 1                                               
      ivcorr = 11548                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 215 THROUGH TEST 234 CONTAIN INTEGER VARIABLES, INTEGER      
!      CONSTANTS AND THE OPERATOR + IN ARITHMETIC EXPRESSION.            
!                                                                        
!      TEST 215 THROUGH TEST 219 - ONE INTEGER VARIABLE AND ONE INTEGER  
!      CONSTANT IN ARITHMETIC EXPRESSION.                                
!                                                                        
 2151 continue                                                          
      ivtnum = 215                                                      
!                                                                        
!       ****  TEST 215  ****                                             
!                                                                        
      if (iczero) 32150, 2150, 32150                                    
 2150 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 + 3                                               
      goto 42150                                                       
32150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42150, 2161, 42150                                    
42150 if (ivcomp - 5) 22150,12150,22150                                 
12150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2161                                                        
22150 ivfail = ivfail + 1                                               
      ivcorr=5                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2161 continue                                                          
      ivtnum = 216                                                      
!                                                                        
!       ****  TEST 216  ****                                             
!                                                                        
      if (iczero) 32160, 2160, 32160                                    
 2160 continue                                                          
      ivon01 = 3                                                        
      ivcomp = 2 + ivon01                                               
      goto 42160                                                       
32160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42160, 2171, 42160                                    
42160 if (ivcomp - 5) 22160,12160,22160                                 
12160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2171                                                        
22160 ivfail = ivfail + 1                                               
      ivcorr = 5                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2171 continue                                                          
      ivtnum = 217                                                      
!                                                                        
!       ****  TEST 217  ****                                             
!                                                                        
      if (iczero) 32170, 2170, 32170                                    
 2170 continue                                                          
      ivon01 = 51                                                       
      ivcomp = ivon01 +52                                               
      goto 42170                                                       
32170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42170, 2181, 42170                                    
42170 if (ivcomp - 103) 22170,12170,22170                               
12170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2181                                                        
22170 ivfail = ivfail + 1                                               
      ivcorr = 103                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2181 continue                                                          
      ivtnum = 218                                                      
!                                                                        
!       ****  TEST 218  ****                                             
!                                                                        
      if (iczero) 32180, 2180, 32180                                    
 2180 continue                                                          
      ivon01 = 676                                                      
      ivcomp = 189 + ivon01                                             
      goto 42180                                                       
32180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42180, 2191, 42180                                    
42180 if (ivcomp - 865) 22180,12180,22180                               
12180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2191                                                        
22180 ivfail = ivfail + 1                                               
      ivcorr = 865                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2191 continue                                                          
      ivtnum = 219                                                      
!                                                                        
!       ****  TEST 219  ****                                             
!                                                                        
      if (iczero) 32190, 2190, 32190                                    
 2190 continue                                                          
      ivon01 = 1358                                                     
      ivcomp = ivon01 + 8001                                            
      goto 42190                                                       
32190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42190, 2201, 42190                                    
42190 if (ivcomp - 9359) 22190,12190,22190                              
12190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2201                                                        
22190 ivfail = ivfail + 1                                               
      ivcorr = 9359                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 220 THROUGH TEST 224 - ONE INTEGER VARIABLE, TWO INTEGER     
!      CONSTANTS IN ARITHMETIC EXPRESSION.                               
!                                                                        
 2201 continue                                                          
      ivtnum = 220                                                      
!                                                                        
!       ****  TEST 220  ****                                             
!                                                                        
      if (iczero) 32200, 2200, 32200                                    
 2200 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 +3 +4                                             
      goto 42200                                                       
32200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42200, 2211, 42200                                    
42200 if (ivcomp - 9) 22200,12200,22200                                 
12200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2211                                                        
22200 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2211 continue                                                          
      ivtnum = 221                                                      
!                                                                        
!       ****  TEST 221  ****                                             
!                                                                        
      if (iczero) 32210, 2210, 32210                                    
 2210 continue                                                          
      ivon01 = 3                                                        
      ivcomp = 2+ivon01+4                                               
      goto 42210                                                       
32210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42210, 2221, 42210                                    
42210 if (ivcomp - 9) 22210,12210,22210                                 
12210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2221                                                        
22210 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2221 continue                                                          
      ivtnum = 222                                                      
!                                                                        
!       ****  TEST 222  ****                                             
!                                                                        
      if (iczero) 32220, 2220, 32220                                    
 2220 continue                                                          
      ivon01 = 4                                                        
      ivcomp= 2+3+ivon01                                                
      goto 42220                                                       
32220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42220, 2231, 42220                                    
42220 if (ivcomp - 9) 22220,12220,22220                                 
12220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2231                                                        
22220 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2231 continue                                                          
      ivtnum = 223                                                      
!                                                                        
!       ****  TEST 223  ****                                             
!                                                                        
      if (iczero) 32230, 2230, 32230                                    
 2230 continue                                                          
      ivon01 = 2189                                                     
      ivcomp = 1358+ivon01+8001                                         
      goto 42230                                                       
32230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42230, 2241, 42230                                    
42230 if (ivcomp - 11548) 22230,12230,22230                             
12230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2241                                                        
22230 ivfail = ivfail + 1                                               
      ivcorr=11548                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2241 continue                                                          
      ivtnum = 224                                                      
!                                                                        
!       ****  TEST 224  ****                                             
!                                                                        
      if (iczero) 32240, 2240, 32240                                    
 2240 continue                                                          
      ivon01 = 11111                                                    
      ivcomp = 11111 + ivon01 + 10111                                   
      goto 42240                                                       
32240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42240, 2251, 42240                                    
42240 if (ivcomp - 32333) 22240,12240,22240                             
12240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2251                                                        
22240 ivfail = ivfail + 1                                               
      ivcorr = 32333                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!      TEST 225 THROUGH TEST 234 USE PARENTHESES TO GROUP ELEMENTS IN    
!      AN ARITHMETIC EXPRESSION. THE RESULTS ARE THE SAME AS TESTS       
!      220 THROUGH 224.                                                  
!                                                                        
 2251 continue                                                          
      ivtnum = 225                                                      
!                                                                        
!       ****  TEST 225  ****                                             
!                                                                        
      if (iczero) 32250, 2250, 32250                                    
 2250 continue                                                          
       ivon01 = 2                                                       
      ivcomp = (ivon01 +3) + 4                                          
      goto 42250                                                       
32250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42250, 2261, 42250                                    
42250 if (ivcomp -9) 22250,12250,22250                                  
12250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2261                                                        
22250 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2261 continue                                                          
      ivtnum = 226                                                      
!                                                                        
!       ****  TEST 226  ****                                             
!                                                                        
      if (iczero) 32260, 2260, 32260                                    
 2260 continue                                                          
      ivon01 = 2                                                        
      ivcomp = ivon01 + (3+4)                                           
      goto 42260                                                       
32260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42260, 2271, 42260                                    
42260 if (ivcomp - 9) 22260,12260,22260                                 
12260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2271                                                        
22260 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2271 continue                                                          
      ivtnum = 227                                                      
!                                                                        
!       ****  TEST 227  ****                                             
!                                                                        
      if (iczero) 32270, 2270, 32270                                    
 2270 continue                                                          
      ivon01 = 3                                                        
      ivcomp = (2+ivon01) + 4                                           
      goto 42270                                                       
32270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42270, 2281, 42270                                    
42270 if (ivcomp - 9) 22270,12270,22270                                 
12270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2281                                                        
22270 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2281 continue                                                          
      ivtnum = 228                                                      
!                                                                        
!       ****  TEST 228  ****                                             
!                                                                        
      if (iczero) 32280, 2280, 32280                                    
 2280 continue                                                          
      ivon01 = 3                                                        
      ivcomp = 2 +(ivon01+4)                                            
      goto 42280                                                       
32280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42280, 2291, 42280                                    
42280 if (ivcomp - 9) 22280, 12280, 22280                               
12280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2291                                                        
22280 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2291 continue                                                          
      ivtnum = 229                                                      
!                                                                        
!       ****  TEST 229  ****                                             
!                                                                        
      if (iczero) 32290, 2290, 32290                                    
 2290 continue                                                          
      ivon01 = 4                                                        
      ivcomp = (2+3)+ivon01                                             
      goto 42290                                                       
32290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42290, 2301, 42290                                    
42290 if (ivcomp - 9) 22290,12290,22290                                 
12290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2301                                                        
22290 ivfail = ivfail + 1                                               
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2301 continue                                                          
      ivtnum = 230                                                      
!                                                                        
!       ****  TEST 230  ****                                             
!                                                                        
      if (iczero) 32300, 2300, 32300                                    
 2300 continue                                                          
      ivon01 = 2189                                                     
      ivcomp = 1358 + (ivon01+8001)                                     
      goto 42300                                                       
32300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42300, 2311, 42300                                    
42300 if (ivcomp - 11548) 22300,12300,22300                             
12300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2311                                                        
22300 ivfail = ivfail + 1                                               
      ivcorr = 11548                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2311 continue                                                          
      ivtnum = 231                                                      
!                                                                        
!       ****  TEST 231  ****                                             
!                                                                        
      if (iczero) 32310, 2310, 32310                                    
 2310 continue                                                          
      ivon01 = 2189                                                     
      ivcomp = (1358+ivon01) + 8001                                     
      goto 42310                                                       
32310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42310, 2321, 42310                                    
42310 if (ivcomp - 11548) 22310,12310,22310                             
12310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2321                                                        
22310 ivfail = ivfail + 1                                               
      ivcorr = 11548                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2321 continue                                                          
      ivtnum = 232                                                      
!                                                                        
!       ****  TEST 232  ****                                             
!                                                                        
      if (iczero) 32320, 2320, 32320                                    
 2320 continue                                                          
      ivon01 = 11111                                                    
      ivcomp = (11111 + ivon01) + 10111                                 
      goto 42320                                                       
32320 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42320, 2331, 42320                                    
42320 if (ivcomp - 32333) 22320,12320,22320                             
12320 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2331                                                        
22320 ivfail = ivfail + 1                                               
      ivcorr = 32333                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2331 continue                                                          
      ivtnum = 233                                                      
!                                                                        
!       ****  TEST 233  ****                                             
!                                                                        
      if (iczero) 32330, 2330, 32330                                    
 2330 continue                                                          
      ivon01 = 11111                                                    
      ivcomp = (ivon01 + 10111) + 11111                                 
      goto 42330                                                       
32330 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42330, 2341, 42330                                    
42330 if (ivcomp - 32333) 22330,12330,22330                             
12330 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2341                                                        
22330 ivfail = ivfail + 1                                               
      ivcorr = 32333                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2341 continue                                                          
      ivtnum = 234                                                      
!                                                                        
!       ****  TEST 234  ****                                             
!                                                                        
      if (iczero) 32340, 2340, 32340                                    
 2340 continue                                                          
      ivon01 = 10111                                                    
      ivcomp = 11111 + (11111+ivon01)                                   
      goto 42340                                                       
32340 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 42340, 2351, 42340                                    
42340 if (ivcomp - 32333) 22340,12340,22340                             
12340 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 2351                                                        
22340 ivfail = ivfail + 1                                               
      ivcorr = 32333                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 2351 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM008" )                          
      end program fm008
