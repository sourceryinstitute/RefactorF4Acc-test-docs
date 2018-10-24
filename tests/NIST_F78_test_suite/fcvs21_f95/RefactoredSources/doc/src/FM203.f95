      program fm203
!                                                                        
!                                                                        
!          THIS ROUTINE CONTINUES THE TESTING OF CHARACTER DATA TYPES    
!      WHICH WAS STARTED IN FM202.  THE CHARACTER TYPE-STATEMENTS SPECIFY
!      CHARACTER VARIABLES AND ONE-DIMENSIONAL CHARACTER ARRAYS OF       
!      LENGTH ONE AND LENGTH TWO.  THE TESTS IN THIS ROUTINE DETERMINE   
!      THAT THE FOLLOWING LANGUAGE FEATURES FUNCTION CORRECTLY.          
!                                                                        
!          (1)  CHARACTER ASSIGNMENT STATEMENTS OF THE FORM              
!                                                                        
!          CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                  
!          CHARACTER ARRAY ELEMENT = CHARACTER VARIABLE                  
!          CHARACTER ARRAY ELEMENT = CHARACTER ARRAY ELEMENT             
!          CHARACTER VARIABLE = CHARACTER ARRAY ELEMENT                  
!                                                                        
!      WHERE THE ARRAY ELEMENTS, VARIABLES AND CONSTANTS ARE OF LENGTH   
!      ONE OR TWO.                                                       
!                                                                        
!          (2)  CHARACTER RELATIONAL EXPRESSIONS OF THE FORM             
!                                                                        
!          CHARACTER ARRAY ELEMENT RELOP CHARACTER CONSTANT              
!          CHARACTER ARRAY ELEMENT RELOP CHARACTER VARIABLE              
!          CHARACTER ARRAY ELEMENT RELOP CHARACTER ARRAY ELEMENT         
!                                                                        
!      WHERE THE ARRAY ELEMENTS, VARIABLES AND CONSTANTS ARE OF LENGTH   
!      ONE OR TWO.                                                       
!                                                                        
!          (3)  CHARACTER EXPRESSIONS ENCLOSED IN PARENTHESES.  THE FORMS
!      TESTED ARE                                                        
!                                                                        
!          (CHARACTER CONSTANT)                                          
!          (CHARACTER VARIABLE)                                          
!          (CHARACTER ARRAY ELEMENT)                                     
!          ((CHARACTER ARRAY ELEMENT))                                   
!                                                                        
!          (4)  CHARACTER RELATIONAL EXPRESSIONS OF THE FORM             
!                                                                        
!          CHARACTER ARRAY ELEMENT .EQ. CHARACTER CONSTANT               
!                                                                        
!      ARE USED IN THIS ROUTINE TO VERIFY THE CHARACTER ASSIGNMENT       
!      STATEMENTS.                                                       
!                                                                        
!      REFERENCES                                                        
!          AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,      
!               X3.9-1978                                                
!                                                                        
!          SECTION 4.8,    CHARACTER TYPE                                
!          SECTION 4.8.1,  CHARACTER CONSTANT                            
!          SECTION 6.2,    CHARACTER EXPRESSIONS                         
!          SECTION 6.3.4,  CHARACTER RELATIONAL EXPRESSION               
!          SECTION 6.3.5,  INTERPRETATION OF CHARACTER RELATIONAL        
!                            EXPRESSIONS                                 
!          SECTION 8.4.2,  CHARACTER TYPE-STATEMENT                      
!          SECTION 10.4,   CHARACTER ASSIGNMENT STATEMENT                
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
      integer :: i
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      character, dimension(1:5) :: catn11
      character :: cvtn01
      character, dimension(1:5) :: catn12
      character :: cvtn02
      character(len=2), dimension(1:5) :: catn13
      character(len=2) :: cvtn03
      character(len=2), dimension(1:5) :: catn14
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
!          TEST 31 THROUGH TEST 33 VERIFY THAT THE CHARACTER ASSIGNMENT  
!      STATEMENT                                                         
!                                                                        
!          CHARACTER ARRAY ELEMENT (LEN 1) = CHARACTER CONSTANT (LEN 1)  
!                                                                        
!      IS CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                  
!                                                                        
!        CHARACTER ARRAY ELEMENT (LEN 1) .EQ. CHARACTER CONSTANT (LEN 1) 
!                                                                        
!      IS USED TO VERIFY THE ASSIGNMENT STATEMENT.  BOTH OF THE ABOVE    
!      STATEMENT FORMS MUST MEET THE LANGUAGE SPECIFICATIONS FOR THESE   
!      TESTS TO PASS.                                                    
!                                                                        
!          THE TWO ARRAYS USED IN THESE TESTS ARE CATN11(5) AND CATN12(5)
!      THE ARRAYS ARE INITIALIZED TO A BLANK CHARACTER BY THE DO-LOOP    
!                                                                        
      do i= 1,5                                                     
      catn11(i) = ' '                                                   
      catn12(i) = ' '                                                   
   end do
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 031  ****                         
!                                                                        
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn11(2) = 'V'                                                   
      if (catn11(2)  ==  'V') ivcomp = 1                                
40310 if (ivcomp - 1) 20310,10310,20310                                 
30310 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10310, 0321, 20310                                    
10310 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0321                                                        
20310 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0321 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 032  ****                         
!                                                                        
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      ivcomp=0                                                          
      ivcorr=1                                                          
      catn11(3) = '+'                                                   
      if (catn11(3)  ==  '+') ivcomp = 1                                
40320 if (ivcomp - 1) 20320,10320,20320                                 
30320 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10320, 0331, 20320                                    
10320 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0331                                                        
20320 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0331 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 033  ****                         
!                                                                        
!                                                                        
      ivtnum =  33                                                      
      if (iczero) 30330, 0330, 30330                                    
 0330 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn11 (4) = '7'                                                  
      if (catn11 (4)  ==  '7') ivcomp = 1                               
40330 if (ivcomp -1) 20330,10330,20330                                  
30330 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10330, 0341, 20330                                    
10330 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0341                                                        
20330 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0341 continue                                                          
!                                                                        
!          TEST 34 THROUGH TEST 36 VERIFY THAT THE CHARACTER ASSIGNMENT  
!      STATEMENTS                                                        
!                                                                        
!          CHARACTER VARIABLE (LEN 1) = CHARACTER CONSTANT (LEN 1)       
!          CHARACTER ARRAY ELEMENT (LEN1) = CHARACTER VARIABLE (LEN1)    
!                                                                        
!      ARE CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                 
!                                                                        
!          CHARACTER ARRAY ELEMENT (LEN1) .EQ. CHAR. CONSTANT (LEN1)     
!                                                                        
!      IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENTS.        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 034  ****                         
!                                                                        
!                                                                        
      ivtnum =  34                                                      
      if (iczero) 30340, 0340, 30340                                    
 0340 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = 'V'                                                      
      catn12(2) = cvtn01                                                
      if (catn12(2)  ==  'V') ivcomp = 1                                
40340 if (ivcomp - 1) 20340,10340,20340                                 
30340 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10340, 0351, 20340                                    
10340 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0351                                                        
20340 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0351 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 035  ****                         
!                                                                        
!                                                                        
      ivtnum =  35                                                      
      if (iczero) 30350, 0350, 30350                                    
 0350 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '+'                                                      
      catn12(3) = cvtn01                                                
      if (catn12(3)  ==  '+') ivcomp = 1                                
40350 if (ivcomp - 1) 20350,10350,20350                                 
30350 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10350, 0361, 20350                                    
10350 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0361                                                        
20350 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0361 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 036  ****                         
!                                                                        
!                                                                        
      ivtnum =  36                                                      
      if (iczero) 30360, 0360, 30360                                    
 0360 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '7'                                                      
      catn12(4) = cvtn01                                                
      if (catn12(4)  ==  '7') ivcomp = 1                                
40360 if (ivcomp - 1) 20360,10360,20360                                 
30360 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10360, 0371, 20360                                    
10360 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0371                                                        
20360 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0371 continue                                                          
!                                                                        
!          TEST 37 THROUGH TEST 39 VERIFY THAT THE CHARACTER ASSIGNMENT  
!      STATEMENTS                                                        
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 1) = CHAR. CONSTANT (LEN 1)          
!          CHAR. ARRAY ELEMENT (LEN 1) = CHAR. ARRAY ELEMENT (LEN 1)     
!                                                                        
!      ARE CORRECT.  THE CHARACTER RELATIONAL EXPRESSION                 
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 1) .EQ. CHAR. CONSTANT (LEN 1)       
!                                                                        
!      IS USED TO VERIFY THE RESULT OF THE ASSIGNMENT STATEMENTS.        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 037  ****                         
!                                                                        
!                                                                        
      ivtnum =  37                                                      
      if (iczero) 30370, 0370, 30370                                    
 0370 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 6                                                        
      catn11 (1) = 'V'                                                  
      catn12 (1) = catn11 (1)                                           
      if (catn12(1)  ==  'V') ivcomp=ivcomp*2                           
      if (catn11(1)  ==  'V') ivcomp=ivcomp*3                           
40370 if (ivcomp-6) 20370,10370,20370                                   
30370 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10370, 0381, 20370                                    
10370 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0381                                                        
20370 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0381 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 038  ****                         
!                                                                        
!                                                                        
      ivtnum =  38                                                      
      if (iczero) 30380, 0380, 30380                                    
 0380 continue                                                          
      ivcomp=1                                                          
      ivcorr=6                                                          
      catn11(2) = '+'                                                   
      catn12(2) = catn11(2)                                             
      if (catn12(2)  ==  '+') ivcomp=ivcomp*2                           
      if (catn11(2)  ==  '+') ivcomp=ivcomp*3                           
40380 if (ivcomp - 6) 20380,10380,20380                                 
30380 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10380, 0391, 20380                                    
10380 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0391                                                        
20380 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0391 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 039  ****                         
!                                                                        
!                                                                        
      ivtnum =  39                                                      
      if (iczero) 30390, 0390, 30390                                    
 0390 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 6                                                        
      catn11 (3) = '7'                                                  
      catn12 (3) = catn11 (3)                                           
      if (catn12(3)  ==  '7') ivcomp = ivcomp * 2                       
      if (catn11(3)  ==  '7') ivcomp = ivcomp * 3                       
40390 if (ivcomp - 6) 20390,10390,20390                                 
30390 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10390, 0401, 20390                                    
10390 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0401                                                        
20390 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0401 continue                                                          
!                                                                        
!          TEST 40 AND TEST 41 VERIFY THAT THE CHARACTER ASSIGNMENT      
!      STATEMENTS                                                        
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 1) = CHAR. CONSTANT (LEN 1)          
!          CHAR. VARIABLE (LEN 1) = CHAR. ARRAY ELEMENT (LEN 1)          
!                                                                        
!      ARE CORRECT.                                                      
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 040  ****                         
!                                                                        
!                                                                        
      ivtnum =  40                                                      
      if (iczero) 30400, 0400, 30400                                    
 0400 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn11(4) = 'X'                                                   
      cvtn02 = catn11 (4)                                               
      if (cvtn02  ==  'X') ivcomp = 1                                   
40400 if (ivcomp - 1) 20400,10400,20400                                 
30400 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10400, 0411, 20400                                    
10400 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0411                                                        
20400 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0411 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 041  ****                         
!                                                                        
!                                                                        
      ivtnum =  41                                                      
      if (iczero) 30410, 0410, 30410                                    
 0410 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn11(3) = '-'                                                   
      cvtn02 = catn11(3)                                                
      if (cvtn02  ==  '-') ivcomp=1                                     
40410 if (ivcomp - 1) 20410,10410,20410                                 
30410 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10410, 0421, 20410                                    
10410 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0421                                                        
20410 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0421 continue                                                          
!                                                                        
!          TEST 42 THROUGH TEST 44 VERIFY THE RESULTS OF CHARACTER       
!      RELATIONAL EXPRESSIONS USING EACH OF THE SIX RELATIONAL OPERATORS.
!      THE CHARACTER DATA 'A' AND '1' ARE COMPARED IN THE EXPRESSION     
!      AND ARE INITIALIZED BY THE CHARACTER ASSIGNMENT STATEMENTS        
!                                                                        
      catn11 (4) = 'A'                                                  
      catn12 (3) = '1'                                                  
      cvtn01 = 'A'                                                      
      cvtn02 = '1'                                                      
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 042  ****                         
!                                                                        
!          RELATIONAL OPERATORS .NE. AND .EQ.                            
!          CHAR. ARRAY ELEMENT (LEN 1) RELOP CHAR. CONSTANT (LEN 1)      
!                                                                        
      ivtnum =  42                                                      
      if (iczero) 30420, 0420, 30420                                    
 0420 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 3                                                        
      if (catn11(4)  ==  '1') ivcomp=ivcomp*2                           
      if ('A'  /=  catn12(3)) ivcomp=ivcomp*3                           
40420 if (ivcomp - 3) 20420,10420,20420                                 
30420 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10420, 0431, 20420                                    
10420 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0431                                                        
20420 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0431 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 043  ****                         
!                                                                        
!          RELATIONAL OPERATORS .LE. AND .GE.                            
!          CHAR. ARRAY ELEMENT (LEN 1) RELOP CHAR. VARIABLE (LEN 1)      
!                                                                        
      ivtnum =  43                                                      
      if (iczero) 30430, 0430, 30430                                    
 0430 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(4)  <=  cvtn02) ivcomp=ivcomp+1                        
      if (cvtn01  >=  catn12(3)) ivcomp=ivcomp+1                        
40430 if (ivcomp - 1) 20430,10430,20430                                 
30430 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10430, 0441, 20430                                    
10430 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0441                                                        
20430 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0441 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 044  ****                         
!                                                                        
!          RELATIONAL OPERATORS .LT. AND .GT.                            
!          CHAR. ARRAY ELEMENT (LEN 1) RELOP CHAR. ARRAY ELEMENT (LEN 1) 
!                                                                        
      ivtnum =  44                                                      
      if (iczero) 30440, 0440, 30440                                    
 0440 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(4)  <  catn12(3)) ivcomp=ivcomp+1                     
      if (catn11(4)  >  catn12(3)) ivcomp=ivcomp+1                     
40440 if (ivcomp - 1) 20440,10440,20440                                 
30440 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10440, 0451, 20440                                    
10440 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0451                                                        
20440 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0451 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 045  ****                         
!                                                                        
!          TEST 45 VERIFIES THAT THE LAST ELEMENTS OF THE ARRAYS USED    
!      IN TEST 31 THROUGH TEST 44 WERE NOT AFFECTED BY THE SETTING       
!      OF OTHER CHARACTER ARRAY ELEMENTS.                                
!                                                                        
      ivtnum =  45                                                      
      if (iczero) 30450, 0450, 30450                                    
 0450 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 30                                                       
      if (catn11(5)  ==  ' ') ivcomp=ivcomp*2                           
      if (catn12(5)  ==  ' ') ivcomp=ivcomp*3                           
      if (catn11(5)  ==  catn12(5)) ivcomp=ivcomp*5                     
40450 if (ivcomp - 30) 20450,10450,20450                                
30450 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10450, 0461, 20450                                    
10450 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0461                                                        
20450 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0461 continue                                                          
!                                                                        
!          TEST 46 THROUGH TEST 49 CONTAIN CHARACTER ARRAY ELEMENTS OF   
!      LENGTH TWO IN CHARACTER ASSIGNMENT STATEMENTS.  THE CHARACTER     
!      RELATIONAL EXPRESSION                                             
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 2) .EQ. CHAR. CONSTANT (LEN 2)       
!                                                                        
!      IS USED TO VERIFY THE TEST RESULTS.                               
!                                                                        
!          THE TWO ARRAYS USED IN THESE TESTS ARE CATN13(5) AND CATN14(5)
!      THE ARRAYS ARE INITIALIZED TO TWO BLANK CHARACTERS BY THE DO-LOOP 
!                                                                        
      do i=1,5                                                      
      catn13(i) = '  '                                                  
      catn14(i) = '  '                                                  
   end do
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 046  ****                         
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 2) = CHAR. CONSTANT (LEN 2)          
!                                                                        
      ivtnum =  46                                                      
      if (iczero) 30460, 0460, 30460                                    
 0460 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn13(1) = 'AB'                                                  
      if (catn13(1)  ==  'AB') ivcomp = 1                               
40460 if (ivcomp - 1) 20460,10460,20460                                 
30460 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10460, 0471, 20460                                    
10460 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0471                                                        
20460 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0471 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 047  ****                         
!                                                                        
!          CHAR. VARIABLE (LEN 2) = CHAR. CONSTANT (LEN 2)               
!          CHAR. ARRAY ELEMENT (LEN 2) = CHAR. VARIABLE (LEN 2)          
!                                                                        
      ivtnum =  47                                                      
      if (iczero) 30470, 0470, 30470                                    
 0470 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = '+-'                                                     
      catn13(2) = cvtn03                                                
      if (catn13(2)  ==  '+-') ivcomp=1                                 
40470 if (ivcomp - 1) 20470,10470,20470                                 
30470 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10470, 0481, 20470                                    
10470 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0481                                                        
20470 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0481 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 048  ****                         
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 2) = CHAR. CONSTANT (LEN 2)          
!          CHAR. ARRAY ELEMENT (LEN 2) = CHAR. ARRAY ELEMENT (LEN 2)     
!                                                                        
      ivtnum =  48                                                      
      if (iczero) 30480, 0480, 30480                                    
 0480 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn13(4) = '24'                                                  
      catn13(3) = catn13(4)                                             
      if (catn13(3)  ==  '24') ivcomp = 1                               
40480 if (ivcomp - 1) 20480,10480,20480                                 
30480 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10480, 0491, 20480                                    
10480 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0491                                                        
20480 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0491 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 049  ****                         
!                                                                        
!          CHAR. ARRAY ELEMENT (LEN 2) = CHAR. CONSTANT (LEN 2)          
!          CHAR. VARIABLE (LEN 2) = CHAR. ARRAY ELEMENT (LEN 2)          
!                                                                        
      ivtnum =  49                                                      
      if (iczero) 30490, 0490, 30490                                    
 0490 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn14(1) = 'AB'                                                  
      cvtn04 = catn14(1)                                                
      if (cvtn04  ==  'AB') ivcomp = 1                                  
40490 if (ivcomp - 1) 20490,10490,20490                                 
30490 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10490, 0501, 20490                                    
10490 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0501                                                        
20490 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0501 continue                                                          
!                                                                        
!          TEST 50 THROUGH TEST 52 VERIFY THE RESULTS OF CHARACTER       
!      RELATIONAL EXPRESSIONS USING EACH OF THE SIX RELATIONAL OPERATORS.
!      THE CHARACTER DATA 'ZA' AND 'Z1' ARE COMPARED IN THE EXPRESSION   
!      AND ARE INITIALIZED BY THE CHARACTER ASSIGNMENT STATEMENTS        
!                                                                        
      catn14(2) = 'ZA'                                                  
      catn14(3) = 'Z1'                                                  
      cvtn03 = 'ZA'                                                     
      cvtn04 = 'Z1'                                                     
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 050  ****                         
!                                                                        
!          RELATIONAL OPERATORS .NE. AND .EQ.                            
!          CHAR. ARRAY ELEMENT (LEN 2) RELOP CHAR. VARIABLE (LEN 2)      
!                                                                        
      ivtnum =  50                                                      
      if (iczero) 30500, 0500, 30500                                    
 0500 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 3                                                        
      if (catn14(2)  ==  'Z1') ivcomp=ivcomp*2                          
      if ('ZA'  /=  catn14(3)) ivcomp=ivcomp*3                          
40500 if (ivcomp - 3) 20500,10500,20500                                 
30500 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10500, 0511, 20500                                    
10500 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0511                                                        
20500 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0511 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 051  ****                         
!                                                                        
!          RELATIONAL OPERATORS .LE. AND .GE.                            
!          CHAR. ARRAY ELEMENT (LEN 2) RELOP CHAR. VARIABLE (LEN 2)      
!                                                                        
      ivtnum =  51                                                      
      if (iczero) 30510, 0510, 30510                                    
 0510 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn14(2)  <=  cvtn04) ivcomp=ivcomp+1                        
      if (cvtn03  >=  catn14(3)) ivcomp=ivcomp+1                        
40510 if (ivcomp - 1) 20510,10510,20510                                 
30510 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10510, 0521, 20510                                    
10510 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0521                                                        
20510 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0521 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 052  ****                         
!                                                                        
!          RELATIONAL OPERATORS .LT. AND .GT.                            
!          CHAR. ARRAY ELEMENT (LEN 2) RELOP CHAR. ARRAY ELEMENT (LEN 2) 
!                                                                        
      ivtnum =  52                                                      
      if (iczero) 30520, 0520, 30520                                    
 0520 continue                                                          
      ivcomp =0                                                         
      ivcorr =1                                                         
      if (catn14(2)  <  catn14(3)) ivcomp=ivcomp+1                     
      if (catn14(2)  >  catn14(3)) ivcomp=ivcomp+1                     
40520 if (ivcomp - 1) 20520,10520,20520                                 
30520 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10520, 0531, 20520                                    
10520 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0531                                                        
20520 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0531 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 053  ****                         
!                                                                        
!          TEST 53 VERIFIES THAT THE LAST ELEMENTS OF THE ARRAYS USED IN 
!      TEST 46 THROUGH TEST 52 WERE NOT AFFECTED BY THE SETTING OF OTHER 
!      CHARACTER ARRAY ELEMENTS.                                         
!                                                                        
      ivtnum =  53                                                      
      if (iczero) 30530, 0530, 30530                                    
 0530 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 30                                                       
      if (catn13(5)  ==  '  ')ivcomp=ivcomp*2                           
      if (catn14(5)  ==  '  ') ivcomp= ivcomp * 3                       
      if (catn14(5)  ==  catn13(5)) ivcomp=ivcomp*5                     
40530 if (ivcomp - 30) 20530,10530,20530                                
30530 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10530, 0541, 20530                                    
10530 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0541                                                        
20530 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0541 continue                                                          
!                                                                        
!          TEST 54 THROUGH TEST 60 VERIFY THAT A CHARACTER PRIMARY CAN   
!      BE ENCLOSED IN PARENTHESES.  THE CHARACTER PRIMARIES FOR THE      
!      SUBSET ARE CHARACTER CONSTANT, CHARACTER VARIABLE, CHARACTER ARRAY
!      ELEMENT, AND CHARACTER EXPRESSION ENCLOSED IN PARENTHESES.  THE   
!      FORM OF A CHARACTER EXPRESSION IS CHARACTER PRIMARY.              
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 054  ****                         
!                                                                        
!          CHARACTER ASSIGNMENT STATEMENT                                
!          CHAR. VARIABLE = (CHARACTER CONSTANT)   LENGTH 1              
!                                                                        
      ivtnum =  54                                                      
      if (iczero) 30540, 0540, 30540                                    
 0540 continue                                                          
      cvtn01 = ' '                                                      
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = ('N')                                                    
      if (cvtn01  ==  'N') ivcomp = 1                                   
40540 if (ivcomp - 1) 20540,10540,20540                                 
30540 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10540, 0551, 20540                                    
10540 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0551                                                        
20540 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0551 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 055  ****                         
!                                                                        
!          CHARACTER ASSIGNMENT STATEMENT                                
!          CHAR. VARIABLE = (CHAR. VARIABLE)   LENGTH 2                  
!                                                                        
      ivtnum =  55                                                      
      if (iczero) 30550, 0550, 30550                                    
 0550 continue                                                          
      cvtn04 = '  '                                                     
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = '/+'                                                     
      cvtn04 = (cvtn03)                                                 
      if (cvtn04  ==  '/+') ivcomp=1                                    
40550 if (ivcomp - 1) 20550,10550,20550                                 
30550 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10550, 0561, 20550                                    
10550 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0561                                                        
20550 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0561 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 056  ****                         
!                                                                        
!          CHARACTER ASSIGNMENT STATEMENT                                
!          CHAR. VARIABLE = (CHAR. ARRAY ELEMENT)   LENGTH 2             
!                                                                        
      ivtnum =  56                                                      
      if (iczero) 30560, 0560, 30560                                    
 0560 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn04 = '  '                                                     
      catn13(1) = 'BC'                                                  
      cvtn04 = (catn13(1))                                              
      if (cvtn04  ==  'BC') ivcomp = 1                                  
40560 if (ivcomp - 1) 20560,10560,20560                                 
30560 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10560, 0571, 20560                                    
10560 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0571                                                        
20560 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0571 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 057  ****                         
!                                                                        
!          CHARACTER ASSIGNMENT STATEMENT                                
!          CHAR. VARIABLE = ((CHAR. ARRAY ELEMENT))  LENGTH 2            
!                                                                        
      ivtnum =  57                                                      
      if (iczero) 30570, 0570, 30570                                    
 0570 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn04 = '  '                                                     
      catn13(3) = 'BC'                                                  
      cvtn04 = ((catn13(3)))                                            
      if (cvtn04  ==  'BC') ivcomp=1                                    
40570 if (ivcomp - 1) 20570,10570,20570                                 
30570 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10570, 0581, 20570                                    
10570 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0581                                                        
20570 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0581 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 058  ****                         
!                                                                        
!          RELATIONAL EXPRESSION, .NE.                                   
!          (CHAR. CONSTANT) .NE. (CHAR. VARIABLE)   LENGTH 1             
!                                                                        
      ivtnum =  58                                                      
      if (iczero) 30580, 0580, 30580                                    
 0580 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = '6'                                                      
      if (('9')  /=  (cvtn01)) ivcomp=1                                 
40580 if (ivcomp - 1) 20580,10580,20580                                 
30580 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10580, 0591, 20580                                    
10580 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0591                                                        
20580 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0591 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 059  ****                         
!                                                                        
!          RELATIONAL EXPRESSION, .GE.                                   
!          (CHAR. VARIABLE) .GE. (CHAR. ARRAY ELEMENT)  LENGTH 2         
!                                                                        
      ivtnum =  59                                                      
      if (iczero) 30590, 0590, 30590                                    
 0590 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = 'DE'                                                     
      catn13(5) = 'DE'                                                  
      if ((cvtn03)  >=  (catn13(5))) ivcomp=1                           
40590 if (ivcomp - 1) 20590,10590,20590                                 
30590 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10590, 0601, 20590                                    
10590 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0601                                                        
20590 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0601 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 203  -  TEST 060  ****                         
!                                                                        
!          RELATIONAL EXPRESSION, .LE.                                   
!          ((CHAR. ARRAY ELEMENT)) .LE. ((CHAR. ARRAY ELEMENT))  LEN 2   
!                                                                        
      ivtnum =  60                                                      
      if (iczero) 30600, 0600, 30600                                    
 0600 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn13(4) = 'MC'                                                  
      catn13(5) = 'MC'                                                  
      if (((catn13(4)))  <=  ((catn13(5)))) ivcomp = 1                  
40600 if (ivcomp - 1) 20600,10600,20600                                 
30600 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10600, 0611, 20600                                    
10600 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0611                                                        
20600 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0611 continue                                                          
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
90001 format (" ",24x,"FM203")                                          
90000 format (" ",20x,"END OF PROGRAM FM203" )                          
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
      end program fm203
