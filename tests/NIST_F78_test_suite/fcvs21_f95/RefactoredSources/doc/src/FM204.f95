      program fm204
!                                                                        
!                                                                        
!         THIS ROUTINE CONTINUES THE TESTING OF CHARACTER VARIABLES AND  
!      CHARACTER ARRAYS OF LENGTH ONE.  THE CHARACTER FEATURES TESTED IN 
!      FM202 AND FM203 ARE USED IN THE TESTS IN THIS ROUTINE.  THE       
!      FOLLOWING CHARACTER FEATURES ARE TESTED                           
!                                                                        
!         (1)  INITIAL DEFINITION OF CHARACTER ENTITIES OF LENGTH ONE BY 
!       SPECIFYING THEM IN A DATA STATEMENT.                             
!                                                                        
!         (2)  THE SUBSET FORTRAN LANGUAGE SPECIFIES THE FOLLOWING       
!      COLLATING SEQUENCE RULES.                                         
!                                                                        
!               A LESS THAN B ... LESS THAN Z,                           
!               0 LESS THAN 1 ... LESS THAN 9,                           
!               ALL OF THE DIGITS PRECEDE A OR ALL OF THE DIGITS FOLLOW  
!                   Z,                                                   
!               BLANK IS LESS THAN THE LETTER A AND BLANK IS LESS THAN   
!                   THE DIGIT ZERO.                                      
!                                                                        
!         (3)  THE VALUE OF THE INTRINSIC FUNCTION ICHAR IS AN INTEGER   
!       IN THE RANGE (0, N-1), WHERE N IS THE NUMBER OF CHARACTERS IN    
!       THE COLLATING SEQUENCE FOR THE PROCESSOR.  FOR ANY CHARACTERS    
!       C1 AND C2 CAPABLE OF REPRESENTATION IN THE PROCESSOR, C1 .LE. C2 
!       IS TRUE IF AND ONLY IF ICHAR(C1) .LE. ICHAR(C2) IS TRUE; AND     
!       C1 .EQ. C2 IF AND ONLY IF ICHAR(C1) .EQ. ICHAR(C2).              
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 3.1.5, COLLATING SEQUENCE AND GRAPHICS                 
!         SECTION 4.8, CHARACTER TYPE                                    
!         SECTION 6.2, CHARACTER EXPRESSIONS                             
!         SECTION 6.3.4, CHARACTER RELATIONAL EXPRESSIONS                
!         SECTION 6.3.5, INTERPRETATION OF CHARACTER RELATIONAL          
!                           EXPRESSIONS                                  
!         SECTION 8.4.2, CHARACTER TYPE-STATEMENT                        
!         SECTION 9.4, CHARACTER CONSTANT IN A DATA STATEMENT            
!         SECTION 10.4, CHARACTER ASSIGNMENT STATEMENT                   
!         SECTION 15.3, INTRINSIC FUNCTIONS                              
!         SECTION 15.10, TABLE 5 INTRINSIC FUNCTIONS                     
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
      integer :: i
      integer :: j
      real :: go
      real :: to
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      integer :: n
      integer :: k
      character(len=1), dimension(1:47) :: catn11
      character(len=1), dimension(1:26) :: catn12
      character(len=1), dimension(1:10) :: catn13
      character(len=1) :: cvtn10
      character(len=1), dimension(1:6) :: catn14
      character :: cvtn01
      integer, dimension(1:47) :: iaon11
      data catn11 / 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1','2','3','4','5','6','7','8','9',' ','=','+','-','*','/','(',')',',','.','''' / 
      data catn12 / 'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z' / 
      data catn14(1),catn14(2),catn14(3),catn14(4),catn14(5),catn14(6) / 6*'V' / ,iaon11 / 47*7 / ,catn13 / '0','1','2','3','4','5','6','7','8','9' / ,cvtn10 / ' ' / 
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
!         TEST 61 THROUGH TEST 73 VERIFY THE CONTENTS OF CHARACTER ARRAY 
!      ELEMENTS AND CHARACTER VARIABLES WHICH WERE INITIALLY DEFINED IN  
!      A DATA STATEMENT.                                                 
!                                                                        
!         TEST 61 THROUGH TEST 65 VERIFY THE CONTENTS OF SELECTED        
!      ELEMENTS OF THE ARRAY CATN11 WHICH WAS INITIALLY SET EQUAL TO THE 
!      47 CHARACTERS OF THE FORTRAN SUBSET LANGUAGE CHARACTER SET.       
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 061  ****                         
!                                                                        
!                                                                        
      ivtnum =  61                                                      
      if (iczero) 30610, 0610, 30610                                    
 0610 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(1)  ==  'A') ivcomp = 1                                
40610 if (ivcomp - 1) 20610, 10610, 20610                               
30610 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10610, 0621, 20610                                    
10610 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0621                                                        
20610 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0621 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 062  ****                         
!                                                                        
!                                                                        
      ivtnum =  62                                                      
      if (iczero) 30620, 0620, 30620                                    
 0620 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(47)  ==  '''') ivcomp = 1                              
40620 if (ivcomp - 1) 20620, 10620, 20620                               
30620 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10620, 0631, 20620                                    
10620 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0631                                                        
20620 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0631 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 063  ****                         
!                                                                        
!                                                                        
      ivtnum =  63                                                      
      if (iczero) 30630, 0630, 30630                                    
 0630 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(46)  ==  '.') ivcomp = 1                               
40630 if (ivcomp - 1) 20630, 10630, 20630                               
30630 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10630, 0641, 20630                                    
10630 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0641                                                        
20630 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0641 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 064  ****                         
!                                                                        
!                                                                        
      ivtnum =  64                                                      
      if (iczero) 30640, 0640, 30640                                    
 0640 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(27)  ==  '0') ivcomp = 1                               
40640 if (ivcomp - 1) 20640, 10640, 20640                               
30640 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10640, 0651, 20640                                    
10640 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0651                                                        
20640 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0651 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 065  ****                         
!                                                                        
!                                                                        
      ivtnum =  65                                                      
      if (iczero) 30650, 0650, 30650                                    
 0650 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn11(36)  ==  '9') ivcomp = 1                               
40650 if (ivcomp - 1) 20650, 10650, 20650                               
30650 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10650, 0661, 20650                                    
10650 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0661                                                        
20650 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0661 continue                                                          
!                                                                        
!         TEST 66 THROUGH TEST 68 VERIFY THE CONTENTS OF SELECTED        
!      ELEMENTS OF THE ARRAY CATN12 WHICH WAS INITIALLY SET EQUAL TO THE 
!      26 LETTERS OF THE ALPHABET.                                       
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 066  ****                         
!                                                                        
!                                                                        
      ivtnum =  66                                                      
      if (iczero) 30660, 0660, 30660                                    
 0660 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn12(1)  ==  'A') ivcomp = 1                                
40660 if (ivcomp - 1) 20660, 10660, 20660                               
30660 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10660, 0671, 20660                                    
10660 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0671                                                        
20660 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0671 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 067  ****                         
!                                                                        
!                                                                        
      ivtnum =  67                                                      
      if (iczero) 30670, 0670, 30670                                    
 0670 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn12(26)  ==  'Z') ivcomp = 1                               
40670 if (ivcomp - 1) 20670, 10670, 20670                               
30670 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10670, 0681, 20670                                    
10670 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0681                                                        
20670 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0681 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 068  ****                         
!                                                                        
!                                                                        
      ivtnum =  68                                                      
      if (iczero) 30680, 0680, 30680                                    
 0680 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn12(20)  ==  'T') ivcomp = 1                               
40680 if (ivcomp - 1) 20680, 10680, 20680                               
30680 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10680, 0691, 20680                                    
10680 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0691                                                        
20680 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0691 continue                                                          
!                                                                        
!         TEST 69 AND TEST 70 VERIFY THE CONTENTS OF SELECTED ELEMENTS   
!      OF THE ARRAY CATN13 WHICH WAS INITIALLY SET EQUAL TO THE TEN      
!      NUMERIC DIGITS.                                                   
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 069  ****                         
!                                                                        
!                                                                        
      ivtnum =  69                                                      
      if (iczero) 30690, 0690, 30690                                    
 0690 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn13(1)  ==  '0') ivcomp = 1                                
40690 if (ivcomp - 1) 20690, 10690, 20690                               
30690 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10690, 0701, 20690                                    
10690 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0701                                                        
20690 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0701 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 070  ****                         
!                                                                        
!                                                                        
      ivtnum =  70                                                      
      if (iczero) 30700, 0700, 30700                                    
 0700 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (catn13(10)  ==  '9') ivcomp = 1                               
40700 if (ivcomp - 1) 20700, 10700, 20700                               
30700 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10700, 0711, 20700                                    
10700 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0711                                                        
20700 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0711 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 071  ****                         
!                                                                        
!         TEST 71 VERIFIES THE CONTENTS OF THE VARIABLE CVTN10 WHICH     
!      WAS INITIALLY SET EQUAL TO BLANK.                                 
!                                                                        
      ivtnum =  71                                                      
      if (iczero) 30710, 0710, 30710                                    
 0710 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (cvtn10  ==  ' ') ivcomp = 1                                   
40710 if (ivcomp - 1) 20710, 10710, 20710                               
30710 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10710, 0721, 20710                                    
10710 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0721                                                        
20710 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0721 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 072  ****                         
!                                                                        
!         TEST 72 VERIFIES THE CONTENTS OF THE ARRAY CATN14 WHICH WAS    
!      INITIALLY SET EQUAL TO ALL V'S.                                   
!                                                                        
      ivtnum =  72                                                      
      if (iczero) 30720, 0720, 30720                                    
 0720 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 6                                                        
      do 722, i= 1,6                                                    
      if (catn14(i)  ==  'V') ivcomp = ivcomp + 1                       
  722 continue                                                          
40720 if (ivcomp - 6) 20720, 10720, 20720                               
30720 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10720, 0731, 20720                                    
10720 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0731                                                        
20720 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0731 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 073  ****                         
!                                                                        
!         TEST 73 VERIFIES THE CONTENTS OF THE ARRAY IAON11 WHICH WAS    
!      INITIALLY SET EQUAL TO ALL 7'S.                                   
!                                                                        
      ivtnum =  73                                                      
      if (iczero) 30730, 0730, 30730                                    
 0730 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 47                                                       
      do 732, i= 1,47                                                   
      if (iaon11(i) - 7) 732, 733, 732                                  
  733 ivcomp = ivcomp + 1                                               
  732 continue                                                          
40730 if (ivcomp - 47) 20730, 10730, 20730                              
30730 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10730, 0741, 20730                                    
10730 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0741                                                        
20730 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0741 continue                                                          
!                                                                        
!         TEST 74 THROUGH TEST 79 VERIFY THE COLLATING SEQUENCE          
!      SPECIFICATIONS FOR THE FORTRAN SUBSET LANGUAGE.                   
!                                                                        
!         TEST 74 AND TEST 75 VERIFY THE COLLATING SEQUENCE FOR LETTERS. 
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 074  ****                         
!                                                                        
!                                                                        
      ivtnum =  74                                                      
      if (iczero) 30740, 0740, 30740                                    
 0740 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 210                                                      
      if ('A'  <  'B') ivcomp = ivcomp * 2                             
      if ('B'  <  'M') ivcomp = ivcomp * 3                             
      if ('M'  <  'V') ivcomp = ivcomp * 5                             
      if ('V'  <  'Z') ivcomp = ivcomp * 7                             
40740 if (ivcomp - 210) 20740, 10740, 20740                             
30740 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10740, 0751, 20740                                    
10740 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0751                                                        
20740 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0751 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 075  ****                         
!                                                                        
!                                                                        
      ivtnum =  75                                                      
      if (iczero) 30750, 0750, 30750                                    
 0750 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 25                                                       
      do 752, i=1,25                                                    
      j= i + 1                                                          
      if (catn12(j)  >  catn12(i)) ivcomp = ivcomp + 1                 
  752 continue                                                          
40750 if (ivcomp - 25) 20750, 10750, 20750                              
30750 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10750, 0761, 20750                                    
10750 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0761                                                        
20750 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0761 continue                                                          
!                                                                        
!         TEST 76 AND TEST 77 VERIFY THE COLLATING SEQUENCE FOR DIGITS.  
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 076  ****                         
!                                                                        
!                                                                        
      ivtnum =  76                                                      
      if (iczero) 30760, 0760, 30760                                    
 0760 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 30                                                       
      if ('0'  <  '1') ivcomp = ivcomp * 2                             
      if ('1'  <  '5') ivcomp = ivcomp * 3                             
      if ('5'  <  '9') ivcomp = ivcomp * 5                             
40760 if (ivcomp - 30) 20760, 10760, 20760                              
30760 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10760, 0771, 20760                                    
10760 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0771                                                        
20760 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0771 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 077  ****                         
!                                                                        
!                                                                        
      ivtnum =  77                                                      
      if (iczero) 30770, 0770, 30770                                    
 0770 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 9                                                        
      do 772, i=1,9                                                     
      j = i + 1                                                         
      if (catn13(i)  <  catn13(j)) ivcomp = ivcomp + 1                 
  772 continue                                                          
40770 if (ivcomp - 9) 20770, 10770, 20770                               
30770 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10770, 0781, 20770                                    
10770 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0781                                                        
20770 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0781 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 078  ****                         
!                                                                        
!         TEST 78 VERIFIES THAT BLANK IS LESS THAN THE LETTER A AND BLANK
!      IS LESS THAN THE DIGIT ZERO.                                      
!                                                                        
      ivtnum =  78                                                      
      if (iczero) 30780, 0780, 30780                                    
 0780 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 6                                                        
      if (' '  <  'A') ivcomp = ivcomp * 2                             
      if (' '  <  '0') ivcomp = ivcomp * 3                             
40780 if (ivcomp - 6) 20780, 10780, 20780                               
30780 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10780, 0791, 20780                                    
10780 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0791                                                        
20780 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0791 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 079  ****                         
!                                                                        
!         TEST 79 VERIFIES THAT THE DIGITS AND LETTERS ARE NOT INTERMIXED
!      IN THE COLLATING SEQUENCE.  EITHER ALL OF THE DIGITS MUST PRECEDE 
!      A OR ALL OF THE DIGITS MUST FOLLOW Z.                             
!                                                                        
      ivtnum =  79                                                      
      if (iczero) 30790, 0790, 30790                                    
 0790 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
      if ('0'  /=  'A') goto 792                                       
      ivcomp = 111                                                      
      goto 40790                                                       
  792 if ('0'  >   'A') goto 793                                      
!                                                                        
!           ZERO IS LESS THAN LETTER A, SO ALL DIGITS MUST BE LESS THAN A
!                                                                        
      do 794, i= 1,10                                                   
      if (catn13(i)  <  'A') ivcomp = ivcomp + 1                       
  794 continue                                                          
      goto 40790                                                       
!                                                                        
!           ZERO IS GREATER THAN LETTER A, SO ALL DIGITS MUST BE GREATER 
!      THAN LETTER Z.                                                    
!                                                                        
  793 do i=1,10                                                     
      if (catn13(i)  >  'Z') ivcomp = ivcomp + 1                       
   end do
40790 if (ivcomp - 10) 20790,10790, 20790                               
30790 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10790, 0801, 20790                                    
10790 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0801                                                        
20790 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0801 continue                                                          
!                                                                        
!         TEST 80 THROUGH TEST 85 PERFORM THE SAME COMPARISONS AS TEST 74
!      THROUGH TEST 79 EXCEPT THAT THE ICHAR INTRINSIC FUNCTION IS USED  
!      IN PLACE OF THE INDIVIDUAL CHARACTERS.                            
!                                                                        
!         TEST 80 AND TEST 81 VERIFY THE COLLATING SEQUENCE FOR LETTERS  
!      USING THE ICHAR INTRINSIC FUNCTION.                               
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 080  ****                         
!                                                                        
!                                                                        
      ivtnum =  80                                                      
      if (iczero) 30800, 0800, 30800                                    
 0800 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 210                                                      
      ivon01 = ichar('A')                                               
      ivon02 = ichar('B')                                               
      ivon03 = ichar('M')                                               
      ivon04 = ichar('V')                                               
      ivon05 = ichar('Z')                                               
      if (ivon01  <  ivon02) ivcomp = ivcomp * 2                       
      if (ivon02  <  ivon03) ivcomp = ivcomp * 3                       
      if (ivon03  <  ivon04) ivcomp = ivcomp * 5                       
      if (ivon04  <  ivon05) ivcomp = ivcomp * 7                       
40800 if (ivcomp - 210) 20800, 10800, 20800                             
30800 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10800, 0811, 20800                                    
10800 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0811                                                        
20800 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0811 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 081  ****                         
!                                                                        
!                                                                        
      ivtnum =  81                                                      
      if (iczero) 30810, 0810, 30810                                    
 0810 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 0                                                        
      ivcomp = 0                                                        
      ivcorr = 25                                                       
      do 812, i=1,25                                                    
      j= i + 1                                                          
      ivon01 = ichar(catn12(j))                                         
      ivon02 = ichar(catn12(i))                                         
      if (ivon01  >  ivon02) ivcomp = ivcomp + 1                       
  812 continue                                                          
40810 if (ivcomp - 25) 20810, 10810, 20810                              
30810 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10810, 0821, 20810                                    
10810 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0821                                                        
20810 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0821 continue                                                          
!                                                                        
!         TEST 82 AND TEST 83 VERIFY THE COLLATING SEQUENCE FOR DIGITS   
!      USING THE ICHAR INTRINSIC FUNCTION.                               
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 082  ****                         
!                                                                        
!                                                                        
      ivtnum =  82                                                      
      if (iczero) 30820, 0820, 30820                                    
 0820 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 30                                                       
      if (ichar('0')  <  ichar('1')) ivcomp = ivcomp *2                
      if (ichar('1')  <  ichar('5')) ivcomp = ivcomp * 3               
      if (ichar('5')  <  ichar('9')) ivcomp = ivcomp * 5               
40820 if (ivcomp - 30) 20820, 10820, 20820                              
30820 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10820, 0831, 20820                                    
10820 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0831                                                        
20820 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0831 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 083  ****                         
!                                                                        
!                                                                        
      ivtnum =  83                                                      
      if (iczero) 30830, 0830, 30830                                    
 0830 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 0                                                        
      ivcomp = 0                                                        
      ivcorr = 9                                                        
      do 832, i=1,9                                                     
      j = i + 1                                                         
      ivon01 = ichar(catn13(j))                                         
      ivon02 = ichar(catn13(i))                                         
      if (ivon02  <  ivon01) ivcomp = ivcomp + 1                       
  832 continue                                                          
40830 if (ivcomp -9) 20830, 10830, 20830                                
30830 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10830, 0841, 20830                                    
10830 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0841                                                        
20830 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0841 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 084  ****                         
!                                                                        
!         TEST 84 VERIFIES THAT BLANK IS LESS THAN THE LETTER A AND BLANK
!      IS LESS THAN THE DIGIT ZERO.  THE INTRINSIC FUNCTION ICHAR IS     
!      USED IN THIS TEST.                                                
!                                                                        
      ivtnum =  84                                                      
      if (iczero) 30840, 0840, 30840                                    
 0840 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 6                                                        
      if (ichar(' ')  <  ichar('A')) ivcomp = ivcomp * 2               
      if (ichar(' ')  <  ichar('0')) ivcomp = ivcomp * 3               
40840 if (ivcomp - 6) 20840, 10840, 20840                               
30840 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10840, 0851, 20840                                    
10840 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0851                                                        
20840 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0851 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 085  ****                         
!                                                                        
!         TEST 85 VERIFIES THAT THE DIGITS AND LETTERS ARE NOT INTERMIXED
!      IN THE COLLATING SEQUENCE.  THE ICHAR INTRINSIC FUNCTION IS USED  
!      TO VERIFY THAT EITHER ALL OF THE DIGITS PRECEDE A OR ALL OF THE   
!      DIGITS FOLLOW Z.                                                  
!                                                                        
      ivtnum =  85                                                      
      if (iczero) 30850, 0850, 30850                                    
 0850 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
      if (ichar('0')  /=  ichar('A')) goto 852                         
      ivcomp = 111                                                      
      goto 40850                                                       
  852 if (ichar('0')  >  ichar('A')) goto 853                         
!                                                                        
!           ZERO IS LESS THAN LETTER A ACCORDING TO ICHAR INTRINSIC      
!      FUNCTION VALUE.  THUS, THE ICHAR VALUE FOR ALL DIGITS MUST BE     
!      LESS THAN ICHAR VALUE FOR LETTER A.                               
!                                                                        
      do 854, i=1,10                                                    
      if (ichar(catn13(i))  <  ichar('A')) ivcomp = ivcomp + 1         
  854 continue                                                          
      goto 40850                                                       
!                                                                        
!           ZERO IS GREATER THAN LETTER A ACCORDING TO ICHAR INTRINSIC   
!      FUNCTION VALUE.  THUS, THE ICHAR VALUE FOR ALL DIGITS MUST BE     
!      GREATER THAN ICHAR VALUE FOR LETTER Z.                            
!                                                                        
  853 do 855, i=1,10                                                    
      if (ichar(catn13(i)) >  ichar('Z')) ivcomp = ivcomp + 1          
  855 continue                                                          
40850 if (ivcomp - 10) 20850, 10850, 20850                              
30850 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10850, 0861, 20850                                    
10850 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0861                                                        
20850 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0861 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 204  -  TEST 086  ****                         
!                                                                        
!           THE ARRAY IAON11 IS SET EQUAL TO THE ICHAR INTRINSIC FUNCTION
!      VALUE OF THE CORRESPONDING ELEMENT IN THE CATN11 ARRAY.  THE      
!      IAON11 ARRAY IS THEN SORTED IN ASCENDING ORDER, AND ENTRIES IN    
!      THE CATN11 ARRAY ARE ARRANGED ACCORDING TO THE ASCENDING SORT     
!      ORDER IN IAON11.  THE RESULTING ORDER OF THE CATN11 ARRAY GIVES   
!      THE PROCESSOR'S COLLATING SEQUENCE FOR THE FORTRAN SUBSET LANGUAGE
!      CHARACTER SET.  THE CATN11 ARRAY IS PRINTED AND MUST BE VISUALLY  
!      CHECKED TO DETERMINE IF THE COLLATING SEQUENCE RULES ARE FOLLOWED 
!      BY THE COMPILER.                                                  
!                                                                        
      ivtnum =  86                                                      
      if (iczero) 30860, 0860, 30860                                    
 0860 continue                                                          
      ivcomp = 0                                                        
!                                                                        
!           INITIALIZE IAON11 TO ZERO.                                   
      do i=1,47                                                     
      iaon11(i) = 0                                                     
   end do
!                                                                        
!           PLACE ICHAR INTRINSIC VALUE IN IAON11.                       
!                                                                        
      do 863, i= 1,47                                                   
      iaon11(i) = ichar(catn11(i))                                      
  863 continue                                                          
!                                                                        
!           SORT FORTRAN CHARACTERS ACCORDING TO THEIR POSITION IN THE   
!      COLLATING SEQUENCE.                                               
!                                                                        
      do 864, i=1,46                                                    
      j=i                                                               
      n = i + 1                                                         
      do k = n,47                                                   
      if (iaon11(j)  <  iaon11(k)) goto 865                           
      j=k                                                               
  865 continue                                                          
      end do
      ivon01 = iaon11(j)                                                
      iaon11(j)= iaon11(i)                                              
      iaon11(i)= ivon01                                                 
      cvtn01 = catn11(j)                                                
      catn11(j) = catn11(i)                                             
      catn11(i) = cvtn01                                                
  864 continue                                                          
      write (i02, 866) catn11                                           
      write (i02, 867) iaon11                                           
  866 format (3x,'FORTRAN CHARACTER SET IN ASCENDING ORDER',3x,/          3x, 'VISUAL VERIFICATION REQUIRED'     //,3x, 12(a1,3x)/          3x, 12(a1,3x)/ 3x, 12(a1,3x)/ 3x, 11(a1,3x))                    
  867 format ( 3x/3x, 'ICHAR INTRINSIC FUNCTION VALUES FOR FORTRAN ',      'CHARACTER SET'// 3x, 12i4/ 3x, 12i4/ 3x, 12i4/                     3x,11i4//)                                                   
      ivcomp = 1                                                        
      ivcorr = 1                                                        
40860 if (ivcomp - 1) 20860, 10860, 20860                               
30860 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10860, 0871, 20860                                    
10860 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0871                                                        
20860 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0871 continue                                                          
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
90001 format (" ",24x,"FM204")                                          
90000 format (" ",20x,"END OF PROGRAM FM204" )                          
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
      end program fm204
