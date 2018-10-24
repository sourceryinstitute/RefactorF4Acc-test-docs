      program fm062
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: rvon01
      real :: rvon02
      real :: rvcomp
      real :: rvcorr
      real :: rvon03
      integer :: ivon01
      real :: rvon04
      real :: rvon05
!      COMMENT SECTION                                                   
!                                                                        
!      FM062                                                             
!                                                                        
!           THIS ROUTINE TESTS ARITHMETIC ASSIGNMENT STATEMENTS WHERE    
!      AN ARITHMETIC EXPRESSION FORMED FROM REAL VARIABLES AND           
!      CONSTANTS CONNECTED BY ARITHMETIC OPERATORS IS ASSIGNED TO        
!      A REAL VARIABLE.  IN CASES INVOLVING THE EXPONENTIATION           
!      OPERATOR, REAL VALUES ARE RAISED TO INTEGER POWERS ONLY.          
!                                                                        
!            A REAL DATUM IS A PROCESSOR APPROXIMATION TO THE VALUE OF A 
!      REAL NUMBER.  IT MAY ASSUME POSITIVE, NEGATIVE AND ZERO VALUES.   
!                                                                        
!           A BASIC REAL CONSTANT IS WRITTEN AS AN INTEGER PART, A       
!      DECIMAL POINT, AND A DECIMAL FRACTION PART IN THAT ORDER.  BOTH   
!      THE INTEGER PART AND THE DECIMAL PART ARE STRINGS OF DIGITS;      
!      EITHER ONE OF THESE STRINGS MAY BE EMPTY BUT NOT BOTH.  THE       
!      CONSTANT IS AN APPROXIMATION TO THE DIGIT STRING INTERPRETED AS A 
!      DECIMAL NUMERAL.                                                  
!                                                                        
!          A DECIMAL EXPONENT IS WRITTEN AS THE LETTER E, FOLLOWED BY AN 
!      OPTIONALLY SIGNED INTEGER CONSTANT.                               
!                                                                        
!          A REAL CONSTANT IS INDICATED BY WRITING A BASIC REAL CONSTANT,
!      A BASIC REAL CONSTANT FOLLOWED BY A DECIMAL EXPONENT, OR AN       
!      INTEGER CONSTANT FOLLOWED BY A DECIMAL EXPONENT.                  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.4, REAL TYPE                                         
!         SECTION 4.4.1, REAL CONSTANT                                   
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
!           ARITHMETIC ASSIGNMENT STATEMENT                              
!                                                                        
!                                                                        
!      TESTS 62 THROUGH 70 USE A MIXTURE OF REAL VARIABLES AND REAL      
!      CONSTANTS CONNECTED BY TWO IDENTICAL ARITHMETIC OPERATORS.        
!      TESTS OCCUR IN PAIRS, ONE WITHOUT PARENTHESES AND ONE WITH        
!      PARENTHESES TO ALTER THE NORMAL ORDER OF EVALUATION.              
!                                                                        
!      TESTS 71 THROUGH 90 USE THREE REAL VARIABLES CONNECTED BY A       
!      PAIR OF DISSIMILAR OPERATORS.  ALL COMBINATIONS AND ORDERINGS     
!      OF OPERATORS ARE EXERCIZED.  WHERE EXPONENTIATION IS TESTED,      
!      INTEGER VARIABLES ARE USED FOR THE POWER PRIMARIES.               
!                                                                        
!      TESTS 91 AND 92 USE A SERIES OF REAL VARIABLES CONNECTED BY ONE   
!      EACH OF THE ARITHMETIC OPERTORS.  PARENTHETICAL NOTATIONS ARE     
!      ALSO TESTED.                                                      
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!                                                                        
      ivtnum =  62                                                      
!                                                                        
!       ****  TEST  62  ****                                             
!                                                                        
      if (iczero) 30620,  620, 30620                                    
  620 continue                                                          
      rvon01 = 7.5                                                      
      rvon02 = 5e2                                                      
      rvcomp = rvon01 + rvon02 + 33e-1                                  
      goto 40620                                                       
30620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40620,  631, 40620                                    
40620 if (rvcomp - 510.75) 20620,10620,40621                            
40621 if (rvcomp - 510.85) 10620,10620,20620                            
10620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  631                                                        
20620 ivfail = ivfail + 1                                               
      rvcorr = 510.8                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  631 continue                                                          
      ivtnum =  63                                                      
!                                                                        
!       ****  TEST  63  ****                                             
!                                                                        
      if (iczero) 30630,  630, 30630                                    
  630 continue                                                          
      rvon01 = 75e-1                                                    
      rvon02 = 500.0                                                    
      rvcomp = rvon01 + (rvon02 + 3.3)                                  
      goto 40630                                                       
30630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40630,  641, 40630                                    
40630 if (rvcomp - 510.75) 20630,10630,40631                            
40631 if (rvcomp - 510.85) 10630,10630,20630                            
10630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  641                                                        
20630 ivfail = ivfail + 1                                               
      rvcorr = 510.8                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  641 continue                                                          
      ivtnum =  64                                                      
!                                                                        
!       ****  TEST  64  ****                                             
!                                                                        
      if (iczero) 30640,  640, 30640                                    
  640 continue                                                          
      rvcomp = 7.5 - 500. - 3.3                                         
      goto 40640                                                       
30640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40640,  651, 40640                                    
40640 if (rvcomp + 495.85) 20640,10640,40641                            
40641 if (rvcomp + 495.75) 10640,10640,20640                            
10640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  651                                                        
20640 ivfail = ivfail + 1                                               
      rvcorr = -495.8                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  651 continue                                                          
      ivtnum =  65                                                      
!                                                                        
!       ****  TEST  65  ****                                             
!                                                                        
      if (iczero) 30650,  650, 30650                                    
  650 continue                                                          
      rvon01 = 7.5                                                      
      rvon02 = 5e2                                                      
      rvcomp = rvon01 - (33e-1 - rvon02)                                
      goto 40650                                                       
30650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40650,  661, 40650                                    
40650 if (rvcomp - 504.15) 20650,10650,40651                            
40651 if (rvcomp - 504.25) 10650,10650,20650                            
10650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  661                                                        
20650 ivfail = ivfail + 1                                               
      rvcorr = 504.2                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  661 continue                                                          
      ivtnum =  66                                                      
!                                                                        
!       ****  TEST  66  ****                                             
!                                                                        
      if (iczero) 30660,  660, 30660                                    
  660 continue                                                          
      rvon01 = 7.5                                                      
      rvcomp = 5e2 * 33e-1 * rvon01                                     
      goto 40660                                                       
30660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40660,  671, 40660                                    
40660 if (rvcomp - 12370) 20660,10660,40661                             
40661 if (rvcomp - 12380) 10660,10660,20660                             
10660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  671                                                        
20660 ivfail = ivfail + 1                                               
      rvcorr = 12375.                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  671 continue                                                          
      ivtnum =  67                                                      
!                                                                        
!       ****  TEST  67  ****                                             
!                                                                        
      if (iczero) 30670,  670, 30670                                    
  670 continue                                                          
      rvon01 = 7.5                                                      
      rvcomp = 5e2 * (rvon01 * 33e-1)                                   
      goto 40670                                                       
30670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40670,  681, 40670                                    
40670 if (rvcomp - 12370) 20670,10670,40671                             
40671 if (rvcomp - 12380) 10670,10670,20670                             
10670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  681                                                        
20670 ivfail = ivfail + 1                                               
      rvcorr = 12375.                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  681 continue                                                          
      ivtnum =  68                                                      
!                                                                        
!       ****  TEST  68  ****                                             
!                                                                        
      if (iczero) 30680,  680, 30680                                    
  680 continue                                                          
      rvon01 = 7.5                                                      
      rvon02 = 33e-1                                                    
      rvon03 = -5e+2                                                    
      rvcomp = rvon01 / rvon02 / rvon03                                 
      goto 40680                                                       
30680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40680,  691, 40680                                    
40680 if (rvcomp + .00459) 20680,10680,40681                            
40681 if (rvcomp + .00449) 10680,10680,20680                            
10680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  691                                                        
20680 ivfail = ivfail + 1                                               
      rvcorr = -.0045454                                                
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  691 continue                                                          
      ivtnum =  69                                                      
!                                                                        
!       ****  TEST  69  ****                                             
!                                                                        
      if (iczero) 30690,  690, 30690                                    
  690 continue                                                          
      rvon01 = 7.5                                                      
      rvon02 = 33e-1                                                    
      rvon03 = -5e+2                                                    
      rvcomp = rvon01 / (rvon02 / rvon03)                               
      goto 40690                                                       
30690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40690,  701, 40690                                    
40690 if (rvcomp + 1180.) 20690,10690,40691                             
40691 if (rvcomp + 1080.) 10690,10690,20690                             
10690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  701                                                        
20690 ivfail = ivfail + 1                                               
      rvcorr = -1136.4                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  701 continue                                                          
      ivtnum =  70                                                      
!                                                                        
!       ****  TEST  70  ****                                             
!                                                                        
      if (iczero) 30700,  700, 30700                                    
  700 continue                                                          
      rvon01 = 3.835e3                                                  
      ivon01 =  5                                                       
      rvcomp = rvon01 ** ivon01                                         
      goto 40700                                                       
30700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40700,  711, 40700                                    
40700 if (rvcomp - 8.29e17) 20700,10700,40701                           
40701 if (rvcomp - 8.30e17) 10700,10700,20700                           
10700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  711                                                        
20700 ivfail = ivfail + 1                                               
      rvcorr = 8.295e17                                                 
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  711 continue                                                          
!                                                                        
!      TESTS 71 THROUGH 74 TEST  RV1 + RV2 <OP2> RV3                     
!                                                                        
      ivtnum =  71                                                      
!                                                                        
!       ****  TEST  71  ****                                             
!                                                                        
      if (iczero) 30710,  710, 30710                                    
  710 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 = 3.35                                                     
      rvon03 = .005679                                                  
      rvcomp = rvon01 + rvon02 - rvon03                                 
      goto 40710                                                       
30710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40710,  721, 40710                                    
40710 if (rvcomp - 528.16) 20710,10710,40711                            
40711 if (rvcomp - 528.26) 10710,10710,20710                            
10710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  721                                                        
20710 ivfail = ivfail + 1                                               
      rvcorr = 528.21                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  721 continue                                                          
      ivtnum =  72                                                      
!                                                                        
!       ****  TEST  72  ****                                             
!                                                                        
      if (iczero) 30720,  720, 30720                                    
  720 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 = 3.35                                                     
      rvon03 = .005679                                                  
      rvcomp = rvon01 + rvon02 * rvon03                                 
      goto 40720                                                       
30720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40720,  731, 40720                                    
40720 if (rvcomp - 524.84) 20720,10720,40721                            
40721 if (rvcomp - 524.94) 10720,10720,20720                            
10720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  731                                                        
20720 ivfail = ivfail + 1                                               
      rvcorr = 524.89                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  731 continue                                                          
      ivtnum =  73                                                      
!                                                                        
!       ****  TEST  73  ****                                             
!                                                                        
      if (iczero) 30730,  730, 30730                                    
  730 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 = 3.35                                                     
      rvon03 = .005679                                                  
      rvcomp = rvon01 + rvon02 / rvon03                                 
      goto 40730                                                       
30730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40730,  741, 40730                                    
40730 if (rvcomp - 1114.2) 20730,10730,40731                            
40731 if (rvcomp - 1115.2) 10730,10730,20730                            
10730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  741                                                        
20730 ivfail = ivfail + 1                                               
      rvcorr = 1114.8                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  741 continue                                                          
      ivtnum =  74                                                      
!                                                                        
!       ****  TEST  74  ****                                             
!                                                                        
      if (iczero) 30740,  740, 30740                                    
  740 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 = 3.35                                                     
      ivon01 = 7                                                        
      rvcomp = rvon01 + rvon02 ** ivon01                                
      goto 40740                                                       
30740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40740,  751, 40740                                    
40740 if (rvcomp - 5259.3) 20740,10740,40741                            
40741 if (rvcomp - 5260.3) 10740,10740,20740                            
10740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  751                                                        
20740 ivfail = ivfail + 1                                               
      rvcorr = 5259.8                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  751 continue                                                          
!                                                                        
!      TESTS 75 THROUGH 78 CHECK     RV1 - RV2 <OP2> RV3                 
!                                                                        
      ivtnum =  75                                                      
!                                                                        
!       ****  TEST  75  ****                                             
!                                                                        
      if (iczero) 30750,  750, 30750                                    
  750 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 = 3.35                                                     
      rvon03 = .5679                                                    
      rvcomp = rvon01 - rvon02 + rvon03                                 
      goto 40750                                                       
30750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40750,  761, 40750                                    
40750 if (rvcomp - 522.03) 20750,10750,40751                            
40751 if (rvcomp - 522.13) 10750,10750,20750                            
10750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  761                                                        
20750 ivfail = ivfail + 1                                               
      rvcorr = 522.09                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  761 continue                                                          
      ivtnum =  76                                                      
!                                                                        
!       ****  TEST  76  ****                                             
!                                                                        
      if (iczero) 30760,  760, 30760                                    
  760 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      rvon03 =    .5679                                                 
      rvcomp = rvon01 - rvon02 * rvon03                                 
      goto 40760                                                       
30760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40760,  771, 40760                                    
40760 if (rvcomp - 522.92) 20760,10760,40761                            
40761 if (rvcomp - 523.02) 10760,10760,20760                            
10760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  771                                                        
20760 ivfail = ivfail + 1                                               
      rvcorr = 522.97                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  771 continue                                                          
      ivtnum =  77                                                      
!                                                                        
!       ****  TEST  77  ****                                             
!                                                                        
      if (iczero) 30770,  770, 30770                                    
  770 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      rvon03 =    .5679                                                 
      rvcomp = rvon01 - rvon02 / rvon03                                 
      goto 40770                                                       
30770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40770,  781, 40770                                    
40770 if (rvcomp - 518.92) 20770,10770,40771                            
40771 if (rvcomp - 519.02) 10770,10770,20770                            
10770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  781                                                        
20770 ivfail = ivfail + 1                                               
      rvcorr = 518.97                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  781 continue                                                          
      ivtnum =  78                                                      
!                                                                        
!       ****  TEST  78  ****                                             
!                                                                        
      if (iczero) 30780,  780, 30780                                    
  780 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      ivon01 =   7                                                      
      rvcomp = rvon01 - rvon02 ** ivon01                                
      goto 40780                                                       
30780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40780,  791, 40780                                    
40780 if (rvcomp + 4210.6) 20780,10780,40781                            
40781 if (rvcomp + 4209.6) 10780,10780,20780                            
10780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  791                                                        
20780 ivfail = ivfail + 1                                               
      rvcorr = -4210.1                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  791 continue                                                          
!                                                                        
!      TESTS 79 THROUGH 82 CHECK     RV1 * RV2 <OP2> RV3                 
!                                                                        
      ivtnum =  79                                                      
!                                                                        
!       ****  TEST  79  ****                                             
!                                                                        
      if (iczero) 30790,  790, 30790                                    
  790 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   .5679                                                  
      rvon03 =   3.35                                                   
      rvcomp = rvon01 * rvon02 + rvon03                                 
      goto 40790                                                       
30790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40790,  801, 40790                                    
40790 if (rvcomp - 301.37) 20790,10790,40791                            
40791 if (rvcomp - 301.47) 10790,10790,20790                            
10790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  801                                                        
20790 ivfail = ivfail + 1                                               
      rvcorr = 301.42                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  801 continue                                                          
      ivtnum =  80                                                      
!                                                                        
!       ****  TEST  80  ****                                             
!                                                                        
      if (iczero) 30800,  800, 30800                                    
  800 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =    .5679                                                 
      rvon03 =   3.35                                                   
      rvcomp = rvon01 * rvon02 - rvon03                                 
      goto 40800                                                       
30800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40800,  811, 40800                                    
40800 if (rvcomp - 294.67) 20800,10800,40801                            
40801 if (rvcomp - 294.77) 10800,10800,20800                            
10800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  811                                                        
20800 ivfail = ivfail + 1                                               
      rvcorr = 294.72                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  811 continue                                                          
      ivtnum =  81                                                      
!                                                                        
!       ****  TEST  81  ****                                             
!                                                                        
      if (iczero) 30810,  810, 30810                                    
  810 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =    .5679                                                 
      rvon03 =   3.35                                                   
      rvcomp = rvon01 * rvon02 / rvon03                                 
      goto 40810                                                       
30810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40810,  821, 40810                                    
40810 if (rvcomp - 88.92) 20810,10810,40811                             
40811 if (rvcomp - 89.02) 10810,10810,20810                             
10810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  821                                                        
20810 ivfail = ivfail + 1                                               
      rvcorr = 88.977                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  821 continue                                                          
      ivtnum =  82                                                      
!                                                                        
!       ****  TEST  82  ****                                             
!                                                                        
      if (iczero) 30820,  820, 30820                                    
  820 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =    .5679                                                 
      ivon01 =   7                                                      
      rvcomp = rvon01 * rvon02 ** ivon01                                
      goto 40820                                                       
30820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40820,  831, 40820                                    
40820 if (rvcomp -  9.94) 20820,10820,40821                             
40821 if (rvcomp - 10.04) 10820,10820,20820                             
10820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  831                                                        
20820 ivfail = ivfail + 1                                               
      rvcorr = 9.999                                                    
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  831 continue                                                          
!                                                                        
!      TESTS 83 THROUGH 86 CHECK     RV1 / RV2 <OP2> RV3                 
!                                                                        
      ivtnum =  83                                                      
!                                                                        
!       ****  TEST  83  ****                                             
!                                                                        
      if (iczero) 30830,  830, 30830                                    
  830 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      rvon03 =    .5679                                                 
      rvcomp = rvon01 / rvon02 + rvon03                                 
      goto 40830                                                       
30830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40830,  841, 40830                                    
40830 if (rvcomp - 157.19) 20830,10830,40831                            
40831 if (rvcomp - 157.29) 10830,10830,20830                            
10830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  841                                                        
20830 ivfail = ivfail + 1                                               
      rvcorr = 157.25                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  841 continue                                                          
      ivtnum =  84                                                      
!                                                                        
!       ****  TEST  84  ****                                             
!                                                                        
      if (iczero) 30840,  840, 30840                                    
  840 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      rvon03 =    .8507                                                 
      rvcomp = rvon01 / rvon02 - rvon03                                 
      goto 40840                                                       
30840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40840,  851, 40840                                    
40840 if (rvcomp - 155.77) 20840,10840,40841                            
40841 if (rvcomp - 155.87) 10840,10840,20840                            
10840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  851                                                        
20840 ivfail = ivfail + 1                                               
      rvcorr = 155.83                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  851 continue                                                          
      ivtnum =  85                                                      
!                                                                        
!       ****  TEST  85  ****                                             
!                                                                        
      if (iczero) 30850,  850, 30850                                    
  850 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      rvon03 =    .8507                                                 
      rvcomp = rvon01 / rvon02 * rvon03                                 
      goto 40850                                                       
30850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40850,  861, 40850                                    
40850 if (rvcomp - 132.7) 20850,10850,40851                             
40851 if (rvcomp - 133.7) 10850,10850,20850                             
10850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  861                                                        
20850 ivfail = ivfail + 1                                               
      rvcorr = 133.29                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  861 continue                                                          
      ivtnum =  86                                                      
!                                                                        
!       ****  TEST  86  ****                                             
!                                                                        
      if (iczero) 30860,  860, 30860                                    
  860 continue                                                          
      rvon01 = 524.87                                                   
      rvon02 =   3.35                                                   
      ivon01 =   7                                                      
      rvcomp = rvon01 / rvon02 ** ivon01                                
      goto 40860                                                       
30860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40860,  871, 40860                                    
40860 if (rvcomp - .106) 20860,10860,40861                              
40861 if (rvcomp - .116) 10860,10860,20860                              
10860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  871                                                        
20860 ivfail = ivfail + 1                                               
      rvcorr = .11085                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  871 continue                                                          
!                                                                        
!      TESTS 87 THROUGH 90 CHECK     RV1 ** IV1 <OP2> RV2                
!                                                                        
      ivtnum =  87                                                      
!                                                                        
!       ****  TEST  87  ****                                             
!                                                                        
      if (iczero) 30870,  870, 30870                                    
  870 continue                                                          
      rvon01 =   3.35                                                   
      ivon01 =   7                                                      
      rvon02 = 524.87                                                   
      rvcomp = rvon01 ** ivon01 + rvon02                                
      goto 40870                                                       
30870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40870,  881, 40870                                    
40870 if (rvcomp - 5210.) 20870,10870,40871                             
40871 if (rvcomp - 5310.) 10870,10870,20870                             
10870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  881                                                        
20870 ivfail = ivfail + 1                                               
      rvcorr = 5259.8                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  881 continue                                                          
      ivtnum =  88                                                      
!                                                                        
!       ****  TEST  88  ****                                             
!                                                                        
      if (iczero) 30880,  880, 30880                                    
  880 continue                                                          
      rvon01 =   3.35                                                   
      ivon01 =   7                                                      
      rvon02 = 524.87                                                   
      rvcomp = rvon01 ** ivon01 - rvon02                                
      goto 40880                                                       
30880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40880,  891, 40880                                    
40880 if (rvcomp - 4160.) 20880,10880,40881                             
40881 if (rvcomp - 4260.) 10880,10880,20880                             
10880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  891                                                        
20880 ivfail = ivfail + 1                                               
      rvcorr = 4210.1                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  891 continue                                                          
      ivtnum =  89                                                      
!                                                                        
!       ****  TEST  89  ****                                             
!                                                                        
      if (iczero) 30890,  890, 30890                                    
  890 continue                                                          
      rvon01 =   3.35                                                   
      ivon01 =   7                                                      
      rvon02 = 524.87                                                   
      rvcomp = rvon01 ** ivon01 * rvon02                                
      goto 40890                                                       
30890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40890,  901, 40890                                    
40890 if (rvcomp - 2.43e6) 20890,10890,40891                            
40891 if (rvcomp - 2.53e6) 10890,10890,20890                            
10890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  901                                                        
20890 ivfail = ivfail + 1                                               
      rvcorr = 2.4852e6                                                 
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  901 continue                                                          
      ivtnum =  90                                                      
!                                                                        
!       ****  TEST  90  ****                                             
!                                                                        
      if (iczero) 30900,  900, 30900                                    
  900 continue                                                          
      rvon01 =   3.35                                                   
      ivon01 =   7                                                      
      rvon02 = 524.87                                                   
      rvcomp = rvon01 ** ivon01 / rvon02                                
      goto 40900                                                       
30900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40900,  911, 40900                                    
40900 if (rvcomp - 8.97) 20900,10900,40901                              
40901 if (rvcomp - 9.07) 10900,10900,20900                              
10900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  911                                                        
20900 ivfail = ivfail + 1                                               
      rvcorr = 9.0211                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  911 continue                                                          
!                                                                        
!      TESTS 91 AND 92 CHECK ALL ARITHMETIC OPERATORS USED TOGETHER      
!                                                                        
      ivtnum =  91                                                      
!                                                                        
!       ****  TEST  91  ****                                             
!                                                                        
      if (iczero) 30910,  910, 30910                                    
  910 continue                                                          
      rvon01 = 780.56                                                   
      rvon02 =    .803                                                  
      rvon03 =   3.35                                                   
      ivon01 =   7                                                      
      rvon04 =  20.07                                                   
      rvon05 = 511.9                                                    
      rvcomp = - rvon01 + rvon02 * rvon03 ** ivon01 / rvon04 - rvon05   
      goto 40910                                                       
30910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40910,  921, 40910                                    
40910 if (rvcomp + 1113.0) 20910,10910,40911                            
40911 if (rvcomp + 1093.0) 10910,10910,20910                            
10910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  921                                                        
20910 ivfail = ivfail + 1                                               
      rvcorr = -1103.0                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  921 continue                                                          
      ivtnum =  92                                                      
!                                                                        
!       ****  TEST  92  ****                                             
!                                                                        
      if (iczero) 30920,  920, 30920                                    
  920 continue                                                          
      rvon01 = 780.56                                                   
      rvon02 =    .803                                                  
      rvon03 =   3.35                                                   
      ivon01 =   7                                                      
      rvon04 =  20.07                                                   
      rvon05 = 511.9                                                    
      rvcomp = (-rvon01) + (rvon02 * rvon03) ** ivon01 / (rvon04-rvon05)
      goto 40920                                                       
30920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40920,  931, 40920                                    
40920 if (rvcomp + 788.) 20920,10920,40921                              
40921 if (rvcomp + 777.) 10920,10920,20920                              
10920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  931                                                        
20920 ivfail = ivfail + 1                                               
      rvcorr = -782.63                                                  
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
  931 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM062" )                          
      end program fm062
