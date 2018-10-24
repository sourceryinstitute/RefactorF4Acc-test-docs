      program fm205
!                                                                        
!                                                                        
!           THE ROUTINE FM205 TESTS CHARACTER CONSTANTS, CHARACTER       
!      VARIABLES, AND CHARACTER ARRAY ELEMENTS WITH A MAXIMUM LENGTH     
!      OF 57 CHARACTERS.  CHARACTER ASSIGNMENT STATEMENTS AND CHARACTER  
!      RELATIONAL EXPRESSIONS OF THE FOLLOWING STATEMENT FORMS ARE       
!      TESTED IN THIS ROUTINE.                                           
!                                                                        
!           (1)  CHARACTER ASSIGNMENT STATEMENTS                         
!                                                                        
!                   CHARACTER VARIABLE = CHARACTER CONSTANT,             
!                   CHARACTER VARIABLE = CHARACTER VARIABLE,             
!                   CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT,        
!                   CHARACTER ARRAY ELEMENT = CHARACTER VARIABLE,        
!                   CHARACTER ARRAY ELEMENT = CHARACTER ARRAY ELEMENT,   
!                   CHARACTER VARIABLE = CHARACTER ARRAY ELEMENT.        
!                                                                        
!           THE CHARACTER ENTITIES IN AN ASSIGNMENT STATEMENT ARE THE    
!           SAME LENGTH.                                                 
!                                                                        
!           (2)  CHARACTER RELATIONAL EXPRESSIONS                        
!                                                                        
!                   CHARACTER VARIABLE RELOP CHARACTER CONSTANT,         
!                   CHARACTER VARIABLE RELOP CHARACTER VARIABLE,         
!                   CHARACTER ARRAY ELEMENT RELOP CHARACTER CONSTANT,    
!                   CHARACTER ARRAY ELEMENT RELOP CHARACTER VARIABLE,    
!                   CHARACTER ARRAY ELEMENT RELOP CHAR. ARRAY ELEMENT.   
!                                                                        
!           THE CHARACTER ENTITIES IN A RELATIONAL EXPRESSION ARE THE    
!           SAME LENGTH.                                                 
!                                                                        
!      REFERENCES                                                        
!           AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,     
!                X3.9-1978.                                              
!                                                                        
!           SECTION 4.8,   CHARACTER TYPE                                
!           SECTION 4.8.1, CHARACTER CONSTANT                            
!           SECTION 6.2,   CHARACTER EXPRESSIONS                         
!           SECTION 6.3.4, CHARACTER RELATIONAL EXPRESSION               
!           SECTION 6.3.5, INTERPRETATION OF CHARACTER RELATIONAL        
!                             EXPRESSIONS                                
!           SECTION 8.4,2, CHARACTER TYPE-STATEMENT                      
!           SECTION 10.4,  CHARACTER ASSIGNMENT STATEMENT                
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
      character(len=3) :: cvtn01
      character(len=7) :: cvtn02
      character(len=12) :: cvtn03
      character(len=25) :: cvtn04
      character(len=41) :: cvtn05
      character(len=57) :: cvtn06
      character(len=3) :: cvtn07
      character(len=7) :: cvtn08
      character(len=12) :: cvtn09
      character(len=25) :: cvtn10
      character(len=41) :: cvtn11
      character(len=57) :: cvtn12
      character(len=3), dimension(1:6) :: catn11
      character(len=7), dimension(1:7) :: catn12
      character(len=12), dimension(1:3) :: catn13
      character(len=25), dimension(1:2) :: catn14
      character(len=41), dimension(1:10) :: catn15
      character(len=57), dimension(1:4) :: catn16
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
!           TEST 87 THROUGH TEST 92 VERIFY THE CHARACTER ASSIGNMENT      
!      STATEMENT                                                         
!                                                                        
!           CHARACTER VARIABLE = CHARACTER CONSTANT                      
!                                                                        
!      IS CORRECT.  THE VARIABLE AND CONSTANT ARE THE SAME LENGTH, AND   
!      THE LENGTHS 3, 7, 12, 25, 41, AND 57 ARE USED IN THESE TESTS.     
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 087  ****                         
!                                                                        
!                                                                        
      ivtnum =  87                                                      
      if (iczero) 30870, 0870, 30870                                    
 0870 continue                                                          
      ivcomp = 0                                                        
      cvtn01 = 'ABC'                                                    
      if (cvtn01  ==  'ABC') ivcomp = 1                                 
      ivcorr = 1                                                        
40870 if (ivcomp - 1) 20870, 10870, 20870                               
30870 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10870, 0881, 20870                                    
10870 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0881                                                        
20870 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0881 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 088  ****                         
!                                                                        
!                                                                        
      ivtnum =  88                                                      
      if (iczero) 30880, 0880, 30880                                    
 0880 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn02 = 'ABCDEFG'                                                
      if (cvtn02  ==  'ABCDEFG') ivcomp = 1                             
40880 if (ivcomp - 1) 20880, 10880, 20880                               
30880 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10880, 0891, 20880                                    
10880 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0891                                                        
20880 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0891 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 089  ****                         
!                                                                        
!                                                                        
      ivtnum =  89                                                      
      if (iczero) 30890, 0890, 30890                                    
 0890 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = 'ABCDEFGHIJKL'                                           
      if (cvtn03  ==  'ABCDEFGHIJKL') ivcomp = 1                        
40890 if (ivcomp - 1) 20890, 10890, 20890                               
30890 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10890, 0901, 20890                                    
10890 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0901                                                        
20890 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0901 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 090  ****                         
!                                                                        
!                                                                        
      ivtnum =  90                                                      
      if (iczero) 30900, 0900, 30900                                    
 0900 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn04 = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                              
      if (cvtn04  ==  'ABCDEFGHIJKLMNOPQRSTUVWXY') ivcomp = 1           
40900 if (ivcomp - 1) 20900, 10900, 20900                               
30900 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10900, 0911, 20900                                    
10900 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0911                                                        
20900 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0911 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 091  ****                         
!                                                                        
!                                                                        
      ivtnum =  91                                                      
      if (iczero) 30910, 0910, 30910                                    
 0910 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn05 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'              
      if (cvtn05  ==  'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO')        ivcomp = 1                                                      
40910 if (ivcomp - 1) 20910, 10910, 20910                               
30910 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10910, 0921, 20910                                    
10910 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0921                                                        
20910 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0921 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 092  ****                         
!                                                                        
!                                                                        
      ivtnum =  92                                                      
      if (iczero) 30920, 0920, 30920                                    
 0920 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn06 =                                                            'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE'     
      if (cvtn06  ==                                                      'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE')         ivcomp = 1                                                   
40920 if (ivcomp - 1) 20920, 10920, 20920                               
30920 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10920, 0931, 20920                                    
10920 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0931                                                        
20920 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0931 continue                                                          
!                                                                        
!           TEST 93 THROUGH TEST 96 VERIFY THE CHARACTER ASSIGNMENT      
!      STATEMENTS                                                        
!                                                                        
!           CHARACTER VARIABLE = CHARACTER CONSTANT                      
!           CHARACTER VARIABLE = CHARACTER VARIABLE                      
!                                                                        
!      ARE CORRECT.  THE VARIABLES AND CONSTANT ARE THE SAME LENGTH,     
!      AND THE LENGTHS 3, 12, 25, AND 57 ARE USED IN THESE TESTS.        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 093  ****                         
!                                                                        
!                                                                        
      ivtnum =  93                                                      
      if (iczero) 30930, 0930, 30930                                    
 0930 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn07 = '   '                                                    
      cvtn01 = 'ABC'                                                    
      cvtn07 = cvtn01                                                   
      if (cvtn07  ==  'ABC') ivcomp = 1                                 
40930 if (ivcomp - 1) 20930, 10930, 20930                               
30930 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10930, 0941, 20930                                    
10930 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0941                                                        
20930 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0941 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 094  ****                         
!                                                                        
!                                                                        
      ivtnum =  94                                                      
      if (iczero) 30940, 0940, 30940                                    
 0940 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn03 = 'ABCDEFGHIJKL'                                           
      cvtn09 = '            '                                           
      cvtn09 = cvtn03                                                   
      if (cvtn09  ==  'ABCDEFGHIJKL') ivcomp = 1                        
40940 if (ivcomp - 1) 20940, 10940, 20940                               
30940 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10940, 0951, 20940                                    
10940 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0951                                                        
20940 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0951 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 095  ****                         
!                                                                        
!                                                                        
      ivtnum =  95                                                      
      if (iczero) 30950, 0950, 30950                                    
 0950 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn04 = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                              
      cvtn10 = '                         '                              
      cvtn10 = cvtn04                                                   
      if (cvtn10  ==  'ABCDEFGHIJKLMNOPQRSTUVWXY') ivcomp = 1           
40950 if (ivcomp - 1) 20950, 10950, 20950                               
30950 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10950, 0961, 20950                                    
10950 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0961                                                        
20950 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0961 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 096  ****                         
!                                                                        
!                                                                        
      ivtnum =  96                                                      
      if (iczero) 30960, 0960, 30960                                    
 0960 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn12 = '   '                                                    
      cvtn06 =                                                             'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE'    
      cvtn12 = cvtn06                                                   
      if (cvtn12  ==                                                       'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE')        ivcomp = 1                                                   
40960 if (ivcomp - 1) 20960, 10960, 20960                               
30960 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10960, 0971, 20960                                    
10960 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0971                                                        
20960 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0971 continue                                                          
!                                                                        
!           TEST 97 AND TEST 98 VERIFY THE CHARACTER ASSIGNMENT          
!      STATEMENT                                                         
!                                                                        
!           CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                 
!                                                                        
!      IS CORRECT.  THE ARRAY ELEMENT AND CONSTANT ARE THE SAME LENGTH,  
!      AND THE LENGTHS 25 AND 41 ARE USED IN THESE TESTS.                
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 097  ****                         
!                                                                        
!                                                                        
      ivtnum =  97                                                      
      if (iczero) 30970, 0970, 30970                                    
 0970 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn14(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                           
      if (catn14(1)  ==  'ABCDEFGHIJKLMNOPQRSTUVWXY') ivcomp = 1        
40970 if (ivcomp - 1) 20970, 10970, 20970                               
30970 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10970, 0981, 20970                                    
10970 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0981                                                        
20970 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0981 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 098  ****                         
!                                                                        
!                                                                        
      ivtnum =  98                                                      
      if (iczero) 30980, 0980, 30980                                    
 0980 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn15(8) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'           
      if (catn15(8)  ==                                                    'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO') ivcomp = 1        
40980 if (ivcomp - 1) 20980, 10980, 20980                               
30980 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10980, 0991, 20980                                    
10980 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0991                                                        
20980 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0991 continue                                                          
!                                                                        
!           TEST 99 AND TEST 100 VERIFY THE CHARACTER ASSIGNMENT         
!      STATEMENTS                                                        
!                                                                        
!               CHARACTER VARIABLE = CHARACTER CONSTANT                  
!               CHARACTER ARRAY ELEMENT = CHARACTER VARIABLE             
!                                                                        
!      ARE CORRECT.  THE CHARACTER ENTITIES ARE THE SAME LENGTH,         
!      AND THE LENGTHS 3 AND 57 ARE USED IN THESE TESTS.                 
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 099  ****                         
!                                                                        
!                                                                        
      ivtnum =  99                                                      
      if (iczero) 30990, 0990, 30990                                    
 0990 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn01 = 'ABC'                                                    
      catn11(5) = cvtn01                                                
      if (catn11(5)  ==  'ABC') ivcomp = 1                              
40990 if (ivcomp - 1) 20990, 10990, 20990                               
30990 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10990, 1001, 20990                                    
10990 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1001                                                        
20990 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1001 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 100  ****                         
!                                                                        
!                                                                        
      ivtnum = 100                                                      
      if (iczero) 31000, 1000, 31000                                    
 1000 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn06 =                                                             'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE'    
      catn16(3) = cvtn06                                                
      if (catn16(3)  ==                                                    'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDE')        ivcomp = 1                                                   
41000 if (ivcomp - 1) 21000, 11000, 21000                               
31000 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11000, 1011, 21000                                    
11000 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1011                                                        
21000 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1011 continue                                                          
!                                                                        
!           TEST 101 AND TEST 102 VERIFY THE CHARACTER ASSIGNMENT        
!      STATEMENTS                                                        
!                                                                        
!           CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                 
!           CHARACTER ARRAY ELEMENT = CHARACTER ARRAY ELEMENT            
!                                                                        
!      ARE CORRECT.  THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND     
!      THE LENGTHS 7 AND 41 ARE USED IN THESE TESTS.                     
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 101  ****                         
!                                                                        
!                                                                        
      ivtnum = 101                                                      
      if (iczero) 31010, 1010, 31010                                    
 1010 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn12(3) = 'ABCDEFG'                                             
      catn12(4) = catn12(3)                                             
      if (catn12(4)  ==  'ABCDEFG') ivcomp = 1                          
41010 if (ivcomp - 1) 21010, 11010, 21010                               
31010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11010, 1021, 21010                                    
11010 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1021                                                        
21010 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 102  ****                         
!                                                                        
!                                                                        
      ivtnum = 102                                                      
      if (iczero) 31020, 1020, 31020                                    
 1020 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn15(3) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'           
      catn15(4) = catn15(3)                                             
      if (catn15(4)  ==                                                    'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO') ivcomp = 1        
41020 if (ivcomp - 1) 21020, 11020, 21020                               
31020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11020, 1031, 21020                                    
11020 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1031                                                        
21020 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1031 continue                                                          
!                                                                        
!           TEST 103 AND TEST 104 VERIFY THE CHARACTER ASSIGNMENT        
!      STATEMENTS                                                        
!                                                                        
!           CHARACTER ARRAY ELEMENT = CHARACTER CONSTANT                 
!           CHARACTER VARIABLE = CHARACTER ARRAY ELEMENT                 
!                                                                        
!      ARE CORRECT.  THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND     
!      THE LENGTHS 12 AND 25 ARE USED.                                   
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 103  ****                         
!                                                                        
!                                                                        
      ivtnum = 103                                                      
      if (iczero) 31030, 1030, 31030                                    
 1030 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn13(1) = 'ABCDEFGHIJKL'                                        
      cvtn09 = '            '                                           
      cvtn09 = catn13(1)                                                
      if (cvtn09  ==  'ABCDEFGHIJKL') ivcomp = 1                        
41030 if (ivcomp - 1) 21030, 11030, 21030                               
31030 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11030, 1041, 21030                                    
11030 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1041                                                        
21030 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1041 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 104  ****                         
!                                                                        
!                                                                        
      ivtnum = 104                                                      
      if (iczero) 31040, 1040, 31040                                    
 1040 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      catn14(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXY'                           
      cvtn10 = '                         '                              
      cvtn10 = catn14(1)                                                
      if (cvtn10  ==  'ABCDEFGHIJKLMNOPQRSTUVWXY') ivcomp = 1           
41040 if (ivcomp - 1) 21040, 11040, 21040                               
31040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11040, 1051, 21040                                    
11040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1051                                                        
21040 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1051 continue                                                          
!                                                                        
!           TEST 105 THROUGH TEST 110 VERIFY THE CHARACTER RELATIONAL    
!      EXPRESSION USING EACH OF THE SIX RELATIONAL OPERATORS IN THE      
!      STATEMENT FORM                                                    
!                                                                        
!           CHARACTER VARIABLE RELOP CHARACTER CONSTANT                  
!                                                                        
!      THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND THE LENGTHS       
!      3, 7, 12, 25, 41, AND 57 ARE USED IN THESE TESTS.                 
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 105  ****                         
!                                                                        
!                                                                        
      ivtnum = 105                                                      
      if (iczero) 31050, 1050, 31050                                    
 1050 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn07 = 'ZAB'                                                    
      if (cvtn07  ==  'ZAB') ivcomp = 1                                 
41050 if (ivcomp - 1) 21050, 11050, 21050                               
31050 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11050, 1061, 21050                                    
11050 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1061                                                        
21050 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 106  ****                         
!                                                                        
!                                                                        
      ivtnum = 106                                                      
      if (iczero) 31060, 1060, 31060                                    
 1060 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn08 = 'ABDDEEF'                                                
      if (cvtn08  >  'ABCDEEF') ivcomp = 1                             
41060 if (ivcomp - 1) 21060, 11060, 21060                               
31060 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11060, 1071, 21060                                    
11060 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1071                                                        
21060 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1071 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 107  ****                         
!                                                                        
!                                                                        
      ivtnum = 107                                                      
      if (iczero) 31070, 1070, 31070                                    
 1070 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn09 = 'ZXYZZZABCDEF'                                           
      if (cvtn09  <  'ZXYZZZACCDEF') ivcomp = 1                        
41070 if (ivcomp - 1) 21070, 11070, 21070                               
31070 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11070, 1081, 21070                                    
11070 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1081                                                        
21070 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1081 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 108  ****                         
!                                                                        
!                                                                        
      ivtnum = 108                                                      
      if (iczero) 31080, 1080, 31080                                    
 1080 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn10 = 'ABCDEFGHIJKKMNOPQRSTUVWXY'                              
      if ('ABCDEFGHIJKLMNOPQRSTUVWXY'  /=  cvtn10) ivcomp = 1           
41080 if (ivcomp - 1) 21080, 11080, 21080                               
31080 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11080, 1091, 21080                                    
11080 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1091                                                        
21080 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1091 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 109  ****                         
!                                                                        
!                                                                        
      ivtnum = 109                                                      
      if (iczero) 31090, 1090, 31090                                    
 1090 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn11 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZAABCDEFGHIJKLMN'              
      if ('ABCDEFGHIJKLMNOPQRSTUVWXYZABBCDEFGHIJKLMN'  >=  cvtn11)           ivcomp = 1                                                   
41090 if (ivcomp - 1) 21090, 11090, 21090                               
31090 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11090, 1101, 21090                                    
11090 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1101                                                        
21090 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1101 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 110  ****                         
!                                                                        
!                                                                        
      ivtnum = 110                                                      
      if (iczero) 31100, 1100, 31100                                    
 1100 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      cvtn12 =                                                             'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZAAAAA'    
      if ('ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYAAAAAA'        <=  cvtn12) ivcomp = 1                                       
41100 if (ivcomp - 1) 21100, 11100, 21100                               
31100 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11100, 1111, 21100                                    
11100 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1111                                                        
21100 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1111 continue                                                          
!                                                                        
!           TEST 111 AND TEST 112 VERIFY THE CHARACTER RELATIONAL        
!      EXPRESSION OF THE FORM                                            
!                                                                        
!           CHARACTER VARIABLE RELOP CHARACTER VARIABLE                  
!                                                                        
!      THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND THE LENGTHS 3     
!      AND 41 ARE USED IN THESE TESTS.                                   
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 111  ****                         
!                                                                        
!                                                                        
      ivtnum = 111                                                      
      if (iczero) 31110, 1110, 31110                                    
 1110 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 3                                                        
      cvtn01 = 'ABC'                                                    
      cvtn07 = 'BBC'                                                    
      if (cvtn01  ==  cvtn07) ivcomp = ivcomp * 2                       
      if (cvtn01  /=  cvtn07) ivcomp = ivcomp * 3                       
41110 if (ivcomp - 3) 21110, 11110, 21110                               
31110 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11110, 1121, 21110                                    
11110 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1121                                                        
21110 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1121 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 112  ****                         
!                                                                        
!                                                                        
      ivtnum = 112                                                      
      if (iczero) 31120, 1120, 31120                                    
 1120 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 6                                                        
      cvtn05 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'              
      cvtn11 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNO'              
      if (cvtn05  >=  cvtn11) ivcomp = ivcomp * 2                       
      if (cvtn05  <=  cvtn11) ivcomp = ivcomp * 3                       
41120 if (ivcomp - 6) 21120, 11120, 21120                               
31120 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11120, 1131, 21120                                    
11120 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1131                                                        
21120 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1131 continue                                                          
!                                                                        
!           TEST 113 AND TEST 114 VERIFY THE CHARACTER RELATIONAL        
!      EXPRESSION OF THE FORM                                            
!                                                                        
!           CHARACTER ARRAY ELEMENT RELOP CHARACTER CONSTANT             
!                                                                        
!      THE CHARACTER ENTITIES ARE THE SAME LENGTH, AND THE LENGTHS 7 AND 
!      25 ARE USED IN THESE TESTS.                                       
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 113  ****                         
!                                                                        
!                                                                        
      ivtnum = 113                                                      
      if (iczero) 31130, 1130, 31130                                    
 1130 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 6                                                        
      catn12(3) = 'AB012CD'                                             
      if (catn12(3)  <  'AB013CD') ivcomp = ivcomp * 2                 
      if ('AB013CD'  >  catn12(3)) ivcomp = ivcomp * 3                 
41130 if (ivcomp - 6) 21130, 11130, 21130                               
31130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11130, 1141, 21130                                    
11130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1141                                                        
21130 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1141 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 114  ****                         
!                                                                        
!                                                                        
      ivtnum = 114                                                      
      if (iczero) 31140, 1140, 31140                                    
 1140 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 2                                                        
      catn14(1) = 'ABCDEFGHIJKLMNOPQRSTUVWXX'                           
      if (catn14(1)  /=  'ABCDEFGHIJKLMNOPQRSTUVWXY')                        ivcomp = ivcomp * 2                                          
      if (catn14(1)  ==  'ABCDEFGHIJKLMNOPQRSTUVWXY')                        ivcomp = ivcomp * 3                                          
41140 if (ivcomp - 2) 21140, 11140, 21140                               
31140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11140, 1151, 21140                                    
11140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1151                                                        
21140 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1151 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 115  ****                         
!                                                                        
!           TEST 115 VERIFIES THE CHARACTER RELATIONAL EXPRESSION        
!      OF THE FORM                                                       
!                                                                        
!           CHARACTER ARRAY ELEMENT RELOP CHARACTER VARIABLE             
!                                                                        
!      THE CHARACTER ENTITIES ARE 12 CHARACTERS IN LENGTH.               
!                                                                        
      ivtnum = 115                                                      
      if (iczero) 31150, 1150, 31150                                    
 1150 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 2                                                        
      catn13(3) = 'ABC+AAB/CDDF'                                        
      if (catn13(3)  <  'BBC+AAB/CCCC') ivcomp = ivcomp * 2            
      if (catn13(3)  >  'BBC+AAB/CCCC') ivcomp = ivcomp * 3            
41150 if (ivcomp - 2) 21150, 11150, 21150                               
31150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11150, 1161, 21150                                    
11150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1161                                                        
21150 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1161 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 205  -  TEST 116  ****                         
!                                                                        
!           TEST 116 VERIFIES THE CHARACTER RELATIONAL EXPRESSION        
!      OF THE FORM                                                       
!                                                                        
!           CHARACTER ARRAY ELEMENT RELOP CHARACTER ARRAY ELEMENT        
!                                                                        
!      THE CHARACTER ENTITIES ARE 57 CHARACTERS IN LENGTH.               
!                                                                        
      ivtnum = 116                                                      
      if (iczero) 31160, 1160, 31160                                    
 1160 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 30                                                       
      catn16(1) =                                                          'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ//012'    
      catn16(2) =                                                          'ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ//112'    
      if (catn16 (1)  <  catn16 (2)) ivcomp = ivcomp * 2               
      if (catn16 (1)  /=  catn16 (2)) ivcomp = ivcomp * 3               
      if (catn16 (1)  <=  catn16 (2)) ivcomp = ivcomp * 5               
41160 if (ivcomp - 30) 21160, 11160, 21160                              
31160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 11160, 1171, 21160                                    
11160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 1171                                                        
21160 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 1171 continue                                                          
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
90001 format (" ",24x,"FM205")                                          
90000 format (" ",20x,"END OF PROGRAM FM205" )                          
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
      end program fm205
