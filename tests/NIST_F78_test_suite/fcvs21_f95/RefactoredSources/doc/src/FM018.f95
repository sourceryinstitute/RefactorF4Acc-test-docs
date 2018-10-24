      program fm018
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM018                                                             
!                                                                        
!              THIS ROUTINE CONTINUES TESTS OF THE FORTRAN               
!      LOGICAL    IF STATEMENT IN ALL OF THE VARIOUS FORMS.    THE       
!      FOLLOWING LOGICAL OPERANDS ARE USED FOR THIS ROUTINE - LOGICAL    
!      CONSTANTS, LOGICAL VARIABLES, LOGICAL ARRAY ELEMENTS, AND         
!      ARITHMETIC EXPRESSIONS WITH VARIOUS RELATIONAL OPERATORS.  BOTH   
!      THE TRUE AND FALSE BRANCHES ARE TESTED IN THE SERIES OF TESTS.    
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.7.1, LOGICAL CONSTANT                                
!         SECTION 6, EXPRESSIONS                                         
!         SECTION 6.1, ARITHMETIC EXPRESSIONS                            
!         SECTION 6.3, RELATIONAL EXPRESSIONS                            
!         SECTION 6.4, LOGICAL EXPRESSIONS                               
!         SECTION 6.6, EVALUATION OF EXPRESSIONS                         
!         SECTION 10, ASSIGNMENT STATEMENTS                              
!         SECTION 10.2, LOGICAL ASSIGNMENT STATEMENT                     
!         SECTION 11.5, LOGICAL IF STATEMENT                             
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      logical :: lctnt1
      logical :: lctnt2
      logical, dimension(1:2) :: latn1a
      integer, dimension(1:2) :: iadn11
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
      ivtnum = 500                                                      
!                                                                        
!       ****  TEST 500  ****                                             
!      TEST 500  -  LIKE TEST 197.  TRUE  .OR.  TRUE    TRUE PATH        
!            TEST OF THE FORTRAN INCLUSIVE OR  (LE)  .OR.  (LT)          
!                                                                        
!                                                                        
      if (iczero) 35000, 5000, 35000                                    
 5000 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .true.                                                   
      if ( lctnt1 .or. lctnt2 )  ivon01 = 1                             
      goto 45000                                                       
35000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45000, 5011, 45000                                    
45000 if ( ivon01 - 1 )  25000, 15000, 25000                            
15000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5011                                                        
25000 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5011 continue                                                          
      ivtnum = 501                                                      
!                                                                        
!       ****  TEST 501  ****                                             
!      TEST 501  -  TEST OF PARENTHESES AROUND A LOGICAL EXPRESSION      
!            (  (LE)  )  .OR.  (LT)                                      
!            USES LOGICAL VARIABLES SET IN LOGICAL ASSIGNMENT  STATEMENTS
!            ( FALSE )  .OR.  FALSE    FALSE PATH                        
!                                                                        
!                                                                        
      if (iczero) 35010, 5010, 35010                                    
 5010 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .false.                                                  
      if ( (lctnt1) .or. lctnt2 )  ivon01 = 0                           
      goto 45010                                                       
35010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45010, 5021, 45010                                    
45010 if ( ivon01 - 1 )  25010, 15010, 25010                            
15010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5021                                                        
25010 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5021 continue                                                          
      ivtnum = 502                                                      
!                                                                        
!       ****  TEST 502  ****                                             
!      TEST 502  -  LIKE TEST 501 EXCEPT THAT IT IT IS OF THE FORM       
!            (LE)  .OR.  ( (LT) )        TRUE  .OR.  (TRUE)              
!            TRUE PATH                                                   
!                                                                        
!                                                                        
      if (iczero) 35020, 5020, 35020                                    
 5020 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .true.                                                   
      if ( lctnt1 .or. ( lctnt2 ) )   ivon01 = 1                        
      goto 45020                                                       
35020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45020, 5031, 45020                                    
45020 if ( ivon01 - 1 )  25020, 15020, 25020                            
15020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5031                                                        
25020 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5031 continue                                                          
      ivtnum = 503                                                      
!                                                                        
!       ****  TEST 503  ****                                             
!      TEST 503  -  TEST OF PARENTHESES IN LOGICAL EXPRESSIONS           
!            (  (LE)  )  .OR.  (  (LT)  )                                
!            (FALSE) .OR. (TRUE)    TRUE PATH                            
!                                                                        
!                                                                        
      if (iczero) 35030, 5030, 35030                                    
 5030 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .true.                                                   
      if ( (lctnt1) .or. (lctnt2) )  ivon01 = 1                         
      goto 45030                                                       
35030 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45030, 5041, 45030                                    
45030 if ( ivon01 - 1 )  25030, 15030, 25030                            
15030 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5041                                                        
25030 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5041 continue                                                          
      ivtnum = 504                                                      
!                                                                        
!       ****  TEST 504  ****                                             
!      TEST 504  -  LIKE TEST 503 ONLY MORE PARENTHESES   TRUE PATH      
!                                                                        
!                                                                        
      if (iczero) 35040, 5040, 35040                                    
 5040 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .false.                                                  
      if ( ( (lctnt1) .or. (lctnt2) ) )  ivon01 = 1                     
      goto 45040                                                       
35040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45040, 5051, 45040                                    
45040 if ( ivon01 - 1 )  25040, 15040, 25040                            
15040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5051                                                        
25040 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5051 continue                                                          
      ivtnum = 505                                                      
!                                                                        
!       ****  TEST 505  ****                                             
!      TEST 505  -  TEST OF PARENTHESES WITH .AND.  FALSE PATH           
!                                                                        
!                                                                        
      if (iczero) 35050, 5050, 35050                                    
 5050 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .false.                                                  
      if ( (lctnt1) .and. lctnt2 )  ivon01 = 0                          
      goto 45050                                                       
35050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45050, 5061, 45050                                    
45050 if ( ivon01 - 1 )  25050, 15050, 25050                            
15050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5061                                                        
25050 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5061 continue                                                          
      ivtnum = 506                                                      
!                                                                        
!       ****  TEST 506  ****                                             
!      TEST 506  -  LIKE TEST 505  FALSE PATH                            
!                                                                        
!                                                                        
      if (iczero) 35060, 5060, 35060                                    
 5060 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .true.                                                   
      if ( lctnt1 .and. (lctnt2) )  ivon01 = 0                          
      goto 45060                                                       
35060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45060, 5071, 45060                                    
45060 if ( ivon01 - 1 )  25060, 15060, 25060                            
15060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5071                                                        
25060 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5071 continue                                                          
      ivtnum = 507                                                      
!                                                                        
!       ****  TEST 507  ****                                             
!      TEST 507  -  MORE PARENTHESES WITH LOGICAL .AND.  FALSE PATH      
!                                                                        
!                                                                        
      if (iczero) 35070, 5070, 35070                                    
 5070 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .false.                                                  
      if ( (lctnt1) .and. (lctnt2) )  ivon01 = 0                        
      goto 45070                                                       
35070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45070, 5081, 45070                                    
45070 if ( ivon01 - 1 )  25070, 15070, 25070                            
15070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5081                                                        
25070 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5081 continue                                                          
      ivtnum = 508                                                      
!                                                                        
!       ****  TEST 508  ****                                             
!      TEST 508  -  TEST OF LOGICAL .NOT. WITH PARENTHESES AROUND A LOGIC
!            PRIMARY.  FOR THIS TEST A LOGICAL ARRAY ELEMENT IS USED AS  
!            THE LOGICAL PRIMARY.  .NOT. (FALSE)   TRUE PATH.            
!                                                                        
!                                                                        
      if (iczero) 35080, 5080, 35080                                    
 5080 continue                                                          
      ivon01 = 0                                                        
      latn1a(1) = .false.                                               
      if ( .not. (latn1a(1)) )  ivon01 = 1                              
      goto 45080                                                       
35080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45080, 5091, 45080                                    
45080 if ( ivon01 - 1 )  25080, 15080, 25080                            
15080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5091                                                        
25080 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5091 continue                                                          
      ivtnum = 509                                                      
!                                                                        
!       ****  TEST 509  ****                                             
!      TEST 509  -  LIKE TEST 508 EXCEPT THAT THE WHOLE EXPRESSION       
!            IS IN PARENTHESES.  FALSE PATH                              
!                                                                        
!                                                                        
      if (iczero) 35090, 5090, 35090                                    
 5090 continue                                                          
      ivon01 = 1                                                        
      latn1a(2) = .true.                                                
      if ( ( .not. (latn1a(2)) ) )  ivon01 = 0                          
      goto 45090                                                       
35090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45090, 5101, 45090                                    
45090 if ( ivon01 - 1 )  25090, 15090, 25090                            
15090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5101                                                        
25090 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5101 continue                                                          
      ivtnum = 510                                                      
!                                                                        
!       ****  TEST 510  ****                                             
!      TEST 510  -  INTEGER CONSTANT EXPONIENTATION                      
!            RELATIONAL EXPRESSION USING  .EQ.  TRUE PATH                
!                                                                        
!                                                                        
      if (iczero) 35100, 5100, 35100                                    
 5100 continue                                                          
      ivon01 = 0                                                        
      if ( 3 ** 3  ==  27 )  ivon01 = 1                                 
      goto 45100                                                       
35100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45100, 5111, 45100                                    
45100 if ( ivon01 - 1 )  25100, 15100, 25100                            
15100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5111                                                        
25100 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5111 continue                                                          
      ivtnum = 511                                                      
!                                                                        
!       ****  TEST 511  ****                                             
!      TEST 511  -  EXPONIENTIATION USING AN INTEGER VARIABLE            
!            RELATIONAL EXPRESSION USING  .NE.  FALSE PATH               
!                                                                        
!                                                                        
      if (iczero) 35110, 5110, 35110                                    
 5110 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 3                                                        
      if ( ivon02 ** 3  /=  27 )  ivon01 = 0                            
      goto 45110                                                       
35110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45110, 5121, 45110                                    
45110 if ( ivon01 - 1 )  25110, 15110, 25110                            
15110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5121                                                        
25110 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5121 continue                                                          
      ivtnum = 512                                                      
!                                                                        
!       ****  TEST 512  ****                                             
!      TEST 512  -  LIKE TEST 511  USES  .LE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35120, 5120, 35120                                    
 5120 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      if ( 3 ** ivon02  <=  27 )  ivon01 = 1                            
      goto 45120                                                       
35120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45120, 5131, 45120                                    
45120 if ( ivon01 - 1 )  25120, 15120, 25120                            
15120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5131                                                        
25120 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5131 continue                                                          
      ivtnum = 513                                                      
!                                                                        
!       ****  TEST 513  ****                                             
!      TEST 513  -  LIKE TEST 511 BUT USES ALL INTEGER VARIABLES         
!            RELATIONAL EXPRESSION USES  .LT.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35130, 5130, 35130                                    
 5130 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 3                                                        
      ivon03 = 27                                                       
      if ( ivon02 ** ivon02  <  ivon03 )  ivon01 = 0                   
      goto 45130                                                       
35130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45130, 5141, 45130                                    
45130 if ( ivon01 - 1 )  25130, 15130, 25130                            
15130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5141                                                        
25130 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5141 continue                                                          
      ivtnum = 514                                                      
!                                                                        
!       ****  TEST 514  ****                                             
!      TEST 514  -  LIKE TEST 511 BUT USES INTEGER ARRAY ELEMENTS        
!            RELATIONAL EXPRESSION USES .GE.  TRUE PATH                  
!                                                                        
!                                                                        
      if (iczero) 35140, 5140, 35140                                    
 5140 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      iadn11(1) = 3                                                     
      iadn11(2) = 27                                                    
      if ( iadn11(1) ** ivon02  >=  iadn11(2) )  ivon01 = 1             
      goto 45140                                                       
35140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45140, 5151, 45140                                    
45140 if ( ivon01 - 1 )  25140, 15140, 25140                            
15140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5151                                                        
25140 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5151 continue                                                          
      ivtnum = 515                                                      
!                                                                        
!       ****  TEST 515  ****                                             
!      TEST 515  -  LIKE TEST 514 BUT USES ALL INTEGER ARRAY ELEMENTS    
!            RELATIONAL EXPRESSION USES  .GT.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35150, 5150, 35150                                    
 5150 continue                                                          
      ivon01 = 1                                                        
      iadn11(1) = 3                                                     
      iadn11(2) = 27                                                    
      if ( iadn11(1) ** iadn11(1)  >  iadn11(2) )  ivon01 = 0          
      goto 45150                                                       
35150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45150, 5161, 45150                                    
45150 if ( ivon01 - 1 )  25150, 15150, 25150                            
15150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5161                                                        
25150 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5161 continue                                                          
      ivtnum = 516                                                      
!                                                                        
!       ****  TEST 516  ****                                             
!      TEST 516  -  TEST OF INTEGER MULTIPLICATION USING INTEGER         
!            CONSTANTS.  RELATIONAL EXPRESSION USES  .LT.  TRUE PATH     
!                                                                        
!                                                                        
      if (iczero) 35160, 5160, 35160                                    
 5160 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 587                                                      
      if ( 3 * 3  <  ivon02 )  ivon01 = 1                              
      goto 45160                                                       
35160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45160, 5171, 45160                                    
45160 if ( ivon01 - 1 )  25160, 15160, 25160                            
15160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5171                                                        
25160 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5171 continue                                                          
      ivtnum = 517                                                      
!                                                                        
!       ****  TEST 517  ****                                             
!      TEST 517  -  INTEGER MULTIPLICATION WITH INTEGER CONSTANTS,       
!            VARIABLES, AND ARRAY ELEMENTS.  RELATIONAL EXPRESSION USES  
!            .GT.  FALSE PATH                                            
!                                                                        
!                                                                        
      if (iczero) 35170, 5170, 35170                                    
 5170 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 32767                                                    
      iadn11(1) = 3                                                     
      if ( iadn11(1) * 587  >  ivon02 )  ivon01 = 0                    
      goto 45170                                                       
35170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45170, 5181, 45170                                    
45170 if ( ivon01 - 1 )  25170, 15170, 25170                            
15170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5181                                                        
25170 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5181 continue                                                          
      ivtnum = 518                                                      
!                                                                        
!       ****  TEST 518  ****                                             
!      TEST 518  -  INTEGER MULTIPLICATION AND EXPONIENTATION            
!            RELATIONAL EXPRESSION USES  .EQ.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35180, 5180, 35180                                    
 5180 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 3                                                        
      ivon03 = 27                                                       
      iadn11(2) = 3                                                     
      if ( iadn11(2) ** 2 * ivon02  ==  ivon03 )  ivon01 = 1            
      goto 45180                                                       
35180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45180, 5191, 45180                                    
45180 if ( ivon01 - 1 )  25180, 15180, 25180                            
15180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5191                                                        
25180 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5191 continue                                                          
      ivtnum = 519                                                      
!                                                                        
!       ****  TEST 519  ****                                             
!      TEST 519  -  INTEGER DIVISION.  RELATIONAL EXPRESSION  .NE.       
!            FALSE PATH                                                  
!                                                                        
!                                                                        
      if (iczero) 35190, 5190, 35190                                    
 5190 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 27                                                       
      iadn11(1) = 3                                                     
      if ( ivon02 / 9  /=  iadn11(1) )  ivon01 = 0                      
      goto 45190                                                       
35190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45190, 5201, 45190                                    
45190 if ( ivon01 - 1 )  25190, 15190, 25190                            
15190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5201                                                        
25190 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5201 continue                                                          
      ivtnum = 520                                                      
!                                                                        
!       ****  TEST 520  ****                                             
!      TEST 520  -  INTEGER VARIABLE DIVISION.  RELATIONAL EXPRESSION    
!            USES .GE.  TRUE PATH                                        
!                                                                        
!                                                                        
      if (iczero) 35200, 5200, 35200                                    
 5200 continue                                                          
      ivon01 = 0                                                        
      ivon02 = 32767                                                    
      ivon03 = 3                                                        
      ivon04 = 9999                                                     
      ivon05 = 587                                                      
      if ( ivon02 / ivon03  >=  ivon04 / ivon05 )  ivon01 = 1           
      goto 45200                                                       
35200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45200, 5211, 45200                                    
45200 if ( ivon01 - 1 )  25200, 15200, 25200                            
15200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5211                                                        
25200 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5211 continue                                                          
      ivtnum = 521                                                      
!                                                                        
!       ****  TEST 521  ****                                             
!      TEST 521  -  INTEGER DIVISION AND EXPONIENTATION                  
!            RELATIONAL EXPRESSION USES  .LT.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35210, 5210, 35210                                    
 5210 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 587                                                      
      ivon03 = 3                                                        
      iadn11(2) = 3                                                     
      if ( ivon02 / iadn11(2) ** 3  <  3 ** ivon03 / ivon02 ) ivon01 =0
      if ( ivon02 / iadn11(2) ** 3  <  3 ** ivon03 / ivon02 )  ivon01=0
      goto 45210                                                       
35210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45210, 5221, 45210                                    
45210 if ( ivon01 - 1 )  25210, 15210, 25210                            
15210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5221                                                        
25210 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5221 continue                                                          
      ivtnum = 522                                                      
!                                                                        
!       ****  TEST 522  ****                                             
!      TEST 522  -  TESTS 522 THRU 535 ARE TESTS OF SIGNED TERMS         
!            +(T)  ALSO  -(T)                                            
!            RELATIONAL EXPRESSION USES .GT.  TRUE PATH                  
!                                                                        
!                                                                        
      if (iczero) 35220, 5220, 35220                                    
 5220 continue                                                          
      ivon01 = 0                                                        
      if ( 3  >  -3 )  ivon01 = 1                                      
      goto 45220                                                       
35220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45220, 5231, 45220                                    
45220 if ( ivon01 - 1 )  25220, 15220, 25220                            
15220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5231                                                        
25220 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5231 continue                                                          
      ivtnum = 523                                                      
!                                                                        
!       ****  TEST 523  ****                                             
!      TEST 523  -  TEST OF SIGNED ZERO  .LT.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35230, 5230, 35230                                    
 5230 continue                                                          
      ivon01 = 1                                                        
      if ( 0  <  -0 )  ivon01 = 0                                      
      goto 45230                                                       
35230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45230, 5241, 45230                                    
45230 if ( ivon01 - 1 )  25230, 15230, 25230                            
15230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5241                                                        
25230 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5241 continue                                                          
      ivtnum = 524                                                      
!                                                                        
!       ****  TEST 524  ****                                             
!      TEST 524  -  TEST OF SIGNED ZERO  .LE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35240, 5240, 35240                                    
 5240 continue                                                          
      ivon01 = 0                                                        
      if ( 0  <=  -0 )  ivon01 = 1                                      
      goto 45240                                                       
35240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45240, 5251, 45240                                    
45240 if ( ivon01 - 1 )  25240, 15240, 25240                            
15240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5251                                                        
25240 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5251 continue                                                          
      ivtnum = 525                                                      
!                                                                        
!       ****  TEST 525  ****                                             
!      TEST 525  -  TEST OF SIGNED ZERO  .EQ.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35250, 5250, 35250                                    
 5250 continue                                                          
      ivon01 = 0                                                        
      if ( 0  ==  -0 )  ivon01 = 1                                      
      goto 45250                                                       
35250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45250, 5261, 45250                                    
45250 if ( ivon01 - 1 )  25250, 15250, 25250                            
15250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5261                                                        
25250 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5261 continue                                                          
      ivtnum = 526                                                      
!                                                                        
!       ****  TEST 526  ****                                             
!      TEST 526  -  TEST OF SIGNED ZERO  .NE.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35260, 5260, 35260                                    
 5260 continue                                                          
      ivon01 = 1                                                        
      if ( 0  /=  -0 )  ivon01 = 0                                      
      goto 45260                                                       
35260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45260, 5271, 45260                                    
45260 if ( ivon01 - 1 )  25260, 15260, 25260                            
15260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5271                                                        
25260 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5271 continue                                                          
      ivtnum = 527                                                      
!                                                                        
!       ****  TEST 527  ****                                             
!      TEST 527  -  TEST OF SIGNED ZERO  .GE.  TRUE PATH                 
!                                                                        
!                                                                        
      if (iczero) 35270, 5270, 35270                                    
 5270 continue                                                          
      ivon01 = 0                                                        
      if ( 0  >=  -0 )  ivon01 = 1                                      
      goto 45270                                                       
35270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45270, 5281, 45270                                    
45270 if ( ivon01 - 1 )  25270, 15270, 25270                            
15270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5281                                                        
25270 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5281 continue                                                          
      ivtnum = 528                                                      
!                                                                        
!       ****  TEST 528  ****                                             
!      TEST 528  -  TEST OF SIGNED ZERO  .GT.  FALSE PATH                
!                                                                        
!                                                                        
      if (iczero) 35280, 5280, 35280                                    
 5280 continue                                                          
      ivon01 = 1                                                        
      if ( 0  >  -0 )  ivon01 = 0                                      
      goto 45280                                                       
35280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45280, 5291, 45280                                    
45280 if ( ivon01 - 1 )  25280, 15280, 25280                            
15280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5291                                                        
25280 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5291 continue                                                          
      ivtnum = 529                                                      
!                                                                        
!       ****  TEST 529  ****                                             
!      TEST 529  -  TEST OF 32767 AND -32766  .GT.  TRUE PATH            
!                                                                        
!                                                                        
      if (iczero) 35290, 5290, 35290                                    
 5290 continue                                                          
      ivon01 = 0                                                        
      if ( 32767  >  -32766 )  ivon01 = 1                              
      goto 45290                                                       
35290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 45290, 5301, 45290                                    
45290 if ( ivon01 - 1 )  25290, 15290, 25290                            
15290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5301                                                        
25290 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5301 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM018" )                          
      end program fm018
