      program fm016
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM016                                                             
!                                                                        
!              THIS ROUTINE BEGINS A SERIES OF TESTS  OF THE FORTRAN     
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
      logical :: lctnt1
      logical :: lctnf1
      logical :: lvtntf
      logical :: lvtnft
      logical, dimension(1:2) :: latn1a
      logical, dimension(1:2) :: ladn1d
      logical, dimension(1:2) :: ladn1b
      data ladn1d / .true.,.false. / 
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
      ivtnum = 139                                                      
!      TEST 139  -  THIS TESTS THE LOGICAL CONSTANT  .TRUE.              
!                                                                        
      if (iczero) 31390, 1390, 31390                                    
 1390 continue                                                          
      ivon01=0                                                          
      if ( .true. ) ivon01 = 1                                          
      goto 41390                                                       
31390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41390, 1401, 41390                                    
41390 if ( ivon01 - 1 )  21390, 11390, 21390                            
11390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1401                                                        
21390 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1401 continue                                                          
      ivtnum = 140                                                      
!      TEST 140  -  THIS TESTS THE LOGICAL CONSTANT  .FALSE.             
!                                                                        
      if (iczero) 31400, 1400, 31400                                    
 1400 continue                                                          
      ivon01=1                                                          
      if ( .false. ) ivon01=0                                           
      goto 41400                                                       
31400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41400, 1411, 41400                                    
41400 if ( ivon01 - 1 )  21400, 11400, 21400                            
11400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1411                                                        
21400 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1411 continue                                                          
      ivtnum = 141                                                      
!      TEST 141  -  THIS TESTS THE LOGICAL VARIABLE = .TRUE.             
!                                                                        
      if (iczero) 31410, 1410, 31410                                    
 1410 continue                                                          
      lctnt1=.true.                                                     
      ivon01 = 0                                                        
      if ( lctnt1 )  ivon01 = 1                                         
      goto 41410                                                       
31410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41410, 1421, 41410                                    
41410 if ( ivon01 - 1 )  21410, 11410, 21410                            
11410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1421                                                        
21410 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1421 continue                                                          
      ivtnum = 142                                                      
!      TEST 142  -  THIS TESTS THE LOGICAL VARIABLE =  .FALSE.           
!                                                                        
      if (iczero) 31420, 1420, 31420                                    
 1420 continue                                                          
      ivon01=1                                                          
      lctnf1=.false.                                                    
      if ( lctnf1 )  ivon01=0                                           
      goto 41420                                                       
31420 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41420, 1431, 41420                                    
41420 if ( ivon01 - 1 )  21420, 11420, 21420                            
11420 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1431                                                        
21420 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1431 continue                                                          
      ivtnum = 143                                                      
!      TEST 143  -  THIS TESTS CHANGING THE VALUE OF A LOGICAL VARIABLE  
!            FROM .TRUE.  TO  .FALSE.                                    
!                                                                        
      if (iczero) 31430, 1430, 31430                                    
 1430 continue                                                          
      lvtntf=.true.                                                     
      lvtntf=.false.                                                    
      ivon01 = 1                                                        
      if ( lvtntf )  ivon01 = 0                                         
      goto 41430                                                       
31430 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41430, 1441, 41430                                    
41430 if ( ivon01 - 1 )  21430, 11430, 21430                            
11430 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1441                                                        
21430 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1441 continue                                                          
      ivtnum = 144                                                      
!      TEST 144  -  THIS TESTS CHANGING THE VALUE OF A LOGICAL VARIABLE  
!            FROM  .FALSE.  TO  .TRUE.                                   
!                                                                        
      if (iczero) 31440, 1440, 31440                                    
 1440 continue                                                          
      lvtnft=.false.                                                    
      lvtnft=.true.                                                     
      ivon01=0                                                          
      if ( lvtnft )  ivon01=1                                           
      goto 41440                                                       
31440 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41440, 1451, 41440                                    
41440 if ( ivon01 - 1 )  21440, 11440, 21440                            
11440 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1451                                                        
21440 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1451 continue                                                          
      ivtnum = 145                                                      
!      TEST 145  -  TEST OF A LOGICAL ARRAY ELEMENT SET TO  .TRUE.       
!                                                                        
      if (iczero) 31450, 1450, 31450                                    
 1450 continue                                                          
      latn1a(1)=.true.                                                  
      ivon01=0                                                          
      if ( latn1a(1) )  ivon01=1                                        
      goto 41450                                                       
31450 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41450, 1461, 41450                                    
41450 if ( ivon01 - 1 )  21450, 11450, 21450                            
11450 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1461                                                        
21450 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1461 continue                                                          
      ivtnum = 146                                                      
!      TEST 146  -  TEST OF A LOGICAL ARRAY ELEMENT SET TO  .FALSE.      
!                                                                        
      if (iczero) 31460, 1460, 31460                                    
 1460 continue                                                          
      latn1a(2) = .false.                                               
      ivon01=1                                                          
      if ( latn1a(2) )  ivon01=0                                        
      goto 41460                                                       
31460 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41460, 1471, 41460                                    
41460 if ( ivon01 - 1 )  21460, 11460, 21460                            
11460 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1471                                                        
21460 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1471 continue                                                          
      ivtnum = 147                                                      
!      TEST 147  -  TEST OF A LOGICAL ARRAY ELEMENT SET  .TRUE.          
!            IN A DATA INITIALIZATION STATEMENT.                         
!                                                                        
      if (iczero) 31470, 1470, 31470                                    
 1470 continue                                                          
      ivon01=0                                                          
      if ( ladn1d(1) )  ivon01=1                                        
      goto 41470                                                       
31470 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41470, 1481, 41470                                    
41470 if ( ivon01 - 1 )  21470, 11470, 21470                            
11470 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1481                                                        
21470 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1481 continue                                                          
      ivtnum = 148                                                      
!      TEST 148  -  TEST OF A LOGICAL ARRAY ELEMENT SET  .FALSE.         
!            IN A DATA INITIALIZATION STATEMENT.                         
!                                                                        
      if (iczero) 31480, 1480, 31480                                    
 1480 continue                                                          
      ivon01=1                                                          
      if ( ladn1d(2) )  ivon01=0                                        
      goto 41480                                                       
31480 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41480, 1491, 41480                                    
41480 if ( ivon01 - 1 )  21480, 11480, 21480                            
11480 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1491                                                        
21480 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1491 continue                                                          
      ivtnum = 149                                                      
!      TEST 149  -  LIKE TEST 145 EXCEPT THAT THE ARRAY DECLARATION WAS  
!            IN A DIMENSION STATEMENT RATHER THAN IN THE TYPE STATEMENT. 
!                                                                        
      if (iczero) 31490, 1490, 31490                                    
 1490 continue                                                          
      ladn1b(1)=.true.                                                  
      ivon01=0                                                          
      if ( ladn1b(1) )  ivon01=1                                        
      goto 41490                                                       
31490 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41490, 1501, 41490                                    
41490 if ( ivon01 - 1 )  21490, 11490, 21490                            
11490 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1501                                                        
21490 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!            FOR TESTS 150 THRU 156  THE TRUE PATH IS USED..             
!                                                                        
 1501 continue                                                          
      ivtnum = 150                                                      
!      TEST 150  -  RELATIONAL EXPRESSION WITH INTEGER CONSTANTS  .LT.   
!                                                                        
      if (iczero) 31500, 1500, 31500                                    
 1500 continue                                                          
      ivon01=0                                                          
      if ( 3  <  76 )  ivon01=1                                        
      goto 41500                                                       
31500 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41500, 1511, 41500                                    
41500 if ( ivon01 - 1 )  21500, 11500, 21500                            
11500 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1511                                                        
21500 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1511 continue                                                          
      ivtnum = 151                                                      
!      TEST 151  -  TEST WITH RELATIONAL EXPRESSION  .LE.                
!                                                                        
      if (iczero) 31510, 1510, 31510                                    
 1510 continue                                                          
      ivon01=0                                                          
      if ( 587  <=  587 )  ivon01=1                                     
      goto 41510                                                       
31510 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41510, 1521, 41510                                    
41510 if ( ivon01 - 1 )  21510, 11510, 21510                            
11510 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1521                                                        
21510 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1521 continue                                                          
      ivtnum = 152                                                      
!      TEST 152  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 
!            RELATIONAL OPERATOR IS  .EQ.                                
!                                                                        
      if (iczero) 31520, 1520, 31520                                    
 1520 continue                                                          
      ivon01=0                                                          
      if ( 9999  ==  9999 )  ivon01=1                                   
      goto 41520                                                       
31520 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41520, 1531, 41520                                    
41520 if ( ivon01 - 1 )  21520, 11520, 21520                            
11520 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1531                                                        
21520 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1531 continue                                                          
      ivtnum = 153                                                      
!      TEST 153  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 
!            RELATIONAL OPERATOR IS  .NE.                                
!                                                                        
      if (iczero) 31530, 1530, 31530                                    
 1530 continue                                                          
      ivon01=0                                                          
      if ( 0  /=  32767 )  ivon01=1                                     
      goto 41530                                                       
31530 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41530, 1541, 41530                                    
41530 if ( ivon01 - 1 )  21530, 11530, 21530                            
11530 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1541                                                        
21530 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1541 continue                                                          
      ivtnum = 154                                                      
!      TEST 154  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 
!            RELATIONAL OPERATOR IS  .GT.                                
!                                                                        
      if (iczero) 31540, 1540, 31540                                    
 1540 continue                                                          
      ivon01=0                                                          
      if ( 32767  >  76 )  ivon01=1                                    
      goto 41540                                                       
31540 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41540, 1551, 41540                                    
41540 if ( ivon01 - 1 )  21540, 11540, 21540                            
11540 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1551                                                        
21540 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1551 continue                                                          
      ivtnum = 155                                                      
!      TEST 155  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 
!            RELATIONAL OPERATOR IS  .GE.                                
!                                                                        
      if (iczero) 31550, 1550, 31550                                    
 1550 continue                                                          
      ivon01=0                                                          
      if ( 32767  >=  76 )  ivon01=1                                    
      goto 41550                                                       
31550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41550, 1561, 41550                                    
41550 if ( ivon01 - 1 )  21550, 11550, 21550                            
11550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1561                                                        
21550 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1561 continue                                                          
      ivtnum = 156                                                      
!      TEST 156  -  TEST OF RELATIONAL EXPRESSION WITH INTEGER CONSTANTS 
!            RELATIONAL OPERATOR IS  .GE.                                
!                                                                        
      if (iczero) 31560, 1560, 31560                                    
 1560 continue                                                          
      ivon01=0                                                          
      if ( 32767  >=  32767 )  ivon01=1                                 
      goto 41560                                                       
31560 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41560, 1571, 41560                                    
41560 if ( ivon01 - 1 )  21560, 11560, 21560                            
11560 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1571                                                        
21560 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
!                                                                        
!            FOR TESTS 157 THRU 162 THE FALSE PATH IS USED..             
!                                                                        
 1571 continue                                                          
      ivtnum = 157                                                      
!      TEST 157  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   
!            RELATIONAL OPERATOR IS  .LT.                                
!                                                                        
      if (iczero) 31570, 1570, 31570                                    
 1570 continue                                                          
      ivon01=1                                                          
      if ( 76  <  3 )  ivon01=0                                        
      goto 41570                                                       
31570 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41570, 1581, 41570                                    
41570 if ( ivon01 - 1 )  21570, 11570, 21570                            
11570 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1581                                                        
21570 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1581 continue                                                          
      ivtnum = 158                                                      
!      TEST 158  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   
!            RELATIONAL OPERATOR IS  .LE.                                
!                                                                        
      if (iczero) 31580, 1580, 31580                                    
 1580 continue                                                          
      ivon01=1                                                          
      if ( 76  <=  3 )  ivon01=0                                        
      goto 41580                                                       
31580 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41580, 1591, 41580                                    
41580 if ( ivon01 - 1 )  21580, 11580, 21580                            
11580 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1591                                                        
21580 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1591 continue                                                          
      ivtnum = 159                                                      
!      TEST 159  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   
!            RELATIONAL OPERATOR IS  .EQ.                                
!                                                                        
      if (iczero) 31590, 1590, 31590                                    
 1590 continue                                                          
      ivon01=1                                                          
      if (  9999  ==  587 ) ivon01=0                                    
      goto 41590                                                       
31590 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41590, 1601, 41590                                    
41590 if ( ivon01 - 1 )  21590, 11590, 21590                            
11590 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1601                                                        
21590 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1601 continue                                                          
      ivtnum = 160                                                      
!      TEST 160  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   
!            RELATIONAL OPERATOR IS  .NE.                                
!                                                                        
      if (iczero) 31600, 1600, 31600                                    
 1600 continue                                                          
      ivon01=1                                                          
      if (  3  /=  3 )  ivon01=0                                        
      goto 41600                                                       
31600 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41600, 1611, 41600                                    
41600 if ( ivon01 - 1 )  21600, 11600, 21600                            
11600 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1611                                                        
21600 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1611 continue                                                          
      ivtnum=161                                                        
!                                                                        
!      TEST 161  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   
!            RELATIONAL OPERATOR IS  .GT.                                
!                                                                        
      if ( iczero )  31610, 1610, 31610                                 
 1610 continue                                                          
      ivon01=1                                                          
      if ( 76  >  32767 )  ivon01=0                                    
      goto 41610                                                       
31610 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if ( iczero )  41610, 1621, 41610                                 
41610 if ( ivon01 - 1 )  21610, 11610, 21610                            
11610 ivpass = ivpass+ 1                                                
      write (i02,80001) ivtnum                                          
      goto 1621                                                        
21610 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 1621 continue                                                          
      ivtnum = 162                                                      
!                                                                        
!                                                                        
!       ****  TEST 162  ****                                             
!                                                                        
!      TEST 162  -  RELATIONAL EXPRESSION INTEGER CONSTANTS FALSE PATH   
!            RELATIONAL OPERATOR IS  .GE.                                
!                                                                        
      if (iczero) 31620, 1620, 31620                                    
 1620 continue                                                          
      ivon01=1                                                          
      if ( 76  >=  32767 )  ivon01 = 0                                  
      goto 41620                                                       
31620 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41620, 1631, 41620                                    
41620 if ( ivon01 - 1 )  21620, 11620, 21620                            
11620 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1631                                                        
21620 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1631 continue                                                          
      ivtnum = 163                                                      
!                                                                        
!       ****  TEST 163  ****                                             
!      TEST 163  -  RELATIONAL EXPRESSION WITH INTEGER VARIABLE          
!            REFERENCES  (IC)  (RO)  (IVR).   TRUE PATH.  USE  .LT.      
!                                                                        
!                                                                        
      if (iczero) 31630, 1630, 31630                                    
 1630 continue                                                          
      ivon01 = 76                                                       
      ivon02 = 0                                                        
      if ( 3  <  ivon01 )  ivon02 = 1                                  
      goto 41630                                                       
31630 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41630, 1641, 41630                                    
41630 if ( ivon02 - 1 )  21630, 11630, 21630                            
11630 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1641                                                        
21630 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1641 continue                                                          
      ivtnum = 164                                                      
!                                                                        
!       ****  TEST 164  ****                                             
!      TEST 164  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCES. 
!            TRUE PATH.  .LE.                                            
!                                                                        
!                                                                        
      if (iczero) 31640, 1640, 31640                                    
 1640 continue                                                          
      ivon01 = 587                                                      
      ivon02 = 0                                                        
      if ( 587  <=  ivon01 )  ivon02 = 1                                
      goto 41640                                                       
31640 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41640, 1651, 41640                                    
41640 if ( ivon02 - 1 )  21640, 11640, 21640                            
11640 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1651                                                        
21640 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1651 continue                                                          
      ivtnum = 165                                                      
!                                                                        
!       ****  TEST 165  ****                                             
!      TEST 165  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            TRUE PATH.  .EQ.                                            
!                                                                        
!                                                                        
      if (iczero) 31650, 1650, 31650                                    
 1650 continue                                                          
      ivon01 = 9999                                                     
      ivon02 = 0                                                        
      if ( 9999  ==  ivon01 )  ivon02 = 1                               
      goto 41650                                                       
31650 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41650, 1661, 41650                                    
41650 if ( ivon02 - 1 )  21650, 11650, 21650                            
11650 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1661                                                        
21650 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1661 continue                                                          
      ivtnum = 166                                                      
!                                                                        
!       ****  TEST 166  ****                                             
!      TEST 166  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            TRUE PATH.  .NE.                                            
!                                                                        
!                                                                        
      if (iczero) 31660, 1660, 31660                                    
 1660 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 0                                                        
      if ( 0  /=  ivon01 )  ivon02 = 1                                  
      goto 41660                                                       
31660 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41660, 1671, 41660                                    
41660 if ( ivon02 - 1 )  21660, 11660, 21660                            
11660 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1671                                                        
21660 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1671 continue                                                          
      ivtnum = 167                                                      
!                                                                        
!       ****  TEST 167  ****                                             
!      TEST 167  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            TRUE PATH.  .GT.                                            
!                                                                        
!                                                                        
      if (iczero) 31670, 1670, 31670                                    
 1670 continue                                                          
      ivon01 = 76                                                       
      ivon02 = 0                                                        
      if ( 32767  >  ivon01 )  ivon02 = 1                              
      goto 41670                                                       
31670 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41670, 1681, 41670                                    
41670 if ( ivon02 - 1 )  21670, 11670, 21670                            
11670 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1681                                                        
21670 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1681 continue                                                          
      ivtnum = 168                                                      
!                                                                        
!       ****  TEST 168  ****                                             
!      TEST 168  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            TRUE PATH.  .GE.                                            
!                                                                        
!                                                                        
      if (iczero) 31680, 1680, 31680                                    
 1680 continue                                                          
      ivon01 = 76                                                       
      ivon02 = 0                                                        
      if ( 32767  >=  ivon01 )  ivon02 = 1                              
      goto 41680                                                       
31680 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41680, 1691, 41680                                    
41680 if ( ivon02 - 1 )  21680, 11680, 21680                            
11680 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1691                                                        
21680 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1691 continue                                                          
      ivtnum = 169                                                      
!                                                                        
!       ****  TEST 169  ****                                             
!      TEST 169  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            TRUE PATH.  .EQ.                                            
!                                                                        
!                                                                        
      if (iczero) 31690, 1690, 31690                                    
 1690 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 0                                                        
      if ( 32767  ==  ivon01 )  ivon02 = 1                              
      goto 41690                                                       
31690 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41690, 1701, 41690                                    
41690 if ( ivon02 - 1 )  21690, 11690, 21690                            
11690 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1701                                                        
21690 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1701 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM016" )                          
      end program fm016
