      program fm017
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM017                                                             
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
      integer :: ivon02
      integer :: ivcomp
      integer :: ivcorr
      integer, dimension(1:3) :: iadn11
      logical, dimension(1:2) :: latn1a
      logical :: lctnt1
      logical :: lctnt2
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
      ivtnum = 170                                                      
!                                                                        
!       ****  TEST 170  ****                                             
!      TEST 170  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            FALSE PATH.  .LT.                                           
!                                                                        
!                                                                        
      if (iczero) 31700, 1700, 31700                                    
 1700 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 1                                                        
      if ( 76  <  ivon01 )  ivon02 = 0                                 
      goto 41700                                                       
31700 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41700, 1711, 41700                                    
41700 if ( ivon02 - 1 )  21700, 11700, 21700                            
11700 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1711                                                        
21700 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1711 continue                                                          
      ivtnum = 171                                                      
!                                                                        
!       ****  TEST 171  ****                                             
!      TEST 171  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            FALSE PATH.  .LE.                                           
!                                                                        
!                                                                        
      if (iczero) 31710, 1710, 31710                                    
 1710 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 1                                                        
      if ( 76  <=  ivon01 )  ivon02 = 0                                 
      goto 41710                                                       
31710 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41710, 1721, 41710                                    
41710 if ( ivon02 - 1 )  21710, 11710, 21710                            
11710 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1721                                                        
21710 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1721 continue                                                          
      ivtnum = 172                                                      
!                                                                        
!       ****  TEST 172  ****                                             
!      TEST 172  -  RELATIONAL EXPRESSIONAL.  INTEGER VARIABLE REFERENCE.
!            FALSE PATH.  .EQ.                                           
!                                                                        
!                                                                        
      if (iczero) 31720, 1720, 31720                                    
 1720 continue                                                          
      ivon01 = 587                                                      
      ivon02 = 1                                                        
      if ( 9999  ==  ivon01 )  ivon02 = 0                               
      goto 41720                                                       
31720 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41720, 1731, 41720                                    
41720 if ( ivon02 - 1 )  21720, 11720, 21720                            
11720 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1731                                                        
21720 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1731 continue                                                          
      ivtnum = 173                                                      
!                                                                        
!       ****  TEST 173  ****                                             
!      TEST 173  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            FALSE PATH.  .NE.                                           
!                                                                        
!                                                                        
      if (iczero) 31730, 1730, 31730                                    
 1730 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 1                                                        
      if ( 3  /=  ivon01 )  ivon02 = 0                                  
      goto 41730                                                       
31730 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41730, 1741, 41730                                    
41730 if ( ivon02 - 1 )  21730, 11730, 21730                            
11730 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1741                                                        
21730 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1741 continue                                                          
      ivtnum = 174                                                      
!                                                                        
!       ****  TEST 174  ****                                             
!      TEST 174  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            FALSE PATH.  .GT.                                           
!                                                                        
!                                                                        
      if (iczero) 31740, 1740, 31740                                    
 1740 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 1                                                        
      if ( 76  >  ivon01 )  ivon02 = 0                                 
      goto 41740                                                       
31740 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41740, 1751, 41740                                    
41740 if ( ivon02 - 1 )  21740, 11740, 21740                            
11740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1751                                                        
21740 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1751 continue                                                          
      ivtnum = 175                                                      
!                                                                        
!       ****  TEST 175  ****                                             
!      TEST 175  -  RELATIONAL EXPRESSION.  INTEGER VARIABLE REFERENCE.  
!            FALSE PATH.  .GE.                                           
!                                                                        
!                                                                        
      if (iczero) 31750, 1750, 31750                                    
 1750 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 1                                                        
      if ( 76  >=  ivon01 )  ivon02 = 0                                 
      goto 41750                                                       
31750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41750, 1761, 41750                                    
41750 if ( ivon02 - 1 )  21750, 11750, 21750                            
11750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1761                                                        
21750 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1761 continue                                                          
      ivtnum = 176                                                      
!                                                                        
!       ****  TEST 176  ****                                             
!      TEST 176  -  RELATIONAL EXPRESSION.  (IVR)  (RO)  (IC)            
!            INTEGER VARIABLE REFERENCE WITH INTEGER CONSTANT            
!            TRUE PATH.  .LT.                                            
!                                                                        
!                                                                        
      if (iczero) 31760, 1760, 31760                                    
 1760 continue                                                          
      ivon01 = 3                                                        
      ivon02 = 0                                                        
      if ( ivon01  <  76 )  ivon02 = 1                                 
      goto 41760                                                       
31760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41760, 1771, 41760                                    
41760 if ( ivon02 - 1 )  21760, 11760, 21760                            
11760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1771                                                        
21760 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1771 continue                                                          
      ivtnum = 177                                                      
!                                                                        
!       ****  TEST 177  ****                                             
!      TEST 177  - LIKE TEST 176.  FALSE PATH.  .EQ.                     
!                                                                        
!                                                                        
      if (iczero) 31770, 1770, 31770                                    
 1770 continue                                                          
      ivon01 = 587                                                      
      ivon02 = 1                                                        
      if ( ivon01  ==  9999 )  ivon02=0                                 
      goto 41770                                                       
31770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41770, 1781, 41770                                    
41770 if ( ivon02 - 1 )  21770, 11770, 21770                            
11770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1781                                                        
21770 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1781 continue                                                          
      ivtnum = 178                                                      
!                                                                        
!       ****  TEST 178  ****                                             
!      TEST 178  -  LIKE TEST 176.  TRUE PATH.  .GE.                     
!                                                                        
!                                                                        
      if (iczero) 31780, 1780, 31780                                    
 1780 continue                                                          
      ivon01 = 32767                                                    
      ivon02 = 0                                                        
      if ( ivon01  >=  32767 )  ivon02 = 1                              
      goto 41780                                                       
31780 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41780, 1791, 41780                                    
41780 if ( ivon02 - 1 )  21780, 11780, 21780                            
11780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1791                                                        
21780 ivfail = ivfail + 1                                               
      ivcomp = ivon02                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1791 continue                                                          
      ivtnum = 179                                                      
!                                                                        
!       ****  TEST 179  ****                                             
!      TEST 179  -  RELATIONAL EXPRESSION.  INTEGER ARRAY ELEMENT        
!            REFERENCE.  (IC)  (RO)  (IAER)   FALSE PATH.  .LT.          
!                                                                        
!                                                                        
      if (iczero) 31790, 1790, 31790                                    
 1790 continue                                                          
      ivon01 = 1                                                        
      iadn11(1) = 3                                                     
      if ( 76  <  iadn11(1) )  ivon01 = 0                              
      goto 41790                                                       
31790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41790, 1801, 41790                                    
41790 if ( ivon01 - 1 )  21790, 11790, 21790                            
11790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1801                                                        
21790 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1801 continue                                                          
      ivtnum = 180                                                      
!                                                                        
!       ****  TEST 180  ****                                             
!      TEST 180  -  LIKE TEST 179.  TRUE PATH.  .LE.                     
!                                                                        
!                                                                        
      if (iczero) 31800, 1800, 31800                                    
 1800 continue                                                          
      ivon01 = 0                                                        
      iadn11(2) = 587                                                   
      if ( 587  <=  iadn11(2) )  ivon01 = 1                             
      goto 41800                                                       
31800 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41800, 1811, 41800                                    
41800 if ( ivon01 - 1 )  21800, 11800, 21800                            
11800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1811                                                        
21800 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1811 continue                                                          
      ivtnum = 181                                                      
!                                                                        
!       ****  TEST 181  ****                                             
!      TEST 181  -  LIKE TEST 179.    FALSE PATH.  .GE.                  
!                                                                        
!                                                                        
      if (iczero) 31810, 1810, 31810                                    
 1810 continue                                                          
      ivon01 = 1                                                        
      iadn11(3) = 32767                                                 
      if ( 76  >=  iadn11(3) )  ivon01 = 0                              
      goto 41810                                                       
31810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41810, 1821, 41810                                    
41810 if ( ivon01 - 1 )  21810, 11810, 21810                            
11810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1821                                                        
21810 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1821 continue                                                          
      ivtnum = 182                                                      
!                                                                        
!       ****  TEST 182  ****                                             
!      TEST 182  -  RELATIONAL EXPRESSION  (IAER)  (RO)  (IC).  TRUE     
!            PATH.  .EQ.                                                 
!                                                                        
!                                                                        
      if (iczero) 31820, 1820, 31820                                    
 1820 continue                                                          
      ivon01 = 0                                                        
      iadn11(2) = 32767                                                 
      if ( iadn11(2)  ==  32767 )  ivon01 = 1                           
      goto 41820                                                       
31820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41820, 1831, 41820                                    
41820 if ( ivon01 - 1 )  21820, 11820, 21820                            
11820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1831                                                        
21820 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1831 continue                                                          
      ivtnum = 183                                                      
!                                                                        
!       ****  TEST 183  ****                                             
!      TEST 183  -  RELATIONAL EXPRESSION  (IVR)  (RO)  (IAER)           
!            FALSE PATH.  .NE.                                           
!                                                                        
!                                                                        
      if (iczero) 31830, 1830, 31830                                    
 1830 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 587                                                      
      iadn11(1) = 587                                                   
      if ( ivon02  /=  iadn11(1) )  ivon01 = 0                          
      goto 41830                                                       
31830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41830, 1841, 41830                                    
41830 if ( ivon01 - 1 )  21830, 11830, 21830                            
11830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1841                                                        
21830 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1841 continue                                                          
      ivtnum = 184                                                      
!                                                                        
!       ****  TEST 184  ****                                             
!      TEST 184  -  RELATIONAL EXPRESSION  (IAER)  (RO)  (IVR)           
!            TRUE PATH  .NE.                                             
!                                                                        
!                                                                        
      if (iczero) 31840, 1840, 31840                                    
 1840 continue                                                          
      ivon01 = 0                                                        
      iadn11(3) = 3                                                     
      ivon02 = 32767                                                    
      if ( iadn11(3)  /=  ivon02 )  ivon01 = 1                          
      goto 41840                                                       
31840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41840, 1851, 41840                                    
41840 if ( ivon01 - 1 )  21840, 11840, 21840                            
11840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1851                                                        
21840 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1851 continue                                                          
      ivtnum = 185                                                      
!                                                                        
!       ****  TEST 185  ****                                             
!      TEST 185  -  TEST OF PARENTHESES  ( (LE) )                        
!            TRUE PATH  LOGICAL CONSTANT  .TRUE.                         
!                                                                        
!                                                                        
      if (iczero) 31850, 1850, 31850                                    
 1850 continue                                                          
      ivon01 = 0                                                        
      if ( ( .true. ) )  ivon01 = 1                                     
      goto 41850                                                       
31850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41850, 1861, 41850                                    
41850 if ( ivon01 - 1 )  21850, 11850, 21850                            
11850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1861                                                        
21850 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1861 continue                                                          
      ivtnum = 186                                                      
!                                                                        
!       ****  TEST 186  ****                                             
!      TEST 186  -  LIKE TEST 185                                        
!            FALSE PATH  LOGICAL CONSTANT  .FALSE.                       
!                                                                        
!                                                                        
      if (iczero) 31860, 1860, 31860                                    
 1860 continue                                                          
      ivon01 = 1                                                        
      if ((( .false. )))  ivon01 = 0                                    
      goto 41860                                                       
31860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41860, 1871, 41860                                    
41860 if ( ivon01 - 1 )  21860, 11860, 21860                            
11860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1871                                                        
21860 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1871 continue                                                          
      ivtnum = 187                                                      
!                                                                        
!       ****  TEST 187  ****                                             
!      TEST 187  -  PARENS AROUND LOGICAL VARIABLE REFERENCE  ( (LVR) )  
!            TRUE PATH                                                   
!                                                                        
!                                                                        
      if (iczero) 31870, 1870, 31870                                    
 1870 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      if ( ( lctnt1 ) )  ivon01 = 1                                     
      goto 41870                                                       
31870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41870, 1881, 41870                                    
41870 if ( ivon01 - 1 )  21870, 11870, 21870                            
11870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1881                                                        
21870 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1881 continue                                                          
      ivtnum = 188                                                      
!                                                                        
!       ****  TEST  188  ****                                            
!      TEST 188  -  PARENS AROUND LOGICAL ARRAY REFERENCE  ( ( LAER ) )  
!            FALSE PATH                                                  
!                                                                        
      if (iczero) 31880, 1880, 31880                                    
 1880 continue                                                          
      ivon01 = 1                                                        
      latn1a(1) = .false.                                               
      if ( ( latn1a(1) ) )  ivon01 = 0                                  
      goto 41880                                                       
31880 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41880, 1891, 41880                                    
41880 if ( ivon01 - 1 )  21880, 11880, 21880                            
11880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1891                                                        
21880 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1891 continue                                                          
      ivtnum = 189                                                      
!                                                                        
!       ****  TEST 189  ****                                             
!      TEST 189  -  USE OF .NOT. WITH A LOGICAL PRIMARY  .NOT. (LP)      
!            FALSE PATH  .NOT. .TRUE.                                    
!                                                                        
!                                                                        
      if (iczero) 31890, 1890, 31890                                    
 1890 continue                                                          
      ivon01 = 1                                                        
      if ( .not. .true. )  ivon01 = 0                                   
      goto 41890                                                       
31890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41890, 1901, 41890                                    
41890 if ( ivon01 - 1 )  21890, 11890, 21890                            
11890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1901                                                        
21890 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1901 continue                                                          
      ivtnum = 190                                                      
!                                                                        
!       ****  TEST 190  ****                                             
!      TEST 190  -  LIKE TEST 189  TRUE PATH  .NOT. .FALSE.              
!                                                                        
!                                                                        
      if (iczero) 31900, 1900, 31900                                    
 1900 continue                                                          
      ivon01 = 0                                                        
      if ( .not. .false. )  ivon01 = 1                                  
      goto 41900                                                       
31900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41900, 1911, 41900                                    
41900 if ( ivon01 - 1 )  21900, 11900, 21900                            
11900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1911                                                        
21900 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1911 continue                                                          
      ivtnum = 191                                                      
!                                                                        
!       ****  TEST 191  ****                                             
!      TEST 191  -  TESTS .NOT. WITH A LOGICAL VARIABLE SET TO .FALSE.   
!            IN A LOGICAL ASSIGNMENT STATEMENT     TRUE PATH             
!                                                                        
!                                                                        
      if (iczero) 31910, 1910, 31910                                    
 1910 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .false.                                                  
      if ( .not. lctnt1 )  ivon01 = 1                                   
      goto 41910                                                       
31910 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41910, 1921, 41910                                    
41910 if ( ivon01 - 1 )  21910, 11910, 21910                            
11910 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1921                                                        
21910 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1921 continue                                                          
      ivtnum = 192                                                      
!                                                                        
!       ****  TEST 192  ****                                             
!      TEST 192  -  LIKE TEST 191 ONLY USES A LOGICAL ARRAY ELEMENT      
!            SET TO .FALSE. IN A LOGICAL ASSIGNMENT STATEMENT    TRUE    
!                                                                        
!                                                                        
      if (iczero) 31920, 1920, 31920                                    
 1920 continue                                                          
      ivon01 = 0                                                        
      latn1a(2) = .false.                                               
      if ( .not. latn1a(2) )  ivon01 = 1                                
      goto 41920                                                       
31920 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41920, 1931, 41920                                    
41920 if ( ivon01 - 1 )  21920, 11920, 21920                            
11920 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1931                                                        
21920 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1931 continue                                                          
      ivtnum = 193                                                      
!                                                                        
!       ****  TEST 193  ****                                             
!      TEST 193  -  USE OF LOGICAL .AND.    (LT) .AND. (LF)              
!            USES TWO LOGICAL VARIABLES EACH SET TO .FALSE.              
!            FALSE  .AND.  FALSE    FALSE PATH                           
!                                                                        
!                                                                        
      if (iczero) 31930, 1930, 31930                                    
 1930 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .false.                                                  
      if ( lctnt1 .and. lctnt2 )  ivon01 = 0                            
      goto 41930                                                       
31930 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41930, 1941, 41930                                    
41930 if ( ivon01 - 1 )  21930, 11930, 21930                            
11930 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1941                                                        
21930 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1941 continue                                                          
      ivtnum = 194                                                      
!                                                                        
!       ****  TEST 194  ****                                             
!      TEST 194  -  LIKE TEST 193    FALSE  .AND.  TRUE   FALSE PATH     
!                                                                        
!                                                                        
      if (iczero) 31940, 1940, 31940                                    
 1940 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .true.                                                   
      if ( lctnt1 .and. lctnt2 )  ivon01 = 0                            
      goto 41940                                                       
31940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41940, 1951, 41940                                    
41940 if ( ivon01 - 1 )  21940, 11940, 21940                            
11940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1951                                                        
21940 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1951 continue                                                          
      ivtnum = 195                                                      
!                                                                        
!       ****  TEST 195  ****                                             
!      TEST 195  -  LIKE TEST 193   TRUE  .AND.  FALSE     FALSE PATH    
!                                                                        
!                                                                        
      if (iczero) 31950, 1950, 31950                                    
 1950 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .false.                                                  
      if ( lctnt1 .and. lctnt2 )  ivon01 = 0                            
      goto 41950                                                       
31950 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41950, 1961, 41950                                    
41950 if ( ivon01 - 1 )  21950, 11950, 21950                            
11950 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1961                                                        
21950 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1961 continue                                                          
      ivtnum = 196                                                      
!                                                                        
!       ****  TEST 196  ****                                             
!      TEST 196  -  LIKE TEST 193   TRUE  .AND.  TRUE    TRUE PATH       
!                                                                        
!                                                                        
      if (iczero) 31960, 1960, 31960                                    
 1960 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .true.                                                   
      if ( lctnt1 .and. lctnt2 )  ivon01 = 1                            
      goto 41960                                                       
31960 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41960, 1971, 41960                                    
41960 if ( ivon01 - 1 )  21960, 11960, 21960                            
11960 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1971                                                        
21960 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1971 continue                                                          
      ivtnum = 197                                                      
!                                                                        
!       ****  TEST 197  ****                                             
!      TEST 197  -  TEST OF THE INCLUSIVE  .OR.  .    (LE)  .OR.  (LT)   
!            USES LOGICAL VARIABLES SET IN LOGICAL ASSIGNMENT STATEMENTS 
!            FALSE  .OR.  FALSE    FALSE PATH                            
!                                                                        
!                                                                        
      if (iczero) 31970, 1970, 31970                                    
 1970 continue                                                          
      ivon01 = 1                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .false.                                                  
      if ( lctnt1 .or. lctnt2 )  ivon01 = 0                             
      goto 41970                                                       
31970 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41970, 1981, 41970                                    
41970 if ( ivon01 - 1 )  21970, 11970, 21970                            
11970 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1981                                                        
21970 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1981 continue                                                          
      ivtnum = 198                                                      
!                                                                        
!       ****  TEST 198  ****                                             
!      TEST 198  -  LIKE TEST 197  FALSE  .OR.  TRUE    TRUE PATH        
!                                                                        
!                                                                        
      if (iczero) 31980, 1980, 31980                                    
 1980 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .false.                                                  
      lctnt2 = .true.                                                   
      if ( lctnt1 .or. lctnt2 )  ivon01 = 1                             
      goto 41980                                                       
31980 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41980, 1991, 41980                                    
41980 if ( ivon01 - 1 )  21980, 11980, 21980                            
11980 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1991                                                        
21980 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1991 continue                                                          
      ivtnum = 199                                                      
!                                                                        
!       ****  TEST 199  ****                                             
!      TEST 199  -  LIKE TEST 197.  TRUE  .OR.  FALSE    TRUE PATH.      
!                                                                        
!                                                                        
      if (iczero) 31990, 1990, 31990                                    
 1990 continue                                                          
      ivon01 = 0                                                        
      lctnt1 = .true.                                                   
      lctnt2 = .false.                                                  
      if ( lctnt1 .or. lctnt2 )  ivon01 = 1                             
      goto 41990                                                       
31990 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41990, 5001, 41990                                    
41990 if ( ivon01 - 1 )  21990, 11990, 21990                            
11990 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 5001                                                        
21990 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 5001 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM017" )                          
      end program fm017
