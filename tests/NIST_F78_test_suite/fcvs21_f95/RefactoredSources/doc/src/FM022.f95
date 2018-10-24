      program fm022
!      COMMENT SECTION.                                                  
!                                                                        
!      FM022                                                             
!                                                                        
!          THIS ROUTINE TESTS ARRAYS WITH FIXED DIMENSION AND SIZE LIMITS
!      SET EITHER IN A BLANK COMMON OR DIMENSION STATEMENT.  THE VALUES  
!      OF THE ARRAY ELEMENTS ARE SET IN VARIOUS WAYS SUCH AS SIMPLE      
!      ASSIGNMENT STATEMENTS, SET TO THE VALUES OF OTHER ARRAY ELEMENTS  
!      (EITHER POSITIVE OR NEGATIVE), SET BY INTEGER TO REAL OR REAL TO  
!      INTEGER CONVERSION, SET BY ARITHMETIC EXPRESSIONS, OR SET BY      
!      USE OF THE  EQUIVALENCE  STATEMENT.                               
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8, SPECIFICATION STATEMENTS                            
!         SECTION 8.1, DIMENSION STATEMENT                               
!         SECTION 8.2, EQUIVALENCE STATEMENT                             
!         SECTION 8.3, COMMON STATEMENT                                  
!         SECTION 8.4, TYPE-STATEMENTS                                   
!         SECTION 9, DATA STATEMENT                                      
!                                                                        
!                                                                        
!                                                                        
!                                                                        
      integer, dimension(1:5) :: iadn14
      real, dimension(1:5) :: radn14
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: icon01
      real :: rcon01
      integer :: icon02
      integer, dimension(1:5) :: iadn11
      real, dimension(1:5) :: radn11
      logical, dimension(1:2) :: ladn11
      integer, dimension(1:5) :: iadn12
      real, dimension(1:5) :: radn12
      logical, dimension(1:2) :: ladn12
      integer, dimension(1:2) :: iadn15
      real, dimension(1:2) :: radn15
      integer, dimension(1:4) :: iadn16
      integer, dimension(1:4) :: iadn17
!                                                                        
      integer, dimension(1:5) :: radn13
      real, dimension(1:5) :: iadn13
      logical, dimension(1:2) :: ladn13
      logical :: lctn01
!                                                                        
      equivalence (iadn14(1), iadn15(1)), (radn14(2),radn15(2))         
      equivalence (ladn13(1),lctn01),  (iadn14(5), icon02)              
      equivalence (radn14(5), rcon01)                                   
      equivalence ( iadn16(3), iadn17(2) )                              
!                                                                        
      data iadn12(1) / 3 / ,radn12(1) / -512. / ,iadn13(1) / 0.5 / ,radn13(1) / -3 / 
!                                                                        
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
      ivtnum = 604                                                      
!                                                                        
!       ****  TEST 604  ****                                             
!      TEST 604  -  THIS TESTS A  SIMPLE ASSIGNMENT STATEMENT IN SETTING 
!      AN INTEGER ARRAY ELEMENT TO A POSITIVE VALUE OF 32767.            
!                                                                        
      if (iczero) 36040, 6040, 36040                                    
 6040 continue                                                          
      iadn11(5) = 32767                                                 
      ivcomp = iadn11(5)                                                
      goto 46040                                                       
36040 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46040, 6051, 46040                                    
46040 if ( ivcomp - 32767 )  26040, 16040, 26040                        
16040 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6051                                                        
26040 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6051 continue                                                          
      ivtnum = 605                                                      
!                                                                        
!       ****  TEST 605  ****                                             
!      TEST 605  -  TEST OF A SIMPLE ASSIGN WITH A NEGATIVE VALUE -32766 
!                                                                        
      if (iczero) 36050, 6050, 36050                                    
 6050 continue                                                          
      iadn11(1) = -32766                                                
      ivcomp = iadn11(1)                                                
      goto 46050                                                       
36050 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46050, 6061, 46050                                    
46050 if ( ivcomp + 32766 )  26050, 16050, 26050                        
16050 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6061                                                        
26050 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6061 continue                                                          
      ivtnum = 606                                                      
!                                                                        
!       ****  TEST 606  ****                                             
!      TEST 606  -  TEST OF UNSIGNED ZERO SET TO AN ARRAY ELEMENT        
!      BY A SIMPLE ASSIGNMENT STATEMENT.                                 
!                                                                        
      if (iczero) 36060, 6060, 36060                                    
 6060 continue                                                          
      iadn11(3) = 0                                                     
      ivcomp = iadn11(3)                                                
      goto 46060                                                       
36060 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46060, 6071, 46060                                    
46060 if ( ivcomp - 0 )  26060, 16060, 26060                            
16060 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6071                                                        
26060 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6071 continue                                                          
      ivtnum = 607                                                      
!                                                                        
!       ****  TEST 607  ****                                             
!      TEST 607  -  TEST OF A NEGATIVELY SIGNED ZERO COMPARED TO A       
!      ZERO UNSIGNED BOTH VALUES SET AS INTEGER ARRAY ELEMENTS.          
!                                                                        
      if (iczero) 36070, 6070, 36070                                    
 6070 continue                                                          
      iadn11(2) = -0                                                    
      iadn11(3) = 0                                                     
      icon01 = 0                                                        
      if ( iadn11(2)  ==  iadn11(3) )  icon01 = 1                       
      goto 46070                                                       
36070 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46070, 6081, 46070                                    
46070 if ( icon01 - 1 )  26070, 16070, 26070                            
16070 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6081                                                        
26070 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6081 continue                                                          
      ivtnum = 608                                                      
!                                                                        
!       ****  TEST 608  ****                                             
!      TEST 608  -  TEST OF SETTING ONE INTEGER ARRAY ELEMENT EQUAL TO   
!      THE VALUE OF ANOTHER INTEGER ARRAY ELEMENT.  THE VALUE IS 32767.  
!                                                                        
      if (iczero) 36080, 6080, 36080                                    
 6080 continue                                                          
      iadn11(1) = 32767                                                 
      iadn12(5) = iadn11(1)                                             
      ivcomp = iadn12(5)                                                
      goto 46080                                                       
36080 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46080, 6091, 46080                                    
46080 if ( ivcomp - 32767 )  26080, 16080, 26080                        
16080 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6091                                                        
26080 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6091 continue                                                          
      ivtnum = 609                                                      
!                                                                        
!       ****  TEST 609  ****                                             
!      TEST 609  -  TEST OF AN ARRAY ELEMENT SET TO ANOTHER ARRAY ELEMENT
!      WHICH HAD BEEN SET AT COMPILE TIME BY A DATA INITIALIZATION       
!      STATEMENT.  AN INTEGER ARRAY IS USED WITH THE VALUE 3.            
!                                                                        
      if (iczero) 36090, 6090, 36090                                    
 6090 continue                                                          
      iadn11(4) = iadn12(1)                                             
      ivcomp = iadn11(4)                                                
      goto 46090                                                       
36090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46090, 6101, 46090                                    
46090 if ( ivcomp - 3 )  26090, 16090, 26090                            
16090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6101                                                        
26090 ivfail = ivfail + 1                                               
      ivcorr = 3                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6101 continue                                                          
      ivtnum = 610                                                      
!                                                                        
!       ****  TEST 610  ****                                             
!      TEST 610  -   TEST OF SETTING A REAL ARRAY ELEMENT TO A POSITIVE  
!      VALUE IN A SIMPLE ASSIGNMENT STATEMENT.  VALUE IS 32767.          
!                                                                        
      if (iczero) 36100, 6100, 36100                                    
 6100 continue                                                          
      radn11(5) = 32767.                                                
      ivcomp = radn11(5)                                                
      goto 46100                                                       
36100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46100, 6111, 46100                                    
46100 if ( ivcomp - 32767 )  26100, 16100, 26100                        
16100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6111                                                        
26100 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6111 continue                                                          
      ivtnum = 611                                                      
!                                                                        
!       ****  TEST 611  ****                                             
!      TEST 611  -  TEST OF SETTING A REAL ARRAY ELEMENT TO A NEGATIVE   
!      VALUE IN A SIMPLE ASSIGNMENT STATEMENT.  VALUE IS -32766.         
!                                                                        
      if (iczero) 36110, 6110, 36110                                    
 6110 continue                                                          
      radn11(1) = -32766.                                               
      ivcomp = radn11(1)                                                
      goto 46110                                                       
36110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46110, 6121, 46110                                    
46110 if ( ivcomp + 32766 )  26110, 16110, 26110                        
16110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6121                                                        
26110 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6121 continue                                                          
      ivtnum = 612                                                      
!                                                                        
!       ****  TEST 612  ****                                             
!      TEST 612  -  TEST OF SETTING A REAL ARRAY ELEMENT TO UNSIGNED ZERO
!      IN A SIMPLE ASSIGNMENT STATEMENT.                                 
!                                                                        
      if (iczero) 36120, 6120, 36120                                    
 6120 continue                                                          
      radn11(3) = 0.                                                    
      ivcomp = radn11(3)                                                
      goto 46120                                                       
36120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46120, 6131, 46120                                    
46120 if ( ivcomp - 0 )  26120, 16120, 26120                            
16120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6131                                                        
26120 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6131 continue                                                          
      ivtnum = 613                                                      
!                                                                        
!       ****  TEST 613  ****                                             
!      TEST 613  -  TEST OF A NEGATIVELY SIGNED ZERO IN A REAL ARRAY     
!      ELEMENT COMPARED TO A REAL ELEMENT SET TO AN UNSIGNED ZERO.       
!                                                                        
      if (iczero) 36130, 6130, 36130                                    
 6130 continue                                                          
      radn11(2) = -0.0                                                  
      radn11(3) = 0.0                                                   
      icon01 = 0                                                        
      if ( radn11(2)  ==  radn11(3) )  icon01 = 1                       
      goto 46130                                                       
36130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46130, 6141, 46130                                    
46130 if ( icon01 - 1 )  26130, 16130, 26130                            
16130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6141                                                        
26130 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6141 continue                                                          
      ivtnum = 614                                                      
!                                                                        
!       ****  TEST 614  ****                                             
!      TEST 614  -  TEST OF SETTING ONE REAL ARRAY ELEMENT EQUAL TO THE  
!      VALUE OF ANOTHER REAL ARRAY ELEMENT.  THE VALUE IS 32767.         
!                                                                        
      if (iczero) 36140, 6140, 36140                                    
 6140 continue                                                          
      radn11(1) = 32767.                                                
      radn12(5) = radn11(1)                                             
      ivcomp = radn12(5)                                                
      goto 46140                                                       
36140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46140, 6151, 46140                                    
46140 if ( ivcomp - 32767 )  26140, 16140, 26140                        
16140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6151                                                        
26140 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6151 continue                                                          
      ivtnum = 615                                                      
!                                                                        
!       ****  TEST 615  ****                                             
!      TEST 615  -  TEST OF A REAL ARRAY ELEMENT SET TO ANOTHER REAL     
!      ARRAY ELEMENT WHICH HAD BEEN SET AT COMPILE TIME BY A DATA        
!      INITIALIZATION STATEMENT. THE VALUE IS -512.                      
!                                                                        
      if (iczero) 36150, 6150, 36150                                    
 6150 continue                                                          
      radn11(4) = radn12(1)                                             
      ivcomp = radn11(4)                                                
      goto 46150                                                       
36150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46150, 6161, 46150                                    
46150 if ( ivcomp + 512 )  26150, 16150, 26150                          
16150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6161                                                        
26150 ivfail = ivfail + 1                                               
      ivcorr = - 512                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6161 continue                                                          
      ivtnum = 616                                                      
!                                                                        
!       ****  TEST 616  ****                                             
!      TEST 616  -  TEST OF SETTING THE VALUE OF AN INTEGER ARRAY ELEMENT
!      BY AN ARITHMETIC EXPRESSION.                                      
!                                                                        
      if (iczero) 36160, 6160, 36160                                    
 6160 continue                                                          
      icon01 = 1                                                        
      iadn11(3) = icon01 + 1                                            
      ivcomp = iadn11(3)                                                
      goto 46160                                                       
36160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46160, 6171, 46160                                    
46160 if ( ivcomp - 2 )  26160, 16160, 26160                            
16160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6171                                                        
26160 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6171 continue                                                          
      ivtnum = 617                                                      
!                                                                        
!       ****  TEST 617  ****                                             
!      TEST 617  -  TEST OF SETTING THE VALUE OF A REAL ARRAY ELEMENT    
!      BY AN ARITHMETIC EXPRESSION.                                      
!                                                                        
      if (iczero) 36170, 6170, 36170                                    
 6170 continue                                                          
      rcon01 = 1.                                                       
      radn11(3) = rcon01 + 1.                                           
      ivcomp = radn11(3)                                                
      goto 46170                                                       
36170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46170, 6181, 46170                                    
46170 if ( ivcomp - 2 )  26170, 16170, 26170                            
16170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6181                                                        
26170 ivfail = ivfail + 1                                               
      ivcorr = 2                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6181 continue                                                          
      ivtnum = 618                                                      
!                                                                        
!       ****  TEST 618  ****                                             
!      TEST 618  -  TEST OF SETTING THE VALUE OF AN INTEGER ARRAY ELEMENT
!      TO ANOTHER INTEGER ARRAY ELEMENT AND CHANGING THE SIGN.           
!                                                                        
      if (iczero) 36180, 6180, 36180                                    
 6180 continue                                                          
      iadn11(2) = 32766                                                 
      iadn11(4) = - iadn11(2)                                           
      ivcomp = iadn11(4)                                                
      goto 46180                                                       
36180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46180, 6191, 46180                                    
46180 if ( ivcomp + 32766 )  26180, 16180, 26180                        
16180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6191                                                        
26180 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6191 continue                                                          
      ivtnum = 619                                                      
!                                                                        
!       ****  TEST 619  ****                                             
!      TEST 619  -  TEST OF SETTING THE VALUE OF A REAL ARRAY ELEMENT    
!      TO THE VALUE OF ANOTHER REAL ARRAY ELEMENT AND CHANGING THE SIGN. 
!                                                                        
      if (iczero) 36190, 6190, 36190                                    
 6190 continue                                                          
      radn11(2) = 32766.                                                
      radn11(4) = - radn11(2)                                           
      ivcomp = radn11(4)                                                
      goto 46190                                                       
36190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46190, 6201, 46190                                    
46190 if ( ivcomp + 32766 )  26190, 16190, 26190                        
16190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6201                                                        
26190 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6201 continue                                                          
      ivtnum = 620                                                      
!                                                                        
!       ****  TEST 620  ****                                             
!      TEST 620  -  TEST OF SETTING THE VALUE OF A LOGICAL ARRAY ELEMENT 
!      TO THE VALUE OF ANOTHER LOGICAL ARRAY ELEMENT.                    
!                                                                        
      if (iczero) 36200, 6200, 36200                                    
 6200 continue                                                          
      ladn11(1) = .true.                                                
      ladn12(1) = ladn11(1)                                             
      icon01 = 0                                                        
      if ( ladn12(1) )  icon01 = 1                                      
      goto 46200                                                       
36200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46200, 6211, 46200                                    
46200 if ( icon01 - 1 )  26200, 16200, 26200                            
16200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6211                                                        
26200 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6211 continue                                                          
      ivtnum = 621                                                      
!                                                                        
!       ****  TEST 621  ****                                             
!      TEST 621  -  TEST OF SETTING THE VALUE OF A LOGICAL ARRAY ELEMENT 
!      TO THE VALUE OF ANOTHER LOGICAL ARRAY ELEMENT AND CHANGING        
!      THE VALUE FROM  .TRUE.  TO  .FALSE. BY USING THE .NOT. STATEMENT. 
!                                                                        
      if (iczero) 36210, 6210, 36210                                    
 6210 continue                                                          
      ladn11(2) = .true.                                                
      ladn12(2) = .not. ladn11(2)                                       
      icon01 = 1                                                        
      if ( ladn12(2) )  icon01 = 0                                      
      goto 46210                                                       
36210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46210, 6221, 46210                                    
46210 if ( icon01 - 1 )  26210, 16210, 26210                            
16210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6221                                                        
26210 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6221 continue                                                          
      ivtnum = 622                                                      
!                                                                        
!       ****  TEST 622  ****                                             
!      TEST 622  -  TEST OF THE TYPE STATEMENT AND THE DATA              
!      INITIALIZATION STATEMENT.  THE EXPLICITLY REAL ARRAY ELEMENT      
!      SHOULD HAVE THE VALUE OF .5                                       
!                                                                        
      if (iczero) 36220, 6220, 36220                                    
 6220 continue                                                          
      ivcomp = 2. * iadn13(1)                                           
      goto 46220                                                       
36220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46220, 6231, 46220                                    
46220 if ( ivcomp - 1 )  26220, 16220, 26220                            
16220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6231                                                        
26220 ivfail = ivfail + 1                                               
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6231 continue                                                          
      ivtnum = 623                                                      
!                                                                        
!       ****  TEST 623  ****                                             
!      TEST 623  -  TEST OF REAL TO INTEGER CONVERSION USING ARRAYS.     
!      THE INITIALIZED VALUE OF 0.5 SHOULD BE TRUNCATED TO ZERO.         
!                                                                        
      if (iczero) 36230, 6230, 36230                                    
 6230 continue                                                          
      iadn11(1) = iadn13(1)                                             
      ivcomp = iadn11(1)                                                
      goto 46230                                                       
36230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46230, 6241, 46230                                    
46230 if ( ivcomp - 0 )  26230, 16230, 26230                            
16230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6241                                                        
26230 ivfail = ivfail + 1                                               
      ivcorr = 0                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6241 continue                                                          
      ivtnum = 624                                                      
!                                                                        
!       ****  TEST 624  ****                                             
!      TEST 624  -  TEST OF THE COMMON STATEMENT BY SETTING THE VALUE OF 
!      AN INTEGER ARRAY ELEMENT IN A DIMENSIONED ARRAY TO THE VALUE      
!      OF A REAL ARRAY ELEMENT IN COMMON.  THE ELEMENT IN COMMON HAD ITS 
!      VALUE SET IN A SIMPLE ASSIGNMENT STATEMENT TO 9999.               
!                                                                        
      if (iczero) 36240, 6240, 36240                                    
 6240 continue                                                          
      radn14(1) = 9999.                                                 
      iadn11(1) = radn14(1)                                             
      ivcomp = iadn11(1)                                                
      goto 46240                                                       
36240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46240, 6251, 46240                                    
46240 if ( ivcomp - 9999 )  26240, 16240, 26240                         
16240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6251                                                        
26240 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6251 continue                                                          
      ivtnum = 625                                                      
!                                                                        
!       ****  TEST 625  ****                                             
!      TEST 625  -  TEST OF SETTING THE VALUE OF AN INTEGER ARRAY ELEMENT
!      IN COMMON TO THE VALUE OF A REAL ARRAY ELEMENT ALSO IN BLANK      
!      COMMON AND CHANGING THE SIGN.  THE VALUE USED IS 9999.            
!                                                                        
      if (iczero) 36250, 6250, 36250                                    
 6250 continue                                                          
      radn14(1) = 9999.                                                 
      iadn14(1) = - radn14(1)                                           
      ivcomp = iadn14(1)                                                
      goto 46250                                                       
36250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46250, 6261, 46250                                    
46250 if ( ivcomp + 9999 ) 26250, 16250, 26250                          
16250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6261                                                        
26250 ivfail = ivfail + 1                                               
      ivcorr = - 9999                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6261 continue                                                          
      ivtnum = 626                                                      
!                                                                        
!       ****  TEST 626  ****                                             
!      TEST 626  -  TEST OF SETTING THE VALUE OF A LOGICAL ARRAY ELEMENT 
!      IN BLANK COMMON TO  .NOT.  .TRUE.                                 
!      THE VALUE OF ANOTHER LOGICAL ARRAY ELEMENT ALSO IN COMMON IS THEN 
!      SET TO .NOT. OF THE VALUE OF THE FIRST.                           
!      VALUE OF THE FIRST ELEMENT SHOULD BE .FALSE.                      
!      VALUE OF THE SECOND ELEMENT SHOULD BE .TRUE.                      
!                                                                        
      if (iczero) 36260, 6260, 36260                                    
 6260 continue                                                          
      ladn13(1) = .not. .true.                                          
      ladn13(2) = .not. ladn13(1)                                       
      icon01 = 0                                                        
      if ( ladn13(2) )  icon01 = 1                                      
      goto 46260                                                       
36260 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46260, 6271, 46260                                    
46260 if ( icon01 - 1 )  26260, 16260, 26260                            
16260 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6271                                                        
26260 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6271 continue                                                          
      ivtnum = 627                                                      
!                                                                        
!       ****  TEST 627  ****                                             
!      TEST 627  -  TEST OF EQUIVALENCE ON THE FIRST ELEMENTS OF INTEGER 
!      ARRAYS ONE OF WHICH IS IN COMMON AND THE OTHER ONE IS DIMENSIONED.
!                                                                        
      if (iczero) 36270, 6270, 36270                                    
 6270 continue                                                          
      iadn14(2) = 32767                                                 
      ivcomp = iadn15(2)                                                
      goto 46270                                                       
36270 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46270, 6281, 46270                                    
46270 if ( ivcomp - 32767 )  26270, 16270, 26270                        
16270 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6281                                                        
26270 ivfail = ivfail + 1                                               
      ivcorr = 32767                                                    
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6281 continue                                                          
      ivtnum = 628                                                      
!                                                                        
!       ****  TEST 628  ****                                             
!      TEST 628  -  TEST OF EQUIVALENCE ON REAL ARRAYS ONE OF WHICH IS   
!      IN COMMON AND THE OTHER ONE IS DIMENSIONED.  THE ARRAYS WERE      
!      ALIGNED ON THEIR SECOND ELEMENTS.                                 
!                                                                        
      if (iczero) 36280, 6280, 36280                                    
 6280 continue                                                          
      radn15(1) = -32766.                                               
      ivcomp = radn14(1)                                                
      goto 46280                                                       
36280 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46280, 6291, 46280                                    
46280 if ( ivcomp + 32766 )  26280, 16280, 26280                        
16280 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6291                                                        
26280 ivfail = ivfail + 1                                               
      ivcorr = -32766                                                   
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6291 continue                                                          
      ivtnum = 629                                                      
!                                                                        
!       ****  TEST 629  ****                                             
!      TEST 629  -  TEST OF EQUIVALENCE WITH LOGICAL ELEMENTS.  AN ARRAY 
!      ELEMENT IN COMMON IS EQUIVALENCED TO A LOGICAL VARIABLE.          
!                                                                        
      if (iczero) 36290, 6290, 36290                                    
 6290 continue                                                          
      ladn13(2) = .true.                                                
      lctn01 = .not. ladn13(2)                                          
      icon01 = 1                                                        
      if ( ladn13(1) )  icon01 = 0                                      
      goto 46290                                                       
36290 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46290, 6301, 46290                                    
46290 if ( icon01 - 1 )  26290, 16290, 26290                            
16290 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6301                                                        
26290 ivfail = ivfail + 1                                               
      ivcomp = icon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6301 continue                                                          
      ivtnum = 630                                                      
!                                                                        
!       ****  TEST 630  ****                                             
!      TEST 630  -  TEST OF EQUIVALENCE WITH REAL AND INTEGER ELEMENTS   
!      WHICH ARE EQUIVALENCED TO ARRAY ELEMENTS IN COMMON.               
!                                                                        
      if (iczero) 36300, 6300, 36300                                    
 6300 continue                                                          
      rcon01 = 1.                                                       
      icon02 = - radn14(5)                                              
      ivcomp = iadn14(5)                                                
      goto 46300                                                       
36300 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46300, 6311, 46300                                    
46300 if ( ivcomp + 1 )  26300, 16300, 26300                            
16300 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6311                                                        
26300 ivfail = ivfail + 1                                               
      ivcorr = -1                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6311 continue                                                          
      ivtnum = 631                                                      
!                                                                        
!       ****  TEST 631  ****                                             
!      TEST 631  -  TEST OF EQUIVALENCE ON INTEGER ARRAY ELEMENTS.       
!      BOTH ARRAYS ARE DIMENSIONED.  THE FOURTH ELEMENT                  
!      OF THE FIRST OF THE ARRAYS SHOULD BE EQUAL TO THE THIRD ELEMENT OF
!      THE SECOND ARRAY.                                                 
!                                                                        
      if (iczero) 36310, 6310, 36310                                    
 6310 continue                                                          
      iadn16(4) = 9999                                                  
      ivcomp = iadn17(3)                                                
      goto 46310                                                       
36310 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46310, 6321, 46310                                    
46310 if ( ivcomp - 9999 )  26310, 16310, 26310                         
16310 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6321                                                        
26310 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 6321 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM022" )                          
      end program fm022
