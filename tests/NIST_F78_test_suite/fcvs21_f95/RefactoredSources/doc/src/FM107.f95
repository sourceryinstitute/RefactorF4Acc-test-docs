      program fm107
!      COMMENT SECTION.                                                  
!                                                                        
!      FM107                                                             
!                                                                        
!          THIS ROUTINE IS A TEST OF THE I FORMAT AND IS TAPE AND PRINTER
!      ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  
!      AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      
!      OUTPUT LISTS ARE INTEGER VARIABLE AND INTEGER ARRAY ELEMENT OR    
!      ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    
!      WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   
!      CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    
!      DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   
!      INTEGER ARRAY FOR THE DUMP SECTION.                               
!                                                                        
!          THE MAJOR PURPOSE OF THIS ROUTINE IS TO TEST WHETHER THE LAST 
!      SET OF PARENTHESES WILL BE REPEATED IN A FORMAT STATEMENT IF THE  
!      NUMBER OF DATA ITEMS IN THE INPUT/OUTPUT LIST IS GREATER THAN THE 
!      NUMBER OF FIELD SPECIFICATIONS WITHIN THE FORMAT STATEMENT.       
!      IN ADDITION THE USE OF TWO AND THREE DIMENSIONED ARRAYS IS TESTED 
!      IN THE IMPLIED-DO LISTS IN BOTH THE WRITE AND READ SECTIONS.      
!                                                                        
!           THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        
!      REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY FOURTH RECORD IS   
!      CHECKED DURING THE READ TEST SECTION PLUS THE LAST TWO RECORDS    
!      AND THE END OF FILE ON THE LAST RECORD.                           
!                                                                        
!           THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   
!      AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   
!      STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  
!      OF THE CONTINUATION LINE.                                         
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 8, SPECIFICATION STATEMENTS                            
!         SECTION 9, DATA STATEMENT                                      
!         SECTION 11.10, DO STATEMENT                                    
!         SECTION 12, INPUT/OUTPUT STATEMENTS                            
!         SECTION 12.8.2, INPUT/OUTPUT LIST                              
!         SECTION 12.9.5.2, FORMATTED DATA TRANSFER                      
!         SECTION 13, FORMAT STATEMENT                                   
!         SECTION 13.2.1, EDIT DESCRIPTORS                               
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: i06
      integer :: iprog
      integer :: ifile
      integer :: ilun
      integer :: itotr
      integer :: irlgn
      integer :: ieof
      integer :: irnum
      integer :: j
      integer :: k
      integer :: iflip
      integer :: ivtnum
      integer :: i
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      real :: go
      real :: to
      integer, dimension(1:31,1:20) :: iadn21
      integer, dimension(1:31,1:10,1:2) :: iadn31
      integer, dimension(1:27) :: itest
      character(len=1), dimension(1:136) :: idump
      character(len=1) :: nine
      data nine / '9' / 
!                                                                        
77701 format ( 80a1 )                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," NO EOF.. MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1)                                                 
77706 format (10x,"FILE I06 CREATED WITH 137 SEQUENTIAL RECORDS"  )     
77751 format ( i3, 2(1i2), 3(1i3), i4, 10(1i3) )                        
77752 format ( i3,2(1i2), 3(1i3), i4, 3(1i3) )                          
77753 format ( //////////////// i3,2i2,3i3,i4,10(i3) )                  
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
!                                                                        
!      DEFAULT ASSIGNMENT FOR FILE 08 IS I06 = 7                         
      i06 = 7                                                           
! X060 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-060               
! X061 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-061               
!                                                                        
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I06 THAT IS 
!      80 CHARACTERS PER RECORD, 31 RECORDS SETS, AND CONSISTS OF ONLY   
!      INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      
!      ROUTINE FM107 AND FOR PURPOSES OF IDENTIFICATION IS FILE 08.      
      iprog = 107                                                       
      ifile = 08                                                        
      ilun = i06                                                        
      itotr = 137                                                       
      irlgn = 80                                                        
      ieof = 0000                                                       
!      THESE DO-LOOPS ARE TO SET THE VALUES INTO THE TWO AND THREE       
!      DIMENSIONED ARRAYS FOR THE I/O LISTS....                          
      do irnum = 1, 31                                             
      do j = 1, 20                                                 
      iadn21(irnum,j) = irnum + j + 99                                  
  end do
  end do
!                                                                        
      do irnum = 1, 31                                             
      do j = 1, 10                                                 
      do k = 1, 2                                                  
      iadn31(irnum,j,k) = irnum + j + k + 298                           
  end do
  end do
  end do
      iflip = 1                                                         
      do irnum = 1, 31                                             
      if ( irnum  ==  31 ) ieof = 9999                                  
      if ( iflip - 1 )  1147, 1147, 1148                                
 1147 write ( i06, 77751 ) iprog, ifile, ilun, irnum, itotr, irlgn, ieof,(iadn21(irnum,j), j = 1, 20)                                     
      iflip = 2                                                         
      goto 1149                                                        
 1148 write ( i06, 77752 ) iprog, ifile, ilun, irnum, itotr, irlgn, ieof,((iadn31(irnum,j,k), k = 1, 2), j = 1, 10)                       
      iflip = 1                                                         
 1149 continue                                                          
      end do
      write (i02,77706)                                                 
!                                                                        
!      REWIND SECTION                                                    
!                                                                        
      rewind i06                                                        
!                                                                        
!      READ SECTION....                                                  
!                                                                        
      ivtnum = 114                                                      
!                                                                        
!      ****    TEST  114  THRU  TEST  121    ****                        
!      TEST 114 THRU 121  -  THESE TESTS READ THE SEQUENTIAL FILE        
!      PREVIOUSLY WRITTEN ON LUN I06 AND CHECK THE FIRST AND EVERY FOURTH
!      RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     
!      SEVERAL VALUES IN THE INTEGER ARRAY WHICH SHOULD FOLLOW A         
!      CALCULATED PATTERN WITH RESPECT TO THE SUBSCRIPTS AND THE RECORD  
!      NUMBER....                                                        
!                                                                        
      irnum = 1                                                         
      read(i06,77751) itest                                             
!      READ THE FIRST RECORD....                                         
      do i = 1, 8                                                  
      ivon01 = 0                                                        
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST         
      if ( itest(4)  ==  irnum )  ivon01 = ivon01 + 1                   
!      THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                
!      THE FOLLOWING TESTS ARE FOR ODD NUMBERED RECORDS                  
      if ( itest(8)  ==  iadn21(irnum,1) )  ivon01 = ivon01 + 1         
!      ELEMENT (8) SHOULD EQUAL IRNUM + 100    ....                      
      if ( itest(12)  ==  iadn21(irnum,5) )  ivon01 = ivon01 + 1        
!      ELEMENT (12) SHOULD EQUAL IRNUM + 104   ....                      
      if ( itest(16)  ==  iadn21(irnum,9) )  ivon01 = ivon01 + 1        
!      ELEMENT (16) SHOULD EQUAL IRNUM + 108   ....                      
      if ( itest(20)  ==  iadn21(irnum,13) )  ivon01 = ivon01 + 1       
!      ELEMENT (20) SHOULD EQUAL IRNUM + 112   ....                      
      if ( itest(27)  ==  iadn21(irnum,20) )  ivon01 = ivon01 + 1       
!      ELEMENT (27) SHOULD EQUAL IRNUM + 119   ....                      
!      WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     
!      CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   
!      THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 
41200 if ( ivon01 - 6 )  21200, 11200, 21200                            
11200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1210                                                          !Break
21200 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1210 continue                                                          
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
!                                                                        
      if ( i  ==  8 )  goto 1221                                         !Break
!      THIS CODE IS TO SKIP READING PAST THE END OF FILE BY NOT READING  
!      FOUR RECORDS PAST RECORD NUMBER 29 ON THE 8TH LOOP....            
!                                                                        
      read ( i06,77753 )  itest                                         
!      READ FOUR RECORDS ON LUN I06....                                  
      irnum = irnum + 4                                                 
!      INCREMENT THE RECORD NUMBER COUNTER....                           
  end do
      if ( iczero )  31200, 1221, 31200                                 
31200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1221 continue                                                          
      ivtnum = 122                                                      
!                                                                        
!       ****  TEST 122  ****                                             
!      TEST 122  -  THIS CHECKS THE VALUE OF THE VARIABLE ITEST(27)      
!      ON RECORD NUMBER 30.  ELEMENT (20) SHOULD EQUAL  IADN31(30,2,10)  
!      WHICH SHOULD BE EQUAL TO 340  ....                                
!                                                                        
      if (iczero) 31220, 1220, 31220                                    
 1220 continue                                                          
      read ( i06,77752 )  itest                                         
      ivcomp = itest(27)                                                
      goto 41220                                                       
31220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41220, 1231, 41220                                    
41220 if ( ivcomp - 340 )  21220, 11220, 21220                          
11220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1231                                                        
21220 ivfail = ivfail + 1                                               
      ivcorr = 340                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1231 continue                                                          
      ivtnum = 123                                                      
!                                                                        
!       ****  TEST 123  ****                                             
!      TEST 123  -  THIS CHECKS THE VALUE OF VARIABLE ITEST(27) ON       
!      RECORD NUMBER 31 WHICH SHOULD EQUAL IADN21(31,20) = 31 + 20 + 99  
!      ITEST(27) SHOULD EQUAL 150  ....                                  
!                                                                        
      if (iczero) 31230, 1230, 31230                                    
 1230 continue                                                          
      read ( i06,77751) itest                                           
      ivcomp = itest(27)                                                
      goto 41230                                                       
31230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41230, 1241, 41230                                    
41230 if ( ivcomp - 150 )  21230, 11230, 21230                          
11230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1241                                                        
21230 ivfail = ivfail + 1                                               
      ivcorr = 150                                                      
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1241 continue                                                          
      ivtnum = 124                                                      
!                                                                        
!       ****  TEST 124  ****                                             
!      TEST 124  -  THIS CHECKS FOR THE PROPER 9999 EOF INDICATOR ON     
!      RECORD NUMBER 31  ....                                            
!                                                                        
      if (iczero) 31240, 1240, 31240                                    
 1240 continue                                                          
      ivcomp = itest(7)                                                 
      goto 41240                                                       
31240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41240, 1251, 41240                                    
41240 if ( ivcomp - 9999 )  21240, 11240, 21240                         
11240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1251                                                        
21240 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1251 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 08  
!      TO THE LINE PRINTER.                                              
! DB**                                                                   
!      ILUN = I06                                                        
!      ITOTR = 137                                                       
!      IRLGN = 80                                                        
! 7777 REWIND ILUN                                                       
!      IENDC = 0                                                         
!      IRCNT = 0                                                         
!      DO 7778  IRNUM = 1, ITOTR                                         
!      READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                
!      WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               
!      IRCNT = IRCNT + 1                                                 
!      IF ( IDUMP(20) .EQ. NINE )  IENDC = IRNUM                         
! 7778 CONTINUE                                                          
!      IF ( IENDC - 136 )   7780,  7779,  7782                           
! 7779 IF ( IRCNT - ITOTR )  7780, 7781, 7782                            
! 7780 WRITE (I02,77702) IRNUM,ILUN,ITOTR                                
!      GO TO  7784                                                       
! 7781 WRITE (I02,77703) ILUN,ITOTR                                      
!      GO TO  7784                                                       
! 7782 WRITE (I02,77704) ILUN, ITOTR                                     
!      DO  7783 I = 1, 5                                                 
!      READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                
!      WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               
!      IF ( IDUMP(20) .EQ. NINE )  GO TO   7784                          
! 7783 CONTINUE                                                          
! 7784 GO TO 99999                                                       
! DE**                                                                   
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
90007 format (" ",20x,"END OF PROGRAM FM107" )                          
      end program fm107
