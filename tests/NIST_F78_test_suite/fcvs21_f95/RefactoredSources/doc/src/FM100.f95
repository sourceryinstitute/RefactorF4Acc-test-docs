      program fm100
!      COMMENT SECTION.                                                  
!                                                                        
!      FM100                                                             
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
!         SECTION 13.5.9.1, INTEGER EDITING                              
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
      integer :: icon11
      integer :: icon12
      integer :: icon13
      integer :: icon14
      integer :: icon15
      integer :: icon16
      integer :: icon17
      integer :: icon18
      integer :: icon19
      integer :: icon10
      integer :: icon21
      integer :: icon22
      integer :: icon32
      integer :: icon41
      integer :: icon42
      integer :: icon43
      integer :: icon44
      integer :: icon45
      integer :: icon51
      integer :: icon52
      integer :: icon53
      integer :: icon54
      integer :: irnum
      integer :: icon31
      integer :: ivtnum
      integer :: irtst
      integer :: i
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      real :: go
      real :: to
      integer :: j
      integer, dimension(1:30) :: itest
      character(len=1), dimension(1:136) :: idump
      character(len=1) :: nine
      data nine / '9' / 
!                                                                        
77701 format ( 80a1 )                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," TOO LONG MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1)                                                 
77706 format (10x,"FILE I06 CREATED WITH 31 SEQUENTIAL RECORDS" )       
77751 format (i3,i2,i2,i3,i3,i3,i4,i1,i1,i1,i1,i1,i1,i1,i1,i1,i1,i2,i2,i3,i3,i4,i4,i4,i4,i4,i5,i5,i5,i5)                                  
      i01 = 5                                                           
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
!      DEFAULT ASSIGNMENT FOR FILE 01 IS I06 = 7                         
      i06 = 7                                                           
! X060 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-060               
! X061 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-061               
!                                                                        
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I06 THAT IS 
!      80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF ONLY   
!      INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      
!      ROUTINE FM100 AND FOR PURPOSES OF IDENTIFICATION IS FILE 01.      
!      ALL OF THE DATA WITH THE EXCEPTION OF THE RECORD NUMBER - IRNUM , 
!      INTEGER VARIABLE ICON31 WHICH IS SET TO THE VALUE OF THE RECORD   
!      NUMBER, AND THE END OF FILE CHECK - IEOF IS SET BY INTEGER        
!      ASSIGNMENT STATEMENTS TO VARIOUS INTEGER CONSTANTS.               
      iprog = 100                                                       
      ifile = 01                                                        
      ilun = i06                                                        
      itotr = 31                                                        
      irlgn = 80                                                        
      ieof = 0000                                                       
      icon11 = 1                                                        
      icon12 = 2                                                        
      icon13 = 3                                                        
      icon14 = 4                                                        
      icon15 = 5                                                        
      icon16 = 6                                                        
      icon17 = 7                                                        
      icon18 = 8                                                        
      icon19 = 9                                                        
      icon10 = 0                                                        
      icon21 = 21                                                       
      icon22 = 22                                                       
      icon32 = 512                                                      
      icon41 = 9995                                                     
      icon42 = 9996                                                     
      icon43 = 9997                                                     
      icon44 = 9998                                                     
      icon45 = 9999                                                     
      icon51 = 32764                                                    
      icon52 = 32765                                                    
      icon53 = 32766                                                    
      icon54 = 32767                                                    
      do irnum = 1, 31                                               
      icon31 = irnum                                                    
      if ( irnum  ==  31 ) ieof = 9999                                  
      write(i06,77751)iprog,ifile,ilun,irnum,itotr,irlgn,ieof,icon11,icon12,icon13,icon14,icon15,icon16,icon17,icon18,icon19,icon10,icon21,icon22,icon31,icon32,icon41,icon42,icon43,icon44,icon45,icon51,icon52,icon53,icon54                                                
    end do
      write (i02,77706)                                                 
!                                                                        
!      REWIND SECTION                                                    
!                                                                        
      rewind i06                                                        
!                                                                        
!      READ SECTION....                                                  
!                                                                        
      ivtnum =   1                                                      
!                                                                        
!       ****  TEST   1  THRU  TEST  8  ****                              
!      TEST 1  THRU  TEST 8  -  THESE TESTS READ THE SEQUENTIAL FILE     
!      PREVIOUSLY WRITTEN ON LUN I06 AND CHECK THE FIRST AND EVERY FOURTH
!      RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     
!      SEVERAL VALUES WHICH SHOULD REMAIN CONSTANT FOR ALL OF THE 31     
!      RECORDS.                                                          
!                                                                        
      irtst = 1                                                         
      read(i06,77751) itest                                             
!      READ THE FIRST RECORD....                                         
      do i = 1, 8                                                    
      ivon01 = 0                                                        
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 1 THRU 8
      if ( itest(4)  ==  irtst )  ivon01 = ivon01 + 1                   
!      THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                
      if ( itest(8)  ==  icon11 )  ivon01 = ivon01 + 1                  
!      THE ELEMENT (8) SHOULD EQUAL ICON11 = 1....                       
      if ( itest(18)  ==  icon21 )  ivon01 = ivon01 + 1                 
!      THE ELEMENT (18) SHOULD EQUAL ICON21 = 21....                     
      if ( itest(20)  ==  irtst )  ivon01 = ivon01 + 1                  
!      THE ELEMENT (20) SHOULD ALSO EQUAL THE RECORD NUMBER....          
      if ( itest(26)  ==  icon45 )  ivon01 = ivon01 + 1                 
!      THE ELEMENT (26. SHOULD EQUAL ICON45 = 9999....                   
      if ( itest(30)  ==  icon54 )  ivon01 = ivon01 + 1                 
!      THE ELEMENT (30) SHOULD EQUAL ICON54 = 32767....                  
      if ( ivon01 - 6 )  20010, 10010, 20010                            
!      WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     
!      CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   
!      THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 
10010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto   21                                                          !Break
20010 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
   21 continue                                                          
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
      if ( ivtnum  ==  9 )  goto 91                                      !Break
!      TAPE SHOULD BE AT RECORD NUMBER 29 FOR TEST 8  -  DO NOT READ MORE
!          UNTIL TEST NUMBER NINE WHICH CHECKS RECORD NUMBER 30....      
      do j = 1, 4                                                    
      read(i06,77751) itest                                             
!      READ FOUR RECORDS ON LUN I06....                                  
    end do
      irtst = irtst + 4                                                 
!      INCREMENT THE RECORD NUMBER COUNTER....                           
    end do
      if (iczero)  30010, 91, 30010                                     
30010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
   91 continue                                                          
      ivtnum =   9                                                      
!                                                                        
!       ****  TEST   9  ****                                             
!      TEST 9  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 30.   
!                                                                        
      if (iczero) 30090,   90, 30090                                    
   90 continue                                                          
      read ( i06, 77751 )  itest                                        
      ivcomp = itest(4)                                                 
      goto 40090                                                       
30090 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40090,  101, 40090                                    
40090 if ( ivcomp - 30 )  20090, 10090, 20090                           
10090 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  101                                                        
20090 ivfail = ivfail + 1                                               
      ivcorr = 30                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  101 continue                                                          
      ivtnum =  10                                                      
!                                                                        
!       ****  TEST  10  ****                                             
!      TEST 10  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 31.  
!                                                                        
      if (iczero) 30100,  100, 30100                                    
  100 continue                                                          
      read ( i06,77751) itest                                           
      ivcomp = itest(4)                                                 
      goto 40100                                                       
30100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40100,  111, 40100                                    
40100 if ( ivcomp - 31 )  20100, 10100, 20100                           
10100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  111                                                        
20100 ivfail = ivfail + 1                                               
      ivcorr = 31                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  111 continue                                                          
      ivtnum =  11                                                      
!                                                                        
!       ****  TEST  11  ****                                             
!      TEST 11  -  THIS CHECKS FOR THE CORRECT END OF FILE CODE 9999     
!      ON RECORD NUMBER 31.                                              
!                                                                        
      if (iczero) 30110,  110, 30110                                    
  110 continue                                                          
      ivcomp = itest(7)                                                 
      goto 40110                                                       
30110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40110,  121, 40110                                    
40110 if ( ivcomp - 9999 )  20110, 10110, 20110                         
10110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  121                                                        
20110 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  121 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 01  
!      TO THE LINE PRINTER.                                              
! DB**                                                                   
!      ILUN = I06                                                        
!      ITOTR = 31                                                        
!      IRLGN = 80                                                        
! 7777 REWIND ILUN                                                       
!      DO 7778  IRNUM = 1, ITOTR                                         
!      READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                
!      WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               
!      IF ( IDUMP(20) .EQ. NINE )  GO TO  7779                           
! 7778 CONTINUE                                                          
!      GO TO 7782                                                        
! 7779 IF ( IRNUM - ITOTR )   7780,  7781,  7782                         
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
90007 format (" ",20x,"END OF PROGRAM FM100" )                          
      end program fm100
