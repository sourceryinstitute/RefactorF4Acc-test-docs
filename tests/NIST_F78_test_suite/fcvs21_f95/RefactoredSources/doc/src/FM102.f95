      program fm102
!      COMMENT SECTION.                                                  
!                                                                        
!      FM102                                                             
!                                                                        
!          THIS ROUTINE IS A TEST OF THE A FORMAT AND IS TAPE AND PRINTER
!      ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  
!      AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      
!      OUTPUT LISTS ARE ALPHANUMERIC INTEGERS AND ARRAY ELEMENTS OR      
!      ARRAY NAME REFERENCES.  ALL READ AND WRITE STATEMENTS ARE DONE    
!      WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   
!      CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    
!      DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   
!      INTEGER ARRAY FOR THE DUMP SECTION.                               
!                                                                        
!           THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        
!      REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY RECORD IS READ AND 
!      CHECKED FOR ACCURACY AND THE END OF FILE ON RECORD 31 IS ALSO     
!      CHECKED.  DURING THE READ AND CHECK PROCESS THE FILE IS REWOUND   
!      TWICE.  THE FIRST PASS CHECKS THE ODD NUMBERED RECORDS AND THE    
!      SECOND PASS CHECKS THE EVEN NUMBERED RECORDS.                     
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
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: i08
      integer :: iprog
      integer :: ifile
      integer :: ilun
      integer :: itotr
      integer :: irlgn
      integer :: ieof
      integer :: iflip
      integer :: irnum
      integer :: ivtnum
      integer :: irtst
      integer :: i
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      real :: go
      real :: to
      integer, dimension(1:7) :: itest
      character(len=1), dimension(1:60) :: iadn11
      character(len=2), dimension(1:30) :: iadn12
      character(len=1), dimension(1:136) :: idump
      character(len=1) :: nine
      character(len=1), dimension(1:60) :: iacn11
      character(len=2), dimension(1:30) :: iacn12
      data nine / '9' / 
      data iadn11 / '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',' ','=','+','-','*','/','(',')',',','.','*','0','*','1','.','2',',','3',')','4','(','5','/','6' / 
      data iadn12 / '6/','5(','4)','3,','2.','1*','0*','.,',')(','/*','-+','= ','ZY','XW','VU','TS','RQ','PO','NM','LK','JI','HG','FE','DC','BA','98','76','54','32','10' / 
77701 format ( 80a1 )                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," TOO LONG MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1)                                                 
77706 format (10x,"FILE I08 CREATED WITH 31 SEQUENTIAL RECORDS" )       
77751 format (i3, 2i2, 3i3, i4, 60a1 )                                  
77752 format ( i3, 2i2, 3i3, i4, 30a2 )                                 
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
!      DEFAULT ASSIGNMENT FOR FILE 03 IS I08 = 7                         
      i08 = 7                                                           
! X080 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-080               
! X081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               
!                                                                        
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I08 THAT IS 
!      80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF ONLY   
!      INTEGERS AND ALPHANUMERICS ( I AND A FORMAT ).  THIS ROUTINE HAS  
!      ONLY ONE FILE AND FOR PURPOSES OF IDENTIFICATION IS FILE 03.      
!      ALL ARRAY ELEMENT DATA FOR THE ALPHANUMERIC CHARACTERS IS SET BY  
!      THE DATA INITIALIZATION STATEMENT.                                
      iprog = 102                                                       
      ifile = 03                                                        
      ilun = i08                                                        
      itotr = 31                                                        
      irlgn = 80                                                        
      ieof = 0000                                                       
      iflip = 1                                                         
      do irnum = 1, 31                                              
      if ( irnum  ==  31 ) ieof = 9999                                  
      if ( iflip - 1 )  232, 232, 233                                   
  232 write ( i08, 77751 )  iprog,ifile,ilun,irnum,itotr,irlgn,ieof,iadn11                                                                
      iflip = 2                                                         
      goto 234                                                         
  233 write ( i08, 77752 ) iprog,ifile,ilun,irnum,itotr,irlgn,ieof,iadn12                                                                 
      iflip = 1                                                         
  234 continue                                                          
      end do
      write (i02,77706)                                                 
!                                                                        
!      REWIND SECTION                                                    
!                                                                        
      rewind i08                                                        
!                                                                        
!      READ SECTION....                                                  
!                                                                        
      ivtnum =  23                                                      
!                                                                        
!      ****  TEST  23  THRU  TEST  38  ****                              
!      TEST 23 THRU 38  -  THESE TESTS READ THE FILE SEQUENTIALLY FORWARD
!      AND CHECK THE ODD NUMBERED RECORDS FOR THE RECORD NUMBER AND THE  
!      VALUE OF SEVERAL OF THE DATA ITEMS WHICH SHOULD REMAIN CONSTANT   
!      FROM RECORD TO RECORD.                                            
!                                                                        
      irtst = 1                                                         
      do i = 1, 16                                                  
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 23 - 38.
      ivon01 = 0                                                        
!      READ AN ODD NUMBERED RECORD....                                   
      read ( i08, 77751 )  itest, iacn11                                
      if ( itest(4)  ==  irtst )  ivon01 = ivon01 + 1                   
!      THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                
      if ( iacn11(1)  ==  iadn11(1) )  ivon01 = ivon01 + 1              
!      THE ELEMENT (1) SHOULD EQUAL IADN11(1) = '0'   ....               
      if ( iacn11(11)  ==  iadn11(11) )  ivon01 = ivon01 + 1            
!      THE ELEMENT (11) SHOULD EQUAL IADN11(11) = 'A' ....               
      if ( iacn11(36)  ==  iadn11(36) )  ivon01 = ivon01 + 1            
!      THE ELEMENT (36) SHOULD EQUAL IADN11(36) = 'Z' ....               
      if ( iacn11(44)  ==  iadn11(44) )  ivon01 = ivon01 + 1            
!      THE ELEMENT (44) SHOULD EQUAL IADN11(44) = ')' ....               
      if ( iacn11(60)  ==  iadn11(60) ) ivon01 = ivon01 + 1             
!      THE ELEMENT (60) SHOULD EQUAL IADN11(60) = '6' ....               
      if ( ivon01 - 6 )  20230, 10230, 20230                            
!      WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     
!      CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   
!      THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 
10230 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 382                                                           !Break
20230 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  382 continue                                                          
!      DO NOT READ PAST RECORD NUMBER 31 ON THE I = 16 LOOP....          
      if ( i  ==  16 )  goto 391                                         !Break
!      SKIP OVER THE EVEN NUMBERED RECORDS BY AN EXTRA READ....          
      read ( i08, 77752 )  itest, iacn12                                
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
      irtst = irtst + 2                                                 
!      INCREMENT THE RECORD NUMBER COUNTER....                           
   end do
      if ( iczero )  30230, 391, 30230                                  
30230 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
  391 continue                                                          
      ivtnum =  39                                                      
!                                                                        
!       ****  TEST  39  ****                                             
!      TEST 39  -  THIS CHECKS FOR THE END OF FILE INDICATOR ON THE 31ST 
!      RECORD.  THE EOF INDICATOR IS ITEST(7) AND SHOULD EQUAL 9999      
!                                                                        
      if (iczero) 30390,  390, 30390                                    
  390 continue                                                          
      ivcomp = itest(7)                                                 
      goto 40390                                                       
30390 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40390,  401, 40390                                    
40390 if ( ivcomp - 9999 )  20390, 10390, 20390                         
10390 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  401                                                        
20390 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  401 continue                                                          
!      REWIND THE FILE AGAIN....                                         
      rewind i08                                                        
!      READ THE FILE AGAIN                                               
      ivtnum =  40                                                      
!      ****  TEST 40  THRU  54  ****                                     
!      TEST 40 THRU 54  -  THESE TESTS CHECK THE EVEN NUMBERED RECORDS   
!      FOR THE CORRECT RECORD NUMBER AND THE VALUE OF SEVERAL DATA ITEMS 
!      WHICH SHOULD REMAIN CONSTANT FOR EACH RECORD.  THESE READ CHECKS  
!      USE A DIFFERENT FORMAT THAN TESTS 23 THRU 38 BECAUSE THE RECORDS  
!      WERE WRITTEN USING A FLIP-FLOP BETWEEN TWO FORMATS.               
!                                                                        
      irtst = 2                                                         
!      THIS RECORD POINTER IS INITIALIZED TO THE SECOND (EVEN) RECORD    
      do i = 1, 15                                                  
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 40 - 54 
      ivon01 = 0                                                        
!      SKIP OVER THE ODD NUMBERED RECORDS....                            
      read ( i08, 77751 )  itest, iacn11                                
!      READ THE EVEN NUMBERED RECORDS....                                
      read ( i08, 77752 )  itest, iacn12                                
      if ( itest(4)  ==  irtst )  ivon01 = ivon01 + 1                   
!      CHECK THE RECORD NUMBER....                                       
      if ( iacn12(1)  ==  iadn12(1) )  ivon01 = ivon01 + 1              
!      ELEMENT (1) SHOULD EQUAL '6/'    ....                             
      if ( iacn12(11)  ==  iadn12(11) )  ivon01 = ivon01 + 1            
!      ELEMENT (11) SHOULD EQUAL '-+'   ....                             
      if ( iacn12(16)  ==  iadn12(16) )  ivon01 = ivon01 + 1            
!      ELEMENT (16) SHOULD EQUAL 'TS'   ....                             
      if ( iacn12(23)  ==  iadn12(23) )  ivon01 = ivon01 + 1            
!      ELEMENT (23) SHOULD EQUAL 'FE'   ....    (THE SYMBOL FOR IRONY)   
      if ( iacn12(30)  ==  iadn12(30) )  ivon01 = ivon01 + 1            
!      ELEMENT (30) SHOULD EQUAL '10'   ....                             
      if ( ivon01 - 6 )  20400, 10400, 20400                            
10400 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 402                                                           !Break
20400 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  402 continue                                                          
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
      irtst = irtst + 2                                                 
!      INCREMENT THE RECORD NUMBER COUNTER....                           
   end do
      if ( iczero )  30400, 411, 30400                                  
30400 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
  411 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 03  
!      TO THE LINE PRINTER.                                              
! DB**                                                                   
!      ILUN = I08                                                        
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
90007 format (" ",20x,"END OF PROGRAM FM102" )                          
      end program fm102
