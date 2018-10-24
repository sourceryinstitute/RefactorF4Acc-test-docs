      program fm103
!      COMMENT SECTION.                                                  
!                                                                        
!      FM103                                                             
!                                                                        
!          THIS ROUTINE IS A TEST OF THE X FORMAT AND IS TAPE AND PRINTER
!      ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  
!      AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      
!      OUTPUT LISTS ARE INTEGER OR REAL VARIABLES, INTEGER ARRAY ELEMENTS
!      OR ARRAY NAME REFERENCES.   READ AND WRITE STATEMENTS ARE DONE    
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
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: i09
      integer :: iprog
      integer :: ifile
      integer :: ilun
      integer :: itotr
      integer :: irlgn
      integer :: ieof
      integer :: icon01
      real :: rcon01
      integer :: icon02
      real :: rcon02
      integer :: iflip
      integer :: irnum
      integer :: ivtnum
      integer :: ivon01
      integer :: icon03
      integer :: icon05
      integer :: iend
      integer :: irno
      real :: rcon03
      integer :: ivcomp
      integer :: ivcorr
      character(len=1), dimension(1:136) :: idump
      character(len=1), dimension(1:5) :: iadn11
      character(len=2), dimension(1:3) :: iadn12
      character(len=3), dimension(1:3) :: iadn13
      character(len=1) :: nine
      character(len=1) :: icon04
      character(len=1) :: icon06
      data nine / '9' / 
      data iadn11 / 'A','B','C','D','E' / ,iadn12 / 'HE','LL','O' / ,iadn13 / 'H','EL','LO' / 
!                                                                        
77701 format ( 80a1 )                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," TOO LONG MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1)                                                 
77706 format (10x,"FILE I09 CREATED WITH 31 SEQUENTIAL RECORDS" )       
77751 format ( i3,2i2,3i3,i4,5x,i5,5x,f5.2,5x,5a1,5x,i5,5x,f5.4,5x,2a2,a1 )                                                               
77752 format ( i3,2i2,3i3,i4,i5,5x,f5.2,5x,5a1,5x,i5,5x,f5.4,5x,a1,2a2,5x )                                                               
77753 format (7x,i3,6x,i4,5x,i5,15x,a1,9x,i5,5x,f5.4,9x,a1 )            
77754 format (7x,i3,6x,i4,i5,15x,a1,9x,i5,5x,f5.4,9x,a1 )               
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
!      DEFAULT ASSIGNMENT FOR FILE 04 IS I09 = 7                         
      i09 = 7                                                           
! X090 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-090               
! X091 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-091               
!                                                                        
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I09 THAT IS 
!      80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF        
!      I, F, A, AND X FORMAT.   THIS IS THE ONLY FILE TESTED IN THE      
!      ROUTINE FM103 AND FOR PURPOSES OF IDENTIFICATION IS FILE 04.      
!      ALL ARRAY ELEMENT DATA FOR THE ALPHANUMERIC CHARACTERS IS SET BY  
!      THE DATA INITIALIZATION STATEMENT. INTEGER AND REAL VARIABLES ARE 
!      SET BY ASSIGNMENT STATEMENTS.                                     
      iprog = 103                                                       
      ifile = 04                                                        
      ilun = i09                                                        
      itotr = 31                                                        
      irlgn = 80                                                        
      ieof = 0000                                                       
      icon01 = 32767                                                    
      rcon01 = 12.34                                                    
      icon02 = 12345                                                    
      rcon02 = .9999                                                    
      iflip = 1                                                         
      do irnum = 1, 31                                              
      if ( irnum  ==  31 ) ieof = 9999                                  
      if ( iflip - 1 )  502, 502, 503                                   
  502 write ( i09, 77751 ) iprog, ifile, ilun, irnum, itotr, irlgn, ieof, icon01, rcon01, iadn11,icon02, rcon02, iadn12                   
      iflip = 2                                                         
      goto 504                                                         
  503 write ( i09, 77752 ) iprog, ifile, ilun, irnum, itotr, irlgn, ieof, icon01, rcon01, iadn11, icon02, rcon02, iadn13                  
      iflip = 1                                                         
  504 continue                                                          
      end do
      write (i02,77706)                                                 
!                                                                        
!      REWIND SECTION                                                    
!                                                                        
      rewind i09                                                        
!                                                                        
!      READ SECTION....                                                  
!                                                                        
!                                                                        
      ivtnum = 55                                                       
!                                                                        
!      ****    TEST  55  THRU  85    ****                                
!      TEST 55 THRU 85  -  THESE TESTS CHECK THE RECORD NUMBER AND       
!      CONTENTS OF SEVERAL OF THE DATA ITEMS WHICH REMAIN CONSTANT FOR   
!      ALL OF THE RECORDS.  A DIFFERENT USE OF THE X SKIP FIELD FORMAT   
!      IS USED IN READING THE FILE THAN WAS USED TO WRITE THE FILE.      
!                                                                        
      iflip = 1                                                         
      do irnum = 1, 31                                              
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 55 - 85.
      ivon01 = 0                                                        
!      READ THE FILE....                                                 
      if ( iflip - 1 )  552, 552, 553                                   
  552 read ( i09,77753 )  irno,iend,icon03,icon04,icon05,rcon03,icon06  
      iflip = 2                                                         
      goto 554                                                           !Break
  553 read ( i09,77754 )  irno,iend,icon03,icon04,icon05,rcon03,icon06  
      iflip = 1                                                         
  554 continue                                                          
      if ( irno  ==  irnum )  ivon01 = ivon01 + 1                       
!      IRNO SHOULD BE THE RECORD NUMBER....                              
      if ( icon03  ==  icon01 )  ivon01 = ivon01 + 1                    
!      ICON03 SHOULD EQUAL 32767 ....                                    
      if ( icon04  ==  iadn11(1) )  ivon01 = ivon01 + 1                 
!      ICON04 SHOULD EQUAL 'A'  ....                                     
      if ( icon05  ==  icon02 )  ivon01 = ivon01 + 1                    
!      ICON05 SHOULD EQUAL 12345  ....                                   
      if(rcon03 >=  .99985 .or. rcon03 <=  .99995) ivon01=ivon01+1      
!      RCON03 SHOULD EQUAL .9999  ....                                   
      if ( icon06  ==  iadn12(3) )  ivon01 = ivon01 + 1                 
!      ICON06 SHOULD EQUAL 'O'  ....                                     
      if ( ivon01 - 6 )  20550, 10550, 20550                            
!      WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     
!      CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   
!      THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 
10550 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 555                                                           !Break
20550 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  555 continue                                                          
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
   end do
      if ( iczero )  30550, 861, 30550                                  
30550 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
  861 continue                                                          
      ivtnum =  86                                                      
!                                                                        
!       ****  TEST  86  ****                                             
!      TEST 86  -  THIS TEST CHECKS THE END OF FILE INDICATOR ON THE     
!      31ST RECORD..                                                     
!                                                                        
      if (iczero) 30860,  860, 30860                                    
  860 continue                                                          
      ivcomp = iend                                                     
      goto 40860                                                       
30860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40860,  871, 40860                                    
40860 if ( ivcomp - 9999 )  20860, 10860, 20860                         
10860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  871                                                        
20860 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  871 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 04  
!      TO THE LINE PRINTER.                                              
! DB**                                                                   
!      ILUN = I09                                                        
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
90007 format (" ",20x,"END OF PROGRAM FM103" )                          
      end program fm103
