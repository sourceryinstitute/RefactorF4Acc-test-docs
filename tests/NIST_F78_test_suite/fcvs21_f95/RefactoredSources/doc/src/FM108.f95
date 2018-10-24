      program fm108
!      COMMENT SECTION.                                                  
!                                                                        
!      FM108                                                             
!                                                                        
!          THIS ROUTINE IS A TEST OF THE X FORMAT AND IS TAPE AND PRINTER
!      ORIENTED.  THE ROUTINE CAN NOT  BE USED FOR DISK.  BOTH THE READ  
!      AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      
!      OUTPUT LISTS ARE INTEGER OR REAL VARIABLES, INTEGER ARRAY ELEMENTS
!      OR ARRAY NAME REFERENCES.   READ AND WRITE STATEMENTS ARE DONE    
!      WITH FORMAT STATEMENTS.  THE ROUTINE HAS AN OPTIONAL SECTION OF   
!      CODE TO DUMP THE FILE AFTER IT HAS BEEN WRITTEN.  DO LOOPS AND    
!      DO-IMPLIED LISTS ARE USED IN CONJUNCTION WITH A ONE DIMENSIONAL   
!      INTEGER ARRAY FOR THE DUMP SECTION.                               
!                                                                        
!          WITH THE EXCEPTION OF THE RECORD PREAMBLES ON EACH RECORD,    
!      ALL OF THE I, F, AND A-FIELDS HAVE A MINUS SIGN IN THE LEFTMOST   
!      CHARACTER POSITION OF EACH FIELD.                                 
!                                                                        
!           THIS ROUTINE WRITES A SINGLE SEQUENTIAL FILE WHICH IS        
!      REWOUND AND READ SEQUENTIALLY FORWARD AND THEN READ SEQUENTIALLY  
!      BACKWARD BY USING THE BACKSPACE COMMAND.   THE FORWARD READ IS    
!      USED TO CHECK ALL OF THE ODD RECORDS AND THE READ REVERSE IN      
!      EFFECT CHECKS THE EVEN NUMBERED RECORDS.  THE ENDFILE COMMAND IS  
!      ALSO USED AFTER THE WRITE SECTION BUT BECAUSE THE RESULT OF       
!      ATTEMPTING TO READ OR READ BEYOND THE ENDFILE MARK IS NOT POSSIBLE
!      TO PREDICT FOR ALL MACHINES, THE ENDFILE  MARK IS NEVER ACTUALLY  
!      READ.                                                             
!                                                                        
!           THE LINE CONTINUATION IN COLUMN 6 IS USED IN  READ, WRITE,   
!      AND FORMAT STATEMENTS.  FOR BOTH SYNTAX AND SEMANTIC TESTS, ALL   
!      STATEMENTS SHOULD BE CHECKED VISUALLY FOR THE PROPER FUNCTIONING  
!      OF THE CONTINUATION LINE.                                         
!                                                                        
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
      integer :: i08
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
      real :: go
      real :: to
      integer :: i
      character(len=1), dimension(1:136) :: idump
      character(len=1), dimension(1:5) :: iadn11
      character(len=2), dimension(1:3) :: iadn12
      character(len=3), dimension(1:3) :: iadn13
      character(len=1) :: nine
      character(len=1) :: icon04
      character(len=2) :: icon06
      data nine / '9' / 
      data iadn11 / '-','W','H','E','E' / ,iadn12 / '-H','EL','L' / ,iadn13 / '-','HE','LL' / 
!                                                                        
77701 format ( 80a1 )                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," NO EOF.. MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1)                                                 
77706 format (10x,"FILE I08 CREATED WITH 31 SEQUENTIAL RECORDS" )       
77751 format ( i3,2i2,3i3,i4,4x,i6,4x,f6.2,5x,5a1,4x,i6,4x,f6.4,5x,2a2,a1 )                                                               
77752 format ( i3,2i2,3i3,i4,i6,4x,f6.2,4x,5a1,5x,i6,4x,f6.4,4x,a1,2a2,5x )                                                               
77753 format (7x,i3,6x,i4,4x,i6,15x,a1,8x,i6,4x,f6.4,9x,a1 )            
77754 format (7x,i3,6x,i4,i6,14x,a1,9x,i6,4x,f6.4,7x,a2,5x )            
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
!      DEFAULT ASSIGNMENT FOR FILE 09 IS I08 = 7                         
      i08 = 7                                                           
! X080 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-080               
! X081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               
!                                                                        
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I08 THAT IS 
!      80 CHARACTERS PER RECORD, 31 RECORDS LONG, AND CONSISTS OF        
!      I, F, A, AND X FORMAT.   THIS IS THE ONLY FILE TESTED IN THE      
!      ROUTINE FM108 AND FOR PURPOSES OF IDENTIFICATION IS FILE 09.      
!      ALL ARRAY ELEMENT DATA FOR THE ALPHANUMERIC CHARACTERS IS SET BY  
!      THE DATA INITIALIZATION STATEMENT. INTEGER AND REAL VARIABLES ARE 
!      SET BY ASSIGNMENT STATEMENTS.                                     
!                                                                        
      iprog = 108                                                       
      ifile = 09                                                        
      ilun = i08                                                        
      itotr = 31                                                        
      irlgn = 80                                                        
      ieof = 0000                                                       
      icon01 = -32766                                                   
      rcon01 = -12.34                                                   
      icon02 = -12345                                                   
      rcon02 = -.9999                                                   
      iflip = 1                                                         
      do irnum = 1, 31                                             
      if ( irnum  ==  31 ) ieof = 9999                                  
      if ( iflip - 1 )  1252, 1252, 1253                                
 1252 write ( i08, 77751 ) iprog, ifile, ilun, irnum, itotr, irlgn, ieof, icon01, rcon01, iadn11,icon02, rcon02, iadn12                   
      iflip = 2                                                         
      goto 1254                                                        
 1253 write ( i08, 77752 ) iprog, ifile, ilun, irnum, itotr, irlgn, ieof, icon01, rcon01, iadn11, icon02, rcon02, iadn13                  
      iflip = 1                                                         
 1254 continue                                                          
      end do
      write (i02,77706)                                                 
!                                                                        
!      ENDFILE SECTION ....                                              
      endfile i08                                                       
!                                                                        
!      REWIND SECTION                                                    
      rewind i08                                                        
!                                                                        
!                                                                        
!      READ FORWARD SECTION ....                                         
!                                                                        
!                                                                        
      ivtnum = 125                                                      
!                                                                        
!      ****    TEST  125  THRU  TEST  140    ****                        
!      TEST 125 THRU 140  -  THESE TESTS CHECK THE ODD NUMBERED RECORDS. 
!      THE FILE 09 IS READ SEQUENTIALLY FORWARD AND THE EVEN NUMBERED    
!      RECORDS ARE SKIPPED BY READING PAST THEM.                         
!                                                                        
      do irnum = 1, 31, 2                                         
      ivon01 = 0                                                        
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 125-140.
      read ( i08,77753 )  irno,iend,icon03,icon04,icon05,rcon03,icon06  
!      READ AN ODD NUMBERED RECORD....                                   
      if ( irno  ==  irnum )  ivon01 = ivon01 + 1                       
!      IRNO SHOULD BE THE RECORD NUMBER....                              
      if ( icon03  ==  icon01 )  ivon01 = ivon01 + 1                    
!      ICON03 SHOULD EQUAL -32766 ....                                   
      if ( icon04  ==  iadn11(1) )  ivon01 = ivon01 + 1                 
!      ICON04 SHOULD EQUAL '-'  ....                                     
      if ( icon05  ==  icon02 )  ivon01 = ivon01 + 1                    
!      ICON05 SHOULD EQUAL -12345 ....                                   
      if(rcon03 >=  -.99995 .or. rcon03 <=  -.99985)ivon01=ivon01+1     
!      RCON03 SHOULD EQUAL -.9999 ....                                   
      if ( icon06  ==  iadn12(3) )  ivon01 = ivon01 + 1                 
!      ICON06 SHOULD EQUAL 'L'  ....                                     
      if ( ivon01 - 6 )  21250, 11250, 21250                            
11250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1261                                                          !Break
21250 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1261 continue                                                          
      if ( irnum  ==  31 )   goto 1255                                 
!      THIS DOES NOT ALLOW READING THE ENDFILE MARK....                  
      read ( i08,77754 )  irno,iend,icon03,icon04,icon05,rcon03,icon06  
!      READ PAST THE EVEN NUMBERED RECORD ....                           
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
 1255 continue                                                          
      end do
      if ( iczero )  31250, 1411, 31250                                 
31250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1411 continue                                                          
      ivtnum = 141                                                      
!                                                                        
!      ****    TEST  141  THRU  TEST  155    ****                        
!      TEST 141 THRU 155  -  THESE TESTS USE THE BACKSPACE COMMAND       
!      TO READ REVERSE AND CHECK THE EVEN NUMBERED RECORDS.  AT THE      
!      BEGINNING OF THIS SERIES, THE FILE 09 SHOULD BE SETTING AT THE    
!      ENDFILE MARK PAST RECORD NUMBER 31.                               
!                                                                        
      backspace i08                                                     
      backspace i08                                                     
      irnum = 30                                                        
!      THE FILE SHOULD NOW BE SETTING AT RECORD NUMBER 30....            
      do i = 1, 15                                                 
      ivon01 = 0                                                        
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 141-155.
      read ( i08,77754 )  irno,iend,icon03,icon04,icon05,rcon03,icon06  
!      READ AN EVEN NUMBERED RECORD....                                  
      if ( irno  ==  irnum )  ivon01 = ivon01 + 1                       
!      IRNO SHOULD BE THE RECORD NUMBER....                              
      if ( icon03  ==  icon01 )  ivon01 = ivon01 + 1                    
!      ICON03 SHOULD EQUAL -32766 ....                                   
      if ( icon04  ==  iadn11(1) )  ivon01 = ivon01 + 1                 
!      ICON04 SHOULD EQUAL '-'  ....                                     
      if ( icon05  ==  icon02 )  ivon01 = ivon01 + 1                    
!      ICON05 SHOULD EQUAL -12345 ....                                   
      if(rcon03 >=  -.99995 .or. rcon03 <=  -.99985)ivon01=ivon01+1     
!      RCON03 SHOULD EQUAL -.9999 ....                                   
      if ( icon06  ==  iadn13(3) )  ivon01 = ivon01 + 1                 
!      ICON06 SHOULD EQUAL 'LL'  ....                                    
      if ( ivon01 - 6 )  21410, 11410, 21410                            
11410 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1421                                                          !Break
21410 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1421 continue                                                          
!      THIS IS TO NOT ALLOW READING BACKWARDS PAST RECORD NUMBER 1....   
      if ( i  ==  15 ) goto 1552                                       
!      BACKSPACE TO THE NEXT EVEN RECORD....                             
      backspace i08                                                     
      backspace i08                                                     
      backspace i08                                                     
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
      irnum = irnum - 2                                                 
!      DECREMENT THE RECORD NUMBER POINTER BY 2 ....                     
 1552 continue                                                          
      end do
      if ( iczero )  31410, 1561, 31410                                 
31410 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
 1561 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 09  
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
90007 format (" ",20x,"END OF PROGRAM FM108" )                          
      end program fm108
