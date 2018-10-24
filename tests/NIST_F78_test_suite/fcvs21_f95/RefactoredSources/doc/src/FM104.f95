      program fm104
!      COMMENT SECTION.                                                  
!                                                                        
!      FM104                                                             
!                                                                        
!          THIS ROUTINE IS A TEST OF THE / FORMAT AND IS TAPE AND PRINTER
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
!      REWOUND AND READ SEQUENTIALLY FORWARD.   EVERY RECORD IS READ AND 
!      CHECKED DURING THE READ TEST SECTION FOR VALUES OF DATA ITEMS     
!      AND THE END OF FILE ON THE LAST RECORD IS ALSO CHECKED.           
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
!                                                                        
      integer, dimension(1:57) :: iacn11
      integer :: ichec
      integer, dimension(1:7) :: itest
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
      integer :: i
      integer :: irnum
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      integer :: ivtnum
      integer :: ivon01
      integer :: iend
      integer :: irn01
      integer :: irn02
      integer :: irn03
      integer :: irn04
      integer :: ivon06
      integer :: ivon07
      integer :: ivon08
      integer :: ivon09
      integer :: ivon10
      integer :: ivcomp
      integer :: ivcorr
      integer, dimension(1:7) :: iprem
      integer, dimension(1:57) :: iadn11
      character(len=1), dimension(1:136) :: idump
      character(len=1) :: nine
      character(len=1) :: izero
      data nine / '9' / ,izero / '0' / 
!                                                                        
77701 format ( 80a1 )                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," NO EOF.. MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1)                                                 
77706 format (10x,"FILE I06 CREATED WITH 28 SEQUENTIAL RECORDS" )       
77751 format (i3,2i2,3i3,i4,57i1,i3/i3,2i2,3i3,i4,57i1,i3/i3,2i2,3i3,i4,57i1,i3/i3,2i2,3i3,i4,57i1,i3 )                                   
77752 format (7x,i3,6x,i4,i1,56x,i3/7x,i3,67x,i3/7x,i3,67x,i3/7x,i3,67x,i3 )                                                              
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
!      DEFAULT ASSIGNMENT FOR FILE 05 IS I06 = 7                         
      i06 = 7                                                           
! X060 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-060               
! X061 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-061               
!                                                                        
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I06 THAT IS 
!      80 CHARACTERS PER RECORD, 28 RECORDS LONG, AND CONSISTS OF ONLY   
!      INTEGERS  ( I FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE      
!      ROUTINE FM104 AND FOR PURPOSES OF IDENTIFICATION IS FILE 05.      
!      SINCE THIS ROUTINE IS A TEST OF / IN A FORMAT STATEMENT, FOUR (4) 
!      RECORDS ARE ACTUALLY WRITTEN WITH ONE WRITE STATEMENT.  ALL FOUR  
!      OF THESE RECORDS WILL HAVE THE SAME RECORD NUMBER IN THE 20       
!      CHARACTER PREAMBLE.  THE INTEGER STORED IN CHARACTER POSITIONS    
!      78 - 80 WILL EQUAL    THE RECORD NUMBER PLUS 0, 1, 2, AND 3 FOR   
!      THE FOUR RECORD SET RESPECTIVELY..  THE INTEGER ARRAY ELEMENTS    
!      IN CHARACTER POSITIONS 21-77 WILL CONTAIN THE INTEGER DIGIT 9.    
      iprog = 104                                                       
      ifile = 05                                                        
      ilun = i06                                                        
      itotr = 28                                                        
      irlgn = 80                                                        
      ieof = 0000                                                       
!      SET THE RECORD PREAMBLE VALUES EXCEPT FOR RECORD NUMBER AND EOF.. 
      iprem(1) = iprog                                                  
      iprem(2) = ifile                                                  
      iprem(3) = ilun                                                   
      iprem(5) = itotr                                                  
      iprem(6) = irlgn                                                  
!      SET THE INTEGER ARRAY ELEMENTS TO THE INTEGER DIGIT 9             
      do i = 1, 57                                                   
      iadn11(i) = 9                                                     
    end do
      do irnum = 1, 7                                               
      if ( irnum  ==  7 )  ieof = 9999                                  
      iprem(4) = irnum                                                  
      iprem(7) = ieof                                                   
      ivon02 = irnum                                                    
      ivon03 = irnum + 1                                                
      ivon04 = irnum + 2                                                
      ivon05 = irnum + 3                                                
      write ( i06, 77751 ) iprog,ifile,ilun,irnum,itotr,irlgn,ieof,iadn11,ivon02,iprem,iadn11,ivon03,iprem,iadn11,ivon04,iprem,iadn11,ivon05                                                                
   end do
      write (i02,77706)                                                 
!                                                                        
!      REWIND SECTION                                                    
!                                                                        
      rewind i06                                                        
!                                                                        
!      READ SECTION....                                                  
!                                                                        
      ivtnum =  87                                                      
!                                                                        
!      ****    TEST  87  THRU  TEST  93    ****                          
!      TEST 87 THRU 93  -  THESE TESTS CHECK EVERY ONE OF THE 28 RECORDS 
!      CREATED AS FILE I06 FOR THE RECORD NUMBER, CONSTANT DATA ITEMS,   
!      AND THE END OF FILE INDICATOR.                                    
!                                                                        
      do irnum = 1, 7                                               
      ivon01 = 0                                                        
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 87 - 93.
      read  ( i06, 77752 )  irn01,iend,ivon06,ivon07,irn02,ivon08,irn03,ivon09,irn04,ivon10                                               
!      CHECK THE DATA ITEM VALUES  ....                                  
!      READ THE FILE I06  -  NOTE, FOUR RECORDS ARE READ IN EACH SINGLE  
!      READ STATEMENT AND THE FORMAT IS DIFFERENT THAN THE ONE USED TO   
!      CREATE THE FILE.                                                  
!                                                                        
      if ( irn01  ==  irnum )  ivon01 = ivon01 + 1                      
!      IRN01 SHOULD EQUAL THE RECORD NUMBER FOR THE SET OF FOUR RECORDS  
!      RECORD NUMBERS GO FROM 1 TO 7  ....                               
      if ( ivon06  ==  9 )  ivon01 = ivon01 + 1                         
!      IVON06 IS THE INTEGER ARRAY ELEMENT WHICH SHOULD BE ALWAYS EQUAL  
!      TO THE INTEGER CONSTANT 9  ....                                   
      if ( ivon07  ==  irnum )  ivon01 = ivon01 + 1                     
!      IVON07 SHOULD ALWAYS EQUAL THE RECORD NUMBER OF THE FIRST RECORD  
!      IN THE SET OF FOUR RECORDS  ....                                  
      if ( irn02  ==  irnum )  ivon01 = ivon01 + 1                      
!      THIS VALUE REMAINS CONSTANT FOR ALL FOUR RECORDS IN THE SET OF 4..
      if ( ivon08  ==  irnum + 1 )  ivon01 = ivon01 + 1                 
!      IVON08 IS THE 80TH CHARACTER IN THE SECOND RECORD OF THE SET OF 4.
      if ( irn03  ==  irnum )  ivon01 = ivon01 + 1                      
!      AGAIN THIS VALUE IS CONSTANT FOR THE SET OF FOUR RECORDS....      
      if ( ivon09  ==  irnum + 2 )  ivon01 = ivon01 + 1                 
!      IVON09 IS THE 80TH CHARACTER IN THE THIRD RECORD OF THE SET OF 4. 
      if ( irn04  ==  irnum )  ivon01 = ivon01 + 1                      
!      STILL EQUALS THE RECORD NUMBER FOR THE SET OF FOUR RECORDS.       
      if ( ivon10  ==  irnum + 3 )  ivon01 = ivon01 + 1                 
!      IVON10 IS THE 80TH CHARACTER IN THE FOURTH RECORD OF THE SET OF 4.
      if ( ivon01 - 9 )  20870, 10870, 20870                            
!      WHEN IVON01 = 9  THEN ALL NINE OF THE DATA ITEMS CHECKED ARE OK...
10870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  881                                                          !Break
20870 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 9                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  881 continue                                                          
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
   end do
      if ( iczero )  30870, 941, 30870                                  
30870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
  941 continue                                                          
      ivtnum =  94                                                      
!                                                                        
!       ****  TEST  94  ****                                             
!      TEST 94  -  THIS TEST CHECKS THE END OF FILE INDICATOR ON THE LAST
!      SET OF 4 RECORDS ( 25,26,27,AND 28 ).                             
!      THE VARIABLE  IEND  IS ACTUALLY IN THE RECORD NUMBERED 25.        
!                                                                        
      if (iczero) 30940,  940, 30940                                    
  940 continue                                                          
      ivcomp = iend                                                     
      goto 40940                                                       
30940 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40940,  951, 40940                                    
40940 if ( ivcomp - 9999 )  20940, 10940, 20940                         
10940 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  951                                                        
20940 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  951 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 05  
!      TO THE LINE PRINTER.                                              
! DB**                                                                   
!      ILUN = I06                                                        
!      ITOTR = 28                                                        
!      IRLGN = 80                                                        
! 7777 REWIND ILUN                                                       
!      DO 7778  IRNUM = 1, ITOTR                                         
!      READ (ILUN,77701) (IDUMP(ICHAR), ICHAR = 1, IRLGN)                
!      WRITE ( I02,77705) (IDUMP(ICHAR), ICHAR = 1, IRLGN)               
!      IF ( IDUMP(20) .EQ. NINE .AND. IDUMP(80) .EQ. IZERO )  GO TO 7779 
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
90007 format (" ",20x,"END OF PROGRAM FM104" )                          
      end program fm104
