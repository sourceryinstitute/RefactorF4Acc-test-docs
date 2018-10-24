      program fm101
!      COMMENT SECTION.                                                  
!                                                                        
!      FM101                                                             
!                                                                        
!          THIS ROUTINE IS A TEST OF THE F FORMAT AND IS TAPE AND PRINTER
!      ORIENTED.  THE ROUTINE CAN ALSO BE USED FOR DISK.  BOTH THE READ  
!      AND WRITE STATEMENTS ARE TESTED.  VARIABLES IN THE INPUT AND      
!      OUTPUT LISTS ARE REAL VARIABLES AND REAL ARRAY ELEMENTS OR        
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
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: i07
      integer :: iprog
      integer :: ifile
      integer :: ilun
      integer :: itotr
      integer :: irlgn
      integer :: ieof
      real :: rcon21
      real :: rcon22
      real :: rcon31
      real :: rcon32
      real :: rcon33
      real :: rcon41
      real :: rcon42
      real :: rcon43
      real :: rcon44
      real :: rcon51
      real :: rcon52
      real :: rcon53
      real :: rcon54
      real :: rcon55
      real :: rcon61
      real :: rcon62
      real :: rcon63
      real :: rcon64
      real :: rcon65
      real :: rcon66
      integer :: irnum
      integer :: ivtnum
      integer :: irtst
      integer :: i
      integer :: ivon01
      integer :: ivcomp
      integer :: ivcorr
      real :: go
      real :: to
      integer :: j
      integer, dimension(1:7) :: itest
      real, dimension(1:20) :: rtest
      character(len=1), dimension(1:136) :: idump
      character(len=1) :: nine
      data nine / '9' / 
!                                                                        
77701 format ( 110a1)                                                   
77702 format (10x,"PREMATURE EOF ONLY " ,i3," RECORDS LUN " ,i2, " OUT OF ",i3," RECORDS")                                                
77703 format (10x,"FILE ON LUN " ,i2," OK... ",i3," RECORDS")           
77704 format (10x,"FILE ON LUN " ,i2," TOO LONG MORE THAN " ,i3, " RECORDS")                                                              
77705 format ( 1x,80a1 / 10x, 30a1)                                     
77706 format (10x,"FILE I07 CREATED WITH 31 SEQUENTIAL RECORDS" )       
77751 format (i3,2i2,3i3,i4,f2.0,f2.1,f3.0,f3.1,f3.2,f4.0,f4.1,f4.2,f4.3,f5.0,f5.1,f5.2,f5.3,f5.4,f6.0,f6.1,f6.2,f6.3,f6.4,f6.5 )         
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
      i07 = 7                                                           
!      DEFAULT ASSIGNMENT FOR FILE 02 IS I07 = 7                         
!                                                                        
! X070 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-070               
! X071 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-071               
!      WRITE SECTION....                                                 
!                                                                        
!      THIS SECTION OF CODE BUILDS A UNIT RECORD FILE ON LUN I07 THAT IS 
!      110 CHARS.    PER RECORD, 31 RECORDS LONG, AND CONSISTS OF ONLY   
!      REALS  ( F FORMAT ).  THIS IS THE ONLY FILE TESTED IN THE         
!      ROUTINE FM101 AND FOR PURPOSES OF IDENTIFICATION IS FILE 02.      
!      ALL OF THE DATA WITH THE EXCEPTION OF THE 20 CHARACTER INTEGER    
!      PREAMBLE FOR EACH RECORD, IS COMPRISED OF REAL VARIABLES SET BY   
!      REAL ASSIGNMENT STATEMENTS TO VARIOUS REAL CONSTANTS.             
!                                                                        
!           ALL THE THE REAL CONSTANTS USED ARE POSITIVE, I.E. NO SIGN.  
!                                                                        
      iprog = 101                                                       
      ifile = 02                                                        
      ilun = i07                                                        
      itotr = 31                                                        
      irlgn = 110                                                       
      ieof = 0000                                                       
      rcon21 = 9.                                                       
      rcon22 = .9                                                       
      rcon31 = 21.                                                      
      rcon32 = 2.1                                                      
      rcon33 = .21                                                      
      rcon41 = 512.                                                     
      rcon42 = 51.2                                                     
      rcon43 = 5.12                                                     
      rcon44 = .512                                                     
      rcon51 = 9995.                                                    
      rcon52 = 999.6                                                    
      rcon53 = 99.97                                                    
      rcon54 = 9.998                                                    
      rcon55 = .9999                                                    
      rcon61 = 32764.                                                   
      rcon62 = 3276.5                                                   
      rcon63 = 327.66                                                   
      rcon64 = 32.767                                                   
      rcon65 = 3.2768                                                   
      rcon66 = .32769                                                   
      do irnum = 1, 31                                              
      if ( irnum  ==  31 ) ieof = 9999                                  
      write(i07,77751)iprog,ifile,ilun,irnum,itotr,irlgn,ieof,rcon21,rcon22,rcon31,rcon32,rcon33,rcon41,rcon42,rcon43,rcon44,rcon51,rcon52,rcon53,rcon54,rcon55,rcon61,rcon62,rcon63,rcon64,rcon65,rcon66   
   end do
      write (i02,77706)                                                 
!                                                                        
!      REWIND SECTION                                                    
!                                                                        
      rewind i07                                                        
!                                                                        
!      READ SECTION....                                                  
!                                                                        
      ivtnum =  12                                                      
!                                                                        
!      ****  TEST    12  THRU    TEST  19  ****                          
!      TEST 12 THRU  TEST 19 -  THESE TESTS READ THE SEQUENTIAL FILE     
!      PREVIOUSLY WRITTEN ON LUN I07 AND CHECK THE FIRST AND EVERY FOURTH
!      RECORD.  THE VALUES CHECKED ARE THE RECORD NUMBER - IRNUM AND     
!      SEVERAL VALUES WHICH SHOULD REMAIN CONSTANT FOR ALL OF THE 31     
!      RECORDS.                                                          
!                                                                        
      irtst = 1                                                         
      read ( i07, 77751)  itest, rtest                                  
!      READ THE FIRST RECORD....                                         
      do i = 1, 8                                                   
      ivon01 = 0                                                        
!      THE INTEGER VARIABLE IS INITIALIZED TO ZERO FOR EACH TEST 1 THRU 8
      if ( itest(4)  ==  irtst )  ivon01 = ivon01 + 1                   
!      THE ELEMENT (4) SHOULD EQUAL THE RECORD NUMBER....                
!          THE TOLERANCE GIVEN IN THE REAL COMPARISONS IS BASED ON 16 BIT
!      MANTISSAS TO ALLOW FOR INPUT, OUTPUT, AND STORAGE CONVERSION,     
!      TRUNCATION, OR ROUNDING TECHNIQUES USED BY THE IMPLEMENTOR.       
      if(rtest(1)  >=  8.9995 .or. rtest(1)  <=  9.0005) ivon01=ivon01+1
!      THE ELEMENT(1) SHOULD EQUAL  RCON21 = 9.        ....              
      if(rtest(4)  >=  2.0995 .or. rtest(4)  <=  2.1005) ivon01=ivon01+1
!      THE ELEMENT( 4) SHOULD EQUAL RCON32 = 2.1       ....              
      if(rtest(9)  >=  .51195 .or. rtest(9)  <=  .51205) ivon01=ivon01+1
!      THE ELEMENT( 9) SHOULD EQUAL RCON44 = .512      ....              
      if ( rtest(13)  >=  9.9975 .or. rtest(13)  <=  9.9985 )            ivon01 = ivon01 + 1                                              
!      THE ELEMENT(13) SHOULD EQUAL RCON54 = 9.998     ....              
      if ( rtest(20)  >=  .32764 .or. rtest(20)  <=  .32774 )            ivon01 = ivon01 + 1                                              
!      THE ELEMENT(20) SHOULD EQUAL RCON66 = .32769    ....              
      if ( ivon01 - 6 )  20190, 10190, 20190                            
!      WHEN IVON01 = 6  THEN ALL SIX OF THE ITEST ELEMENTS THAT WERE     
!      CHECKED HAD THE EXPECTED VALUES....  IF IVON01 DOES NOT EQUAL 6   
!      THEN AT LEAST ONE OF THE VALUES WAS INCORRECT....                 
10190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  201                                                          !Break
20190 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 6                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  201 continue                                                          
      ivtnum = ivtnum + 1                                               
!      INCREMENT THE TEST NUMBER....                                     
      if ( ivtnum  ==  20 )  goto 194                                    !Break
!      TAPE SHOULD BE AT RECORD NUMBER 29 FOR TEST 19 -  DO NOT READ MORE
!          UNTIL TEST NUMBER 20   WHICH CHECKS RECORD NUMBER 30....      
      do j = 1, 4                                                   
      read ( i07, 77751)  itest, rtest                                  
!      READ FOUR RECORDS ON LUN I07....                                  
   end do
      irtst = irtst + 4                                                 
!      INCREMENT THE RECORD NUMBER COUNTER....                           
   end do
      if ( iczero )  30190, 194, 30190                                  
30190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
  194 continue                                                          
      ivtnum =  20                                                      
!                                                                        
!       ****  TEST  20  ****                                             
!      TEST 20 -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 30.   
!                                                                        
      if (iczero) 30200,  200, 30200                                    
  200 continue                                                          
      read ( i07, 77751)  itest, rtest                                  
      ivcomp = itest(4)                                                 
      goto 40200                                                       
30200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40200,  211, 40200                                    
40200 if ( ivcomp - 30 )  20200, 10200, 20200                           
10200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  211                                                        
20200 ivfail = ivfail + 1                                               
      ivcorr = 30                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  211 continue                                                          
      ivtnum =  21                                                      
!                                                                        
!       ****  TEST  21  ****                                             
!      TEST 21  -  THIS CHECKS THE RECORD NUMBER ON EXPECTED RECORD 31.  
!                                                                        
      if (iczero) 30210,  210, 30210                                    
  210 continue                                                          
      read ( i07, 77751)  itest, rtest                                  
      ivcomp = itest(4)                                                 
      goto 40210                                                       
30210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40210,  221, 40210                                    
40210 if ( ivcomp - 31 )  20210, 10210, 20210                           
10210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  221                                                        
20210 ivfail = ivfail + 1                                               
      ivcorr = 31                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  221 continue                                                          
      ivtnum =  22                                                      
!                                                                        
!       ****  TEST  22  ****                                             
!      TEST 22  -  THIS CHECKS FOR THE CORRECT END OF FILE CODE 9999     
!      ON RECORD NUMBER 31.                                              
!                                                                        
      if (iczero) 30220,  220, 30220                                    
  220 continue                                                          
      ivcomp = itest(7)                                                 
      goto 40220                                                       
30220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 40220,  231, 40220                                    
40220 if ( ivcomp - 9999 )  20220, 10220, 20220                         
10220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto  231                                                        
20220 ivfail = ivfail + 1                                               
      ivcorr = 9999                                                     
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
  231 continue                                                          
!      THIS CODE IS OPTIONALLY COMPILED AND IS USED TO DUMP THE FILE 02  
!      TO THE LINE PRINTER.                                              
! DB**                                                                   
!      ILUN = I07                                                        
!      ITOTR = 31                                                        
!      IRLGN = 110                                                       
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
90007 format (" ",20x,"END OF PROGRAM FM101" )                          
      end program fm101
