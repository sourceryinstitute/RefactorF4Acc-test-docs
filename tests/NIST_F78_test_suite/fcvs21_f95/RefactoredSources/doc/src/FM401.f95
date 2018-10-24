      program fm401
!                                                                        
!                                                                        
!         THIS ROUTINE TESTS FOR PROPER EDITING OF LOGICAL DATA BY       
!      THE L EDIT DESCRIPTOR OF THE FORMAT SPECIFICATION.  THE L EDIT    
!      DESCRIPTOR IS FIRST TESTED FOR PROPER EDITING ON OUTPUT BY        
!      DIRECTING THE EDITED RESULT TO A PRINT FILE.  THE RESULTS MUST    
!      BE VISUALLY CHECKED FOR CORRECTNESS  BY  EXAMINING THE EXECUTION  
!      REPORT PRODUCED BY THIS ROUTINE. NEXT A NONPRINTER FILE WHICH     
!      IS CONNECTED FOR SEQUENTIAL ACCESS IS CREATED WITH LOGICAL DATA   
!      FIELDS AND THEN REPOSITIONED TO THE FIRST RECORD IN THE FILE.     
!      THE FILE IS THEN READ USING THE SAME EDIT DESCRIPTORS AS WERE     
!      USED TO CREATE THE FILE AND THE INTERNAL DATA REPRESENTATION AS A 
!      RESULT OF READING THE LOGICAL DATA IS CHECKED.                    
!         THE FOLLOWING L EDITING TESTS ARE MADE TO SEE THAT             
!                                                                        
!           (1) THE VALUE T OR F IS PRODUCED ON OUTPUT WHEN THE INTERNAL 
!               DATUM IS TRUE AND FALSE RESPECTIVELY,                    
!           (2) THE VALUE OF THE INPUT LIST ITEM IS TRUE OR FALSE        
!               WHEN THE INPUT FIELD IS T AND F RESPECTIVELY,            
!           (3) THE VALUES .T, .F,   T,    F, .TRUE., .FALSE.,   .T, AND 
!                  .F ARE ACCEPTABLE FORMS FOR INPUT DATA FIELDS         
!           (4) THE INPUT VALUES T OR F MAY BE FOLLOWED BY               
!               ADDITIONAL CHARACTERS IN THE FIELD,                      
!           (5) THE REPEATABLE  EDIT DESCRIPTOR FOR L EDITING FUNCTIONS  
!               CORRECTLY,                                               
!           (6) THE FIELDS CONTAINING LOGICAL DATA CAN BE WRITTEN        
!               USING ONE  L EDIT DESCRIPTOR AND READ USING A DIFFERENT  
!               FORM OF THE L EDIT DESCRIPTOR.                           
!                                                                        
!      REFERENCES -                                                      
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1978                                                 
!                                                                        
!         SECTION 4.7,      LOGICAL TYPE                                 
!         SECTION 13.1.1,   FORMAT STATEMENT                             
!         SECTION 13.5.10,  L EDITING                                    
!                                                                        
!                                                                        
!                                                                        
!      ******************************************************************
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   
!      X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 
!      FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       
!      ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT
!      ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   
!      OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING
!      THE RESULT OF EXECUTING THESE TESTS.                              
!                                                                        
!      THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      
!      FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        
!                                                                        
!            SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!      ******************************************************************
!                                                                        
!                                                                        
!                                                                        
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      logical  :: lcon01
      logical  :: lcon02
      logical  :: lcon03
      integer :: i08
      integer :: iprog
      integer :: ifile
      integer :: itotr
      integer :: irlgn
      integer :: irecn
      integer :: ieof
      integer :: i
      logical  :: lvon01
      logical  :: lvon02
      integer :: ivcomp
      integer :: ivcorr
      logical, dimension(1:5) :: laon15
      logical, dimension(1:2) :: laon12
      integer, dimension(1:132) :: idump
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION.                                           
!                                                                        
!      INITIALIZE CONSTANTS                                              
!      ********************                                              
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010     THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD.
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD.
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass = 0                                                        
      ivfail = 0                                                        
      ivdele = 0                                                        
      iczero = 0                                                        
!                                                                        
!      WRITE OUT PAGE HEADERS                                            
!                                                                        
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90008)                                                 
      write (i02,90004)                                                 
      write (i02,90010)                                                 
      write (i02,90004)                                                 
      write (i02,90016)                                                 
      write (i02,90001)                                                 
      write (i02,90004)                                                 
      write (i02,90012)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
!                                                                        
!                                                                        
!                                                                        
!         TEST 001 THROUGH 007 TESTS THE L EDIT DESCRIPTOR FOR PROPER    
!      EDITING OF LOGICAL DATUM ON OUTPUT.  TO VALIDATE THESE TESTS      
!      THE EDITED DATUM IS SENT TO A PRINT FILE AND THEREFORE MUST BE    
!      VISUALLY CHECKED FOR CORRECTNESS.  ON OUTPUT THE EDITED FIELD     
!      CONSISTS OF W-1 (W IS NUMBER OF POSITIONS IN THE FIELD) BLANKS    
!      FOLLOWED BY A T OR F AS THE VALUE OF THE DATUM IS TRUE OR FALSE   
!      RESPECTIVELY.  SEE SECTION  13.5.10 L EDITING.                    
!                                                                        
!                                                                        
80052 format (" ",4x,  "TESTS 001 THROUGH 007 MUST BE VISUALLY VERIFIED.")                                                                
80054 format (" ",  "IMMEDIATELY FOLLOWING THIS NARRATIVE IS A REFERENCE LINE")                                                           
80056 format (" ",  "OF THE FORM '123456 ...'.   THE REFERENCE LINE IS TO")                                                               
80058 format (" ","AID IN THE VISUAL VERIFICATION OF THE TESTS.  FOR" ) 
80062 format (" ","THE OUTPUT TO BE CORRECT THE DATA VALUES DISPLAYED" )
80064 format (" ",  "IN THE COMPUTED COLUMN MUST MATCH THAT IN THE CORRECT ")                                                             
80066 format (" ","COLUMN IN BOTH VALUE AND CHARACTER POSITION." )      
80072 format (" ","REFERENCE LINE     -      " ,"1234567890" ,5x,  "1234567890")                                                          
      write (i02,80052)                                                 
      write (i02,80054)                                                 
      write (i02,80056)                                                 
      write (i02,80058)                                                 
      write (i02,80062)                                                 
      write (i02,80064)                                                 
      write (i02,80066)                                                 
      write (i02,90004)                                                 
      write (i02,80072)                                                 
!                                                                        
!      ****  FCVS PROGRAM  401  -  TEST 001  ****                        
!                                                                        
!         TEST 001 TESTS FOR PROPER EDITING OF THE L EDIT DESCRIPTOR     
!      ON OUTPUT WHERE THE FIELD IS 1 POSITION IN LENGTH, THE            
!      VALUE OF THE DATUM IS TRUE AND THE OUTPUT LIST ITEM IS A          
!      VARIABLE.                                                         
!                                                                        
      ivtnum = 001                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      lcon01 = .true.                                                   
 0012 format (" ",4x,i5,26x,l1,14x,"T")                                 
      write (i02, 0012) ivtnum, lcon01                                  
      goto 0021                                                        
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM  401  -  TEST 002  ****                        
!                                                                        
!         TEST 002 IS SIMILAR TO TEST 001 EXCEPT THAT THE OUTPUT LIST    
!      ITEM IS AN ARRAY ELEMENT.                                         
!                                                                        
      ivtnum = 002                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      laon12(2) = .true.                                                
 0022 format (" ",4x,i5,26x,l1,14x,"T")                                 
      write (i02, 0022) ivtnum, laon12(2)                               
      goto 0031                                                        
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
 0031 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM  401  -  TEST 003  ****                        
!                                                                        
!         TEST 003 TESTS TO SEE THAT ON OUTPUT 9 BLANKS PRECEDE THE VALUE
!      T WHERE THE L EDIT DESCRIPTOR INDICATES THAT THE FIELD OCCUPIES   
!      10 POSITIONS.  THE VALUE OF THE INTERNAL DATUM IS TRUE.           
!                                                                        
      ivtnum = 003                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      lcon01 = .true.                                                   
 0032 format (" ",4x,i5,17x,l10,5x,"         T" )                       
      write (i02, 0032) ivtnum, lcon01                                  
      goto 0041                                                        
30030 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0041 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM  401  -  TEST 004  ****                        
!                                                                        
!         TEST 004 TESTS TO SEE THAT THE VALUE F IS PRODUCED ON OUTPUT   
!      WHEN THE VALUE OF THE INTERNAL DATUM IS FALSE AND THE L EDITING   
!      FIELD IS 1 POSITION IN LENGTH.                                    
!                                                                        
      ivtnum = 004                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      lcon02 = .false.                                                  
 0042 format (" ",4x,i5,26x,l1,14x,"F")                                 
      write (i02, 0042) ivtnum, lcon02                                  
      goto 0051                                                        
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM  401  -  TEST 005  ****                        
!                                                                        
!         TEST 005 VERIFIES THAT ON OUTPUT 9 BLANKS PRECEDE THE VALUE F  
!      WHERE THE L EDIT DESCRIPTOR IS L10 (FIELD OCCUPIES 10 POSITIONS). 
!      THE VALUE OF THE INTERNAL DATUM IS FALSE.                         
!                                                                        
      ivtnum = 005                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      lcon02 = .false.                                                  
 0052 format (" ",4x,i5,17x,l10,5x,"         F" )                       
      write (i02, 0052) ivtnum, lcon02                                  
      goto 0061                                                        
30050 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM  401  -  TEST 006  ****                        
!                                                                        
!         TEST 006 TESTS THE OPTIONAL REPEAT SPECIFICATION OF THE L      
!      EDIT DESCRIPTOR WHERE THE FIELD OCCUPIES 1 POSITION  (EDIT        
!      DESCRIPTOR IS 5L1).                                               
!                                                                        
      ivtnum = 006                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      lcon01 = .true.                                                   
      lcon02 = .false.                                                  
      lcon03 = .false.                                                  
      laon12(1) = .false.                                               
      laon12(2) = .true.                                                
 0062 format (" ",4x,i5,17x,"     ",5l1,5x,"     TFFFT" )               
      write (i02, 0062) ivtnum, lcon01, lcon02, lcon03, laon12(1),      laon12(2)                                                         
      goto 0071                                                        
30060 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0071 continue                                                          
!                                                                        
!      ***  FCVS PROGRAM  401  -  TEST 007  ****                         
!                                                                        
!         TEST 007 TESTS THE OPTIONAL REPEAT SPECIFICATION  OF THE L     
!      EDIT DESCRIPTOR WHERE THE FIELD OCCUPIES 3 POSITIONS (EDIT        
!      DESCRIPTOR IS 3L3).                                               
!                                                                        
      ivtnum = 007                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      lcon01    = .true.                                                
      lcon02    = .false.                                               
      laon12(2) = .true.                                                
 0072 format (" ",4x,i5,17x," ",3l3,5x,"   T  F  T" )                   
      write (i02, 0072)  ivtnum, lcon01, lcon02, laon12(2)              
      goto 0081                                                        
30070 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0081 continue                                                          
!                                                                        
!         THE FOLLOWING BLOCK OF SOURCE CODE BEGINNING WITH COMMENT LINE 
!      **** CREATE-FILE SECTION AND ENDING WITH THE COMMENT LINE         
!      **** END-OF-CREATE-FILE SECTION BUILDS A FILE WHICH IS USED IN    
!      TESTING THE L EDIT DESCRIPTOR.  THE FILE PROPERTIES ARE           
!                                                                        
!               FILE IDENTIFIER     - I08 (X-NUMBER 08)                  
!               RECORD SIZE         - 80 CHARACTERS                      
!               ACCESS METHOD       - SEQUENTIAL                         
!               RECORD TYPE         - FORMATTED                          
!               DESIGNATED DEVICE   - DISK                               
!               TYPE OF DATA        - LOGICAL (L FORMAT)                 
!               RECORDS IN FILE     - 141                                
!                                                                        
!         THE FIRST 20 POSITIONS OF EACH RECORD IN THE FILE UNIQUELY     
!      IDENTIFY   THAT RECORD.  THE REMAINING POSITONS OF THE RECORD     
!      CONTAIN DATA WHICH IS USED IN TESTING THE L EDIT DESCRIPTOR.      
!      A DESCRIPTION OF EACH FIELD OF THE 20-CHARACTER PREAMBLE FOLLOWS. 
!                                                                        
!                 VARIABLE NAME IN PROGRAM     CHARACTER POSITIONS       
!                -----------------------    -------------------          
!                                                                        
!               IPROG  (ROUTINE NAME)         -     1 THRU  3            
!               IFILE  (LOGICAL/ X-NUMBER)    -     4 THRU  5            
!               ITOTR  (RECORDS IN FILE)      -     6  THRU  9           
!               IRLGN  (CHARACTERS IN RECORD) -    10 THRU 12            
!               IRECN  (RECORD NUMBER)        -    13 THRU 16            
!               IEOF   (9999 IF LAST RECORD)  -    17 THRU 20            
!                                                                        
!      DEFAULT ASSIGNMENT FOR FILE IS I08 = 07                           
      i08 = 07                                                          
! X080 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-080               
! X081 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-081               
      iprog = 401                                                       
      ifile = i08                                                       
      itotr = 141                                                       
      irlgn = 80                                                        
      irecn = 0                                                         
      ieof  = 0                                                         
!                                                                        
!         THERE ARE 10 SETS OF 14 RECORDS PER SET PLUS ONE               
!      TRAILER RECORD FOR A TOTAL OF 141 DATA RECORDS IN THE FILE.       
!      ALTHOUGH ONLY 12 RECORDS ARE USED IN TESTING, THE FILE IS MADE    
!      LARGER TO PRECLUDE THE FILE FROM BEING TOTALY STORED IN MEMORY    
!      DURING EXECUTION OF THIS ROUTINE.                                 
!                                                                        
!                                                                        
!                                                                        
!  ****  CREATE-FILE SECTION                                             
      lcon01 = .true.                                                   
      lcon02 = .false.                                                  
70001 format (i3,i2,i4,i3,2i4,58x,"T","F")                              
70002 format (i3,i2,i4,i3,2i4,40x,"         T" ,"         F" )          
70003 format (i3,i2,i4,i3,2i4,47x,".TRUE.",".FALSE.")                   
70004 format (i3,i2,i4,i3,2i4,56x,".T",".F")                            
70005 format (i3,i2,i4,i3,2i4,48x,"    .T","    .F")                    
70006 format (i3,i2,i4,i3,2i4,38x,"THIS IS ALLOWED" ,"FINALLY")         
70007 format (i3,i2,i4,i3,2i4,48x,"TRUE  ","FALSE ")                    
70008 format (i3,i2,i4,i3,2i4,40x,"  .TIME.  " ,"  .FIELD. " )          
70009 format (i3,i2,i4,i3,2i4,07x,  "THIS IS VERY LARGE FIELD FOR INPUT OF LOGICAL VALUES.")                                              
70010 format (i3,i2,i4,i3,2i4,55x,"TFTFT")                              
70011 format (i3,i2,i4,i3,2i4,44x,"   T   T   F   F" )                  
70012 format (i3,i2,i4,i3,2i4,55x,l5)                                   
70013 format (i3,i2,i4,i3,2i4,55x,4x,l1)                                
70014 format (i3,i2,i4,i3,2i4,59x," ")                                  
      do i=1,10                                                    
      irecn  = irecn + 1                                                
      write (i08, 70001) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70002) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70003) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70004) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70005) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70006) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70007) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70008) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70009) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70010) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70011) iprog, ifile, itotr, irlgn, irecn, ieof        
      irecn  = irecn  + 1                                               
      write (i08, 70012) iprog, ifile, itotr, irlgn, irecn, ieof, lcon01
      irecn  = irecn  + 1                                               
      write (i08, 70012) iprog, ifile, itotr, irlgn, irecn, ieof, lcon02
      irecn  = irecn  + 1                                               
      write (i08, 70013) iprog, ifile, itotr, irlgn, irecn, ieof, lcon01
  end do
      irecn  = irecn  + 1                                               
      ieof = 9999                                                       
      write (i08, 70014) iprog, ifile, itotr, irlgn, irecn, ieof        
      endfile i08                                                       
      rewind i08                                                        
      write (i02, 90004)                                                
70015 format ("   FILE I08 HAS BEEN CREATED AND CONTAINS 141 RECORDS" ) 
70016 format (" ","INCORRECT NUMBER OF RECORDS IN FILE - " ,  i4 ,  " RECORDS")                                                           
70017 format (" ","WRITTEN BUT 141 RECORDS SHOULD HAVE BEEN WRITTEN." ) 
      if (irecn - 141) 4013, 4014, 4013                                 
 4013 write (i02, 70016) irecn                                          
      write (i02, 70017)                                                
      goto 4015                                                        
 4014 write (i02, 70015)                                                
      write (i02, 90004)                                                
 4015 continue                                                          
!                                                                        
!  **** END-OF-CREATE-FILE SECTION                                       
!                                                                        
!                                                                        
!                                                                        
!      TEST 8 AND 9 VERIFY THAT ON INPUT THE VALUE T AND F IS TRUE       
!      AND FALSE RESPECTIVELY. THE FIELD IS ONE POSITION IN LENGTH AND   
!      USES THE EDIT DESCRIPTOR L1.                                      
!                                                                        
!                                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0082 format (78x,l1,l1)                                                
      read (i08, 0082) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT IS FOR TESTS 8  
!      AND 9                                                             
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 008  ****                         
!                                                                        
!                                                                        
!         TEST 8 TESTS THE FIELD VALUE T FOR A TRUE CONDITION.           
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40080 if (ivcomp - 1) 20080, 10080, 20080                               
30080 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10080, 0091, 20080                                    
10080 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0091                                                        
20080 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0091 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 009  ****                         
!                                                                        
!                                                                        
!         TEST 9 TESTS THE VALUE F FOR A FALSE CONDITION                 
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40090 if (ivcomp - 0) 20090, 10090, 20090                               
30090 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10090, 0101, 20090                                    
10090 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0101                                                        
20090 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0101 continue                                                          
!                                                                        
!                                                                        
!         THE INPUT FIELD MAY CONSIST OF OPTIONAL BLANKS FOLLOWED BY T OR
!      F. TEST 10 AND 11 VERIFY THAT THE VALUE T OR F PRECEDED BY BLANKS 
!      ON INPUT IS TRUE OR FALSE RESPECTIVELY.  THE EDIT DESCRIPTOR BEING
!      TESTED IS L10 (INPUT FIELD HAS 10 POSITIONS).                     
!                                                                        
!                                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0102 format (60x,l10,l10)                                              
      read (i08, 0102) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED  FORMAT STATEMENT IS FOR TESTS 10
!      AND 11                                                            
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 010  ****                         
!                                                                        
!                                                                        
!         TEST 10 TESTS A FIELD OF BLANKS FOLLOWED BY A T FOR A TRUE     
!      CONDITION.                                                        
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40100 if (ivcomp - 1) 20100, 10100, 20100                               
30100 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10100, 0111, 20100                                    
10100 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0111                                                        
20100 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0111 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 011  ****                         
!                                                                        
!                                                                        
!         TEST 11 TESTS A FIELD OF BLANKS FOLLOWED BY A F FOR A FALSE    
!      CONDITION                                                         
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40110 if (ivcomp - 0) 20110, 10110, 20110                               
30110 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10110, 0121, 20110                                    
10110 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0121                                                        
20110 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0121 continue                                                          
!                                                                        
!                                                                        
!         TESTS 12 AND 13 VERIFY THAT THE FIELD CONTENTS .TRUE . OR      
!      .FALSE. ARE ACCEPTABLE INPUT FORMS AND THE VALUE OF THE INTERNAL  
!      DATUM IS TRUE OR FALSE RESPECTIVELY.                              
!                                                                        
!                                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0122 format (67x,l6,l7)                                                
      read (i08, 0122) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT IS FOR TESTS 12 
!      AND 13                                                            
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 012  ****                         
!                                                                        
!                                                                        
!         TEST 12 TESTS THE INPUT FIELD CONTENTS .TRUE. FOR A TRUE       
!      CONDITION.                                                        
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40120 if (ivcomp - 1) 20120, 10120, 20120                               
30120 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10120, 0131, 20120                                    
10120 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0131                                                        
20120 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0131 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 013  ****                         
!                                                                        
!                                                                        
!         TEST 13 TESTS THE INPUT FIELD CONTENTS .FALSE. FOR A FALSE     
!      CONDITION.                                                        
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40130 if (ivcomp - 0) 20130, 10130, 20130                               
30130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10130, 0141, 20130                                    
10130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0141                                                        
20130 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0141 continue                                                          
!                                                                        
!                                                                        
!         TESTS 14 AND 15 VERIFY THAT VALUE .T OR .F ARE ACCEPTABLE INPUT
!      FORMS AND THAT THE VALUE OF THE INTERNAL DATUM IS TRUE OR  FALSE  
!      RESPECTIVELY.                                                     
!                                                                        
!                                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0142 format (76x,l2,l2)                                                
      read (i08, 0142) lvon01, lvon02                                   
!         THE ABOVE READ STATEMENT AND ASSOCIATED FORMAT IS FOR TESTS    
!      14 AND 15                                                         
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 014  ****                         
!                                                                        
!         TEST 14 TESTS THE INPUT FIELD CONTENTS .T FOR A TRUE CONDITION 
!                                                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40140 if (ivcomp - 1) 20140, 10140, 20140                               
30140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10140, 0151, 20140                                    
10140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0151                                                        
20140 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0151 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 015  ****                         
!                                                                        
!                                                                        
!         TEST 15 TESTS THE INPUT FIELD CONTENTS .F FOR A FALSE CONDITION
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40150 if (ivcomp - 0) 20150, 10150, 20150                               
30150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10150, 0161, 20150                                    
10150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0161                                                        
20150 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0161 continue                                                          
!                                                                        
!                                                                        
!         TEST 16 AND 17 VERIFY THAT VALUE .T OR .F PRECEDED BY BLANKS   
!      ARE ACCEPTABLE INPUT FORMS AND THE VALUE OF THE INTERNAL DATA     
!      AS A RESULT OF THE READ ARE TRUE AND FALSE RESPECTIVELY.          
!                                                                        
!                                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0162 format (68x,l6,l6)                                                
      read (i08, 0162) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT ARE FOR TESTS   
!      16 AND 17.                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 016  ****                         
!                                                                        
!         TEST 16 TESTS THE INPUT FIELD CONTENTS .T PRECEDED BY 4 BLANKS 
!      FOR A TRUE CONDITION.                                             
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40160 if (ivcomp - 1) 20160, 10160, 20160                               
30160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10160, 0171, 20160                                    
10160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0171                                                        
20160 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0171 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 017  ****                         
!                                                                        
!                                                                        
!         TEST 17 TESTS THE INPUT FIELD CONTENTS .F PRECEDED BY 4 BLANKS 
!      FOR A FALSE CONDITION.                                            
!                                                                        
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40170 if (ivcomp - 0) 20170, 10170, 20170                               
30170 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10170, 0181, 20170                                    
10170 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0181                                                        
20170 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0181 continue                                                          
!                                                                        
!                                                                        
!         THE INPUT FIELD MAY HAVE T OR F FOLLOWED BY ADDITIONAL         
!      CHARACTERS IN THE FIELD.  TESTS 18 THROUGH 24 VERIFY THAT T OR F  
!      FOLLOWED BY ADDITIONAL CHARACTERS ARE ACCEPTABLE INPUT FORMS AND  
!      THE VALUE OF THE LOGICAL ENTITIES AS A RESULT OF THE READ ARE TRUE
!      AND FALSE RESPECTIVELY.                                           
!                                                                        
!                                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0182 format (58x,l15,l7)                                               
      read (i08, 0182) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT ARE FOR TESTS   
!      18 AND 19.                                                        
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 018  ****                         
!                                                                        
!                                                                        
!         TEST 18 TESTS THE INPUT FIELD CONTENTS OF 'THIS IS ALLOWED'    
!      FOR A TRUE CONDITION.                                             
!                                                                        
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40180 if (ivcomp - 1) 20180, 10180, 20180                               
30180 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10180, 0191, 20180                                    
10180 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0191                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0191 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 019  ****                         
!                                                                        
!                                                                        
!         TEST 19 TEST THE INPUT FIELD CONTENTS 'FINALLY' FOR A          
!      FALSE CONDITION.                                                  
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40190 if (ivcomp - 0) 20190, 10190, 20190                               
30190 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10190, 0201, 20190                                    
10190 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0201                                                        
20190 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0201 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 020  ****                         
!                                                                        
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0202 format (68x,l6,l6)                                                
      read (i08, 0202) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED FORMAT STATEMENTS ARE FOR TESTS  
!      20 AND 21.                                                        
!                                                                        
!         TEST 20 TESTS THE INPUT FIELD CONTENTS OF 'TRUE  ' (T FOLLOWED 
!      BY CHARACTERS WHICH INCLUDE SPACES) FOR A TRUE CONDITION.         
!                                                                        
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40200 if (ivcomp - 1) 20200, 10200, 20200                               
30200 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10200, 0211, 20200                                    
10200 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0211                                                        
20200 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0211 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 021  ****                         
!                                                                        
!                                                                        
!         TEST 21 TESTS THE INPUT FIELD CONTENTS OF 'FALSE '             
!      (F FOLLOWED BY CHARACTERS WHICH INCLUDE SPACES) FOR A FALSE       
!      CONDITION.                                                        
!                                                                        
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40210 if (ivcomp - 0) 20210, 10210, 20210                               
30210 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10210, 0221, 20210                                    
10210 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0221                                                        
20210 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0221 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 022  ****                         
!                                                                        
!                                                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
 0222 format (60x,l10,l10)                                              
      read (i08, 0222) lvon01, lvon02                                   
!         THE ABOVE READ AND ASSOCIATED FORMAT STATEMENT ARE FOR TESTS   
!      22 AND 23.                                                        
!                                                                        
!         TEST 22 TESTS THE INPUT FIELD CONTENTS OF '  .TIME.  ' (.T     
!      FOLLOWED BY CHARACTERS WHICH INCLUDE SPACES AND PERIODS) FOR A    
!      TRUE CONDITION.                                                   
!                                                                        
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40220 if (ivcomp - 1) 20220, 10220, 20220                               
30220 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10220, 0231, 20220                                    
10220 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0231                                                        
20220 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0231 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 023  ****                         
!                                                                        
!                                                                        
!         TEST 23 TESTS THE INPUT FIELD CONTENTS OF '  .FIELD. ' (.F     
!      FOLLOWED BY CHARACTERS WHICH INCLUDE SPACES AND PERIODS)  FOR A   
!      FALSE CONDITION.                                                  
!                                                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 1                                                        
      if (.not. lvon02) ivcomp = 0                                      
      ivcorr = 0                                                        
40230 if (ivcomp - 0) 20230, 10230, 20230                               
30230 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10230, 0241, 20230                                    
10230 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0241                                                        
20230 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0241 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 024  ****                         
!                                                                        
!                                                                        
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      lvon01 = .false.                                                  
 0242 format (27x,l53)                                                  
      read (i08, 0242) lvon01                                           
!                                                                        
!         TEST 24 TESTS USE OF A LARGE INPUT FIELD WITH THE CONTENTS     
!      'THIS IS A VERY LARGE FIELD FOR INPUT OF LOGICAL VALUES. '.  THE  
!      EDIT DESCRIPTOR IS L53 AND THE VALUE OF THE INTERNAL DATUM AS A   
!      RESULT OF THE READ SHOULD GIVE A TRUE CONDITION.                  
!                                                                        
      ivcomp = 0                                                        
      if (lvon01) ivcomp = 1                                            
      ivcorr = 1                                                        
40240 if (ivcomp - 1) 20240, 10240, 20240                               
30240 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10240, 0251, 20240                                    
10240 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0251                                                        
20240 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0251 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 025  ****                         
!                                                                        
!                                                                        
!         TEST 25 TESTS USE OF THE OPTIONAL REPEAT SPECIFICATION  WITH   
!      THE L EDIT DESCRIPTOR.  THE INPUT FIELD IS 1 POSITION IN LENGTH.  
!                                                                        
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      laon15(1) = .false.                                               
      laon15(2) = .true.                                                
      laon15(3) = .false.                                               
      laon15(4) = .true.                                                
      laon15(5) = .false.                                               
 0252 format (75x,5l1)                                                  
      read (i08, 0252) (laon15(i), i = 1, 5)                            
      ivcomp = 1                                                        
      ivcorr = 2310                                                     
      if (laon15(1))       ivcomp = ivcomp * 2                          
      if (.not. laon15(2))  ivcomp = ivcomp * 3                         
      if (laon15(3))       ivcomp = ivcomp * 5                          
      if (.not. laon15(4)) ivcomp = ivcomp * 7                          
      if (laon15(5))       ivcomp = ivcomp * 11                         
40250 if (ivcomp - 2310) 20250, 10250, 20250                            
30250 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10250, 0261, 20250                                    
10250 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0261                                                        
20250 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0261 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 026  ****                         
!                                                                        
!                                                                        
!         TEST 26 IS SIMILAR  TO TEST 25 EXCEPT  THAT EACH INPUT FIELD   
!      CONTAINING LOGICAL DATA IS 4 CHARACTERS IN LENGTH.  THE  EDIT     
!      DESCRIPTOR IS 4L4.                                                
!                                                                        
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      laon15(1) = .false.                                               
      laon15(2) = .false.                                               
      laon15(3) = .true.                                                
      laon15(4) = .true.                                                
 0262  format (64x,4l4)                                                 
      read (i08, 0262) (laon15(i), i = 1, 4)                            
      ivcomp = 1                                                        
      ivcorr = 210                                                      
      if (laon15 (1))      ivcomp = ivcomp * 2                          
      if (laon15(2))       ivcomp = ivcomp * 3                          
      if (.not. laon15(3)) ivcomp = ivcomp * 5                          
      if (.not. laon15(4)) ivcomp = ivcomp * 7                          
40260 if (ivcomp - 210) 20260, 10260, 20260                             
30260 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10260, 0271, 20260                                    
10260 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0271                                                        
20260 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0271 continue                                                          
!                                                                        
!                                                                        
!         THE PURPOSE OF TESTS 27 THROUGH 29 IS TO VERIFY THAT RECORDS   
!      CAN BE WRITTEN USING ONE EDIT DESCRIPTOR FORM AND READ USING      
!      ANOTHER FORM.                                                     
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 027  ****                         
!                                                                        
!                                                                        
!         TEST 27 READS  A  RECORD WITH THE EDIT DESCRIPTORS 4X,L1.  THE 
!      RECORD WAS  WRITTEN  USING  THE  DESCRIPTOR L5.  THE VALUE OF THE 
!      LOGICAL ENTITIES AS A RESULT OF THE READ SHOULD BE TRUE.          
!                                                                        
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      lvon01 = .false.                                                  
 0272 format (55x,20x,4x,l1)                                            
      read (i08, 0272) lvon01                                           
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (lvon01) ivcomp = 1                                            
40270 if (ivcomp - 1) 20270, 10270, 20270                               
30270 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10270, 0281, 20270                                    
10270 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0281                                                        
20270 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0281 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 028  ****                         
!                                                                        
!         TEST 28 READS A RECORD WITH THE EDIT DESCRIPTOR 4X,L1.  THE    
!      RECORD WAS WRITTEN USING THE EDIT DESCRIPTOR L5.  THIS TEST IS    
!      SIMILAR TO TEST 27 EXCEPT THE VALUE OF THE LOGICAL ENTITIES AS A  
!      RESULT OF THE READ SHOULD BE FALSE.                               
!                                                                        
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      lvon02 = .true.                                                   
 0282 format (55x,20x,4x,l1)                                            
      read (i08, 0282) lvon02                                           
      ivcomp = 1                                                        
      ivcorr = 0                                                        
      if (.not. lvon02) ivcomp = 0                                      
40280 if (ivcomp - 0) 20280, 10280, 20280                               
30280 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10280, 0291, 20280                                    
10280 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0291                                                        
20280 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0291 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 401  -  TEST 029  ****                         
!                                                                        
!                                                                        
!         TEST 29 READS A RECORD WITH THE EDIT DESCRIPTOR L5.  THE       
!      RECORD WAS WRITTEN USING THE EDIT DESCRIPTORS 4X,L1.  THE VALUE   
!      OF INTERNAL DATUM AS A RESULT OF THE READ SHOULD BE TRUE.         
!                                                                        
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      lvon01 = .false.                                                  
 0292 format (55x,20x,l5)                                               
      read (i08, 0292) lvon01                                           
      ivcomp = 0                                                        
      ivcorr = 1                                                        
      if (lvon01) ivcomp = 1                                            
40290 if (ivcomp - 1) 20290, 10290, 20290                               
30290 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10290, 0301, 20290                                    
10290 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0301                                                        
20290 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0301 continue                                                          
!                                                                        
!                                                                        
!                                                                        
!         THE FOLLOWING SOURCE CODE BRACKETED BY THE COMMENT LINES       
!      *****  BEGIN-FILE-DUMP SECTION AND *****  END-FILE-DUMP SECTION   
!      MAY OR MAY NOT  APPEAR AS COMMENTS IN THE SOURCE PROGRAM.         
!      THIS CODE IS OPTIONAL AND BY DEFAULT IT IS AUTOMATICALLY COMMENTED
!      OUT BY THE EXECUTIVE ROUTINE.  A DUMP OF THE FILE USED BY THIS    
!      ROUTINE IS PROVIDED BY USING THE *OPT1 EXECUTIVE ROUTINE CONTROL  
!      CARD.  IF THE OPTIONAL CODE IS SELECTED THE ROUTINE WILL DUMP     
!      THE CONTENTS OF THE FILE TO THE PRINT FILE FOLLOWING THE TEST     
!      REPORT AND BEFORE THE TEST REPORT SUMMARY.                        
!                                                                        
!    *****   BEGIN-FILE-DUMP SECTION     *****                           
!                                                                        
!                                                                        
! DB**                                                                   
!      REWIND I08                                                        
!      ITOTR = 141                                                       
!      IRNUM = 1                                                         
!      ILUN  = I08                                                       
! 7701 FORMAT     (I3,I2,I4,I3,2I4,60A1)                                 
! 7702 FORMAT (" ",I3,I2,I4,I3,2I4,60A1)                                 
! 7703 FORMAT (10X,"FILE ",I2," HAS ",I3," RECORDS - OK" )               
! 7704 FORMAT (10X,"FILE ",I2," HAS ",I3," RECORDS - THERE SHOULD BE " , 
!     1I3,9H RECORDS.)                                                   
!      DO 7771 IRNUM = 1, ITOTR                                          
!      READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1     (IDUMP(ICH), ICH = 1,60)                                     
!      WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1     (IDUMP(ICH), ICH = 1,60)                                     
!      IF (IEOF .EQ. 9999)   GO TO 7772                                  
! 7771 CONTINUE                                                          
!      GO TO 7775                                                        
! 7772 IF (IRNUM - ITOTR)   7774, 7773, 7775                             
! 7773 WRITE  (I02,  7703)  ILUN, IRNUM                                  
!      GO TO 7779                                                        
! 7774 WRITE (I02,  7704) ILUN, IRNUM, ITOTR                             
!      GO TO 7779                                                        
! 7775 DO 7776  I = 1,20                                                 
!      READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1     (IDUMP(ICH), ICH = 1,60)                                     
!      WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1     (IDUMP(ICH), ICH = 1,60)                                     
!      IRNUM = IRNUM + 1                                                 
!      IF (IEOF .EQ. 9999)  GO TO 7777                                   
! 7776 CONTINUE                                                          
! 7777 WRITE  (I02 ,  7704)  ILUN, IRNUM, ITOTR                          
! 7779 CONTINUE                                                          
! DE**   *  END-FILE-DUMP SECTION   *                                    
!         TEST  029 IS THE LAST TEST IN THIS PROGRAM.  THE ROUTINE SHOULD
!      HAVE MADE 29 EXPLICIT TESTS AND PROCESSED ONE FILE CONNECTED  FOR 
!      SEQUENTIAL ACCESS                                                 
!                                                                        
!                                                                        
!                                                                        
!      WRITE OUT TEST SUMMARY                                            
!                                                                        
      write (i02,90004)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
      write (i02,90000)                                                 
      write (i02,90004)                                                 
      write (i02,90020) ivfail                                          
      write (i02,90022) ivpass                                          
      write (i02,90024) ivdele                                          
      stop                                                              
90001 format (" ",24x,"FM401")                                          
90000 format (" ",20x,"END OF PROGRAM FM401" )                          
!                                                                        
!      FORMATS FOR TEST DETAIL LINES                                     
!                                                                        
80000 format (" ",4x,i5,6x,"DELETED")                                   
80002 format (" ",4x,i5,7x,"PASS")                                      
80010 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80012 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
80018 format (" ",4x,i5,7x,"FAIL",2x,a14,1x,a14)                        
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
!                                                                        
90002 format ("1")                                                      
90004 format (" ")                                                      
90006 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90008 format (" ",21x,"VERSION 2.1" )                                   
90010 format (" ",8x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )         
90012 format (" ",5x,"TEST",5x,"PASS/FAIL",5x,"COMPUTED",8x,"CORRECT")  
90014 format (" ",5x,"----------------------------------------------" ) 
90016 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARY                                 
!                                                                        
90020 format (" ",19x,i5," TESTS FAILED" )                              
90022 format (" ",19x,i5," TESTS PASSED" )                              
90024 format (" ",19x,i5," TESTS DELETED" )                             
      end program fm401
