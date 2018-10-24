      program fm402
!                                                                        
!                                                                        
!                                                                        
!          THIS ROUTINE TESTS THE A(W) (W IS SIZE OF FIELD IN CHARACTERS)
!      EDIT DESCRIPTOR OF THE FORMAT SPECIFICATION BOTH WITH AND WITHOUT 
!      THE OPTIONAL W.  THE A  EDIT DESCRIPTOR IS USED WITH AN INPUT/    
!      OUTPUT LIST ITEM OF TYPE CHARACTER.  IF A FIELD WIDTH W IS SPECI- 
!      FIED WITH THE A EDIT DESCRIPTOR THE FIELD CONSISTS OF W CHARAC-   
!      TERS.  IF A FIELD WIDTH W IS NOT SPECIFIED WITH THE A EDIT DES-   
!      CRIPTOR, THE NUMBER OF CHARACTERS IN THE FIELD IS THE LENGTH OF   
!      THE CHARACTER INPUT/OUTPUT LIST ITEM.  THIS ROUTINE FIRST         
!      TESTS  FOR PROPER EDITING OF CHARACTER DATA ON OUTPUT BY DIRECTING
!      THE EDITED RESULT  TO A PRINT FILE.    RESULTS OF THIS SET OF     
!      TESTS MUST BE VISUALLY CHECKED FOR CORRECTNESS.  NEXT AN EXTERNAL 
!      FILE CONNECTED FOR SEQUENTIAL ACCESS IS CREATED WITH CHARACTER    
!      DATA.  FINALLY THE FILE IS REWOUND AND READ WITH THE A(W) EDIT    
!      DESCRIPTOR AND CHECKED FOR PROPER EDITING ON INPUT.               
!                                                                        
!          THIS ROUTINE TESTS FOR PROPER EDITING BY                      
!                                                                        
!          (1) THE A EDIT DESCRIPTOR WITHOUT THE OPTIONAL W ON BOTH INPUT
!              AND OUTPUT,                                               
!                                                                        
!          (2) THE AW EDIT DESCRIPTOR WHEN THE LENGTH OF THE INPUT/OUTPUT
!              LIST ITEM IS LESS THAN THE WIDTH W,                       
!                                                                        
!          (3) THE AW EDIT DESCRIPTOR WHEN THE LENGTH OF THE INPUT/OUTPUT
!              LIST ITEM IS BOTH EQUAL TO AND GREATER THAN THE WIDTH W,  
!                                                                        
!          (4) THE A EDIT DESCRIPTOR WHEN USED WITH THE OPTIONAL REPEAT  
!              SPECIFICATION.                                            
!                                                                        
!      REFERENCES -                                                      
!                                                                        
!      AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,          
!      X3.9-1978                                                         
!                                                                        
!          SECTION 3.1,     FORTRAN CHARACTER SET                        
!          SECTION 4.8,     CHARACTER TYPE                               
!          SECTION 8.4.2,   CHARACTER TYPE-STATEMENT                     
!          SECTION 10.4,    CHARACTER ASSIGNMENT  STATEMENT              
!          SECTION 13.5.11, A EDITING                                    
!                                                                        
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
      integer :: i09
      integer :: iprog
      integer :: ifile
      integer :: itotr
      integer :: irlgn
      integer :: irecn
      integer :: ieof
      integer :: i
      integer :: j
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
      real :: go
      real :: to
      integer, dimension(1:80) :: idump
      character(len=1), dimension(1:46) :: catn11
      character(len=5), dimension(1:5) :: catn12
      character(len=1), dimension(1:2,1:3,1:2) :: catn31
      character(len=1), dimension(1:46) :: catn14
      character(len=1) :: cvtn11
      character(len=10) :: cvtn12
      character(len=2) :: cvtn13
      character(len=50) :: cctn15
      character(len=50) :: cvtn15
      character(len=1) :: cvtn01
      data catn14 / 46*' ' / 
      data cctn15 / 'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789' / 
      data catn11 / '0','1','2','3','4','5','6','7','8','9','=','+','-','*','/','(',')',',','.','''','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z' / 
      data catn12 / 'ABMYZ','01589','=+-()','A5+Z.','1''A,4' / 
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
!         TEST 001 THROUGH 014 TESTS THE   EDIT DESCRIPTOR FOR PROPER    
!      EDITING OF CHARACTER DATA  ON OUTPUT.  TO VALIDATE THESE TESTS    
!      THE EDITED DATA  IS SENT TO A PRINT FILE AND THEREFORE MUST BE    
!      VISUALLY CHECKED FOR CORRECTNESS.  ON OUTPUT THE EDITED FIELD     
!      SIZE IS AW WHERE W IS NUMBER OF POSITIONS IN THE FIELD OR         
!      IS THE SIZE OF THE OUTPUT DATUM ITEM.  SEE SECTION 13.5.11 A      
!      EDITING                                                           
!                                                                        
!                                                                        
80052 format (" ",4x,  "TESTS 001 THROUGH 014 MUST BE VISUALLY VERIFIED.")                                                                
80054 format (" ",  "IMMEDIATELY FOLLOWING THIS NARRATIVE IS A REFERENCE LINE")                                                           
80056 format (" ",  "OF THE FORM '123456 ...'.   THE REFERENCE LINE IS TO")                                                               
80058 format (" ","AID IN THE VISUAL VERIFICATION OF THE TESTS.  FOR" ) 
80062 format (" ","THE OUTPUT TO BE CORRECT THE DATA VALUES DISPLAYED"  )                                                                 
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
!      ****  FCVS PROGRAM  402  -  TEST 001  ****                        
!                                                                        
!         TEST 001 TESTS FOR PROPER EDITING OF THE A EDIT DESCRIPTOR     
!      ON OUTPUT WHERE THE FIELD IS 1 POSITION IN LENGTH, THE            
!      VALUE OF THE DATUM IS LETTERS AND THE OUTPUT LIST ITEM IS A       
!      VARIABLE.                                                         
!                                                                        
      ivtnum = 001                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      cvtn01 = 'A'                                                      
 0012 format (" ",4x,i5,26x,a,14x,"A")                                  
      write (i02, 0012) ivtnum, cvtn01                                  
      goto 0021                                                        
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM  402  -  TEST 002  ****                        
!                                                                        
!         TEST 002 IS SIMILAR TO TEST 001 EXCEPT THAT THE OUTPUT LIST    
!      ITEM IS AN ARRAY ELEMENT.                                         
!                                                                        
      ivtnum = 002                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      catn31 (1,2,1) =  'Z'                                             
 0022 format (" ",4x,i5,26x,a,14x,"Z")                                  
      write (i02, 0022) ivtnum, catn31 (1,2,1)                          
      goto 0031                                                        
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
 0031 continue                                                          
!                                                                        
!      ***  FCVS PROGRAM 402  -  TEST 003  ****                          
!                                                                        
!         TEST 003 VERIFIES THAT THE  A  EDIT DESCRIPTOR (WITHOUT THE    
!      W OPTION) CAN PROPERLY EDIT SPECIAL CHARACTERS ON OUTPUT.  THE    
!      SPECIAL CHARACTER / (SLASH) IS USED FOR THIS TEST AND IS STORED   
!      IN AN OUTPUT LIST ITEM 1 POSITION IN LENGTH.                      
!                                                                        
      ivtnum = 003                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      cvtn11 = '/'                                                      
 0032 format (" ",4x,i5,26x,a,14x,"/")                                  
      write (i02, 0032) ivtnum, cvtn11                                  
      goto 0041                                                        
30030 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0041 continue                                                          
!                                                                        
!      ***  FCVS PROGRAM 402  -  TEST 004  ***                           
!                                                                        
!         TEST 004 IS SIMILAR TO TEST 003 EXCEPT THAT THE DATA BEING     
!      EDITED IS NUMERIC.                                                
!                                                                        
      ivtnum = 004                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      cvtn11 = '9'                                                      
 0042 format (" ",4x,i5,26x,a,14x,"9")                                  
      write (i02, 0042) ivtnum, cvtn11                                  
      goto 0051                                                        
30040 ivdele = ivdele  + 1                                              
      write (i02, 80000) ivtnum                                         
 0051 continue                                                          
!                                                                        
!      ***  FCVS PROGRAM 402  -  TEST 005  ***                           
!                                                                        
!         TEST 005 IS SIMILAR TO TEST 003 EXCEPT THAT IT USES THE SPECIAL
!      CHARACTER QUOTE.                                                  
!                                                                        
      ivtnum = 005                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      cvtn11 = ''''                                                     
 0052 format (" ",4x,i5,26x,a,14x,"'")                                  
      write (i02, 0052) ivtnum, cvtn11                                  
      goto 0061                                                        
30050 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
!                                                                        
!                                                                        
!         TESTS 006 THROUGH TEST  011  TESTS THE A EDIT DESCRIPTOR       
!      WITHOUT THE FIELD WIDTH SPECIFICATION (W OPTION) WHERE THE SIZE   
!      OF THE OUTPUT DATA ITEM  IS 05 CHARACTERS IN LENGTH.              
!                                                                        
!                                                                        
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 006  ****                         
!                                                                        
!         TEST 006 TESTS USE OF THE A EDIT DESCRIPTOR WITH LETTERS       
!                                                                        
      ivtnum = 006                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      catn12(1) = 'ABMYZ'                                               
 0062 format(" ",4x,i5,17x,"     ",a,5x,"     ABMYZ" )                  
      write (i02, 0062) ivtnum, catn12(1)                               
      goto 0071                                                        
30060 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0071 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 007  ****                         
!                                                                        
!         TEST 007 TESTS USE OF THE A EDIT DESCRIPTOR WITH DIGITS        
!                                                                        
      ivtnum = 007                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      catn12(2) = '01589'                                               
 0072 format(" ",4x,i5,17x,"     ",a,5x,"     01589" )                  
      write (i02, 0072) ivtnum, catn12(2)                               
      goto 0081                                                        
30070 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0081 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 008  ****                         
!                                                                        
!         TEST 008 TESTS USE OF THE  A  EDIT DESCRIPTOR WITH SPECIAL     
!      CHARACTERS.                                                       
!                                                                        
      ivtnum = 008                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      catn12(3) = '=+-()'                                               
 0082 format(" ",4x,i5,17x,"     ",a,5x,"     =+-()" )                  
      write (i02, 0082) ivtnum, catn12(3)                               
      goto 0091                                                        
30080 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0091 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM FM402  -  TEST 009  ****                       
!                                                                        
!         TEST 009 TESTS USE OF THE  A  EDIT DESCRIPTOR WITH A MIX       
!      OF LETTERS, DIGITS AND SPECIAL CHARACTERS                         
!                                                                        
      ivtnum = 009                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      catn12(4) = 'A5+.Z'                                               
 0092 format(" ",4x,i5,17x,"     ",a,5x,"     A5+.Z" )                  
      write (i02, 0092) ivtnum, catn12(4)                               
      goto 0101                                                        
30090 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0101 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM FM402  -  TEST 010  ****                       
!                                                                        
!         TEST 010 TESTS USE OF THE  A  EDIT DESCRIPTOR WITH A MIX       
!      OF LETTERS, DIGITS AND SPECIAL CHARACTERS  INCLUDING APOSTROPES   
!                                                                        
      ivtnum = 010                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      catn12(5) = '1''A,4'                                              
 0102 format(" ",4x,i5,17x,"     ",a,5x,"     1'A,4" )                  
      write (i02, 0102) ivtnum, catn12(5)                               
      goto 0111                                                        
30100 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
!                                                                        
 0111 continue                                                          
!      ****  FCVS PROGRAM FM402  -  TEST 11  ****                        
!                                                                        
!         TEST 011 USES THE  A  EDIT DESCRIPTOR (WITHOUT THE OPTIONAL    
!      FIELD WIDTH SPECIFIED) WITH THE OPTIONAL REPEAT SPECIFICATION.    
!      EACH OUTPUT LIST ITEM WILL BE ONE CHARACTER IN LENGTH.            
!                                                                        
      ivtnum = 011                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
 0112 format (" ",4x,i5,17x,10a,5x,"059=+PQUVY" )                       
      write  (i02, 0112) ivtnum, catn11(1), catn11(6), catn11(10),      catn11(11), catn11(12), catn11(36), catn11(37), catn11(41),       catn11(42), catn11(45)                                            
      goto 0121                                                        
30110 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0121 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM FM402  -  TEST 12  ****                        
!                                                                        
!         TEST 012 IS SIMILAR TO 011 IN THAT THE  A  DESCRIPTOR IS USED  
!      WITH THE OPTIONAL REPEAT SPECIFICATION E. G., 3A  HOWEVER, EACH   
!      OUTPUT LIST ITEM HAS A DIFFERENT NUMBER OF CHARACTERS IN THE ITEM 
!      E. G., THE FIRST I/O LIST ITEM HAS 5 CHARACTERS, THE SECOND       
!      ITEM HAS 2 CHARACTERS AND THE THIRD ITEM HAS 1 CHARACTER.         
!                                                                        
      ivtnum = 012                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      cvtn13 = 'YZ'                                                     
      cvtn11 = ')'                                                      
      catn12(2) = '(12AB'                                               
 0122 format (" ",4x,i5,17x,"*",3a,"*",5x,"*(12ABYZ)*" )                
      write  (i02, 0122) ivtnum, catn12(2), cvtn13, cvtn11              
      goto 0131                                                        
30120 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0131 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM FM402  -  TEST 13  ***                         
!                                                                        
!         TEST 013 TESTS FOR PROPER EDITING OF THE  A  EDIT DESCRIPTOR   
!      (WITH THE FIELD WIDTH SPECIFIED) WHEN THE OUTPUT LIST ITEM        
!      HAS FEWER CHARACTERS THAN SPECIFIED BY THE EDIT DESCRIPTOR.  THE  
!      OUTPUT FIELD SHOULD CONSISTS OF BLANKS FOLLOWED BY CHARACTERS     
!      FROM THE INTERNAL REPRESENTATION.                                 
!                                                                        
      ivtnum = 013                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      catn12(1) = 'ABMYZ'                                               
 0132 format (" ",4x,i5,17x,a10,5x,"     ABMYZ" )                       
      write (i02, 0132) ivtnum, catn12(1)                               
      goto 0141                                                        
30130 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0141 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM FM402  -  TEST 14  ****                        
!                                                                        
!         TEST 014 TESTS FOR PROPER EDITING OF THE  A  EDIT DESCRIPTOR   
!      (WITH THE FIELD WIDTH SPECIFIED) WHEN THE OUTPUT LIST ITEM        
!      IS GREATER THAN THAT SPECIFIED BY THE EDIT DESCRIPTOR.  THE OUTPUT
!      FIELD SHOULD CONSIST OF THE LEFTMOST CHARACTERS FROM THE INTERNAL 
!      REPRESENTATION.                                                   
!                                                                        
      ivtnum = 014                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      cvtn12 = '12345ABCDE'                                             
 0142 format (" ",4x,i5,17x,"     ",a5,5x,"     12345" )                
      write (i02, 0142) ivtnum, cvtn12                                  
      goto 0151                                                        
30140 ivdele = ivdele + 1                                               
      write (i02, 80000) ivtnum                                         
 0151 continue                                                          
!                                                                        
!         THE FOLLOWING BLOCK OF SOURCE CODE BEGINNING WITH COMMENT LINE 
!      **** CREATE-FILE SECTION AND ENDING WITH THE COMMENT LINE         
!      **** END-OF-CREATE-FILE SECTION BUILDS A FILE WHICH IS USED IN    
!      TESTING THE A EDIT DESCRIPTOR.  THE FILE PROPERTIES ARE:          
!                                                                        
!               FILE IDENTIFIER     - I09 (X-NUMBER 09)                  
!               RECORD SIZE         - 80 CHARACTERS                      
!               ACCESS METHOD       - SEQUENTIAL                         
!               RECORD TYPE         - FORMATTED                          
!               DESIGNATED DEVICE   - DISK                               
!               TYPE OF DATA        - CHARACTER (A FORMAT)               
!               RECORDS IN FILE     - 143 PLUS THE ENDFILE RECORD        
!                                                                        
!         THE FIRST 20 POSITIONS OF EACH RECORD IN THE FILE UNIQUELY     
!      IDENTIFIES THAT RECORD.  THE REMAINING POSITONS OF THE RECORD     
!      CONTAIN DATA WHICH IS USED IN TESTING THE A EDIT DESCRIPTOR.      
!      A DESCRIPTION OF EACH FIELD OF THE 20-CHARACTER PREAMBLE FOLLOWS. 
!                                                                        
!                 VARIABLE NAME IN PROGRAM     CHARACTER POSITIONS       
!                 -------- ---- -- -------     --------- ---------       
!                                                                        
!               IPROG  (ROUTINE NAME)         -     1 THRU  3            
!               IFILE  (LOGICAL/ X-NUMBER)    -     4 THRU  5            
!               ITOTR  (RECORDS IN FILE)      -     6 THRU  9            
!               IRLGN  (CHARACTERS IN RECORD) -    10 THRU 12            
!               IRECN  (RECORD NUMBER)        -    13 THRU 16            
!               IEOF   (9999 IF LAST RECORD)  -    17 THRU 20            
!                                                                        
!      DEFAULT ASSIGNMENT FOR FILE IS I09 = 07                           
      i09 = 07                                                          
! X090 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-090               
! X091 THIS CARD IS REPLACED BY THE CONTENTS OF CARD X-091               
      iprog = 402                                                       
      ifile = i09                                                       
      itotr = 143                                                       
      irlgn = 80                                                        
      irecn = 0                                                         
      ieof  = 0                                                         
!                                                                        
!                                                                        
!  *****  CREATE-FILE  SECTION  *****                                    
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 015  ****                         
!                                                                        
!                                                                        
!          TEST 15 WRITES RECORDS USING THE A EDIT DESCRIPTOR WITHOUT THE
!       OPTIONAL FIELD WIDTH SPECIFICATION.  EACH CHARACTER OF THE       
!       FORTRAN   SET     IS WRITTEN  WITH AN  A  EDIT DESCRIPTOR FROM   
!       THE INTERNAL REPRESENTATION WHICH IS ONE CHARACTER IN LENGTH.    
!       TEN DIFFERENT CHARACTERS ARE WRITTEN IN EACH RECORD UNTIL THE    
!       FULL CHARACTER SET IS EXHAUSTED.  THIS SEQUENCE IS REPEATED UNTIL
!       50 RECORDS HAVE BEEN WRITTEN (5 RECORDS PER SET AND 10 SETS).    
!       THE RECORDS ARE WRITTEN TO A MASS STORAGE FILE.                  
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
70003 format (i3,i2,i4,i3,2i4,50x,10a)                                  
70004 format (i3,i2,i4,i3,2i4,54x,6a)                                   
      irecn = 0                                                         
      do i=1,10                                                    
      irecn = irecn + 1                                                 
      write (i09, 70003) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11 (j), j = 1,10)                                       
!         CHARACTERS 0 THROUGH 9 ARE CONTAINED IN THIS RECORD            
      irecn = irecn + 1                                                 
      write (i09, 70003) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11(j), j = 11,20)                                       
!         CHARACTERS =,+,-,*,/,(,),,,. AND ' ARE IN THIS RECORD.         
      irecn = irecn + 1                                                 
      write (i09, 70003) iprog,    ifile, itotr, irlgn, irecn, ieof,          (catn11(j), j = 21,30)                                      
!         CHARACTERS A THROUGH J  ARE IN THIS RECORD                     
      irecn = irecn + 1                                                 
      write (i09, 70003) iprog, ifile, itotr, irlgn, irecn, ieof,             (catn11(j), j = 31,40)                                      
!          CHARACTERS K THROUGH T ARE IN THIS RECORD                     
      irecn = irecn + 1                                                 
      write (i09, 70004) iprog, ifile, itotr, irlgn, irecn, ieof,             (catn11(j), j = 41,46)                                      
!          CHARACTERS U THROUGH Z ARE IN THIS RECORD                     
  end do
      ivcomp = irecn                                                    
      ivcorr = 050                                                      
      ivon01 = 50                                                       
40150 if (ivon01 - irecn )  20150, 10150, 20150                         
!          VALUE IN  IVCOMP  IS THE NUMBER OF RECORDS WRITTEN            
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
!      ****  FCVS PROGRAM 402  -  TEST 016  ****                         
!                                                                        
!                                                                        
!          TEST 16 IS THE SAME AS TEST 15 EXCEPT THAT THE 50 RECORDS     
!       WRITTEN  USE  THE A EDIT DESCRIPTOR WITH THE OPTIONAL FIELD WIDTH
!       SPECIFIED.                                                       
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
70005 format  (i3,i2,i4,i3,2i4,50x,10a1)                                
70006 format  (i3,i2,i4,i3,2i4,54x,6a1)                                 
      irecn = 50                                                        
      do i=1,10                                                    
      irecn = irecn + 1                                                 
      write (i09, 70005) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11(j), j = 1,10)                                        
!          CHARACTERS 0 THROUGH 9  ARE CONTAINED IN THIS RECORD          
      irecn = irecn + 1                                                 
      write (i09, 70005) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11(j), j = 11,20)                                       
!          CHARACTERS =,+,-,*,/,(,),,,.  AND ' ARE IN THIS RECORD        
      irecn = irecn + 1                                                 
      write (i09, 70005) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11(j), j = 21,30)                                       
!          CHARACTERS A THROUGH J ARE IN THIS RECORD                     
      irecn = irecn + 1                                                 
      write (i09, 70005) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11(j), j = 31,40)                                       
!          CHARACTERS K THROUGH T ARE IN THIS RECORD                     
      irecn = irecn + 1                                                 
      write (i09, 70006) iprog, ifile, itotr, irlgn, irecn, ieof,            (catn11(j), j = 41,46)                                       
!          CHARACTERS U THROUGH Z ARE IN THIS RECORD                     
  end do
      ivcomp = irecn - 50                                               
      ivcorr = 50                                                       
      ivon01 = 100                                                      
40160 if (ivon01 - irecn) 20160, 10160, 20160                           
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
!      ****  FCVS PROGRAM 402  -  TEST 017  ****                         
!                                                                        
!                                                                        
!          TEST 17 WRITES 40 RECORDS CONTAINING CHARACTER DATA WHICH IS  
!      USED  FOR LATER TESTS.  THE FILE SHOULD CONTAIN 140 RECORDS       
!      FOLLOWING EXECUTION OF THIS TEST.                                 
!                                                                        
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
70007 format (i3,i2,i4,i3,2i4,  "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789                        ")                                           
70008 format (i3,i2,i4,i3,2i4,  "=+-*/(),'.ABMYZ01589=+-()A5+Z.1'A,4                         ")                                           
      irecn = 100                                                       
      do i = 1,20                                                  
      irecn = irecn + 1                                                 
      write (i09, 70007) iprog, ifile, itotr, irlgn, irecn, ieof        
!          CHARACTERS 0 THROUGH 9  AND A THROUGH Z ARE IN THIS RECORD    
      irecn = irecn + 1                                                 
      write (i09, 70008) iprog, ifile, itotr, irlgn, irecn, ieof        
!          SPECIAL CHARACTERS ARE IN THIS RECORD                         
  end do
      ivcomp = irecn - 100                                              
      ivcorr = 40                                                       
      ivon01 = 140                                                      
40170 if (ivon01 - irecn) 20170, 10170, 20170                           
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
!      ****  FCVS PROGRAM 402  -  TEST 018  ****                         
!                                                                        
!                                                                        
!         TEST 18 WRITES A RECORD WHICH CONTAINS A LONG FIELD (50 CHAR-  
!      ACTERS) USING AN A EDIT DESCRIPTOR WITHOUT THE OPTIONAL FIELD     
!      WIDTH SPECIFICATION.                                              
!                                                                        
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      irecn = 141                                                       
70009 format (i3,i2,i4,i3,2i4,10x,a)                                    
      write (i09, 70009) iprog, ifile, itotr, irlgn, irecn, ieof, cctn15
      ivcomp = irecn - 140                                              
      ivcorr = 1                                                        
      ivon01 = 141                                                      
40180 if (ivon01 - irecn) 20180, 10180, 20180                           
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
!      ****  FCVS PROGRAM 402  -  TEST 019  ****                         
!                                                                        
!                                                                        
!         TEST 19 WRITES A LONG FIELD (50 CHARACTERS)                    
!      USING AN A EDIT DESCRIPTOR  WITH   THE OPTIONAL FIELD WIDTH       
!      SPECIFICATION.                                                    
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      irecn = 142                                                       
70010 format (i3,i2,i4,i3,2i4,10x,a50)                                  
      write (i09, 70010) iprog, ifile, itotr, irlgn, irecn, ieof, cctn15
      ivcomp = irecn - 141                                              
      ivcorr = 1                                                        
      ivon01 = 142                                                      
40190 if (ivon01 - irecn) 20190, 10190, 20190                           
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
!      ****  FCVS PROGRAM 402  -  TEST 020  ****                         
!                                                                        
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      irecn  = irecn  + 1                                               
      ieof = 9999                                                       
70011 format (i3,i2,i4,i3,2i4,59x," ")                                  
      write (i09, 70011) iprog, ifile, itotr, irlgn, irecn, ieof        
      endfile i09                                                       
      rewind i09                                                        
      write (i02, 90004)                                                
70012 format ("   FILE I09 HAS BEEN CREATED AND CONTAINS 143 RECORDS" ) 
70013 format (" INCORRECT NUMBER OF RECORDS IN FILE - " , i5   ,  " RECORDS")                                                             
70014 format (" WRITTEN BUT 143 RECORDS SHOULD HAVE BEEN WRITTEN." )    
      if (irecn - 143) 4020, 4021, 4020                                 
 4020 write (i02, 70013) irecn                                          
      write (i02, 70014)                                                
      goto 4022                                                        
 4021 write (i02, 70012)                                                
      write (i02, 90004)                                                
!                                                                        
!  **** END-OF-CREATE-FILE SECTION  ****                                 
!                                                                        
 4022 continue                                                          
!                                                                        
!          TESTS 20 THROUGH 24 READ 5 OF THE FIRST 50 RECORDS USING THE  
!      A EDIT DESCRIPTOR WITHOUT THE OPTIONAL FIELD WIDTH SPECIFICATION. 
!      EACH CHARACTER IS CHECKED FOR PROPER EDITING.  THE FIELDS ARE     
!      WRITTEN AND READ WITH THE SAME A EDIT DESCRIPTOR FORM.  THE       
!      RESULTING NUMBER FROM EACH TEST IN IVCOMP AND IVCORR IS           
!      THE NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT OF THE READ.   
!                                                                        
!                                                                        
!          TEST 20 READS AND CHECKS THE CHARACTERS 0 THROUGH 9.  THE     
!      VALUE RESULTING FROM THE TEST IN IVCOMP AND IVCORR REFLECTS THE   
!      NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT OF THE READ.       
!                                                                        
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0202 format (70x,10a)                                                  
      read (i09, 0202) (catn14(j), j = 1,10)                            
      do i=1,10                                                    
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40200 if (ivcomp - 10) 20200, 10200, 20200                              
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
!      ****  FCVS PROGRAM 402  -  TEST 021  ****                         
!                                                                        
!                                                                        
!          TEST 21 READS AND CHECKS THE CHARACTERS =,+,-,*,/,(,),,,., AND
!      '.  THE NUMBER RESULTING FROM THE TEST IN IVCOMP AND IVCORR       
!      REFLECTS THE NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT OF    
!      THE READ.                                                         
!                                                                        
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0212 format (70x,10a)                                                  
      read (i09, 0212) (catn14(j), j = 11,20)                           
      do i = 11,20                                                 
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40210 if (ivcomp - 10) 20210, 10210, 20210                              
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
!      ****  FCVS PROGRAM 402  -  TEST 022  ****                         
!                                                                        
!                                                                        
!          TEST 22 READS AND CHECKS THE CHARACTERS A THROUGH J.          
!                                                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0222 format (70x,10a)                                                  
      read (i09, 0222) (catn14(j), j = 21,30)                           
      do i = 21,30                                                 
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40220 if (ivcomp - 10) 20220, 10220, 20220                              
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
!      ****  FCVS PROGRAM 402  -  TEST 023  ****                         
!                                                                        
!                                                                        
!          TEST 23 READS AND CHECKS THE CHARACTERS K THROUGH T.          
!                                                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0232 format (70x,10a)                                                  
      read (i09, 0232) (catn14(j), j = 31,40)                           
      do i = 31,40                                                 
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40230 if (ivcomp - 10) 20230, 10230, 20230                              
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
!      ****  FCVS PROGRAM 402  -  TEST 024  ****                         
!                                                                        
!                                                                        
!          TEST 24 READS AND CHECKS THE CHARACTERS U THROUGH Z.          
!                                                                        
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 06                                                       
 0242 format (74x,6a)                                                   
      read (i09, 0242) (catn14(j), j = 41,46)                           
      do i = 41,46                                                 
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40240 if (ivcomp - 6) 20240, 10240, 20240                               
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
!                                                                        
!          TESTS 25 THROUGH 29  READ RECORD NUMBERS 56 THROUGH 60 USING  
!      THE A EDIT DESCRIPTOR WITH THE OPTIONAL FIELD WIDTH SPECIFIED.    
!      EACH FIELD IS 1 CHARACTER IN LENGTH AND IS CHECKED FOR PROPER     
!      EDITING. THE FIELDS ARE WRITTEN AND READ WITH THE SAME EDIT       
!      DESCRIPTOR.  THE NUMBER RESULTING FROM EACH TEST IN IVCOMP AND    
!      IVCORR IS THE THE NUMBER OF CORRECT CHARACTERS FOUND AS A RESULT  
!      OF THE READ.                                                      
!                                                                        
!                                                                        
70020 format (12x,2i4,59x,a1)                                           
      rewind i09                                                        
      do i = 1, 150                                                
      read (i09, 70020, end = 4027) irecn, ieof                         
      if (irecn  ==  55) goto 4027                                       !Break
  end do
 4027 if (irecn - 55) 4028, 4029, 4028                                  
!                                                                        
!          THE CODE IMMEDIATELY PRECEDING POSITIONS THE FILE TO RECORD   
!      NUMBER 55 FOR TESTS 25 THROUGH 29.                                
!                                                                        
70021 format (  "  THE INITIAL RECORD FOR TESTS 25 THROUGH 29 COULD NOT BE FOUND,")                                                       
70022 format (" THEREFORE TESTS 25 THROUGH 29     ARE   DELETED." )     
 4028 write (i02, 70021)                                                
      write (i02, 70022)                                                
      goto 301                                                         
 4029 continue                                                          
      do i = 1,46                                                  
      catn14(i) = ' '                                                   
  end do
!                                                                        
!          THE ABOVE DO LOOP   INITIALIZES THE ARRAY CATN14 TO BLANKS.   
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 025  ****                         
!                                                                        
!                                                                        
!          TEST 25 READS AND CHECKS THE CHARACTERS 0 THROUGH 9.          
!                                                                        
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0252 format (70x,10a1)                                                 
      read (i09, 0252) (catn14(j), j = 1, 10)                           
      do i = 1,10                                                  
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40250 if (ivcomp - 10) 20250, 10250, 20250                              
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
!      ****  FCVS PROGRAM 402  -  TEST 026  ****                         
!                                                                        
!                                                                        
!          TEST 26 READS AND CHECKS THE CHARACTERS =,+,-,*,/,(,),,,., AND
!      '.                                                                
!                                                                        
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0262 format (70x,10a1)                                                 
      read (i09, 0262) (catn14(j), j = 11, 20)                          
      do i = 11,20                                                 
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40260 if (ivcomp -10) 20260, 10260, 20260                               
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
!      ****  FCVS PROGRAM 402  -  TEST 027  ****                         
!                                                                        
!                                                                        
!          TEST 27 READS AND CHECKS THE CHARACTERS A THROUGH J.          
!                                                                        
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0272 format (70x,10a1)                                                 
      read (i09, 0272) (catn14(j), j = 21,30)                           
      do i = 21,30                                                 
      if  (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                
  end do
40270 if (ivcomp - 10) 20270, 10270, 20270                              
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
!      ****  FCVS PROGRAM 402  -  TEST 028  ****                         
!                                                                        
!                                                                        
!          TEST 28 READS AND CHECKS THE CHARACTERS K THROUGH T.          
!                                                                        
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 10                                                       
 0282 format (70x,10a1)                                                 
      read (i09, 0282) (catn14(j), j = 31,40)                           
      do i = 31, 40                                                
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40280 if (ivcomp - 10) 20280, 10280, 20280                              
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
!      ****  FCVS PROGRAM 402  -  TEST 029  ****                         
!                                                                        
!                                                                        
!          TEST 29 READS AND CHECKS THE CHARACTERS U THROUGH Z.          
!                                                                        
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      ivcomp = 0                                                        
      ivcorr = 6                                                        
 0292 format (74x,6a1)                                                  
      read (i09, 0292) (catn14(j), j = 41,46)                           
      do i = 41,46                                                 
      if (catn14(i)  ==  catn11(i)) ivcomp = ivcomp + 1                 
  end do
40290 if (ivcomp - 6) 20290, 10290, 20290                               
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
!          TESTS 30 THROUGH 32 READ RECORD NUMBERS 101 THROUGH 103. THESE
!      TESTS TEST FOR PROPER EDITING ON INPUT WHERE THE INPUT FIELD      
!      AND THE INPUT LIST ITEM ARE OF DIFFERENT SIZES.                   
!                                                                        
!                                                                        
70031 format (12x,2i4,59x,a1)                                           
      rewind i09                                                        
      do i = 1,150                                                 
      read (i09, 70031, end = 4032) irecn, ieof                         
      if (irecn  ==  100) goto 4032                                      !Break
  end do
 4032 if (irecn - 100) 4033, 4034, 4033                                 
70032 format (  "  THE START RECORD FOR TESTS 30 THROUGH 32 COULD NOT   BE FOUND,")                                                       
70033 format (" THEREFORE TESTS 30 THROUGH 32     ARE   DELETED." )     
 4033 write (i02, 70032)                                                
      write (i02, 70033)                                                
      goto 331                                                         
 4034 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 030  ****                         
!                                                                        
!                                                                        
!          TEST 30 TESTS THE  A EDIT DESCRIPTOR WITH THE OPTIONAL REPEAT 
!      SPECIFICATION.  THE A  EDIT DESCRIPTOR DOES NOT HAVE THE OPTIONAL 
!      FIELD WIDTH SPECIFICATION AND THE INPUT LIST ITEMS  VARY IN SIZE  
!      FROM 1 TO 10 CHARACTERS.  RECORD NUMBER 101 IS READ AND WAS       
!      CREATED IN TEST 17 WITH THE FORMAT STATEMENT                      
!                                                                        
!      FORMAT (I3,I2,I4,I3,2I4,60HABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789   
!     1                     )                                            
!                                                                        
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      ivcomp = 1                                                        
      ivcorr = 210                                                      
      catn14(1) = ' '                                                   
      cvtn13 = '  '                                                     
      catn12(3) = '     '                                               
      cvtn12 = '          '                                             
 0302 format (20x,4a,42x,a1)                                            
      read (i09, 0302, end = 0303)  catn14(1), cvtn13, catn12(3), cvtn12
 0303 if (catn14(1)  ==  'A')           ivcomp = ivcomp * 2             
      if (cvtn13     ==  'BC')          ivcomp = ivcomp * 3             
      if (catn12(3)  ==  'DEFGH')       ivcomp = ivcomp * 5             
      if (cvtn12   ==  'IJKLMNOPQR')    ivcomp = ivcomp * 7             
40300 if (ivcomp - 210) 20300, 10300, 20300                             
30300 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10300, 0311, 20300                                    
10300 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0311                                                        
20300 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0311 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 031  ****                         
!                                                                        
!                                                                        
!          TEST 31 TESTS FOR PROPER EDITING OF THE A EDIT DESCRIPTOR WHEN
!      THE SPECIFIED WIDTH OF THE DESCRIPTOR IS LESS THAN THE INTERNAL   
!      REPRESENTATION OF THE INPUT LIST ITEM.  THE CHARACTERS SHOULD     
!      APPEAR LEFT-JUSTIFIED WITH TRAILING BLANKS IN THE INTERNAL        
!      REPRESENTATION.     RECORD NUMBER 102 IS READ AND WAS CREATED     
!      IN TEST 17 WITH THE FORMAT STATEMENT                              
!                                                                        
!      FORMAT (I3,I2,I4,I3,2I4,60H=+-*/(),'.ABMYZ01589=+-()A5+Z.1'A,4    
!     1                     )                                            
!                                                                        
!                                                                        
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      cvtn12 = '9999999999'                                             
      ivcomp = 0                                                        
      ivcorr = 1                                                        
 0312 format (20x,10x,a5,40x)                                           
      read (i09, 0312, end = 0313) cvtn12                               
 0313 if (cvtn12  ==  'ABMYZ     ')  ivcomp = 1                         
40310 if (ivcomp - 1) 20310, 10310, 20310                               
30310 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10310, 0321, 20310                                    
10310 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0321                                                        
20310 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0321 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 032  ****                         
!                                                                        
!                                                                        
!          TEST 32  TESTS FOR PROPER  EDITING OF THE  A  EDIT            
!      DESCRIPTOR WHEN THE WIDTH OF THE DESCRIPTOR IS GREATER THAN THE   
!      INTERNAL REPRESENTATION OF THE INPUT LIST ITEM.  THE RIGHTMOST    
!      CHARACTERS SHOULD BE TAKEN FROM THE INPUT FIELD.  RECORD NUMBER   
!      103 IS EXPECTED TO BE READ.  THE RECORD WAS CREATED IN TEST 17    
!      WITH THE FORMAT STATEMENT                                         
!                                                                        
!      FORMAT (I3,I2,I4,I3,2I4,60HABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789   
!     1                     )                                            
!                                                                        
!                                                                        
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      catn12 (5) = 'AAAAA'                                              
      ivcomp = 0                                                        
      ivcorr = 1                                                        
 0322 format (20x,10x,a10,35x)                                          
      read (i09, 0322, end = 0323) catn12 (5)                           
 0323 if (catn12(5)  ==  'PQRST')  ivcomp = 1                           
40320 if (ivcomp - 1) 20320, 10320, 20320                               
30320 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10320, 0331, 20320                                    
10320 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0331                                                        
20320 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0331 continue                                                          
!                                                                        
!                                                                        
!          TESTS 33 AND 34 READ A LONG INPUT FIELD (50 CHARACTERS) AND   
!      CHECK RESULTING INTERNAL REPRESENTATION.  THE RECORD IS READ      
!      WITH THE SAME A EDIT DESCRIPTOR AS WAS USED TO WRITE THE RECORD.  
!                                                                        
!                                                                        
70034 format (12x,2i4,60x)                                              
      rewind i09                                                        
      do i = 1,150                                                 
      read (i09, 70034, end = 4036) irecn, ieof                         
      if (irecn  ==  140) goto 4036                                      !Break
  end do
 4036 if (irecn - 140) 4037, 4038, 4037                                 
!          THE ABOVE CODE POSITIONS THE FILE TO RECORD NUMBER 140 FOR    
!      TESTS 33 AND 34.                                                  
!                                                                        
70035 format (  "    THE START RECORD FOR TESTS 33 AND 34 COULD NOT BE  FOUND,")                                                          
70036 format (" THEREFORE TESTS 33 AND 34     ARE   DELETED." )         
 4037 write (i02, 70035)                                                
      write (i02, 70036)                                                
      goto 351                                                         
 4038 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 033  ****                         
!                                                                        
!                                                                        
!          TEST 33 READS A LONG FIELD WITH THE WIDTH SPECIFIED ON THE  A 
!      EDIT DESCRIPTOR.  RECORD NUMBER 141 IS READ.   THE RECORD WAS     
!      CREATED IN TEST 18 AND CONTAINS FIELD DATA OF                     
!                                                                        
!                  'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789'  
!                                                                        
!      WITHOUT THE SURROUNDING APOSTROPHES.                              
!                                                                        
!                                                                        
!                                                                        
      ivtnum =  33                                                      
      if (iczero) 30330, 0330, 30330                                    
 0330 continue                                                          
      cvtn15 = '                                                   '    
      ivcomp = 0                                                        
      ivcorr = 1                                                        
 0332 format (20x,10x,a50)                                              
      read (i09, 0332) cvtn15                                           
      if (cvtn15  ==  'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789')   ivcomp = 1                                                  
40330 if  (ivcomp -1 ) 20330, 10330, 20330                              
30330 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10330, 0341, 20330                                    
10330 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0341                                                        
20330 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0341 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 402  -  TEST 034  ****                         
!                                                                        
!                                                                        
!          TEST 34 READS A LONG FIELD USING THE A EDIT DESCRIPTOR        
!      WITHOUT THE OPTIONAL WIDTH SPECIFIED.  RECORD NUMBER 142 IS READ. 
!      THE RECORD WAS CREATED IN TEST 19 AND CONTAINS THE FIELD DATA     
!                                                                        
!                  'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789'  
!                                                                        
!      WITHOUT THE SURROUNDING APOSTROPHES.                              
!                                                                        
!                                                                        
      ivtnum =  34                                                      
      if (iczero) 30340, 0340, 30340                                    
 0340 continue                                                          
      cvtn15 = '                                                   '    
      ivcomp = 0                                                        
      ivcorr = 1                                                        
 0342 format (20x,10x,a)                                                
      read (i09, 0342) cvtn15                                           
      if (cvtn15  ==  'ABCDEFG    HIJKLMN    OPQRSTUVWXYZ      0123456789')   ivcomp = 1                                                  
40340 if (ivcomp - 1) 20340, 10340, 20340                               
30340 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10340, 0351, 20340                                    
10340 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0351                                                        
20340 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0351 continue                                                          
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
! DB**   BEGIN FILE DUMP CODE                                            
!      REWIND I09                                                        
!      IRNUM = 1                                                         
!      IRLGN = 80                                                        
!      ILUN  = I09                                                       
! 7701 FORMAT     (I3,I2,I4,I3,2I4,60A1)                                 
! 7702 FORMAT (" ",I3,I2,I4,I3,2I4,60A1)                                 
! 7703 FORMAT (10X,"FILE ",I2," HAS ",I3," RECORDS - OK" )               
! 7704 FORMAT (10X,"FILE ",I2," HAS ",I3," RECORDS - THERE SHOULD BE " , 
!     1I3,9H RECORDS.)                                                   
!      DO 7771 IRNUM = 1, ITOTR                                          
!      READ (ILUN, 7701)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1    (IDUMP(ICH), ICH = 1,60)                                      
!      WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1    (IDUMP(ICH), ICH = 1,60)                                      
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
!     1    (IDUMP(ICH), ICH = 1,60)                                      
!      WRITE (I02, 7702)  IPROG, IFILE, ITOTR, IRLGN, IRECN, IEOF,       
!     1    (IDUMP(ICH), ICH = 1,60)                                      
!      IRNUM = IRNUM + 1                                                 
!      IF (IEOF .EQ. 9999)  GO TO 7777                                   
! 7776 CONTINUE                                                          
! 7777 WRITE  (I02, 7704)  ILUN, IRNUM, ITOTR                            
! 7779 CONTINUE                                                          
! DE**      END OF DUMP CODE                                             
!                                                                        
!          THERE SHOULD BE  34 TESTS IN THIS ROUTINE                     
!                                                                        
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
90001 format (" ",24x,"FM402")                                          
90000 format (" ",20x,"END OF PROGRAM FM402" )                          
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
      end program fm402
