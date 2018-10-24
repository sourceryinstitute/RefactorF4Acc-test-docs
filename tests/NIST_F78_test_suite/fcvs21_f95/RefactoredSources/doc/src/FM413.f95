      program fm413
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE TESTS FOR PROPER PROCESSING OF UNFORMATTED RECORDS
!      IN FILES CONNECTED FOR DIRECT ACCESS.  FOR THE SUBSET LANGUAGE A  
!      FILE CONNECTED FOR DIRECT ACCESS MUST HAVE UNFORMATTED RECORDS    
!      THIS ROUTINE FIRST TESTS SEVERAL SYNTACTICAL VARIATIONS OF THE    
!      READ AND WRITE STATEMENTS USED IN CREATING AND ACCESSING          
!      RECORDS OF THE FILE.  THE OPEN STATEMENT IS USED TO CONNECT       
!      THE FILE TO  A  UNIT  AND ESTABLISH ITS CONNECTION FOR DIRECT     
!      ACCESS.  THE FIRST SERIES OF TESTS CREATE  AND ACCESS   THE       
!      RECORDS OF THE FILE IN RECORD NUMBER SEQUENCE AND THE LAST        
!      SERIES OF TESTS CREATE AND ACCESS   RECORDS OF THE FILE IN RANDOM 
!      ORDER.                                                            
!                                                                        
!         UNFORMATTED RECORDS MAY HAVE BOTH CHARACTER AND NONCHARACTER   
!      DATA AND THIS DATA IS TRANSFERRED WITHOUT EDITING BETWEEN THE     
!      CURRENT RECORD AND THE ENTITIES SPECIFIED BY THE INPUT/OUTPUT     
!      LIST.   THIS ROUTINE BOTH READS AND WRITES RECORDS CONTAINING     
!      THE DATA TYPES OF INTEGER ,REAL AND LOGICAL WITH I/O LIST ITEMS   
!      REPRESENTED AS VARIABLE NAMES, ARRAY ELEMENT NAMES AND ARRAY      
!      NAMES.  THIS ROUTINE DOES NOT TEST DATA OF TYPE CHARACTER.        
!                                                                        
!           ROUTINE FM411 TESTS USE OF UNFORMATTED RECORDS               
!      WITH A FILE CONNECTED FOR SEQUENTIAL ACCESS.                      
!                                                                        
!         THIS ROUTINE TESTS                                             
!                                                                        
!              (1) THE STATEMENT CONSTRUCTS                              
!                                                                        
!                  A. WRITE (U,REC=RN)  VARIABLE-NAME,...                
!                  B. WRITE (U,REC=RN)  ARRAY-ELEMENT-NAME,...           
!                  C. WRITE (U,REC=RN)  ARRAY-NAME,...                   
!                  D. WRITE (U,REC=RN)            -  NO OUTPUT LIST      
!                  E. WRITE (U,REC=RN)  IMPLIED-DO-LIST                  
!                  F. READ (U,REC=RN)   VARIABLE-NAME,...                
!                  G. READ (U,REC=RN)   ARRAY-ELEMENT-NAME,...           
!                  H. READ (U,REC=RN)   ARRAY-NAME,...                   
!                  I. READ (U,REC=RN)             -  NO INPUT LIST       
!                  J. READ (U,REC=RN)   IMPLIED-DO-LIST                  
!                                                                        
!              (2) USE OF A READ STATEMENT WHERE THE NUMBER OF VALUES    
!                  IN THE INPUT LIST IS LESS THAN OR EQUAL TO THE        
!                  NUMBER OF VALUES IN THE RECORD.                       
!              (3) USE OF THE STATEMENT                                  
!                       OPEN (U,ACCESS='DIRECT',RECL=RL)                 
!                  FOR CONNECTING A FILE TO THE UNIT.                    
!                                                                        
!              (4) THAT THE RECORDS OF A DIRECT ACCESS FILE NEED NOT BE  
!                  BE CREATED AND READ IN ORDER OF THEIR RECORD NUMBERS. 
!                                                                        
!              (5) THAT THE VALUES OF THE RECORD MAY BE CHANGED WHEN     
!                  THE RECORD IS REWRITTEN.                              
!      REFERENCES -                                                      
!                                                                        
!            AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,    
!            X3.9-1977                                                   
!                                                                        
!              SECTION 4.1,        DATA TYPES                            
!              SECTION 12.1.2,     UNFORMATTED RECORD                    
!              SECTION 12.2.4,     FILE ACCESS                           
!              SECTION 12.2.4.2,   DIRECT ACCESS                         
!              SECTION 12.3.3,     UNIT SPECIFIER AND IDENTIFIER         
!              SECTION 12.7.2,     END-OF-FILE SPECIFIER                 
!              SECTION 12.8,       READ, WRITE AND PRINT STATEMENTS      
!              SECTION 12.8.1,     CONTROL INFORMATION LIST              
!              SECTION 12.8.2,     INPUT/OUTPUT LIST                     
!              SECTION 12.8.2.1,   INPUT LIST ITEMS                      
!              SECTION 12.8.2.2,   OUTPUT LIST ITEMS                     
!              SECTION 12.8.2.3,   IMPLIED-DO  LIST                      
!              SECTION 12.9.5.1,   UNFORMATTED DATA TRANSFER             
!              SECTION 12.10.1,    OPEN STATEMENT                        
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
      integer :: icon21
      integer :: icon22
      integer :: icon31
      integer :: icon32
      integer :: icon33
      integer :: icon34
      integer :: icon55
      integer :: icon56
      real :: rcon21
      real :: rcon22
      real :: rcon31
      real :: rcon32
      real :: rcon33
      real :: rcon34
      real :: rcon55
      real :: rcon56
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: i10
      integer :: iprog
      integer :: ifile
      integer :: itotr
      integer :: irlgn
      integer :: irecn
      integer :: ieof
      integer :: ivtnum
      integer :: ivcorr
      integer :: ivcomp
      integer :: k
      integer :: i
      integer :: j
      integer :: irec
      integer :: ivon22
      integer :: ivon56
      integer :: ivon21
      integer :: ivon31
      integer :: ivon32
      integer :: ivon33
      integer :: ivon34
      integer :: ivon55
      real :: rvon22
      real :: rvon31
      real :: rvon21
      real :: rvon32
      real :: rvon33
      real :: rvon34
      real :: rvon55
      real :: rvon56
      integer :: irecck
      integer :: ivon61
      integer :: ivon62
      integer :: ivon63
      integer :: ivon64
      integer :: ivon65
      integer :: ivon66
      integer :: ivon67
      integer :: ivon68
      logical, dimension(1:8) :: laon11
      logical, dimension(1:2,1:4) :: laon21
      logical, dimension(1:2,1:2,1:2) :: laon31
      logical :: lcont1
      logical :: lconf2
      logical :: lvont1
      logical :: lvonf2
      logical, dimension(1:8) :: laon12
      logical, dimension(1:2,1:4) :: laon22
      logical, dimension(1:2,1:2,1:2) :: laon32
      logical :: lcont3
      logical :: lconf4
      logical :: lvont3
      logical :: lvonf4
      logical :: lcont5
      logical :: lconf6
      logical :: lcont7
      logical :: lconf8
      logical :: lvont5
      logical :: lvonf6
      logical :: lvont7
      logical :: lvonf8
      integer, dimension(1:80) :: idump
      integer, dimension(1:8) :: iaon11
      integer, dimension(1:2,1:4) :: iaon21
      integer, dimension(1:2,1:2,1:2) :: iaon31
      integer, dimension(1:8) :: iaon12
      integer, dimension(1:2,1:4) :: iaon22
      integer, dimension(1:2,1:2,1:2) :: iaon32
      real, dimension(1:8) :: raon11
      real, dimension(1:2,1:4) :: raon21
      real, dimension(1:2,1:2,1:2) :: raon31
      real, dimension(1:8) :: raon12
      real, dimension(1:2,1:4) :: raon22
      real, dimension(1:2,1:2,1:2) :: raon32
      data iaon11 / 11,-11,777,-777,512,-512,-32767,32767 / 
      data iaon21 / 11,-11,777,-777,512,-512,-32767,32767 / 
      data iaon31 / 11,-11,777,-777,512,-512,-32767,32767 / 
      data laon11 / .true.,.false.,.true.,.false.,.true.,.false.,.true.,.false. / 
      data laon21 / .true.,.false.,.true.,.false.,.true.,.false.,.true.,.false. / 
      data laon31 / .true.,.false.,.true.,.false.,.true.,.false.,.true.,.false. / 
      data raon11 / 11.,-11.,7.77,-7.77,.512,-.512,-32767.,32767. / 
      data raon21 / 11.,-11.,7.77,-7.77,.512,-.512,-32767.,32767. / 
      data raon31 / 11.,-11.,7.77,-7.77,.512,-.512,-32767.,32767. / 
      icon21 = 11                                                       
      icon22 = -11                                                      
      icon31 = +777                                                     
      icon32 = -777                                                     
      icon33 =  512                                                     
      icon34 = -512                                                     
      icon55 = -32767                                                   
      icon56 =  32767                                                   
      rcon21 = 11.                                                      
      rcon22 = -11.                                                     
      rcon31 = +7.77                                                    
      rcon32 = -7.77                                                    
      rcon33 = .512                                                     
      rcon34 = -.512                                                    
      rcon55 = -32767.                                                  
      rcon56 =  32767.                                                  
      lcont1 = .true.                                                   
      lconf2 = .false.                                                  
      lcont3 = .true.                                                   
      lconf4 = .false.                                                  
      lcont5 = .true.                                                   
      lconf6 = .false.                                                  
      lcont7 = .true.                                                   
      lconf8 = .false.                                                  
!                                                                        
!           THE FILE USED IN THIS ROUTINE HAS THE FOLLOWING PROPERTIES   
!                                                                        
!                   FILE IDENTIFIER     - I10 (X-NUMBER 10)              
!                   RECORD SIZE         - 80                             
!                   ACCESS METHOD       - DIRECT                         
!                   RECORD TYPE         - UNFORMATTED                    
!                   DESIGNATED DEVICE   - DISK                           
!                   TYPE OF DATA        - INTEGER, REAL AND LOGICAL      
!                   RECORDS IN FILE     - 214                            
!                                                                        
!           THE FIRST 6 FIELDS OF EACH RECORD IN THE FILE UNIQUELY IDENT-
!      IFIES THAT RECORD.  THE REMAINING FIELDS OF THE RECORD CONTAIN    
!      DATA WHICH ARE USED IN TESTING.  A DESCRIPTION OF EACH FIELD      
!      OF THE  PREAMBLE FOLLOWS.                                         
!                                                                        
!                   VARIABLE NAME IN PROGRAM          FIELD NUMBER       
!                   ------------------------          ------------       
!                                                                        
!                   IPROG  (ROUTINE NAME)         -       1              
!                   IFILE  (LOGICAL/X-NUMBER)     -       2              
!                   ITOTR  (RECORDS IN FILE)      -       3              
!                   IRLGN  (LENGTH OF RECORD)     -       4              
!                   IRECN  (RECORD NUMBER)        -       5              
!                   IEOF   (9999 IF LAST RECORD)  -       6              
!                                                                        
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
      i10 = 9                                                           
!      I10  CONTAINS THE LOGICAL UNIT NUMBER FOR A DIRECT ACCESS FILE    
!      WITH UNFORMATTED RECORDS                                          
! X100        THE CARD IS REPLACED BY CONTENTS OF X-100 CARD             
! X101        THE CARD IS REPLACED BY CONTENTS OF X-101 CARD             
      iprog = 413                                                       
      ifile = i10                                                       
      itotr = 214                                                       
      irlgn = 80                                                        
      irecn = 0                                                         
      ieof = 0                                                          
!                                                                        
!                                                                        
!                                                                        
!         TESTS 001 THROUGH 013 OPEN A FILE CONNECTED FOR DIRECT ACCESS  
!      AND WRITE 12 RECORDS INTO THE FILE.  THESE TESTS TEST USE OF THE  
!      ALLOWABLE FORMS OF THE OPEN AND WRITE STATEMENTS ON A FILE        
!      CONNECTED FOR DIRECT ACCESS.  THE WRITE STATEMENT IS USED WITH    
!      THE I/O LIST ITEM AS A VARIABLE, ARRAY ELEMENT AND AN ARRAY.      
!         THE PURPOSE OF TESTS 001 THROUGH 013 IS TO CHECK THE COMPILER'S
!      ABILITY TO HANDLE THE VARIOUS STATEMENT  CONSTRUCTS OF THE OPEN   
!      AND WRITE STATEMENTS.  LATER TESTS WITHIN THIS ROUTINE READ       
!      AND CHECK THE RECORDS WHICH WERE CREATED.                         
!         THE VALUE IN IVCORR FOR TESTS 002 THROUGH 013 IS THE RECORD    
!      NUMBER USED TO WRITE THE RECORD.                                  
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 001  ****                         
!                                                                        
!                                                                        
!         TEST 001 USES THE OPEN STATEMENT TO CONNECT A FILE FOR DIRECT  
!        ACCESS.  THIS IS THE FIRST ROUTINE TO USE AN OPEN STATEMENT.    
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcorr = 1                                                        
      ivcomp = 0                                                        
      open ( i10, access = 'DIRECT', recl = 80 )                        
      ivcomp = 1                                                        
40010 if (ivcomp - 1) 20010, 10010, 20010                               
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10010, 0021, 20010                                    
10010 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0021                                                        
20010 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 002  ****                         
!                                                                        
!                                                                        
!         TEST 002 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS A VARIABLE OF INTEGER TYPE.                                    
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      irecn = 01                                                        
      ivcorr = 01                                                       
      write (i10,rec=01)    iprog, ifile, itotr, irlgn, irecn, ieof,       icon21, icon22, icon31, icon32, icon33, icon34, icon55, icon56 
      ivcomp = irecn                                                    
40020 if (ivcomp - 01)  20020, 10020, 20020                             
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10020, 0031, 20020                                    
10020 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0031                                                        
20020 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0031 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 003  ****                         
!                                                                        
!                                                                        
!         TEST 003 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS A VARIABLE OF REAL TYPE.                                       
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      irecn = 02                                                        
      ivcorr = 02                                                       
      write (i10,rec=02)    iprog, ifile, itotr, irlgn, irecn, ieof,       rcon21, rcon22, rcon31, rcon32, rcon33, rcon34, rcon55, rcon56 
      ivcomp = irecn                                                    
40030 if (ivcomp - 02)  20030, 10030, 20030                             
30030 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10030, 0041, 20030                                    
10030 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0041                                                        
20030 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0041 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 004  ****                         
!                                                                        
!                                                                        
!         TEST 004 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS A VARIABLE OF LOGICAL TYPE.                                    
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      irecn = 03                                                        
      ivcorr = 03                                                       
      write (i10,rec=03)    iprog, ifile, itotr, irlgn, irecn, ieof,       lcont1, lconf2,  lcont3, lconf4, lcont5, lconf6, lcont7, lconf8
      ivcomp = irecn                                                    
40040 if (ivcomp - 03)  20040, 10040, 20040                             
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10040, 0051, 20040                                    
10040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0051                                                        
20040 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 005  ****                         
!                                                                        
!                                                                        
!         TEST 005 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN ARRAY ELEMENT OF INTEGER TYPE.   ONE, TWO AND THREE         
!      DIMENSION ARRAYS ARE USED.                                        
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      irecn = 04                                                        
      ivcorr = 04                                                       
      write (i10,rec=04)    iprog, ifile, itotr, irlgn, irecn, ieof,       iaon11(1), iaon11(2), iaon21(1,2), iaon21(2,2), iaon31(1,1,2),    iaon31(2,1,2), iaon11(7), iaon11(8)                            
      ivcomp = irecn                                                    
40050 if (ivcomp - 04)  20050, 10050, 20050                             
30050 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10050, 0061, 20050                                    
10050 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0061                                                        
20050 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 006  ****                         
!                                                                        
!                                                                        
!         TEST 006 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN ARRAY ELEMENT OF REAL TYPE.  ONE, TWO AND THREE             
!      DIMENSION ARRAYS ARE USED.                                        
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      irecn = 05                                                        
      ivcorr = 05                                                       
      write (i10,rec=05)    iprog, ifile, itotr, irlgn, irecn, ieof,       raon11(1), raon11(2), raon21(1,2), raon21(2,2), raon31(1,1,2),    raon31(2,1,2), raon11(7), raon11(8)                            
      ivcomp = irecn                                                    
40060 if (ivcomp - 05)  20060, 10060, 20060                             
30060 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10060, 0071, 20060                                    
10060 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0071                                                        
20060 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0071 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 007  ****                         
!                                                                        
!                                                                        
!         TEST 007 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN ARRAY ELEMENT OF LOGICAL TYPE.  ONE, TWO AND THREE          
!      DIMENSION ARRAYS ARE USED.                                        
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      irecn = 06                                                        
      ivcorr = 06                                                       
      write (i10,rec=06)    iprog, ifile, itotr, irlgn, irecn, ieof,       laon11(1), laon11(2), laon21(1,2), laon21(2,2), laon31(1,1,2),    laon31(2,1,2), laon11(7), laon11(8)                            
      ivcomp = irecn                                                    
40070 if (ivcomp - 06)  20070, 10070, 20070                             
30070 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10070, 0081, 20070                                    
10070 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0081                                                        
20070 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0081 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 008  ****                         
!                                                                        
!                                                                        
!         TEST 008 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN ARRAY OF INTEGER TYPE.                                      
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      irecn = 07                                                        
      ivcorr = 07                                                       
      write (i10,rec=07)    iprog, ifile, itotr, irlgn, irecn, ieof,       iaon31                                                         
      ivcomp = irecn                                                    
40080 if (ivcomp - 07)  20080, 10080, 20080                             
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
!      ****  FCVS PROGRAM 413  -  TEST 009  ****                         
!                                                                        
!                                                                        
!         TEST 009 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN ARRAY OF REAL TYPE.                                         
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      irecn = 08                                                        
      ivcorr = 08                                                       
      write (i10,rec=08)    iprog, ifile, itotr, irlgn, irecn, ieof,       raon31                                                         
      ivcomp = irecn                                                    
40090 if (ivcomp - 08)  20090, 10090, 20090                             
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
!      ****  FCVS PROGRAM 413  -  TEST 010  ****                         
!                                                                        
!                                                                        
!         TEST 010 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN ARRAY OF LOGICAL TYPE.                                      
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      irecn = 09                                                        
      ivcorr = 09                                                       
      write (i10,rec=09)    iprog, ifile, itotr, irlgn, irecn, ieof,       laon31                                                         
      ivcomp = irecn                                                    
40100 if (ivcomp - 09)  20100, 10100, 20100                             
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
!      ****  FCVS PROGRAM 413  -  TEST 011  ****                         
!                                                                        
!                                                                        
!         TEST 011 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN IMPLIED-DO   WITH AN ITEM OF INTEGER TYPE.                  
!         THE FIELD VALUES ARE WRITTEN IN MIXED ORDER VIS-A-VIS THE      
!      ELEMENT SEQUENCE OF ARRAY IAON31.  THE SEQUENCE OF VALUES WRITTEN 
!      IN THE RECORD ARE 11, 512, 777, -32767, -11, -512, -777, 32767.   
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      irecn = 10                                                        
      ivcorr = 10                                                       
      write (i10,rec=10)    iprog, ifile, itotr, irlgn, irecn, ieof,       (((iaon31 (j,k,i), i=1,2), k=1,2), j=1,2)                      
      ivcomp = irecn                                                    
40110 if (ivcomp - 10)  20110, 10110, 20110                             
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
!      ****  FCVS PROGRAM 413  -  TEST 012  ****                         
!                                                                        
!                                                                        
!         TEST 012 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN IMPLIED-DO WITH AN ITEM OF REAL TYPE.  THE FIELD VALUES     
!      (IN FIELD POSITION ORDER) WRITTEN IN THE RECORD ARE 11., -11.,    
!      7.77, -7.77, .512, -.512, -32767., 32767.                         
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      irecn = 11                                                        
      ivcorr = 11                                                       
      write (i10,rec=11)    iprog, ifile, itotr, irlgn, irecn, ieof,       (((raon31 (j,k,i), j=1,2), k=1,2), i=1,2)                      
      ivcomp = irecn                                                    
40120 if (ivcomp - 11)  20120, 10120, 20120                             
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
!      ****  FCVS PROGRAM 413  -  TEST 013  ****                         
!                                                                        
!                                                                        
!         TEST 013 USES A WRITE STATEMENT WHERE THE OUTPUT LIST ITEM     
!      IS AN IMPLIED-DO   WITH AN ITEM OF LOGICAL TYPE.                  
!         THE FIELD VALUES ARE WRITTEN IN MIXED ORDER VIS-A-VIS THE      
!      ELEMENT SEQUENCE OF ARRAY LAON31.  THE SEQUENCE OF VALUES WRITTEN 
!      IN THE RECORD ARE .TRUE., .TRUE., .FALSE., .FALSE., .TRUE., .TRUE.
!      .FALSE, .FALSE.                                                   
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      irecn = 12                                                        
      ivcorr = 12                                                       
      write (i10,rec=12)    iprog, ifile, itotr, irlgn, irecn, ieof,       (((laon31 (j,k,i), k=1,2), j=1,2), i=1,2)                      
      ivcomp = irecn                                                    
40130 if (ivcomp - 12)  20130, 10130, 20130                             
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
!         TESTS  14 AND 15 TEST THE WRITE WITHOUT OUTPUT LIST ITEMS.     
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 014  ****                         
!                                                                        
!                                                                        
!         TEST 014 USES A WRITE STATEMENT WITHOUT ANY OUTPUT LIST ITEMS. 
!      THE OUTPUT LIST ITEMS ARE OPTIONAL  AND THIS TEST USES THIS FORM  
!      TO ESTABLISH A RECORD NUMBER FOR A RECORD IN THE FILE.            
!      ALSO THE LENGTH OF AN UNFORMATTED RECORD MAY BE ZERO.             
!                                                                        
!                   SEE SECTIONS 12.1.2,   UNFORMATTED RECORDS           
!                                12.2.4.2 (5) AND (6), DIRECT ACCESS     
!                                12.8,  READ, WRITE AND PRINT STATEMENTS 
!                                                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      irecn = 13                                                        
      ivcorr = 13                                                       
      write (i10,rec=13)                                                
      ivcomp = irecn                                                    
40140 if (ivcomp - 13)  20140, 10140, 20140                             
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
!      ****  FCVS PROGRAM 413  -  TEST 015  ****                         
!                                                                        
!                                                                        
!         TEST 015 IS SIMILAR TO TEST 014 ABOVE EXCEPT THE RN OF THE     
!      RECORD SPECIFIER (REC = RN) IS AN INTEGER VARIABLE.               
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      irecn = 14                                                        
      ivcorr = 14                                                       
      irec = 14                                                         
      write (i10,rec = irec)                                            
      ivcomp = irecn                                                    
40150 if (ivcomp - 14)  20150, 10150, 20150                             
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
!         TESTS  16  AND  17  VERIFY THAT RECORDS MAY BE CREATED IN      
!      OTHER THAN SEQUENTIAL ORDER.  ALSO THAT A VARIABLE MAY BY USED    
!      AS THE OPERAND OF THE REC SPECIFIER FOR A WRITE STATEMENT.        
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 016  ****                         
!                                                                        
!                                                                        
!         TEST 016 TESTS USE OF THE REC SPECIFIER WHERE THE OPERAND      
!      IS A VARIABLE.  THIS TEST IS SIMILAR  TO TEST 15 EXCEPT THE WRITE 
!      STATEMENT CONTAINS OUTPUT LIST ITEMS.  ONE HUNDRED RECORDS ARE    
!      WRITTEN BY INCREMENTING THE VARIABLE BY 2 FOR EACH WRITE.   TEST  
!      032 READS THE RECORDS WRITTEN BY THIS METHOD.                     
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      irecn = 13                                                        
      irec = 13                                                         
      do i = 1,100                                                 
      irec = irec + 2                                                   
      irecn = irecn + 2                                                 
      write (i10, rec = irec) iprog, ifile, itotr, irlgn, irecn, ieof,     icon21, icon22, icon31, icon32, icon33, icon34, icon55, icon56 
  end do
      ivcorr = 100                                                      
      ivcomp = irec - 113                                               
40160 if (ivcomp - 100) 20160, 10160, 20160                             
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
!      ****  FCVS PROGRAM 413  -  TEST 017  ****                         
!                                                                        
!                                                                        
!         TEST  17 IS SIMILAR  TO TEST 16 EXCEPT THE RECORD IS           
!      WRITTEN IN REVERSE ORDER OF RECORD NUMBER.  ONE HUNDERD RECORDS   
!      ARE WRITTEN AND THE VARIABLE OF THE REC SPECIFIER IS DECREMENTED  
!      BY TWO FOR EACH WRITE.                                            
!                                                                        
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      irecn = 216                                                       
      irec = 216                                                        
      ivcomp = 0                                                        
      do i=1,100                                                   
      irec = irec - 2                                                   
      irecn = irecn - 2                                                 
      write (i10, rec = irec) iprog, ifile, itotr, irlgn, irecn, ieof,     icon21, icon22, icon31, icon32, icon33, icon34, icon55, icon56 
      ivcomp = ivcomp + 1                                               
  end do
      ivcorr = 100                                                      
40170 if (ivcomp - 100) 20170, 10170, 20170                             
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
!         TESTS 018 THROUGH 030 READ AND CHECK THE RECORDS CREATED IN    
!      TESTS 002 THROUGH 014.  EACH OF THE TESTS IN THIS SET IS CHECKING 
!      TWO THINGS.  FIRST, THAT THE READ STATEMENT CONSTRUCT IS ACCEPTED 
!      BY THE COMPILER AND SECOND THAT THE RECORDS CREATED IN TESTS 002  
!      THROUGH 013 AND READ IN THESE TESTS CAN GIVE PREDICTIBLE VALUES.  
!      THE READ STATEMENT IS USED WITH THE I/O LIST ITEM  AS A VARIABLE, 
!      AN ARRAY ELEMENT AND AN ARRAY.                                    
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 018  ****                         
!                                                                        
!                                                                        
!         TEST 018 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      VARIABLE OF INTEGER TYPE.                                         
!                                                                        
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivon22 = 0                                                        
      ivon56 = 0                                                        
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 01) iprog, ifile, itotr, irlgn, irecn, ieof,        ivon21, ivon22, ivon31, ivon32, ivon33, ivon34, ivon55, ivon56 
      if (irecn  ==  01)     ivcomp = ivcomp * 2                        
      if (ivon22  ==  -11)   ivcomp = ivcomp * 3                        
      if (ivon56  ==  32767) ivcomp = ivcomp * 5                        
40180 if (ivcomp - 30)  20180, 10180, 20180                             
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
!      ****  FCVS PROGRAM 413  -  TEST 019  ****                         
!                                                                        
!                                                                        
!         TEST 019 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      VARIABLE OF REAL TYPE.                                            
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      rvon22 = 0.0                                                      
      rvon31 = 0.0                                                      
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 02) iprog, ifile, itotr, irlgn, irecn, ieof,        rvon21, rvon22, rvon31, rvon32, rvon33, rvon34, rvon55, rvon56 
      if (irecn  ==  02)      ivcomp = ivcomp * 2                       
      if (rvon22  ==  -11.)  ivcomp = ivcomp * 3                        
      if (rvon31  ==  7.77)  ivcomp = ivcomp * 5                        
40190 if (ivcomp - 30)  20190, 10190, 20190                             
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
!      ****  FCVS PROGRAM 413  -  TEST 020  ****                         
!                                                                        
!                                                                        
!         TEST 020 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      VARIABLE OF LOGICAL TYPE.                                         
!                                                                        
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      lvont1 = .false.                                                  
      lvonf6 = .true.                                                   
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 03) iprog, ifile, itotr, irlgn, irecn, ieof,        lvont1, lvonf2,  lvont3, lvonf4, lvont5, lvonf6, lvont7, lvonf8
      if (irecn  ==  03)     ivcomp = ivcomp * 2                        
      if (.not. lvonf6)      ivcomp = ivcomp * 3                        
      if (lvont1)            ivcomp = ivcomp * 5                        
40200 if (ivcomp - 30)  20200, 10200, 20200                             
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
!      ****  FCVS PROGRAM 413  -  TEST 021  ****                         
!                                                                        
!                                                                        
!         TEST 021 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      ARRAY ELEMENT OF INTEGER TYPE.  ONE, TWO, AND THREE               
!      DIMENSION ARRAYS ARE USED.                                        
!                                                                        
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      iaon12(2) = 0                                                     
      iaon12(8) = 0                                                     
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 04) iprog, ifile, itotr, irlgn, irecn, ieof,        iaon12(1), iaon12(2), iaon22(1,2), iaon22(2,2), iaon32(1,1,2),    iaon32(2,1,2), iaon12(7), iaon12(8)                            
      if (irecn  ==  04)   ivcomp = ivcomp * 2                          
      if (iaon12(2)  ==  -11)   ivcomp = ivcomp * 3                     
      if (iaon12(8)  ==  32767) ivcomp = ivcomp * 5                     
!                                                                        
!         THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 
!      FIELD VALUE AND A POSITIVE FIELD VALUE.                           
!                                                                        
40210 if (ivcomp - 30)   20210, 10210, 20210                            
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
!      ****  FCVS PROGRAM 413  -  TEST 022  ****                         
!                                                                        
!                                                                        
!         TEST 022 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      ARRAY ELEMENT OF REAL TYPE.  ONE, TWO, AND THREE                  
!      DIMENSION ARRAYS ARE USED.                                        
!                                                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      raon22(2,2) = 0.0                                                 
      raon32(1,1,2) = 0.0                                               
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 05) iprog, ifile, itotr, irlgn, irecn, ieof,        raon12(1), raon12(2), raon22(1,2), raon22(2,2), raon32(1,1,2),    raon32(2,1,2), raon12(7), raon12(8)                            
      if (irecn  ==  05)             ivcomp = ivcomp * 2                
      if (raon22(2,2)  ==  -7.77)    ivcomp = ivcomp * 3                
      if (raon32(1,1,2)  ==   .512 ) ivcomp = ivcomp * 5                
!                                                                        
!         THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 
!      FIELD VALUE AND A POSITIVE FIELD VALUE.                           
!                                                                        
40220 if (ivcomp - 30)   20220, 10220, 20220                            
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
!      ****  FCVS PROGRAM 413  -  TEST 023  ****                         
!                                                                        
!                                                                        
!         TEST 023 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      ARRAY ELEMENT OF LOGICAL TYPE.  ONE, TWO, AND THREE               
!      DIMENSION ARRAYS ARE USED.                                        
!                                                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      laon12(1) = .false.                                               
      laon32(2,1,2) = .true.                                            
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 06) iprog, ifile, itotr, irlgn, irecn, ieof,        laon12(1), laon12(2), laon22(1,2), laon22(2,2), laon32(1,1,2),    laon32(2,1,2), laon12(7), laon12(8)                            
      if (irecn  ==  06)   ivcomp = ivcomp * 2                          
      if (laon12(1))          ivcomp = ivcomp * 3                       
      if (.not. laon32(2,1,2))  ivcomp = ivcomp * 5                     
40230 if (ivcomp - 30)   20230, 10230, 20230                            
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
!      ****  FCVS PROGRAM 413  -  TEST 024  ****                         
!                                                                        
!                                                                        
!         TEST 024 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      ARRAY OF INTEGER TYPE.                                            
!                                                                        
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      iaon32(2,1,1) = 0                                                 
      iaon32(2,2,2) = 0                                                 
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 07) iprog, ifile, itotr, irlgn, irecn, ieof,        iaon32                                                         
      if (irecn  ==  07)   ivcomp = ivcomp * 2                          
      if (iaon32(2,1,1)  ==  -11)    ivcomp = ivcomp * 3                
      if (iaon32(2,2,2)  ==  32767)  ivcomp = ivcomp * 5                
!                                                                        
!         THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 
!      FIELD VALUE AND A POSITIVE FIELD VALUE.                           
!                                                                        
40240 if (ivcomp - 30)   20240, 10240, 20240                            
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
!      ****  FCVS PROGRAM 413  -  TEST 025  ****                         
!                                                                        
!                                                                        
!         TEST 025 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      ARRAY OF REAL TYPE.                                               
!                                                                        
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      raon32(2,1,1) = 0.0                                               
      raon32(2,2,2) = 0.0                                               
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 08) iprog, ifile, itotr, irlgn, irecn, ieof,        raon32                                                         
      if (irecn  ==  08)   ivcomp = ivcomp * 2                          
      if (raon32(2,1,1)  ==  -11.)   ivcomp = ivcomp * 3                
      if (raon32(2,2,2)  ==   32767.) ivcomp = ivcomp * 5               
!                                                                        
!         THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 
!      FIELD VALUE AND A POSITIVE FIELD VALUE.                           
!                                                                        
40250 if (ivcomp - 30)   20250, 10250, 20250                            
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
!      ****  FCVS PROGRAM 413  -  TEST 026  ****                         
!                                                                        
!                                                                        
!         TEST 026 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      ARRAY OF LOGICAL TYPE.                                            
!                                                                        
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      laon32(1,1,1) = .false.                                           
      laon32(2,2,2) = .true.                                            
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 09) iprog, ifile, itotr, irlgn, irecn, ieof,        laon32                                                         
      if (irecn  ==  09)   ivcomp = ivcomp * 2                          
      if (laon32(1,1,1))     ivcomp = ivcomp * 3                        
      if (.not. laon32(2,2,2))    ivcomp = ivcomp * 5                   
40260 if (ivcomp - 30)   20260, 10260, 20260                            
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
!      ****  FCVS PROGRAM 413  -  TEST 027  ****                         
!                                                                        
!                                                                        
!         TEST 027 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      IMPLIED-DO WITH AN ITEM OF INTEGER TYPE.  THE STORAGE VALUES IN   
!      THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  
!      DIFFERENT STORAGE SEQUENCE IN THE ARRAY THAN FOUND IN THE RECORD  
!      OF THE FILE.  THIS RECORD IS RECORD NUMBER 10 AND WAS CREATED IN  
!      TEST 012 ABOVE.  THE FIELD VALUE, FIELD POSITION, POSITION WITHIN 
!      ARRAY IAON32 AND SUBSCRIPT VALUE AFTER THE READ IS                
!                                                                        
!       VALUE      11    777     512  -32767   -11   -777   -512   32767 
!       FIELD POS   1      3      2      4      5      7      6      8   
!       IAON32      1      2      3      4      5      6      7      8   
!       SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,2
!                                                                        
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      iaon32(2,1,1) = 0                                                 
      iaon32(2,2,1) = 0                                                 
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 10) iprog, ifile, itotr, irlgn, irecn, ieof,        (((iaon32 (j,k,i), k=1,2), j=1,2), i=1,2)                      
      if (irecn  ==  10)   ivcomp = ivcomp * 2                          
      if (iaon32(2,1,1)  ==  777)      ivcomp = ivcomp * 3              
      if (iaon32(2,2,1)  ==  -32767)   ivcomp = ivcomp * 5              
!                                                                        
!         THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 
!      FIELD VALUE AND A POSITIVE FIELD VALUE.                           
!                                                                        
40270 if (ivcomp - 30)   20270, 10270, 20270                            
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
!      ****  FCVS PROGRAM 413  -  TEST 028  ****                         
!                                                                        
!                                                                        
!         TEST 028 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      IMPLIED-DO WITH AN ITEM OF  REAL   TYPE.  THE STORAGE VALUES IN   
!      THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  
!      SEQUENCE THE SAME AS FOUND IN THE RECORD OF THE FILE.  THIS REC-  
!      ORD IS RECORD NUMBER 011 AND WAS CREATED IN TEST 013 ABOVE.       
!      THE FIELD VALUE, FIELD POSITION, POSITION WITHIN ARRAY RAON32  AND
!      SUBSCRIPT VALUE AFTER THE THE READ IS                             
!                                                                        
!       VALUE      11.   -11.   7.77   -7.77  .512   -.512 -32767. 32767.
!       FIELD POS   1      2      3      4      5      6      7      8   
!       RAON32      1      2      3      4      5      6      7      8   
!       SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,2
!                                                                        
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      raon32(1,2,1) = 0.0                                               
      raon32(1,2,2) = 0.0                                               
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 11) iprog, ifile, itotr, irlgn, irecn, ieof,        (((raon32 (j,k,i), j=1,2), k=1,2), i=1,2)                      
      if (irecn  ==  11)   ivcomp = ivcomp * 2                          
      if (raon32(1,2,1)  ==  7.77)     ivcomp = ivcomp * 3              
      if (raon32(1,2,2)  ==  -32767.)  ivcomp = ivcomp * 5              
!                                                                        
!         THE ABOVE 3 IF STATEMENTS CHECK THE RECORD NUMBER,  A NEGATIVE 
!      FIELD VALUE AND A POSITIVE FIELD VALUE.                           
!                                                                        
40280 if (ivcomp - 30)   20280, 10280, 20280                            
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
!      ****  FCVS PROGRAM 413  -  TEST 029  ****                         
!                                                                        
!                                                                        
!         TEST 029 USES A READ STATEMENT WHERE THE INPUT LIST ITEM IS A  
!      IMPLIED-DO WITH AN ITEM OF LOGICAL TYPE.  THE STORAGE VALUES IN   
!      THE ARRAY (BY THE IMPLIED-DO DURING THE READ) SHOULD RESULT IN A  
!      DIFFERENT STORAGE SEQUENCE IN THE ARRAY THAN FOUND IN THE RECORD  
!      OF THE FILE.  THIS RECORD IS RECORD NUMBER 12 AND WAS CREATED IN  
!      TEST 014 ABOVE.  THE FIELD VALUE, FIELD POSITION, POSITION WITHIN 
!      ARRAY LAON32 AND SUBSCRIPT VALUE AFTER THE READ IS                
!                                                                        
!       VALUE       T      T      F      F      T       T     F      F   
!       FIELD POS   1      5      3      7       2      6     4      8   
!       LAON32      1      2      3      4      5      6      7      8   
!       SUBSCRIPT 1,1,1   2,1,1  1,2,1  2,2,1  1,1,2  2,1,2  1,2,2  2,2,2
!                                                                        
!                                                                        
      ivtnum =  29                                                      
      if (iczero) 30290, 0290, 30290                                    
 0290 continue                                                          
      laon32(1,2,1) = .true.                                            
      laon32(2,1,1) = .false.                                           
      ivcorr = 30                                                       
      ivcomp = 1                                                        
      read (i10, rec = 12) iprog, ifile, itotr, irlgn, irecn, ieof,        (((laon32 (j,k,i), i=1,2), k=1,2), j=1,2)                      
      if (irecn  ==  12)   ivcomp = ivcomp * 2                          
      if ( .not. laon32(1,2,1))   ivcomp = ivcomp * 3                   
      if (laon32(2,1,1))          ivcomp = ivcomp * 5                   
40290 if (ivcomp - 30)   20290, 10290, 20290                            
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
!      ****  FCVS PROGRAM 413  -  TEST 030  ****                         
!                                                                        
!                                                                        
!         TEST 030 USES A READ STATEMENT WITHOUT ANY INPUT LIST ITEMS    
!      (INPUT LIST ITEMS ARE OPTIONAL FOR THE READ STATEMENT). THIS      
!      RECORD WAS WRITTEN IN TEST 14 AND SHOULD BE RECORD NUMBER 13.     
!      THE PURPOSE OF THIS TEST IS TO SEE THAT THE STATEMENT CONSTRUCT   
!      IS ACCEPTABLE TO THE COMPILER.                                    
!      ALSO THE LENGTH OF AN UNFORMATTED RECORD MAY BE ZERO.             
!                                                                        
!                   SEE SECTIONS 12.1.2,   UNFORMATTED RECORDS           
!                                12.8,   READ, WRITE AND PRINT STATEMENTS
!                                                                        
!                                                                        
      ivtnum =  30                                                      
      if (iczero) 30300, 0300, 30300                                    
 0300 continue                                                          
      irecn = 13                                                        
      ivcorr = 13                                                       
      read (i10, rec = 13)                                              
      ivcomp = irecn                                                    
40300 if (ivcomp - 13)   20300, 10300, 20300                            
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
!      ****  FCVS PROGRAM 413  -  TEST 031  ****                         
!                                                                        
!                                                                        
!         TEST 031 USES A READ STATEMENT IN WHICH THE NUMBER OF VALUES   
!      REQUIRED BY THE INPUT LIST IS LESS THAN THE NUMBER OF VALUES IN   
!      THE RECORD.                                                       
!                                                                        
!                   SEE SECTION 12.9.5.1, UNFORMATED DATA TRANSFER       
!                                                                        
!                                                                        
      ivtnum =  31                                                      
      if (iczero) 30310, 0310, 30310                                    
 0310 continue                                                          
      ivon21 = 0                                                        
      ivon22 = 0                                                        
      ivon31 = 0                                                        
      ivcorr = 0                                                        
      ivcomp = 1                                                        
      read (i10, rec = 01) iprog, ifile, itotr, irlgn, irecn, ieof,            ivon21, ivon22, ivon31                                     
      if (irecn  ==  01)  ivcomp = ivcomp * 2                           
      if (ivon21  ==  11)  ivcomp = ivcomp * 3                          
      if (ivon22  ==  -11) ivcomp = ivcomp * 5                          
40310 if (ivcomp - 30) 20310, 10310, 20310                              
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
!                                                                        
!         TEST 032 AND 033 VERIFIES THAT RECORDS MAY BE READ IN ANY ORDER
!      ALSO THAT A VARIABLE MAY BE USED AS THE OPERAND OF THE REC SPEC-  
!      IFIER FOR A READ STATEMENT.                                       
!                                                                        
!               SEE SECTION 2.2.4.2(1) , DIRECT ACCESS                   
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 413  -  TEST 032  ****                         
!                                                                        
!                                                                        
!         TEST 032 READS THE RECORDS WRITTEN IN TEST 16.  EVERY OTHER    
!      RECORD IS READ FOR A TOTAL OF 100 RECORDS (THE REC SPECIFIER      
!      VARIABLE IS INCREMENTED BY 2).                                    
!                                                                        
!                                                                        
      ivtnum =  32                                                      
      if (iczero) 30320, 0320, 30320                                    
 0320 continue                                                          
      irecck = 13                                                       
      irecn = 0                                                         
      irec = 13                                                         
      ivcomp = 0                                                        
      do i = 1,100                                                 
      irec = irec + 2                                                   
      irecck = irecck + 2                                               
      read (i10, rec = irec) iprog, ifile, itotr, irlgn, irecn, ieof,      ivon21, ivon22, ivon31, ivon32, ivon33, ivon34, ivon55, ivon56 
      if (irecn  ==  irecck)    ivcomp = ivcomp + 1                     
  end do
      ivcorr = 100                                                      
40320 if (ivcomp - 100)  20320, 10320, 20320                            
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
!      ****  FCVS PROGRAM 413  -  TEST 033  ****                         
!                                                                        
!                                                                        
!         TEST 033 READS THE RECORDS WRITTEN IN TEST 17.  THIS TEST IS   
!      SIMILAR  TO TEST 32 ABOVE EXCEPT THE FILE IS READ IN REVERSE      
!      RECORD NUMBER ORDER.                                              
!                                                                        
!                                                                        
      ivtnum =  33                                                      
      if (iczero) 30330, 0330, 30330                                    
 0330 continue                                                          
      irecck = 216                                                      
      irecn = 0                                                         
      ivcomp = 0                                                        
      irec = 216                                                        
      do i = 1,100                                                 
      irec = irec - 2                                                   
      irecck = irecck - 2                                               
      read (i10, rec = irec)  iprog, ifile, itotr, irlgn, irecn, ieof,     ivon21, ivon22, ivon31, ivon32, ivon33, ivon34, ivon55, ivon56 
      if (irecn  ==  irecck)           ivcomp = ivcomp + 1              
  end do
      ivcorr = 100                                                      
40330 if (ivcomp - 100)      20330, 10330, 20330                        
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
!      ****  FCVS PROGRAM 413  -  TEST 034  ****                         
!                                                                        
!                                                                        
!         TEST 034 VERIFIES THAT THE VALUES OF A RECORD MAY BE CHANGED   
!      WHEN THE RECORD IS REWRITTEN.  RECORD NUMBER 01 IS USED FOR       
!      TESTING.  THE RECORD WAS WRITTEN IN TEST 02 AND READ IN TEST 18.  
!      A RECORD CANNOT BE DELETED FROM THE FILE BUT IT CAN BE REWRITTEN. 
!                                                                        
!                   SEE SECTION  12.2.4.2 (5), DIRECT ACCESS             
!                                                                        
!                                                                        
      ivtnum =  34                                                      
      if (iczero) 30340, 0340, 30340                                    
 0340 continue                                                          
      irecn = 01                                                        
      write (i10, rec = 01)  iprog, ifile, itotr, irlgn, irecn, ieof,       icon31, icon32, icon21, icon22, icon55, icon56, icon33, icon34
      read (i10, rec=01)  iprog, ifile, itotr, irlgn, irecn, ieof,          ivon61, ivon62, ivon63, ivon64, ivon65,ivon66, ivon67, ivon68 
      ivcorr = 210                                                      
      ivcomp = 1                                                        
      if (irecn  ==  01)             ivcomp = ivcomp * 2                
      if (ivon61  ==  777)           ivcomp = ivcomp  * 3               
      if (ivon62  ==  -777)          ivcomp = ivcomp * 5                
      if (ivon66  ==  32767)         ivcomp = ivcomp * 7                
40340 if (ivcomp - 210)  20340, 10340, 20340                            
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
! DB**  BEGIN FILE DUMP CODE                                             
!      ITOTR = 214                                                       
!      ILUN  = I10                                                       
!      IRLGN = 80                                                        
!      IRNUM = 1                                                         
! 7701 FORMAT (80A1)                                                     
! 7702 FORMAT (1X,80A1)                                                  
! 7703 FORMAT (10X,"FILE ",I2," HAS ",I3," RECORDS - OK" )               
! 7704 FORMAT (10X,"FILE ",I2," HAS ",I3," RECORDS - THERE SHOULD BE " ,I
!     13,9H RECORDS.)                                                    
!      DO 7771 IRNUM = 1, ITOTR                                          
!      READ (ILUN, REC = IRNUM) (IDUMP(ICH), ICH = 1, IRLGN)             
!      WRITE (I02,  7702) (IDUMP(ICH), ICH = 1, IRLGN)                   
! 7771 CONTINUE                                                          
! DE**      END OF DUMP CODE                                             
!         TEST  034 IS THE LAST TEST IN THIS PROGRAM.  THE ROUTINE SHOULD
!      HAVE MADE 34 EXPLICIT TESTS AND PROCESSED ONE FILE CONNECTED  FOR 
!      DIRECT ACCESS                                                     
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
90001 format (" ",24x,"FM413")                                          
90000 format (" ",20x,"END OF PROGRAM FM413" )                          
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
      end program fm413
