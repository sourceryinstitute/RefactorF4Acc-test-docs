      program fm700
!                                                                        
!      THIS PROGRAM TESTS THE DATA STATEMENT WITH           ANS REF.     
!           VARIABLE NAMES, ARRAY NAMES, ARRAY ELEMENT      9.1          
!           NAMES, SUBSTRING NAMES, AND IMPLIED-DO LISTS.   9.2          
!                                                           9.3          
!      SYMBOLIC NAMES OF CONSTANTS ARE PERMITTED IN THE                  
!           CLIST OF THE DATA STATEMENT.   IF NECESSARY,                 
!           THE CLIST CONSTANT IS CONVERTED TO THE TYPE                  
!           OF THE NLIST ENTITY ACCORDING TO THE RULES                   
!           FOR ARITHMETIC CONVERSION.                                   
!                                                                        
!                                                                        
! BB** ********************** BBCCOMNT **********************************
! ****                                                                   
! ****            1978 FORTRAN COMPILER VALIDATION SYSTEM                
! ****                          VERSION 2.1                              
! ****                                                                   
! ****                                                                   
! ****           SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO         
! ****          NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
! ****               SOFTWARE STANDARDS VALIDATION GROUP                 
! ****                      BUILDING 225  RM A266                        
! ****                     GAITHERSBURG, MD  20899                       
! ****                                                                   
! ****                                                                   
! ****                                                                   
! BE** ********************** BBCCOMNT **********************************
! BB** ********************** BBCINITA **********************************
! **** SPECIFICATION STATEMENTS                                          
! ****                                                                   
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivn001
      integer :: ivn002
      integer :: i
      integer :: j
      real :: go
      real :: to
      integer :: ivn003
      integer :: ivn004
      real :: rvcomp
      real :: rvcorr
      real :: rvn001
      doubleprecision :: dvn002
      character(len=13) :: zvers
      character(len=17) :: zversd
      character(len=17) :: zdate
      character(len=5) :: zprog
      character(len=20) :: zcompl
      character(len=20) :: zname
      character(len=10) :: ztape
      character(len=13) :: zproj
      character(len=31) :: remrks
      character(len=13) :: ztaped
      integer, dimension(1:2,1:3) :: i2n001
      integer, dimension(1:7) :: i2n002
      integer, dimension(1:3,1:7) :: i2n003
! BE** ********************** BBCINITA **********************************
!                                                                        
      integer, dimension(1:3,1:10) :: i2n004
      integer, dimension(1:4,1:5) :: i2n005
      integer, dimension(1:6,1:8) :: i2n006
      character(len=25) :: cvcomp
      character(len=25) :: cvcorr
      character(len=25) :: cvn001
      character(len=5) :: cvn002
      character(len=5), dimension(1:3) :: c1n001
      character(len=4), dimension(1:3,1:4) :: c2n001
      character(len=17) :: cvn003
      real, dimension(1:2) :: r2e001
      real, dimension(1:5,1:3) :: r2n001
      double precision :: dvcomp
      double precision :: dvcorr
      double precision :: dvn001
      double precision, dimension(1:9) :: d1n001
      complex :: zvcomp
      complex :: zvcorr
      complex :: zvn001
      complex, dimension(1:10) :: z1n001
      integer, parameter :: ipn001=-14
      character(len=5), parameter :: cpn001='SEVEN'
      integer, parameter :: ipn002=5
      double precision, parameter :: dpn001=0.1948d+3
      equivalence (zvcomp, r2e001)                                      
      data ivn001,c1n001,i2n001(2,1),cvn001(11:22) / -137,'FIRST','SECND','THIRD',65,'ELEVENTWELVE' / 
      data (i2n001(1,i), i=1,3) /-47, 198, -217/                        
      data ivn002,cvn002 / ipn001,cpn001 / 
      data i2n002, (i2n003(i,7), i=1,3), c2n001, cvn003(13:16)               /3*19, 7*-4, 13*'SAME'/                                      
      data ivn003,ivn004,rvn001,zvn001,dvn001,dvn002 / -0.473e+3,239.2d-1,71,(71,-27),6,9.1534e-2 / 
      data (i2n004(2,j), j=1,10) /9,8,7,6,5,4,3,2,1,0/                  
      data ((r2n001(i,j), j=1,3), i=3,5)                                     /3.1, 3.2, 3.3, 4.1, 4.2, 4.3, 5.1, 5.2, 5.3/                
      data (z1n001(i), i=3,7) /ipn002*(7.3, -2.28)/                     
      data (d1n001(i), i=1,9,2) /ipn002*dpn001/                         
      data (i2n005(i,i+1),i=1,4) / 91, -82, 73, -64/                    
      data ((i2n006(2*i,i*j-1), i=2,3), j=1,3,2) /41, 62, 45, 68/       
!                                                                        
!                                                                        
! BB** ********************** BBCINITB **********************************
! **** INITIALIZE SECTION                                                
      data zvers,zversd,zdate / 'VERSION 2.1  ','93/10/21*21.02.00','*NO DATE*TIME' / 
      data zcompl,zname,ztape / '*NONE SPECIFIED*','*NO COMPANY NAME*','*NO TAPE*' / 
      data zproj,ztaped,zprog / '*NO PROJECT*','*NO TAPE DATE','XXXXX' / 
      data remrks / '                               ' / 
! **** THE FOLLOWING 9 COMMENT LINES (CZ01, CZ02, ...) CAN BE REPLACED   
! **** FOR IDENTIFYING THE TEST ENVIRONMENT                              
! ****                                                                   
! Z01  ZVERS  = 'VERSION OF THE COMPILER VALIDATION SYSTEM'              
! Z02  ZVERSD = 'CREATION DATE/TIME OF THE COMPILER VALIDATION SYSTEM'   
! Z03  ZPROG  = 'PROGRAM NAME'                                           
! Z04  ZDATE  = 'DATE OF TEST'                                           
! Z05  ZCOMPL = 'COMPILER IDENTIFICATION'                                
! Z06  ZPROJ  = 'PROJECT NUMBER/IDENTIFICATION'                          
! Z07  ZNAME  = 'NAME OF USER'                                           
! Z08  ZTAPE  = 'TAPE OWNER/ID'                                          
! Z09  ZTAPED = 'DATE TAPE COPIED'                                       
!                                                                        
      ivpass = 0                                                        
      ivfail = 0                                                        
      ivdele = 0                                                        
      ivinsp = 0                                                        
      ivtotl = 0                                                        
      ivtotn = 0                                                        
      iczero = 0                                                        
!                                                                        
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         
      i01 = 05                                                          
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             
      i02 = 06                                                          
!                                                                        
! X010   REPLACED BY FEXEC X-010 CONTROL CARD (CARD-READER UNIT NUMBER). 
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
! X011   REPLACED BY FEXEC X-011 CONTROL CARD.  CX011 IS FOR SYSTEMS     
!      REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX010.  
!                                                                        
! X020   REPLACED BY FEXEC X-020 CONTROL CARD (PRINTER UNIT NUMBER).     
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02= 6       
! X021   REPLACED BY FEXEC X-021 CONTROL CARD.  CX021 IS FOR SYSTEMS     
!      REQUIRING ADDITIONAL STATEMENTS FOR FILES ASSOCIATED WITH CX020.  
!                                                                        
! BE** ********************** BBCINITB **********************************
           zprog = 'FM700'                                              
           ivtotl = 23                                                  
! BB** ********************** BBCHED0A **********************************
! ****                                                                   
! **** WRITE REPORT TITLE                                                
! ****                                                                   
      write (i02, 90002)                                                
      write (i02, 90006)                                                
      write (i02, 90007)                                                
      write (i02, 90008)  zvers, zversd                                 
      write (i02, 90009)  zprog, zprog                                  
      write (i02, 90010)  zdate, zcompl                                 
! BE** ********************** BBCHED0A **********************************
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
!                                                                        
!                                                                        
!           TESTS 1 THRU 5 TEST DATA STATEMENT WITH VARIABLE NAMES,      
!      ARRAY NAMES, ARRAY ELEMENT NAMES, SUBSTRING NAMES, AND IMPLIED-   
!      DO LISTS.                                                         
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 700  *****                        
!      VARIABLE NAME                                                     
!                                                                        
           ivtnum = 1                                                   
           ivcomp = 0                                                   
           ivcorr = -137                                                
      ivcomp = ivn001                                                   
40010 if (ivcomp + 137) 20010, 10010, 20010                        
10010 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 700  *****                        
!      ARRAY NAME                                                        
!                                                                        
           ivtnum = 2                                                   
           cvcomp = ' '                                                 
           cvcorr = 'SECND'                                             
      cvcomp = c1n001(2)                                                
           ivcomp = 0                                                   
           if (cvcomp == 'SECND') ivcomp = 1                            
40020 if (ivcomp - 1) 20020, 10020, 20020                          
10020 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02, 80018) ivtnum, cvcomp, cvcorr                    
 0021      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 700  *****                        
!      ARRAY ELEMENT NAME                                                
!                                                                        
           ivtnum = 3                                                   
           ivcomp = 0                                                   
           ivcorr = 65                                                  
      ivcomp = i2n001(2,1)                                              
40030 if (ivcomp - 65) 20030, 10030, 20030                         
10030 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0031      continue                                                     
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 700  *****                        
!      SUBSTRING NAME                                                    
!                                                                        
           ivtnum = 4                                                   
           cvcomp = ' '                                                 
           cvcorr = 'ELEVENTWELVE'                                      
      cvcomp = cvn001(11:22)                                            
           ivcomp = 0                                                   
           if (cvcomp == 'ELEVENTWELVE') ivcomp = 1                     
40040 if (ivcomp - 1) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02, 80018) ivtnum, cvcomp, cvcorr                    
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 700  *****                        
!      IMPLIED-DO LIST                                                   
!                                                                        
           ivtnum = 5                                                   
           ivcomp = 0                                                   
           ivcorr = -217                                                
      ivcomp = i2n001(1,3)                                              
40050 if (ivcomp + 217) 20050, 10050, 20050                        
10050 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0051      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 700  *****                        
!      CLIST CONTAINS A SYMBOLIC NAME OF AN INTEGER CONSTANT             
!                                                                        
           ivtnum = 6                                                   
           ivcomp = 0                                                   
           ivcorr = -14                                                 
      ivcomp = ivn002                                                   
40060 if (ivcomp + 14) 20060, 10060, 20060                         
10060 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 700  *****                        
!      CLIST CONTAINS A SYMBOLIC NAME OF A CHARACTER CONSTANT            
!                                                                        
           ivtnum = 7                                                   
           cvcomp = ' '                                                 
           cvcorr = 'SEVEN'                                             
      cvcomp = cvn002                                                   
           ivcomp = 0                                                   
           if (cvcomp == 'SEVEN') ivcomp = 1                            
40070 if (ivcomp - 1) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02, 80018) ivtnum, cvcomp, cvcorr                    
 0071      continue                                                     
!                                                                        
!           TESTS 8 THRU 11 TEST COMBINATIONS OF SUBSTRING NAMES AND     
!      ARRAY NAMES AND THE R*C FORMAT OF THE CLIST                       
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 700  *****                        
!                                                                        
           ivtnum = 8                                                   
           ivcomp = 0                                                   
           ivcorr = 23                                                  
      ivcomp = i2n002(3) - i2n002(4)                                    
40080 if (ivcomp - 23) 20080, 10080, 20080                         
10080 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 700  *****                        
!                                                                        
           ivtnum = 9                                                   
           ivcomp = 0                                                   
           ivcorr = -4                                                  
      do i = 1, 3                                                  
      if (i2n003(i,7) + 4) 0093, 0092, 0093                             
      0092 end do
0093  ivcomp = i2n003(3,7)                                              
40090 if (ivcomp + 4) 20090, 10090, 20090                          
10090 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0091      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 700  *****                        
!                                                                        
           ivtnum = 10                                                  
           cvcomp = ' '                                                 
           cvcorr = 'SAME'                                              
      do i = 1, 3                                                  
      do j = 1, 4                                                  
      if (c2n001(i,j) /= 'SAME') goto 0103                               !Break
       end do
      end do
0103  cvcomp = c2n001(3,4)                                              
           ivcomp = 0                                                   
           if (cvcomp == 'SAME') ivcomp = 1                             
40100 if (ivcomp - 1) 20100, 10100, 20100                          
10100 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02, 80018) ivtnum, cvcomp, cvcorr                    
 0101      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 700  *****                        
!                                                                        
           ivtnum = 11                                                  
           cvcomp = ' '                                                 
           cvcorr = 'SAME'                                              
      cvcomp = cvn003(13:16)                                            
           ivcomp = 0                                                   
           if (cvcomp == 'SAME') ivcomp = 1                             
40110 if (ivcomp - 1) 20110, 10110, 20110                          
10110 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02, 80018) ivtnum, cvcomp, cvcorr                    
 0111      continue                                                     
!                                                                        
!           TESTS 12 THRU 17 TEST ARITHMETIC CONVERSION OF CLIST         
!      CONSTANTS TO THE TYPE OF THE CORRESPONDING NLIST ENTITIES         
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 700  *****                        
!      REAL TO INTEGER                                                   
!                                                                        
           ivtnum = 12                                                  
           ivcomp = 0                                                   
           ivcorr =  -473                                               
      ivcomp = ivn003                                                   
40120 if (ivcomp + 473) 20120, 10120, 20120                        
10120 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0121      continue                                                     
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 700  *****                        
!      DOUBLE PRECISION TO INTEGER                                       
!                                                                        
           ivtnum = 13                                                  
           ivcomp = 0                                                   
           ivcorr = 23                                                  
      ivcomp = ivn004                                                   
40130 if (ivcomp - 23) 20130, 10130, 20130                         
10130 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0131      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 700  *****                        
!      INTEGER TO REAL                                                   
!                                                                        
           ivtnum = 14                                                  
           rvcomp = 0.0                                                 
           rvcorr = 71.0                                                
      rvcomp = rvn001                                                   
           if (rvcomp - 0.70996e+02) 20140, 10140, 40140                
40140 if (rvcomp - 0.71004e+02) 10140, 10140, 20140                
10140 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           write (i02, 80012) ivtnum, rvcomp, rvcorr                    
 0141      continue                                                     
!                                                                        
! T015*  TEST 015   ****  FCVS PROGRAM 700  *****                        
!      COMPLEX                                                           
!                                                                        
           ivtnum = 15                                                  
           zvcomp = (0.0, 0.0)                                          
           zvcorr = (71.0, -27.0)                                       
      zvcomp = zvn001                                                   
           if (r2e001(1) - 0.70996e+02) 20150, 40152, 40151             
40151 if (r2e001(1) - 0.71004e+02) 40152, 40152, 20150             
40152 if (r2e001(2) + 0.27002e+02) 20150, 10150, 40150             
40150 if (r2e001(2) + 0.26998e+02) 10150, 10150, 20150             
10150 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           write (i02, 80045) ivtnum, zvcomp, zvcorr                    
 0151      continue                                                     
!                                                                        
! T016*  TEST 016   ****  FCVS PROGRAM 700  *****                        
!      INTEGER TO DOUBLE PRECISION                                       
!                                                                        
           ivtnum = 16                                                  
           dvcomp = 0.0d0                                               
           dvcorr = 6.0d0                                               
      dvcomp = dvn001                                                   
           if (dvcomp - 0.5999999997d+01) 20160, 10160, 40160           
40160 if (dvcomp - 0.6000000003d+01) 10160, 10160, 20160           
10160 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           write (i02, 80031) ivtnum, dvcomp, dvcorr                    
 0161      continue                                                     
!                                                                        
! T017*  TEST 017   ****  FCVS PROGRAM 700  *****                        
!      REAL TO DOUBLE PRECISION                                          
!                                                                        
           ivtnum = 17                                                  
           dvcomp = 0.0d0                                               
           dvcorr = 9.1534d-2                                           
      dvcomp = dvn002                                                   
           if (dvcomp - 0.91529d-01) 20170, 10170, 40170                
40170 if (dvcomp - 0.91539d-01) 10170, 10170, 20170                
10170 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           write (i02, 80031) ivtnum, dvcomp, dvcorr                    
 0171      continue                                                     
!                                                                        
!      TESTS 18 THRU 21 TEST DIFFERENT DATA TYPES USING THE IMPLIED-DO   
!                                                                        
! T018*  TEST 018   ****  FCVS PROGRAM 700  *****                        
!      INTEGER                                                           
!                                                                        
           ivtnum = 18                                                  
           ivcomp = 0                                                   
           ivcorr = 3                                                   
      ivcomp = i2n004(2,7)                                              
40180 if (ivcomp - 3) 20180, 10180, 20180                          
10180 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0181      continue                                                     
!                                                                        
! T019*  TEST 019   ****  FCVS PROGRAM 700  *****                        
!      REAL                                                              
!                                                                        
           ivtnum = 19                                                  
           rvcomp = 0.0                                                 
           rvcorr = 4.1                                                 
      rvcomp = r2n001(4,1)                                              
           if (rvcomp - 0.40998e+01) 20190, 10190, 40190                
40190 if (rvcomp - 0.41002e+01) 10190, 10190, 20190                
10190 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           write (i02, 80012) ivtnum, rvcomp, rvcorr                    
 0191      continue                                                     
!                                                                        
! T020*  TEST 020   ****  FCVS PROGRAM 700  *****                        
!      COMPLEX                                                           
!                                                                        
           ivtnum = 20                                                  
           zvcomp = (0.0, 0.0)                                          
           zvcorr = (7.3, -2.28)                                        
      zvcomp = z1n001(7)                                                
           if (r2e001(1) - 0.72996e+01) 20200, 40202, 40201             
40201 if (r2e001(1) - 0.73004e+01) 40202, 40202, 20200             
40202 if (r2e001(2) + 0.22802e+01) 20200, 10200, 40200             
40200 if (r2e001(2) + 0.22798e+01) 10200, 10200, 20200             
10200 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           write (i02, 80045) ivtnum, zvcomp, zvcorr                    
 0201      continue                                                     
!                                                                        
! T021*  TEST 021   ****  FCVS PROGRAM 700  *****                        
!      DOUBLE PRECISION                                                  
!                                                                        
           ivtnum = 21                                                  
           dvcomp = 0.0d0                                               
           dvcorr = 0.1948d+3                                           
      dvcomp = d1n001(9)                                                
           if (dvcomp - 0.1947999999d+03) 20210, 10210, 40210           
40210 if (dvcomp - 0.1948000001d+03) 10210, 10210, 20210           
10210 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           write (i02, 80031) ivtnum, dvcomp, dvcorr                    
 0211      continue                                                     
!                                                                        
!           TESTS 22 AND 23 TEST THAT EACH SUBSCRIPT EXPRESSION          
!      IN AN IMPLIED-DO LIST MAY CONTAIN IMPLIED-DO-VARIABLES OF         
!      THE LIST THAT HAS THE SUBSCRIPT EXPRESSION WITHIN ITS RANGE.      
!                                                                        
! T022*  TEST 022   ****  FCVS PROGRAM 700  *****                        
!                                                                        
           ivtnum = 22                                                  
           ivcomp = 0                                                   
           ivcorr = 155                                                 
      ivcomp = i2n005(3,4) - i2n005(2,3)                                
40220 if (ivcomp - 155) 20220, 10220, 20220                        
10220 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0221      continue                                                     
!                                                                        
! T023*  TEST 023   ****  FCVS PROGRAM 700  *****                        
!                                                                        
           ivtnum = 23                                                  
           ivcomp = 0                                                   
           ivcorr = 130                                                 
      ivcomp = i2n006(6,2) + i2n006(6,8)                                
40230 if (ivcomp - 130) 20230, 10230, 20230                        
10230 ivpass = ivpass + 1                                          
           write (i02, 80002) ivtnum                                    
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           write (i02, 80010) ivtnum, ivcomp, ivcorr                    
 0231      continue                                                     
!                                                                        
! BB** ********************** BBCSUM0  **********************************
! **** WRITE OUT TEST SUMMARY                                            
! ****                                                                   
      ivtotn = ivpass + ivfail + ivdele + ivinsp                        
      write (i02, 90004)                                                
      write (i02, 90014)                                                
      write (i02, 90004)                                                
      write (i02, 90020) ivpass                                         
      write (i02, 90022) ivfail                                         
      write (i02, 90024) ivdele                                         
      write (i02, 90026) ivinsp                                         
      write (i02, 90028) ivtotn, ivtotl                                 
! BE** ********************** BBCSUM0  **********************************
! BB** ********************** BBCFOOT0 **********************************
! **** WRITE OUT REPORT FOOTINGS                                         
! ****                                                                   
      write (i02,90016) zprog, zprog                                    
      write (i02,90018) zproj, zname, ztape, ztaped                     
      write (i02,90019)                                                 
! BE** ********************** BBCFOOT0 **********************************
90001 format (" ",56x,"FM700")                                          
90000 format (" ",50x,"END OF PROGRAM FM700" )                          
! BB** ********************** BBCFMT0A **********************************
! **** FORMATS FOR TEST DETAIL LINES                                     
! ****                                                                   
80000 format (" ",2x,i3,4x,"DELETED",32x,a31)                           
80002 format (" ",2x,i3,4x," PASS  ",32x,a31)                           
80004 format (" ",2x,i3,4x,"INSPECT",32x,a31)                           
80008 format (" ",2x,i3,4x," FAIL  ",32x,a31)                           
80010 format (" ",2x,i3,4x," FAIL  ",/," ",15x,"COMPUTED= " ,           i6,/," ",15x,"CORRECT=  " ,i6)                                    
80012 format (" ",2x,i3,4x," FAIL  ",/," ",16x,"COMPUTED= " ,           e12.5,/," ",16x,"CORRECT=  " ,e12.5)                              
80018 format (" ",2x,i3,4x," FAIL  ",/," ",16x,"COMPUTED= " ,           a21,/," ",16x,"CORRECT=  " ,a21)                                  
80020 format (" ",16x,"COMPUTED= " ,a21,1x,a31)                         
80022 format (" ",16x,"CORRECT=  " ,a21,1x,a31)                         
80024 format (" ",16x,"COMPUTED= " ,i6,16x,a31)                         
80026 format (" ",16x,"CORRECT=  " ,i6,16x,a31)                         
80028 format (" ",16x,"COMPUTED= " ,e12.5,10x,a31)                      
80030 format (" ",16x,"CORRECT=  " ,e12.5,10x,a31)                      
80050 format (" ",48x,a31)                                              
! BE** ********************** BBCFMT0A **********************************
! BB** ********************** BBCFMAT1 **********************************
! **** FORMATS FOR TEST DETAIL LINES - FULL LANGUAGE                     
! ****                                                                   
80031 format (" ",2x,i3,4x," FAIL  ",/," ",16x,"COMPUTED= " ,           d17.10,/," ",16x,"CORRECT=  " ,d17.10)                            
80033 format (" ",16x,"COMPUTED= " ,d17.10,10x,a31)                     
80035 format (" ",16x,"CORRECT=  " ,d17.10,10x,a31)                     
80037 format (" ",16x,"COMPUTED= " ,"(",e12.5,", ",e12.5,")",6x,a31)    
80039 format (" ",16x,"CORRECT=  " ,"(",e12.5,", ",e12.5,")",6x,a31)    
80041 format (" ",16x,"COMPUTED= " ,"(",f12.5,", ",f12.5,")",6x,a31)    
80043 format (" ",16x,"CORRECT=  " ,"(",f12.5,", ",f12.5,")",6x,a31)    
80045 format (" ",2x,i3,4x," FAIL  ",/," ",16x,"COMPUTED= " ,           "(",f12.5,", ",f12.5,")"/," ",16x,"CORRECT=  " ,                  "(",f12.5,", ",f12.5,")")                                         
! BE** ********************** BBCFMAT1 **********************************
! BB** ********************** BBCFMT0B **********************************
! **** FORMAT STATEMENTS FOR PAGE HEADERS                                
! ****                                                                   
90002 format ("1")                                                      
90004 format (" ")                                                      
90006 format (" ",20x,"NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY" )
90007 format (" ",19x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90008 format (" ",21x,a13,a17)                                          
90009 format (" ",/," *",a5,"BEGIN*",12x,"TEST RESULTS - " ,a5,/)       
90010 format (" ",8x,"TEST DATE*TIME= " ,a17,"  -  COMPILER= " ,a20)    
90013 format (" "," TEST   ","PASS/FAIL " ,6x,"DISPLAYED RESULTS" ,            7x,"REMARKS",24x)                                          
90014 format (" ","----------------------------------------------" ,            "---------------------------------" )                     
90015 format (" ",48x,"THIS PROGRAM HAS " ,i3," TESTS",/)               
! ****                                                                   
! **** FORMAT STATEMENTS FOR REPORT FOOTINGS                             
! ****                                                                   
90016 format (" ",/," *",a5,"END*",14x,"END OF TEST - " ,a5,/)          
90018 format (" ",a13,13x,a20,"   *   ",a10,"/",                                a13)                                                      
90019 format (" ","FOR OFFICIAL USE ONLY     " ,35x,"COPYRIGHT  1982" ) 
! ****                                                                   
! **** FORMAT STATEMENTS FOR RUN SUMMARY                                 
! ****                                                                   
90020 format (" ",21x,i5," TESTS PASSED" )                              
90022 format (" ",21x,i5," TESTS FAILED" )                              
90024 format (" ",21x,i5," TESTS DELETED" )                             
90026 format (" ",21x,i5," TESTS REQUIRE INSPECTION" )                  
90028 format (" ",21x,i5," OF ",i3," TESTS EXECUTED" )                  
! BE** ********************** BBCFMT0B **********************************
      stop                                                              
      end program fm700
