      program fm701
!                                                                        
!      THIS ROUTINE TESTS ARRAY DECLARATORS WHERE DIMENSION      ANS REF.
!                   BOUND EXPRESSIONS MAY CONTAIN CONSTANTS,     5.1.1.2 
!                   SYMBOLIC NAMES OF CONSTANTS, OR VARIABLES    5.1.1   
!                   OF TYPE INTEGER.                                     
!                                                                        
!      THIS ROUTINE USES ROUTINES 602 THROUGH 609 AS SUBROUTINES.        
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
      integer :: ivcorr
      integer :: ivcomp
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
      integer, dimension(1:3,1:5) :: i2d001
      integer, dimension(1:2,1:4) :: i2d002
      integer, dimension(1:5,1:2) :: i2d003
! BE** ********************** BBCINITA **********************************
!                                                                        
      integer, parameter :: ipn001=1
      integer, parameter :: ipn002=-1
      integer, parameter :: ipn003=4
      integer, dimension(ipn001:2,1:3) :: i2n004
      integer, dimension(1:2,-1:ipn001) :: i2n005
      integer, dimension(ipn002:ipn001,1:ipn003) :: i2n006
      integer, dimension(+5:7,+1:2) :: i2n007
      integer, dimension(0:2,1:2) :: i2n008
      integer, dimension(1:3,-1:1) :: i2n009
      integer, dimension(1:4,1:2) :: i2n010
      integer, dimension(2*2+1:7,1:2) :: i2n011
      integer, dimension(1:+2,2:+4) :: i2n012
      integer, dimension(-2:0,1:2) :: i2n013
      integer, dimension(1:3,-3:-1) :: i2d014
      integer, dimension(1:2*2+1,1:2) :: i2n015
      integer, dimension(1:2,6/3-1:2*5-7) :: i2n016
      character(len=4) :: cvcomp
      character(len=4) :: cvcorr
      character(len=4), dimension(0:5,1:6) :: c2n001
      character(len=4), dimension(1:2,1:3) :: c2d002
      character(len=4), dimension(-2:1,3:10) :: c2n003
      character(len=4), dimension(1:2,5:7) :: c2d004
      character(len=4), dimension(1:6) :: c1n005
      character(len=4), dimension(1:2,1:2,5:7) :: c3d006
      data i2d001 / 12*0,-47,2*0 / 
      data i2d002 / 6*0,5,0 / 
      data i2d003 / 6,8*0,-11 / 
      data i2n004 / -4,5*4 / 
      data i2n005 / -5,5*5 / 
      data i2n006 / 6*6,-6,5*6 / 
      data i2n007 / 3*7,-7,2*7 / 
      data i2n008 / -8,5*8 / 
      data i2n009 / 2*9,-9,6*9 / 
      data i2n010 / -10,7*10 / 
      data i2n011 / 3*11,-11,2*11 / 
      data i2n012 / 7,5*-7 / 
      data i2n013 / 8,5*-8 / 
      data i2d014 / 9,8*-9 / 
      data i2n015 / 9*-10,10 / 
      data i2n016 / 11,4*-11,-10 / 
      data c2n001 / 'C001',35*'    ' / 
      data c2d002 / 5*'    ','C002' / 
      data c2n003 / 'C003',31*'    ' / 
      data c2d004 / 'C004',5*'    ' / 
      data c1n005 / 'C005',5*'    ' / 
      data c3d006 / 'C006',11*'    ' / 
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
           zprog='FM701'                                                
           ivtotl =  35                                                 
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
!      TESTS 1-3 - LOWER AND/OR UPPER BOUNDS ARE ARITHMETIC EXPRESSIONS  
!                  OF TYPE INTEGER, USING VARIABLES                      
!                                                                        
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 001 LOWER BOUND                                              
!                                                                        
           ivtnum =   1                                                 
           ivcorr = -47                                                 
      call sn702(1,1,2,6,i2d001,i2d002,i2d003,ivcomp)

40010 if (ivcomp + 47) 20010, 10010, 20010                         
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 002 UPPER BOUND                                              
!                                                                        
           ivtnum =   2                                                 
           ivcorr = 5                                                   
      call sn702(2,1,2,6,i2d001,i2d002,i2d003,ivcomp)

40020 if (ivcomp - 5) 20020, 10020, 20020                          
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0021      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 003 BOTH LOWER AND UPPER BOUNDS                              
!                                                                        
           ivtnum =   3                                                 
           ivcorr = 17                                                  
      call sn702(3,1,2,6,i2d001,i2d002,i2d003,ivcomp)

40030 if (ivcomp - 17) 20030, 10030, 20030                         
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0031      continue                                                     
!                                                                        
!      TESTS 4-6 - LOWER AND/OR UPPER BOUNDS ARE SYMBOLIC NAMES          
!                  OF INTEGER CONSTANTS                                  
!                                                                        
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 004 LOWER BOUND                                              
!                                                                        
           ivtnum =   4                                                 
           ivcomp = 0                                                   
           ivcorr = -4                                                  
      ivcomp = i2n004(1,1)                                              
40040 if (ivcomp + 4) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 005 UPPER BOUND                                              
!                                                                        
           ivtnum =   5                                                 
           ivcomp = 0                                                   
           ivcorr = -5                                                  
      ivcomp = i2n005(1,-1)                                             
40050 if (ivcomp + 5) 20050, 10050, 20050                          
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0051      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 006 BOTH UPPER AND LOWER BOUNDS                              
!                                                                        
           ivtnum =   6                                                 
           ivcomp = 0                                                   
           ivcorr = -6                                                  
      ivcomp = i2n006(-1,3)                                             
40060 if (ivcomp + 6) 20060, 10060, 20060                          
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 007 LOWER BOUND POSITIVE                                     
!                                                                        
           ivtnum =   7                                                 
           ivcomp = 0                                                   
           ivcorr = -7                                                  
      ivcomp = i2n007(5,2)                                              
40070 if (ivcomp + 7) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0071      continue                                                     
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 008 LOWER BOUND ZERO                                         
!                                                                        
           ivtnum =   8                                                 
           ivcomp = 0                                                   
           ivcorr = -8                                                  
      ivcomp = i2n008(0,1)                                              
40080 if (ivcomp + 8) 20080, 10080, 20080                          
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 009 LOWER BOUND NEGATIVE                                     
!                                                                        
           ivtnum =   9                                                 
           ivcomp = 0                                                   
           ivcorr = -9                                                  
      ivcomp = i2n009(3,-1)                                             
40090 if (ivcomp + 9) 20090, 10090, 20090                          
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0091      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 010 LOWER BOUND OMITTED                                      
!                                                                        
           ivtnum =  10                                                 
           ivcomp = 0                                                   
           ivcorr = -10                                                 
      ivcomp = i2n010(1,1)                                              
40100 if (ivcomp + 10) 20100, 10100, 20100                         
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0101      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 011 LOWER BOUND IS AN INTEGER EXPRESSION                     
!                                                                        
           ivtnum =  11                                                 
           ivcomp = 0                                                   
           ivcorr = -11                                                 
      ivcomp = i2n011(5,2)                                              
40110 if (ivcomp + 11) 20110, 10110, 20110                         
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0111      continue                                                     
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 012 UPPER BOUND POSITIVE                                     
!                                                                        
           ivtnum =  12                                                 
           ivcomp = 0                                                   
           ivcorr = 7                                                   
      ivcomp = i2n012(1,2)                                              
40120 if (ivcomp - 7) 20120, 10120, 20120                          
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0121      continue                                                     
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 013 UPPER BOUND ZERO                                         
!                                                                        
           ivtnum =  13                                                 
           ivcomp = 0                                                   
           ivcorr = 8                                                   
      ivcomp = i2n013(-2,1)                                             
40130 if (ivcomp - 8) 20130, 10130, 20130                          
10130 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0131      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 014 UPPER BOUND NEGATIVE                                     
!                                                                        
           ivtnum =  14                                                 
           ivcomp = 0                                                   
           ivcorr = 9                                                   
      ivcomp = i2d014(1,-3)                                             
40140 if (ivcomp - 9) 20140, 10140, 20140                          
10140 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0141      continue                                                     
!                                                                        
! T015*  TEST 015   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 015 UPPER BOUND IS INTEGER EXPRESSION                        
!                                                                        
           ivtnum =  15                                                 
           ivcomp = 0                                                   
           ivcorr = 10                                                  
      ivcomp = i2n015(5,2)                                              
40150 if (ivcomp - 10) 20150, 10150, 20150                         
10150 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0151      continue                                                     
!                                                                        
! T016*  TEST 016   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 016 UPPER BOUNDS ARE INTEGER EXPRESSIONS                     
!                                                                        
           ivtnum =  16                                                 
           ivcomp = 0                                                   
           ivcorr = -110                                                
      ivcomp = i2n016(1,1)*i2n016(2,3)                                  
40160 if (ivcomp + 110) 20160, 10160, 20160                        
10160 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0161      continue                                                     
!                                                                        
! T017*  TEST 017   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 017 ZERO AS A DIMENSION                                      
!                                                                        
           ivtnum =  17                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'C001'                                              
      cvcomp = c2n001(0,1)                                              
           if (cvcomp  ==  'C001') ivcomp = 1                           
           if (ivcomp - 1) 20170, 10170, 20170                          
10170 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0171      continue                                                     
!                                                                        
! T018*  TEST 018   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 018 UPPER DIMENSION UNDEFINED IN THE SUBROUTINE              
!                                                                        
           ivtnum =  18                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'C002'                                              
      call sn703(1,1,2,c2d002,c2d004,cvcomp)

           if (cvcomp  ==  'C002') ivcomp = 1                           
           if (ivcomp - 1) 20180, 10180, 20180                          
10180 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0181      continue                                                     
!                                                                        
! T019*  TEST 019   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 019 NEGATIVE DIMENSION                                       
!                                                                        
           ivtnum =  19                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'C003'                                              
      cvcomp = c2n003(-2,3)                                             
           if (cvcomp  ==  'C003') ivcomp = 1                           
           if (ivcomp - 1) 20190, 10190, 20190                          
10190 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0191      continue                                                     
!                                                                        
! T020*  TEST 020   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 020 VARIABLE DIMENSION                                       
!                                                                        
           ivtnum =  20                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'C004'                                              
      call sn703(2,1,2,c2d002,c2d004,cvcomp)

           if (cvcomp  ==  'C004') ivcomp = 1                           
           if (ivcomp - 1) 20200, 10200, 20200                          
10200 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0201      continue                                                     
!                                                                        
! T021*  TEST 021   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 021 POSITIVE DIMENSION                                       
!                                                                        
           ivtnum =  21                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'C005'                                              
      cvcomp = c1n005(1)                                                
           if (cvcomp  ==  'C005') ivcomp = 1                           
           if (ivcomp - 1) 20210, 10210, 20210                          
10210 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0211      continue                                                     
!                                                                        
!      TESTS 22-25 - MIXED DIMENSION BOUNDS WITH VARIABLE NUMBER OF      
!                    ELEMENTS IN EACH DIMENSION                          
!                                                                        
!                                                                        
! T022*  TEST 022   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  22                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'C006'                                              
      call sn704(1,1,2,5,c3d006,cvcomp)

           if (cvcomp  ==  'C006') ivcomp = 1                           
           if (ivcomp - 1) 20220, 10220, 20220                          
10220 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0221      continue                                                     
!                                                                        
! T023*  TEST 023   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  23                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'IJKL'                                              
      call sn704(2,1,2,6,c3d006,cvcomp)

           if (cvcomp  ==  'IJKL') ivcomp = 1                           
           if (ivcomp - 1) 20230, 10230, 20230                          
10230 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0231      continue                                                     
!                                                                        
! T024*  TEST 024   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  24                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'EFGH'                                              
      call sn704(3,1,1,5,c3d006,cvcomp)

           if (cvcomp  ==  'EFGH') ivcomp = 1                           
           if (ivcomp - 1) 20240, 10240, 20240                          
10240 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0241      continue                                                     
!                                                                        
! T025*  TEST 025   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  25                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'ABCD'                                              
      call sn704(4,2,2,6,c3d006,cvcomp)

           if (cvcomp  ==  'ABCD') ivcomp = 1                           
           if (ivcomp - 1) 20250, 10250, 20250                          
10250 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0251      continue                                                     
!                                                                        
!      TESTS 26-28 - LOWER BOUND IS AN EXPRESSION INVOLVING              
!                    ARITHMETIC OPERATORS                                
!                                                                        
!                                                                        
! T026*  TEST 026   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  26                                                 
           ivcorr = -47                                                 
      call sn705(1,2,-1,1,i2d001,i2d002,i2d003,ivcomp)

40260 if (ivcomp + 47) 20260, 10260, 20260                         
10260 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0261      continue                                                     
!                                                                        
! T027*  TEST 027   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  27                                                 
           ivcorr = 5                                                   
      call sn705(2,2,-1,1,i2d001,i2d002,i2d003,ivcomp)

40270 if (ivcomp - 5) 20270, 10270, 20270                          
10270 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0271      continue                                                     
!                                                                        
! T028*  TEST 028   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  28                                                 
           ivcorr = 17                                                  
      call sn705(3,2,-1,1,i2d001,i2d002,i2d003,ivcomp)

40280 if (ivcomp - 17) 20280, 10280, 20280                         
10280 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0281      continue                                                     
!                                                                        
!      TESTS 29-31 - UPPER BOUND IS AN EXPRESSION INVOLVING              
!                    ARITHMETIC OPERATORS                                
!                                                                        
!                                                                        
! T029*  TEST 029   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  29                                                 
           ivcorr = -47                                                 
      call sn706(1,4,0,3,i2d001,i2d002,i2d003,ivcomp)

40290 if (ivcomp + 47) 20290, 10290, 20290                         
10290 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0291      continue                                                     
!                                                                        
! T030*  TEST 030   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  30                                                 
           ivcorr = 5                                                   
      call sn706(2,4,0,3,i2d001,i2d002,i2d003,ivcomp)

40300 if (ivcomp - 5) 20300, 10300, 20300                          
10300 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0301      continue                                                     
!                                                                        
! T031*  TEST 031   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  31                                                 
           ivcorr = 17                                                  
      call sn706(3,4,0,3,i2d001,i2d002,i2d003,ivcomp)

40310 if (ivcomp - 17) 20310, 10310, 20310                         
10310 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0311      continue                                                     
!                                                                        
! T032*  TEST 032   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 032 "/" IN LOWER BOUND                                       
!                                                                        
           ivtnum =  32                                                 
           ivcorr = -47                                                 
      call sn707(1,3,2,i2d001,i2d002,ivcomp)

40320 if (ivcomp + 47) 20320, 10320, 20320                         
10320 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0321      continue                                                     
!                                                                        
! T033*  TEST 033   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!      TEST 033 "**" IN UPPER BOUND                                      
!                                                                        
           ivtnum =  33                                                 
           ivcorr = 5                                                   
      call sn707(2,3,2,i2d001,i2d002,ivcomp)

40330 if (ivcomp - 5) 20330, 10330, 20330                          
10330 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0331      continue                                                     
!                                                                        
!      TESTS 34-35 - UPPER AND LOWER BOUNDS WITH ARITHMETIC OPERATORS    
!                    IN EXPRESSION                                       
!                                                                        
!                                                                        
! T034*  TEST 034   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  34                                                 
           ivcorr = -47                                                 
      call sn708(3,-2,2,i2d001,ivcomp)

40340 if (ivcomp + 47) 20340, 10340, 20340                         
10340 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0341      continue                                                     
!                                                                        
! T035*  TEST 035   ****  FCVS PROGRAM 701  ****                         
!                                                                        
!                                                                        
           ivtnum =  35                                                 
           ivcorr = 9                                                   
      call sn709(-1,-2,1,i2d014,ivcomp)

40350 if (ivcomp - 9) 20350, 10350, 20350                          
10350 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0351      continue                                                     
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
90001 format (" ",56x,"FM701")                                          
90000 format (" ",50x,"END OF PROGRAM FM701" )                          
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
           end program fm701
      subroutine sn702(ivd001,ivd002,ivd003,ivd004,i2d001,i2d002,i2d003,ivd005)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd004
      integer :: ivd005
      integer, dimension(ivd002:3,1:5) :: i2d001
      integer, dimension(1:2,1:2*ivd003) :: i2d002
      integer, dimension(ivd004/3-1:ivd002+4,1:2) :: i2d003
!                                                                        
      if (ivd001 - 2) 70010, 70020, 70030                               
70010 ivd005 = i2d001(1,5)                                              
      return                                                            
70020 ivd005 = i2d002(1,4)                                              
      return                                                            
70030 ivd005 = i2d003(1,1) - i2d003(5,2)                                
      return                                                            
      end subroutine sn702
      subroutine sn703(ivd001,ivd002,ivd003,c2d001,c2d002,cvd001)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      character(len=4) :: cvd001
      character(len=4), dimension(1:2,1:*) :: c2d001
      character(len=4), dimension(ivd002:ivd003,5:7) :: c2d002
!                                                                        
      if (ivd001 - 1) 70010, 70010, 70020                               
70010 cvd001 = c2d001(2,3)                                              
      return                                                            
70020 cvd001 = c2d002(1,5)                                              
      return                                                            
      end subroutine sn703
      subroutine sn704(ivd001,ivd002,ivd003,ivd004,c3d001,cvd001)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd004
      character(len=4) :: cvd001
      character(len=4), dimension(ivd002:ivd003,1:2,ivd004:7) :: c3d001
!                                                                        
      if (ivd001 - 2) 70010, 70020, 70030                               
70010 cvd001 = c3d001(1,1,5)                                            
      return                                                            
70020 c3d001(1,2,6) = 'IJKL'                                            
      cvd001 = c3d001(1,2,6)                                            
      return                                                            
70030 if (ivd001 - 3) 70040, 70040, 70050                               
70040 c3d001(1,1,5) = 'EFGH'                                            
      cvd001 = c3d001(1,1,5)                                            
      return                                                            
70050 c3d001(2,2,6) = 'ABCD'                                            
      cvd001 = c3d001(2,2,6)                                            
      return                                                            
      end subroutine sn704
      subroutine sn705(ivd001,ivd002,ivd003,ivd004,i2d001,i2d002,i2d003,ivd005)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd004
      integer :: ivd005
      integer, dimension(ivd002-1:3,1:5) :: i2d001
      integer, dimension(ivd003+2:2,1:4) :: i2d002
      integer, dimension(2*ivd004-1:5,1:2) :: i2d003
!                                                                        
      if (ivd001 - 2) 70010, 70020, 70030                               
70010 ivd005 = i2d001(1,5)                                              
      return                                                            
70020 ivd005 = i2d002(1,4)                                              
      return                                                            
70030 ivd005 = i2d003(1,1) - i2d003(5,2)                                
      return                                                            
      end subroutine sn705
      subroutine sn706(ivd001,ivd002,ivd003,ivd004,i2d001,i2d002,i2d003,ivd005)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd004
      integer :: ivd005
      integer, dimension(1:ivd002-1,1:5) :: i2d001
      integer, dimension(1:ivd003+2,1:4) :: i2d002
      integer, dimension(1:2*ivd004-1,1:2) :: i2d003
!                                                                        
      if (ivd001 - 2) 70010, 70020, 70030                               
70010 ivd005 = i2d001(1,5)                                              
      return                                                            
70020 ivd005 = i2d002(1,4)                                              
      return                                                            
70030 ivd005 = i2d003(1,1) - i2d003(5,2)                                
      return                                                            
      end subroutine sn706
      subroutine sn707(ivd001,ivd002,ivd003,i2d001,i2d002,ivd004)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd004
      integer, dimension(ivd002/3:3,1:5) :: i2d001
      integer, dimension(1:2,1:ivd003**2) :: i2d002
!                                                                        
      if (ivd001 - 1) 70010, 70010, 70020                               
70010 ivd004 = i2d001(1,5)                                              
      return                                                            
70020 ivd004 = i2d002(1,4)                                              
      return                                                            
      end subroutine sn707
      subroutine sn708(ivd001,ivd002,ivd003,i2d001,ivd004)
!                                                                        
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd004
      integer, dimension(ivd001/3:ivd001,ivd002+3:4*(2*ivd003-1)/3+1) :: i2d001
!                                                                        
      ivd004 = i2d001(1,5)                                              
      return                                                            
      end subroutine sn708
      subroutine sn709(ivd001,ivd002,ivd003,i2d001,ivd005)
!                                                                        
      integer, parameter :: ipn001=-3
      integer :: ivd001
      integer :: ivd002
      integer :: ivd003
      integer :: ivd005
      integer, dimension(ipn001+4:(2*ivd003+1),ipn001:(1-ivd001)/ivd002) :: i2d001
!                                                                        
      ivd005 = i2d001(1,-3)                                             
      return                                                            
      end subroutine sn709
