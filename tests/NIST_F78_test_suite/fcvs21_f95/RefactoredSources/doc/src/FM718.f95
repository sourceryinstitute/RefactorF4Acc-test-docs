      program fm718
!                                                                        
!      THIS ROUTINE TESTS LOGICAL EXPRESSIONS AND            ANS REF.    
!      USE OF THE LOGICAL OPERATORS .NOT., .AND., .OR.,      6.4, 6.4.2, 
!      .EQV., AND .NEQV.                                     6.4.3, 6.4.4
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
      real :: false
      real :: true
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
! BE** ********************** BBCINITA **********************************
!                                                                        
      logical :: lvcomp
      logical :: lvcorr
      logical :: lvn001
      logical, parameter :: lpn001=.true.
      logical, parameter :: lpn002=.false.
      logical, parameter :: lpn003=.true.
      logical, parameter :: lpn004=.false.
      data zvers,zversd,zdate / 'VERSION 2.1  ','93/10/21*21.02.00','*NO DATE*TIME' / 
      data zcompl,zname,ztape / '*NONE SPECIFIED*','*NO COMPANY NAME*','*NO TAPE*' / 
      data zproj,ztaped,zprog / '*NO PROJECT*','*NO TAPE DATE','XXXXX' / 
      data remrks / '                               ' / 
!                                                                        
!                                                                        
! BB** ********************** BBCINITB **********************************
! **** INITIALIZE SECTION                                                
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
           zprog='FM718'                                                
           ivtotl =  29                                                 
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
! T001*  TEST 001   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!      LOGICAL EXPRESSION CONTAINING SYMBOLIC NAME OF A LOGICAL CONSTANT 
!                                                                        
           ivtnum =   1                                                 
           lvcorr = .true.                                              
      lvcomp = lpn001                                                   
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20010, 10010, 20010                          
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0011      continue                                                     
!                                                                        
!      TESTS 2-3 - TEST LOGICAL EXPRESSIONS INVOLVING .NOT.              
!                                                                        
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   2                                                 
           lvcorr = .true.                                              
      lvcomp = .not..false.                                             
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20020, 10020, 20020                          
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0021      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   3                                                 
           ivcorr = 1                                                   
      ivcomp = 0                                                        
      if (.not. lpn002) ivcomp = 1                                      
40030 if (ivcomp - 1) 20030, 10030, 20030                          
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0031      continue                                                     
!                                                                        
!      TESTS 4-5 - TEST LOGICAL EXPRESSIONS INVOLVING .AND.              
!                                                                        
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   4                                                 
           lvcorr = .true.                                              
      lvcomp = .true..and.lpn003                                        
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   5                                                 
           ivcorr = 1                                                   
      ivcomp = 0                                                        
      if (lpn003.and..true.) ivcomp = 1                                 
40050 if (ivcomp - 1) 20050, 10050, 20050                          
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0051      continue                                                     
!                                                                        
!      TESTS 6-7 - TEST LOGICAL EXPRESSIONS INVOLVING .OR.               
!                                                                        
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   6                                                 
           lvcorr = .true.                                              
      lvcomp = .true..or.lpn004                                         
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20060, 10060, 20060                          
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   7                                                 
           ivcorr = 1                                                   
      ivcomp = 0                                                        
      if (lpn001.or..false.) ivcomp = 1                                 
40070 if (ivcomp - 1) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0071      continue                                                     
!                                                                        
!      TESTS 8-9 - TEST LOGICAL EXPRESSIONS INVOLVING .EQV.              
!                                                                        
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   8                                                 
           lvcorr = .true.                                              
      lvcomp = .false..eqv.lpn002                                       
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20080, 10080, 20080                          
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =   9                                                 
           ivcorr = 1                                                   
      ivcomp = 0                                                        
      if (lpn003.eqv..true.) ivcomp = 1                                 
40090 if (ivcomp - 1) 20090, 10090, 20090                          
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0091      continue                                                     
!                                                                        
!      TESTS 10-11 - TEST LOGICAL EXPRESSIONS INVOLVING .NEQV.           
!                                                                        
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  10                                                 
           lvcorr = .true.                                              
      lvcomp = .false..neqv.lpn001                                      
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20100, 10100, 20100                          
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0101      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  11                                                 
           ivcorr = 1                                                   
      ivcomp = 0                                                        
      if (lpn003.neqv..false.) ivcomp = 1                               
40110 if (ivcomp - 1) 20110, 10110, 20110                          
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0111      continue                                                     
!                                                                        
!      TESTS 12-17 - TEST LOGICAL EXPRESSIONS INVOLVING VARIOUS COMBINA- 
!      TIONS OF LOGICAL OPERATORS AND ALSO TEST PRECEDENCE AMONG THE     
!      LOGICAL OPERATORS WITH OR WITHOUT PARENTHESES                     
!                                                                        
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  12                                                 
           lvcorr = .true.                                              
      lvn001 = .true.                                                   
      lvcomp = lvn001.eqv.lpn002.and..true..neqv.lpn003                 
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20120, 10120, 20120                          
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0121      continue                                                     
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  13                                                 
           lvcorr = .false.                                             
      lvcomp = (.true..eqv..false.).and.(lvn001.neqv.lpn003)            
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20130, 10130, 20130                          
10130 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0131      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  14                                                 
           lvcorr = .true.                                              
      lvn001 = .false.                                                  
      lvcomp = lvn001.eqv.lpn002.and..not.lpn001.or..false.             
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20140, 10140, 20140                          
10140 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0141      continue                                                     
!                                                                        
! T015*  TEST 015   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  15                                                 
           lvcorr = .false.                                             
      lvcomp = (lvn001.eqv.lpn002).and.(.not.lpn001.or..false.)         
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20150, 10150, 20150                          
10150 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0151      continue                                                     
!                                                                        
! T016*  TEST 016   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  16                                                 
           lvcorr = .true.                                              
      lvcomp = lpn001.eqv.lvn001.or..not.lpn003.neqv..true.             
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20160, 10160, 20160                          
10160 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0161      continue                                                     
!                                                                        
! T017*  TEST 017   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  17                                                 
           lvcorr = .true.                                              
      lvcomp = lpn001.and.(lvn001.or..not.(lpn002.eqv.(lpn003.neqv.              lpn004)))                                                
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20170, 10170, 20170                          
10170 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0171      continue                                                     
!                                                                        
!     TESTS 18-21 - TEST LOGICAL EXPRESSIONS INVOLOVING .EQV.            
!                                                                        
!                                                                        
! T018*  TEST 018   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  18                                                 
           lvcorr = .true.                                              
      lvcomp = lpn001.eqv.lpn003                                        
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20180, 10180, 20180                          
10180 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0181      continue                                                     
!                                                                        
! T019*  TEST 019   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  19                                                 
           lvcorr = .false.                                             
      lvcomp = lpn001.eqv.lpn002                                        
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20190, 10190, 20190                          
10190 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0191      continue                                                     
!                                                                        
! T020*  TEST 020   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  20                                                 
           lvcorr = .false.                                             
      lvcomp = lpn002.eqv.lpn003                                        
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20200, 10200, 20200                          
10200 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0201      continue                                                     
!                                                                        
! T021*  TEST 021   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  21                                                 
           lvcorr = .true.                                              
      lvcomp = lpn002.eqv.lpn004                                        
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20210, 10210, 20210                          
10210 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0211      continue                                                     
!                                                                        
!     TESTS 22-25 - TEST LOGICAL EXPRESSIONS INVOLVING .NEQV.            
!                                                                        
!                                                                        
! T022*  TEST 022   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  22                                                 
           lvcorr = .false.                                             
      lvcomp = lpn001.neqv.lpn003                                       
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20220, 10220, 20220                          
10220 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0221      continue                                                     
!                                                                        
! T023*  TEST 023   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  23                                                 
           lvcorr = .true.                                              
      lvcomp = lpn001.neqv.lpn002                                       
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20230, 10230, 20230                          
10230 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0231      continue                                                     
!                                                                        
! T024*  TEST 024   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  24                                                 
           lvcorr = .true.                                              
      lvcomp = lpn002.neqv.lpn003                                       
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20240, 10240, 20240                          
10240 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0241      continue                                                     
!                                                                        
! T025*  TEST 025   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  25                                                 
           lvcorr = .false.                                             
      lvcomp = lpn002.neqv.lpn004                                       
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20250, 10250, 20250                          
10250 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0251      continue                                                     
!                                                                        
!     TESTS 26-29 TEST LOGICAL CONSTANT EXPRESSIONS USING SYMBOLIC NAMES 
!     OF LOGICAL CONSTANTS                                               
!                                                                        
!                                                                        
! T026*  TEST 026   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  26                                                 
           lvcorr = .false.                                             
      lvcomp = lpn001.eqv.lpn002.neqv.lpn004                            
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20260, 10260, 20260                          
10260 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0261      continue                                                     
!                                                                        
! T027*  TEST 027   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  27                                                 
           lvcorr = .true.                                              
      lvcomp = lpn003.neqv.lpn001.and.lpn002                            
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20270, 10270, 20270                          
10270 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0271      continue                                                     
!                                                                        
! T028*  TEST 028   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  28                                                 
           lvcorr = .false.                                             
      lvcomp = (lpn003.neqv.lpn001).and.lpn002                          
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 0) 20280, 10280, 20280                          
10280 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0281      continue                                                     
!                                                                        
! T029*  TEST 029   ****  FCVS PROGRAM 718  ****                         
!                                                                        
!                                                                        
           ivtnum =  29                                                 
           lvcorr = .true.                                              
      lvcomp = .not.(lpn002.eqv.lpn004.and.lpn001.or.lpn003)            
           ivcomp = 0                                                   
           if (lvcomp) ivcomp = 1                                       
           if (ivcomp - 1) 20290, 10290, 20290                          
10290 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0291      continue                                                     
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
90001 format (" ",56x,"FM718")                                          
90000 format (" ",50x,"END OF PROGRAM FM718" )                          
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
           end program fm718
