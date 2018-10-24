      program fm520
!                                                                        
!      TESTING PARAMETER STATEMENT                                       
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
      real :: rvcomp
      real :: rvcorr
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
      integer, parameter :: ipn001=5+5
      integer, parameter :: ipn002=8-3
      integer, parameter :: ipn003=1*5
! BE** ********************** BBCINITA **********************************
!                                                                        
      real, parameter :: rpn001=5.1+4.9
      real, parameter :: rpn002=8.7-3.7
      real, parameter :: rpn003=2.0*2.5
!                                                                        
!      TEST 1 - 7 TEST INTEGER ARITHMETIC EXPRESSION USING               
!                 ONLY SYMBOLIC NAMES OF ARITHMETIC CONSTANTS            
!                 S06AF-2P 4.A                                           
!                                                                        
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
           zprog='FM520'                                                
           ivtotl =  30                                                 
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
! T001*  TEST 001   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   1                                                 
        ivcomp=+ipn001                                                  
           ivcorr=+10                                                   
40010 if (ivcomp - 10) 20010, 10010, 20010                         
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0021      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   2                                                 
        ivcomp=-ipn001                                                  
           ivcorr=-10                                                   
40020 if (ivcomp + 10) 20020, 10020, 20020                         
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0031      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   3                                                 
        ivcomp=ipn001+ipn002                                            
           ivcorr=15                                                    
40030 if (ivcomp - 15) 20030, 10030, 20030                         
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   4                                                 
        ivcomp=ipn001-ipn002                                            
           ivcorr=5                                                     
40040 if (ivcomp - 5) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0051      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   5                                                 
        ivcomp=ipn001*ipn002                                            
           ivcorr=50                                                    
40050 if (ivcomp - 50) 20050, 10050, 20050                         
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0061      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   6                                                 
        ivcomp=ipn001/ipn002                                            
           ivcorr=2                                                     
40060 if (ivcomp - 2) 20060, 10060, 20060                          
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0071      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   7                                                 
        ivcomp=ipn001**ipn002                                           
           ivcorr=100000                                                
40070 if (ivcomp - 100000) 20070, 10070, 20070                     
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0081      continue                                                     
!                                                                        
!                                                                        
!      TEST 8 - 14 TEST REAL ARITHMETIC EXPRESSION USING                 
!                  ONLY SYMBOLIC NAMES OF ARITHMETIC CONSTANTS           
!                  S06AF-2P 4.A                                          
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   8                                                 
        rvcomp=+rpn001                                                  
           rvcorr=+10.0                                                 
           if (rvcomp - 0.99995e+01) 20080, 10080, 40080                
40080 if (rvcomp - 0.10001e+02) 10080, 10080, 20080                
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0091      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =   9                                                 
        rvcomp=-rpn001                                                  
           rvcorr=-10.0                                                 
           if (rvcomp + 0.10001e+02) 20090, 10090, 40090                
40090 if (rvcomp + 0.99995e+01) 10090, 10090, 20090                
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0101      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  10                                                 
        rvcomp=rpn001+rpn002                                            
           rvcorr=15.0                                                  
           if (rvcomp - 0.14999e+02) 20100, 10100, 40100                
40100 if (rvcomp - 0.15001e+02) 10100, 10100, 20100                
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0111      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =  11                                                 
        rvcomp=rpn001-rpn002                                            
           rvcorr=5.0                                                   
           if (rvcomp - 0.49997e+01) 20110, 10110, 40110                
40110 if (rvcomp - 0.50003e+01) 10110, 10110, 20110                
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0121      continue                                                     
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =  12                                                 
        rvcomp=rpn001*rpn002                                            
           rvcorr=50.0                                                  
           if (rvcomp - 0.49997e+02) 20120, 10120, 40120                
40120 if (rvcomp - 0.50003e+02) 10120, 10120, 20120                
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0131                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0131      continue                                                     
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  13                                                 
        rvcomp=rpn001/rpn002                                            
           rvcorr=2.0                                                   
           if (rvcomp - 0.19999e+01) 20130, 10130, 40130                
40130 if (rvcomp - 0.20001e+01) 10130, 10130, 20130                
10130 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0141                                                   
20130 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0141      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  14                                                 
         rvcomp=rpn001**rpn002                                          
           rvcorr=100000.0                                              
           if (rvcomp - 0.99995e+05) 20140, 10140, 40140                
40140 if (rvcomp - 0.10001e+06) 10140, 10140, 20140                
10140 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0151                                                   
20140 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0151      continue                                                     
!                                                                        
!                                                                        
!      TEST 15 - 18 REPEATS TEST 1 - 7 USING MORE THAN ONE OPERATOR      
!                   S06AF-2P 4.C                                         
!                                                                        
! T015*  TEST 015   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =  15                                                 
        ivcomp=ipn001+ipn001-ipn002                                     
           ivcorr=15                                                    
40150 if (ivcomp - 15) 20150, 10150, 20150                         
10150 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0161                                                   
20150 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0161      continue                                                     
!                                                                        
! T016*  TEST 016   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  16                                                 
        ivcomp=ipn001+ipn001-ipn002*ipn002                              
           ivcorr=-5                                                    
40160 if (ivcomp + 5) 20160, 10160, 20160                          
10160 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0171                                                   
20160 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0171      continue                                                     
!                                                                        
! T017*  TEST 017   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =  17                                                 
        ivcomp=ipn001+ipn001-ipn002*ipn002/ipn003                       
           ivcorr=15                                                    
40170 if (ivcomp - 15) 20170, 10170, 20170                         
10170 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0181                                                   
20170 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0181      continue                                                     
!                                                                        
! T018*  TEST 018   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  18                                                 
        ivcomp=ipn001+ipn001**ipn002-ipn002*ipn002/ipn003               
           ivcorr=100005                                                
40180 if (ivcomp - 100005) 20180, 10180, 20180                     
10180 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0191                                                   
20180 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0191      continue                                                     
!                                                                        
!                                                                        
!      TEST 19 - 22 REPEATS TEST 8 - 14 USING MORE THAN ONE OPERATOR     
!                   S06AF-2P 4.C                                         
!                                                                        
! T019*  TEST 019   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  19                                                 
        rvcomp=rpn001+rpn001-rpn002                                     
           rvcorr=15.0                                                  
           if (rvcomp - 0.14999e+02) 20190, 10190, 40190                
40190 if (rvcomp - 0.15001e+02) 10190, 10190, 20190                
10190 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0201                                                   
20190 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0201      continue                                                     
!                                                                        
! T020*  TEST 020   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  20                                                 
        rvcomp=rpn001+rpn001-rpn002*rpn002                              
           rvcorr=-5.0                                                  
           if (rvcomp + 0.50003e+01) 20200, 10200, 40200                
40200 if (rvcomp + 0.49997e+01) 10200, 10200, 20200                
10200 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0211                                                   
20200 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0211      continue                                                     
!                                                                        
! T021*  TEST 021   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  21                                                 
        rvcomp=rpn001+rpn001-rpn002*rpn002/rpn003                       
           rvcorr=15.0                                                  
           if (rvcomp - 0.14999e+02) 20210, 10210, 40210                
40210 if (rvcomp - 0.15001e+02) 10210, 10210, 20210                
10210 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0221                                                   
20210 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0221      continue                                                     
!                                                                        
! T022*  TEST 022   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  22                                                 
        rvcomp=rpn001+rpn001**rpn002-rpn002*rpn002/rpn003               
           rvcorr=100005.0                                              
           if (rvcomp - 0.10000e+06) 20220, 10220, 40220                
40220 if (rvcomp - 0.10001e+06) 10220, 10220, 20220                
10220 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0231                                                   
20220 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0231      continue                                                     
!                                                                        
!                                                                        
!      TEST 23 - 26 REPEATS TEST 15 - 18 USING PARENTHESES               
!                   S06AF-2P 4.D                                         
!                                                                        
!                                                                        
!                                                                        
! T023*  TEST 023   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  23                                                 
        ivcomp=ipn001+(ipn001-ipn002)                                   
           ivcorr=15                                                    
40230 if (ivcomp - 15) 20230, 10230, 20230                         
10230 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0241                                                   
20230 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0241      continue                                                     
!                                                                        
! T024*  TEST 024   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  24                                                 
        ivcomp=((ipn001+ipn001)-ipn002)*ipn002                          
           ivcorr=75                                                    
40240 if (ivcomp - 75) 20240, 10240, 20240                         
10240 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0251                                                   
20240 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0251      continue                                                     
!                                                                        
! T025*  TEST 025   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  25                                                 
        ivcomp=(ipn001+ipn001)-ipn002*(ipn002/ipn003)                   
           ivcorr=15                                                    
40250 if (ivcomp - 15) 20250, 10250, 20250                         
10250 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0261                                                   
20250 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0261      continue                                                     
!                                                                        
! T026*  TEST 026   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  26                                                 
        ivcomp=(ipn001+ipn001)**2-((ipn002*ipn002)/ipn003)              
           ivcorr=395                                                   
40260 if (ivcomp - 395) 20260, 10260, 20260                        
10260 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0271                                                   
20260 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0271      continue                                                     
!                                                                        
!      TEST 27 - 30 REPEATS TEST 19 - 22 USING PARENTHESES               
!                   S06AF-2P 4.D                                         
!                                                                        
! T027*  TEST 027   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  27                                                 
        rvcomp=rpn001+(rpn001-rpn002)                                   
           rvcorr=15.0                                                  
           if (rvcomp - 0.14999e+02) 20270, 10270, 40270                
40270 if (rvcomp - 0.15001e+02) 10270, 10270, 20270                
10270 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0281                                                   
20270 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0281      continue                                                     
!                                                                        
! T028*  TEST 028   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  28                                                 
        rvcomp=((rpn001+rpn001)-rpn002)*rpn002                          
           rvcorr=75.0                                                  
           if (rvcomp - 0.74996e+02) 20280, 10280, 40280                
40280 if (rvcomp - 0.75004e+02) 10280, 10280, 20280                
10280 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0291                                                   
20280 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0291      continue                                                     
!                                                                        
! T029*  TEST 029   ****  FCVS PROGRAM 520  ****                         
!                                                                        
           ivtnum =  29                                                 
        rvcomp=(rpn001+rpn001)-rpn002*(rpn002/rpn003)                   
           rvcorr=15.0                                                  
           if (rvcomp - 0.14999e+02) 20290, 10290, 40290                
40290 if (rvcomp - 0.15001e+02) 10290, 10290, 20290                
10290 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0301                                                   
20290 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0301      continue                                                     
!                                                                        
! T030*  TEST 030   ****  FCVS PROGRAM 520  ****                         
!                                                                        
!                                                                        
           ivtnum =  30                                                 
        rvcomp=(rpn001+rpn001)**3.0-((rpn002*rpn002)/rpn003)            
           rvcorr=7995.0                                                
           if (rvcomp - 0.79946e+04) 20300, 10300, 40300                
40300 if (rvcomp - 0.79954e+04) 10300, 10300, 20300                
10300 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0311                                                   
20300 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0311      continue                                                     
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
90001 format (" ",56x,"FM520")                                          
90000 format (" ",50x,"END OF PROGRAM FM520" )                          
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
           end program fm520
