      program fm906
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM906                                                          
! *****                       LSTDI2 - (372)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST LIST DIRECTED INPUT                              13.6    
! *****    DOUBLE PRECISION, COMPLEX DATA TYPES INCLUDED         12.4    
! *****                                                                  
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
! *****                                                                  
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 12 CARD IMAGES IN COL. 1-44   
! OL.      1-----------------------------------------44                  
! ARD 1    2.5D0                                                         
! ARD 2    1.5  2.5D0  3.5E0                                             
! ARD 3    (3.0,4.0)                                                     
! ARD 4    (1.0,0.0)  (0.0,0.0)  (0.0,3.0)                               
! ARD 5    2, 2.5D0, 2.5D0, T, (3.0,4.0), 'TEST'                         
! ARD 6    ( 2.5 , 3.5 )                                                 
! ARD 7    (1.0        ,                                                 
! ARD 8       2.0)                                                       
! ARD 9    , (2.0, 3.0),,6.0D0, 2*,                                      
! ARD 10   1.0D0  (2.0, 2.0)  3.0D0  (4.0, 4.0)  5.0D0                   
! ARD 11   6.0D0  (7.0, 7.0) / 8.0D0  (9.0, 9.0) 10.0D0                  
! ARD 12   2.0D0 4.0D0 / 6.0D0 8.0D0 10.0D0                              
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 372                        
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: irvi
      integer :: nuvi
      integer :: ivtnum
      real :: avs
      integer :: ivi
      integer :: ivcorr
      real :: rvcorr
      integer :: ivcomp
      integer :: lvcorr
        logical :: avb
        character(len=4) :: a4vk
        character(len=4) :: cvcorr
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
        double precision :: dvcorr
        double precision, dimension(1:4) :: a1d
        complex :: avc
        complex :: bvc
        complex :: cvc
        complex :: zvcorr
        real, dimension(1:6) :: r2e
        equivalence (avc,r2e(1)),(bvc,r2e(3)),(cvc,r2e(5))              
! *****                                                                  
! BB** ********************** BBCINITA **********************************
! **** SPECIFICATION STATEMENTS                                          
! ****                                                                   
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
! **** INITIALIZE SECTION                                                
! BE** ********************** BBCINITA **********************************
! BB** ********************** BBCINITB **********************************
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
      irvi = i01                                                        
      nuvi = i02                                                        
      ivtotl = 28                                                       
      zprog = 'FM906'                                                   
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
! *****                                                                  
! *****    HEADING FOR SEGMENT 372                                       
        write(nuvi,37200)                                               
37200 format(" ", /" LSTDI2 - (372) " ,                                        " LIST DIRECTED INPUT" ,                                          " FOR D.P. AND COMPLEX DATA TYPES" //                             " ANS REF. - 13.6  12.4" )                               
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! T001*  TEST 1 - CARD 1    DOUBLE PRECISION                             
           ivtnum = 1                                                   
        read(irvi, *) avd                                               
           if (avd - 0.2499999998d+01) 20010, 10010, 40010              
40010 if (avd - 0.2500000002d+01) 10010, 10010, 20010              
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 2.5d0                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0011      continue                                                     
! *****  TESTS 2 THRU 4 - CARD 2    SEVERAL DOUBLE PRECISION             
! T002*  TEST 2                                                          
           ivtnum = 2                                                   
        read(irvi, *) avd, bvd, cvd                                     
           if (avd - 0.1499999999d+01) 20020, 10020, 40020              
40020 if (avd - 0.1500000001d+01) 10020, 10020, 20020              
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = 1.5d0                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                                          
           ivtnum = 3                                                   
           if (bvd - 0.2499999998d+01) 20030, 10030, 40030              
40030 if (bvd - 0.2500000002d+01) 10030, 10030, 20030              
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 2.5d0                                               
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                                          
           ivtnum = 4                                                   
           if (cvd - 0.3499999998d+01) 20040, 10040, 40040              
40040 if (cvd - 0.3500000002d+01) 10040, 10040, 20040              
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 3.5d0                                               
           write (nuvi, 80031) ivtnum, cvd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5 - CARD 3    COMPLEX                                      
           ivtnum = 5                                                   
        read(irvi, *) avc                                               
           if (r2e(1) - 0.29998e+01) 20050, 40052, 40051                
40051 if (r2e(1) - 0.30002e+01) 40052, 40052, 20050                
40052 if (r2e(2) - 0.39998e+01) 20050, 10050, 40050                
40050 if (r2e(2) - 0.40002e+01) 10050, 10050, 20050                
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           zvcorr = (3.0, 4.0)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0051      continue                                                     
! *****  TESTS 6 THRU 8 - CARD 4    SEVERAL COMPLEX                      
! T006*  TEST 6                                                          
           ivtnum = 6                                                   
        read(irvi, *) avc, bvc, cvc                                     
           if (r2e(1) - 0.99995e+00) 20060, 40062, 40061                
40061 if (r2e(1) - 0.10001e+01) 40062, 40062, 20060                
40062 if (r2e(2) + 0.50000e-04) 20060, 10060, 40060                
40060 if (r2e(2) - 0.50000e-04) 10060, 10060, 20060                
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           zvcorr = (1.0, 0.0)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0061      continue                                                     
! T007*  TEST 7                                                          
           ivtnum = 7                                                   
           if (r2e(3) + 0.50000e-04) 20070, 40072, 40071                
40071 if (r2e(3) - 0.50000e-04) 40072, 40072, 20070                
40072 if (r2e(4) + 0.50000e-04) 20070, 10070, 40070                
40070 if (r2e(4) - 0.50000e-04) 10070, 10070, 20070                
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           zvcorr = (0.0, 0.0)                                          
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0071      continue                                                     
! T008*  TEST 8                                                          
           ivtnum = 8                                                   
           if (r2e(5) + 0.50000e-04) 20080, 40082, 40081                
40081 if (r2e(5) - 0.50000e-04) 40082, 40082, 20080                
40082 if (r2e(6) - 0.29998e+01) 20080, 10080, 40080                
40080 if (r2e(6) - 0.30002e+01) 10080, 10080, 20080                
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           zvcorr = (0.0, 3.0)                                          
           write (nuvi, 80045) ivtnum, cvc, zvcorr                      
 0081      continue                                                     
! *****  TESTS 9 THRU 14 - CARD 5    MIXED LIST                          
! T009*  TEST 9                                                          
           ivtnum = 9                                                   
        read(irvi, *) ivi, avd, avs, avb, avc, a4vk                     
           if (ivi - 2) 20090, 10090, 20090                             
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           ivcorr = 2                                                   
           write (nuvi, 80010) ivtnum, ivi, ivcorr                      
 0091      continue                                                     
! T010*  TEST 10                                                         
           ivtnum = 10                                                  
           if (avd - 0.2499999998d+01) 20100, 10100, 40100              
40100 if (avd - 0.2500000002d+01) 10100, 10100, 20100              
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 2.5d0                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0101      continue                                                     
! T011*  TEST 11                                                         
           ivtnum = 11                                                  
           if (avs - 0.24998e+01) 20110, 10110, 40110                   
40110 if (avs - 0.25002e+01) 10110, 10110, 20110                   
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 2.5                                                 
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0111      continue                                                     
! T012*  TEST 12                                                         
           ivtnum = 12                                                  
           ivcomp = 0                                                   
           if (avb) ivcomp = 1                                          
           if (ivcomp - 1) 20120, 10120, 20120                          
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           lvcorr = 1                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivcomp                                   
           write (nuvi, 80026) lvcorr                                   
 0121      continue                                                     
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                        
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! T013*  TEST 13                                                         
           ivtnum = 13                                                  
           if (r2e(1) - 0.29998e+01) 20130, 40132, 40131                
40131 if (r2e(1) - 0.30002e+01) 40132, 40132, 20130                
40132 if (r2e(2) - 0.39998e+01) 20130, 10130, 40130                
40130 if (r2e(2) - 0.40002e+01) 10130, 10130, 20130                
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           zvcorr = (3.0, 4.0)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0131      continue                                                     
! T014*  TEST 14                                                         
           ivtnum = 14                                                  
           ivcomp = 0                                                   
           if (a4vk == 'TEST') ivcomp = 1                               
           if (ivcomp - 1) 20140, 10140, 20140                          
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           cvcorr = 'TEST'                                              
           write (nuvi, 80018) ivtnum, a4vk, cvcorr                     
 0141      continue                                                     
! T015*  TEST 15 - CARD 6    COMPLEX CONSTANT W/EMBEDDED BLANKS          
           ivtnum = 15                                                  
        read(irvi, *) avc                                               
           if (r2e(1) - 0.24998e+01) 20150, 40152, 40151                
40151 if (r2e(1) - 0.25002e+01) 40152, 40152, 20150                
40152 if (r2e(2) - 0.34998e+01) 20150, 10150, 40150                
40150 if (r2e(2) - 0.35002e+01) 10150, 10150, 20150                
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           zvcorr = (2.5, 3.5)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0151      continue                                                     
! T016*  TEST 16 - CARDS 7-8   COMPLEX WITH EMBEDDED END-OF-RECORD       
           ivtnum = 16                                                  
        read(irvi, *) avc                                               
           if (r2e(1) - 0.99995e+00) 20160, 40162, 40161                
40161 if (r2e(1) - 0.10001e+01) 40162, 40162, 20160                
40162 if (r2e(2) - 0.19999e+01) 20160, 10160, 40160                
40160 if (r2e(2) - 0.20001e+01) 10160, 10160, 20160                
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           zvcorr = (1.0, 2.0)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0161      continue                                                     
! *****  TESTS 17 THRU 22 - CARD 9    NULL VALUES                        
! T017*  TEST 17                                                         
           ivtnum = 17                                                  
        avd = 1.0d0                                                     
        bvc = (4.0, 5.0)                                                
        cvc = (7.0, 8.0)                                                
        cvd = 9.0d0                                                     
        read(irvi, *) avd, avc, bvc, bvd, cvc, cvd                      
           if (avd - 0.9999999995d+00) 20170, 10170, 40170              
40170 if (avd - 0.1000000001d+01) 10170, 10170, 20170              
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0171      continue                                                     
! T018*  TEST 18                                                         
           ivtnum = 18                                                  
           if (r2e(1) - 0.19999e+01) 20180, 40182, 40181                
40181 if (r2e(1) - 0.20001e+01) 40182, 40182, 20180                
40182 if (r2e(2) - 0.29998e+01) 20180, 10180, 40180                
40180 if (r2e(2) - 0.30002e+01) 10180, 10180, 20180                
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           zvcorr = (2.0, 3.0)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0181      continue                                                     
! T019*  TEST 19                                                         
           ivtnum = 19                                                  
           if (r2e(3) - 0.39998e+01) 20190, 40192, 40191                
40191 if (r2e(3) - 0.40002e+01) 40192, 40192, 20190                
40192 if (r2e(4) - 0.49997e+01) 20190, 10190, 40190                
40190 if (r2e(4) - 0.50003e+01) 10190, 10190, 20190                
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           zvcorr = (4.0, 5.0)                                          
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0191      continue                                                     
! T020*  TEST 20                                                         
           ivtnum = 20                                                  
           if (bvd - 0.5999999997d+01) 20200, 10200, 40200              
40200 if (bvd - 0.6000000003d+01) 10200, 10200, 20200              
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           dvcorr = 6.0d0                                               
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0201      continue                                                     
! T021*  TEST 21                                                         
           ivtnum = 21                                                  
           if (r2e(5) - 0.69996e+01) 20210, 40212, 40211                
40211 if (r2e(5) - 0.70004e+01) 40212, 40212, 20210                
40212 if (r2e(6) - 0.79996e+01) 20210, 10210, 40210                
40210 if (r2e(6) - 0.80004e+01) 10210, 10210, 20210                
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           zvcorr = (7.0, 8.0)                                          
           write (nuvi, 80045) ivtnum, cvc, zvcorr                      
 0211      continue                                                     
! T022*  TEST 22                                                         
           ivtnum = 22                                                  
           if (cvd - 0.8999999995d+01) 20220, 10220, 40220              
40220 if (cvd - 0.9000000005d+01) 10220, 10220, 20220              
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           dvcorr = 9.0d0                                               
           write (nuvi, 80031) ivtnum, cvd, dvcorr                      
 0221      continue                                                     
! *****  TESTS 23 THRU 27 - CARDS 10-11    SLASH TERMINATOR              
! T023*  TEST 23                                                         
           ivtnum = 23                                                  
        read(irvi, *) avd, avc, bvd, bvc, cvd                           
        read(irvi, *) avd, avc, bvd, bvc, cvd                           
           if (avd - 0.5999999997d+01) 20230, 10230, 40230              
40230 if (avd - 0.6000000003d+01) 10230, 10230, 20230              
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           dvcorr = 6.0d0                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0231      continue                                                     
! T024*  TEST 24                                                         
           ivtnum = 24                                                  
           if (r2e(1) - 0.69996e+01) 20240, 40242, 40241                
40241 if (r2e(1) - 0.70004e+01) 40242, 40242, 20240                
40242 if (r2e(2) - 0.69996e+01) 20240, 10240, 40240                
40240 if (r2e(2) - 0.70004e+01) 10240, 10240, 20240                
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           zvcorr = (7.0, 7.0)                                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0241      continue                                                     
! T025*  TEST 25                                                         
           ivtnum = 25                                                  
           if (bvd - 0.2999999998d+01) 20250, 10250, 40250              
40250 if (bvd - 0.3000000002d+01) 10250, 10250, 20250              
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           dvcorr = 3.0d0                                               
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0251      continue                                                     
! T026*  TEST 26                                                         
           ivtnum = 26                                                  
           if (r2e(3) - 0.39998e+01) 20260, 40262, 40261                
40261 if (r2e(3) - 0.40002e+01) 40262, 40262, 20260                
40262 if (r2e(4) - 0.39998e+01) 20260, 10260, 40260                
40260 if (r2e(4) - 0.40002e+01) 10260, 10260, 20260                
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           zvcorr = (4.0, 4.0)                                          
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0261      continue                                                     
! T027*  TEST 27                                                         
           ivtnum = 27                                                  
           if (cvd - 0.4999999997d+01) 20270, 10270, 40270              
40270 if (cvd - 0.5000000003d+01) 10270, 10270, 20270              
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           dvcorr = 5.0d0                                               
           write (nuvi, 80031) ivtnum, cvd, dvcorr                      
 0271      continue                                                     
! T028*  TEST 28                                                         
           ivtnum = 28                                                  
        a1d(3) = 3.0d0                                                  
        read(irvi, *) (a1d(ivi), ivi=1,4)                               
           if (a1d(3) - 0.2999999998d+01) 20280, 10280, 40280           
40280 if (a1d(3) - 0.3000000002d+01) 10280, 10280, 20280           
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           dvcorr = 3.0d0                                               
           write (nuvi, 80031) ivtnum, a1d(3), dvcorr                   
 0281      continue                                                     
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
! *****                                                                  
! *****    END OF TEST SEGMENT 372                                       
        stop                                                            
        end program fm906
