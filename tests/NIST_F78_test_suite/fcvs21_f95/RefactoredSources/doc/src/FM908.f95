      program fm908
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM908                                                          
! *****                       INTER3 - (392)                             
! *****                                                                  
! ***********************************************************************
! *****  TESTING OF INTERNAL FILES -                           ANS. REF  
! *****          USING READ                                      12.2.5  
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
! *****  S P E C I F I C A T I O N S  SEGMENT 392                        
! *****                                                                  
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: nuvi
      integer :: ivtnum
      real :: avs
      real :: bvs
      integer :: kvi
      integer :: ivcorr
      real :: rvcorr
      integer :: ivcomp
      integer :: lvcorr
      integer :: jvi
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
        double precision :: dvd
        double precision :: evd
        double precision :: dvcorr
        logical :: avb
        character(len=43) :: a43vk
        character(len=43) :: d43vk
        character(len=43) :: f43vk
        character(len=43) :: g43vk
        character(len=43) :: k43vk
        character(len=43) :: n43vk
        character(len=8) :: a8vk
        character(len=51) :: e51vk
        character(len=53) :: l53vk
        character(len=82) :: i82vk
        character(len=97) :: j97vk
        character(len=43), dimension(1:2) :: c431k
        character(len=30) :: cvcorr
        character(len=29), dimension(1:5) :: b291k
        character(len=29), dimension(1:5) :: m291k
        character(len=13), dimension(1:2) :: h131k
        complex :: avc
        complex :: bvc
        complex :: cvc
        complex :: dvc
        complex :: zvcorr
        real, dimension(1:8) :: r2e
        equivalence (r2e(1),avc),(r2e(3),bvc),(r2e(5),cvc),(r2e(7),dvc) 
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
      nuvi = i02                                                        
      ivtotl = 54                                                       
      zprog = 'FM908'                                                   
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
! *****    HEADER FOR SEGMENT 392                                        
! *****                                                                  
        write(nuvi,39200)                                               
39200 format(" ",/ " INTER3 - (392) INTERNAL FILES -- USING READ"                  //" ANS. REF. - 12.2.5" )                            
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
      a43vk = ' 2.1000000D1 23.45600D3      23.450000000D2'             
      d43vk = '34.58673D2 3458.67300 34.58673D2 3458.673  '             
      f43vk = 'T   10.98THISISIT  3.4945D2  3             '             
      g43vk = '   2.343   34.394                      '                 
      k43vk = '  0.934, 34.567   34.65        0.63540D1   '             
      n43vk = '34 34.98395.83000D2 F.FALSE.13.45300E+2    '             
      e51vk = ' 348  3.4783E1384.3847D1    T      3.48570 KDFJ D/.'     
      l53vk = '   0.345 ,3.4345E01,F, 34.85900D-1,  10.000012345678'    
      i82vk = '  2.34 ,  2.456     2.34 ,  2.456     0.234E01,  2.456E00   0.234E+001, 2.456E-000'                                        
      j97vk = '   5.67980,   0.9876       5.67980,    0.9876   05.6798E00, 9.8760E-1  5.67980E0000,0.09876E+001'                          
      b291k(1) =  '34.38457D1 34.38457D1       '                        
      b291k(2) = '34.38457D1                   '                        
      b291k(3) = '34.38457D1 34.38457D1        '                        
      b291k(4) = '                             '                        
      b291k(5) = '34.38457D1                   '                        
      m291k(1) = '   98                        '                        
      m291k(2) = '8.40485D02                   '                        
      m291k(3) = '                             '                        
      m291k(4) = ' .TRUE. 340.435E-1,  3.494E+1'                        
      m291k(5) = '87654321                     '                        
      c431k(1) = ' 2.1000000D1 23.45600D3      23.450000000D2'          
      c431k(2) = '                                           '          
      h131k(1) = '34.84'                                                
      h131k(2) = '349.887'                                              
! T001*  TEST 1                          DOUBLE PRECISION FROM VARIABLE  
           ivtnum = 1                                                   
        read(unit=a43vk,fmt=39201) avd                                  
39201 format(13x,d10.5)                                               
           if (avd - 0.2345599998d+05) 20010, 10010, 40010              
40010 if (avd - 0.2345600002d+05) 10010, 10010, 20010              
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 23.456d3                                            
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                          FROM ELEMENT    
           ivtnum = 2                                                   
        read(unit=c431k(1),fmt=39204) avd                               
39204 format(d12.7)                                                   
           if (avd - 0.2099999999d+02) 20020, 10020, 40020              
40020 if (avd - 0.2100000001d+02) 10020, 10020, 20020              
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = 2.1d1                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                          FROM SUBSTRING  
           ivtnum = 3                                                   
        read(unit=a43vk(19:),fmt=39206) avd                             
39206 format(11x,d14.9)                                               
           if (avd - 0.2344999998d+04) 20030, 10030, 40030              
40030 if (avd - 0.2345000002d+04) 10030, 10030, 20030              
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 23.45d2                                             
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                          FROM ARRAY      
           ivtnum = 4                                                   
        read(unit=c431k,fmt=39208) cvd                                  
39208 format(25x,d18.10)                                              
           if (cvd - 0.2344999998d+04) 20040, 10040, 40040              
40040 if (cvd - 0.2345000002d+04) 10040, 10040, 20040              
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 23.45d2                                             
           write (nuvi, 80031) ivtnum, cvd, dvcorr                      
 0041      continue                                                     
! *****                                                                  
! *****  TESTS 5 THRU 9 - LIST FROM ARRAY                                
! *****                                                                  
! T005*  TEST 5                                                          
           ivtnum = 5                                                   
        read(unit=b291k,fmt=39210) avd, bvd, cvd, dvd, evd              
39210 format(d10.5,1x,d10.5,/,d10.5,/,d10.5,//,d10.5)                 
           if (avd - 0.3438456998d+03) 20050, 10050, 40050              
40050 if (avd - 0.3438457002d+03) 10050, 10050, 20050              
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 34.38457d1                                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0051      continue                                                     
! T006*  TEST 6                                                          
           ivtnum = 6                                                   
           if (bvd - 0.3438456998d+03) 20060, 10060, 40060              
40060 if (bvd - 0.3438457002d+03) 10060, 10060, 20060              
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 34.38457d1                                          
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0061      continue                                                     
! T007*  TEST 7                                                          
           ivtnum = 7                                                   
           if (cvd - 0.3438456998d+03) 20070, 10070, 40070              
40070 if (cvd - 0.3438457002d+03) 10070, 10070, 20070              
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 34.38457d1                                          
           write (nuvi, 80031) ivtnum, cvd, dvcorr                      
 0071      continue                                                     
! T008*  TEST 8                                                          
           ivtnum = 8                                                   
           if (dvd - 0.3438456998d+03) 20080, 10080, 40080              
40080 if (dvd - 0.3438457002d+03) 10080, 10080, 20080              
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = 34.38457d1                                          
           write (nuvi, 80031) ivtnum, dvd, dvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                                          
           ivtnum = 9                                                   
           if (evd - 0.3438456998d+03) 20090, 10090, 40090              
40090 if (evd - 0.3438457002d+03) 10090, 10090, 20090              
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = 34.38457d1                                          
           write (nuvi, 80031) ivtnum, evd, dvcorr                      
 0091      continue                                                     
! *****                                                                  
! *****  TESTS 10 THRU 13 - LIST FROM VARIABLE WITH DIFFERENT FORMATS    
! *****                                                                  
! T010*  TEST 10                                                         
           ivtnum = 10                                                  
        read(unit=d43vk,fmt=39212) avd, bvd, cvd, dvd                   
39212 format(d10.5,1x,f10.5,d11.5,g11.5)                              
           if (avd - 0.3458672998d+04) 20100, 10100, 40100              
40100 if (avd - 0.3458673002d+04) 10100, 10100, 20100              
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 34.58673d2                                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0101      continue                                                     
! T011*  TEST 11                                                         
           ivtnum = 11                                                  
           if (bvd - 0.3458672998d+04) 20110, 10110, 40110              
40110 if (bvd - 0.3458673002d+04) 10110, 10110, 20110              
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr = 34.58673d2                                          
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0111      continue                                                     
! T012*  TEST 12                                                         
           ivtnum = 12                                                  
           if (cvd - 0.3458672998d+04) 20120, 10120, 40120              
40120 if (cvd - 0.3458673002d+04) 10120, 10120, 20120              
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr = 34.58673d2                                          
           write (nuvi, 80031) ivtnum, cvd, dvcorr                      
 0121      continue                                                     
! T013*  TEST 13                                                         
           ivtnum = 13                                                  
           if (dvd - 0.3458672998d+04) 20130, 10130, 40130              
40130 if (dvd - 0.3458673002d+04) 10130, 10130, 20130              
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr = 34.58673d2                                          
           write (nuvi, 80031) ivtnum, dvd, dvcorr                      
 0131      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****  TESTS 14 THRU 19 - MIXED TYPES                                  
! *****                                                                  
! T014*  TEST 14                                                         
           ivtnum = 14                                                  
        read(unit=e51vk,fmt=39214) kvi, avs, avd, avb, bvs, a8vk        
39214 format(i4,1x,e9.4,d10.4,1x,l4,1x,f12.5,1x,a8)                   
           if (kvi - 348) 20140, 10140, 20140                           
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           ivcorr = 348                                                 
           write (nuvi, 80010) ivtnum, kvi, ivcorr                      
 0141      continue                                                     
! T015*  TEST 15                                                         
           ivtnum = 15                                                  
           if (avs - 0.34781e+02) 20150, 10150, 40150                   
40150 if (avs - 0.34785e+02) 10150, 10150, 20150                   
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = 34.783                                              
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0151      continue                                                     
! T016*  TEST 16                                                         
           ivtnum = 16                                                  
           if (avd - 0.3843846998d+04) 20160, 10160, 40160              
40160 if (avd - 0.3843847002d+04) 10160, 10160, 20160              
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr = 384.3847d1                                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0161      continue                                                     
! T017*  TEST 17                                                         
           ivtnum = 17                                                  
           ivcomp = 0                                                   
           if (avb) ivcomp = 1                                          
           if (ivcomp - 1) 20170, 10170, 20170                          
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           lvcorr = 1                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivcomp                                   
           write (nuvi, 80026) lvcorr                                   
 0171      continue                                                     
! T018*  TEST 18                                                         
           ivtnum = 18                                                  
           if (bvs - 0.34855e+01) 20180, 10180, 40180                   
40180 if (bvs - 0.34859e+01) 10180, 10180, 20180                   
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = 3.4857                                              
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0181      continue                                                     
! T019*  TEST 19                                                         
           ivtnum = 19                                                  
           ivcomp = 0                                                   
           if (a8vk == 'KDFJ D/.') ivcomp = 1                           
           if (ivcomp - 1) 20190, 10190, 20190                          
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           cvcorr = 'KDFJ D/.'                                          
           write (nuvi, 80018) ivtnum, a8vk, cvcorr                     
 0191      continue                                                     
! *****                                                                  
! *****  TESTS 20 THRU 25 - MIXED TYPES WITH TC, TLC, TRC, AND NX        
! *****                                                                  
! T020*  TEST 20                                                         
           ivtnum = 20                                                  
        read(unit=f43vk,fmt=39216) avb, avs, a8vk, avd, bvs, kvi        
39216 format(l1,t5,f5.2,a8,tr2,d8.4,tl8,f6.4,4x,i1)                   
           ivcomp = 0                                                   
           if (avb) ivcomp = 1                                          
           if (ivcomp - 1) 20200, 10200, 20200                          
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           lvcorr = 1                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivcomp                                   
           write (nuvi, 80026) lvcorr                                   
 0201      continue                                                     
! T021*  TEST 21                                                         
           ivtnum = 21                                                  
           if (avs - 0.10979e+02) 20210, 10210, 40210                   
40210 if (avs - 0.10981e+02) 10210, 10210, 20210                   
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           rvcorr = 10.98                                               
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0211      continue                                                     
! T022*  TEST 22                                                         
           ivtnum = 22                                                  
           ivcomp = 0                                                   
           if (a8vk == 'THISISIT') ivcomp = 1                           
           if (ivcomp - 1) 20220, 10220, 20220                          
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           cvcorr = 'THISISIT'                                          
           write (nuvi, 80018) ivtnum, a8vk, cvcorr                     
 0221      continue                                                     
! T023*  TEST 23                                                         
           ivtnum = 23                                                  
           if (avd - 0.3494499998d+03) 20230, 10230, 40230              
40230 if (avd - 0.3494500002d+03) 10230, 10230, 20230              
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           dvcorr = 3.4945d2                                            
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0231      continue                                                     
! T024*  TEST 24                                                         
           ivtnum = 24                                                  
           if (bvs - 0.34943e+01) 20240, 10240, 40240                   
40240 if (bvs - 0.34947e+01) 10240, 10240, 20240                   
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           rvcorr = 3.4945                                              
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0241      continue                                                     
! T025*  TEST 25                                                         
           ivtnum = 25                                                  
           if (kvi - 3) 20250, 10250, 20250                             
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           ivcorr = 3                                                   
           write (nuvi, 80010) ivtnum, kvi, ivcorr                      
 0251      continue                                                     
! T026*  TEST 26                                 COMPLEX FROM VARIABLE   
           ivtnum = 26                                                  
        read(unit=g43vk,fmt=39218) avc                                  
39218 format(f10.5,1x,f10.5)                                          
           if (r2e(1) - 0.23428e+01) 20260, 40262, 40261                
40261 if (r2e(1) - 0.23432e+01) 40262, 40262, 20260                
40262 if (r2e(2) - 0.34392e+02) 20260, 10260, 40260                
40260 if (r2e(2) - 0.34396e+02) 10260, 10260, 20260                
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           zvcorr = (2.343, 34.394)                                     
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0261      continue                                                     
! T027*  TEST 27                                 COMPLEX FROM ARRAY      
           ivtnum = 27                                                  
        read(unit=h131k,fmt=39220) avc                                  
39220 format(e12.5,/,e12.5)                                           
           if (r2e(1) - 0.34838e+02) 20270, 40272, 40271                
40271 if (r2e(1) - 0.34842e+02) 40272, 40272, 20270                
40272 if (r2e(2) - 0.34987e+03) 20270, 10270, 40270                
40270 if (r2e(2) - 0.34991e+03) 10270, 10270, 20270                
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           zvcorr = (34.84, 349.887)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0271      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****  TESTS 28 THRU 31 - COMPLEX LIST FROM VARIABLE POSITION 1X BEYOND
! *****                     VARIABLE LENGTH                              
! T028*  TEST 28                                                         
           ivtnum = 28                                                  
        read(unit=i82vk,fmt=39222) avc, bvc, cvc, dvc                   
39222 format(2(2(g7.5,1x),2x),2(g10.4e2,1x),1x,2(g11.7e4,1x))         
           if (r2e(1) - 0.23398e+01) 20280, 40282, 40281                
40281 if (r2e(1) - 0.23402e+01) 40282, 40282, 20280                
40282 if (r2e(2) - 0.24558e+01) 20280, 10280, 40280                
40280 if (r2e(2) - 0.24562e+01) 10280, 10280, 20280                
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           zvcorr = (2.34, 2.456)                                       
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0281      continue                                                     
! T029*  TEST 29                                                         
           ivtnum = 29                                                  
           if (r2e(3) - 0.23398e+01) 20290, 40292, 40291                
40291 if (r2e(3) - 0.23402e+01) 40292, 40292, 20290                
40292 if (r2e(4) - 0.24558e+01) 20290, 10290, 40290                
40290 if (r2e(4) - 0.24562e+01) 10290, 10290, 20290                
10290 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           zvcorr = (2.34, 2.456)                                       
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0291      continue                                                     
! T030*  TEST 30                                                         
           ivtnum = 30                                                  
           if (r2e(5) - 0.23398e+01) 20300, 40302, 40301                
40301 if (r2e(5) - 0.23402e+01) 40302, 40302, 20300                
40302 if (r2e(6) - 0.24558e+01) 20300, 10300, 40300                
40300 if (r2e(6) - 0.24562e+01) 10300, 10300, 20300                
10300 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           zvcorr = (2.34, 2.456)                                       
           write (nuvi, 80045) ivtnum, cvc, zvcorr                      
 0301      continue                                                     
! T031*  TEST 31                                                         
           ivtnum = 31                                                  
           if (r2e(7) - 0.23398e+01) 20310, 40312, 40311                
40311 if (r2e(7) - 0.23402e+01) 40312, 40312, 20310                
40312 if (r2e(8) - 0.24558e+01) 20310, 10310, 40310                
40310 if (r2e(8) - 0.24562e+01) 10310, 10310, 20310                
10310 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           zvcorr = (2.34, 2.456)                                       
           write (nuvi, 80045) ivtnum, dvc, zvcorr                      
 0311      continue                                                     
! *****                                                                  
! *****  TESTS 32 THRU 35 - COMPLEX LIST USING EW.D AND EW.DEN           
! *****                                                                  
! T032*  TEST 32                                                         
           ivtnum = 32                                                  
        read(unit=j97vk(1:),fmt=39224) avc, bvc, cvc, dvc               
39224 format(2(2(e10.5,1x),2x),2(e10.4e2,1x),1x,2(e12.5e4,1x))        
           if (r2e(1) - 0.56795e+01) 20320, 40322, 40321                
40321 if (r2e(1) - 0.56801e+01) 40322, 40322, 20320                
40322 if (r2e(2) - 0.98755e+00) 20320, 10320, 40320                
40320 if (r2e(2) - 0.98765e+00) 10320, 10320, 20320                
10320 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           zvcorr = (5.6798, 0.9876)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0321      continue                                                     
! T033*  TEST 33                                                         
           ivtnum = 33                                                  
           if (r2e(3) - 0.56795e+01) 20330, 40332, 40331                
40331 if (r2e(3) - 0.56801e+01) 40332, 40332, 20330                
40332 if (r2e(4) - 0.98755e+00) 20330, 10330, 40330                
40330 if (r2e(4) - 0.98765e+00) 10330, 10330, 20330                
10330 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           zvcorr = (5.6798, 0.9876)                                    
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0331      continue                                                     
! T034*  TEST 34                                                         
           ivtnum = 34                                                  
           if (r2e(5) - 0.56795e+01) 20340, 40342, 40341                
40341 if (r2e(5) - 0.56801e+01) 40342, 40342, 20340                
40342 if (r2e(6) - 0.98755e+00) 20340, 10340, 40340                
40340 if (r2e(6) - 0.98765e+00) 10340, 10340, 20340                
10340 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           zvcorr = (5.6798, 0.9876)                                    
           write (nuvi, 80045) ivtnum, cvc, zvcorr                      
 0341      continue                                                     
! T035*  TEST 35                                                         
           ivtnum = 35                                                  
           if (r2e(7) - 0.56795e+01) 20350, 40352, 40351                
40351 if (r2e(7) - 0.56801e+01) 40352, 40352, 20350                
40352 if (r2e(8) - 0.98755e+00) 20350, 10350, 40350                
40350 if (r2e(8) - 0.98765e+00) 10350, 10350, 20350                
10350 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           zvcorr = (5.6798, 0.9876)                                    
           write (nuvi, 80045) ivtnum, dvc, zvcorr                      
 0351      continue                                                     
! *****                                                                  
! *****  TESTS 36 THRU 38 - MIXED TYPES FROM VARIABLE                    
! *****                                                                  
! T036*  TEST 36                                                         
           ivtnum = 36                                                  
        read(unit=k43vk,fmt=39226) avc, avs, avd                        
39226 format(f7.3,1x,f7.3,1x,f10.5,1x,d13.5)                          
           if (r2e(1) - 0.93395e+00) 20360, 40362, 40361                
40361 if (r2e(1) - 0.93405e+00) 40362, 40362, 20360                
40362 if (r2e(2) - 0.34565e+02) 20360, 10360, 40360                
40360 if (r2e(2) - 0.34569e+02) 10360, 10360, 20360                
10360 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0361                                                   
20360 ivfail = ivfail + 1                                          
           zvcorr = (0.934, 34.567)                                     
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0361      continue                                                     
! T037*  TEST 37                                                         
           ivtnum = 37                                                  
           if (avs - 0.34648e+02) 20370, 10370, 40370                   
40370 if (avs - 0.34652e+02) 10370, 10370, 20370                   
10370 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0371                                                   
20370 ivfail = ivfail + 1                                          
           rvcorr = 34.65                                               
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0371      continue                                                     
! T038*  TEST 38                                                         
           ivtnum = 38                                                  
           if (avd - 0.6353999996d+01) 20380, 10380, 40380              
40380 if (avd - 0.6354000004d+01) 10380, 10380, 20380              
10380 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0381                                                   
20380 ivfail = ivfail + 1                                          
           dvcorr = 0.6354d1                                            
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0381      continue                                                     
! *****                                                                  
! *****  TESTS 39 THRU 43 - MIXED TYPES FROM ARRAY                       
! *****                                                                  
! T039*  TEST 39                                                         
           ivtnum = 39                                                  
        read(unit=l53vk,fmt=39228) avc, avb, avd, avs, a8vk             
39228 format(f9.4,1x,e9.4,1x,l1,1x,d12.5,1x,g9.4,a8)                  
           if (r2e(1) - 0.34498e+00) 20390, 40392, 40391                
40391 if (r2e(1) - 0.34502e+00) 40392, 40392, 20390                
40392 if (r2e(2) - 0.34343e+02) 20390, 10390, 40390                
40390 if (r2e(2) - 0.34347e+02) 10390, 10390, 20390                
10390 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0391                                                   
20390 ivfail = ivfail + 1                                          
           zvcorr = (0.345, 34.345)                                     
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0391      continue                                                     
! T040*  TEST 40                                                         
           ivtnum = 40                                                  
           ivcomp = 0                                                   
           if (avb) ivcomp = 1                                          
           if (ivcomp - 0) 20400, 10400, 20400                          
10400 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0401                                                   
20400 ivfail = ivfail + 1                                          
           lvcorr = 0                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivcomp                                   
           write (nuvi, 80026) lvcorr                                   
 0401      continue                                                     
! T041*  TEST 41                                                         
           ivtnum = 41                                                  
           if (avd - 0.3485899998d+01) 20410, 10410, 40410              
40410 if (avd - 0.3485900002d+01) 10410, 10410, 20410              
10410 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0411                                                   
20410 ivfail = ivfail + 1                                          
           dvcorr = 34.859d-1                                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0411      continue                                                     
! T042*  TEST 42                                                         
           ivtnum = 42                                                  
           if (avs - 0.99995e+01) 20420, 10420, 40420                   
40420 if (avs - 0.10001e+02) 10420, 10420, 20420                   
10420 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0421                                                   
20420 ivfail = ivfail + 1                                          
           rvcorr = 10.0                                                
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0421      continue                                                     
! T043*  TEST 43                                                         
           ivtnum = 43                                                  
           ivcomp = 0                                                   
           if (a8vk == '12345678') ivcomp = 1                           
           if (ivcomp - 1) 20430, 10430, 20430                          
10430 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0431                                                   
20430 ivfail = ivfail + 1                                          
           cvcorr = '12345678'                                          
           write (nuvi, 80018) ivtnum, a8vk, cvcorr                     
 0431      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****  TESTS 44 THRU 48 - READ 5 RECORD FROM ARRAY POSITION 1X BEYOND  
! *****                     ARRAY ELEMENT                                
! *****                                                                  
! T044*  TEST 44                                                         
           ivtnum = 44                                                  
        read(unit=m291k,fmt=39230) kvi, avd, avb, avc, a8vk             
39230 format(i5,/,d10.5,//,1x,l6,1x,2(e10.3,1x),/,a8)                 
           if (kvi - 98) 20440, 10440, 20440                            
10440 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0441                                                   
20440 ivfail = ivfail + 1                                          
           ivcorr = 98                                                  
           write (nuvi, 80010) ivtnum, kvi, ivcorr                      
 0441      continue                                                     
! T045*  TEST 45                                                         
           ivtnum = 45                                                  
           if (avd - 0.8404849995d+03) 20450, 10450, 40450              
40450 if (avd - 0.8404850004d+03) 10450, 10450, 20450              
10450 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0451                                                   
20450 ivfail = ivfail + 1                                          
           dvcorr = 84.0485d1                                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0451      continue                                                     
! T046*  TEST 46                                                         
           ivtnum = 46                                                  
           ivcomp = 0                                                   
           if (avb) ivcomp = 1                                          
           if (ivcomp - 1) 20460, 10460, 20460                          
10460 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0461                                                   
20460 ivfail = ivfail + 1                                          
           lvcorr = 1                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivcomp                                   
           write (nuvi, 80026) lvcorr                                   
 0461      continue                                                     
! T047*  TEST 47                                                         
           ivtnum = 47                                                  
           if (r2e(1) - 0.34041e+02) 20470, 40472, 40471                
40471 if (r2e(1) - 0.34046e+02) 40472, 40472, 20470                
40472 if (r2e(2) - 0.34938e+02) 20470, 10470, 40470                
40470 if (r2e(2) - 0.34942e+02) 10470, 10470, 20470                
10470 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0471                                                   
20470 ivfail = ivfail + 1                                          
           zvcorr = (34.0435, 34.94)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0471      continue                                                     
! T048*  TEST 48                                                         
           ivtnum = 48                                                  
           ivcomp = 0                                                   
           if (a8vk == '87654321') ivcomp = 1                           
           if (ivcomp - 1) 20480, 10480, 20480                          
10480 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0481                                                   
20480 ivfail = ivfail + 1                                          
           cvcorr = '87654321'                                          
           write (nuvi, 80018) ivtnum, a8vk, cvcorr                     
 0481      continue                                                     
! *****                                                                  
! ***** TESTS 49 THRU 54 - MIXED TYPES, NX, AND :                        
! *****                                                                  
! T049*  TEST 49                                                         
           ivtnum = 49                                                  
        read(unit=n43vk,fmt=39232)jvi,avs,avd,avb,a8vk,bvs              
39232 format(i2,1x,f6.3,d10.5,l2,a8,e10.5,:,i5,2x,f10.4)              
           if (jvi - 34) 20490, 10490, 20490                            
10490 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0491                                                   
20490 ivfail = ivfail + 1                                          
           ivcorr = 34                                                  
           write (nuvi, 80010) ivtnum, jvi, ivcorr                      
 0491      continue                                                     
! T050*  TEST 50                                                         
           ivtnum = 50                                                  
           if (avs - 0.34981e+02) 20500, 10500, 40500                   
40500 if (avs - 0.34985e+02) 10500, 10500, 20500                   
10500 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0501                                                   
20500 ivfail = ivfail + 1                                          
           rvcorr = 34.983                                              
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0501      continue                                                     
! T051*  TEST 51                                                         
           ivtnum = 51                                                  
           if (avd - 0.9582999995d+04) 20510, 10510, 40510              
40510 if (avd - 0.9583000005d+04) 10510, 10510, 20510              
10510 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0511                                                   
20510 ivfail = ivfail + 1                                          
           dvcorr = 95.83d2                                             
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0511      continue                                                     
! T052*  TEST 52                                                         
           ivtnum = 52                                                  
           ivcomp = 0                                                   
           if (avb) ivcomp = 1                                          
           if (ivcomp - 0) 20520, 10520, 20520                          
10520 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0521                                                   
20520 ivfail = ivfail + 1                                          
           lvcorr = 0                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivcomp                                   
           write (nuvi, 80026) lvcorr                                   
 0521      continue                                                     
! T053*  TEST 53                                                         
           ivtnum = 53                                                  
           ivcomp = 0                                                   
           if (a8vk == '.FALSE.1') ivcomp = 1                           
           if (ivcomp - 1) 20530, 10530, 20530                          
10530 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0531                                                   
20530 ivfail = ivfail + 1                                          
           cvcorr = '.FALSE.1'                                          
           write (nuvi, 80018) ivtnum, a8vk, cvcorr                     
 0531      continue                                                     
! T054*  TEST 54                                                         
           ivtnum = 54                                                  
           if (bvs - 0.34528e+03) 20540, 10540, 40540                   
40540 if (bvs - 0.34532e+03) 10540, 10540, 20540                   
10540 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0541                                                   
20540 ivfail = ivfail + 1                                          
           rvcorr = 345.3                                               
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0541      continue                                                     
! *****                                                                  
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
! *****    END OF TEST SEGMENT 392                                       
      stop                                                              
      end program fm908
