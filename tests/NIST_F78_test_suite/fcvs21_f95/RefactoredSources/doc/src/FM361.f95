      program fm361
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM361               XMAX - (165)                               
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                       SUBSET REF
! *****    TEST OF INTRINSIC FUNCTIONS AMAX0,AMAX1,MAX0,MAX1      15.3   
! *****    CHOOSING LARGEST VALUE                               (TABLE 5)
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
      integer :: nuvi
      integer :: ivtnum
      integer :: ihbvi
      integer :: ihdvi
      real :: rhavs
      real :: rvcorr
      integer :: ihevi
      integer :: ihcvi
      real :: rhbvs
      real :: rhdvs
      real :: rhevs
      real :: rhcvs
      integer :: ihavi
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
      ivtotl = 48                                                       
      zprog = 'FM361'                                                   
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
! *****    HEADER FOR SEGMENT 165                                        
        write (nuvi,16501)                                              
16501 format (" ", // 2x,"XMAX - (165) INTRINSIC FUNCTIONS--  " //13x,          "AMAX0, AMAX1, MAX0, MAX1  "    /13x,                             "(CHOOSING LARGEST VALUE)" //2x,                                  "SUBSET REF. - 15.3" )                                  
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! *****                                                                  
! *****    TEST OF AMAX0                                                 
! *****                                                                  
        write(nuvi, 16502)                                              
16502 format (/ 8x, "TEST OF AMAX0" )                                 
! T001*  TEST 1                                            BOTH ZEROES   
           ivtnum = 1                                                   
        ihbvi = 0                                                       
        ihdvi = 0                                                       
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs + 0.00005) 20010, 10010, 40010                     
40010 if (rhavs - 0.00005) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0011      continue                                                     
! T002*  TEST 2                                 ONE NON-ZERO, ONE ZERO   
           ivtnum = 2                                                   
        ihbvi = 6                                                       
        ihdvi = 0                                                       
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs - 5.9997) 20020, 10020, 40020                      
40020 if (rhavs - 6.0003) 10020, 10020, 20020                      
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 6.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0021      continue                                                     
! T003*  TEST 3                                      BOTH VALUES EQUAL   
           ivtnum = 3                                                   
        ihbvi = 7                                                       
        ihdvi = 7                                                       
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs - 6.9996) 20030, 10030, 40030                      
40030 if (rhavs - 7.0004) 10030, 10030, 20030                      
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 7.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0031      continue                                                     
! T004*  TEST 4                          UNEQUAL VALUES, BOTH POSITIVE   
           ivtnum = 4                                                   
        ihbvi = 7                                                       
        ihdvi = 5                                                       
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs - 6.9996) 20040, 10040, 40040                      
40040 if (rhavs - 7.0004) 10040, 10040, 20040                      
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 7.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0041      continue                                                     
! T005*  TEST 5                                 ONE NEGATIVE, ONE ZERO   
           ivtnum = 5                                                   
        ihbvi = -6                                                      
        ihdvi = 0                                                       
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs + 0.00005) 20050, 10050, 40050                     
40050 if (rhavs - 0.00005) 10050, 10050, 20050                     
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0051      continue                                                     
! T006*  TEST 6                       BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 6                                                   
        ihbvi = -7                                                      
        ihdvi = -7                                                      
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs + 7.0004) 20060, 10060, 40060                      
40060 if (rhavs + 6.9996) 10060, 10060, 20060                      
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = -7.0                                                
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0061      continue                                                     
! T007*  TEST 7                   BOTH VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 7                                                   
        ihbvi = -7                                                      
        ihdvi = -5                                                      
        rhavs = amax0(ihbvi,ihdvi)                                      
           if (rhavs + 5.0003) 20070, 10070, 40070                      
40070 if (rhavs + 4.9997) 10070, 10070, 20070                      
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = -5.0                                                
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0071      continue                                                     
! T008*  TEST 8  1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY A MINUS SIGN   
           ivtnum = 8                                                   
        ihdvi = 6                                                       
        ihevi = 0                                                       
        rhavs = amax0(ihdvi, -ihevi)                                    
           if (rhavs - 5.9997) 20080, 10080, 40080                      
40080 if (rhavs - 6.0003) 10080, 10080, 20080                      
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = 6.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0081      continue                                                     
! T009*  TEST 9                      EXPRESSIONS PRESENTED TO FUNCTION   
           ivtnum = 9                                                   
        ihdvi = 3                                                       
        ihevi = 4                                                       
        rhavs = amax0(ihdvi + ihevi, -ihevi - ihdvi)                    
           if (rhavs - 6.9996) 20090, 10090, 40090                      
40090 if (rhavs - 7.0004) 10090, 10090, 20090                      
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 7.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0091      continue                                                     
! T010*  TEST 10                                           3 ARGUMENTS   
           ivtnum = 10                                                  
        ihbvi = 0                                                       
        ihcvi = 1                                                       
        ihdvi = 3                                                       
        rhavs = amax0(ihbvi, ihcvi, ihdvi)                              
           if (rhavs - 2.9998) 20100, 10100, 40100                      
40100 if (rhavs - 3.0002) 10100, 10100, 20100                      
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 3.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0101      continue                                                     
! T011*  TEST 11                                           4 ARGUMENTS   
           ivtnum = 11                                                  
        ihbvi = 0                                                       
        ihcvi = 1                                                       
        ihdvi = 4                                                       
        rhavs = amax0(ihdvi, -ihbvi, ihcvi, ihbvi)                      
           if (rhavs - 3.9998) 20110, 10110, 40110                      
40110 if (rhavs - 4.0002) 10110, 10110, 20110                      
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 4.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0111      continue                                                     
! T012*  TEST 12                                           5 ARGUMENTS   
           ivtnum = 12                                                  
        ihdvi = 4.0                                                     
        ihevi = 5.0                                                     
        rhavs = amax0(ihdvi, -ihdvi, -ihevi, +ihdvi, ihevi)             
           if (rhavs - 4.9997) 20120, 10120, 40120                      
40120 if (rhavs - 5.0003) 10120, 10120, 20120                      
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 5.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0121      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****    TEST OF AMAX1                                                 
! *****                                                                  
        write(nuvi, 16504)                                              
16504 format (/ 8x, "TEST OF AMAX1" )                                 
! T013*  TEST 13                                      BOTH VALUES ZERO   
           ivtnum = 13                                                  
        rhbvs = 0.0                                                     
        rhdvs = 0.0                                                     
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs + 0.00005) 20130, 10130, 40130                     
40130 if (rhavs - 0.00005) 10130, 10130, 20130                     
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0131      continue                                                     
! T014*  TEST 14                     FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 14                                                  
        rhbvs = 5.625                                                   
        rhdvs = 0.0                                                     
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs - 5.6247) 20140, 10140, 40140                      
40140 if (rhavs - 5.6253) 10140, 10140, 20140                      
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = 5.625                                               
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0141      continue                                                     
! T015*  TEST 15                                     BOTH VALUES EQUAL   
           ivtnum = 15                                                  
        rhbvs = 6.5                                                     
        rhdvs = 6.5                                                     
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs - 6.4996) 20150, 10150, 40150                      
40150 if (rhavs - 6.5004) 10150, 10150, 20150                      
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = 6.5                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0151      continue                                                     
! T016*  TEST 16                                      VALUES NOT EQUAL   
           ivtnum = 16                                                  
        rhbvs = 7.125                                                   
        rhdvs = 5.125                                                   
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs - 7.1246) 20160, 10160, 40160                      
40160 if (rhavs - 7.1254) 10160, 10160, 20160                      
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = 7.125                                               
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0161      continue                                                     
! T017*  TEST 17                     FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 17                                                  
        rhbvs = -5.625                                                  
        rhdvs = 0.0                                                     
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs + 0.00005) 20170, 10170, 40170                     
40170 if (rhavs - 0.00005) 10170, 10170, 20170                     
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0171      continue                                                     
! T018*  TEST 18                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 18                                                  
        rhbvs = -6.5                                                    
        rhdvs = -6.5                                                    
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs + 6.5004) 20180, 10180, 40180                      
40180 if (rhavs + 6.4996) 10180, 10180, 20180                      
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = -6.5                                                
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0181      continue                                                     
! T019*  TEST 19                       VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 19                                                  
        rhbvs = -7.125                                                  
        rhdvs = -5.125                                                  
        rhavs = amax1(rhbvs, rhdvs)                                     
           if (rhavs + 5.1253) 20190, 10190, 40190                      
40190 if (rhavs + 5.1247) 10190, 10190, 20190                      
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           rvcorr = -5.125                                              
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0191      continue                                                     
! T020*  TEST 20   1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 20                                                  
        rhdvs = 5.625                                                   
        rhevs = 0.0                                                     
        rhavs = amax1(rhdvs, -rhevs)                                    
           if (rhavs - 5.6247) 20200, 10200, 40200                      
40200 if (rhavs - 5.6253) 10200, 10200, 20200                      
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           rvcorr = 5.625                                               
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0201      continue                                                     
! T021*  TEST 21                     EXPRESSIONS PRESENTED TO FUNCTION   
           ivtnum = 21                                                  
        rhdvs = 3.5                                                     
        rhevs = 4.0                                                     
        rhavs = amax1(rhdvs + rhevs, -rhevs - rhdvs)                    
           if (rhavs - 7.4996) 20210, 10210, 40210                      
40210 if (rhavs - 7.5004) 10210, 10210, 20210                      
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           rvcorr = 7.5                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0211      continue                                                     
! T022*  TEST 22                                           3 ARGUMENTS   
           ivtnum = 22                                                  
        rhbvs = 0.0                                                     
        rhcvs = 1.0                                                     
        rhdvs = 0.5                                                     
        rhavs = amax1(rhbvs, rhcvs, rhdvs)                              
           if (rhavs - 0.99995) 20220, 10220, 40220                     
40220 if (rhavs - 1.0001) 10220, 10220, 20220                      
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0221      continue                                                     
! T023*  TEST 23                                           4 ARGUMENTS   
           ivtnum = 23                                                  
        rhbvs = 1.5                                                     
        rhcvs = 3.4                                                     
        rhdvs = 3.5                                                     
        rhavs = amax1(-rhdvs, rhcvs, rhbvs, rhdvs)                      
           if (rhavs - 3.4998) 20230, 10230, 40230                      
40230 if (rhavs - 3.5002) 10230, 10230, 20230                      
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           rvcorr = 3.5                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0231      continue                                                     
! T024*  TEST 24                                           5 ARGUMENTS   
           ivtnum = 24                                                  
        rhdvs = 3.5                                                     
        rhevs = 4.5                                                     
        rhavs = amax1(rhdvs, -rhdvs, -rhevs, +rhdvs, rhevs)             
           if (rhavs - 4.4997) 20240, 10240, 40240                      
40240 if (rhavs - 4.5003) 10240, 10240, 20240                      
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           rvcorr = 4.5                                                 
           write (nuvi, 80012) ivtnum, rhavs, rvcorr                    
 0241      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****    TEST OF MAX0                                                  
! *****                                                                  
        write(nuvi, 16505)                                              
16505 format (/ 8x, "TEST OF MAX0" )                                  
! *****                                                                  
! T025*  TEST 25                                      BOTH VALUES ZERO   
           ivtnum = 25                                                  
        ihbvi = 0                                                       
        ihdvi = 0                                                       
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi - 0) 20250, 10250, 20250                           
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0251      continue                                                     
! T026*  TEST 26                     FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 26                                                  
        ihbvi = 6                                                       
        ihdvi = 0                                                       
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi - 6) 20260, 10260, 20260                           
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           ivcorr = 6                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0261      continue                                                     
! T027*  TEST 27                                     BOTH VALUES EQUAL   
           ivtnum = 27                                                  
        ihbvi = 7                                                       
        ihdvi = 7                                                       
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi - 7) 20270, 10270, 20270                           
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           ivcorr = 7                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0271      continue                                                     
! T028*  TEST 28                                      VALUES NOT EQUAL   
           ivtnum = 28                                                  
        ihbvi = 7                                                       
        ihdvi = 5                                                       
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi - 7) 20280, 10280, 20280                           
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           ivcorr = 7                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0281      continue                                                     
! T029*  TEST 29                     FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 29                                                  
        ihbvi = -6                                                      
        ihdvi = 0                                                       
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi - 0) 20290, 10290, 20290                           
10290 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0291      continue                                                     
! T030*  TEST 30                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 30                                                  
        ihbvi = -7                                                      
        ihdvi = -7                                                      
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi + 7) 20300, 10300, 20300                           
10300 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           ivcorr = -7                                                  
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0301      continue                                                     
! T031*  TEST 31                       VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 31                                                  
        ihbvi = -7                                                      
        ihdvi = -5                                                      
        ihavi = max0(ihbvi, ihdvi)                                      
           if (ihavi + 5) 20310, 10310, 20310                           
10310 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0311      continue                                                     
! T032*  TEST 32   1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 32                                                  
        ihdvi = 6                                                       
        ihevi = 0                                                       
        ihavi = max0(ihdvi, -ihevi)                                     
           if (ihavi - 6) 20320, 10320, 20320                           
10320 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           ivcorr = 6                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0321      continue                                                     
! T033*  TEST 33                     EXPRESSIONS PRESENTED TO FUNCTION   
           ivtnum = 33                                                  
        ihdvi = 3                                                       
        ihevi = 4                                                       
        ihavi = max0(ihdvi + ihevi, -ihevi - ihdvi)                     
           if (ihavi - 7) 20330, 10330, 20330                           
10330 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           ivcorr = 7                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0331      continue                                                     
! T034*  TEST 34                                           3 ARGUMENTS   
           ivtnum = 34                                                  
        ihbvi = 0                                                       
        ihcvi = 3                                                       
        ihdvi = -4                                                      
        ihavi = max0(ihdvi, ihbvi, ihcvi)                               
           if (ihavi - 3) 20340, 10340, 20340                           
10340 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           ivcorr = 3                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0341      continue                                                     
! T035*  TEST 35                                           4 ARGUMENTS   
           ivtnum = 35                                                  
        ihbvi = -1                                                      
        ihcvi = 0                                                       
        ihdvi = 4                                                       
        ihavi = max0(ihdvi, ihcvi, ihbvi, ihdvi)                        
           if (ihavi - 4) 20350, 10350, 20350                           
10350 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           ivcorr = 4                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0351      continue                                                     
! T036*  TEST 36                                           5 ARGUMENTS   
           ivtnum = 36                                                  
        ihdvi = 4                                                       
        ihevi = 5                                                       
        ihavi = max0(ihdvi, -ihdvi, -ihevi, +ihdvi, ihevi)              
           if (ihavi - 5) 20360, 10360, 20360                           
10360 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0361                                                   
20360 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0361      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****    TEST OF MAX1                                                  
! *****                                                                  
        write(nuvi, 16507)                                              
16507 format (/ 8x, "TEST OF MAX1" )                                  
! T037*  TEST 37                                     BOTH VALUES EQUAL   
           ivtnum = 37                                                  
        rhbvs = 0.0                                                     
        rhdvs = 0.0                                                     
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi - 0) 20370, 10370, 20370                           
10370 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0371                                                   
20370 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0371      continue                                                     
! T038*  TEST 38                     FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 38                                                  
        rhbvs = 5.625                                                   
        rhdvs = 0.0                                                     
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi - 5) 20380, 10380, 20380                           
10380 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0381                                                   
20380 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0381      continue                                                     
! T039*  TEST 39                                     BOTH VALUES EQUAL   
           ivtnum = 39                                                  
        rhbvs = 6.5                                                     
        rhdvs = 6.5                                                     
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi - 6) 20390, 10390, 20390                           
10390 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0391                                                   
20390 ivfail = ivfail + 1                                          
           ivcorr = 6                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0391      continue                                                     
! T040*  TEST 40                                      VALUES NOT EQUAL   
           ivtnum = 40                                                  
        rhbvs = 7.125                                                   
        rhdvs = 5.125                                                   
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi - 7) 20400, 10400, 20400                           
10400 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0401                                                   
20400 ivfail = ivfail + 1                                          
           ivcorr = 7                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0401      continue                                                     
! T041*  TEST 41                     FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 41                                                  
        rhbvs = -5.625                                                  
        rhdvs = 0.0                                                     
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi - 0) 20410, 10410, 20410                           
10410 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0411                                                   
20410 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0411      continue                                                     
! T042*  TEST 42                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 42                                                  
        rhbvs = - 6.5                                                   
        rhdvs = - 6.5                                                   
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi + 6) 20420, 10420, 20420                           
10420 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0421                                                   
20420 ivfail = ivfail + 1                                          
           ivcorr = -6                                                  
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0421      continue                                                     
! T043*  TEST 43                      VALUES NOT EQUAL,  BOTH NEGATIVE   
           ivtnum = 43                                                  
        rhbvs = -7.125                                                  
        rhdvs = -5.125                                                  
        ihavi = max1(rhbvs, rhdvs)                                      
           if (ihavi + 5) 20430, 10430, 20430                           
10430 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0431                                                   
20430 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0431      continue                                                     
! T044*  TEST 44 1ST VALUE NON-ZERO, 2ND ZERO PRECEDED BY A MINUS SIGN   
           ivtnum = 44                                                  
        rhdvs = 5.625                                                   
        rhevs = 0.0                                                     
        ihavi = max1(rhdvs, -rhevs)                                     
           if (ihavi - 5) 20440, 10440, 20440                           
10440 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0441                                                   
20440 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0441      continue                                                     
! T045*  TEST 45                     EXPRESSIONS PRESENTED TO FUNCTION   
           ivtnum = 45                                                  
        rhdvs = 3.5                                                     
        rhevs = 4.0                                                     
        ihavi = max1(rhdvs + rhevs, -rhevs - rhdvs)                     
           if (ihavi - 7) 20450, 10450, 20450                           
10450 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0451                                                   
20450 ivfail = ivfail + 1                                          
           ivcorr = 7                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0451      continue                                                     
! T046*  TEST 46                                           3 ARGUMENTS   
           ivtnum = 46                                                  
        rhbvs = 0.0                                                     
        rhcvs = 4.0                                                     
        rhdvs = 0.0                                                     
        ihavi = max1(rhbvs, -rhcvs, rhdvs)                              
           if (ihavi - 0) 20460, 10460, 20460                           
10460 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0461                                                   
20460 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0461      continue                                                     
! T047*  TEST 47                                           4 ARGUMENTS   
           ivtnum = 47                                                  
        rhbvs = 3.49                                                    
        rhcvs = 0.0                                                     
        rhdvs = 3.5                                                     
        ihavi = max1(rhdvs, rhbvs, -rhbvs, rhcvs)                       
           if (ihavi - 3) 20470, 10470, 20470                           
10470 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0471                                                   
20470 ivfail = ivfail + 1                                          
           ivcorr = 3                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0471      continue                                                     
! T048*  TEST 48                                           5 ARGUMENTS   
           ivtnum = 48                                                  
        rhdvs = 3.5                                                     
        rhevs = 4.5                                                     
        ihavi = max1(rhdvs, -rhdvs, -rhevs, +rhdvs, rhevs)              
           if (ihavi - 4) 20480, 10480, 20480                           
10480 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0481                                                   
20480 ivfail = ivfail + 1                                          
           ivcorr = 4                                                   
           write (nuvi, 80010) ivtnum, ihavi, ivcorr                    
 0481      continue                                                     
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
! *****    END OF TEST SEGMENT 165                                       
        stop                                                            
        end program fm361
