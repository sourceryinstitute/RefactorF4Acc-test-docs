      program fm362
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM362               XMIN - (167)                               
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                       SUBSET REF
! *****    TEST INTRINSIC FUNCTIONS AMIN0,AMIN1,MIN0,MIN1         15.3   
! *****    CHOOSING SMALLEST VALUE.                             (TABLE 5)
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
      integer :: iibvi
      integer :: iidvi
      real :: riavs
      real :: rvcorr
      integer :: iievi
      integer :: iicvi
      real :: ribvs
      real :: ridvs
      real :: rievs
      real :: ricvs
      integer :: iiavi
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
      ivtotl = 47                                                       
      zprog = 'FM362'                                                   
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
! *****    HEADER FOR SEGMENT 167                                        
        write (nuvi,16700)                                              
16700 format (" ", // 2x,"XMIN - (167) INTRINSIC FUNCTIONS--  " //13x,          "AMIN0, AMIN1, MIN0, MIN1" / 13x,                                 "(CHOOSING SMALLEST VALUE)" //2x,                                 "SUBSET REF. - 15.3" )                                  
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
! *****    TEST OF AMIN0                                                 
! *****                                                                  
        write(nuvi, 16702)                                              
16702 format (/ 8x, "TEST OF AMIN0" )                                 
! T001*  TEST 1                                       BOTH VALUES ZERO   
           ivtnum = 1                                                   
        iibvi = 0                                                       
        iidvi = 0                                                       
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs + 0.00005) 20010, 10010, 40010                     
40010 if (riavs - 0.00005) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0011      continue                                                     
! T002*  TEST 2                      FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 2                                                   
        iibvi = 6                                                       
        iidvi = 0                                                       
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs + 0.00005) 20020, 10020, 40020                     
40020 if (riavs - 0.00005) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0021      continue                                                     
! T003*  TEST 3                                      BOTH VALUES EQUAL   
           ivtnum = 3                                                   
        iibvi = 7                                                       
        iidvi = 7                                                       
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs - 6.9996) 20030, 10030, 40030                      
40030 if (riavs - 7.0004) 10030, 10030, 20030                      
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 7.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0031      continue                                                     
! T004*  TEST 4                                       VALUES NOT EQUAL   
           ivtnum = 4                                                   
        iibvi = 7                                                       
        iidvi = 5                                                       
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs - 4.9997) 20040, 10040, 40040                      
40040 if (riavs - 5.0003) 10040, 10040, 20040                      
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 5.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0041      continue                                                     
! T005*  TEST 5                      FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 5                                                   
        iibvi = -6                                                      
        iidvi = 0                                                       
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs + 6.0003) 20050, 10050, 40050                      
40050 if (riavs + 5.9997) 10050, 10050, 20050                      
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = -6.0                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0051      continue                                                     
! T006*  TEST 6                       BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 6                                                   
        iibvi = -7                                                      
        iidvi = -7                                                      
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs + 7.0004) 20060, 10060, 40060                      
40060 if (riavs + 6.9996) 10060, 10060, 20060                      
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = -7.0                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0061      continue                                                     
! T007*  TEST 7                        VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 7                                                   
        iibvi = -7                                                      
        iidvi = -5                                                      
        riavs = amin0(iibvi, iidvi)                                     
           if (riavs + 7.0004) 20070, 10070, 40070                      
40070 if (riavs + 6.9996) 10070, 10070, 20070                      
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = -7.0                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0071      continue                                                     
! T008*  TEST 8  FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 8                                                   
        iidvi = 6                                                       
        iievi = 0                                                       
        riavs = amin0(iidvi, -iievi)                                    
           if (riavs + 0.00005) 20080, 10080, 40080                     
40080 if (riavs - 0.00005) 10080, 10080, 20080                     
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0081      continue                                                     
! T009*  TEST 9                                            3 ARGUMENTS   
           ivtnum = 9                                                   
        iibvi = 0                                                       
        iicvi = 9                                                       
        iidvi = 8                                                       
        riavs = amin0(iibvi, iicvi, iidvi)                              
           if (riavs + 0.00005) 20090, 10090, 40090                     
40090 if (riavs - 0.00005) 10090, 10090, 20090                     
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0091      continue                                                     
! T010*  TEST 10                                           4 ARGUMENTS   
           ivtnum = 10                                                  
        iibvi = 34                                                      
        iicvi = 8                                                       
        iidvi = 4                                                       
        riavs = amin0(iidvi, iibvi, iicvi, iidvi)                       
           if (riavs - 3.9998) 20100, 10100, 40100                      
40100 if (riavs - 4.0002) 10100, 10100, 20100                      
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 4.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0101      continue                                                     
! T011*  TEST 11                                           5 ARGUMENTS   
           ivtnum = 11                                                  
        iidvi = 4.0                                                     
        iievi = 5.0                                                     
        riavs = amin0(iidvi, -iidvi, -iievi, +iidvi, iievi)             
           if (riavs + 5.0003) 20110, 10110, 40110                      
40110 if (riavs + 4.9997) 10110, 10110, 20110                      
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = -5.0                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0111      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****    TEST OF AMIN1                                                 
! *****                                                                  
        write(nuvi, 16704)                                              
16704 format (/ 8x, "TEST OF AMIN1" )                                 
! T012*  TEST 12                                      BOTH VALUES ZERO   
           ivtnum = 12                                                  
        ribvs = 0.0                                                     
        ridvs = 0.0                                                     
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs + 0.00005) 20120, 10120, 40120                     
40120 if (riavs - 0.00005) 10120, 10120, 20120                     
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0121      continue                                                     
! T013*  TEST 13                     FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 13                                                  
        ribvs = 5.625                                                   
        ridvs = 0.0                                                     
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs + 0.00005) 20130, 10130, 40130                     
40130 if (riavs - 0.00005) 10130, 10130, 20130                     
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0131      continue                                                     
! T014*  TEST 14                                     BOTH VALUES EQUAL   
           ivtnum = 14                                                  
        ribvs = 6.5                                                     
        ridvs = 6.5                                                     
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs - 6.4996) 20140, 10140, 40140                      
40140 if (riavs - 6.5004) 10140, 10140, 20140                      
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = 6.5                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0141      continue                                                     
! T015*  TEST 15                                      VALUES NOT EQUAL   
           ivtnum = 15                                                  
        ribvs = 7.125                                                   
        ridvs = 5.125                                                   
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs - 5.1247) 20150, 10150, 40150                      
40150 if (riavs - 5.1253) 10150, 10150, 20150                      
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = 5.125                                               
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0151      continue                                                     
! T016*  TEST 16                     FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 16                                                  
        ribvs = -5.625                                                  
        ridvs = 0.0                                                     
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs + 5.6253) 20160, 10160, 40160                      
40160 if (riavs + 5.6247) 10160, 10160, 20160                      
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = -5.625                                              
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0161      continue                                                     
! T017*  TEST 17                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 17                                                  
        ribvs = -6.5                                                    
        ridvs = -6.5                                                    
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs + 6.5004) 20170, 10170, 40170                      
40170 if (riavs + 6.4996) 10170, 10170, 20170                      
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           rvcorr = -6.5                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0171      continue                                                     
! T018*  TEST 18                       VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 18                                                  
        ribvs = -7.125                                                  
        ridvs = -5.125                                                  
        riavs = amin1(ribvs, ridvs)                                     
           if (riavs + 7.1254) 20180, 10180, 40180                      
40180 if (riavs + 7.1246) 10180, 10180, 20180                      
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = -7.125                                              
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0181      continue                                                     
! T019*  TEST 19 FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 19                                                  
        ridvs = 5.625                                                   
        rievs = 0.0                                                     
        riavs = amin1(ridvs, -rievs)                                    
           if (riavs + 0.00005) 20190, 10190, 40190                     
40190 if (riavs - 0.00005) 10190, 10190, 20190                     
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0191      continue                                                     
! T020*  TEST 20                                EXPRESSION AS ARGUMENT   
           ivtnum = 20                                                  
        ridvs = 3.5                                                     
        rievs = 4.0                                                     
        riavs = amin1(ridvs + rievs, -rievs - ridvs)                    
           if (riavs + 7.5004) 20200, 10200, 40200                      
40200 if (riavs + 7.4996) 10200, 10200, 20200                      
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           rvcorr = -7.5                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0201      continue                                                     
! T021*  TEST 21                                           3 ARGUMENTS   
           ivtnum = 21                                                  
        ribvs = 0.0                                                     
        ricvs = 1.0                                                     
        ridvs = 10.9                                                    
        riavs = amin1(ridvs, ricvs, ribvs)                              
           if (riavs + 0.00005) 20210, 10210, 40210                     
40210 if (riavs - 0.00005) 10210, 10210, 20210                     
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0211      continue                                                     
! T022*  TEST 22                                           4 ARGUMENTS   
           ivtnum = 22                                                  
        ribvs = -9.0                                                    
        ricvs = 10.0                                                    
        ridvs = 3.5                                                     
        riavs = amin1(ridvs, ricvs, -ribvs, ridvs)                      
           if (riavs - 3.4998) 20220, 10220, 40220                      
40220 if (riavs - 3.5002) 10220, 10220, 20220                      
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           rvcorr = 3.5                                                 
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0221      continue                                                     
! T023*  TEST 23                                           5 ARGUMENTS   
           ivtnum = 23                                                  
        ridvs = 3.5                                                     
        rievs = 4.5                                                     
        riavs = amin1(ridvs, -ridvs, -rievs, +ridvs, rievs)             
           if (riavs + 4.5003) 20230, 10230, 40230                      
40230 if (riavs + 4.4997) 10230, 10230, 20230                      
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           rvcorr = -4.5                                                
           write (nuvi, 80012) ivtnum, riavs, rvcorr                    
 0231      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****    TEST OF MIN0                                                  
! *****                                                                  
        write(nuvi, 16705)                                              
16705 format (/ 8x, "TEST OF MIN0" )                                  
! T024*  TEST 24                                      BOTH VALUES ZERO   
           ivtnum = 24                                                  
        iibvi = 0                                                       
        iidvi = 0                                                       
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi - 0) 20240, 10240, 20240                           
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0241      continue                                                     
! T025*  TEST 25                     FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 25                                                  
        iibvi = 6                                                       
        iidvi = 0                                                       
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi - 0) 20250, 10250, 20250                           
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0251      continue                                                     
! T026*  TEST 26                                     BOTH VALUES EQUAL   
           ivtnum = 26                                                  
        iibvi = 7                                                       
        iidvi = 7                                                       
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi - 7) 20260, 10260, 20260                           
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           ivcorr = 7                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0261      continue                                                     
! T027*  TEST 27                                      VALUES NOT EQUAL   
           ivtnum = 27                                                  
        iibvi = 7                                                       
        iidvi = 5                                                       
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi - 5) 20270, 10270, 20270                           
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0271      continue                                                     
! T028*  TEST 28                     FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 28                                                  
        iibvi = -6                                                      
        iidvi = 0                                                       
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi + 6) 20280, 10280, 20280                           
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           ivcorr = -6                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0281      continue                                                     
! T029*  TEST 29                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 29                                                  
        iibvi = -7                                                      
        iidvi = -7                                                      
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi + 7) 20290, 10290, 20290                           
10290 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           ivcorr = -7                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0291      continue                                                     
! T030*  TEST 30                       VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 30                                                  
        iibvi = -7                                                      
        iidvi = -5                                                      
        iiavi = min0(iibvi, iidvi)                                      
           if (iiavi + 7) 20300, 10300, 20300                           
10300 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           ivcorr = -7                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0301      continue                                                     
! T031*  TEST 31 FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 31                                                  
        iidvi = 6                                                       
        iievi = 0                                                       
        iiavi = min0(iidvi, -iievi)                                     
           if (iiavi - 0) 20310, 10310, 20310                           
10310 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0311      continue                                                     
! T032*  TEST 32                      EXPRESSION PRESENTED TO FUNCTION   
           ivtnum = 32                                                  
        iidvi = 3                                                       
        iievi = 4                                                       
        iiavi = min0(iidvi + iievi, -iievi - iidvi)                     
           if (iiavi + 7) 20320, 10320, 20320                           
10320 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           ivcorr = -7                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0321      continue                                                     
! T033*  TEST 33                                           3 ARGUMENTS   
           ivtnum = 33                                                  
        iibvi = 0                                                       
        iicvi = 10                                                      
        iidvi = -11                                                     
        iiavi = min0(iicvi, iibvi, -iidvi)                              
           if (iiavi - 0) 20330, 10330, 20330                           
10330 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0331      continue                                                     
! T034*  TEST 34                                           4 ARGUMENTS   
           ivtnum = 34                                                  
        iiavi = 10                                                      
        iibvi = -4                                                      
        iicvi = 8                                                       
        iidvi = 4                                                       
        iiavi = min0(iiavi, -iibvi, iicvi, iidvi)                       
           if (iiavi - 4) 20340, 10340, 20340                           
10340 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           ivcorr = 4                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0341      continue                                                     
! T035*  TEST 35                                           5 ARGUMENTS   
           ivtnum = 35                                                  
        iidvi = 4                                                       
        iievi = 5                                                       
        iiavi = min0(iidvi, -iidvi, -iievi, +iidvi, iievi)              
           if (iiavi + 5) 20350, 10350, 20350                           
10350 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0351      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****    TEST OF MIN1                                                  
! *****                                                                  
        write(nuvi, 16707)                                              
16707 format (/ 8x, "TEST OF MIN1" )                                  
! T036*  TEST 36                                      BOTH VALUES ZERO   
           ivtnum = 36                                                  
        ribvs = 0.0                                                     
        ridvs = 0.0                                                     
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi - 0) 20360, 10360, 20360                           
10360 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0361                                                   
20360 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0361      continue                                                     
! T037*  TEST 37                     FIRST VALUE NON-ZERO, SECOND ZERO   
           ivtnum = 37                                                  
        ribvs = 5.625                                                   
        ridvs = 0.0                                                     
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi - 0) 20370, 10370, 20370                           
10370 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0371                                                   
20370 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0371      continue                                                     
! T038*  TEST 38                                     BOTH VALUES EQUAL   
           ivtnum = 38                                                  
        ribvs = 6.5                                                     
        ridvs = 6.5                                                     
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi - 6) 20380, 10380, 20380                           
10380 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0381                                                   
20380 ivfail = ivfail + 1                                          
           ivcorr = 6                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0381      continue                                                     
! T039*  TEST 39                                      VALUES NOT EQUAL   
           ivtnum = 39                                                  
        ribvs = 7.125                                                   
        ridvs = 5.125                                                   
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi - 5) 20390, 10390, 20390                           
10390 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0391                                                   
20390 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0391      continue                                                     
! T040*  TEST 40                     FIRST VALUE NEGATIVE, SECOND ZERO   
           ivtnum = 40                                                  
        ribvs = -5.625                                                  
        ridvs = 0.0                                                     
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi + 5) 20400, 10400, 20400                           
10400 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0401                                                   
20400 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0401      continue                                                     
! T041*  TEST 41                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 41                                                  
        ribvs = -6.5                                                    
        ridvs = -6.5                                                    
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi + 6) 20410, 10410, 20410                           
10410 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0411                                                   
20410 ivfail = ivfail + 1                                          
           ivcorr = -6                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0411      continue                                                     
! T042*  TEST 42                       VALUES NOT EQUAL, BOTH NEGATIVE   
           ivtnum = 42                                                  
        ribvs = -7.125                                                  
        ridvs = -5.125                                                  
        iiavi = min1(ribvs, ridvs)                                      
           if (iiavi + 7) 20420, 10420, 20420                           
10420 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0421                                                   
20420 ivfail = ivfail + 1                                          
           ivcorr = -7                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0421      continue                                                     
! T043*  TEST 43 FIRST VALUE NON-ZERO, 2ND ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 43                                                  
        ridvs = 5.625                                                   
        rievs = 0.0                                                     
        iiavi = min1(ridvs, -rievs)                                     
           if (iiavi - 0) 20430, 10430, 20430                           
10430 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0431                                                   
20430 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0431      continue                                                     
! T044*  TEST 44                      EXPRESSION PRESENTED TO FUNCTION   
           ivtnum = 44                                                  
        ridvs = 3.5                                                     
        rievs = 4.0                                                     
        iiavi = min1(ridvs + rievs, -rievs - ridvs)                     
           if (iiavi + 7) 20440, 10440, 20440                           
10440 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0441                                                   
20440 ivfail = ivfail + 1                                          
           ivcorr = -7                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0441      continue                                                     
! T045*  TEST 45                                           3 ARGUMENTS   
           ivtnum = 45                                                  
        ribvs = 0.0                                                     
        ricvs = 1.0                                                     
        ridvs = 2.0                                                     
        iiavi = min1(ribvs, ricvs, ridvs)                               
           if (iiavi - 0) 20450, 10450, 20450                           
10450 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0451                                                   
20450 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0451      continue                                                     
! T046*  TEST 46                                           4 ARGUMENTS   
           ivtnum = 46                                                  
        riavs = -3.5                                                    
        ribvs = 12.0                                                    
        ricvs = 3.6                                                     
        ridvs = 3.5                                                     
        iiavi = min1(-riavs, ribvs, ricvs, ridvs)                       
           if (iiavi - 3) 20460, 10460, 20460                           
10460 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0461                                                   
20460 ivfail = ivfail + 1                                          
           ivcorr = 3                                                   
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0461      continue                                                     
! T047*  TEST 47                                           5 ARGUMENTS   
           ivtnum = 47                                                  
        ridvs = 3.5                                                     
        rievs = 4.5                                                     
        iiavi = min1(ridvs, -ridvs, -rievs, +ridvs, rievs)              
           if (iiavi + 4) 20470, 10470, 20470                           
10470 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0471                                                   
20470 ivfail = ivfail + 1                                          
           ivcorr = -4                                                  
           write (nuvi, 80010) ivtnum, iiavi, ivcorr                    
 0471      continue                                                     
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
! *****    END OF TEST SEGMENT 167                                       
        stop                                                            
        end program fm362
