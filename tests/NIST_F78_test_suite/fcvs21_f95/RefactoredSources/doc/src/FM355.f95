      program fm355
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM355               XAINT - (154)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                       SUBSET REF
! *****    TEST INTRINSIC FUNCTIONS AINT, ANINT, NINT             15.3   
! *****    TRUNCATION (SIGN OF A * LARGEST INTEGER LE ABS(A) )  (TABLE 5)
! *****                                                                  
! *****   GENERAL COMMENTS                                               
! *****         FLOAT FUNCTION ASSUMED WORKING                           
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
      real :: rcbvs
      real :: rcavs
      real :: rvcorr
      real :: rcdvs
      integer :: icavi
      integer :: ivcorr
      real :: rcevs
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
      zprog = 'FM355'                                                   
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
! *****    HEADER FOR SEGMENT 154                                        
        write (nuvi,15401)                                              
15401 format (" ", //  2x,"XAINT - (154) INTRINSIC FUNCTIONS--" //10x,          "AINT, ANINT, NINT (TYPE CONVERSION) "  //                        "  SUBSET REF. - 15.3" )                                
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! *****                                                                  
! *****    TEST OF AINT                                                  
! *****                                                                  
        write(nuvi, 15402)                                              
15402 format (/ 8x, "TEST OF AINT" )                                  
! T001*  TEST 1                                           THE VALUE ZERO 
           ivtnum = 1                                                   
        rcbvs = 0.0                                                     
        rcavs = aint(rcbvs)                                             
           if (rcavs + 0.00005) 20010, 10010, 40010                     
40010 if (rcavs - 0.00005) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0011      continue                                                     
! T002*  TEST 2                          ZERO PREFIXED WITH A MINUS SIGN 
           ivtnum = 2                                                   
        rcdvs = -0.0                                                    
        rcavs = aint(rcbvs)                                             
           if (rcavs + 0.00005) 20020, 10020, 40020                     
40020 if (rcavs - 0.00005) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = -0.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0021      continue                                                     
! T003*  TEST 3                                         A VALUE IN (0,1) 
           ivtnum = 3                                                   
        rcdvs = 0.375                                                   
        rcavs = aint(rcbvs)                                             
           if (rcavs + 0.00005) 20030, 10030, 40030                     
40030 if (rcavs - 0.00005) 10030, 10030, 20030                     
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0031      continue                                                     
! T004*  TEST 4                                              THE VALUE 1 
           ivtnum = 4                                                   
        rcbvs = float(1)                                                
        rcavs = aint(rcbvs)                                             
           if (rcavs - 0.99995) 20040, 10040, 40040                     
40040 if (rcavs - 1.0001) 10040, 10040, 20040                      
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0041      continue                                                     
! T005*  TEST 5                        AN INTEGRAL VALUE OTHER THAN 0, 1 
           ivtnum = 5                                                   
        rcbvs = float(6)                                                
        rcavs = aint(rcbvs)                                             
           if (rcavs - 5.9997) 20050, 10050, 40050                      
40050 if (rcavs - 6.0003) 10050, 10050, 20050                      
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 6.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0051      continue                                                     
! T006*  TEST 6                                       A VALUE IN (X,X+1) 
           ivtnum = 6                                                   
        rcbvs = 3.75                                                    
        rcavs = aint(rcbvs)                                             
           if (rcavs - 2.9998) 20060, 10060, 40060                      
40060 if (rcavs - 3.0002) 10060, 10060, 20060                      
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = 3.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0061      continue                                                     
! T007*  TEST 7                 A NEGATIVE VALUE WITH MAGNITUDE IN (0,1) 
           ivtnum = 7                                                   
        rcbvs = -0.375                                                  
        rcavs = aint(rcbvs)                                             
           if (rcavs + 0.00005) 20070, 10070, 40070                     
40070 if (rcavs - 0.00005) 10070, 10070, 20070                     
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0071      continue                                                     
! T008*  TEST 8                                             THE VALUE -1 
           ivtnum = 8                                                   
        rcbvs = float(-1)                                               
        rcavs = aint(rcbvs)                                             
           if (rcavs + 1.0001) 20080, 10080, 40080                      
40080 if (rcavs + 0.99995) 10080, 10080, 20080                     
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = -1.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0081      continue                                                     
! T009*  TEST 9                                A NEGATIVE INTEGRAL VALUE 
           ivtnum = 9                                                   
        rcbvs = float(-6)                                               
        rcavs = aint(rcbvs)                                             
           if (rcavs + 6.0003) 20090, 10090, 40090                      
40090 if (rcavs + 5.9997) 10090, 10090, 20090                      
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = -6.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0091      continue                                                     
! T010*  TEST 10              A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1) 
           ivtnum = 10                                                  
        rcbvs = -3.75                                                   
        rcavs = aint(rcbvs)                                             
           if (rcavs + 3.0002) 20100, 10100, 40100                      
40100 if (rcavs + 2.9998) 10100, 10100, 20100                      
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = -3.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0101      continue                                                     
! T011*  TEST 11              AN ARITHMETIC EXPRESSION PRESENTED TO AINT 
           ivtnum = 11                                                  
        rcbvs = 3.25                                                    
        rcdvs = 3.0                                                     
        rcavs = aint(float(25) + rcdvs * rcbvs)                         
           if (rcavs - 33.998) 20110, 10110, 40110                      
40110 if (rcavs - 34.002) 10110, 10110, 20110                      
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 34.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0111      continue                                                     
! T012*  TEST 12                            AN ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 12                                                  
        rcbvs = 3.7521e-36                                              
        rcavs = aint(rcbvs)                                             
           if (rcavs + 0.00005) 20120, 10120, 40120                     
40120 if (rcavs - 0.00005) 10120, 10120, 20120                     
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0121      continue                                                     
! *****                                                                  
        write(nuvi, 90002)                                              
        write(nuvi, 90013)                                              
        write(nuvi, 90014)                                              
! *****                                                                  
! *****    TEST OF ANINT                                                 
! *****                                                                  
        write(nuvi, 15404)                                              
15404 format (/ 08x, "TEST OF ANINT" )                                
! *****                                                                  
! T013*  TEST 13                                          THE VALUE ZERO 
           ivtnum = 13                                                  
        rcbvs = 0.0                                                     
        rcavs = anint(rcbvs)                                            
           if (rcavs + 0.00005) 20130, 10130, 40130                     
40130 if (rcavs - 0.00005) 10130, 10130, 20130                     
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0131      continue                                                     
! T014*  TEST 14               THE VALUE ZERO PREFIXED WITH A MINUS SIGN 
           ivtnum = 14                                                  
        rcdvs = 0.0                                                     
        rcavs = anint(-rcbvs)                                           
           if (rcavs + 0.00005) 20140, 10140, 40140                     
40140 if (rcavs - 0.00005) 10140, 10140, 20140                     
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0141      continue                                                     
! T015*  TEST 15                                       A VALUE IN (0,.5) 
           ivtnum = 15                                                  
        rcbvs = 0.25                                                    
        rcavs = anint(rcbvs)                                            
           if (rcavs + 0.00005) 20150, 10150, 40150                     
40150 if (rcavs - 0.00005) 10150, 10150, 20150                     
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0151      continue                                                     
! T016*  TEST 16                                           THE VALUE 0.5 
           ivtnum = 16                                                  
        rcbvs = float(1) / float(2)                                     
        rcavs = anint(rcbvs)                                            
           if (rcavs - 0.99995) 20160, 10160, 40160                     
40160 if (rcavs - 1.0001) 10160, 10160, 20160                      
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0161      continue                                                     
! T017*  TEST 17                                       A VALUE IN (.5,1) 
           ivtnum = 17                                                  
        rcbvs = 0.75                                                    
        rcavs = anint(rcbvs)                                            
           if (rcavs - 0.99995) 20170, 10170, 40170                     
40170 if (rcavs - 1.0001) 10170, 10170, 20170                      
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0171      continue                                                     
! T018*  TEST 18                        AN INTEGRAL VALUE OTHER THAN 0,1 
           ivtnum = 18                                                  
        rcbvs = float(5)                                                
        rcavs = anint(rcbvs)                                            
           if (rcavs - 4.9997) 20180, 10180, 40180                      
40180 if (rcavs - 5.0003) 10180, 10180, 20180                      
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = 5.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0181      continue                                                     
! T019*  TEST 19                                     A VALUE IN (X,X+.5) 
           ivtnum = 19                                                  
        rcbvs = 10.46875                                                
        rcavs = anint(rcbvs)                                            
           if (rcavs - 9.9995) 20190, 10190, 40190                      
40190 if (rcavs - 10.001) 10190, 10190, 20190                      
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           rvcorr = 10.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0191      continue                                                     
! T020*  TEST 20                     A VALUE WITH FRACTIONAL PART OF 0.5 
           ivtnum = 20                                                  
        rcbvs = float(16) - float(1) / float(2)                         
        rcavs = anint(rcbvs)                                            
           if (rcavs - 15.999) 20200, 10200, 40200                      
40200 if (rcavs - 16.001) 10200, 10200, 20200                      
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           rvcorr = 16.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0201      continue                                                     
! T021*  TEST 21                                   A VALUE IN (X+.5,X+1) 
           ivtnum = 21                                                  
        rcbvs = 27.96875                                                
        rcavs = anint(rcbvs)                                            
           if (rcavs - 27.998) 20210, 10210, 40210                      
40210 if (rcavs - 28.002) 10210, 10210, 20210                      
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           rvcorr = 28.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0211      continue                                                     
! T022*  TEST 22               A NEGATIVE VALUE WITH MAGNITUDE IN (0,.5) 
           ivtnum = 22                                                  
        rcbvs = -0.25                                                   
        rcavs = anint(rcbvs)                                            
           if (rcavs + 0.00005) 20220, 10220, 40220                     
40220 if (rcavs - 0.00005) 10220, 10220, 20220                     
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           rvcorr = -0.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0221      continue                                                     
! T023*  TEST 23                                          THE VALUE -0.5 
           ivtnum = 23                                                  
        rcbvs = float(-1) / float(2)                                    
        rcavs = anint(rcbvs)                                            
           if (rcavs + 1.0001) 20230, 10230, 40230                      
40230 if (rcavs + 0.99995) 10230, 10230, 20230                     
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           rvcorr = -1.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0231      continue                                                     
! T024*  TEST 24               A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1) 
           ivtnum = 24                                                  
        rcbvs = -0.75                                                   
        rcavs = anint(rcbvs)                                            
           if (rcavs + 1.0001) 20240, 10240, 40240                      
40240 if (rcavs + 0.99995) 10240, 10240, 20240                     
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           rvcorr = -1.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0241      continue                                                     
! T025*  TEST 25                               A NEGATIVE INTEGRAL VALUE 
           ivtnum = 25                                                  
        rcbvs = float(-5)                                               
        rcavs = anint(rcbvs)                                            
           if (rcavs + 5.0003) 20250, 10250, 40250                      
40250 if (rcavs + 4.9997) 10250, 10250, 20250                      
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           rvcorr = -5.0                                                
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0251      continue                                                     
! T026*  TEST 26             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5) 
           ivtnum = 26                                                  
        rcbvs = -10.46875                                               
        rcavs = anint(rcbvs)                                            
           if (rcavs + 10.001) 20260, 10260, 40260                      
40260 if (rcavs + 9.9995) 10260, 10260, 20260                      
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           rvcorr = -10.0                                               
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0261      continue                                                     
! T027*  TEST 27           A NEGATIVE VALUE WITH FRACTIONAL COMPONENT .5 
           ivtnum = 27                                                  
        rcbvs = float(-15) - float(1) / float(2)                        
        rcavs = anint(rcbvs)                                            
           if (rcavs + 16.001) 20270, 10270, 40270                      
40270 if (rcavs + 15.999) 10270, 10270, 20270                      
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           rvcorr = -16.0                                               
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0271      continue                                                     
! T028*  TEST 28           A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1) 
           ivtnum = 28                                                  
        rcbvs = -27.96875                                               
        rcavs = anint(rcbvs)                                            
           if (rcavs + 28.002) 20280, 10280, 40280                      
40280 if (rcavs + 27.998) 10280, 10280, 20280                      
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           rvcorr = -28.0                                               
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0281      continue                                                     
! T029*  TEST 29             AN ARITHMETIC EXPRESSION PRESENTED TO ANINT 
           ivtnum = 29                                                  
        rcdvs = 8.00                                                    
        rcbvs = 7.25                                                    
        rcavs = anint(rcdvs - rcbvs)                                    
           if (rcavs - 0.99995) 20290, 10290, 40290                     
40290 if (rcavs - 1.0001) 10290, 10290, 20290                      
10290 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0291      continue                                                     
! T030*  TEST 30                            AN ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 30                                                  
        rcbvs = -5.9876e-35                                             
        rcavs = anint(rcbvs)                                            
           if (rcavs + 0.00005) 20300, 10300, 40300                     
40300 if (rcavs - 0.00005) 10300, 10300, 20300                     
10300 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, rcavs, rvcorr                     
 0301      continue                                                     
! *****                                                                  
        write(nuvi, 90002)                                              
        write(nuvi, 90013)                                              
        write(nuvi, 90014)                                              
! *****                                                                  
! *****    TEST OF NINT                                                  
! *****                                                                  
        write(nuvi, 15405)                                              
15405 format (/ 8x, "TEST OF NINT" )                                  
! *****                                                                  
! T031*  TEST 31                                          THE VALUE ZERO 
           ivtnum = 31                                                  
        rcbvs = 0.0                                                     
        icavi = nint(rcbvs)                                             
           if (icavi - 0) 20310, 10310, 20310                           
10310 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0311      continue                                                     
! T032*  TEST 32                         ZERO PREFIXED WITH A MINUS SIGN 
           ivtnum = 32                                                  
        rcdvs = 0.0                                                     
        icavi = nint(-rcdvs)                                            
           if (icavi - 0) 20320, 10320, 20320                           
10320 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0321      continue                                                     
! T033*  TEST 33                                       A VALUE IN (0,.5) 
           ivtnum = 33                                                  
        rcbvs = 0.25                                                    
        icavi = nint(rcbvs)                                             
           if (icavi - 0) 20330, 10330, 20330                           
10330 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0331      continue                                                     
! T034*  TEST 34                                           THE VALUE 0.5 
           ivtnum = 34                                                  
        rcbvs = float(1) / float(2)                                     
        icavi = nint(rcbvs)                                             
           if (icavi - 1) 20340, 10340, 20340                           
10340 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0341      continue                                                     
! T035*  TEST 35                                       A VALUE IN (.5,1) 
           ivtnum = 35                                                  
        rcbvs = 0.75                                                    
        icavi = nint(rcbvs)                                             
           if (icavi - 1) 20350, 10350, 20350                           
10350 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0351      continue                                                     
! T036*  TEST 36                       AN INTEGRAL VALUE OTHER THAN 0, 1 
           ivtnum = 36                                                  
        rcbvs = float(5)                                                
        icavi = nint(rcbvs)                                             
           if (icavi - 5) 20360, 10360, 20360                           
10360 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0361                                                   
20360 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0361      continue                                                     
! T037*  TEST 37                                     A VALUE IN (X,X+.5) 
           ivtnum = 37                                                  
        rcbvs = 10.46875                                                
        icavi = nint(rcbvs)                                             
           if (icavi - 10) 20370, 10370, 20370                          
10370 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0371                                                   
20370 ivfail = ivfail + 1                                          
           ivcorr = 10                                                  
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0371      continue                                                     
! T038*  TEST 38                     A VALUE WITH FRACTIONAL PART OF 0.5 
           ivtnum = 38                                                  
        rcbvs = float(15) + float(1) / float(2)                         
        icavi = nint(rcbvs)                                             
           if (icavi - 16) 20380, 10380, 20380                          
10380 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0381                                                   
20380 ivfail = ivfail + 1                                          
           ivcorr = 16                                                  
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0381      continue                                                     
! T039*  TEST 39                                   A VALUE IN (X+.5,X+1) 
           ivtnum = 39                                                  
        rcbvs = 27.96875                                                
        icavi = nint(rcbvs)                                             
           if (icavi - 28) 20390, 10390, 20390                          
10390 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0391                                                   
20390 ivfail = ivfail + 1                                          
           ivcorr = 28                                                  
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0391      continue                                                     
! T040*  TEST 40               A NEGATIVE VALUE WITH MAGNITUDE IN (0.,5) 
           ivtnum = 40                                                  
        rcbvs = -0.25                                                   
        icavi = nint(rcbvs)                                             
           if (icavi - 0) 20400, 10400, 20400                           
10400 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0401                                                   
20400 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0401      continue                                                     
! T041*  TEST 41                                          THE VALUE -0.5 
           ivtnum = 41                                                  
        rcbvs = float(-1) / float(2)                                    
        icavi = nint(rcbvs)                                             
           if (icavi + 1) 20410, 10410, 20410                           
10410 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0411                                                   
20410 ivfail = ivfail + 1                                          
           ivcorr = -1                                                  
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0411      continue                                                     
! T042*  TEST 42               A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1) 
           ivtnum = 42                                                  
        rcbvs = -0.75                                                   
        icavi = nint(rcbvs)                                             
           if (icavi + 1) 20420, 10420, 20420                           
10420 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0421                                                   
20420 ivfail = ivfail + 1                                          
           ivcorr = -1                                                  
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0421      continue                                                     
! T043*  TEST 43                               A NEGATIVE INTEGRAL VALUE 
           ivtnum = 43                                                  
        rcbvs = float(-5)                                               
        icavi = nint(rcbvs)                                             
           if (icavi + 5) 20430, 10430, 20430                           
10430 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0431                                                   
20430 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0431      continue                                                     
! T044*  TEST 44             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5) 
           ivtnum = 44                                                  
        rcbvs = -10.46875                                               
        icavi = nint(rcbvs)                                             
           if (icavi + 10) 20440, 10440, 20440                          
10440 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0441                                                   
20440 ivfail = ivfail + 1                                          
           ivcorr = -10                                                 
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0441      continue                                                     
! T045*  TEST 45        A NEGATIVE VALUE WITH FRACTIONAL COMPONENT 0.5 S1
           ivtnum = 45                                                  
        rcbvs = float(-15) - float(1) / float(2)                        
        icavi = nint(rcbvs)                                             
           if (icavi + 16) 20450, 10450, 20450                          
10450 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0451                                                   
20450 ivfail = ivfail + 1                                          
           ivcorr = -16                                                 
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0451      continue                                                     
! T046*  TEST 46         A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1) S1
           ivtnum = 46                                                  
        rcbvs = -27.96875                                               
        icavi = nint(rcbvs)                                             
           if (icavi + 28) 20460, 10460, 20460                          
10460 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0461                                                   
20460 ivfail = ivfail + 1                                          
           ivcorr = -28                                                 
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0461      continue                                                     
! T047*  TEST 47            AN ARITHMETIC EXPRESSION PRESENTED TO NINT S1
           ivtnum = 47                                                  
        rcdvs = 8.00                                                    
        rcevs = 7.25                                                    
        icavi = nint(rcdvs - rcevs)                                     
           if (icavi - 1) 20470, 10470, 20470                           
10470 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0471                                                   
20470 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
 0471      continue                                                     
! T048*  TEST 48                            AN ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 48                                                  
        rcbvs = -5.9876e-33                                             
        icavi = nint(rcbvs)                                             
           if (icavi - 0) 20480, 10480, 20480                           
10480 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0481                                                   
20480 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, icavi, ivcorr                    
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
! *****    END OF TEST SEGMENT 154                                       
        stop                                                            
        end program fm355
