      program fm820
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM820                                                          
! *****                       YCSIN - (188)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTION CSIN                           15.3   
! *****    INTRINSIC FUNCTION CABS ASSUMED WORKING               TABLE 5 
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
! *****    S P E C I F I C A T I O N S SEGMENT 188                       
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
      real :: dvs
      real :: rvcorr
        complex :: avc
        complex :: bvc
        complex :: zvcorr
        real, dimension(1:2) :: r2e
        equivalence (avc, r2e)                                          
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
      ivtotl = 18                                                       
      zprog = 'FM820'                                                   
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
! *****    HEADER FOR SEGMENT 188                                        
        write(nuvi,18800)                                               
18800 format(" "/"  YCSIN - (188) INTRINSIC FUNCTIONS" //                      "  CSIN, CCOS  (COMPLEX SINE, COSINE)" //                         "  ANS REF. - 15.3" )                                    
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
        write(nuvi, 18801)                                              
18801 format(/ 8x, "TEST OF CSIN" )                                   
! *****                                                                  
! T001*  TEST 1                                TEST AT ZERO (0.0, 0.0)   
           ivtnum = 1                                                   
        avc = csin(( 0.0, 0.0))                                         
           if (r2e(1) + 0.50000e-04) 20010, 40012, 40011                
40011 if (r2e(1) - 0.50000e-04) 40012, 40012, 20010                
40012 if (r2e(2) + 0.50000e-04) 20010, 10010, 40010                
40010 if (r2e(2) - 0.50000e-04) 10010, 10010, 20010                
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           zvcorr = (0.00000000000000, 0.00000000000000)                
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0011      continue                                                     
! T002*  TEST 2            TEST SIN ON THE REAL LINE, CSIN SAME AS SIN   
           ivtnum = 2                                                   
        avc = csin(( 2.0, 0.0))                                         
           if (r2e(1) - 0.90925e+00) 20020, 40022, 40021                
40021 if (r2e(1) - 0.90935e+00) 40022, 40022, 20020                
40022 if (r2e(2) + 0.50000e-04) 20020, 10020, 40020                
40020 if (r2e(2) - 0.50000e-04) 10020, 10020, 20020                
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           zvcorr = (0.90929742682568, 0.00000000000000)                
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0021      continue                                                     
! T003*  TEST 3            TEST SIN ON THE REAL LINE, CSIN SAME AS SIN   
           ivtnum = 3                                                   
        avc = csin(( -1000.0, 0.0))                                     
           if (r2e(1) + 0.82692e+00) 20030, 40032, 40031                
40031 if (r2e(1) + 0.82683e+00) 40032, 40032, 20030                
40032 if (r2e(2) + 0.50000e-04) 20030, 10030, 40030                
40030 if (r2e(2) - 0.50000e-04) 10030, 10030, 20030                
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           zvcorr = (-0.82687954053200, 0.00000000000000)               
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0031      continue                                                     
! T004*  TEST 4                           EXPRESSION PRESENTED TO CSIN   
           ivtnum = 4                                                   
        avc = csin(( 150.0, 350.0) / (100.0, 0.0))                      
           if (r2e(1) - 0.16530e+02) 20040, 40042, 40041                
40041 if (r2e(1) - 0.16533e+02) 40042, 40042, 20040                
40042 if (r2e(2) - 0.11701e+01) 20040, 10040, 40040                
40040 if (r2e(2) - 0.11703e+01) 10040, 10040, 20040                
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           zvcorr = (16.531309523248, 1.1701791625591)                  
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0041      continue                                                     
! T005*  TEST 5                             VARIABLE PRESENTED TO CSIN   
           ivtnum = 5                                                   
        bvc = ( 4.75, 2.50) - (9.50, 1.25)                              
        avc = csin(bvc)                                                 
           if (r2e(1) - 0.18870e+01) 20050, 40052, 40051                
40051 if (r2e(1) - 0.18872e+01) 40052, 40052, 20050                
40052 if (r2e(2) - 0.60232e-01) 20050, 10050, 40050                
40050 if (r2e(2) - 0.60239e-01) 10050, 10050, 20050                
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           zvcorr = (1.8870883629759, 0.060235606171638)                
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0051      continue                                                     
! T006*  TEST 6                             VARIABLE PRESENTED TO CSIN   
           ivtnum = 6                                                   
        bvc = ( 0.125, 2.0) * (10.0, 0.0)                               
        avc = csin(bvc)                                                 
           if (r2e(1) - 0.23019e+09) 20060, 40062, 40061                
40061 if (r2e(1) - 0.23022e+09) 40062, 40062, 20060                
40062 if (r2e(2) - 0.76487e+08) 20060, 10060, 40060                
40060 if (r2e(2) - 0.76496e+08) 10060, 10060, 20060                
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           zvcorr = (230207154.14527, 76491717.784289)                  
           write (nuvi, 80145) ivtnum, avc, zvcorr                      
80145 format (" ",2x,i3,4x," FAIL  ",/," ",16x,"COMPUTED: " ,                   "(",e12.5,", ",e12.5,")"/," ",16x,"CORRECT:  " ,                  "(",e12.5,", ",e12.5,")")                                 
 0061      continue                                                     
! T007*  TEST 7                                TEST WHERE REAL IS ZERO   
           ivtnum = 7                                                   
        bvc = ( 0.0, 1.0)                                               
        avc = csin(bvc)                                                 
           if (r2e(1) + 0.50000e-04) 20070, 40072, 40071                
40071 if (r2e(1) - 0.50000e-04) 40072, 40072, 20070                
40072 if (r2e(2) - 0.11751e+01) 20070, 10070, 40070                
40070 if (r2e(2) - 0.11753e+01) 10070, 10070, 20070                
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           zvcorr = (0.00000000000000, 1.1752011936438)                 
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0071      continue                                                     
! T008*  TEST 8                                TEST WHERE REAL IS ZERO   
           ivtnum = 8                                                   
        bvc = ( 0.0, -4.75)                                             
        avc = csin(bvc)                                                 
           if (r2e(1) + 0.50000e-04) 20080, 40082, 40081                
40081 if (r2e(1) - 0.50000e-04) 40082, 40082, 20080                
40082 if (r2e(2) + 0.57791e+02) 20080, 10080, 40080                
40080 if (r2e(2) + 0.57785e+02) 10080, 10080, 20080                
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           zvcorr = (0.00000000000000, -57.787816415992)                
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                TEST WHERE REAL IS ZERO   
           ivtnum = 9                                                   
        avc = csin(( 0.0, -10.0))                                       
           if (r2e(1) + 0.50000e-04) 20090, 40092, 40091                
40091 if (r2e(1) - 0.50000e-04) 40092, 40092, 20090                
40092 if (r2e(2) + 0.11014e+05) 20090, 10090, 40090                
40090 if (r2e(2) + 0.11012e+05) 10090, 10090, 20090                
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           zvcorr = (0.00000000000000, -11013.232874703)                
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0091      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
        write(nuvi, 18811)                                              
18811 format(/ 08x, "TEST OF CCOS" )                                  
! T010*  TEST 10                              TEST FOR ZERO (0.0, 0.0)   
           ivtnum = 10                                                  
        avc = ccos(( 0.0, 0.0))                                         
           if (r2e(1) - 0.99995e+00) 20100, 40102, 40101                
40101 if (r2e(1) - 0.10001e+01) 40102, 40102, 20100                
40102 if (r2e(2) + 0.50000e-04) 20100, 10100, 40100                
40100 if (r2e(2) - 0.50000e-04) 10100, 10100, 20100                
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           zvcorr = (1.00000000000000, 0.00000000000000)                
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0101      continue                                                     
! T011*  TEST 11                 TEST WITH ZERO IMAGINARY,  CCOS = COS   
           ivtnum = 11                                                  
        avc = ccos((3.5, 1.0) - (0.0, 1.0))                             
           if (r2e(1) + 0.93651e+00) 20110, 40112, 40111                
40111 if (r2e(1) + 0.93641e+00) 40112, 40112, 20110                
40112 if (r2e(2) + 0.50000e-04) 20110, 10110, 40110                
40110 if (r2e(2) - 0.50000e-04) 10110, 10110, 20110                
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           zvcorr = (-0.93645668729080, 0.00000000000000)               
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0111      continue                                                     
! T013*  TEST 12                          EXPRESSION PRESENTED TO CCOS   
           ivtnum = 12                                                  
        avc = ccos(( 3.5, 5.5) - (2.0, 2.0))                            
           if (r2e(1) - 0.11722e+01) 20130, 40132, 40131                
40131 if (r2e(1) - 0.11724e+01) 40132, 40132, 20130                
40132 if (r2e(2) + 0.16502e+02) 20130, 10130, 40130                
40130 if (r2e(2) + 0.16500e+02) 10130, 10130, 20130                
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           zvcorr = (1.1723152409601, -16.501187784675)                 
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0131      continue                                                     
! T014*  TEST 13                         VARIABLE WITHIN AN EXPRESSION   
           ivtnum = 13                                                  
        bvc = ( 4.75, 1.25)                                             
        avc = ccos(bvc - (9.50, 0.0))                                   
           if (r2e(1) - 0.71005e-01) 20140, 40142, 40141                
40141 if (r2e(1) - 0.71013e-01) 40142, 40142, 20140                
40142 if (r2e(2) + 0.16009e+01) 20140, 10140, 40140                
40140 if (r2e(2) + 0.16007e+01) 10140, 10140, 20140                
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           zvcorr = (0.071008803346314, -1.6007861854666)               
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0141      continue                                                     
! T015*  TEST 14                         VARIABLE WITHIN AN EXPRESSION   
           ivtnum = 14                                                  
        bvc = ( 1.00, 10.0)                                             
        avc = ccos(bvc + ( 0.25, 10.0))                                 
           if (r2e(1) - 0.76487e+08) 20150, 40152, 40151                
40151 if (r2e(1) - 0.76496e+08) 40152, 40152, 20150                
40152 if (r2e(2) + 0.23022e+09) 20150, 10150, 40150                
40150 if (r2e(2) + 0.23019e+09) 10150, 10150, 20150                
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           zvcorr = (76491717.784289, -230207154.14527)                 
           write (nuvi, 80145) ivtnum, avc, zvcorr                      
 0151      continue                                                     
! T016*  TEST 15                              TEST WITH ZERO REAL PART   
           ivtnum = 15                                                  
        bvc = ( 0.0, 1.0)                                               
        avc = ccos(bvc)                                                 
           if (r2e(1) - 0.15430e+01) 20160, 40162, 40161                
40161 if (r2e(1) - 0.15432e+01) 40162, 40162, 20160                
40162 if (r2e(2) + 0.50000e-04) 20160, 10160, 40160                
40160 if (r2e(2) - 0.50000e-04) 10160, 10160, 20160                
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           zvcorr = (1.5430806348152, 0.00000000000000)                 
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0161      continue                                                     
! T017*  TEST 16                              TEST WITH ZERO REAL PART   
           ivtnum = 16                                                  
        bvc = ( 0.0, -4.75)                                             
        avc = ccos(bvc)                                                 
           if (r2e(1) - 0.57793e+02) 20170, 40172, 40171                
40171 if (r2e(1) - 0.57800e+02) 40172, 40172, 20170                
40172 if (r2e(2) + 0.50000e-04) 20170, 10170, 40170                
40170 if (r2e(2) - 0.50000e-04) 10170, 10170, 20170                
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           zvcorr = (57.796468111195, 0.00000000000000)                 
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0171      continue                                                     
! T018*  TEST 17                              TEST WITH ZERO REAL PART   
           ivtnum = 17                                                  
        avc = ccos(( 0.0, -10.0))                                       
           if (r2e(1) - 0.11012e+05) 20180, 40182, 40181                
40181 if (r2e(1) - 0.11014e+05) 40182, 40182, 20180                
40182 if (r2e(2) + 0.50000e-04) 20180, 10180, 40180                
40180 if (r2e(2) - 0.50000e-04) 10180, 10180, 20180                
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           zvcorr = (11013.232920103, 0.00000000000000)                 
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0181      continue                                                     
! T019*  TEST 18              THE FUNCTION TOGETHER WITH CSIN AND CABS   
           ivtnum = 18                                                  
        dvs = (cabs(ccos((-2.25, 0.0))) ** 2) +                                 (cabs(csin((-2.25, 0.0))) ** 2)                           
           if (dvs - 0.99995e+00) 20190, 10190, 40190                   
40190 if (dvs - 0.10001e+01) 10190, 10190, 20190                   
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           rvcorr = 1.00000000000000                                    
           write (nuvi, 80012) ivtnum, dvs, rvcorr                      
 0191      continue                                                     
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
! *****  END OF TEST SEGMENT 188                                         
      stop                                                              
      end program fm820
