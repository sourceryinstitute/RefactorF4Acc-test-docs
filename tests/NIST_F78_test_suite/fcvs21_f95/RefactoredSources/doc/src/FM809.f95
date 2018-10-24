      program fm809
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM809               YCONJG - (170)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTION CMPLX (CONVERT TO COMPLEX),    15.3   
! *****    AIMAG (IMAGINARY PART), AND CONJG (CONJUGATE)        (TABLE 5)
! *****                                                                  
! *****    S P E C I F I C A T I O N S  SEGMENT 170                      
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
      real :: rwbvs
      real :: rwdvs
      real :: rwavs
      real :: rwcvs
      real :: rvcorr
        complex :: cwavc
        complex :: cwbvc
        complex :: cwdvc
        complex :: cwevc
        complex :: zvcorr
        real, dimension(1:2) :: r2e
        equivalence (cwavc,r2e)                                         
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
      ivtotl = 25                                                       
      zprog = 'FM809'                                                   
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
! *****    HEADER FOR SEGMENT 170 WRITTEN                                
        write (nuvi,17001)                                              
17001 format(" ", //1x,"YCONJG - (170) INTRINSIC FUNCTION--" //17x,            "CMPLX (CONVERT TO COMPLEX)," /17x,                               "AIMAG (IMAG. PART)," /17x,                                       "CONJG (CONJUGATE)" //,2x,                                        "ANS REF. - 15.3" )                                      
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
! *****    TEST OF CMPLX                                                 
! *****                                                                  
        write(nuvi, 17002)                                              
17002 format(/ 8x, "TEST OF CMPLX" )                                  
! T001*  TEST 1                                           PAIR OF ZEROES 
           ivtnum = 1                                                   
        rwbvs = 0.0                                                     
        rwdvs = 0.0                                                     
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) + 0.00005) 20010, 40012, 40011                    
40011 if (r2e(1) - 0.00005) 40012, 40012, 20010                    
40012 if (r2e(2) + 0.00005) 20010, 10010, 40010                    
40010 if (r2e(2) - 0.00005) 10010, 10010, 20010                    
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           zvcorr = (0.0 , 0.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0011      continue                                                     
! T002*  TEST 2                        FIRST VALUE NON-ZERO, SECOND ZERO 
           ivtnum = 2                                                   
        rwbvs = 3.0                                                     
        rwdvs = 0.0                                                     
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) - 2.9998) 20020, 40022, 40021                     
40021 if (r2e(1) - 3.0002) 40022, 40022, 20020                     
40022 if (r2e(2) + 0.00005) 20020, 10020, 40020                    
40020 if (r2e(2) - 0.00005) 10020, 10020, 20020                    
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           zvcorr = (3.0 , 0.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0021      continue                                                     
! T003*  TEST 3                        FIRST VALUE ZERO, SECOND NON-ZERO 
           ivtnum = 3                                                   
        rwbvs = 0.0                                                     
        rwdvs = 4.0                                                     
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) + 0.00005) 20030, 40032, 40031                    
40031 if (r2e(1) - 0.00005) 40032, 40032, 20030                    
40032 if (r2e(2) - 3.9998) 20030, 10030, 40030                     
40030 if (r2e(2) - 4.0002) 10030, 10030, 20030                     
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           zvcorr = (0.0 , 4.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0031      continue                                                     
! T004*  TEST 4                                  PAIR OF NON-ZERO VALUES 
           ivtnum = 4                                                   
        rwbvs = 3.0                                                     
        rwdvs = 4.0                                                     
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) - 2.9998) 20040, 40042, 40041                     
40041 if (r2e(1) - 3.0002) 40042, 40042, 20040                     
40042 if (r2e(2) - 3.9998) 20040, 10040, 40040                     
40040 if (r2e(2) - 4.0002) 10040, 10040, 20040                     
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           zvcorr = (3.0 , 4.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0041      continue                                                     
! T005*  TEST 5                        FIRST VALUE NEGATIVE, SECOND ZERO 
           ivtnum = 5                                                   
        rwbvs = -3.0                                                    
        rwdvs = 0.0                                                     
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) + 3.0002) 20050, 40052, 40051                     
40051 if (r2e(1) + 2.9998) 40052, 40052, 20050                     
40052 if (r2e(2) + 0.00005) 20050, 10050, 40050                    
40050 if (r2e(2) - 0.00005) 10050, 10050, 20050                    
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           zvcorr = (-3.0, 0.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0051      continue                                                     
! T006*  TEST 6                        FIRST VALUE ZERO, SECOND NEGATIVE 
           ivtnum = 6                                                   
        rwbvs = 0.0                                                     
        rwdvs = -4.0                                                    
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) + 0.00005) 20060, 40062, 40061                    
40061 if (r2e(1) - 0.00005) 40062, 40062, 20060                    
40062 if (r2e(2) + 4.0002) 20060, 10060, 40060                     
40060 if (r2e(2) + 3.9998) 10060, 10060, 20060                     
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           zvcorr = (0.0, -4.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0061      continue                                                     
! T007*  TEST 7                                  PAIR OF NEGATIVE VALUES 
           ivtnum = 7                                                   
        rwbvs = -3.0                                                    
        rwdvs = -4.0                                                    
        cwavc = cmplx(rwbvs, rwdvs)                                     
           if (r2e(1) + 3.0002) 20070, 40072, 40071                     
40071 if (r2e(1) + 2.9998) 40072, 40072, 20070                     
40072 if (r2e(2) + 4.0002) 20070, 10070, 40070                     
40070 if (r2e(2) + 3.9998) 10070, 10070, 20070                     
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           zvcorr = (-3.0, -4.0)                                        
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0071      continue                                                     
! T008*  TEST 8                     FIRST VALUE PRECEDED BY A MINUS SIGN 
           ivtnum = 8                                                   
        rwavs = 3.0                                                     
        rwbvs = 0.0                                                     
        cwavc = cmplx(-rwavs, rwbvs)                                    
           if (r2e(1) + 3.0002) 20080, 40082, 40081                     
40081 if (r2e(1) + 2.9998) 40082, 40082, 20080                     
40082 if (r2e(2) + 0.00005) 20080, 10080, 40080                    
40080 if (r2e(2) - 0.00005) 10080, 10080, 20080                    
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           zvcorr = (-3.0, 0.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0081      continue                                                     
! T009*  TEST 9                ONE ARGUMENT A CONSTANT, OTHER A VARIABLE 
           ivtnum = 9                                                   
        rwavs = 4.0                                                     
        cwavc = cmplx(0.0, rwavs)                                       
           if (r2e(1) + 0.00005) 20090, 40092, 40091                    
40091 if (r2e(1) - 0.00005) 40092, 40092, 20090                    
40092 if (r2e(2) - 3.9998) 20090, 10090, 40090                     
40090 if (r2e(2) - 4.0002) 10090, 10090, 20090                     
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           zvcorr = (0.0, 4.0)                                          
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0091      continue                                                     
! T010*  TEST 10         PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT 
           ivtnum = 10                                                  
        rwavs = 1.5                                                     
        rwbvs = 2.0                                                     
        rwcvs = 3.5                                                     
        cwavc = cmplx((rwcvs + rwavs)/ rwbvs, (rwcvs - rwavs) / rwbvs)  
           if (r2e(1) - 2.4998) 20100, 40102, 40101                     
40101 if (r2e(1) - 2.5002) 40102, 40102, 20100                     
40102 if (r2e(2) - 0.99995) 20100, 10100, 40100                    
40100 if (r2e(2) - 1.0001) 10100, 10100, 20100                     
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           zvcorr = (2.5, 1.0)                                          
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0101      continue                                                     
! *****                                                                  
        write(nuvi, 90002)                                              
        write(nuvi, 90013)                                              
        write(nuvi, 90014)                                              
! *****                                                                  
! *****    TEST OF AIMAG                                                 
! *****                                                                  
        write(nuvi, 17004)                                              
17004 format(/ 8x, "TEST OF AIMAG" )                                  
! T011*  TEST 11                            THE COMPLEX VALUE ZERO (0,0) 
           ivtnum = 11                                                  
        rwavs = aimag((0.0, 0.0))                                       
           if (rwavs + 0.00005) 20110, 10110, 40110                     
40110 if (rwavs - 0.00005) 10110, 10110, 20110                     
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0111      continue                                                     
! T012*  TEST 12              COMPLEX VALUE HAVING ONLY A REAL COMPONENT 
           ivtnum = 12                                                  
        rwavs = aimag((3.0, 0.0))                                       
           if (rwavs + 0.00005) 20120, 10120, 40120                     
40120 if (rwavs - 0.00005) 10120, 10120, 20120                     
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0121      continue                                                     
! T013*  TEST 13                                 ARBITRARY COMPLEX VALUE 
           ivtnum = 13                                                  
        rwavs = aimag((3.0, 4.0))                                       
           if (rwavs - 3.9998) 20130, 10130, 40130                      
40130 if (rwavs - 4.0002) 10130, 10130, 20130                      
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = 4.0                                                 
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0131      continue                                                     
! T014*  TEST 14       IMAGINARY COMPONENT A ZERO PRECEDED BY MINUS SIGN 
           ivtnum = 14                                                  
        rwavs = aimag((-3.0, -0.0))                                     
           if (rwavs + 0.00005) 20140, 10140, 40140                     
40140 if (rwavs - 0.00005) 10140, 10140, 20140                     
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0141      continue                                                     
! T015*  TEST 15        ARBITRARY COMPLEX VALUE WITH NEGATIVE COMPONENTS 
           ivtnum = 15                                                  
        rwavs = aimag((-3.0, -4.0))                                     
           if (rwavs + 4.0002) 20150, 10150, 40150                      
40150 if (rwavs + 3.9998) 10150, 10150, 20150                      
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = -4.0                                                
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0151      continue                                                     
! T016*  TEST 16         COMPLEX VALUE ZERO (0,0) PRECEDED BY MINUS SIGN 
           ivtnum = 16                                                  
        cwdvc = (0.0, 0.0)                                              
        rwavs = aimag(-cwdvc)                                           
           if (rwavs + 0.00005) 20160, 10160, 40160                     
40160 if (rwavs - 0.00005) 10160, 10160, 20160                     
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0161      continue                                                     
! T017*  TEST 17                        ARGUMENT IS A COMPLEX EXPRESSION 
           ivtnum = 17                                                  
        cwdvc = (3.5, 4.5)                                              
        cwevc = (4.0, 5.0)                                              
        rwavs = aimag(cwdvc - cwevc)                                    
           if (rwavs + 0.50003) 20170, 10170, 40170                     
40170 if (rwavs + 0.49997) 10170, 10170, 20170                     
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           rvcorr = -0.5                                                
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0171      continue                                                     
! T018*  TEST 18                           CONJG FORMS ARGUMENT TO AIMAG 
           ivtnum = 18                                                  
        cwdvc = (3.0, 4.0)                                              
        rwavs = aimag(conjg(cwdvc))                                     
           if (rwavs + 4.0002) 20180, 10180, 40180                      
40180 if (rwavs + 3.9998) 10180, 10180, 20180                      
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = -4.0                                                
           write (nuvi, 80012) ivtnum, rwavs, rvcorr                    
 0181      continue                                                     
! *****                                                                  
        write(nuvi, 90002)                                              
        write(nuvi, 90013)                                              
        write(nuvi, 90014)                                              
! *****                                                                  
! *****    TEST OF CONJG                                                 
! *****                                                                  
        write (nuvi,17006)                                              
17006 format (/ 8x, "TEST OF CONJG" )                                 
! T019*  TEST 19                                COMPLEX VALUE ZERO (0,0) 
           ivtnum = 19                                                  
        cwavc = conjg((0.0, 0.0))                                       
           if (r2e(1) + 0.00005) 20190, 40192, 40191                    
40191 if (r2e(1) - 0.00005) 40192, 40192, 20190                    
40192 if (r2e(2) + 0.00005) 20190, 10190, 40190                    
40190 if (r2e(2) - 0.00005) 10190, 10190, 20190                    
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           zvcorr = (0.0, 0.0)                                          
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0191      continue                                                     
! T020*  TEST 20                COMPLEX VALUE HAVING ONLY REAL COMPONENT 
           ivtnum = 20                                                  
        cwavc = conjg((3.0, 0.0))                                       
           if (r2e(1) - 2.9998) 20200, 40202, 40201                     
40201 if (r2e(1) - 3.0002) 40202, 40202, 20200                     
40202 if (r2e(2) + 0.00005) 20200, 10200, 40200                    
40200 if (r2e(2) - 0.00005) 10200, 10200, 20200                    
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           zvcorr = (3.0, 0.0)                                          
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0201      continue                                                     
! T021*  TEST 21                                 ARBITRARY COMPLEX VALUE 
           ivtnum = 21                                                  
        cwavc = conjg((3.0, 4.0))                                       
           if (r2e(1) - 2.9998) 20210, 40212, 40211                     
40211 if (r2e(1) - 3.0002) 40212, 40212, 20210                     
40212 if (r2e(2) + 4.0002) 20210, 10210, 40210                     
40210 if (r2e(2) + 3.9998) 10210, 10210, 20210                     
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           zvcorr = (3.0, -4.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0211      continue                                                     
        cwbvc = (3.0, -4.0)                                             
! T022*  TEST 22        SECOND ARGUMENT IS A ZERO PRECEDED BY MINUS SIGN 
           ivtnum = 22                                                  
        cwavc = conjg((-3.0, -0.0))                                     
           if (r2e(1) + 3.0002) 20220, 40222, 40221                     
40221 if (r2e(1) + 2.9998) 40222, 40222, 20220                     
40222 if (r2e(2) + 0.00005) 20220, 10220, 40220                    
40220 if (r2e(2) - 0.00005) 10220, 10220, 20220                    
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           zvcorr = (-3.0, 0.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0221      continue                                                     
! T023*  TEST 23         ABITRARY COMPLEX VALUE WITH NEGATIVE COMPONENTS 
           ivtnum = 23                                                  
        cwavc = conjg((-3.0, -4.0))                                     
           if (r2e(1) + 3.0002) 20230, 40232, 40231                     
40231 if (r2e(1) + 2.9998) 40232, 40232, 20230                     
40232 if (r2e(2) - 3.9998) 20230, 10230, 40230                     
40230 if (r2e(2) - 4.0002) 10230, 10230, 20230                     
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           zvcorr = (-3.0, 4.0)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0231      continue                                                     
        cwbvc = (-3.0, 4.0)                                             
! T024*  TEST 24                   COMPLEX ZERO PRECEDED BY A MINUS SIGN 
           ivtnum = 24                                                  
        cwdvc = (0.0, 0.0)                                              
        cwavc = conjg(-cwdvc)                                           
           if (r2e(1) + 0.00005) 20240, 40242, 40241                    
40241 if (r2e(1) - 0.00005) 40242, 40242, 20240                    
40242 if (r2e(2) + 0.00005) 20240, 10240, 40240                    
40240 if (r2e(2) - 0.00005) 10240, 10240, 20240                    
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           zvcorr = (0.0, 0.0)                                          
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0241      continue                                                     
! T025*  TEST 25                COMPLEX EXPRESSION PRESENTED AS ARGUMENT 
           ivtnum = 25                                                  
        cwdvc = (3.5, 4.5)                                              
        cwevc = (4.0, 5.0)                                              
        cwavc = conjg(cwdvc - cwevc)                                    
           if (r2e(1) + 0.50003) 20250, 40252, 40251                    
40251 if (r2e(1) + 0.49997) 40252, 40252, 20250                    
40252 if (r2e(2) - 0.49997) 20250, 10250, 40250                    
40250 if (r2e(2) - 0.50003) 10250, 10250, 20250                    
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           zvcorr = (-0.5, 0.5)                                         
           write (nuvi, 80045) ivtnum, cwavc, zvcorr                    
 0251      continue                                                     
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
! *****    END OF TEST SEGMENT 170                                       
        stop                                                            
        end program fm809
