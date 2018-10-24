      program fm829
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM829                                                          
! *****                       YGEN1 - (206)                              
! *****                                                                  
! ***********************************************************************
! *****  TESTING OF GENERIC FUNCTIONS                            ANS REF 
! *****          INT, REAL, DBLE, CMPLX                           15.3   
! *****                                                          TABLE 5 
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
! *****  S P E C I F I C A T I O N S  SEGMENT 206                        
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
      integer :: lvi
      integer :: ivcorr
      real :: avs
      real :: bvs
      real :: rvcorr
      real :: cvs
      integer :: ivi
      integer :: kvi
      integer :: jvi
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
        double precision :: dvcorr
        complex :: avc
        complex :: bvc
        complex :: cvc
        complex :: zvcorr
        real, dimension(1:2) :: r2e
        equivalence (bvc, r2e)                                          
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
      ivtotl = 35                                                       
      zprog = 'FM829'                                                   
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
! *****    HEADER FOR SEGMENT 206                                        
        write(nuvi,20600)                                               
20600 format( " ", /  " YGEN1 - (206) GENERIC FUNCTIONS --" //                  "  INT, REAL, DBLE, CMPLX" //                                     "  ANS REF. - 15.3" )                                   
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
! T001*  TEST 1                          TEST OF INT                     
! *****                                          WITH INTEGER ARG        
           ivtnum = 1                                                   
        lvi = int(485)                                                  
           if (lvi -   485) 20010, 10010, 20010                         
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           ivcorr =   485                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0011      continue                                                     
! T002*  TEST 2                                  WITH DOUBLE PREC ARG    
           ivtnum = 2                                                   
        lvi = int(1.375d0)                                              
           if (lvi -     1) 20020, 10020, 20020                         
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           ivcorr =     1                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0021      continue                                                     
! T003*  TEST 3                                  WITH COMPLEX ARG        
           ivtnum = 3                                                   
        lvi = int((1.24, 5.67))                                         
           if (lvi -     1) 20030, 10030, 20030                         
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           ivcorr =     1                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0031      continue                                                     
! T004*  TEST 4                          TEST OF INT AND IFIX            
! *****                                          WITH REAL ARGS          
           ivtnum = 4                                                   
        lvi = int(6.0001) + ifix(-1.750)                                
           if (lvi -     5) 20040, 10040, 20040                         
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           ivcorr =     5                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0041      continue                                                     
! T005*  TEST 5                          TEST OF INT AND IDINT           
! *****                                          WITH DOUBLE PREC ARGS   
           ivtnum = 5                                                   
        avd = -1.11d1                                                   
        lvi = int(avd) * idint(3.5d0)                                   
           if (lvi +    33) 20050, 10050, 20050                         
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           ivcorr =   -33                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0051      continue                                                     
! T006*  TEST 6             INTEGER, REAL, DOUBLE PRECISION, AND COMPLEX 
! *****                                                        ARGUMENTS 
           ivtnum = 6                                                   
        lvi = int(-327) + int(6.75) * int(123) - int(6.0001d0)                  / ifix(13.3) + int((2.4, 3.5)) + idint(-3.375d0)          
           if (lvi -   410) 20060, 10060, 20060                         
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           ivcorr =   410                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0061      continue                                                     
! T007*  TEST 7                          TEST OF REAL                    
! *****                                          WITH REAL ARG           
           ivtnum = 7                                                   
        avs = -3.0                                                      
        bvs = real(avs)                                                 
           if (bvs +  0.30002e+01) 20070, 10070, 40070                  
40070 if (bvs +  0.29998e+01) 10070, 10070, 20070                  
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = -3.0                                                
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0071      continue                                                     
! T008*  TEST 8                                  WITH DOUBLE PRECISION   
           ivtnum = 8                                                   
        avd = 0.96875d0                                                 
        bvs = real(avd)                                                 
           if (bvs -  0.96870e+00) 20080, 10080, 40080                  
40080 if (bvs -  0.96880e+00) 10080, 10080, 20080                  
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = 0.96875                                             
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                  WITH COMPLEX            
           ivtnum = 9                                                   
        bvs = real((2.5, -3.0))                                         
           if (bvs -  0.24998e+01) 20090, 10090, 40090                  
40090 if (bvs -  0.25002e+01) 10090, 10090, 20090                  
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 2.5                                                 
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0091      continue                                                     
! T010*  TEST 10                         TEST OF REAL AND FLOAT          
           ivtnum = 10                                                  
        bvs = real(6) + float(8)                                        
           if (bvs -  0.13999e+02) 20100, 10100, 40100                  
40100 if (bvs -  0.14001e+02) 10100, 10100, 20100                  
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 14.0                                                
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0101      continue                                                     
! T011*  TEST 11                         TEST OF REAL AND SNGL           
           ivtnum = 11                                                  
        avd = 2.5d0                                                     
        bvs = real(avd) + sngl(0.35875d2)                               
           if (bvs -  0.38373e+02) 20110, 10110, 40110                  
40110 if (bvs -  0.38377e+02) 10110, 10110, 20110                  
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 38.375                                              
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0111      continue                                                     
! T012*  TEST 12                         TEST OF REAL, FLOAT, AND SNGL   
           ivtnum = 12                                                  
        bvs = real(13) + float(9) * sngl(0.7625d1) - real(2.625d0) +              real(3.5) / real((2.0, 4.0))                            
           if (bvs -  0.80746e+02) 20120, 10120, 40120                  
40120 if (bvs -  0.80754e+02) 10120, 10120, 20120                  
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 80.75                                               
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0121      continue                                                     
! T013*  TEST 13                         TEST OF DBLE                    
! *****                                          WITH INTEGER ARG        
           ivtnum = 13                                                  
        lvi = 9                                                         
        bvd = dble(lvi)                                                 
           if (bvd -  0.89995d+01) 20130, 10130, 40130                  
40130 if (bvd -  0.90005d+01) 10130, 10130, 20130                  
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr = 9.0d0                                               
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0131      continue                                                     
! T014*  TEST 14                                 WITH REAL ARG           
           ivtnum = 14                                                  
        avs = 10.5                                                      
        bvd = dble(avs)                                                 
           if (bvd -  0.10499d+02) 20140, 10140, 40140                  
40140 if (bvd -  0.10501d+02) 10140, 10140, 20140                  
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr = 10.5d0                                              
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0141      continue                                                     
! T015*  TEST 15                                 WITH DOUBLE PREC ARG    
           ivtnum = 15                                                  
        avd = 9.9d0                                                     
        bvd = dble(avd)                                                 
           if (bvd -  0.9899999995d+01) 20150, 10150, 40150             
40150 if (bvd -  0.9900000005d+01) 10150, 10150, 20150             
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           dvcorr = 9.9d0                                               
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0151      continue                                                     
! T016*  TEST 16                                 WITH COMPLEX ARG        
           ivtnum = 16                                                  
        avc = (2.5, 5.5)                                                
        bvd = dble(avc)                                                 
           if (bvd -  0.24998d+01) 20160, 10160, 40160                  
40160 if (bvd -  0.25002d+01) 10160, 10160, 20160                  
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr = 2.5d0                                               
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0161      continue                                                     
! T017*  TEST 17                         TEST OF CMPLX WITH ONE ARG      
! *****                                          WITH INTEGER ARG        
           ivtnum = 17                                                  
        bvc = cmplx(9)                                                  
           if (r2e(1) -  0.89995e+01) 20170, 40172, 40171               
40171 if (r2e(1) -  0.90005e+01) 40172, 40172, 20170               
40172 if (r2e(2) +  0.50000e-04) 20170, 10170, 40170               
40170 if (r2e(2) -  0.50000e-04) 10170, 10170, 20170               
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           zvcorr = (9,0)                                               
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0171      continue                                                     
! T018*  TEST 18                                 WITH REAL               
           ivtnum = 18                                                  
        bvc = cmplx(4.093)                                              
           if (r2e(1) -  0.40928e+01) 20180, 40182, 40181               
40181 if (r2e(1) -  0.40932e+01) 40182, 40182, 20180               
40182 if (r2e(2) +  0.50000e-04) 20180, 10180, 40180               
40180 if (r2e(2) -  0.50000e-04) 10180, 10180, 20180               
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           zvcorr = (4.093,0.0)                                         
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0181      continue                                                     
! T019*  TEST 19                                 WITH DOUBLE PREC ARG    
           ivtnum = 19                                                  
        avd = 0.375d-3                                                  
        bvc = cmplx(avd)                                                
           if (r2e(1) -  0.37498e-03) 20190, 40192, 40191               
40191 if (r2e(1) -  0.37502e-03) 40192, 40192, 20190               
40192 if (r2e(2) +  0.50000e-04) 20190, 10190, 40190               
40190 if (r2e(2) -  0.50000e-04) 10190, 10190, 20190               
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           zvcorr = (0.375e-3, 0.0e0)                                   
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0191      continue                                                     
! T020*  TEST 20                                 WITH COMPLEX            
           ivtnum = 20                                                  
        avc = (4.5, 1.2)                                                
        bvc = cmplx(avc)                                                
           if (r2e(1) -  0.44997e+01) 20200, 40202, 40201               
40201 if (r2e(1) -  0.45003e+01) 40202, 40202, 20200               
40202 if (r2e(2) -  0.11999e+01) 20200, 10200, 40200               
40200 if (r2e(2) -  0.12001e+01) 10200, 10200, 20200               
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           zvcorr = (4.5, 1.2)                                          
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0201      continue                                                     
! T021*  TEST 21                         TEST OF CMPLX WITH TWO ARGS     
! *****                                          WITH INTEGER ARGS       
           ivtnum = 21                                                  
        bvc = cmplx(3, 1)                                               
           if (r2e(1) -  0.29998e+01) 20210, 40212, 40211               
40211 if (r2e(1) -  0.30002e+01) 40212, 40212, 20210               
40212 if (r2e(2) -  0.99995e+00) 20210, 10210, 40210               
40210 if (r2e(2) -  0.10001e+01) 10210, 10210, 20210               
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           zvcorr = (3.0, 1.0)                                          
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0211      continue                                                     
! T022*  TEST 22                                 WITH REAL ARGS          
           ivtnum = 22                                                  
        bvc = cmplx(8.34, 634.3)                                        
           if (r2e(1) -  0.83395e+01) 20220, 40222, 40221               
40221 if (r2e(1) -  0.83405e+01) 40222, 40222, 20220               
40222 if (r2e(2) -  0.63426e+03) 20220, 10220, 40220               
40220 if (r2e(2) -  0.63434e+03) 10220, 10220, 20220               
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           zvcorr = (8.34, 634.3)                                       
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0221      continue                                                     
! T023*  TEST 23                                 WITH DOUBLE PREC ARGS   
           ivtnum = 23                                                  
        avd = 0.96875d0                                                 
        bvd = 3.5d-1                                                    
        bvc = cmplx(avd, bvd)                                           
           if (r2e(1) -  0.96870e+00) 20230, 40232, 40231               
40231 if (r2e(1) -  0.96880e+00) 40232, 40232, 20230               
40232 if (r2e(2) -  0.34998e+00) 20230, 10230, 40230               
40230 if (r2e(2) -  0.35002e+00) 10230, 10230, 20230               
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           zvcorr = (0.96875, 0.35)                                     
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0231      continue                                                     
! T024*  TEST 24                         TEST OF INT AND =               
! *****                                          WITH REAL EXPR          
           ivtnum = 24                                                  
        cvs = 0.0                                                       
        cvd = 0.0d0                                                     
        cvc = (0.0,0.0)                                                 
        lvi = 0                                                         
        avs = 5.0                                                       
        ivi = 1.0 * 5.0 + 6.0                                           
        kvi = lvi + int(1.0 * avs + 6.0)                                
           if (kvi -    11) 20240, 10240, 20240                         
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           ivcorr =    11                                               
           write (nuvi, 80010) ivtnum, kvi, ivcorr                      
 0241      continue                                                     
! T025*  TEST 25                                 WITH DOUBLE PREC EXPR   
           ivtnum = 25                                                  
        avd = 3.48d0                                                    
        ivi = 3.48d0 * 47.98d0                                          
        kvi = lvi + int(avd * 47.98d0)                                  
           if (kvi -   166) 20250, 10250, 20250                         
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           ivcorr =   166                                               
           write (nuvi, 80010) ivtnum, kvi, ivcorr                      
 0251      continue                                                     
! T026*  TEST 26                                 WITH COMPLEX EXPR       
           ivtnum = 26                                                  
        avc = (3.9, 5.0)                                                
        ivi = (3.4, 4.5) + (3.9, 5.0)                                   
        kvi = lvi + int((3.4, 4.5) + avc)                               
           if (kvi -     7) 20260, 10260, 20260                         
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           ivcorr =     7                                               
           write (nuvi, 80010) ivtnum, kvi, ivcorr                      
 0261      continue                                                     
! T027*  TEST 27                         TEST OF REAL AND =              
! *****                                          WITH INT EXPR           
           ivtnum = 27                                                  
        ivi = 20                                                        
        avs = 20 + 34 / 20                                              
        bvs = cvs + real(ivi + 34 / ivi)                                
           if (bvs -  0.20999e+02) 20270, 10270, 40270                  
40270 if (bvs -  0.21001e+02) 10270, 10270, 20270                  
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           rvcorr = 21.0                                                
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0271      continue                                                     
! T028*  TEST 28                                 WITH DOUBLE PREC EXPR   
           ivtnum = 28                                                  
        jvi = 28                                                        
        avd = 0.9834d0                                                  
        avs = 3.0748d0 / 0.9834d0                                       
        bvs = cvs + real(3.0748d0 / avd)                                
           if (bvs -  0.31265e+01) 20280, 10280, 40280                  
40280 if (bvs -  0.31269e+01) 10280, 10280, 20280                  
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           rvcorr = 3.1267033                                           
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0281      continue                                                     
! T029*  TEST 29                                 WITH COMPLEX            
           ivtnum = 29                                                  
        jvi = 29                                                        
        avc = (1.0, 384.9)                                              
        avs = (3.495, 98.734) * (1.0, 384.9)                            
        bvs = cvs + real((3.495, 98.734) * avc)                         
           if (bvs +  0.38001e+05) 20290, 10290, 40290                  
40290 if (bvs +  0.37997e+05) 10290, 10290, 20290                  
10290 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           rvcorr = -37999.222                                          
           write (nuvi, 80012) ivtnum, bvs, rvcorr                      
 0291      continue                                                     
! T030*  TEST 30                         TEST OF DBLE AND =              
! *****                                          WITH INTEGER EXPR       
           ivtnum = 30                                                  
        jvi = 30                                                        
        ivi = 5                                                         
        avd = 1 * 5 + 6                                                 
        bvd = cvd + dble(1 * ivi + 6)                                   
           if (bvd -  0.10999d+02) 20300, 10300, 40300                  
40300 if (bvd -  0.11001d+02) 10300, 10300, 20300                  
10300 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           dvcorr = .11000000d+02                                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0301      continue                                                     
! T031*  TEST 31                                 WITH REAL EXPR          
           ivtnum = 31                                                  
        jvi = 31                                                        
        avs = -4.5                                                      
        avd = 1.3 / (-4.5)                                              
        bvd = cvd + dble(1.3 / avs)                                     
           if (bvd +  0.28891d+00) 20310, 10310, 40310                  
40310 if (bvd +  0.28887d+00) 10310, 10310, 20310                  
10310 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           dvcorr = -0.288888888888888889d+00                           
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0311      continue                                                     
! T032*  TEST 32                                 WITH COMPLEX EXPR       
           ivtnum = 32                                                  
        jvi = 32                                                        
        avc = (3.9, 5.0)                                                
        avd = (3.4, 4.5) + (3.9, 5.0)                                   
        bvd = cvd + dble((3.4, 4.5) + avc)                              
           if (bvd -  0.72996d+01) 20320, 10320, 40320                  
40320 if (bvd -  0.73004d+01) 10320, 10320, 20320                  
10320 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           dvcorr = .73000000d+01                                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0321      continue                                                     
! T033*  TEST 33                         TEST OF CMPLX AND =             
! *****                                          WITH INTEGER EXPR       
           ivtnum = 33                                                  
        jvi = 33                                                        
        ivi = 673                                                       
        avc = 394 - 673                                                 
        bvc = cvc + cmplx(394 - ivi)                                    
           if (r2e(1) +  0.27902e+03) 20330, 40332, 40331               
40331 if (r2e(1) +  0.27898e+03) 40332, 40332, 20330               
40332 if (r2e(2) +  0.50000e-04) 20330, 10330, 40330               
40330 if (r2e(2) -  0.50000e-04) 10330, 10330, 20330               
10330 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           zvcorr = (-279.00000, .00000000)                             
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0331      continue                                                     
! T034*  TEST 34                                 WITH REAL EXPR          
           ivtnum = 34                                                  
        jvi = 34                                                        
        avs = 3.48                                                      
        avc = 3.48 * 47.98                                              
        bvc = cvc + cmplx(avs * 47.98)                                  
           if (r2e(1) -  0.16696e+03) 20340, 40342, 40341               
40341 if (r2e(1) -  0.16698e+03) 40342, 40342, 20340               
40342 if (r2e(2) +  0.50000e-04) 20340, 10340, 40340               
40340 if (r2e(2) -  0.50000e-04) 10340, 10340, 20340               
10340 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           zvcorr = (166.97040, .00000000)                              
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0341      continue                                                     
! T035*  TEST 35                                                         
           ivtnum = 35                                                  
        jvi = 35                                                        
        avd = 0.94d1                                                    
        avc = 3.0283d3 / 0.94d1                                         
        bvc = cvc + cmplx(3.0283d3 / avd)                               
           if (r2e(1) -  0.32214e+03) 20350, 40352, 40351               
40351 if (r2e(1) -  0.32218e+03) 40352, 40352, 20350               
40352 if (r2e(2) +  0.50000e-04) 20350, 10350, 40350               
40350 if (r2e(2) -  0.50000e-04) 10350, 10350, 20350               
10350 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           zvcorr = (322.15957, .000000000)                             
           write (nuvi, 80045) ivtnum, bvc, zvcorr                      
 0351      continue                                                     
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
! *****    END OF TEST SEGMENT 206                                       
      stop                                                              
      end program fm829
