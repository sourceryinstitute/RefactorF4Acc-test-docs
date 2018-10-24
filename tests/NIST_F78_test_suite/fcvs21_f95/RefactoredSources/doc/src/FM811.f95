      program fm811
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM811               YCMMX - (174)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TESTS THE USE OF INTEGER, REAL, DOUBLE PRECISION,      15.10  
! *****    AND COMPLEX EXPRESSIONS CONTAINING REFERENCE         (TABLE 5)
! *****    TO THE INTRINSIC FUNCTIONS OF THE FULL LANGUAGE        6.1.4  
! *****                                                                  
! *****  GENERAL COMMENTS                                                
! *****    SEGMENTS TESTING XINT, XREAL, XAINT, XABS, XAMOD,             
! *****    XSIGN, XDIM, XMAX, XMIN, YIDINT, YSNGL                        
! *****    YDINT, YDABS, YCABS, YDMOD, YDSIGN,                           
! *****    YDMAX1, YDMIN1, YDBLE, YCONJG ASSUMED WORKING                 
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
! *****    S P E C I F I C A T I O N S  SEGMENT 174                      
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
      real :: ryavs
      real :: rvcorr
      integer :: iyavi
      integer :: iybvi
      integer :: iydvi
        double precision :: dyavd
        double precision :: dybvd
        double precision :: dydvd
        double precision :: dvcorr
        complex :: cyavc
        complex :: cydvc
        complex :: zvcorr
        real, dimension(1:2) :: r2e
        equivalence (cyavc,r2e)                                         
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
      ivtotl = 10                                                       
      zprog = 'FM811'                                                   
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
! *****    HEADER FOR SEGMENT 174 WRITTEN                                
        write (nuvi,17401)                                              
17401 format(  " ", //1x, "YCMMX - (174) INTRINSIC FUNCTIONS--" //             16x, "INTEGER, REAL, D.P." /                                      16x, "AND COMPLEX IN MIXED MODE EXPRESSIONS" //                   2x, "ANS REF. - 15.10" )                                 
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
! T001*  TEST 1                                                    IDINT 
           ivtnum = 1                                                   
        dybvd = 5.2d0                                                   
        cyavc = idint(dybvd) + (1.0, 2.0)                               
           if (r2e(1) - 5.9997) 20010, 40012, 40011                     
40011 if (r2e(1) - 6.0003) 40012, 40012, 20010                     
40012 if (r2e(2) - 1.9999) 20010, 10010, 40010                     
40010 if (r2e(2) - 2.0001) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           zvcorr = (6.0, 2.0)                                          
           write (nuvi, 80045) ivtnum, cyavc, zvcorr                    
 0011      continue                                                     
! T002*  TEST 2                                                     SNGL 
           ivtnum = 2                                                   
        dyavd = 5.5d0                                                   
        cyavc = sngl(dyavd) - (3.0, 4.0)                                
           if (r2e(1) - 2.4998) 20020, 40022, 40021                     
40021 if (r2e(1) - 2.5002) 40022, 40022, 20020                     
40022 if (r2e(2) + 4.0002) 20020, 10020, 40020                     
40020 if (r2e(2) + 3.9998) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           zvcorr = (2.5, -4.0)                                         
           write (nuvi, 80045) ivtnum, cyavc, zvcorr                    
 0021      continue                                                     
! T003*  TEST 3                                  SNGL, DINT, DNINT, CABS 
           ivtnum = 3                                                   
        dybvd = 5.8d0                                                   
        ryavs = sngl(dint(dybvd) + dnint(dybvd)) * cabs((3.0, 4.0))     
           if (ryavs - 54.997) 20030, 10030, 40030                      
40030 if (ryavs - 55.003) 10030, 10030, 20030                      
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 55.0                                                
           write (nuvi, 80012) ivtnum, ryavs, rvcorr                    
 0031      continue                                                     
! T004*  TEST 4                                            IDNINT, AIMAG 
           ivtnum = 4                                                   
        cydvc = (3.0, 4.0)                                              
        dybvd = 5.8d0                                                   
        cyavc = ((idnint(dybvd) - cydvc)) * aimag((4.0, 3.0))           
           if (r2e(1) - 8.9995) 20040, 40042, 40041                     
40041 if (r2e(1) - 9.0005) 40042, 40042, 20040                     
40042 if (r2e(2) + 12.001) 20040, 10040, 40040                     
40040 if (r2e(2) + 11.999) 10040, 10040, 20040                     
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           zvcorr = (9.0, -12.0)                                        
           write (nuvi, 80045) ivtnum, cyavc, zvcorr                    
 0041      continue                                                     
! T005*  TEST 5                                              CABS, CMPLX 
           ivtnum = 5                                                   
        iyavi = 5                                                       
        ryavs = cabs(cmplx(3.0, 4.0)) / iyavi                           
           if (ryavs - 0.99995) 20050, 10050, 40050                     
40050 if (ryavs - 1.0001) 10050, 10050, 20050                      
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write (nuvi, 80012) ivtnum, ryavs, rvcorr                    
 0051      continue                                                     
! T006*  TEST 6                                        CONJG, SNGL, DMOD 
           ivtnum = 6                                                   
        dybvd = 5.0d0                                                   
        dydvd = 3.0d0                                                   
        cyavc = conjg((3.0, 4.0)) * sngl(dmod(dybvd, dydvd))            
           if (r2e(1) - 5.9997) 20060, 40062, 40061                     
40061 if (r2e(1) - 6.0003) 40062, 40062, 20060                     
40062 if (r2e(2) + 8.0004) 20060, 10060, 40060                     
40060 if (r2e(2) + 7.9996) 10060, 10060, 20060                     
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           zvcorr = (6.0, -8.0)                                         
           write (nuvi, 80045) ivtnum, cyavc, zvcorr                    
 0061      continue                                                     
! T007*  TEST 7                                      DSIGN, AIMAG, CONJG 
           ivtnum = 7                                                   
        cydvc = (-3.0, -4.0)                                            
        dybvd = 4.0d0                                                   
        dydvd = 1.0d0                                                   
        dyavd = dsign(dybvd, dydvd) / aimag(conjg(cydvc))               
           if (dyavd - 0.9999999995d0) 20070, 10070, 40070              
40070 if (dyavd - 1.000000001d0) 10070, 10070, 20070               
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, dyavd, dvcorr                    
 0071      continue                                                     
! T008*  TEST 8                           DPROD, CABS, AIMAG, SNGL, DDIM 
           ivtnum = 8                                                   
        cydvc = (3.0, 4.0)                                              
        dybvd = -7.0d0                                                  
        dydvd = 3.0d0                                                   
        dyavd = dprod(cabs(cydvc + (-3.0, 3.0)),                                       aimag(cydvc) + (sngl(ddim(dybvd, dydvd))))         
           if (dyavd - 27.99999998d0) 20080, 10080, 40080               
40080 if (dyavd - 28.00000002d0) 10080, 10080, 20080               
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = 28.0d0                                              
           write (nuvi, 80031) ivtnum, dyavd, dvcorr                    
 0081      continue                                                     
! T009*  TEST 9                                       AMAX1, CABS, AIMAG 
           ivtnum = 9                                                   
        cydvc = (3.0, 4.0)                                              
        dyavd = amax1(cabs(cydvc), aimag(cydvc * cydvc))                
           if (dyavd - 23.99999998d0) 20090, 10090, 40090               
40090 if (dyavd - 24.00000002d0) 10090, 10090, 20090               
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = 24.0d0                                              
           write (nuvi, 80031) ivtnum, dyavd, dvcorr                    
 0091      continue                                                     
! T010*  TEST 10                                       AIMAG, ABS, AMIN0 
           ivtnum = 10                                                  
        cydvc = (3.0, -3.)                                              
        iybvi = 4                                                       
        iydvi = -3                                                      
        cyavc = ((3.0, 4.0) + aimag((3.0, 4.0))) *                               (abs(amin0(iybvi, iydvi)) - cydvc)                       
           if (r2e(1) + 12.001) 20100, 40102, 40101                     
40101 if (r2e(1) + 11.999) 40102, 40102, 20100                     
40102 if (r2e(2) - 20.999) 20100, 10100, 40100                     
40100 if (r2e(2) - 21.001) 10100, 10100, 20100                     
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           zvcorr = (-12.0, 21.0)                                       
           write (nuvi, 80045) ivtnum, cyavc, zvcorr                    
 0101      continue                                                     
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
! *****    END OF TEST SEGMENT 174                                       
        stop                                                            
        end program fm811
