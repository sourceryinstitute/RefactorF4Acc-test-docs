      program fm805
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM805               YDDIM - (164)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTION DDIM AND PROD--POSITIVE        15.3   
! *****    DIFFERENCE AND DOUBLE PRECISION PRODUCT, RESP.       (TABLE 5)
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
! *****     S P E C I F I C A T I O N S  SEGMENT 164                     
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
      real :: rsavs
      real :: rsbvs
        double precision :: dsavd
        double precision :: dsbvd
        double precision :: dsdvd
        double precision :: dsevd
        double precision :: dvcorr
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
      zprog = 'FM805'                                                   
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
! *****    HEADER FOR SEGMENT 164                                        
        write (nuvi,16401)                                              
16401 format (" ",// 1x,"YDDIM - (164) INTRINSIC FUNCTIONS-- " //16x,           "DDIM (POSITIVE DIFFERENCE)" /,16x,                               "DPROD (D.P. PRODUCT)" //                                         2x,"ANS REF. - 15.3" )                                  
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
! *****    TEST OF DDIM                                                  
! *****                                                                  
        write(nuvi, 16402)                                              
16402 format(/ 8x, "TEST OF DDIM" )                                   
! T001*  TEST 1                                        BOTH VALUES EQUAL 
           ivtnum = 1                                                   
        dsbvd = 0.25d0                                                  
        dsdvd = 0.25d0                                                  
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd + 5.0d-10) 20010, 10010, 40010                     
40010 if (dsavd - 5.0d-10) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0011      continue                                                     
! T002*  TEST 2                       BOTH VALUES EQUAL, INTEGRAL VALUES 
           ivtnum = 2                                                   
        dsbvd = 2.0d0                                                   
        dsdvd = 2.0d0                                                   
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd + 5.0d-10) 20020, 10020, 40020                     
40020 if (dsavd - 5.0d-10) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0021      continue                                                     
! T003*  TEST 3                             FIRST VALUE LESS THAN SECOND 
           ivtnum = 3                                                   
        dsbvd = 0.25d1                                                  
        dsdvd = 0.55d1                                                  
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd + 5.0d-10) 20030, 10030, 40030                     
40030 if (dsavd - 5.0d-10) 10030, 10030, 20030                     
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0031      continue                                                     
! T004*  TEST 4                          FIRST VALUE GREATER THAN SECOND 
           ivtnum = 4                                                   
        dsbvd = 0.55d1                                                  
        dsdvd = 0.25d1                                                  
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd - 2.999999998d0) 20040, 10040, 40040               
40040 if (dsavd - 3.000000002d0) 10040, 10040, 20040               
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 3.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0041      continue                                                     
! T005*  TEST 5                         BOTH VALUES EQUAL, BOTH NEGATIVE 
           ivtnum = 5                                                   
        dsbvd = -0.25d1                                                 
        dsdvd = -0.25d1                                                 
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd + 5.0d-10) 20050, 10050, 40050                     
40050 if (dsavd - 5.0d-10) 10050, 10050, 20050                     
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0051      continue                                                     
! T006*  TEST 6           FIRST VALUE GREATER THAN SECOND, BOTH NEGATIVE 
           ivtnum = 6                                                   
        dsbvd = -0.25d1                                                 
        dsdvd = -0.55d1                                                 
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd - 2.999999998d0) 20060, 10060, 40060               
40060 if (dsavd - 3.000000002d0) 10060, 10060, 20060               
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 3.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0061      continue                                                     
! T007*  TEST 7                    FIRST VALUE POSITIVE, SECOND NEGATIVE 
           ivtnum = 7                                                   
        dsbvd = 0.55d1                                                  
        dsdvd = -0.25d1                                                 
        dsavd = ddim(dsbvd, dsdvd)                                      
           if (dsavd - 7.999999996d0) 20070, 10070, 40070               
40070 if (dsavd - 8.000000004d0) 10070, 10070, 20070               
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 8.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0071      continue                                                     
! T008*  TEST 8                  ARITHMETIC EXPRESSION PRESENTED TO DDIM 
           ivtnum = 8                                                   
        dsdvd = 0.25d1                                                  
        dsevd = 0.125d1                                                 
        dsavd = ddim(dsdvd / dsevd, dsdvd * dsevd)                      
           if (dsavd + 5.0d-10) 20080, 10080, 40080                     
40080 if (dsavd - 5.0d-10) 10080, 10080, 20080                     
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dsavd, dvcorr                    
 0081      continue                                                     
! *****                                                                  
! *****    TEST OF DPROD                                                 
! *****                                                                  
        write(nuvi, 16404)                                              
        remrks = '+ OR - 0.00005'                                       
16404 format(// 8x, "TEST OF DPROD" )                                 
! T009*  TEST 9                     PAIR OF VALUES, ONE OF WHICH IS ZERO 
           ivtnum = 9                                                   
        rsavs = 0.0                                                     
        rsbvs = 2.0                                                     
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd + 5.0d-5) 20090, 10090, 40090                      
40090 if (dsavd - 5.0d-5) 10090, 10090, 20090                      
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0091      continue                                                     
! T010*  TEST 10                     PAIR OF VALUES, ONE OF WHICH IS ONE 
           ivtnum = 10                                                  
        rsavs = 1.0                                                     
        rsbvs = 2.0                                                     
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd - 1.9999d0) 20100, 10100, 40100                    
40100 if (dsavd - 2.0001d0) 10100, 10100, 20100                    
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 2.0d0                                               
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0101      continue                                                     
! T011*  TEST 11                                 PAIR OF NON-ZERO VALUES 
           ivtnum = 11                                                  
        rsavs = 3.333333                                                
        rsbvs = 2.3948094                                               
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd - 7.9823d0) 20110, 10110, 40110                    
40110 if (dsavd - 7.9831d0) 10110, 10110, 20110                    
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr = 7.982697202d0                                       
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0111      continue                                                     
! T012*  TEST 12                            ONE POSITIVE, ONE NEGATIVE   
           ivtnum = 12                                                  
        rsavs = 0.123456                                                
        rsbvs = -2.98765                                                
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd + 3.6887d-1) 20120, 10120, 40120                   
40120 if (dsavd + 3.6882d-1) 10120, 10120, 20120                   
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr = -3.688433184d-1                                     
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0121      continue                                                     
! T013*  TEST 13                          ONE VALUE ONE(1), ONE NEGATIVE 
           ivtnum = 13                                                  
        rsavs = 1.0834001                                               
        rsbvs = -2.034985                                               
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd + 2.2049d0) 20130, 10130, 40130                    
40130 if (dsavd + 2.2045d0) 10130, 10130, 20130                    
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr = -2.204702953d0                                      
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0131      continue                                                     
! T014*  TEST 14                                 PAIR OF NEGATIVE VALUES 
           ivtnum = 14                                                  
        rsavs = -3.077734                                               
        rsbvs = -2.348343                                               
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd - 7.2272d0) 20140, 10140, 40140                    
40140 if (dsavd - 7.2280d0) 10140, 10140, 20140                    
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr = 7.227575095d0                                       
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0141      continue                                                     
! T015*  TEST 15                  ONE POSITIVE VALUE, ONE NEGATIVE VALUE 
           ivtnum = 15                                                  
        rsavs = 3.3333324                                               
        rsbvs = -2.343953                                               
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd + 7.8136d0) 20150, 10150, 40150                    
40150 if (dsavd + 7.8127d0) 10150, 10150, 20150                    
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           dvcorr = -7.813174479d0                                      
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0151      continue                                                     
! T016*  TEST 16                ARITHMETIC EXPRESSION PRESENTED TO DPROD 
           ivtnum = 16                                                  
        rsavs = 1.555674                                                
        rsbvs = 2.00012                                                 
        dsavd = dprod(rsavs - rsbvs, rsavs + rsbvs)                     
           if (dsavd + 1.5805d0) 20160, 10160, 40160                    
40160 if (dsavd + 1.5802d0) 10160, 10160, 20160                    
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr = -1.580358420d0                                      
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0161      continue                                                     
! T017*  TEST 17                       DPROD FORMS THE ARGUMENTS TO DDIM 
           ivtnum = 17                                                  
        dsavd = ddim(dprod(0.4, 2.0), dprod(3.0, 0.1))                  
           if (dsavd - 0.49997d0) 20170, 10170, 40170                   
40170 if (dsavd - 0.50003d0) 10170, 10170, 20170                   
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           dvcorr = 0.5d0                                               
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0171      continue                                                     
! T018*  TEST 18                  ARGUMENTS WITH HIGH AND LOW MAGNITUDES 
           ivtnum = 18                                                  
        rsavs = 1.23456e-33                                             
        rsbvs = 1.23456e+34                                             
        dsavd = dprod(rsavs, rsbvs)                                     
           if (dsavd - 1.5240d1) 20180, 10180, 40180                    
40180 if (dsavd - 1.5242d1) 10180, 10180, 20180                    
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           dvcorr = 1.524138394d1                                       
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 80033) dsavd                                    
           write (nuvi, 80035) dvcorr, remrks                           
 0181      continue                                                     
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
! *****    END OF TEST SEGMENT 164                                       
        stop                                                            
        end program fm805
