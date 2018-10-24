      program fm357
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM357               XAMOD - (159)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTIONS AMOD AND MOD - REMAINDERING,  15.3   
! *****    WHICH IS DEFINED AS A1-(A1/A2)A2 WHERE (X) IS AN     (TABLE 5)
! *****    INTEGER WHOSE MAGNITUDE IS LE ABS(X) AND WHOSE SIGN           
! *****    IS THE SAME AS X.                                             
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
      real :: rebvs
      real :: redvs
      real :: reavs
      real :: rvcorr
      real :: recvs
      real :: refvs
      integer :: iebvi
      integer :: iedvi
      integer :: ieavi
      integer :: ivcorr
      integer :: ieevi
      integer :: iefvi
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
      ivtotl = 22                                                       
      zprog = 'FM357'                                                   
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
! *****    HEADER FOR SEGMENT 159 WRITTEN                                
        write (nuvi,15901)                                              
15901 format (" ", //, 2x,"XAMOD - (159) INTRINSIC FUNCTION-- " //16x,          "AMOD, MOD (REMAINDERING)" //" SUBSET REF. - 15.3" )    
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
! *****    TEST OF AMOD                                                  
! *****                                                                  
        write(nuvi, 15902)                                              
15902 format (/ 8x, "TEST OF AMOD" )                                  
! *****                                                                  
! T001*  TEST 1                      FIRST VALUE ZERO, SECOND NON-ZERO   
           ivtnum = 1                                                   
        rebvs = 0.0                                                     
        redvs = 4.5                                                     
        reavs = amod(rebvs, redvs)                                      
           if (reavs + 0.00005) 20010, 10010, 40010                     
40010 if (reavs - 0.00005) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi,80012) ivtnum, reavs, rvcorr                     
 0011      continue                                                     
! T002*  TEST 2                                      BOTH VALUES EQUAL   
           ivtnum = 2                                                   
        rebvs = 3.5                                                     
        redvs = 3.5                                                     
        reavs = amod(rebvs, redvs)                                      
           if (reavs + 0.00005) 20020, 10020, 40020                     
40020 if (reavs - 0.00005) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0021      continue                                                     
! T003*  TEST 3         FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   
           ivtnum = 3                                                   
        rebvs = -10.9                                                   
        redvs = -3.3                                                    
        reavs = amod(rebvs, redvs)                                      
           if (reavs + 1.0001) 20030, 10030, 40030                      
40030 if (reavs + 0.99995) 10030, 10030, 20030                     
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = -1.0                                                
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0031      continue                                                     
! T004*  TEST 4             FIRST MAGNITUDE LARGER, MULTIPLE OF SECOND   
           ivtnum = 4                                                   
        redvs = 1.5                                                     
        rebvs = 1.5 + redvs + 1.5                                       
        reavs = amod(rebvs, redvs)                                      
           if (reavs + 0.00005) 20040, 10040, 40040                     
40040 if (reavs - 0.00005) 10040, 10040, 20040                     
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0041      continue                                                     
! T005*  TEST 5         FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   
           ivtnum = 5                                                   
        rebvs = 7.625                                                   
        redvs = 2.125                                                   
        reavs = amod(rebvs, redvs)                                      
           if (reavs - 1.2499) 20050, 10050, 40050                      
40050 if (reavs - 1.2501) 10050, 10050, 20050                      
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 1.25                                                
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0051      continue                                                     
! T006*  TEST 6                      FIRST VALUE ZERO, SECOND NEGATIVE   
           ivtnum = 6                                                   
        rebvs = 0.0                                                     
        redvs = -4.5                                                    
        reavs = amod(rebvs, redvs)                                      
           if (reavs + 0.00005) 20060, 10060, 40060                     
40060 if (reavs - 0.00005) 10060, 10060, 20060                     
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0061      continue                                                     
! T007*  TEST 7                       BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 7                                                   
        rebvs = -3.5                                                    
        redvs = -3.5                                                    
        reavs = amod(rebvs, redvs)                                      
           if (reavs + 0.00005) 20070, 10070, 40070                     
40070 if (reavs - 0.00005) 10070, 10070, 20070                     
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0071      continue                                                     
! T008*  TEST 8        FIRST VALUE NEGATIVE, SECOND POSITIVE, MULTIPLE   
           ivtnum = 8                                                   
        rebvs = 1.5                                                     
        redvs = -(1.5 + redvs + 1.5)                                    
        reavs = amod(-rebvs, -redvs)                                    
           if (reavs + 0.00005) 20080, 10080, 40080                     
40080 if (reavs - 0.00005) 10080, 10080, 20080                     
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0081      continue                                                     
! T009*  TEST 9         FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   
           ivtnum = 9                                                   
        rebvs = 10.5                                                    
        redvs = -3.3                                                    
        reavs = amod(rebvs, redvs)                                      
           if (reavs - 0.59997) 20090, 10090, 40090                     
40090 if (reavs - 0.60003) 10090, 10090, 20090                     
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 0.6                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0091      continue                                                     
! T010*  TEST 10       PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT   
           ivtnum = 10                                                  
        recvs = 7.625                                                   
        redvs = 2.125                                                   
        refvs = 2.0                                                     
        reavs = amod(recvs - refvs, redvs + refvs)                      
           if (reavs - 1.4999) 20100, 10100, 40100                      
40100 if (reavs - 1.5001) 10100, 10100, 20100                      
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 1.5                                                 
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0101      continue                                                     
! T011*  TEST 11                TEST LOW AND HIGH MAGNITUDE ARGUMENTS    
           ivtnum = 11                                                  
        recvs = 1.0e-16                                                 
        redvs = 1.0e+16                                                 
        reavs = amod(recvs, redvs)                                      
           if (reavs - 0.99995e-16) 20110, 10110, 40110                 
40110 if (reavs - 1.0001e-16) 10110, 10110, 20110                  
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 1.0e-16                                             
           write(nuvi, 80012) ivtnum, reavs, rvcorr                     
 0111      continue                                                     
! *****                                                                  
! *****    TEST OF MOD                                                   
! *****                                                                  
        write(nuvi, 15904)                                              
15904 format (/ 8x, "TEST OF MOD" )                                   
! *****                                                                  
! T012*  TEST 12                     FIRST VALUE ZERO, SECOND NON-ZERO   
           ivtnum = 12                                                  
        iebvi = 0                                                       
        iedvi = 4                                                       
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20120, 10120, 20120                           
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0121      continue                                                     
! T013*  TEST 13                                     BOTH VALUES EQUAL   
           ivtnum = 13                                                  
        iebvi = 3                                                       
        iedvi = 3                                                       
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20130, 10130, 20130                           
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0131      continue                                                     
! T014*  TEST 14        FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   
           ivtnum = 14                                                  
        iebvi = -10                                                     
        iedvi = -3                                                      
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi + 1) 20140, 10140, 20140                           
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           ivcorr = -1                                                  
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0141      continue                                                     
! T015*  TEST 15            FIRST MAGNITUDE LARGER, MULTIPLE OF SECOND   
           ivtnum = 15                                                  
        iebvi = 9                                                       
        iedvi = 3                                                       
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20150, 10150, 20150                           
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0151      continue                                                     
! T016*  TEST 16        FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND   
           ivtnum = 16                                                  
        iebvi = 7                                                       
        iedvi = 2                                                       
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 1) 20160, 10160, 20160                           
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0161      continue                                                     
! T017*  TEST 17                     FIRST VALUE ZERO, SECOND NEGATIVE   
           ivtnum = 17                                                  
        iebvi = 0                                                       
        iedvi = -4                                                      
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20170, 10170, 20170                           
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0171      continue                                                     
! T018*  TEST 18                      BOTH VALUES EQUAL, BOTH NEGATIVE   
           ivtnum = 18                                                  
        iebvi = -3                                                      
        iedvi = -3                                                      
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20180, 10180, 20180                           
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0181      continue                                                     
! T019*  TEST 19       FIRST MAGNITUDE LARGER, MULTIPLE, BOTH NEGATIVE   
           ivtnum = 19                                                  
        iebvi = -9                                                      
        iedvi = -3                                                      
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20190, 10190, 20190                           
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0191      continue                                                     
! T020*  TEST 20      FIRST NUMBER NEGATIVE, SECOND POSITIVE, MULTIPLE   
           ivtnum = 20                                                  
        iebvi = -9                                                      
        iedvi = 3                                                       
        ieavi = mod(iebvi, iedvi)                                       
           if (ieavi - 0) 20200, 10200, 20200                           
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0201      continue                                                     
! T021*  TEST 21               FIRST VALUE ZERO PRECEDED BY MINUS SIGN   
           ivtnum = 21                                                  
        iebvi = 0                                                       
        iedvi = 4                                                       
        ieavi = mod(-iebvi, iedvi)                                      
           if (ieavi - 0) 20210, 10210, 20210                           
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0211      continue                                                     
! T022*  TEST 22       PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT   
           ivtnum = 22                                                  
        iedvi = 10                                                      
        ieevi = 3                                                       
        iefvi = 2                                                       
        ieavi = mod(iedvi - iefvi, ieevi + iefvi)                       
           if (ieavi - 3) 20220, 10220, 20220                           
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           ivcorr = 3                                                   
           write (nuvi, 80010) ivtnum, ieavi, ivcorr                    
 0221      continue                                                     
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
! *****    END OF TEST SEGMENT 159                                       
      stop                                                              
      end program fm357
