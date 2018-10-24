      program fm359
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM359               XSIGN - (161)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                       SUBSET REF
! *****    TEST INTRINSIC FUNCTION - SIGN, ISIGN - (TRANSFER      15.3   
! *****    OF SIGN - SIGN OF A2 TIMES ABS(A1)  )                (TABLE 5)
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
      real :: rfbvs
      real :: rfavs
      real :: rvcorr
      real :: rfdvs
      real :: rfevs
      integer :: ifbvi
      integer :: ifdvi
      integer :: ifavi
      integer :: ivcorr
      integer :: ifevi
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
      zprog = 'FM359'                                                   
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
! *****    HEADER FOR SEGMENT 161                                        
        write (nuvi,16101)                                              
16101 format(" ", //  2x,"XSIGN - (161) INTRINSIC FUNCTIONS-- " //12x,           "SIGN, ISIGN (TRANSFER OF SIGN)" //                               2x,"SUBSET REF. - 15.3 " )                             
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
! *****    TEST OF SIGN                                                  
! *****                                                                  
        write(nuvi, 16102)                                              
16102 format (/ 8x, "TEST OF SIGN" )                                  
! T001*  TEST 1                                         BOTH VALUES ZERO 
           ivtnum = 1                                                   
        rfbvs = 0.0                                                     
        rfavs = sign(rfbvs, rfbvs)                                      
           if (rfavs + 0.00005) 20010, 10010, 40010                     
40010 if (rfavs - 0.00005) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0011      continue                                                     
! T002*  TEST 2                        FIRST VALUE POSITIVE, SECOND ZERO 
           ivtnum = 2                                                   
        rfbvs = 1.5                                                     
        rfdvs = 0.0                                                     
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs - 1.4999) 20020, 10020, 40020                      
40020 if (rfavs - 1.5001) 10020, 10020, 20020                      
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 1.5                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0021      continue                                                     
! T003*  TEST 3                        FIRST VALUE NEGATIVE, SECOND ZERO 
           ivtnum = 3                                                   
        rfbvs = -1.5                                                    
        rfdvs = 0.0                                                     
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs - 1.4999) 20030, 10030, 40030                      
40030 if (rfavs - 1.5001) 10030, 10030, 20030                      
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 1.5                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0031      continue                                                     
! T004*  TEST 4                        FIRST VALUE ZERO, SECOND POSITIVE 
           ivtnum = 4                                                   
        rfbvs = 0.0                                                     
        rfdvs = 2.5                                                     
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs + 0.00005) 20040, 10040, 40040                     
40040 if (rfavs - 0.00005) 10040, 10040, 20040                     
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0041      continue                                                     
! T005*  TEST 5                                     BOTH VALUES POSITIVE 
           ivtnum = 5                                                   
        rfbvs = 1.5                                                     
        rfdvs = 2.5                                                     
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs - 1.4999) 20050, 10050, 40050                      
40050 if (rfavs - 1.5001) 10050, 10050, 20050                      
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 1.5                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0051      continue                                                     
! T006*  TEST 6                    FIRST VALUE NEGATIVE, SECOND POSITIVE 
           ivtnum = 6                                                   
        rfbvs = -1.5                                                    
        rfdvs = 2.5                                                     
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs - 1.4999) 20060, 10060, 40060                      
40060 if (rfavs - 1.5001) 10060, 10060, 20060                      
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = 1.5                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0061      continue                                                     
! T007*  TEST 7                        FIRST VALUE ZERO, SECOND NEGATIVE 
           ivtnum = 7                                                   
        rfbvs = 0.0                                                     
        rfdvs = -2.5                                                    
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs + 0.00005) 20070, 10070, 40070                     
40070 if (rfavs - 0.00005) 10070, 10070, 20070                     
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0071      continue                                                     
! T008*  TEST 8                                     BOTH VALUES NEGATIVE 
           ivtnum = 8                                                   
        rfbvs = -1.5                                                    
        rfdvs = -2.5                                                    
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs + 1.5001) 20080, 10080, 40080                      
40080 if (rfavs + 1.4999) 10080, 10080, 20080                      
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = -1.5                                                
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0081      continue                                                     
! T009*  TEST 9                    FIRST VALUE POSITIVE, SECOND NEGATIVE 
           ivtnum = 9                                                   
        rfbvs = 1.5                                                     
        rfdvs = -2.5                                                    
        rfavs = sign(rfbvs, rfdvs)                                      
           if (rfavs + 1.5001) 20090, 10090, 40090                      
40090 if (rfavs + 1.4999) 10090, 10090, 20090                      
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = -1.5                                                
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0091      continue                                                     
! T010*  TEST 10     BOTH VALUES ZERO, 1ST ZERO PRECEDED BY A MINUS SIGN 
           ivtnum = 10                                                  
        rfdvs = 0.0                                                     
        rfevs = 0.0                                                     
        rfavs = sign(-rfdvs, rfevs)                                     
           if (rfavs + 0.0005) 20100, 10100, 40100                      
40100 if (rfavs - 0.00005) 10100, 10100, 20100                     
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 0.0                                                 
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0101      continue                                                     
! T011*  TEST 11                ARITHMETIC EXPRESSIONS PRESENTED TO SIGN 
           ivtnum = 11                                                  
        rfdvs = 1.5                                                     
        rfevs = 2.0                                                     
        rfavs = sign(rfdvs + rfevs, rfdvs - rfevs)                      
           if (rfavs + 3.5002) 20110, 10110, 40110                      
40110 if (rfavs + 3.4998) 10110, 10110, 20110                      
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = -3.5                                                
           write (nuvi, 80012) ivtnum, rfavs, rvcorr                    
 0111      continue                                                     
! *****                                                                  
! *****    TEST OF ISIGN                                                 
! *****                                                                  
        write(nuvi, 16104)                                              
16104 format (/ 8x, "TEST OF ISIGN" )                                 
! *****                                                                  
! T012*  TEST 12                                        BOTH VALUES ZERO 
           ivtnum = 12                                                  
        ifbvi = 0                                                       
        ifdvi = 0                                                       
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 0) 20120, 10120, 20120                           
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0121      continue                                                     
! T013*  TEST 13                       FIRST VALUE POSITIVE, SECOND ZERO 
           ivtnum = 13                                                  
        ifbvi = 2                                                       
        ifdvi = 0                                                       
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 2) 20130, 10130, 20130                           
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           ivcorr = 2                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0131      continue                                                     
! T014*  TEST 14                       FIRST VALUE NEGATIVE, SECOND ZERO 
           ivtnum = 14                                                  
        ifbvi = -2                                                      
        ifdvi = 0                                                       
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 2) 20140, 10140, 20140                           
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           ivcorr = 2                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0141      continue                                                     
! T015*  TEST 15                       FIRST VALUE ZERO, SECOND POSITIVE 
           ivtnum = 15                                                  
        ifbvi = 0                                                       
        ifdvi = 5                                                       
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 0) 20150, 10150, 20150                           
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0151      continue                                                     
! T016*  TEST 16                                    BOTH VALUES POSITIVE 
           ivtnum = 16                                                  
        ifbvi = 2                                                       
        ifdvi = 5                                                       
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 2) 20160, 10160, 20160                           
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           ivcorr = 2                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0161      continue                                                     
! T017*  TEST 17                   FIRST VALUE NEGATIVE, SECOND POSITIVE 
           ivtnum = 17                                                  
        ifbvi = -2                                                      
        ifdvi = 5                                                       
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 2) 20170, 10170, 20170                           
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           ivcorr = 2                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0171      continue                                                     
! T018*  TEST 18                       FIRST VALUE ZERO, SECOND NEGATIVE 
           ivtnum = 18                                                  
        ifbvi = 0                                                       
        ifdvi = -5                                                      
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi - 0) 20180, 10180, 20180                           
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0181      continue                                                     
! T019*  TEST 19                                    BOTH VALUES NEGATIVE 
           ivtnum = 19                                                  
        ifbvi = -2                                                      
        ifdvi = -5                                                      
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi + 2) 20190, 10190, 20190                           
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           ivcorr = -2                                                  
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0191      continue                                                     
! T020*  TEST 20                   FIRST VALUE POSITIVE, SECOND NEGATIVE 
           ivtnum = 20                                                  
        ifbvi = 2                                                       
        ifdvi = -5                                                      
        ifavi = isign(ifbvi, ifdvi)                                     
           if (ifavi + 2) 20200, 10200, 20200                           
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           ivcorr = -2                                                  
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0201      continue                                                     
! T021*  TEST 21     BOTH VALUES ZERO, 1ST ZERO PRECEDED BY A MINUS SIGN 
           ivtnum = 21                                                  
        ifdvi = 0                                                       
        ifevi = 0                                                       
        ifavi = isign(-ifdvi, ifevi)                                    
           if (ifavi - 0) 20210, 10210, 20210                           
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
 0211      continue                                                     
! T022*  TEST 22               ARITHMETIC EXPRESSIONS PRESENTED TO ISIGN 
           ivtnum = 22                                                  
        ifdvi = 2                                                       
        ifevi = 3                                                       
        ifavi = isign(ifdvi + ifevi, ifdvi - ifevi)                     
           if (ifavi + 5) 20220, 10220, 20220                           
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, ifavi, ivcorr                    
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
! *****    END OF TEST SEGMENT 161                                       
      stop                                                              
      end program fm359
