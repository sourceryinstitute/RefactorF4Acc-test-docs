      program fm363
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM363               X66MX - (171)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                       SUBSET REF
! *****    TEST THAT ALL INTRINSIC FUNCTIONS WOULD ACCEPT         15.3   
! *****    ANY EXPRESSION OF THE TYPE SPECIFIED IN THE          (TABLE 5)
! *****    INTRINSIC FUNCTION TABLE - ANS REFS - 15.10                   
! *****                                                                  
! *****  GENERAL COMMENTS                                                
! *****    SEGMENTS XINT, XREAL, XAINT, XABS, XAMOD,                     
! *****    XSIGN, XDIM, XMAX, XMIN ASSUMED WORKING                       
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
      real :: rjbvs
      integer :: ijavi
      integer :: ivcorr
      integer :: ijbvi
      integer :: ijdvi
      integer :: ijevi
      real :: rjdvs
      real :: rjevs
      real :: rjfvs
      real :: rjavs
      real :: rvcorr
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
      ivtotl = 14                                                       
      zprog = 'FM363'                                                   
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
! *****    HEADER FOR SEGMENT 171 WRITTEN                                
        write (nuvi,17101)                                              
17101 format(" ",// 2x,"X66MX - (171) SUBSET INTRINSIC FUNCTIONS--" //          10x,"IN ARITHMETIC EXPRESSIONS"                                   //2x, " SUBSET REF. - 15.10, 6.1.4" )                   
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
! *****    TEST OF INTRINSIC FUNCTIONS IN EXPRESSIONS                    
! *****                                                                  
! T001*  TEST 1                                                          
           ivtnum = 1                                                   
        rjbvs = 5.2                                                     
        ijavi = int(rjbvs) + 3                                          
           if (ijavi - 8) 20010, 10010, 20010                           
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           ivcorr = 8                                                   
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0011      continue                                                     
! T002*  TEST 2                                                          
           ivtnum = 2                                                   
        rjbvs = 4.8                                                     
        ijavi = ifix(rjbvs) - 2                                         
           if (ijavi - 2) 20020, 10020, 20020                           
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           ivcorr = 2                                                   
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0021      continue                                                     
! T003*  TEST 3                                                          
           ivtnum = 3                                                   
        rjbvs = 2.8                                                     
        ijavi = 50 * nint(rjbvs)                                        
           if (ijavi - 150) 20030, 10030, 20030                         
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           ivcorr = 150                                                 
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0031      continue                                                     
! T004*  TEST 4                                                          
           ivtnum = 4                                                   
        ijbvi = -4                                                      
        ijavi = iabs(ijbvi) / (-4)                                      
           if (ijavi + 1) 20040, 10040, 20040                           
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           ivcorr = -1                                                  
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0041      continue                                                     
! T005*  TEST 5                                                          
           ivtnum = 5                                                   
        ijbvi = 7                                                       
        ijdvi = 4                                                       
        ijavi = mod(ijbvi, ijdvi) ** 2                                  
           if (ijavi - 9) 20050, 10050, 20050                           
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           ivcorr = 9                                                   
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0051      continue                                                     
! T006*  TEST 6                                                          
           ivtnum = 6                                                   
        ijbvi = -3                                                      
        ijdvi = 1                                                       
        ijavi = 2 ** isign(ijbvi, ijdvi)                                
           if (ijavi - 8) 20060, 10060, 20060                           
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           ivcorr = 8                                                   
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0061      continue                                                     
! T007*  TEST 7                                                          
           ivtnum = 7                                                   
        ijbvi = 5                                                       
        ijdvi = 2                                                       
        ijevi = -2                                                      
        ijavi = idim(ijbvi, ijdvi) * 2 + max0(ijevi, ijdvi) - 7         
           if (ijavi - 1) 20070, 10070, 20070                           
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0071      continue                                                     
! T008*  TEST 8                                                          
           ivtnum = 8                                                   
        ijbvi = 2                                                       
        ijdvi = 3                                                       
        rjbvs = 2.2                                                     
        rjdvs = 4.8                                                     
        rjevs = -2.2                                                    
        rjfvs = -3.8                                                    
        ijavi = min0(ijbvi, ijdvi) * 2 - max1(rjbvs, rjdvs) / 2         + min1(rjevs, rjfvs) + 5                                   
           if (ijavi - 4) 20080, 10080, 20080                           
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           ivcorr = 4                                                   
           write (nuvi, 80010) ivtnum, ijavi, ivcorr                    
 0081      continue                                                     
! T009*  TEST 9                                                          
           ivtnum = 9                                                   
        ijbvi = 2                                                       
        rjavs = float(ijbvi) + 3.5                                      
           if (rjavs - 5.4997) 20090, 10090, 40090                      
40090 if (rjavs - 5.5003) 10090, 10090, 20090                      
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 5.5                                                 
           write (nuvi, 80012) ivtnum, rjavs, rvcorr                    
 0091      continue                                                     
! T010*  TEST 10                                                         
           ivtnum = 10                                                  
        ijbvi = 2                                                       
        rjavs = real(ijbvi) * 3.0                                       
           if (rjavs - 5.9997) 20100, 10100, 40100                      
40100 if (rjavs - 6.0003) 10100, 10100, 20100                      
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 6.0                                                 
           write (nuvi, 80012) ivtnum, rjavs, rvcorr                    
 0101      continue                                                     
! T011*  TEST 11                                                         
           ivtnum = 11                                                  
        rjbvs = 4.5                                                     
        rjavs = aint(rjbvs) ** 0.5                                      
           if (rjavs - 1.9999) 20110, 10110, 40110                      
40110 if (rjavs - 2.0001) 10110, 10110, 20110                      
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 2.0                                                 
           write (nuvi, 80012) ivtnum, rjavs, rvcorr                    
 0111      continue                                                     
! T012*  TEST 12                                                         
           ivtnum = 12                                                  
        rjbvs = 2.8                                                     
        rjdvs = 2.2                                                     
        rjavs = 1.5 * anint(rjbvs) + 6.6 / abs(rjdvs)                   
           if (rjavs - 7.4996 ) 20120, 10120, 40120                     
40120 if (rjavs - 7.5004 ) 10120, 10120, 20120                     
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 7.5                                                 
           write (nuvi, 80012) ivtnum, rjavs, rvcorr                    
 0121      continue                                                     
! T013*  TEST 13                                                         
           ivtnum = 13                                                  
        rjbvs = 4.5                                                     
        rjdvs = 2.2                                                     
        ijbvi = -5                                                      
        ijdvi = 5                                                       
        rjavs = (amod(rjbvs, rjdvs) + 1.4) * (isign(ijbvi, ijdvi) - 3.0)
           if (rjavs - 2.9998) 20130, 10130, 40130                      
40130 if (rjavs - 3.0002) 10130, 10130, 20130                      
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = 3.0                                                 
           write (nuvi, 80012) ivtnum, rjavs, rvcorr                    
 0131      continue                                                     
! T014*  TEST 14                                                         
           ivtnum = 14                                                  
        rjbvs = 6.2                                                     
        rjdvs = 5.2                                                     
        ijbvi = 2                                                       
        ijdvi = 3                                                       
        rjevs = 2.0                                                     
        rjfvs = 3.0                                                     
        rjavs = (dim(rjbvs, rjdvs) * amax0(ijbvi, ijdvi)) **                   (amin0(ijbvi, ijdvi) - amin1(rjevs, rjfvs))                
           if (rjavs - 0.99995) 20140, 10140, 40140                     
40140 if (rjavs - 1.0001) 10140, 10140, 20140                      
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = 1.0                                                 
           write (nuvi, 80012) ivtnum, rjavs, rvcorr                    
 0141      continue                                                     
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
! *****    END OF TEST SEGMENT 171                                       
        stop                                                            
        end program fm363
