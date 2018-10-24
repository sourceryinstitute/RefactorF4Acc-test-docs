      program fm818
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM818                                                          
! *****                       YDLG10 - (185)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTION DLOG10                         15.3   
! *****                                                          TABLE 5 
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
! *****  S P E C I F I C A T I O N S  SEGMENT 185                        
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
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
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
      ivtotl = 15                                                       
      zprog = 'FM818'                                                   
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
! *****    HEADER FOR SEGMENT 185                                        
        write(nuvi,18500)                                               
18500 format(" ", / "  YDLG10 - (185) INTRINSIC FUNCTIONS" //                  "  DLOG10 (DOUBLE PRECISION COMMON LOGARITHM)" //                 "  ANS REF. - 15.3" )                                    
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
! T001*  TEST 1                                 ONE, SINCE LN(1.0) = 0.0 
           ivtnum = 1                                                   
        bvd = 1.0d0                                                     
        avd = dlog10(bvd)                                               
           if (avd + 0.5000000000d-09) 20010, 10010, 40010              
40010 if (avd - 0.5000000000d-09) 10010, 10010, 20010              
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000000000000000d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                      A VALUE CLOSE TO 10 
           ivtnum = 2                                                   
        avd = dlog10(9.875d0)                                           
           if (avd - 0.9945371038d+00) 20020, 10020, 40020              
40020 if (avd - 0.9945371048d+00) 10020, 10020, 20020              
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = 0.99453710429849784235d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                          THE VALUE 10.D0 
           ivtnum = 3                                                   
        avd = dlog10(10.0d0)                                            
           if (avd - 0.9999999995d+00) 20030, 10030, 40030              
40030 if (avd - 0.1000000001d+01) 10030, 10030, 20030              
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 1.0000000000000000000d+00                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                         THE VALUE 20.5D0 
           ivtnum = 4                                                   
        avd = dlog10(20.5d0)                                            
           if (avd - 0.1311753860d+01) 20040, 10040, 40040              
40040 if (avd - 0.1311753862d+01) 10040, 10040, 20040              
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 1.3117538610557542993d+00                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                         THE VALUE 99.0D0 
           ivtnum = 5                                                   
        avd = dlog10(99.0d0)                                            
           if (avd - 0.1995635193d+01) 20050, 10050, 40050              
40050 if (avd - 0.1995635196d+01) 10050, 10050, 20050              
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 1.9956351945975499153d+00                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0051      continue                                                     
! T006*  TEST 6                            VARIABLE WITHIN AN EXPRESSION 
           ivtnum = 6                                                   
        bvd = 1.0d0                                                     
        cvd = 8.0d0                                                     
        avd = dlog10(3.0d0 * bvd / cvd)                                 
           if (avd + 0.4259687325d+00) 20060, 10060, 40060              
40060 if (avd + 0.4259687320d+00) 10060, 10060, 20060              
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = -0.42596873227228114835d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0061      continue                                                     
! T007*  TEST 7                            VARIABLE WITHIN AN EXPRESSION 
           ivtnum = 7                                                   
        bvd = 1.0d0                                                     
        cvd = 8.0d0                                                     
        avd = dlog10(5.0d0 * bvd / cvd)                                 
           if (avd + 0.2041199828d+00) 20070, 10070, 40070              
40070 if (avd + 0.2041199825d+00) 10070, 10070, 20070              
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = -0.20411998265592478085d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0071      continue                                                     
! T008*  TEST 8                         AN EXPRESSION SUPPLIED TO DLOG10 
           ivtnum = 8                                                   
        avd = dlog10(75.d0 / 100.0d0)                                   
           if (avd + 0.1249387367d+00) 20080, 10080, 40080              
40080 if (avd + 0.1249387365d+00) 10080, 10080, 20080              
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = -0.12493873660829995313d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0081      continue                                                     
! T009*  TEST 9                            VARIABLE WITHIN AN EXPRESSION 
           ivtnum = 9                                                   
        bvd = 1.0d0                                                     
        cvd = 8.0d0                                                     
        avd = dlog10(7.0d0 * bvd / cvd)                                 
           if (avd + 0.5799194701d-01) 20090, 10090, 40090              
40090 if (avd + 0.5799194694d-01) 10090, 10090, 20090              
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = -0.057991946977686754929d+00                        
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0091      continue                                                     
! T010*  TEST 10                                    A VALUE CLOSE TO ONE 
           ivtnum = 10                                                  
        avd = dlog10(0.9921875d0)                                       
           if (avd + 0.3406248694d-02) 20100, 10100, 40100              
40100 if (avd + 0.3406248690d-02) 10100, 10100, 20100              
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = -0.0034062486919115022492d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0101      continue                                                     
! T012*  TEST 11                                   A VALUE CLOSE TO ZERO 
           ivtnum = 11                                                  
        bvd = 256.0d0                                                   
        avd = dlog10(1.0d0 / bvd)                                       
           if (avd + 0.2408239967d+01) 20120, 10120, 40120              
40120 if (avd + 0.2408239964d+01) 10120, 10120, 20120              
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr = -2.4082399653118495617d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0121      continue                                                     
! T013*  TEST 12                                   A VALUE CLOSE TO ZERO 
           ivtnum = 12                                                  
        bvd = 128.0d0                                                   
        avd = dlog10(1.0d0 / (bvd * 8d0))                               
           if (avd + 0.3010299959d+01) 20130, 10130, 40130              
40130 if (avd + 0.3010299955d+01) 10130, 10130, 20130              
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr = -3.0102999566398119521d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0131      continue                                                     
! T014*  TEST 13                           AN ARGUMENT OF HIGH MAGNITUDE 
           ivtnum = 13                                                  
        bvd = 2.0d+35                                                   
        avd = dlog10(bvd)                                               
           if (avd - 0.3530102997d+01) 20140, 10140, 40140              
40140 if (avd - 0.3530103002d+02) 10140, 10140, 20140              
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr = 35.301029995663981195d+00                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0141      continue                                                     
! T015*  TEST 14                            AN ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 14                                                  
        bvd = 2.0d-35                                                   
        avd = dlog10(bvd)                                               
           if (avd + 0.3469897003d+02) 20150, 10150, 40150              
40150 if (avd + 0.3469896998d+02) 10150, 10150, 20150              
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           dvcorr = -34.698970004336018805d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0151      continue                                                     
! T016*  TEST 15                               THE FUNCTION APPIED TWICE 
           ivtnum = 15                                                  
        avd = dlog10(20.0d0) - dlog10(2.0d0)                            
           if (avd - 0.9999999995d+00) 20160, 10160, 40160              
40160 if (avd - 0.1000000001d+01) 10160, 10160, 20160              
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr = 1.00000000000000d+00                                
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0161      continue                                                     
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
! *****    END OF TEST SEGMENT 185                                       
      stop                                                              
      end program fm818
