      program fm822
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM822                                                          
! *****                       YDTAN - (192)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTION DTAN                           15.3   
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
! *****                                                                  
! *****    S P E C I F I C A T I O N S SEGMENT 192                       
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
        double precision :: pivd
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
      ivtotl = 14                                                       
      zprog = 'FM822'                                                   
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
! *****    HEADER FOR SEGMENT 192                                        
        write(nuvi,19200)                                               
19200 format(" ", / "  YDTAN - (192) INTRINSIC FUNCTIONS" //                   "  DTAN  (DOUBLE PRECISION TANGENT)" //                           "  ANS REF. - 15.3" )                                    
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
        pivd = 3.1415926535897932384626434d0                            
! *****                                                                  
! T001*  TEST 1                            ZERO (0.0), SINCE TAN(0) = 0. 
           ivtnum = 1                                                   
        bvd = 0.0d0                                                     
        avd = dtan(bvd)                                                 
           if (avd +  0.5000000000d-09) 20010, 10010, 40010             
40010 if (avd -  0.5000000000d-09) 10010, 10010, 20010             
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr =    0.00000000000000000000d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                                     2*PI 
           ivtnum = 2                                                   
        bvd = 6.28318530717958647692d0                                  
        avd = dtan(bvd)                                                 
           if (avd +  0.5000000000d-09) 20020, 10020, 40020             
40020 if (avd -  0.5000000000d-09) 10020, 10020, 20020             
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr =    0.00000000000000000000d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                                     3*PI 
           ivtnum = 3                                                   
        bvd = 9.42477796076937971538d0                                  
        avd = dtan(bvd)                                                 
           if (avd +  0.5000000000d-09) 20030, 10030, 40030             
40030 if (avd -  0.5000000000d-09) 10030, 10030, 20030             
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr =    0.00000000000000000000d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                                     PI/4 
           ivtnum = 4                                                   
        avd = dtan(pivd / 4.0d0)                                        
           if (avd -  0.9999999995d+00) 20040, 10040, 40040             
40040 if (avd -  0.1000000001d+01) 10040, 10040, 20040             
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr =    1.00000000000000000000d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                                   5*PI/4 
           ivtnum = 5                                                   
        bvd = 5.0d0 * pivd / 4.0d0                                      
        avd = dtan(bvd)                                                 
           if (avd -  0.9999999995d+00) 20050, 10050, 40050             
40050 if (avd -  0.1000000001d+01) 10050, 10050, 20050             
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr =    1.00000000000000000000d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0051      continue                                                     
! T006*  TEST 6                                         A NEGATIVE VALUE 
           ivtnum = 6                                                   
        bvd = -2.0d0 / 1.0d0                                            
        avd = dtan(bvd)                                                 
           if (avd -  0.2185039862d+01) 20060, 10060, 40060             
40060 if (avd -  0.2185039865d+01) 10060, 10060, 20060             
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr =    2.1850398632615189916d+00                        
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0061      continue                                                     
! T007*  TEST 7                                         A POSITIVE VALUE 
           ivtnum = 7                                                   
        bvd = 350.0d0 / 100.0d0                                         
        avd = dtan(bvd)                                                 
           if (avd -  0.3745856399d+00) 20070, 10070, 40070             
40070 if (avd -  0.3745856404d+00) 10070, 10070, 20070             
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr =    0.37458564015859466633d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0071      continue                                                     
! T008*  TEST 8                                           (PI / 2) - 1/8 
           ivtnum = 8                                                   
        bvd = 1.44579632679489661923d0                                  
        avd = dtan(bvd)                                                 
           if (avd -  0.7958289861d+01) 20080, 10080, 40080             
40080 if (avd -  0.7958289870d+01) 10080, 10080, 20080             
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr =    7.9582898658670111779d+00                        
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                        (PI / 2) + 1/256  
           ivtnum = 9                                                   
        bvd = 1.57470257679489661923d0                                  
        avd = dtan(bvd)                                                 
           if (avd +  0.2559986981d+03) 20090, 10090, 40090             
40090 if (avd +  0.2559986977d+03) 10090, 10090, 20090             
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = -255.99869791534211708d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0091      continue                                                     
! T010*  TEST 10                                         3*PI/2 - 1/1024 
           ivtnum = 10                                                  
        avd = dtan((3.0d0 * pivd / 2.0d0) - 1.0d0 / 1024.0d0)           
           if (avd -  0.1023999674d+04) 20100, 10100, 40100             
40100 if (avd -  0.1023999675d+04) 10100, 10100, 20100             
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 1023.9996744791459706d+00                           
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0101      continue                                                     
! T011*  TEST 11                                             3*PI + 1/64 
           ivtnum = 11                                                  
        bvd = (3.0d0 * pivd / 2.0d0) + 1.0d0 / 64.0d0                   
        avd = dtan(bvd)                                                 
           if (avd +  0.6399479162d+02) 20110, 10110, 40110             
40110 if (avd +  0.6399479155d+02) 10110, 10110, 20110             
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr =  -63.994791581893645218d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0111      continue                                                     
! T012*  TEST 12                  LARGE VALUE TO TEST ARGUMENT REDUCTION 
           ivtnum = 12                                                  
        avd = dtan(2000.0d0)                                            
           if (avd +  0.2530998330d+01) 20120, 10120, 40120             
40120 if (avd +  0.2530998326d+01) 10120, 10120, 20120             
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr =   -2.5309983280933409104d+00                        
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0121      continue                                                     
! T013*  TEST 13                               ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 13                                                  
        bvd = pivd * 1.0d-15                                            
        avd = dtan(bvd)                                                 
           if (avd -  0.3141592652d-14) 20130, 10130, 40130             
40130 if (avd -  0.3141592655d-14) 10130, 10130, 20130             
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr =    3.1415926535897932385d-15                        
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0131      continue                                                     
! T014*  TEST 14                              THE FUNCTION APPLIED TWICE 
           ivtnum = 14                                                  
        avd = dtan(pivd / 6.0d0) * dtan(pivd / 6.0d0)                   
           if (avd -  0.3333333331d+00) 20140, 10140, 40140             
40140 if (avd -  0.3333333335d+00) 10140, 10140, 20140             
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr =    0.33333333333333333333d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
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
! *****    END OF TEST SEGMENT 192                                       
      stop                                                              
      end program fm822
