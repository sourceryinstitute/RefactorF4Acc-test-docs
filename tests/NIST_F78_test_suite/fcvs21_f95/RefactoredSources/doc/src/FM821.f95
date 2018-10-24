      program fm821
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM821                                                          
! *****                       YDCOS - (190)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTION DCOS                           15.3   
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
! *****    S P E C I F I C A T I O N S SEGMENT 190                       
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
      ivtotl = 19                                                       
      zprog = 'FM821'                                                   
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
! *****    HEADER FOR SEGMENT 190                                        
        write(nuvi,19000)                                               
19000 format(" "/" YDCOS - (190) INTRINSIC FUNCTIONS" //                       "  DCOS (DOUBLE PRECISION COSINE)" //                             "  ANS REF. - 15.3" )                                    
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
! T001*  TEST 1                               ZERO (0.0), SINCE COS(0)=1 
           ivtnum = 1                                                   
        bvd = 0.0d0                                                     
        avd = dcos(bvd)                                                 
           if (avd - 0.9999999995d+00) 20010, 10010, 40010              
40010 if (avd - 0.1000000001d+01) 10010, 10010, 20010              
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 1.00000000000000000000d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                           VALUES NEAR PI 
           ivtnum = 2                                                   
        avd = dcos(pivd)                                                
           if (avd + 0.1000000001d+01) 20020, 10020, 40020              
40020 if (avd + 0.9999999995d+00) 10020, 10020, 20020              
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = -1.00000000000000000000d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                                PI - 1/16 
           ivtnum = 3                                                   
        bvd = 3.07909265358979323846d0                                  
        avd = dcos(bvd)                                                 
           if (avd + 0.9980475112d+00) 20030, 10030, 40030              
40030 if (avd + 0.9980475102d+00) 10030, 10030, 20030              
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = -0.99804751070009914963d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                                PI + 1/32 
           ivtnum = 4                                                   
        avd = dcos(3.17284265358979323846d0)                            
           if (avd + 0.9995117590d+00) 20040, 10040, 40040              
40040 if (avd + 0.9995117580d+00) 10040, 10040, 20040              
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = -0.99951175848513636924d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                         VALUES NEAR 2*PI 
           ivtnum = 5                                                   
        bvd = pivd * 2.0d0                                              
        avd = dcos(bvd)                                                 
           if (avd - 0.9999999995d+00) 20050, 10050, 40050              
40050 if (avd - 0.1000000001d+01) 10050, 10050, 20050              
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 1.00000000000000000000d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0051      continue                                                     
! T006*  TEST 6                                         VALUES NEAR 2*PI 
           ivtnum = 6                                                   
        bvd = (2.0d0 * pivd) - 1.0d0 / 64.0d0                           
        avd = dcos(bvd)                                                 
           if (avd - 0.9998779316d+00) 20060, 10060, 40060              
40060 if (avd - 0.9998779327d+00) 10060, 10060, 20060              
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 0.99987793217100665474d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0061      continue                                                     
! T007*  TEST 7                                         VALUES NEAR 2*PI 
           ivtnum = 7                                                   
        bvd = (2.0d0 * pivd) + 1.0d0 / 128.0d0                          
        avd = dcos(bvd)                                                 
           if (avd - 0.9999694820d+00) 20070, 10070, 40070              
40070 if (avd - 0.9999694831d+00) 10070, 10070, 20070              
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 0.99996948257709511331d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0071      continue                                                     
! T008*  TEST 8                          AN EXPRESSION PRESENTED TO DCOS 
           ivtnum = 8                                                   
        bvd = 350.0d1                                                   
        avd = dcos(bvd / 100.0d1)                                       
           if (avd + 0.9364566878d+00) 20080, 10080, 40080              
40080 if (avd + 0.9364566868d+00) 10080, 10080, 20080              
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = -0.93645668729079633770d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                      A NEGATIVE ARGUMENT 
           ivtnum = 9                                                   
        bvd = -1.5d0                                                    
        avd = dcos(bvd)                                                 
           if (avd - 0.7073720163d-01) 20090, 10090, 40090              
40090 if (avd - 0.7073720171d-01) 10090, 10090, 20090              
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = 0.070737201667702910088d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0091      continue                                                     
! T010*  TEST 10                LARGE VALUES TO CHECK ARGUMENT REDUCTION 
           ivtnum = 10                                                  
        avd = dcos(200.0d0)                                             
           if (avd - 0.4871876747d+00) 20100, 10100, 40100              
40100 if (avd - 0.4871876753d+00) 10100, 10100, 20100              
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 0.48718767500700591035d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0101      continue                                                     
! T011*  TEST 11                LARGE VALUES TO CHECK ARGUMENT REDUCTION 
           ivtnum = 11                                                  
        avd = dcos(-31416.0d0)                                          
           if (avd - 0.9973027257d+00) 20110, 10110, 40110              
40110 if (avd - 0.9973027268d+00) 10110, 10110, 20110              
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr = 0.99730272627420107808d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0111      continue                                                     
! T012*  TEST 12                                        VALUES NEAR PI/2 
           ivtnum = 12                                                  
        avd = dcos(1.57079632679489661923d0)                            
           if (avd + 0.5000000000d-09) 20120, 10120, 40120              
40120 if (avd - 0.5000000000d-09) 10120, 10120, 20120              
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000000000000000d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0121      continue                                                     
! T013*  TEST 13                                         (PI / 2) - 1/32 
           ivtnum = 13                                                  
        bvd = (1.53954632679489661923d0)                                
        avd = dcos(bvd)                                                 
           if (avd - 0.3124491397d-01) 20130, 10130, 40130              
40130 if (avd - 0.3124491400d-01) 10130, 10130, 20130              
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr = 0.031244913985326078739d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0131      continue                                                     
! T014*  TEST 14                                         (PI / 2) + 1/16 
           ivtnum = 14                                                  
        avd = dcos(1.63329632679489661923d0)                            
           if (avd + 0.6245931788d-01) 20140, 10140, 40140              
40140 if (avd + 0.6245931781d-01) 10140, 10140, 20140              
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr = -0.062459317842380198585d+00                        
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0141      continue                                                     
! T015*  TEST 15                                      VALUES NEAR 3*PI/2 
           ivtnum = 15                                                  
        bvd = 3.0d0 * pivd / 2.0d0                                      
        avd = dcos(bvd)                                                 
           if (avd + 0.5000000000d-09) 20150, 10150, 40150              
40150 if (avd - 0.5000000000d-09) 10150, 10150, 20150              
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000000000000000d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0151      continue                                                     
! T016*  TEST 16                                      VALUES NEAR 3*PI/2 
           ivtnum = 16                                                  
        bvd = (3.0d0 * pivd / 2.0d0) + 1.0d0 / 16.0d0                   
        avd = dcos(bvd)                                                 
           if (avd - 0.6245931781d-01) 20160, 10160, 40160              
40160 if (avd - 0.6245931788d-01) 10160, 10160, 20160              
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr = 0.062459317842380198585d+00                         
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0161      continue                                                     
! T017*  TEST 17                                      VALUES NEAR 3*PI/2 
           ivtnum = 17                                                  
        bvd = (3.0d0 * pivd / 2.0d0) - 1.0d0 / 512.0d0                  
        avd = dcos(bvd)                                                 
           if (avd + 0.1953123760d-02) 20170, 10170, 40170              
40170 if (avd + 0.1953123757d-02) 10170, 10170, 20170              
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           dvcorr = -0.0019531237582368040269d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0171      continue                                                     
! T018*  TEST 18                               ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 18                                                  
        bvd = -3.1415926535898d-35                                      
        avd = dcos(bvd)                                                 
           if (avd - 0.9999999995d+00) 20180, 10180, 40180              
40180 if (avd - 0.1000000001d+01) 10180, 10180, 20180              
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           dvcorr = 1.00000000000000000000d+00                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0181      continue                                                     
! T019*  TEST 19                              THE FUNCTION APPLIED TWICE 
           ivtnum = 19                                                  
        avd = dcos(pivd / 4.0d0) * dcos(3.0d0 * pivd / 4.0d0)           
           if (avd + 0.5000000003d+00) 20190, 10190, 40190              
40190 if (avd + 0.4999999997d+00) 10190, 10190, 20190              
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           dvcorr = -0.5000000000000000000000d+00                       
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
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
! *****  END OF TEST SEGMENT 190                                         
      stop                                                              
      end program fm821
