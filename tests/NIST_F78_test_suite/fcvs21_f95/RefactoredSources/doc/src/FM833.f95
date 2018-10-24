      program fm833
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM833                                                          
! *****                       YGEN6 - (211)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****      TEST GENERIC FUNCTIONS                               15.3   
! *****       SPECIFIC AND GENERIC NAME OF SAME FUNCTION WITH    TABLE 5 
! *****       SAME TYPE OF ARGUMENT IN A STATEMENT                       
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
! *****  S P E C I F I C A T I O N S  SEGMENT 211                        
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
      integer :: kvi
      integer :: jvi
      integer :: lvi
      integer :: ivcorr
      real :: bvs
      real :: cvs
      real :: avs
      real :: rvcorr
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
        double precision :: dvcorr
        complex :: avc
        complex :: bvc
        complex :: zvcorr
        real, dimension(1:2) :: r2e
        equivalence (avc, r2e)                                          
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
      ivtotl = 11                                                       
      zprog = 'FM833'                                                   
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
! *****    HEADER FOR SEGMENT 211                                        
        write(nuvi,21100)                                               
21100 format( " ", /  " YGEN6 - (211) GENERIC FUNCTIONS --" //          "  SPECIFIC AND GENERIC NAME OF SAME FUNCTION IN A STATEMENT" //  "  ANS REF. - 15.3" )                                           
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
! T001*  TEST 1                      TEST OF ISIGN AND SIGN WITH INTEGER 
           ivtnum = 1                                                   
        kvi = 5                                                         
        jvi = -3                                                        
        lvi = isign(kvi, jvi) - sign(kvi, jvi)                          
           if (lvi -  0) 20010, 10010, 20010                            
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           ivcorr =     0                                               
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0011      continue                                                     
! T002*  TEST 2                         TEST OF AMAX1 AND MAX WITH REALS 
           ivtnum = 2                                                   
        bvs = 2.5                                                       
        cvs = 3.5                                                       
        avs = amax1(bvs, cvs) - max(bvs, cvs)                           
           if (avs + 0.50000e-04) 20020, 10020, 40020                   
40020 if (avs - 0.50000e-04) 10020, 10020, 20020                   
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 0.0000                                              
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                    TEST OF DEXP AND EXP WITH DOUBLE PREC 
           ivtnum = 3                                                   
        bvd = 1.0d0                                                     
        avd = dexp(bvd) - exp(bvd)                                      
           if (avd + 0.5000000000d-09) 20030, 10030, 40030              
40030 if (avd - 0.5000000000d-09) 10030, 10030, 20030              
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000d+00                                      
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                  TEST OF DTANH AND TANH WITH DOUBLE PREC 
           ivtnum = 4                                                   
        bvd = 0.5d0                                                     
        avd = dtanh(bvd) - tanh(bvd)                                    
           if (avd + 0.5000000000d-09) 20040, 10040, 40040              
40040 if (avd - 0.5000000000d-09) 10040, 10040, 20040              
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000d+00                                      
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5                  TEST OF DASIN AND ASIN WITH DOUBLE PREC 
           ivtnum = 5                                                   
        bvd = -1.0d0                                                    
        avd = dasin(bvd) - asin(bvd)                                    
           if (avd + 0.5000000000d-09) 20050, 10050, 40050              
40050 if (avd - 0.5000000000d-09) 10050, 10050, 20050              
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000d+00                                      
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0051      continue                                                     
! T006*  TEST 6                 TEST OF DNINT AND ANINT WITH DOUBLE PREC 
           ivtnum = 6                                                   
        bvd = 2.75d0                                                    
        avd = dnint(bvd) - anint(bvd)                                   
           if (avd + 0.5000000000d-09) 20060, 10060, 40060              
40060 if (avd - 0.5000000000d-09) 10060, 10060, 20060              
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000d+00                                      
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0061      continue                                                     
! T007*  TEST 7                    TEST OF DMOD AND MOD WITH DOUBLE PREC 
           ivtnum = 7                                                   
        bvd = 6.0d0                                                     
        cvd = 3.0d0                                                     
        avd = dmod(bvd, cvd) - mod(bvd, cvd)                            
           if (avd + 0.5000000000d-09) 20070, 10070, 40070              
40070 if (avd - 0.5000000000d-09) 10070, 10070, 20070              
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 0.00000000d+00                                      
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0071      continue                                                     
! T008*  TEST 8                        TEST OF CABS AND ABS WITH COMPLEX 
           ivtnum = 8                                                   
        bvc = (4.0, 3.0)                                                
        avc = cabs(bvc) - abs(bvc)                                      
           if (r2e(1) + 0.50000e-04) 20080, 40082, 40081                
40081 if (r2e(1) - 0.50000e-04) 40082, 40082, 20080                
40082 if (r2e(2) + 0.50000e-04) 20080, 10080, 40080                
40080 if (r2e(2) - 0.50000e-04) 10080, 10080, 20080                
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           zvcorr = ( 0.0000,  0.0000)                                  
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0081      continue                                                     
! T009*  TEST 9                      TEST OF CSQRT AND SQRT WITH COMPLEX 
           ivtnum = 9                                                   
        bvc = (3.0, 4.0)                                                
        avc = csqrt(bvc) - sqrt(bvc)                                    
           if (r2e(1) + 0.50000e-04) 20090, 40092, 40091                
40091 if (r2e(1) - 0.50000e-04) 40092, 40092, 20090                
40092 if (r2e(2) + 0.50000e-04) 20090, 10090, 40090                
40090 if (r2e(2) - 0.50000e-04) 10090, 10090, 20090                
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           zvcorr = ( 0.0000,  0.0000)                                  
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0091      continue                                                     
! T010*  TEST 10                       TEST OF CLOG AND LOG WITH COMPLEX 
           ivtnum = 10                                                  
        bvc = (1.0, 0.0)                                                
        avc = clog(bvc) - log(bvc)                                      
           if (r2e(1) + 0.50000e-04) 20100, 40102, 40101                
40101 if (r2e(1) - 0.50000e-04) 40102, 40102, 20100                
40102 if (r2e(2) + 0.50000e-04) 20100, 10100, 40100                
40100 if (r2e(2) - 0.50000e-04) 10100, 10100, 20100                
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           zvcorr = ( 0.0000,  0.0000)                                  
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0101      continue                                                     
! T011*  TEST 11                       TEST OF CSIN AND SIN WITH COMPLEX 
           ivtnum = 11                                                  
         bvc = (1.5, 3.5)                                               
         avc = csin(bvc) - sin(bvc)                                     
            if (r2e(1) + 0.50000e-04) 20110, 40112, 40111               
40111 if (r2e(1) - 0.50000e-04) 40112, 40112, 20110               
40112 if (r2e(2) + 0.50000e-04) 20110, 10110, 40110               
40110 if (r2e(2) - 0.50000e-04) 10110, 10110, 20110               
10110 ivpass = ivpass + 1                                         
            write (nuvi, 80002) ivtnum                                  
            goto 0111                                                  
20110 ivfail = ivfail + 1                                         
            zvcorr = ( 0.0000,  0.0000)                                 
            write (nuvi, 80045) ivtnum, avc, zvcorr                     
 0111       continue                                                    
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
! *****    END OF TEST SEGMENT 211                                       
      stop                                                              
      end program fm833
