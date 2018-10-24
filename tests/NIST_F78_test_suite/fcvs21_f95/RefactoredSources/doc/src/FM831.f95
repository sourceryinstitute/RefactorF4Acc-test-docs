      program fm831
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM831                                                          
! *****                       YGEN3 - (208)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****      TEST GENERIC FUNCTIONS                               15.3   
! *****       ABS, MOD, SIGN, SIN, COS, TAN, SINH, COSH, TANH    TABLE 5 
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
! *****  S P E C I F I C A T I O N S  SEGMENT 208                        
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
      real :: rvcorr
      real :: bvs
        double precision :: avd
        double precision :: cvd
        double precision :: dvd
        double precision :: dvcorr
        complex :: avc
        complex :: cvc
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
      ivtotl = 12                                                       
      zprog = 'FM831'                                                   
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
! *****    HEADER FOR SEGMENT 208                                        
        write(nuvi,20800)                                               
20800 format( " ", /  " YGEN3 - (208) GENERIC FUNCTIONS --" //                  "  ABS, MOD, SIGN, SIN, COS, TAN, SINH, COSH, TANH" //            "  ANS REF. - 15.3" )                                   
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
! T001*  TEST 1                       TEST OF ABS AND SIGN WITH INTEGERS 
           ivtnum = 1                                                   
        lvi = abs(-25) - sign(2, -15)                                   
           if (lvi - 27) 20010, 10010, 20010                            
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           ivcorr = 27                                                  
           write (nuvi, 80010) ivtnum, lvi, ivcorr                      
 0011      continue                                                     
! T002*  TEST 2                     TEST OF MOD, SIGN AND ABS WITH REALS 
           ivtnum = 2                                                   
        avs = mod(24.5, 2.5) + sign(-1.50, -5.125) - abs(-63.5)         
           if (avs +  0.63004e+02) 20020, 10020, 40020                  
40020 if (avs +  0.62996e+02) 10020, 10020, 20020                  
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = -63.0                                               
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                     TEST OF SIN AND COS WITH DOUBLE PREC 
           ivtnum = 3                                                   
        cvd = 1.125d0                                                   
        avd = (sin(cvd)) ** 2 + (cos(cvd)) ** 2                         
           if (avd -  0.9999999995d+00) 20030, 10030, 40030             
40030 if (avd -  0.1000000001d+01) 10030, 10030, 20030             
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0031      continue                                                     
! T004*  TEST 4                     TEST OF TAN AND MOD WITH DOUBLE PREC 
           ivtnum = 4                                                   
        avd = tan(3.5d0) * mod(32.5d0, 5.0d0)                           
           if (avd -  0.9364640999d+00) 20040, 10040, 40040             
40040 if (avd -  0.9364641009d+00) 10040, 10040, 20040             
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 0.9364641003965d0                                   
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5                   TEST OF SINH AND COSH WITH DOUBLE PREC 
           ivtnum = 5                                                   
        cvd = 3.25d0                                                    
        avd = (sinh(cvd)) ** 2 - (cosh(cvd)) ** 2                       
           if (avd +  0.1000000001d+01) 20050, 10050, 40050             
40050 if (avd +  0.9999999995d+00) 10050, 10050, 20050             
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = -1.0d0                                              
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0051      continue                                                     
! T006*  TEST 6                            TEST OF TANH WITH DOUBLE PREC 
           ivtnum = 6                                                   
        avd = tanh(0.5d0) * tanh(0.75d0)                                
           if (avd -  0.2935132281d+00) 20060, 10060, 40060             
40060 if (avd -  0.2935132285d+00) 10060, 10060, 20060             
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 0.29351322831389d0                                  
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0061      continue                                                     
! T007*  TEST 7                     TEST OF ABS AND SIN WITH DOUBLE PREC 
           ivtnum = 7                                                   
        avd = abs(4.57812500d0) * sin(1.125d0)                          
           if (avd -  0.4130693827d+01) 20070, 10070, 40070             
40070 if (avd -  0.4130693832d+01) 10070, 10070, 20070             
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 4.130693829235d0                                    
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0071      continue                                                     
! T008*  TEST 8                     TEST OF ABS, MOD AND SIGN            
! *****                               WITH INTEGER, REAL AND DOUBLE PREC 
           ivtnum = 8                                                   
        lvi = -25                                                       
        avs = 32.750                                                    
        bvs = 1.375                                                     
        cvd = 0.75d0                                                    
        dvd = 1.125d0                                                   
        avd = abs(lvi) - (mod(avs, bvs) * sign(cvd, dvd))               
           if (avd -  0.2415624998d+02) 20080, 10080, 40080             
40080 if (avd -  0.2415625002d+02) 10080, 10080, 20080             
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = 24.15625d0                                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                 TEST OF ABS WITH COMPLEX 
           ivtnum = 9                                                   
        avs = abs((-2.125, 5.0))                                        
           if (avs -  0.54325e+01) 20090, 10090, 40090                  
40090 if (avs -  0.54331e+01) 10090, 10090, 20090                  
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 5.4328279                                           
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0091      continue                                                     
! T010*  TEST 10                        TEST OF SIN AND COS WITH COMPLEX 
           ivtnum = 10                                                  
        avc = sin((2.5, 3.5)) * cos((-4.75, 1.25))                      
           if (r2e(1) +  0.20512e+02) 20100, 40102, 40101               
40101 if (r2e(1) +  0.20510e+02) 40102, 40102, 20100               
40102 if (r2e(2) +  0.16820e+02) 20100, 10100, 40100               
40100 if (r2e(2) +  0.16817e+02) 10100, 10100, 20100               
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           zvcorr = (-20.5109598, -16.8182771)                          
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0101      continue                                                     
! T011*  TEST 11                     TEST OF SIN, COS AND TAN            
! *****                                            WITH REAL AND COMPLEX 
           ivtnum = 11                                                  
        avs = 2.0                                                       
        cvc = (3.125, 1.5)                                              
        bvs = 3.5                                                       
        avc = sin(avs) + cos(cvc) + tan(bvs)                            
           if (r2e(1) +  0.10683e+01) 20110, 40112, 40111               
40111 if (r2e(1) +  0.10681e+01) 40112, 40112, 20110               
40112 if (r2e(2) +  0.35331e-01) 20110, 10110, 40110               
40110 if (r2e(2) +  0.35327e-01) 10110, 10110, 20110               
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           zvcorr = (-1.068203, -0.0353288)                             
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0111      continue                                                     
! T012*  TEST 12                     TEST OF ABS, MOD, SIN AND COS       
! *****                                   WITH INTEGER, REAL AND COMPLEX 
           ivtnum = 12                                                  
        avc = abs(-2) * mod(17.250, 3.125) + sin(3.125) -                         cos((-0.375, 1.625))                                    
           if (r2e(1) -  0.81218e+00) 20120, 40122, 40121               
40121 if (r2e(1) -  0.81227e+00) 40122, 40122, 20120               
40122 if (r2e(2) +  0.89403e+00) 20120, 10120, 40120               
40120 if (r2e(2) +  0.89393e+00) 10120, 10120, 20120               
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           zvcorr = (0.8122242, -0.893981)                              
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0121      continue                                                     
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
! *****    END OF TEST SEGMENT 208                                       
      stop                                                              
      end program fm831
