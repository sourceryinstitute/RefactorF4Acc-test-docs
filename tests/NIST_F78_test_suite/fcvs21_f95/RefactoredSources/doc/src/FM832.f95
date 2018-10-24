      program fm832
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM832                                                          
! *****                       YGEN5 - (210)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****      TEST GENERIC FUNCTIONS                               15.3   
! *****       SQRT,EXP,LOG,LOG10,COS,SINH,TANH,ASIN,ATAN,ATAN2   TABLE 5 
! *****          EACH FUNCTION IS FIRST CALLED WITH A REAL VALUE         
! *****          AND THEN WITH A DOUBLE PRECISION VALUE                  
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
! *****  S P E C I F I C A T I O N S  SEGMENT 210                        
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
      real :: avs
      real :: bvs
      real :: rvcorr
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
      ivtotl = 20                                                       
      zprog = 'FM832'                                                   
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
! *****    HEADER FOR SEGMENT 210                                        
        write(nuvi,21000)                                               
21000 format( " ", /  " YGEN5 - (210) GENERIC FUNCTIONS --" //                  "  SQRT,EXP,LOG,LOG10,COS,SINH,TANH,ASIN,ATAN,ATAN2" //           "  ANS REF. - 15.3" )                                   
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
! *****    TEST WITH REAL ARGUMENTS                                      
! *****                                                                  
        write(nuvi, 21001)                                              
21001 format (/ 8x, "TEST WITH REAL ARGUMENTS" )                      
! T001*  TEST 1                                             TEST OF SQRT 
           ivtnum = 1                                                   
        avs = 2.0                                                       
        bvs = 1.0                                                       
        avd = sqrt(avs*bvs)                                             
           if (avd -  0.14141e+01) 20010, 10010, 40010                  
40010 if (avd -  0.14143e+01) 10010, 10010, 20010                  
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr =          0.14142135381699e+01                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                              TEST OF EXP 
           ivtnum = 2                                                   
        avs = 10.0                                                      
        avd = exp(avs / 10.0)                                           
           if (avd -  0.27181e+01) 20020, 10020, 40020                  
40020 if (avd -  0.27185e+01) 10020, 10020, 20020                  
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr =          0.27182817459106e+01                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                              TEST OF LOG 
           ivtnum = 3                                                   
        avs = 0.1234                                                    
        bvs = .0000567                                                  
        avd = log(avs + bvs)                                            
           if (avd +  0.20920e+01) 20030, 10030, 40030                  
40030 if (avd +  0.20917e+01) 10030, 10030, 20030                  
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr =         -0.20918648242950e+01                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                            TEST OF LOG10 
           ivtnum = 4                                                   
        avs = 0.375                                                     
        bvd = 3.75d0                                                    
        avd = log10(avs)                                                
           if (avd +  0.42599e+00) 20040, 10040, 40040                  
40040 if (avd +  0.42594e+00) 10040, 10040, 20040                  
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr =         -0.42596873641014e+00                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                              TEST OF COS 
           ivtnum = 5                                                   
        avs = .25                                                       
        avd = cos(avs*2)                                                
           if (avd -  0.87753e+00) 20050, 10050, 40050                  
40050 if (avd -  0.87763e+00) 10050, 10050, 20050                  
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr =          0.87758255004883e+00                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0051      continue                                                     
! T006*  TEST 6                                             TEST OF SINH 
           ivtnum = 6                                                   
        avd = sinh(avs+3.0)                                             
           if (avd -  0.12875e+02) 20060, 10060, 40060                  
40060 if (avd -  0.12877e+02) 10060, 10060, 20060                  
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr =          0.12875782966614e+02                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0061      continue                                                     
! T007*  TEST 7                                             TEST OF TANH 
           ivtnum = 7                                                   
        cvd = 0.5d1                                                     
        avd = tanh(avs*20.0)                                            
           if (avd -  0.99986e+00) 20070, 10070, 40070                  
40070 if (avd -  0.99996e+00) 10070, 10070, 20070                  
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr =          0.99990922212601e+00                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0071      continue                                                     
! T008*  TEST 8                                             TEST OF ASIN 
           ivtnum = 8                                                   
        avd = asin(avs*4.0)                                             
           if (avd -  0.15707e+01) 20080, 10080, 40080                  
40080 if (avd -  0.15709e+01) 10080, 10080, 20080                  
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr =          0.15707963705063e+01                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                             TEST OF ATAN 
           ivtnum = 9                                                   
        avs = 500.0                                                     
        avd = atan(-2.0*avs)                                            
           if (avd +  0.15699e+01) 20090, 10090, 40090                  
40090 if (avd +  0.15697e+01) 10090, 10090, 20090                  
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr =         -0.15697963237762e+01                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0091      continue                                                     
! T010*  TEST 10                                           TEST OF ATAN2 
           ivtnum = 10                                                  
        avs = 0.0                                                       
        bvs = -5.0                                                      
        avd = atan2(avs, bvs)                                           
           if (avd -  0.31414e+01) 20100, 10100, 40100                  
40100 if (avd -  0.31418e+01) 10100, 10100, 20100                  
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr =          0.31415927410126e+01                       
           write (nuvi, 80012) ivtnum, avd, rvcorr                      
 0101      continue                                                     
! *****                                                                  
           write (nuvi, 90002)                                          
           write (nuvi, 90013)                                          
           write (nuvi, 90014)                                          
! *****                                                                  
! *****    TEST WITH DOUBLE PRECISION ARGUMENTS                          
! *****                                                                  
        write (nuvi, 21002)                                             
21002 format (/ 08x, "TEST WITH DOUBLE PRECISION ARGUMENTS" )         
! T011*  TEST 11                                            TEST OF SQRT 
           ivtnum = 11                                                  
        avs = 2.0                                                       
        bvs = 1.0                                                       
        bvd = sqrt(dble(avs))                                           
           if (bvd -  0.1414213561d+01) 20110, 10110, 40110             
40110 if (bvd -  0.1414213563d+01) 10110, 10110, 20110             
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr =          0.14142135623731d+01                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0111      continue                                                     
! T012*  TEST 12                                             TEST OF EXP 
           ivtnum = 12                                                  
        avs = 10.0                                                      
        bvd = exp(1.0d0)                                                
           if (bvd -  0.2718281827d+01) 20120, 10120, 40120             
40120 if (bvd -  0.2718281830d+01) 10120, 10120, 20120             
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr =          0.27182818284590d+01                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0121      continue                                                     
! T013*  TEST 13                                             TEST OF LOG 
           ivtnum = 13                                                  
        avs = 0.1234                                                    
        bvs = .0000567                                                  
        bvd = log(0.1234567d0)                                          
           if (bvd +  0.2091864793d+01) 20130, 10130, 40130             
40130 if (bvd +  0.2091864790d+01) 10130, 10130, 20130             
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr =         -0.20918647916786d+01                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0131      continue                                                     
! T014*  TEST 14                                           TEST OF LOG10 
           ivtnum = 14                                                  
        avs = 0.375                                                     
        bvd = 3.75d0                                                    
        bvd = log10(bvd / 1.0d1)                                        
           if (bvd +  0.4259687325d+00) 20140, 10140, 40140             
40140 if (bvd +  0.4259687320d+00) 10140, 10140, 20140             
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr =         -0.42596873227228d+00                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0141      continue                                                     
! T015*  TEST 15                                             TEST OF COS 
           ivtnum = 15                                                  
        avs = .25                                                       
        bvd = cos(0.5d0)                                                
           if (bvd -  0.8775825614d+00) 20150, 10150, 40150             
40150 if (bvd -  0.8775825624d+00) 10150, 10150, 20150             
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           dvcorr =          0.87758256189037d+00                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0151      continue                                                     
! T016*  TEST 16                                            TEST OF SINH 
           ivtnum = 16                                                  
        bvd = sinh(3.25d0)                                              
           if (bvd -  0.1287578284d+02) 20160, 10160, 40160             
40160 if (bvd -  0.1287578286d+02) 10160, 10160, 20160             
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr =          0.12875782854681d+02                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0161      continue                                                     
! T017*  TEST 17                                            TEST OF TANH 
           ivtnum = 17                                                  
        cvd = 0.5d1                                                     
        bvd = tanh(cvd)                                                 
           if (bvd -  0.9999092037d+00) 20170, 10170, 40170             
40170 if (bvd -  0.9999092048d+00) 10170, 10170, 20170             
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           dvcorr =          0.99990920426260d+00                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0171      continue                                                     
! T018*  TEST 18                                            TEST OF ASIN 
           ivtnum = 18                                                  
        bvd = asin(100.0d0 / 1.0d2)                                     
           if (bvd -  0.1570796326d+01) 20180, 10180, 40180             
40180 if (bvd -  0.1570796328d+01) 10180, 10180, 20180             
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           dvcorr =          0.15707963267949d+01                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0181      continue                                                     
! T019*  TEST 19                                            TEST OF ATAN 
           ivtnum = 19                                                  
        avs = 500.0                                                     
        bvd = atan(-1.0d3)                                              
           if (bvd +  0.1569796328d+01) 20190, 10190, 40190             
40190 if (bvd +  0.1569796326d+01) 10190, 10190, 20190             
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           dvcorr =         -0.15697963271282d+01                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0191      continue                                                     
! T020*  TEST 20                                           TEST OF ATAN2 
           ivtnum = 20                                                  
        avs = 0.0                                                       
        bvs = -5.0                                                      
        bvd = atan2(0.0d0, -5.0d0)                                      
           if (bvd -  0.3141592652d+01) 20200, 10200, 40200             
40200 if (bvd -  0.3141592655d+01) 10200, 10200, 20200             
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           dvcorr =          0.31415926535898d+01                       
           write (nuvi, 80031) ivtnum, bvd, dvcorr                      
 0201      continue                                                     
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
! *****    END OF TEST SEGMENT 210                                       
      stop                                                              
      end program fm832
