      program fm804
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM804               YDMOD - (160)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****     TO TEST INTRINSIC FUNCTION - DMOD -                   15.3   
! *****     (REMAINDERING -TYPE DOUBLE PRECISION)               (TABLE 5)
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
! *****  S P E C I F I C A T I O N S  SEGMENT 160                        
! *****                                                                  
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
      real :: dvcorr
        double precision :: dqavd
        double precision :: dqbvd
        double precision :: dqdvd
        double precision :: dqevd
        double precision :: dqfvd
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
      zprog = 'FM804'                                                   
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
! *****    HEADER FOR SEGMENT 160                                        
        write (nuvi, 16001)                                             
16001 format( " ", //" YDMOD - (160) INTRINSIC FUNCTION--" //                   16x,"DMOD (REMAINDERING)"  //                                     "  ANS REF. - 15.3  " )                                 
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
! T001*  TEST 1                        FIRST VALUE ZERO, SECOND NON-ZERO 
           ivtnum = 1                                                   
        dqbvd = 0.0d0                                                   
        dqdvd = 4.5d0                                                   
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 5.0d-10) 20010, 10010, 40010                     
40010 if (dqavd - 5.0d-10) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0011      continue                                                     
! T002*  TEST 2                                        BOTH VALUES EQUAL 
           ivtnum = 2                                                   
        dqbvd = 0.35d1                                                  
        dqdvd = 0.35d1                                                  
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 5.0d-10) 20020, 10020, 40020                     
40020 if (dqavd - 5.0d-10) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0021      continue                                                     
! T003*  TEST 3           FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND 
           ivtnum = 3                                                   
        dqbvd = -0.10d2                                                 
        dqdvd = -0.3d1                                                  
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 1.000000001d0) 20030, 10030, 40030               
40030 if (dqavd + 0.9999999995d0) 10030, 10030, 20030              
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = -1.0d0                                              
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0031      continue                                                     
! T004*  TEST 4               FIRST MAGNITUDE LARGER, MULTIPLE OF SECOND 
           ivtnum = 4                                                   
        dqdvd = 1.5d0                                                   
        dqbvd = 1.5d0 + dqdvd + 1.5d0                                   
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 5.0d-10) 20040, 10040, 40040                     
40040 if (dqavd - 5.0d-10) 10040, 10040, 20040                     
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0041      continue                                                     
! T005*  TEST 5           FIRST MAGNITUDE LARGER, NOT MULTIPLE OF SECOND 
           ivtnum = 5                                                   
        dqbvd = 7.625d0                                                 
        dqdvd = 2.125d0                                                 
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd - 1.249999999d0) 20050, 10050, 40050               
40050 if (dqavd - 1.250000001d0) 10050, 10050, 20050               
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 1.25d0                                              
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0051      continue                                                     
! T006*  TEST 6                        FIRST VALUE ZERO, SECOND NEGATIVE 
           ivtnum = 6                                                   
        dqbvd = 0.0d0                                                   
        dqdvd = -0.45d1                                                 
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 5.0d-10) 20060, 10060, 40060                     
40060 if (dqavd - 5.0d-10) 10060, 10060, 20060                     
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0061      continue                                                     
! T007*  TEST 7                         BOTH VALUES EQUAL, BOTH NEGATIVE 
           ivtnum = 7                                                   
        dqbvd = -3.5d1                                                  
        dqdvd = -3.5d1                                                  
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 5.0d-10) 20070, 10070, 40070                     
40070 if (dqavd - 5.0d-10) 10070, 10070, 20070                     
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0071      continue                                                     
! T008*  TEST 8          FIRST MAGNITUDE LARGER, MULIPLES, BOTH NEGATIVE 
           ivtnum = 8                                                   
        dqdvd = 3.5d0                                                   
        dqbvd = -(3.5d0 + dqdvd + 3.5d0)                                
        dqavd = dmod(dqbvd, -dqdvd)                                     
           if (dqavd + 5.0d-10) 20080, 10080, 40080                     
40080 if (dqavd - 5.0d-10) 10080, 10080, 20080                     
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0081      continue                                                     
! T009*  TEST 9          FIRST VALUE POSITIVE, SECOND NEGATIVE, MULTIPLE 
           ivtnum = 9                                                   
        dqbvd = 10.5d0                                                  
        dqdvd = -3.5d0                                                  
        dqavd = dmod(dqbvd, dqdvd)                                      
           if (dqavd + 5.0d-10) 20090, 10090, 40090                     
40090 if (dqavd - 5.0d-10) 10090, 10090, 20090                     
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0091      continue                                                     
! T010*  TEST 10                 FIRST VALUE ZERO PRECEDED BY MINUS SIGN 
           ivtnum = 10                                                  
        dqdvd = 0.0d0                                                   
        dqevd = 4.5d0                                                   
        dqavd = dmod(-dqdvd, dqevd)                                     
           if (dqavd + 5.0d-10) 20100, 10100, 40100                     
40100 if (dqavd - 5.0d-10) 10100, 10100, 20100                     
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0101      continue                                                     
! T011*  TEST 11         PAIR OF ARITHMETIC EXPRESSIONS USED AS ARGUMENT 
           ivtnum = 11                                                  
        dqdvd = 0.7625d1                                                
        dqevd = 0.2125d1                                                
        dqfvd = 0.2d1                                                   
        dqavd = dmod(dqdvd - dqfvd, dqevd + dqfvd)                      
           if (dqavd - 0.1499999999d1) 20110, 10110, 40110              
40110 if (dqavd - 0.1500000001d1) 10110, 10110, 20110              
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr = 0.15d1                                              
           write (nuvi, 80031) ivtnum, dqavd, dvcorr                    
 0111      continue                                                     
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
! *****    END OF TEST SEGMENT 160                                       
        stop                                                            
        end program fm804
