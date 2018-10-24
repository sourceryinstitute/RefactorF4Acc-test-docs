      program fm503
        character(len=6) :: a6xvk
        character(len=6) :: a6xvk_ARG
        logical :: axvb
        logical :: axvb_ARG
        complex :: axvc
        complex :: axvc_ARG
        double precision :: axvd
        double precision :: axvd_ARG
      real :: axvs
      real :: axvs_ARG
        character(len=6) :: b6xvk
        character(len=6) :: b6xvk_ARG
      real :: bxvs
      real :: bxvs_ARG
      integer :: ixvi
      integer :: ixvi_ARG
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM503                                                          
! *****                       BLKD2 - (261)                              
! *****   THIS PROGRAM USES FM504 (UNNAMED BLOCK DATA SUBPROGRAM         
! *****   AND SUBROUTINE SN505                                           
! ***********************************************************************
! *****  TESTING OF BLOCK DATA SUBPROGRAMS                       ANS REF 
! *****          DATA INTERNAL FORMS                               16    
! *****  THIS SEGMENT USES SEGMENTS 702 AND 703, BLOCK DATA PROGRAM      
! *****  FM504 AND SUBROUTINE SN505                                      
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
! *****  S P E C I F I C A T I O N S  SEGMENT 261                        
! *****                                                                  
! *****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       
! *****  DOUBLE PRECISION AXVD, DVCORR                                   
! *****  COMPLEX AXVC, ZVCORR                                            
! *****  LOGICAL AXVB                                                    
! *****  CHARACTER*6 A6XVK, B6XVK, CVCORR                                
! *****                                                                  
! *****  COMMON /BLK9/ AXVS, BXVS, IXVI, AXVD, AXVC, AXVB                
! *****  COMMON /BLK7/ A6XVK, B6XVK                                      
! *****                                                                  
! *****                                                                  
      call block_data(a6xvk,axvb,axvc,axvd,axvs,b6xvk,bxvs,ixvi)

        call sn505(a6xvk,axvb,axvc,axvd,axvs,b6xvk,bxvs,ixvi)

        stop                                                            
! *****                                                                  
! *****          END OF TEST SEGMENT 261                                 
        end program fm503
        subroutine block_data(a6xvk_ARG,axvb_ARG,axvc_ARG,axvd_ARG,axvs_ARG,b6xvk_ARG,bxvs_ARG,ixvi_ARG)
! *****                                                                  
! *****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       
        character(len=6), intent(InOut) :: a6xvk_ARG
        logical, intent(InOut) :: axvb_ARG
        complex, intent(InOut) :: axvc_ARG
        double precision, intent(InOut) :: axvd_ARG
      real, intent(InOut) :: axvs_ARG
        character(len=6), intent(InOut) :: b6xvk_ARG
      real, intent(InOut) :: bxvs_ARG
      integer, intent(InOut) :: ixvi_ARG
      real :: axvs
      real :: bxvs
      integer :: ixvi
        double precision :: axvd
        complex :: axvc
        logical :: axvb
        character(len=6) :: a6xvk
        character(len=6) :: b6xvk
! *****                                                                  
        data axvs,bxvs,ixvi,axvd,axvc,axvb / 34.25e-1,43.23,21,1.23456,(234.23,34.9),.true. / 
        data a6xvk,b6xvk / 'ABCDE','FGHIJK' / 
! *****                                                                  
        a6xvk_ARG = a6xvk
        axvb_ARG = axvb
        axvc_ARG = axvc
        axvd_ARG = axvd
        axvs_ARG = axvs
        b6xvk_ARG = b6xvk
        bxvs_ARG = bxvs
        ixvi_ARG = ixvi
        end subroutine block_data
        subroutine sn505(a6xvk,axvb,axvc,axvd,axvs,b6xvk,bxvs,ixvi)
! *****                                                                  
! *****                                                                  
! *****  DECLARATION OF VARIABLES IN COMMON BLOCKS                       
      real, intent(In) :: axvs
      real, intent(In) :: bxvs
      integer, intent(In) :: ixvi
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
      real :: rvcorr
      integer :: ivi
      integer :: ivcorr
      integer :: lvcorr
        double precision, intent(In) :: axvd
        double precision :: dvcorr
        complex, intent(In) :: axvc
        complex :: zvcorr
        logical, intent(In) :: axvb
        character(len=6) :: a6xvk
        character(len=6) :: b6xvk
        character(len=6) :: cvcorr
! *****                                                                  
! *****  LOCAL DECLARATION                                               
        double precision :: avd
        complex :: avc
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
      ivtotl = 8                                                        
      zprog = 'FM503'                                                   
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
        write(nuvi,26100)                                               
26100 format( " ", /  " BLKD2 - (261) BLOCK DATA SUBPROGRAM --" //              "  DATA INTERNAL FORMS" //                                        "  ANS REF. - 16" )                                     
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
! T001*  TEST 1                            REAL VARIABLE - EXPONENT FORM 
           ivtnum = 1                                                   
        avs = axvs                                                      
           if (avs - 0.34248e+01) 20010, 10010, 40010                   
40010 if (avs - 0.34252e+01) 10010, 10010, 20010                   
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 34.25e-1                                            
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0011      continue                                                     
! T002*  TEST 2                        REAL VARIABLE - NON EXPONENT FORM 
           ivtnum = 2                                                   
        avs = bxvs                                                      
           if (avs - 0.43227e+02) 20020, 10020, 40020                   
40020 if (avs - 0.43233e+02) 10020, 10020, 20020                   
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 43.23                                               
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                         INTEGER VARIABLE 
           ivtnum = 3                                                   
        ivi = ixvi                                                      
           if (ivi -    21) 20030, 10030, 20030                         
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           ivcorr =    21                                               
           write (nuvi, 80010) ivtnum, ivi, ivcorr                      
 0031      continue                                                     
! T004*  TEST 4                                DOUBLE PRECISION VARIABLE 
           ivtnum = 4                                                   
        avd = axvd                                                      
           if (avd - 0.12345d+01) 20040, 10040, 40040                   
40040 if (avd - 0.12347d+01) 10040, 10040, 20040                   
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 1.23456d+0                                          
           write (nuvi, 80031) ivtnum, avd, dvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                         COMPLEX VARIABLE 
           ivtnum = 5                                                   
        avc = axvc                                                      
           if (r2e(1) - 0.23421e+03) 20050, 40052, 40051                
40051 if (r2e(1) - 0.23425e+03) 40052, 40052, 20050                
40052 if (r2e(2) - 0.34898e+02) 20050, 10050, 40050                
40050 if (r2e(2) - 0.34902e+02) 10050, 10050, 20050                
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           zvcorr = (234.23, 34.9)                                      
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0051      continue                                                     
! T006*  TEST 6                                         LOGICAL VARIABLE 
           ivtnum = 6                                                   
           ivi = 0                                                      
           if (axvb) ivi = 1                                            
           if (ivi - 1) 20060, 10060, 20060                             
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           lvcorr = 1                                                   
           remrks = '1 = TRUE ;  0 = FALSE'                             
           write (nuvi, 80008) ivtnum, remrks                           
           write (nuvi, 80024) ivi                                      
           write (nuvi, 80026) lvcorr                                   
 0061      continue                                                     
! T007*  TEST 7            6 CHARACTER VARIABLE - INIT WITH 5 CHARACTERS 
           ivtnum = 7                                                   
           ivi = 0                                                      
           if (a6xvk == 'ABCDE ') ivi = 1                               
           if (ivi - 1) 20070, 10070, 20070                             
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           cvcorr = 'ABCDE '                                            
           write (nuvi, 80018) ivtnum, a6xvk, cvcorr                    
 0071      continue                                                     
! T008*  TEST 8            6 CHARACTER VARIABLE - INIT WITH 6 CHARACTERS 
           ivtnum = 8                                                   
           ivi = 0                                                      
           if (b6xvk == 'FGHIJK') ivi = 1                               
           if (ivi - 1) 20080, 10080, 20080                             
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           cvcorr = 'FGHIJK'                                            
           write (nuvi, 80018) ivtnum, b6xvk, cvcorr                    
 0081      continue                                                     
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
        end subroutine sn505
