      program fm828
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM828                                                          
! *****                       YCFOR - (203)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST COMPLEX TRIGONOMETRIC FORMULAE                   15.3    
! *****                                                          TABLE 5 
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
! *****  S P E C I F I C A T I O N S  SEGMENT 203                        
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
      real :: pivs
      integer :: ivtnum
      real :: cvs
      real :: dvs
      real :: avs
      real :: rvcorr
      real :: bvs
        complex :: avc
        complex :: bvc
        complex :: cvc
        complex :: dvc
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
      ivtotl = 9                                                        
      zprog = 'FM828'                                                   
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
! *****    HEADER FOR SEGMENT 203                                        
        write(nuvi,20300)                                               
20300 format(" ", / "  YCFOR - (203) INTRINSIC FUNCTIONS" //                   "  COMPLEX TRIGONOMETRIC FORMULAE" //                             "  ANS REF. - 15.3" )                                    
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
        pivs = 3.1415926535897932384626434                              
! *****                                                                  
! T001*  TEST 1                                           SQRT(Z)**2 = Z 
           ivtnum = 1                                                   
        bvc = (1.0, 0.0) + (0.0, -2.5)                                  
        avc = csqrt((1.0, -2.5)) ** 2 - bvc                             
           if (r2e(1) + 0.50000e-04) 20010, 40012, 40011                
40011 if (r2e(1) - 0.50000e-04) 40012, 40012, 20010                
40012 if (r2e(2) + 0.50000e-04) 20010, 10010, 40010                
40010 if (r2e(2) - 0.50000e-04) 10010, 10010, 20010                
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           zvcorr = (0.0000, 0.0000)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0011      continue                                                     
! T002*  TEST 2   ANGLE SUBTENDED BY SQRT(Z) IS 1/2 ANGLE SUBTENDED BY Z 
           ivtnum = 2                                                   
        bvc = csqrt((2.0, 3.25))                                        
        cvs = aimag(bvc)                                                
        dvs = cabs((bvc + conjg(bvc)) / (2.0, 0.0))                     
        avs = atan2(3.0 + 0.25, 1.0 * 2.0) - 2.0 * atan2(cvs, dvs)      
           if (avs + 0.50000e-04) 20020, 10020, 40020                   
40020 if (avs - 0.50000e-04) 10020, 10020, 20020                   
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 0.0000                                              
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                          EXP(LOG(Z)) = Z 
           ivtnum = 3                                                   
        bvc = (0.0, 0.0) - (1.5, 0.75)                                  
        avc = cexp(clog(bvc)) + (1.5, 0.75)                             
           if (r2e(1) + 0.50000e-04) 20030, 40032, 40031                
40031 if (r2e(1) - 0.50000e-04) 40032, 40032, 20030                
40032 if (r2e(2) + 0.50000e-04) 20030, 10030, 40030                
40030 if (r2e(2) - 0.50000e-04) 10030, 10030, 20030                
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           zvcorr = (0.0000, 0.0000)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0031      continue                                                     
! T004*  TEST 4                               ABS(EXP(Z)) = EXP(REAL(Z)) 
           ivtnum = 4                                                   
        avs = cabs(cexp((-2.5, 1.375))) - exp(5.0 / (-2.0))             
           if (avs + 0.50000e-04) 20040, 10040, 40040                   
40040 if (avs - 0.50000e-04) 10040, 10040, 20040                   
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 0.0000                                              
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0041      continue                                                     
! T005*  TEST 5            ANGLE SUBTENDED BY EXP(Z) IS IMAG(Z) MOD 2 PI 
           ivtnum = 5                                                   
        bvc = (0.0625, 0.0)                                             
        cvc = cexp(bvc + (0.0, 1.125))                                  
        dvs = atan2(aimag(cvc), cabs((cvc + conjg(cvc)) / (2.0, 0.0)))  
        avs = dvs - amod(aimag((0.0625, 1.125)), 2.0 * pivs)            
           if (avs + 0.50000e-04) 20050, 10050, 40050                   
40050 if (avs - 0.50000e-04) 10050, 10050, 20050                   
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 0.0000                                              
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0051      continue                                                     
! T006*  TEST 6                              EXP(IY) = COS(Y) + I SIN(Y) 
           ivtnum = 6                                                   
        avc = cexp(cmplx(0.0, 37.5 / 10.0))                                     - cmplx(cos(3.75), sin(2.75 + 1.0))                       
           if (r2e(1) + 0.50000e-04) 20060, 40062, 40061                
40061 if (r2e(1) - 0.50000e-04) 40062, 40062, 20060                
40062 if (r2e(2) + 0.50000e-04) 20060, 10060, 40060                
40060 if (r2e(2) - 0.50000e-04) 10060, 10060, 20060                
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           zvcorr = (0.0000, 0.0000)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0061      continue                                                     
! T007*  TEST 7                    COS(Z) = 0.5 * (EXP(I*Z) + EXP(-I*Z)) 
           ivtnum = 7                                                   
         bvc = cexp((-1.5, -2.75))                                      
         cvc = (bvc + 1 / bvc) / (2.0, 0.0)                             
         dvc = (2.75, -1.5)                                             
         avc = cvc - ccos(dvc * (-1.0, 0.0))                            
            if (r2e(1) + 0.50000e-04) 20070, 40072, 40071               
40071 if (r2e(1) - 0.50000e-04) 40072, 40072, 20070               
40072 if (r2e(2) + 0.50000e-04) 20070, 10070, 40070               
40070 if (r2e(2) - 0.50000e-04) 10070, 10070, 20070               
10070 ivpass = ivpass + 1                                         
            write (nuvi, 80002) ivtnum                                  
            goto 0071                                                  
20070 ivfail = ivfail + 1                                         
            zvcorr = (0.0000, 0.0000)                                   
            write (nuvi, 80045) ivtnum, avc, zvcorr                     
 0071       continue                                                    
! T008*  TEST 8                                       ABS(EXP(IY)) = 1.0 
           ivtnum = 8                                                   
        bvc = (3.25, 3.25)                                              
        cvc = (3.25, 0.0)                                               
        avc = cabs(cexp(bvc - cvc)) - cos(0.0)                          
           if (r2e(1) + 0.50000e-04) 20080, 40082, 40081                
40081 if (r2e(1) - 0.50000e-04) 40082, 40082, 20080                
40082 if (r2e(2) + 0.50000e-04) 20080, 10080, 40080                
40080 if (r2e(2) - 0.50000e-04) 10080, 10080, 20080                
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           zvcorr = (0.0000, 0.0000)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0081      continue                                                     
! T009*  TEST 9                               DEMOIVRE THEOREM FOR N = 3 
           ivtnum = 9                                                   
        bvs = 3.0/2.0                                                   
        bvc = cmplx(cos(1.5), sin(bvs)) ** 3                            
        avc = bvc - cmplx(cos(4.5), -sin(4.5 + pivs))                   
           if (r2e(1) + 0.50000e-04) 20090, 40092, 40091                
40091 if (r2e(1) - 0.50000e-04) 40092, 40092, 20090                
40092 if (r2e(2) + 0.50000e-04) 20090, 10090, 40090                
40090 if (r2e(2) - 0.50000e-04) 10090, 10090, 20090                
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           zvcorr = (0.0000, 0.0000)                                    
           write (nuvi, 80045) ivtnum, avc, zvcorr                      
 0091      continue                                                     
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
! *****    END OF TEST SEGMENT 203                                       
      stop                                                              
      end program fm828
