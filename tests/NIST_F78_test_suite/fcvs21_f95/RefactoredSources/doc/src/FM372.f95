      program fm372
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM372                                                          
! *****                       XSIN - (186)                               
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REF 
! *****    TEST INTRINSIC FUNCTION SIN                          15.3     
! *****                                                        TABLE 5   
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
      real :: pivs
      integer :: ivtnum
      real :: bvs
      real :: avs
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
      ivtotl = 17                                                       
      zprog = 'FM372'                                                   
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
! *****    HEADER FOR SEGMENT 186                                        
        write(nuvi,18600)                                               
18600 format(" "," XSIN - (186) INTRINSIC FUNCTIONS" //                        "  SIN (SINE)" //                                                 "  SUBSET REF. - 15.3" )                                 
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
! T001*  TEST 1                               ZERO (0.0), SINCE SIN(0)=0 
           ivtnum = 1                                                   
        bvs = 0.0                                                       
        avs = sin(bvs)                                                  
           if (avs + 0.50000e-04) 20010, 10010, 40010                   
40010 if (avs - 0.50000e-04) 10010, 10010, 20010                   
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                                       PI 
           ivtnum = 2                                                   
        avs = sin(pivs)                                                 
           if (avs + 0.50000e-04) 20020, 10020, 40020                   
40020 if (avs - 0.50000e-04) 10020, 10020, 20020                   
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 0.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                                 PI - 1/8 
           ivtnum = 3                                                   
        bvs = 3.0165926535                                              
        avs = sin(bvs)                                                  
           if (avs - 0.12466e+00) 20030, 10030, 40030                   
40030 if (avs - 0.12468e+00) 10030, 10030, 20030                   
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 0.12467473338523                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                                PI - 1/16 
           ivtnum = 4                                                   
        avs = sin(3.2040926535)                                         
           if (avs + 0.62463e-01) 20040, 10040, 40040                   
40040 if (avs + 0.62456e-01) 10040, 10040, 20040                   
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = -0.06245931784238                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                                     2*PI 
           ivtnum = 5                                                   
        bvs = pivs * 2.0                                                
        avs = sin(bvs)                                                  
           if (avs + 0.50000e-04) 20050, 10050, 40050                   
40050 if (avs - 0.50000e-04) 10050, 10050, 20050                   
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 0.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0051      continue                                                     
! T007*  TEST 6                                            THE VALUE 2.0 
           ivtnum = 6                                                   
        bvs = 2.0                                                       
        avs = sin(bvs)                                                  
           if (avs - 0.90925e+00) 20070, 10070, 40070                   
40070 if (avs - 0.90935e+00) 10070, 10070, 20070                   
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = 0.90929742682568                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0071      continue                                                     
! T008*  TEST 7                                           THE VALUE -2.0 
           ivtnum = 7                                                   
        bvs = -2.0                                                      
        avs = sin(bvs)                                                  
           if (avs + 0.90935e+00) 20080, 10080, 40080                   
40080 if (avs + 0.90925e+00) 10080, 10080, 20080                   
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = -0.90929742682568                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0081      continue                                                     
! T009*  TEST 08                A LARGE VALUE TO TEST ARGUMENT REDUCTION 
           ivtnum = 08                                                  
        avs = sin(100.0)                                                
           if (avs + 0.50639e+00) 20090, 10090, 40090                   
40090 if (avs + 0.50634e+00) 10090, 10090, 20090                   
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = -0.50636564110976                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0091      continue                                                     
! T010*  TEST 09                                      A VERY LARGE VALUE 
           ivtnum = 09                                                  
        avs = sin(-1000.0)                                              
           if (avs + 0.82692e+00) 20100, 10100, 40100                   
40100 if (avs + 0.82683e+00) 10100, 10100, 20100                   
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = -0.82687954053200                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0101      continue                                                     
! T011*  TEST 10                                                    PI/2 
           ivtnum = 10                                                  
        avs = sin(1.5707963268)                                         
           if (avs - 0.99995e+00) 20110, 10110, 40110                   
40110 if (avs - 0.10001e+01) 10110, 10110, 20110                   
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 1.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0111      continue                                                     
! T012*  TEST 11                                             PI/2 - 1/32 
           ivtnum = 11                                                  
        bvs = 1.5395463268                                              
        avs = sin(bvs)                                                  
           if (avs - 0.99946e+00) 20120, 10120, 40120                   
40120 if (avs - 0.99957e+00) 10120, 10120, 20120                   
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 0.99951175848514                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0121      continue                                                     
! T013*  TEST 12                                             PI/2 - 1/64 
           ivtnum = 12                                                  
        avs = sin(1.5864213268)                                         
           if (avs - 0.99982e+00) 20130, 10130, 40130                   
40130 if (avs - 0.99993e+00) 10130, 10130, 20130                   
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr =  0.99987793217101                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0131      continue                                                     
! T014*  TEST 13                                                  3*PI/2 
           ivtnum = 13                                                  
        bvs = 3.0 * pivs / 2.0                                          
        avs = sin(bvs)                                                  
           if (avs + 0.10001e+01) 20140, 10140, 40140                   
40140 if (avs + 0.99995e+00) 10140, 10140, 20140                   
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = -1.00000000000000                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0141      continue                                                     
! T015*  TEST 14                                           3*PI/2 - 1/16 
           ivtnum = 14                                                  
        bvs = (3.0 * pivs / 2.0) - 1.0 / 16.0                           
        avs = sin(bvs)                                                  
           if (avs + 0.99810e+00) 20150, 10150, 40150                   
40150 if (avs + 0.99799e+00) 10150, 10150, 20150                   
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = -0.99804751070010                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0151      continue                                                     
! T016*  TEST 15                                          3*PI/2 - 1/512 
           ivtnum = 15                                                  
        bvs = (3.0 * pivs / 2.0) + 1.0 / 512.0                          
        avs = sin(bvs)                                                  
           if (avs + 0.10001e+01) 20160, 10160, 40160                   
40160 if (avs + 0.99994e+00) 10160, 10160, 20160                   
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = -0.99999809265197                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0161      continue                                                     
! T017*  TEST 16                               ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 16                                                  
        bvs = pivs * 1.0e-37                                            
        avs = sin(bvs)                                                  
           if (avs + 0.50000e-04) 20170, 10170, 40170                   
40170 if (avs - 0.50000e-04) 10170, 10170, 20170                   
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           rvcorr = 3.14159265358979e-37                                
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0171      continue                                                     
! T018*  TEST 17                              THE FUNCTION APPLIED TWICE 
           ivtnum = 17                                                  
        avs = sin(pivs / 4.0) * sin(3.0 * pivs / 4.0)                   
           if (avs - 0.49997e+00) 20180, 10180, 40180                   
40180 if (avs - 0.50003e+00) 10180, 10180, 20180                   
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = 0.50000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0181      continue                                                     
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
! *****    END OF TEST SEGMENT 186                                       
      stop                                                              
      end program fm372
