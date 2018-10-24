      program fm373
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM373                                                          
! *****                       XCOS - (189)                               
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REF 
! *****    TEST INTRINSIC FUNCTION COS                          15.3     
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
      ivtotl = 18                                                       
      zprog = 'FM373'                                                   
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
! *****    HEADER FOR SEGMENT 189                                        
        write(nuvi,18900)                                               
18900 format(" "/" XCOS - (189) INTRINSIC FUNCTIONS" //                        "  COS (COSINE)" //                                               "  SUBSET REF. - 15.3" )                                 
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
! T001*  TEST 1                               ZERO (0.0), SINCE COS(0)=1 
           ivtnum = 1                                                   
        bvs = 0.0                                                       
        avs = cos(bvs)                                                  
           if (avs - 0.99995e+00) 20010, 10010, 40010                   
40010 if (avs - 0.10001e+01) 10010, 10010, 20010                   
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 1.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                           VALUES NEAR PI 
           ivtnum = 2                                                   
        avs = cos(pivs)                                                 
           if (avs + 0.10001e+01) 20020, 10020, 40020                   
40020 if (avs + 0.99995e+00) 10020, 10020, 20020                   
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = -1.00000000000000                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                                PI - 1/16 
           ivtnum = 3                                                   
        bvs = 3.0790926536                                              
        avs = cos(bvs)                                                  
           if (avs + 0.99810e+00) 20030, 10030, 40030                   
40030 if (avs + 0.99799e+00) 10030, 10030, 20030                   
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = -0.99804751070010                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                                PI + 1/32 
           ivtnum = 4                                                   
        avs = cos(3.1728426535)                                         
           if (avs + 0.99957e+00) 20040, 10040, 40040                   
40040 if (avs + 0.99946e+00) 10040, 10040, 20040                   
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = -0.99951175848514                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                         VALUES NEAR 2*PI 
           ivtnum = 5                                                   
        bvs = pivs * 2.0                                                
        avs = cos(bvs)                                                  
           if (avs - 0.99995e+00) 20050, 10050, 40050                   
40050 if (avs - 0.10001e+01) 10050, 10050, 20050                   
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 1.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0051      continue                                                     
! T006*  TEST 6                                         VALUES NEAR 2*PI 
           ivtnum = 6                                                   
        bvs = (2.0 * pivs) - 1.0 / 64.0                                 
        avs = cos(bvs)                                                  
           if (avs - 0.99982e+00) 20060, 10060, 40060                   
40060 if (avs - 0.99993e+00) 10060, 10060, 20060                   
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = 0.99987793217101                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0061      continue                                                     
! T007*  TEST 7                                         VALUES NEAR 2*PI 
           ivtnum = 7                                                   
        bvs = (2.0 * pivs) + 1.0 / 128.0                                
        avs = cos(bvs)                                                  
           if (avs - 0.99992e+00) 20070, 10070, 40070                   
40070 if (avs - 0.10001e+01) 10070, 10070, 20070                   
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = 0.99996948257710                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0071      continue                                                     
! T008*  TEST 8                           AN EXPRESSION PRESENTED TO COS 
           ivtnum = 8                                                   
        bvs = 350.0                                                     
        avs = cos(bvs / 100.0)                                          
           if (avs + 0.93651e+00) 20080, 10080, 40080                   
40080 if (avs + 0.93641e+00) 10080, 10080, 20080                   
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = -0.93645668729080                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0081      continue                                                     
! T009*  TEST 9                                      A NEGATIVE ARGUMENT 
           ivtnum = 9                                                   
        bvs = -1.5                                                      
        avs = cos(bvs)                                                  
           if (avs - 0.70733e-01) 20090, 10090, 40090                   
40090 if (avs - 0.70741e-01) 10090, 10090, 20090                   
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = 0.07073720166770                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0091      continue                                                     
! T010*  TEST 10                TEST LARGE VALUES FOR ARGUMENT REDUCTION 
           ivtnum = 10                                                  
        avs = cos(200.0)                                                
           if (avs - 0.48716e+00) 20100, 10100, 40100                   
40100 if (avs - 0.48722e+00) 10100, 10100, 20100                   
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = 0.48718767500701                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0101      continue                                                     
! T011*  TEST 11                TEST LARGE VALUES FOR ARGUMENT REDUCTION 
           ivtnum = 11                                                  
        avs = cos(-31416.0)                                             
           if (avs - 0.99725e+00) 20110, 10110, 40110                   
40110 if (avs - 0.99736e+00) 10110, 10110, 20110                   
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           rvcorr = 0.99730272627420                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0111      continue                                                     
! T012*  TEST 12                                   TEST VALUES NEAR PI/2 
           ivtnum = 12                                                  
        avs = cos(1.5707963268)                                         
           if (avs + 0.50000e-04) 20120, 10120, 40120                   
40120 if (avs - 0.50000e-04) 10120, 10120, 20120                   
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = 0.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0121      continue                                                     
! T013*  TEST 13                                         (PI / 2) - 1/32 
           ivtnum = 13                                                  
        bvs = (1.5395463267)                                            
        avs = cos(bvs)                                                  
           if (avs - 0.31243e-01) 20130, 10130, 40130                   
40130 if (avs - 0.31247e-01) 10130, 10130, 20130                   
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = 0.03124491398533                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0131      continue                                                     
! T014*  TEST 14                                         (PI / 2) + 1/16 
           ivtnum = 14                                                  
        avs = cos(1.6332963267)                                         
           if (avs + 0.62463e-01) 20140, 10140, 40140                   
40140 if (avs + 0.62456e-01) 10140, 10140, 20140                   
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = -0.06245931784238                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0141      continue                                                     
! T015*  TEST 15                                 TEST VALUES NEAR 3*PI/2 
           ivtnum = 15                                                  
        bvs = 3.0 * pivs / 2.0                                          
        avs = cos(bvs)                                                  
           if (avs + 0.50000e-04) 20150, 10150, 40150                   
40150 if (avs - 0.50000e-04) 10150, 10150, 20150                   
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = 0.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0151      continue                                                     
! T016*  TEST 16                                 TEST VALUES NEAR 3*PI/2 
           ivtnum = 16                                                  
        bvs = (3.0 * pivs / 2.0) - 1.0 / 16.0                           
        avs = cos(bvs)                                                  
           if (avs + 0.62463e-01) 20160, 10160, 40160                   
40160 if (avs + 0.62456e-01) 10160, 10160, 20160                   
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = -0.06245931784238                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0161      continue                                                     
! T018*  TEST 17                               ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 17                                                  
        bvs = -3.141593e-35                                             
        avs = cos(bvs)                                                  
           if (avs - 0.99995e+00) 20180, 10180, 40180                   
40180 if (avs - 0.10001e+01) 10180, 10180, 20180                   
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           rvcorr = 1.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0181      continue                                                     
! T019*  TEST 18                              THE FUNCTION APPLIED TWICE 
           ivtnum = 18                                                  
        avs = cos(pivs / 4.0) * cos(3.0 * pivs / 4.0)                   
           if (avs + 0.50003e+00) 20190, 10190, 40190                   
40190 if (avs + 0.49997e+00) 10190, 10190, 20190                   
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           rvcorr = -0.50000000000000                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
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
! *****    END OF TEST SEGMENT 189                                       
      stop                                                              
      end program fm373
