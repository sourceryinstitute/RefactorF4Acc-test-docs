      program fm371
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM371                                                          
! *****                       XALG10 - (184)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REF 
! *****    TEST INTRINSIC FUNCTION ALOG10                        15.3    
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
      integer :: ivtnum
      real :: bvs
      real :: avs
      real :: rvcorr
      real :: cvs
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
      ivtotl = 15                                                       
      zprog = 'FM371'                                                   
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
! *****    HEADER FOR SEGMENT 184                                        
        write(nuvi,18400)                                               
18400 format(" ", / "  XALG10 - (184) INTRINSIC FUNCTIONS" //                  "  ALOG10 (COMMON LOGARITHM)" //                                  "  SUBSET REF. - 15.3" )                                 
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
! T001*  TEST 1                                 ONE, SINCE LN(1.0) = 0.0 
           ivtnum = 1                                                   
        bvs = 1.0                                                       
        avs = alog10(bvs)                                               
           if (avs + 0.50000e-04) 20010, 10010, 40010                   
40010 if (avs - 0.50000e-04) 10010, 10010, 20010                   
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           rvcorr = 0.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0011      continue                                                     
! T002*  TEST 2                                     A VALUE CLOSE TO TEN 
           ivtnum = 2                                                   
        avs = alog10(9.875)                                             
           if (avs - 0.99448e+00) 20020, 10020, 40020                   
40020 if (avs - 0.99459e+00) 10020, 10020, 20020                   
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           rvcorr = 0.99453710429850                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0021      continue                                                     
! T003*  TEST 3                                           THE VALUE 10.0 
           ivtnum = 3                                                   
        avs = alog10(10.0)                                              
           if (avs - 0.99995e+00) 20030, 10030, 40030                   
40030 if (avs - 0.10001e+01) 10030, 10030, 20030                   
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           rvcorr = 1.00000000000000                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0031      continue                                                     
! T004*  TEST 4                                           THE VALUE 20.5 
           ivtnum = 4                                                   
        avs = alog10(20.5)                                              
           if (avs - 0.13116e+01) 20040, 10040, 40040                   
40040 if (avs - 0.13119e+01) 10040, 10040, 20040                   
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           rvcorr = 1.31175386105575                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0041      continue                                                     
! T005*  TEST 5                                           THE VALUE 99.0 
           ivtnum = 5                                                   
        avs = alog10(99.0)                                              
           if (avs - 0.19955e+01) 20050, 10050, 40050                   
40050 if (avs - 0.19958e+01) 10050, 10050, 20050                   
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           rvcorr = 1.99563519459755                                    
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0051      continue                                                     
! T006*  TEST 6                           VARIABLES WITHIN AN EXPRESSION 
           ivtnum = 6                                                   
        bvs = 1.0                                                       
        cvs = 8.0                                                       
        avs = alog10(3.0 * bvs / cvs)                                   
           if (avs + 0.42599e+00) 20060, 10060, 40060                   
40060 if (avs + 0.42594e+00) 10060, 10060, 20060                   
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           rvcorr = -0.42596873227228                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0061      continue                                                     
! T007*  TEST 7                           VARIABLES WITHIN AN EXPRESSION 
           ivtnum = 7                                                   
        bvs = 1.0                                                       
        cvs = 8.0                                                       
        avs = alog10(5.0 * bvs / cvs)                                   
           if (avs + 0.20413e+00) 20070, 10070, 40070                   
40070 if (avs + 0.20411e+00) 10070, 10070, 20070                   
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           rvcorr = -0.20411998265592                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0071      continue                                                     
! T008*  TEST 8                         AN EXPRESSION SUPPLIED TO ALOG10 
           ivtnum = 8                                                   
        avs = alog10(75.0 / 100.0)                                      
           if (avs + 0.12495e+00) 20080, 10080, 40080                   
40080 if (avs + 0.12493e+00) 10080, 10080, 20080                   
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           rvcorr = -0.12493873660830                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0081      continue                                                     
! T009*  TEST 9                           VARIABLES WITHIN AN EXPRESSION 
           ivtnum = 9                                                   
        bvs = 1.0                                                       
        cvs = 8.0                                                       
        avs = alog10(7.0 * bvs / cvs)                                   
           if (avs + 0.57995e-01) 20090, 10090, 40090                   
40090 if (avs + 0.57989e-01) 10090, 10090, 20090                   
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           rvcorr = -0.05799194697769                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0091      continue                                                     
! T010*  TEST 10                                    A VALUE CLOSE TO ONE 
           ivtnum = 10                                                  
        avs = alog10(0.9921875)                                         
           if (avs + 0.34065e-02) 20100, 10100, 40100                   
40100 if (avs + 0.34060e-02) 10100, 10100, 20100                   
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           rvcorr = -0.0034062486919115                                 
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0101      continue                                                     
! T012*  TEST 11                                   A VALUE CLOSE TO ZERO 
           ivtnum = 11                                                  
        bvs = 256.0                                                     
        avs = alog10(1.0 / bvs)                                         
           if (avs + 0.24084e+01) 20120, 10120, 40120                   
40120 if (avs + 0.24081e+01) 10120, 10120, 20120                   
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           rvcorr = -2.40823996531185                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0121      continue                                                     
! T013*  TEST 12                                   A VALUE CLOSE TO ZERO 
           ivtnum = 12                                                  
        bvs = 128.0                                                     
        avs = alog10(1.0 / (bvs * 8.0))                                 
           if (avs + 0.30105e+01) 20130, 10130, 40130                   
40130 if (avs + 0.30101e+01) 10130, 10130, 20130                   
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           rvcorr = -3.01029995663981                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0131      continue                                                     
! T014*  TEST 13                           AN ARGUMENT OF HIGH MAGNITUDE 
           ivtnum = 13                                                  
        bvs = 2.0e+35                                                   
        avs = alog10(bvs)                                               
           if (avs - 0.35299e+02) 20140, 10140, 40140                   
40140 if (avs - 0.35303e+02) 10140, 10140, 20140                   
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           rvcorr = 35.30102999566398                                   
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0141      continue                                                     
! T015*  TEST 14                            AN ARGUMENT OF LOW MAGNITUDE 
           ivtnum = 14                                                  
        bvs = 2.0e-35                                                   
        avs = alog10(bvs)                                               
           if (avs + 0.34701e+02) 20150, 10150, 40150                   
40150 if (avs + 0.34697e+02) 10150, 10150, 20150                   
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           rvcorr = -34.69897000433602                                  
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0151      continue                                                     
! T016*  TEST 15                              THE FUNCTION APPLIED TWICE 
           ivtnum = 15                                                  
        avs = alog10(20.0) - alog10(2.0)                                
           if (avs - 0.99995e+00) 20160, 10160, 40160                   
40160 if (avs - 0.10001e+01) 10160, 10160, 20160                   
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           rvcorr = 1.0000000                                           
           write (nuvi, 80012) ivtnum, avs, rvcorr                      
 0161      continue                                                     
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
! *****    END OF TEST SEGMENT 184                                       
      stop                                                              
      end program fm371
