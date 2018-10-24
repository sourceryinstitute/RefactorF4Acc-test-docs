      program fm719
!                                                                        
!      THIS ROUTINE TESTS DO STATEMENTS USING REAL,         ANS REF.     
!           DOUBLE PRECISION, OR MIXED TYPE DO-VARIABLES.   11.10        
!      ALSO TESTED ARE ACTIVE AND INACTIVE                  11.10.2      
!           DO LOOPS.                                       11.10.3      
!                                                                        
!      THIS ROUTINE USES FUNCTION SUBPROGRAM IF720 AND                   
!                        SUBROUTINE SUBPROGRAM SN721.                    
!                                                                        
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
      integer :: ivtnum
      real :: rvcomp
      real :: rvcorr
      real :: rvn001
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivn001
      integer :: ivn002
      integer :: ivn003
      integer :: ivn004
      integer :: ivn005
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
      double precision :: dvcomp
      double precision :: dvcorr
      double precision :: dvn001
! BE** ********************** BBCINITA **********************************
!                                                                        
!                                                                        
!                                                                        
! BB** ********************** BBCINITB **********************************
! **** INITIALIZE SECTION                                                
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
           zprog = 'FM719'                                              
           ivtotl = 14                                                  
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
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 719  ****                         
!      REAL DO-VARIABLE                                                  
!                                                                        
           ivtnum = 1                                                   
           rvcomp = 0.0                                                 
           rvcorr = 3.0                                                 
      do rvn001 = 1.1, 2.4, 0.5                                    
      rvcomp = rvcomp + 1.0                                             
       end do
           if (rvcomp - 0.29998e+01) 20010, 10010, 40010                
40010 if (rvcomp - 0.30002e+01) 10010, 10010, 20010                
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 719  ****                         
!      DOUBLE PRECISION DO-VARIABLE                                      
!                                                                        
           ivtnum = 2                                                   
           dvcomp = 0.0d0                                               
           dvcorr = 6.0d0                                               
      do dvn001 = 1.0d-2, 12.0d-2, 2.0d-2                          
      dvcomp = dvcomp + 1.0d0                                           
       end do
           if (dvcomp - 0.5999999997d+01) 20020, 10020, 40020           
40020 if (dvcomp - 0.6000000003d+01) 10020, 10020, 20020           
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0021      continue                                                     
!                                                                        
!      TESTS 3 THRU 10 TEST ACTIVE AND INACTIVE DO-LOOPS                 
!                                                                        
!                                                                        
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM A FUNCTION BACK TO LOOP                            
!                                                                        
           ivtnum = 3                                                   
           ivcomp = 0                                                   
           ivcorr =     9                                               
      do ivn001 = 1, 3                                             
      ivcomp  = ivcomp+if720(ivn001)
       end do
40030 if (ivcomp -     9) 20030, 10030, 20030                      
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0031      continue                                                     
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM A SUBROUTINE TO A STATEMENT OUTSIDE LOOP           
!                                                                        
           ivtnum = 4                                                   
           ivcomp = 0                                                   
           ivcorr =   -59                                               
      ivn002 = 0                                                        
      do ivn001 = 1, 5                                             
      call sn721(ivn002,*0043)

       end do
0043  ivcomp = ivn002 - 60                                              
40040 if (ivcomp +    59) 20040, 10040, 20040                      
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM A SUBROUTINE TO A STATEMENT INSIDE LOOP            
!                                                                        
           ivtnum = 5                                                   
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      ivn002 = 1                                                        
      do ivn001 = 1, 8                                             
      call sn721(ivn002,*0052)

      goto 20050                                                         !Break
0052  ivn002 = ivn002 - 1                                               
       end do
      ivcomp = ivn002                                                   
40050 if (ivcomp - 1) 20050, 10050, 20050                          
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0051      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM AN ENTRY TO A STATEMENT OUTSIDE LOOP               
!                                                                        
           ivtnum = 6                                                   
           ivcomp = 0                                                   
           ivcorr =   -34                                               
      ivn002 = -17                                                      
      do ivn001 = 1, 4                                             
      call en721(ivn002,*0063)

       end do
0063  ivcomp = ivn002                                                   
40060 if (ivcomp +    34) 20060, 10060, 20060                      
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM AN ENTRY TO A STATEMENT INSIDE LOOP                
!                                                                        
           ivtnum = 7                                                   
           ivcomp = 0                                                   
           ivcorr =    63                                               
      ivn002 = 7                                                        
      do ivn001 = 1, 3                                             
      call en721(ivn002,*0072)

      goto 20070                                                         !Break
0072  ivn002 = ivn002 + 1                                               
       end do
      ivcomp = ivn002                                                   
40070 if (ivcomp -    63) 20070, 10070, 20070                      
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0071      continue                                                     
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM AN ENTRY TO A STATEMENT OUTSIDE INNER LOOP OF A    
!      NESTED DO-LOOP                                                    
!                                                                        
           ivtnum = 8                                                   
           ivcomp = 0                                                   
           ivcorr =     3                                               
      ivn003 = 0                                                        
      do ivn001 = 1, 3                                             
      ivn003 = ivn003 + 1                                               
      do ivn002 = ivn001, 4                                        
      call en722(1,*0083,*0084)

       end do
0083  ivcomp = ivn003                                                   
      0084 end do
40080 if (ivcomp -     3) 20080, 10080, 20080                      
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM AN ENTRY TO A STATEMENT INSIDE INNER LOOP OF A     
!      NESTED DO-LOOP                                                    
!                                                                        
           ivtnum = 9                                                   
           ivcomp = 0                                                   
           ivcorr =    12                                               
      ivn003 = 0                                                        
      do ivn001 = 1, 3                                             
      ivn003 = ivn003 + 1                                               
      do ivn002 = ivn001, ivn001 + 1                               
      call en722(2,*0094,*0092)

      ivn004 = 10                                                       
0092  ivn004 = ivn002*ivn003                                            
       end do
0094  ivcomp = ivn004                                                   
       end do
40090 if (ivcomp -    12) 20090, 10090, 20090                      
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0091      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 719  ****                         
!      RETURN IS FROM AN ENTRY TO A STATEMENT EITHER INSIDE OR OUTSIDE   
!      INNER LOOP OF A NESTED DO-LOOP                                    
!                                                                        
           ivtnum = 10                                                  
           ivcomp = 0                                                   
           ivcorr =     9                                               
      ivn003 = 0                                                        
      ivn004 = 0                                                        
      do ivn001 = 1, 3                                             
      ivn003 = ivn003 + 1                                               
      ivn005 = (3 + (-1)**ivn001)/2                                     
      do ivn002 = ivn001, ivn001 + 1                               
      call en722(ivn005,*0104,*0102)

      ivn004 = 10                                                       
0102  ivn004 = ivn004 + ivn002 + ivn003                                 
       end do
0104  ivcomp = ivn004                                                   
       end do
40100 if (ivcomp -     9) 20100, 10100, 20100                      
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0101      continue                                                     
!      TESTS 11 THRU 14 TEST DO STATEMENTS WITH MIXED INTEGER, REAL,     
!      AND DOUBLE PRECISION.                                             
!                                                                        
!                                                                        
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 719  ****                         
!                                                                        
           ivtnum = 11                                                  
           ivcomp = 0                                                   
           ivcorr =    30                                               
      do ivn001 = 6.7, 0.9325d+1                                   
      ivcomp = ivcomp + ivn001                                          
       end do
40110 if (ivcomp -    30) 20110, 10110, 20110                      
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0111      continue                                                     
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 719  ****                         
!                                                                        
           ivtnum = 12                                                  
           ivcomp = 0                                                   
           ivcorr =   -26                                               
      dvn001 = 3.54d0                                                   
      do ivn001 = -5.3, 2*(dvn001 - 8), -1.46                      
      ivcomp = ivcomp + ivn001                                          
       end do
40120 if (ivcomp +    26) 20120, 10120, 20120                      
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0121      continue                                                     
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 719  ****                         
!                                                                        
           ivtnum = 13                                                  
           rvcomp = 0.0                                                 
           rvcorr = 4.84e-6                                             
      ivn001 = 1                                                        
      dvn001 = 2.0d-7                                                   
      do rvn001 = (ivn001 + .12)*1.0e-6, dvn001*(6 + 0.7), 6.0e-8  
      rvcomp = rvcomp + rvn001                                          
       end do
           if (rvcomp - 0.48397e-05) 20130, 10130, 40130                
40130 if (rvcomp - 0.48403e-05) 10130, 10130, 20130                
10130 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           write (i02,80012) ivtnum, rvcomp, rvcorr                     
 0131      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 719  ****                         
!                                                                        
           ivtnum = 14                                                  
           dvcomp = 0.0d0                                               
           dvcorr = 1.8d3                                               
      ivn001 = 1                                                        
      do dvn001 = 2.25e+2, 300*(1.65 + ivn001), 150                
      dvcomp = dvcomp + dvn001                                          
       end do
           if (dvcomp - 0.1799999999d+04) 20140, 10140, 40140           
40140 if (dvcomp - 0.1800000001d+04) 10140, 10140, 20140           
10140 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0141      continue                                                     
!                                                                        
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
90001 format (" ",56x,"FM719")                                          
90000 format (" ",50x,"END OF PROGRAM FM719" )                          
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
      stop                                                              
      end program fm719
      function if720(ivn001)
      integer :: ivn001
      if720 = 2*ivn001 - 1                                              
      return                                                            
      end function if720
      subroutine sn721(ivn001,*)
      integer :: ivn001
      integer :: ivn002
      ivn001 = ivn001 + 1                                               
      return 1                                                          
      entry en721(ivn002,*)
      ivn002 = 2*ivn002                                                 
      return 1                                                          
      entry en722(ivn003,*,*)
      return ivn003                                                     
      end subroutine sn721
