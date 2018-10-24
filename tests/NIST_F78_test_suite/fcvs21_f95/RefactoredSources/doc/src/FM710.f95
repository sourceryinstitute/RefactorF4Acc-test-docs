      program fm710
!                                                                        
!      THIS ROUTINE TESTS SUBSCRIPT EXPRESSIONS AND          ANS REF.    
!      CHARACTER SUBSTRINGS.                                 5.4.2, 5.4.3
!                                                            5.7.1, 5.7.2
!                                                                        
!      THIS ROUTINE ASSUMES THE INTRINSIC FUNCTIONS                      
!                               INT AND IABS ARE WORKING.                
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
      integer :: ivcomp
      integer :: ivcorr
      real :: rvd001
      integer :: ivn001
      integer :: ivn002
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
      integer, dimension(1:2,1:3) :: i2n001
      integer, dimension(1:3,1:5) :: i2n002
      integer, dimension(-1:8) :: i1n003
      integer, dimension(1:10,1:4) :: i2n004
! BE** ********************** BBCINITA **********************************
!                                                                        
      character(len=10) :: cvcomp
      character(len=10) :: cvcorr
      character(len=10) :: cvn001
      character(len=10), dimension(1:2,1:4) :: c2n001
      data i2n001 / 1,2,3,4,5,6 / 
      data i2n002 / 11,21,31,12,22,32,13,23,33,14,24,34,15,25,35 / 
      data i1n003 / 1,2,3,4,5,6,7,8,9,10 / 
      data i2n004 / 10,9,8,7,6,5,4,3,2,1,4,-2,6,-3,8,-4,10,-5,2,-1,1,3,5,7,9,2,4,6,8,10,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1 / 
      data c2n001 / '11FIRSTELE','21SECONDXX','12THIRDXYZ','22FOURTHWW','13FIFTHABC','23SIXTHIJK','14SEVENTHH','24EIGHTHUV' / 
      data zvers,zversd,zdate / 'VERSION 2.1  ','93/10/21*21.02.00','*NO DATE*TIME' / 
      data zcompl,zname,ztape / '*NONE SPECIFIED*','*NO COMPANY NAME*','*NO TAPE*' / 
      data zproj,ztaped,zprog / '*NO PROJECT*','*NO TAPE DATE','XXXXX' / 
      data remrks / '                               ' / 
!                                                                        
!                                                                        
! BB** ********************** BBCINITB **********************************
! **** INITIALIZE SECTION                                                
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
           zprog='FM710'                                                
           ivtotl =  19                                                 
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
!      TESTS 1-2 - SUBSCRIPT EXPRESSION TO IDENTIFY VARIOUS              
!                  ARRAY ELEMENTS                                        
!                                                                        
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 001 ARRAY ELEMENT REFERENCE                                  
!                                                                        
           ivtnum =   1                                                 
           ivcomp = 0                                                   
           ivcorr = 34                                                  
      ivcomp = i2n002(i2n001(1,2),i2n001(2,3)/2 + 1)                    
40010 if (ivcomp - 34) 20010, 10010, 20010                         
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 002 FUNCTION REFERENCE                                       
!                                                                        
           ivtnum =   2                                                 
           rvd001 = 2.64                                                
           ivcomp = 0                                                   
           ivcorr = 25                                                  
      ivcomp = i2n002(int(rvd001), 19 - iabs(-7)*2)                     
40020 if (ivcomp - 25) 20020, 10020, 20020                         
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0021      continue                                                     
!                                                                        
!      TESTS 3-7 - TEST SUBSCRIPT VALUE IN IDENTIFYING                   
!                  ARRAY ELEMENTS                                        
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 003 RANGE                                                    
!                                                                        
           ivtnum = 3                                                   
           write (i02, 80004) ivtnum                                    
           write (i02, 80020)                                           
      write (i02, 70030) (i1n003(ivn001), ivn001=5,8)                   
70030 format (" ",26x,4i4)                                              
           ivinsp = ivinsp + 1                                          
           write (i02, 80022)                                           
           write (i02, 70031)                                           
70031 format (" ",26x,"   7   8   9  10" )                         
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 004 SINGLE ELEMENT                                           
!                                                                        
           ivtnum =   4                                                 
           ivcomp = 0                                                   
           ivcorr = 4                                                   
      ivcomp = i1n003(2)                                                
40040 if (ivcomp - 4) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 005 EXPRESSION                                               
!                                                                        
           ivtnum =   5                                                 
           ivn001 = -3                                                  
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      ivcomp = i1n003((ivn001+5)*3 - 7)                                 
40050 if (ivcomp - 1) 20050, 10050, 20050                          
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0051      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 006 31ST ELEMENT IN 2 DIMENSIONAL, 40 ELEMENT ARRAY          
!                                                                        
           ivtnum =   6                                                 
           ivcomp = 0                                                   
           ivcorr = -10                                                 
      ivcomp = i2n004(1,4)                                              
40060 if (ivcomp + 10) 20060, 10060, 20060                         
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 007 4TH ELEMENT OF FIRST ARRAY EQUAL TO                      
!               11TH ELEMENT OF SECOND ARRAY                             
!                                                                        
           ivtnum =   7                                                 
           ivcomp = 0                                                   
           ivcorr = 1                                                   
           if (i1n003(2) == i2n004(1,2)) ivcomp = 1                     
40070 if (ivcomp - 1) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0071      continue                                                     
!                                                                        
!      TESTS 8-15 - CHARACTER SUBSTRING NAME                             
!                                                                        
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 008 USING LEFT AND RIGHT POSITION OF SUBSTRING               
!                                                                        
           ivtnum =   8                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvn001 = 'THIS IS IT'                                        
           cvcorr = 'HIS       '                                        
      cvcomp = cvn001(2:4)                                              
           if (cvcomp  ==  'HIS       ') ivcomp = 1                     
           if (ivcomp - 1) 20080, 10080, 20080                          
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 009 LEFT POSITION OMITTED, VALUE OF 1 ASSUMED                
!                                                                        
           ivtnum =   9                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'THIS      '                                        
      cvcomp = cvn001(:4)                                               
           if (cvcomp  ==  'THIS      ') ivcomp = 1                     
           if (ivcomp - 1) 20090, 10090, 20090                          
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0091      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 010 RIGHT POSITION OMITTED, RIGHT-HAND END OF STRING ASSUMED 
!                                                                        
           ivtnum =  10                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'S IS IT   '                                        
      cvcomp = cvn001(4:)                                               
           if (cvcomp  ==  'S IS IT   ') ivcomp = 1                     
           if (ivcomp - 1) 20100, 10100, 20100                          
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0101      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 011 EXTRACT SUBSTRING FROM ARRAY                             
!                                                                        
           ivtnum =  11                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = '12THIR    '                                        
      cvcomp = c2n001(1,2)(1:6)                                         
           if (cvcomp  ==  '12THIR    ') ivcomp = 1                     
           if (ivcomp - 1) 20110, 10110, 20110                          
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0111      continue                                                     
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 012 ENTIRE SUBSTRING                                         
!                                                                        
           ivtnum =  12                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'THIS IS IT'                                        
      cvcomp = cvn001(:)                                                
           if (cvcomp  ==  'THIS IS IT') ivcomp = 1                     
           if (ivcomp - 1) 20120, 10120, 20120                          
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0121      continue                                                     
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 013 ENTIRE SUBSTRING FROM ARRAY                              
!                                                                        
           ivtnum =  13                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = '23SIXTHIJK'                                        
      cvcomp = c2n001(2,3)(:)                                           
           if (cvcomp  ==  '23SIXTHIJK') ivcomp = 1                     
           if (ivcomp - 1) 20130, 10130, 20130                          
10130 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0131      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      RIGHT POSITION OMITTED USING ARRAY                                
!                                                                        
           ivtnum =  14                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'EVENTHH   '                                        
      cvcomp = c2n001(1,4)(4:)                                          
           if (cvcomp  ==  'EVENTHH   ') ivcomp = 1                     
           if (ivcomp - 1) 20140, 10140, 20140                          
10140 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0141      continue                                                     
!                                                                        
! T015*  TEST 015   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      LEFT POSITION OMITTED                                             
!                                                                        
           ivtnum =  15                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = '24EI      '                                        
      cvcomp = c2n001(2,4)(:4)                                          
           if (cvcomp  ==  '24EI      ') ivcomp = 1                     
           if (ivcomp - 1) 20150, 10150, 20150                          
10150 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0151      continue                                                     
!                                                                        
!      TESTS 16-19 - SUBSTRING EXPRESSION                                
!                                                                        
!                                                                        
! T016*  TEST 016   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 016 ARITHMETIC EXPRESSION                                    
!                                                                        
           ivtnum =  16                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'HIS IS IT '                                        
      cvcomp = cvn001(2:5*2)                                            
           if (cvcomp  ==  'HIS IS IT ') ivcomp = 1                     
           if (ivcomp - 1) 20160, 10160, 20160                          
10160 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0161      continue                                                     
!                                                                        
! T017*  TEST 017   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 017 SUBSTRING EXPRESSION IN AN ASSIGNMENT STATEMENT          
!                                                                        
           ivtnum =  17                                                 
           ivn001 = 5                                                   
           ivn002 = 8                                                   
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'THISLIKEIT'                                        
      cvn001(ivn001:ivn002) = 'LIKE'                                    
           cvcomp = cvn001                                              
           if (cvcomp  ==  'THISLIKEIT') ivcomp = 1                     
           if (ivcomp - 1) 20170, 10170, 20170                          
10170 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0171      continue                                                     
!                                                                        
! T018*  TEST 018   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 018 SUBSTRING EXPRESSION CONTAINING ARRAY ELEMENT REFERENCE  
!                                                                        
           ivtnum =  18                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'HISLIKE   '                                        
      cvcomp = cvn001(i2n001(2,1):i2n002(3,5)-27)                       
           if (cvcomp  ==  'HISLIKE   ') ivcomp = 1                     
           if (ivcomp - 1) 20180, 10180, 20180                          
10180 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0181      continue                                                     
!                                                                        
! T019*  TEST 019   ****  FCVS PROGRAM 710  ****                         
!                                                                        
!      TEST 019 SUBSTRING EXPRESSION CONTAINING FUNCTION REFERENCES      
!                                                                        
           ivtnum =  19                                                 
           rvd001 = 1.475                                               
           ivn001 = 1                                                   
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'IFTHABC   '                                        
      cvcomp = c2n001(1,3)(int(rvd001)+3 : (ivn001*5 + 7)/iabs(-6) + 8) 
           if (cvcomp  ==  'IFTHABC   ') ivcomp = 1                     
           if (ivcomp - 1) 20190, 10190, 20190                          
10190 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0191      continue                                                     
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
90001 format (" ",56x,"FM710")                                          
90000 format (" ",50x,"END OF PROGRAM FM710" )                          
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
           end program fm710
