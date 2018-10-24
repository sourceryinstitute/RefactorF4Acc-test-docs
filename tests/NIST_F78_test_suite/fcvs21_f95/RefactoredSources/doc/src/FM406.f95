      program fm406
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM406                                                          
! *****                       INTER2 - (391)                             
! *****                                                                  
! ***********************************************************************
! *****  TESTING OF INTERNAL FILES -                           SUBSET REF
! *****          USING WRITE                                     12.2.5  
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
! *****  S P E C I F I C A T I O N S  SEGMENT 391                        
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
      integer :: kvi
      integer :: ivcomp
      integer :: i
      real :: avs
      real :: bvs
        logical :: avb
        character(len=4) :: a4vk
        character(len=5) :: a5vk
        character(len=10) :: a10vk
        character(len=38) :: a38vk
        character(len=38) :: cvcorr
        character(len=38), dimension(1:8) :: avcorr
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
      zprog = 'FM406'                                                   
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
! *****                                                                  
! *****    HEADER FOR SEGMENT 391                                        
! *****                                                                  
        write(nuvi,39100)                                               
39100 format(" ",/ " INTER2 - (391) INTERNAL FILES -- USING WRITE"              //" SUBSET REF. - 12.2.5" )                             
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
        write (nuvi, 39199)                                             
39199 format (" ",48x,"NOTE 1: OPTIONAL LEADING ZERO  " /                       " ",48x,"   MAY BE BLANK FOR ABSOLUTE   " /                       " ",48x,"   VALUE < 1                   " /                       " ",48x,"NOTE 2: LEADING PLUS SIGN IS   " /                       " ",48x,"   OPTIONAL                    " /                       " ",48x,"NOTE 3: E EXPONENT MAY BE E+   " /                       " ",48x,"   OR +0 BEFORE VALUE          " )             
! T001*  TEST 1                              CHARACTER VARIABLE, INTEGER 
           ivtnum = 1                                                   
        a10vk = 'XXXXXXXXXX'                                            
        kvi = 3                                                         
        write(a10vk,39101) kvi                                          
39101 format(i2)                                                      
           ivcomp = 0                                                   
           avcorr(1) = ' 3        '                                     
           avcorr(2) = '+3        '                                     
           do i = 1, 2                                            
           if (a10vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40011, 10010, 40011                          
           40011 end do
           goto 20010                                                  
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           cvcorr = ' 3        '                                        
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a10vk                                    
           write (nuvi, 80022) cvcorr                                   
 0011      continue                                                     
! T002*  TEST 2                                          REAL, FW.D      
           ivtnum = 2                                                   
        a10vk = 'XXXXXXXXXX'                                            
        avs = 2.1                                                       
        write(a10vk,39103) avs                                          
39103 format(f3.1)                                                    
           ivcomp = 0                                                   
           if (a10vk == '2.1       ') ivcomp = 1                        
           if (ivcomp - 1) 20020, 10020, 20020                          
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           cvcorr = '2.1       '                                        
           write (nuvi, 80018) ivtnum, a10vk, cvcorr                    
 0021      continue                                                     
! T003*  TEST 3                                   CHECK FOR MISSING SIGN 
           ivtnum = 3                                                   
        a10vk = 'XXXXXXXXXX'                                            
        avs = -0.0001                                                   
        write(a10vk,39104) avs                                          
39104 format(f4.1)                                                    
           ivcomp = 0                                                   
           avcorr(1) = ' 0.0      '                                     
           avcorr(2) = '  .0      '                                     
           avcorr(3) = '+0.0      '                                     
           avcorr(4) = ' +.0      '                                     
           do i = 1, 4                                            
           if (a10vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40031, 10030, 40031                          
           40031 end do
           goto 20030                                                  
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           cvcorr = ' 0.0      '                                        
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a10vk                                    
           write (nuvi, 80022) cvcorr                                   
 0031      continue                                                     
! T004*  TEST 4                              CONVERSION ERROR            
           ivtnum = 4                                                   
        a10vk = 'XXXXXXXXXX'                                            
        avs = 231.75                                                    
        write(a10vk,39105) avs                                          
39105 format(f4.2)                                                    
           ivcomp = 0                                                   
           if (a10vk == '****      ') ivcomp = 1                        
           if (ivcomp - 1) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           cvcorr = '****      '                                        
           write (nuvi, 80018) ivtnum, a10vk, cvcorr                    
 0041      continue                                                     
! T005*  TEST 5                                          REAL, EW.D      
           ivtnum = 5                                                   
        a10vk = 'XXXXXXXXXX'                                            
        avs = 23.45e2                                                   
        write(a10vk,39106) avs                                          
39106 format(1x,e9.4)                                                 
           ivcomp = 0                                                   
           avcorr(1) = ' .2345E+04'                                     
           avcorr(2) = ' .2345+004'                                     
           do i = 1, 2                                            
           if (a10vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40051, 10050, 40051                          
           40051 end do
           goto 20050                                                  
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           cvcorr = ' .2345E+04'                                        
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a10vk                                    
           write (nuvi, 80022) cvcorr                                   
 0051      continue                                                     
! T006*  TEST 6                                          REAL, EW.DEN    
           ivtnum = 6                                                   
        a10vk = 'XXXXXXXXXX'                                            
        write(a10vk,39107) avs                                          
39107 format(1x,e8.4e1)                                               
           ivcomp = 0                                                   
           avcorr(1) = ' .2345E+4 '                                     
           avcorr(2) = ' .2345+04 '                                     
           do i = 1, 2                                            
           if (a10vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40061, 10060, 40061                          
           40061 end do
           goto 20060                                                  
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           cvcorr = ' .2345E+4 '                                        
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a10vk                                    
           write (nuvi, 80022) cvcorr                                   
 0061      continue                                                     
! T007*  TEST 7                                          LOGICAL         
           ivtnum = 7                                                   
        a10vk = 'XXXXXXXXXX'                                            
        avb = .true.                                                    
        write(a10vk,39108) avb                                          
39108 format(l6)                                                      
           ivcomp = 0                                                   
           if (a10vk == '     T    ') ivcomp = 1                        
           if (ivcomp - 1) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           cvcorr = '     T    '                                        
           write (nuvi, 80018) ivtnum, a10vk, cvcorr                    
 0071      continue                                                     
! T008*  TEST 8                                          CHARACTER, AW   
           ivtnum = 8                                                   
        a10vk = 'XXXXXXXXXX'                                            
        a4vk = 'TEST'                                                   
        write(a10vk,39109) a4vk                                         
39109 format(a4)                                                      
           ivcomp = 0                                                   
           if (a10vk == 'TEST      ') ivcomp = 1                        
           if (ivcomp - 1) 20080, 10080, 20080                          
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           cvcorr = 'TEST      '                                        
           write (nuvi, 80018) ivtnum, a10vk, cvcorr                    
 0081      continue                                                     
! T009*  TEST 9                                          BLANK RECORD    
           ivtnum = 9                                                   
         a10vk = 'XXXXXXXXXX'                                           
         write(a10vk,39110)                                             
39110 format()                                                       
            ivcomp = 0                                                  
            if (a10vk == '          ') ivcomp = 1                       
            if (ivcomp - 1) 20090, 10090, 20090                         
10090 ivpass = ivpass + 1                                         
            write (nuvi, 80002) ivtnum                                  
            goto 0091                                                  
20090 ivfail = ivfail + 1                                         
            cvcorr = '          '                                       
            write (nuvi, 80018) ivtnum, a10vk, cvcorr                   
 0091       continue                                                    
! T010*  TEST 10                                         MIXED TYPES     
           ivtnum = 10                                                  
        a38vk = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'                
        kvi = 23                                                        
        avs = 23.345                                                    
        avb = .true.                                                    
        a4vk = 'ENDS'                                                   
        write(a38vk,39111) kvi, avs, avb, a4vk                          
39111 format(i5,1x,f8.3,1x,l5,1x,a4)                                  
           ivcomp = 0                                                   
           avcorr(1) = '   23   23.345     T ENDS             '         
           avcorr(2) = '  +23  +23.345     T ENDS             '         
           avcorr(3) = '   23  +23.345     T ENDS             '         
           avcorr(4) = '  +23   23.345     T ENDS             '         
           do i = 1, 4                                            
           if (a38vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40101, 10100, 40101                          
           40101 end do
           goto 20100                                                  
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           cvcorr = '   23   23.345     T ENDS             '            
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a38vk                                    
           write (nuvi, 80022) cvcorr                                   
 0101      continue                                                     
! T011*  TEST 11                                 MIXED TYPES, WITH       
! *****                                  CHARACTER AND HOLLERITH STRINGS 
           ivtnum = 11                                                  
        a38vk = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'                
        avs = 23.456                                                    
        avb = .false.                                                   
        kvi = 98                                                        
        a5vk = 'YOURS'                                                  
        write(a38vk,39112) avs, avb, kvi, a5vk                          
39112 format(f7.3,1x,l5,1x,i5,1x,a5,1x,'PROGRAMS',1x,"ONE")           
           ivcomp = 0                                                   
           avcorr(1) = ' 23.456     F    98 YOURS PROGRAMS ONE'         
           avcorr(2) = '+23.456     F   +98 YOURS PROGRAMS ONE'         
           avcorr(3) = ' 23.456     F   +98 YOURS PROGRAMS ONE'         
           avcorr(4) = '+23.456     F    98 YOURS PROGRAMS ONE'         
           do i = 1, 4                                            
           if (a38vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40111, 10110, 40111                          
           40111 end do
           goto 20110                                                  
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           cvcorr = ' 23.456     F    98 YOURS PROGRAMS ONE'            
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a38vk                                    
           write (nuvi, 80022) cvcorr                                   
 0111      continue                                                     
! T012*  TEST 12                           MIXED TYPES, WITH EXPRESSION  
           ivtnum = 12                                                  
        a38vk = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'                
        avs = 5.2345                                                    
        bvs = 1.2345                                                    
        avb = .true.                                                    
        write(a38vk,39113) avs, 5, bvs*2, avb, 'TWO'                    
39113 format(f9.4,1x,i4,1x,"BVS",1x,f9.4,1x,l1,1x,a3)                 
           ivcomp = 0                                                   
           avcorr(1) = '   5.2345    5 BVS    2.4690 T TWO    '         
           avcorr(2) = '   5.2345    5 BVS   +2.4690 T TWO    '         
           avcorr(3) = '   5.2345   +5 BVS    2.4690 T TWO    '         
           avcorr(4) = '   5.2345   +5 BVS   +2.4690 T TWO    '         
           avcorr(5) = '  +5.2345    5 BVS    2.4690 T TWO    '         
           avcorr(6) = '  +5.2345    5 BVS   +2.4690 T TWO    '         
           avcorr(7) = '  +5.2345   +5 BVS    2.4690 T TWO    '         
           avcorr(8) = '  +5.2345   +5 BVS   +2.4690 T TWO    '         
           do i = 1, 8                                            
           if (a38vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40121, 10120, 40121                          
           40121 end do
           goto 20120                                                  
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           cvcorr = '   5.2345    5 BVS    2.4690 T TWO    '            
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a38vk                                    
           write (nuvi, 80022) cvcorr                                   
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
! *****    END OF TEST SEGMENT 391                                       
      stop                                                              
      end program fm406
