      program fm909
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM909                                                          
! *****                       INTER4 - (393)                             
! *****                                                                  
! ***********************************************************************
! *****  TESTING OF INTERNAL FILES -                            ANS. REF 
! *****          USING WRITE                                     12.2.5  
! *****                                                                  
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
! *****  S P E C I F I C A T I O N S  SEGMENT 393                        
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
      integer :: ivcomp
      integer :: i
      integer :: jvi
      integer :: kvi
      real :: avs
      real :: bvs
        logical :: avb
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
        double precision :: dvd
        double precision, dimension(1:5) :: b1d
        complex :: avc
        complex :: bvc
        complex :: cvc
        character(len=8) :: a8vk
        character(len=97) :: a97vk
        character(len=97) :: cvcorr
        character(len=97), dimension(1:24) :: avcorr
        character(len=29), dimension(1:5) :: a291k
        character(len=43), dimension(1:2) :: a431k
        character(len=1), dimension(1:97) :: a97e1
        character(len=1), dimension(1:97) :: a97e2
        equivalence (a97vk, a97e1), (a431k, a97e1)                      
        equivalence (cvcorr, a97e2)                                     
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
      ivtotl = 27                                                       
      zprog = 'FM909'                                                   
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
! *****    HEADER FOR SEGMENT 393                                        
! *****                                                                  
        write(nuvi,39300)                                               
39300 format(" ",/ " INTER4 - (393) INTERNAL FILES --  USING WRITE"           //" ANS. REF. - 12.2.5" )                                 
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
39199 format (" ",48x,"NOTE 1: FOR NUMERIC VALUES,    " /                       " ",48x,"   OPTIONAL LEADING ZERO MAY BE" /                       " ",48x,"   BLANK FOR ABSOLUTE VALUE < 1" /                       " ",48x,"NOTE 2: LEADING PLUS SIGN IS   " /                       " ",48x,"   OPTIONAL FOR NUMERIC VALUES " /                       " ",48x,"NOTE 3: E FORMAT EXPONENT MAY  " /                       " ",48x,"   BE E+NN OR +0NN FOR REALS   " /                       " ",48x,"NOTE 4: D FORMAT EXPONENT MAY  " /                       " ",48x,"   BE D+NN, E+NN, OR +0NN FOR  " /                       " ",48x,"   DOUBLE PRECISION VALUES     " /)            
           ivtnum = 1                                                   
! *****                                                                  
! T001*  TEST 1                          DOUBLE PRECISION INTO VARIABLE  
        a97vk = 'XXXXXXXXXXXXXXXXXX'                                    
        avd = 23.456d3                                                  
        write(unit=a97vk,fmt=39301) avd                                 
39301 format(13x,d10.5)                                               
           ivcomp = 0                                                   
           avcorr(1) = '             .23456D+05'                        
           avcorr(2) = '             .23456E+05'                        
           avcorr(3) = '             .23456+005'                        
           do i = 1, 3                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40011, 10010, 40011                          
           40011 end do
           goto 20010                                                  
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           cvcorr = '             .23456D+05'                           
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
70010 format(" ",16x,"COMPUTED: " ,54a1)                           
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
70020 format(" ",26x,43a1)                                         
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
70030 format(" ",16x,"CORRECT:  " ,54a1)                           
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
70040 format(" ",26x,43a1)                                         
 0011      continue                                                     
! T002*  TEST 2                                   INTO ARRAY ELEMENT     
           ivtnum = 2                                                   
        avd = 2.1d1                                                     
        a431k(1) = ' '                                                  
        a431k(2) = 'WRONG'                                              
        write(unit=a431k(1),fmt=39303) avd                              
39303 format(d12.7)                                                   
           ivcomp = 0                                                   
           avcorr(1) = '.2100000D+02'                                   
           avcorr(2) = '.2100000E+02'                                   
           avcorr(3) = '.2100000+002'                                   
           do i = 1, 3                                            
           if (a431k(1) == avcorr(i)) ivcomp = 1                        
           if (ivcomp - 1) 40021, 10020, 40021                          
           40021 end do
           goto 20020                                                  
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           cvcorr = '.2100000D+02'                                      
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a431k(1)                                 
           write (nuvi, 80022) cvcorr                                   
 0021      continue                                                     
! T003*  TEST 3                                     CHARACTER SUBSTRING  
           ivtnum = 3                                                   
        a97vk = ' SOME WHERE'                                           
        avd = 23.45d2                                                   
        write(unit=a97vk(21:),fmt=39305) avd                            
39305 format(11x,d14.9)                                               
           ivcomp = 0                                                   
           avcorr(1) = ' SOME WHERE                    .234500000D+04'  
           avcorr(2) = ' SOME WHERE                    .234500000E+04'  
           avcorr(3) = ' SOME WHERE                    .234500000+004'  
           do i = 1, 3                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40031, 10030, 40031                          
           40031 end do
           goto 20030                                                  
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           cvcorr =    ' SOME WHERE                    .234500000D+04'  
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0031      continue                                                     
! *****    TESTS 4 - 5                              ARRAY, SINGLE RECORD 
! T004*  TEST 4                                                          
           ivtnum = 4                                                   
        cvd = 23.45d2                                                   
        a431k(2) = ' '                                                  
        write(unit=a431k,fmt=39306) cvd                                 
39306 format(24x,d19.10)                                              
           ivcomp = 0                                                   
           avcorr(1) = '                           0.2345000000D+04'    
           avcorr(2) = '                           0.2345000000E+04'    
           avcorr(3) = '                           0.2345000000+004'    
           avcorr(4) = '                            .2345000000D+04'    
           avcorr(5) = '                            .2345000000E+04'    
           avcorr(6) = '                            .2345000000+004'    
           avcorr(7) = '                           +.2345000000D+04'    
           avcorr(8) = '                           +.2345000000E+04'    
           avcorr(9) = '                           +.2345000000+004'    
           avcorr(10) = '                          +0.2345000000D+04'   
           avcorr(11) = '                          +0.2345000000E+04'   
           avcorr(12) = '                          +0.2345000000+004'   
           do i = 1, 12                                           
           if (a431k(1) == avcorr(i)) ivcomp = 1                        
           if (ivcomp - 1) 40041, 10040, 40041                          
           40041 end do
           goto 20040                                                  
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           cvcorr = '                           0.2345000000D+04'       
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70050) (a97e1(i), i = 1,43)                     
           write (nuvi, 70060) (a97e2(i), i = 1,43)                     
70050 format(" ",16x,"COMPUTED: " ,43a1)                           
70060 format(" ",16x,"CORRECT:  " ,43a1)                           
 0041      continue                                                     
! T005*  TEST 5                                                          
           ivtnum = 5                                                   
           ivcomp = 0                                                   
           if (a431k(2) == ' ') ivcomp = 1                              
           if (ivcomp - 1) 20050, 10050, 20050                          
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           cvcorr = ' '                                                 
           write (nuvi, 80018) ivtnum, a431k(2), cvcorr                 
 0051      continue                                                     
! *****    TESTS 6 - 10             ARRAY, 5 RECORDS, ONE BLANK          
! T006*  TEST 6                                                          
           ivtnum = 6                                                   
        b1d(1) = 11d1                                                   
        b1d(2) = 21d1                                                   
        b1d(3) = 31d1                                                   
        b1d(4) = 32d1                                                   
        b1d(5) = 51d1                                                   
        write(unit=a291k,fmt=39307) (b1d(jvi), jvi=1,5)                 
39307 format(e11.6e2/1x,e10.5e2/2x,2(e9.4e2,3x)//4x,e7.2e2)           
           ivcomp = 0                                                   
           if (a291k(1) == '.110000E+03') ivcomp = 1                    
           if (ivcomp - 1) 20060, 10060, 20060                          
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           cvcorr = '.110000E+03'                                       
           write (nuvi, 80018) ivtnum, a291k(1), cvcorr                 
 0061      continue                                                     
! T007*  TEST 7                                                          
           ivtnum = 7                                                   
           ivcomp = 0                                                   
           if (a291k(2) == ' .21000E+03') ivcomp = 1                    
           if (ivcomp - 1) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           cvcorr = ' .21000E+03'                                       
           write (nuvi, 80018) ivtnum, a291k(2), cvcorr                 
 0071      continue                                                     
! T008*  TEST 8                                                          
           ivtnum = 8                                                   
           ivcomp = 0                                                   
           if (a291k(3) == '  .3100E+03   .3200E+03') ivcomp = 1        
           if (ivcomp - 1) 20080, 10080, 20080                          
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           cvcorr = '  .3100+003   .3200E+03'                           
           write (nuvi, 70070) ivtnum, a291k(3), cvcorr                 
70070 format (" ",2x,i3,4x," FAIL  ",/," ",16x,"COMPUTED: " ,                   a29,/," ",16x,"CORRECT:  " ,a29)                     
 0081      continue                                                     
! T009*  TEST 9                                                          
           ivtnum = 9                                                   
           ivcomp = 0                                                   
           if (a291k(4) == ' ') ivcomp = 1                              
           if (ivcomp - 1) 20090, 10090, 20090                          
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           cvcorr = ' '                                                 
           write (nuvi, 80018) ivtnum, a291k(4), cvcorr                 
 0091      continue                                                     
! T010*  TEST 10                                                         
           ivtnum = 10                                                  
           ivcomp = 0                                                   
           if (a291k(5) == '    .51E+03') ivcomp = 1                    
           if (ivcomp - 1) 20100, 10100, 20100                          
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           cvcorr = '    .51E+03'                                       
           write (nuvi, 80018) ivtnum, a291k(5), cvcorr                 
 0101      continue                                                     
! *****                                                                  
        write(nuvi, 90002)                                              
        write(nuvi, 90013)                                              
        write(nuvi, 90014)                                              
! *****                                                                  
! T011*  TEST 11                           VARIABLE, MORE THEN ONE FIELD 
           ivtnum = 11                                                  
        avd = 34.58673d2                                                
        bvd = 34.58673d2                                                
        cvd = 34.58673d2                                                
        dvd = 34.58673d2                                                
        write(unit=a97vk,fmt=39309) avd, bvd, cvd, dvd                  
39309 format(d10.5,1x,f10.5,1x,d11.5,g11.5)                           
           ivcomp = 0                                                   
           cvcorr = '.34587D+04 3458.67300 0.34587D+04 3458.7'          
           if (a97vk == cvcorr) ivcomp = 1                              
           if (ivcomp - 1) 20110, 10110, 20110                          
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           remrks = '54 PERMISSIBLE REPRESENTATIONS'                    
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'SEE NOTES ABOVE'                                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0111      continue                                                     
! T012*  TEST 12                                 GW.D FIELD WITH D.P.    
           ivtnum = 12                                                  
        avd = 314.5673d0                                                
        bvd = 14.45673d-1                                               
        cvd = 85.7343d6                                                 
        write(unit=a97vk,fmt=39310) avd, bvd, cvd                       
39310 format(g12.5,1x,g14.5e3,1x,g10.5e2)                             
           ivcomp = 0                                                   
           avcorr(1) = '  314.57        1.4457      .85734E+08'         
           avcorr(2) = ' +314.57        1.4457      .85734E+08'         
           avcorr(3) = '  314.57       +1.4457      .85734E+08'         
           avcorr(4) = ' +314.57       +1.4457      .85734E+08'         
           do i = 1, 4                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40121, 10120, 40121                          
           40121 end do
           goto 20120                                                  
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           cvcorr = '  314.57        1.4457      .85734E+08'            
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS. SEE '                    
           write (nuvi, 80050) remrks                                   
           remrks = 'NOTES ABOVE'                                       
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0121      continue                                                     
! T013*  TEST 13                         DIFFERENT TYPES IN SAME RECORD  
           ivtnum = 13                                                  
        kvi = 348                                                       
        avs = 34.783                                                    
        avd = 384.3847d1                                                
        avb = .true.                                                    
        bvs = 3.4857                                                    
        a8vk = 'KDFJ D/.'                                               
        write(unit=a97vk,fmt=39311) kvi, avs, avd, avb, bvs, a8vk       
39311 format(i4,1x,e9.4,1x,d10.4,1x,l4,1x,f12.5,1x,a8)                
           ivcomp = 0                                                   
           cvcorr = ' 348 .3478E+02 0.3844D+04    T      3.48570 KDFJ D/.'                                                                
           if (a97vk == cvcorr) ivcomp = 1                              
           if (ivcomp - 1) 20130, 10130, 20130                          
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           remrks = '72 PERMISSIBLE REPRESENTATIONS'                    
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'SEE NOTES ABOVE'                                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0131      continue                                                     
! T014*  TEST 14                                 POSITIONAL EDITING      
           ivtnum = 14                                                  
        avb = .true.                                                    
        avs = 10.98                                                     
        a8vk = 'THISISIT'                                               
        avd = 3.4945d2                                                  
        bvs = 3.4945                                                    
        kvi = 3                                                         
        write(unit=a97vk,fmt=39312) avb, avs, a8vk, avd, bvs, kvi       
39312 format(l1,t5,f5.2,a8,tr2,e10.4e2,tl10,f6.4,6x,i1)               
           ivcomp = 0                                                   
           if (a97vk == 'T   10.98THISISIT  3.4945E+03  3')                  ivcomp = 1                                                   
           if (ivcomp - 1) 20140, 10140, 20140                          
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           cvcorr = 'T   10.98THISISIT  3.4945E+03  3'                  
           write (nuvi, 80008) ivtnum                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0141      continue                                                     
! T015*  TEST 15                                      COLON AND SIGN     
           ivtnum = 15                                                  
        avb = .true.                                                    
        avs = 98.11                                                     
        a8vk = 'THISISIT'                                               
        avd = 3.4945d2                                                  
        kvi = 33                                                        
        write(unit=a97vk,fmt=39313) avb, avs, a8vk, avd, kvi            
39313 format(l1,s,f7.2,a8,sp,d11.5,6x,ss,i2,:,f9.3)                   
           ivcomp = 0                                                   
           avcorr(1) = 'T  98.11THISISIT+.34945D+03      33'            
           avcorr(2) = 'T  98.11THISISIT+.34945E+03      33'            
           avcorr(3) = 'T  98.11THISISIT+.34945+003      33'            
           do i = 1, 3                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40151, 10150, 40151                          
           40151 end do
           goto 20150                                                  
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           cvcorr = 'T  98.11THISISIT+.34945D+03      33'               
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0151      continue                                                     
! T016*  TEST 16                             COMPLEX TYPES INTO VARIABLE 
           ivtnum = 16                                                  
        avc = (2.343, 34.394)                                           
        write(unit=a97vk,fmt=39314) avc                                 
39314 format(f10.5,1x,f10.5)                                          
           ivcomp = 0                                                   
           avcorr(1) = '   2.34300   34.39400'                          
           avcorr(2) = '   2.34300  +34.39400'                          
           avcorr(3) = '  +2.34300   34.39400'                          
           avcorr(4) = '  +2.34300  +34.39400'                          
           do i = 1, 4                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40161, 10160, 40161                          
           40161 end do
           goto 20160                                                  
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           cvcorr = '  +2.34300  +34.39400'                             
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0161      continue                                                     
! T017*  TEST 17                                                         
           ivtnum = 17                                                  
        avc = (34.84, 349.887)                                          
        write(unit=a97vk,fmt=39315) avc                                 
39315 format(e12.5,1x,e12.5)                                          
           ivcomp = 0                                                   
           if (a97vk == ' 0.34840E+02  0.34989E+03') ivcomp = 1         
           if (ivcomp - 1) 20170, 10170, 20170                          
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           cvcorr = ' 0.34840E+02  0.34989E+03'                         
           remrks = '16 PERMISSIBLE REPRESENTATIONS'                    
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'SEE NOTES ABOVE'                                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0171      continue                                                     
! T018*  TEST 18                                       LIST OF COMPLEX   
           ivtnum = 18                                                  
        avc = (2.34, 2.456)                                             
        bvc = (2.34, 2.456)                                             
        cvc = (2.34, 2.456)                                             
        write(unit=a97vk,fmt=39316) avc, bvc, cvc                       
39316 format(2(g9.4,1x),2(g10.4e2,1x),2(g11.5e3,1x))                  
           ivcomp = 0                                                   
           avcorr(1) = '2.340     2.456      2.340      2.456     2.3400      2.4560'                                                     
           avcorr(2) = '2.340     2.456      2.340     +2.456     2.3400      2.4560'                                                     
           avcorr(3) = '2.340     2.456     +2.340      2.456     2.3400      2.4560'                                                     
           avcorr(4) = '2.340     2.456     +2.340     +2.456     2.3400      2.4560'                                                     
           do i = 1, 4                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40181, 10180, 40181                          
           40181 end do
           goto 20180                                                  
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           cvcorr = '2.340     2.456      2.340      2.456     2.3400      2.4560'                                                        
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0181      continue                                                     
! T019*  TEST 19                                    LIST FROM SUBSTRING  
           ivtnum = 19                                                  
        avc = (5.6798, 0.9876)                                          
        bvc = (5.6798, 0.9876)                                          
        cvc = (5.6798, 0.9876)                                          
        write(unit=a97vk(1:),fmt=39317) avc, bvc, cvc                   
39317 format(2(e6.2e1,1x),1x,2(e7.2e2,1x),1x,2(e9.2e3,1x))            
           ivcomp = 0                                                   
           avcorr(1) = '.57E+1 .99E+0  .57E+01 .99E+00   .57E+001  .99E+000'                                                              
           avcorr(2) = '.57E+1 .99E+0  .57E+01 .99E+00   .57E+001 0.99E+000'                                                              
           avcorr(3) = '.57E+1 .99E+0  .57E+01 .99E+00   .57E+001 +.99E+000'                                                              
           avcorr(4) = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001  .99E+000'                                                              
           avcorr(5) = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001 0.99E+000'                                                              
           avcorr(6) = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001 +.99E+000'                                                              
           avcorr(7) = '.57E+1 .99E+0  .57E+01 .99E+00  +.57E+001  .99E+000'                                                              
           avcorr(8) = '.57E+1 .99E+0  .57E+01 .99E+00  +.57E+001 0.99E+000'                                                              
           avcorr(9) = '.57E+1 .99E+0  .57E+01 .99E+00  +.57E+001 +.99E+000'                                                              
           do i = 1, 9                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40191, 10190, 40191                          
           40191 end do
           goto 20190                                                  
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           cvcorr = '.57E+1 .99E+0  .57E+01 .99E+00  0.57E+001 0.99E+000'                                                                 
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0191      continue                                                     
! T020*  TEST 20                                         MIXED TYPES     
           ivtnum = 20                                                  
        avc = (0.934, 34.567)                                           
        avs = 34.65                                                     
        avd = 0.6354d1                                                  
        write(unit=a97vk,fmt=39318) avc, avs, avd                       
39318 format(f7.3,1x,f7.3,1x,f10.5,1x,e13.5e2)                        
           ivcomp = 0                                                   
           if (a97vk == '  0.934  34.567   34.65000   0.63540E+01') ivcomp = 1                                                            
           if (a97vk == '   .934  34.567   34.65000    .63540E+01') ivcomp = 1                                                            
           if (a97vk == '  0.934  34.567   34.64999   0.63540E+01') ivcomp = 1                                                            
           if (a97vk == '   .934  34.567   34.64999    .63540E+01') ivcomp = 1                                                            
           if (ivcomp - 1) 20200, 10200, 20200                          
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           cvcorr = '  0.934  34.567   34.65000   0.63540E+01'          
           remrks = '32 PERMISSIBLE REPRESENTATIONS'                    
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'SEE NOTES ABOVE'                                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0201      continue                                                     
! *****                                                                  
        write(nuvi, 90002)                                              
        write(nuvi, 90013)                                              
        write(nuvi, 90014)                                              
! *****                                                                  
! T021*  TEST 21                     MIXED TYPES WITH POSITIONAL EDITING 
           ivtnum = 21                                                  
        avc = (0.345, 34.349)                                           
        avb = .false.                                                   
        avd = 34.859d-1                                                 
        avs = 10.0                                                      
        a8vk = '12345678'                                               
        write(unit=a97vk,fmt=39319) avc, avb, avd, avs, a8vk            
39319 format(f9.4,1x,e9.4,1x,l1,1x,d12.5,1x,g9.4,a8)                  
           ivcomp = 0                                                   
           if (a97vk == '   0.3450 .3435E+02 F  0.34859D+01 10.00    12345678') ivcomp = 1                                                
           if (ivcomp - 1) 20210, 10210, 20210                          
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           cvcorr = '   0.3450 .3435E+02 F  0.34859D+01 10.00    12345678'                                                                
           remrks = '96 PERMISSIBLE REPRESENTATIONS'                    
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'SEE NOTES ABOVE'                                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0211      continue                                                     
! *****    TESTS 22 - 26                     MIXED TYPES INTO 5 RECORDS  
! T022*  TEST 22                                                         
           ivtnum = 22                                                  
        kvi = 98                                                        
        avd = 84.0489d1                                                 
        avb = .true.                                                    
        avc = (34.0435, 34.94)                                          
        a8vk = 'THE LAST'                                               
        write(unit=a291k,fmt=39320) kvi, avd, avb, avc, a8vk            
39320 format(i5/e10.5e2//1x,l6,2(1x,e10.3)/a8)                        
           ivcomp = 0                                                   
           avcorr(1) = '   98'                                          
           avcorr(2) = '  +98'                                          
           do i = 1, 2                                            
           if (a291k(1) == avcorr(i)) ivcomp = 1                        
           if (ivcomp - 1) 40221, 10220, 40221                          
           40221 end do
           goto 20220                                                  
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           cvcorr = '   98'                                             
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020) a291k(1)                                 
           write (nuvi, 80022) cvcorr                                   
 0221      continue                                                     
! T023*  TEST 23                                                         
           ivtnum = 23                                                  
           ivcomp = 0                                                   
           if (a291k(2) == '.84049E+03') ivcomp = 1                     
           if (ivcomp - 1) 20230, 10230, 20230                          
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           cvcorr = '.84049E+03'                                        
           write (nuvi, 80018) ivtnum, a291k(2), cvcorr                 
 0231      continue                                                     
! T024*  TEST 24                                                         
           ivtnum = 24                                                  
           ivcomp = 0                                                   
           if (a291k(3) == ' ') ivcomp = 1                              
           if (ivcomp - 1) 20240, 10240, 20240                          
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           cvcorr = ' '                                                 
           write (nuvi, 80018) ivtnum, a291k(3), cvcorr                 
 0241      continue                                                     
! T025*  TEST 25                                                         
           ivtnum = 25                                                  
           ivcomp = 0                                                   
           if (a291k(4) == '      T  0.340E+02  0.349E+02') ivcomp = 1  
           if (ivcomp - 1) 20250, 10250, 20250                          
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           cvcorr = '      T  0.340E+02  0.349E+02'                     
           remrks = '64 PERMISSIBLE REPRESENTATIONS'                    
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'SEE NOTES ABOVE'                                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70080) a291k(4), cvcorr                         
70080 format (" ",16x,"COMPUTED: " , a29,/                                      " ",16x,"CORRECT:  " ,a29)                           
 0251      continue                                                     
! T026*  TEST 26                                                         
           ivtnum = 26                                                  
           ivcomp = 0                                                   
           if (a291k(5) == 'THE LAST') ivcomp = 1                       
           if (ivcomp - 1) 20260, 10260, 20260                          
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           cvcorr = 'THE LAST'                                          
           write (nuvi, 80018) ivtnum, a291k(5), cvcorr                 
 0261      continue                                                     
! T027*  TEST 27                      MIXED TYPES WITH SS, SP, NX, AND : 
           ivtnum = 27                                                  
        jvi = 34                                                        
        avs = 34.983                                                    
        bvs = 345.3                                                     
        avd = 95.83d2                                                   
        avb = .false.                                                   
        a8vk = '.FALSE.1'                                               
        write(unit=a97vk,fmt=39321)jvi, avs, avd, avb, a8vk, bvs        
39321 format(s,i2,1x,sp,f7.3,ss,1x,d10.5,l2,1x,a8,1x,e10.5,:,i5,f10.4)
           ivcomp = 0                                                   
           avcorr(1) = '34 +34.983 .95830D+04 F .FALSE.1 .34530E+03'    
           avcorr(2) = '34 +34.983 .95830D+04 F .FALSE.1 .34530+003'    
           avcorr(3) = '34 +34.983 .95830E+04 F .FALSE.1 .34530E+03'    
           avcorr(4) = '34 +34.983 .95830E+04 F .FALSE.1 .34530+003'    
           avcorr(5) = '34 +34.983 .95830+004 F .FALSE.1 .34530E+03'    
           avcorr(6) = '34 +34.983 .95830+004 F .FALSE.1 .34530+003'    
           do i = 1, 6                                            
           if (a97vk == avcorr(i)) ivcomp = 1                           
           if (ivcomp - 1) 40271, 10270, 40271                          
           40271 end do
           goto 20270                                                  
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           cvcorr = '34 +34.983 .95830D+04 F .FALSE.1 .34530E+03'       
           remrks = 'COMPUTED VALUE NOT CONSISTENT'                     
           write (nuvi, 80008) ivtnum, remrks                           
           remrks = 'WITH PERMISSIBLE OPTIONS ABOVE'                    
           write (nuvi, 80050) remrks                                   
           write (nuvi, 70010) (a97e1(i), i = 1,54)                     
           write (nuvi, 70020) (a97e1(i), i= 55,97)                     
           write (nuvi, 70030) (a97e2(i), i = 1,54)                     
           write (nuvi, 70040) (a97e2(i), i= 55,97)                     
 0271      continue                                                     
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
! *****    END OF TEST SEGMENT 393                                       
      stop                                                              
      end program fm909
