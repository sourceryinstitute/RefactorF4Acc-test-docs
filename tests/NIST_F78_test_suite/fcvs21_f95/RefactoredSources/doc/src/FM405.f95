      program fm405
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM405                                                          
! *****                       INTER1 - (390)                             
! *****                                                                  
! ***********************************************************************
! *****  TESTING OF INTERNAL FILES -                           SUBSET REF
! *****          USING READ                                      12.2.5  
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
! *****  S P E C I F I C A T I O N S  SEGMENT 390                        
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
      real :: evs
      integer :: nuvi
      integer :: ivtnum
      integer :: ivi
      integer :: kvi
      integer :: ivcomp
      real :: avs
      real :: bvs
      real :: cvs
      real :: dvs
        logical :: avb
        logical :: bvb
        logical :: cvb
        character(len=1) :: a1vk
        character(len=4) :: a4vk
        character(len=1) :: b1vk
        character(len=4) :: b4vk
        character(len=38) :: a38vk
        character(len=38), dimension(1:4) :: b381k
        character(len=5) :: a5vk
        character(len=8) :: a8vk
        character(len=5) :: b5vk
        character(len=8) :: b8vk
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
! *****                                                                  
           evs = 0.001                                                  
! *****                                                                  
           nuvi = i02                                                   
           ivtotl=15                                                    
           zprog='FM405'                                                
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
        a38vk = '2.1 TEST 3 23.45E2 .TRUE.  F          '                
        b381k(1) = '   23   23.345     T ENDS             '             
        b381k(2) = ' 23.456     F    98 YOURS PROGRAMS    '             
        b381k(3) = ' 13.1234  13.1234E0 1312.34           '             
        b381k(4) = '   5.2345   56    5.2345 T TRUE 5.2345'             
! *****                                                                  
! *****    HEADER FOR SEGMENT 390                                        
! *****                                                                  
           write(nuvi,39000)                                            
39000 format(/2x," INTER1 - (390) INTERNAL FILES -- USING READ"                    //" SUBSET REF. - 12.2.5" )                          
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! ******                                                                 
! *************************************************************          
! T001*  TEST 1                    CHARACTER VARIABLE, INTEGER           
           ivtnum=1                                                     
        read(a38vk,39001) ivi                                           
39001 format(8x,i2)                                                   
        kvi = 3                                                         
           ivcomp=0                                                     
           if (ivi  ==  kvi) ivcomp=1                                   
           if (ivcomp-1) 20010,10010,20010                              
10010 ivpass=ivpass + 1                                            
           write (nuvi,80002) ivtnum                                    
           goto 0011                                                   
20010 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80024) ivi                                       
           write (nuvi,80026) kvi                                       
 0011      continue                                                     
! *****                                                                  
! T002*  TEST 2                              REAL, FW.D                  
           ivtnum=2                                                     
        read(a38vk,39004) avs                                           
39004 format(f3.1)                                                    
        bvs = 2.1                                                       
           ivcomp=0                                                     
           if (avs  <  bvs + evs .and. avs  >  bvs - evs) ivcomp=1    
           if (ivcomp-1) 20020,10020,20020                              
10020 ivpass=ivpass + 1                                            
           write(nuvi,80002)ivtnum                                      
           goto 0021                                                   
20020 ivfail=ivfail+1                                              
           write(nuvi,80008) ivtnum                                     
           write (nuvi,80028) avs                                       
           write (nuvi,80030) bvs                                       
 0021      continue                                                     
! T003*  TEST 3                               REAL, EW.D                 
           ivtnum=3                                                     
        read(a38vk,39006) avs                                           
39006 format(11x,e7.2)                                                
        bvs = 23.45e2                                                   
           ivcomp=0                                                     
           if (avs  <  bvs + evs .and. avs  >  bvs - evs) ivcomp=1    
           if (ivcomp-1) 20030,10030,20030                              
10030 ivpass=ivpass + 1                                            
           write(nuvi,80002)ivtnum                                      
           goto 0031                                                   
20030 ivfail=ivfail + 1                                            
           write(nuvi,80008)ivtnum                                      
           write (nuvi,80028) avs                                       
           write (nuvi,80030) bvs                                       
 0031      continue                                                     
! T004*  TEST 4                          SAME REAL, EW.DEN               
           ivtnum=4                                                     
           ivcomp=0                                                     
        read(a38vk,39008) cvs                                           
39008 format(10x,e8.2e2)                                              
           if (cvs  <  bvs + evs .and. cvs  >  bvs - evs) ivcomp=1    
           if (ivcomp-1) 20040,10040,20040                              
10040 ivpass=ivpass+1                                              
           write(nuvi,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail=ivfail + 1                                            
           write(nuvi,80008)ivtnum                                      
           write (nuvi,80028) cvs                                       
           write (nuvi,80030) bvs                                       
 0041      continue                                                     
! T005*  TEST 5                          LOGICAL, WITH PERIODS           
           ivtnum=5                                                     
        read(a38vk,39010) avb                                           
39010 format(19x,l6)                                                  
           ivcomp=0                                                     
           if (avb) ivcomp=1                                            
           if (ivcomp-1) 20050,10050,20050                              
10050 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0051                                                   
20050 ivfail=ivfail + 1                                            
           write (nuvi,80008) ivtnum                                    
70050 format (" ",16x,"COMPUTED: " ,l1,                                 /17x,"CORRECT:  " ,"T")                                      
           write (nuvi,70050) avb                                       
 0051      continue                                                     
! T006*  TEST 6                         LOGICAL, WITHOUT PERIODS         
           ivtnum=6                                                     
        read(a38vk,39012) cvb                                           
39012 format(25x,l3)                                                  
           ivcomp=0                                                     
           if (.not. cvb) ivcomp=1                                      
           if (ivcomp-1) 20060,10060,20060                              
10060 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0061                                                   
20060 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70060 format (" ",16x,"COMPUTED: " ,l1)                            
           write (nuvi,70060) cvb                                       
70061 format (" ",16x,"CORRECT:  " ,"F")                           
           write (nuvi,70061)                                           
 0061      continue                                                     
! T007*  TEST 7                                  CHARACTER, A            
           ivtnum=7                                                     
        read(a38vk,39014) a1vk                                          
39014 format(9x,a1)                                                   
        b1vk = '3'                                                      
           ivcomp=0                                                     
           if (a1vk  ==  b1vk) ivcomp=1                                 
           if (ivcomp-1) 20070,10070,20070                              
10070 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0071                                                   
20070 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a1vk                                      
           write (nuvi,80022) b1vk                                      
 0071      continue                                                     
! T008*  TEST 8                                  CHARACTER, AW           
           ivtnum=8                                                     
        read(a38vk,39016) a4vk                                          
39016 format(4x,a4)                                                   
        b4vk = 'TEST'                                                   
           ivcomp=0                                                     
           if (a4vk  ==  b4vk) ivcomp=1                                 
           if (ivcomp-1) 20080,10080,20080                              
10080 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0081                                                   
20080 ivfail=ivfail + 1                                            
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a4vk                                      
           write (nuvi,80022) b4vk                                      
 0081      continue                                                     
! T009*  TEST 9                          CHARACTER, EXTRA BLANKS         
           ivtnum = 9                                                   
        read(a38vk,39018) a4vk                                          
39018 format(11x,a7)                                                  
           b4vk = '45E2'                                                
           ivcomp=0                                                     
           if (a4vk  ==  b4vk) ivcomp=1                                 
           if (ivcomp-1) 20090,10090,20090                              
10090 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0091                                                   
20090 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a4vk                                      
           write (nuvi,80022) b4vk                                      
 0091      continue                                                     
! T010*  TEST 10                         CHARACTER, NO PADDING           
           ivtnum = 10                                                  
        read(a38vk,39020) a4vk                                          
39020 format(a3)                                                      
           ivcomp=0                                                     
           b4vk = '2.1 '                                                
           if (a4vk  ==  b4vk) ivcomp=1                                 
           if (ivcomp-1) 20100,10100,20100                              
10100 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0101                                                   
20100 ivfail=ivfail + 1                                            
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a4vk                                      
           write (nuvi,80022) b4vk                                      
 0101      continue                                                     
! T011*  TEST 11             CHECK TO SEE IF SECOND VARIABLE             
! *****                          START READING JUST AFTER FIRST VARIABLE 
           ivtnum = 11                                                  
        read(a38vk,39022) a4vk, a1vk                                    
39022 format(1x,a,a)                                                  
        b4vk = '.1 T'                                                   
        b1vk = 'E'                                                      
           ivcomp=0                                                     
           if (a4vk  ==  b4vk .and. a1vk  ==  b1vk) ivcomp=1            
           if (ivcomp-1) 20110,10110,20110                              
10110 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0111                                                   
20110 ivfail=ivfail + 1                                            
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a4vk,a1vk                                 
           write (nuvi,80022) b4vk,b1vk                                 
0111  continue                                                     
! T012*  TEST 12                      MIXED TYPES, ARRAY ELEMENT         
           ivtnum = 12                                                  
        read(b381k(1),39024) ivi, avs, avb, a4vk                        
39024 format(i5,1x,f8.3,1x,l5,1x,a4)                                  
        kvi = 23                                                        
        bvs = 23.345                                                    
        b4vk = 'ENDS'                                                   
           if (ivi  ==  kvi .and.                                            avs  <  bvs + evs .and. avs  >  bvs - evs .and.                 avb .and.                                                         a4vk  ==  b4vk) goto 39026                                   
           ivfail=ivfail + 1                                            
70120 format (" ",2x,i3,4x," FAIL  ","MIXED DATA TYPES" ,16x,           "COMPLEX IF - SEE SOURCE CODE" )                             
           write(nuvi,70120)ivtnum                                      
70121 format (" ",16x,"COMPUTED: " ,i5,2x,f10.5,2x,l1,2x,a4)       
           write (nuvi,70121) ivi,avs,avb,a4vk                          
70122 format (" ",16x,"CORRECT:  " ,                                    "   23",2x,"  23.34500" ,2x,"T",2x,"ENDS")                   
           write (nuvi,70122)                                           
           goto 39027                                                   
39026 ivpass=ivpass+1                                              
           write(nuvi,80002) ivtnum                                     
39027 continue                                                     
! T013*  TEST 13                     MIXED TYPES, ARRAY ELEMENT          
! *****                             WITH RUN TIME EXPRESSION AS SUBSCRIPT
           ivtnum = 13                                                  
        kvi = 1                                                         
        read(b381k(kvi*2),39028) avs, avb, ivi, a5vk, a8vk              
39028 format(f7.3,1x,l5,1x,i5,1x,a5,1x,a8)                            
        bvs = 23.456                                                    
        kvi = 98                                                        
        b5vk = 'YOURS'                                                  
        b8vk = 'PROGRAMS'                                               
           if (avs  <  bvs + evs .and. avs  >  bvs - evs .and.             .not. avb .and.                                                   ivi  ==  kvi .and.                                                a5vk  ==  b5vk .and.                                              a8vk  ==  b8vk) goto 39030                                   
           ivfail=ivfail+1                                              
70130 format (" ",2x,i3,4x," FAIL  ","MIXED DATA TYPES" ,16x,           "COMPLEX IF - SEE SOURCE CODE" )                             
           write (nuvi,70130) ivtnum                                    
70131 format (" ",16x,"COMPUTED: " ,                                    f7.3,2x,l1,2x,i5,2x,a5,2x,a8)                                
           write (nuvi,70131) avs,avb,ivi,a5vk,a8vk                     
70132 format (" ",16x,"CORRECT:  " ,                                    " 23.456",2x,"F",2x,"   98",2x,"YOURS",2x,"PROGRAMS")        
           write (nuvi,70132)                                           
           goto 39031                                                   
39030 ivpass=ivpass + 1                                            
           write(nuvi,80002) ivtnum                                     
39031 continue                                                     
! T014*  TEST 14                 MIXED TYPES, ALSO BN AND BZ             
! *****                                                                  
           ivtnum = 14                                                  
        read(b381k(4),39032) avs, ivi, bvs, avb, a4vk, cvs              
39032 format(f9.4,1x,i4,1x,bn,f9.4,1x,l1,1x,a4,1x,bz,f6.4)            
        dvs = 5.2345                                                    
        kvi = 56                                                        
        bvb = .true.                                                    
        b4vk = 'TRUE'                                                   
           if (avs  <  dvs + evs .and. avs  >  dvs - evs .and.             ivi  ==  kvi .and.                                                bvs  <  dvs + evs .and. bvs  >  dvs - evs .and.                 avb .and.                                                         a4vk  ==  b4vk .and.                                              cvs  <  dvs + evs .and. cvs  >  dvs - evs) goto 39034      
           ivfail=ivfail + 1                                            
70140 format (" ",2x,i3,4x," FAIL  ","MIXED DATA TYPES" ,16x,           "COMPLEX IF - SEE SOURCE CODE" )                             
           write(nuvi,70140) ivtnum                                     
70141 format (" ",16x,"COMPUTED: " ,                                    f9.4,2x,i4,2x,f9.4,2x,l1,2x,a4,2x,f9.4)                      
           write (nuvi,70141) avs,ivi,bvs,avb,a4vk,cvs                  
70142 format (" ",16x,"CORRECT:  " ,                                    "   5.2345",2x,"  56",2x,"   5.2345",2x,"T",2x,"TRUE",            2x,"   5.2345")                                              
           write (nuvi,70142)                                           
           goto 39035                                                   
39034 ivpass=ivpass+1                                              
           write(nuvi,80002) ivtnum                                     
39035 continue                                                     
! T015*  TEST 15             REAL VARIABLES WITH SCALING FACTOR          
           ivtnum = 15                                                  
        read(b381k(3),39036) avs, bvs, cvs                              
39036 format(f9.5, 1x, e9.3, 1x, 2pf7.4)                              
        dvs = 13.1234                                                   
           if (avs  <  dvs + evs .and. avs  >  dvs - evs .and.             bvs  <  dvs + evs .and. bvs  >  dvs - evs .and.                 cvs  <  dvs + evs .and. cvs  >  dvs - evs) goto 39038      
           ivfail=ivfail + 1                                            
70150 format (" ",2x,i3,4x," FAIL  ","REAL  DATA TYPES" ,16x,           "COMPLEX IF - SEE SOURCE CODE" )                             
           write(nuvi,70150) ivtnum                                     
70151 format (" ",16x,"COMPUTED: " ,f9.4,2x,f9.3,2x,f7.4)          
           write (nuvi,70151) avs,bvs,cvs                               
70152 format (" ",16x,"CORRECT:  " ,                                    "  13.1234",2x,"   13.123",2x,"13.1234")                     
           write (nuvi,70152)                                           
           goto 39039                                                   
39038 ivpass=ivpass+1                                              
           write(nuvi,80002) ivtnum                                     
39039 continue                                                     
! *****                                                                  
! *****    END OF TEST SEGMENT 390                                       
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
      stop                                                              
      end program fm405
