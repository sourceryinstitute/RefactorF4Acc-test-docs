      program fm923
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM923                                                          
! *****                       LSTDI1 - (370)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST LIST DIRECTED INPUT ON                           13.6    
! *****    INTEGER REAL, LOGICAL, AND CHARACTER DATA TYPES.      12.4    
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
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 34 CARD IMAGES IN COL. 1-80   
! OL.      1----------------------------------------------------------61 
! ARD 1    25                                                            
! ARD 2    10.75                                                         
! ARD 3    12.875E01                                                     
! ARD 4    T                                                             
! ARD 5    'ABCDEF'                                                      
! ARD 6    10 15 22 40                                                   
! ARD 7    100.5 0.25E-1 -1.625E2                                        
! ARD 8    T F F T F                                                     
! ARD 9    'AB' 'ABCD' 'ABCDEF'                                          
! ARD 10   '123456' T 17.5 -11 2.5E0                                     
! ARD 11   -5,'2468',T,15.0                                              
! ARD 12   F    'CHAR' -1                0.25                            
! ARD 13   5 10 15                                                       
! ARD 14   -1.25E1  F  T  -6   '-6'                                      
! ARD 15   F 'ZYXW' 'DCBA'  15.5                                         
! ARD 16   'ONE ',,3,F                                                   
! ARD 17   'TWO ', 2, , 2.0                                              
! ARD 18   ,4, 1*, 8, ,, 14                                              
! ARD 19   5, -0.25E1, 4*, 'TEST', F                                     
! ARD 20   1 2 3 4 5                                                     
! ARD 21   6 7 8/ 9 10                                                   
! ARD 22   12045,12 45                                                   
! ARD 23   12045                                                         
! OL.    62---------------80                                             
! ARD 23                  12                                             
! OL.      1----------------------------------------------------------61 
! ARD 24   45                                                            
! ARD 25   'CAN''T, AND/OR   WON''T'                                     
! ARD 26   '1234567890' '12345678' '1234567890123'                       
! ARD 27   TRUCK .FOUR .FALSE. .TWIN. F12. F7.2 .TRUE. .T=3+4            
! ARD 28   T T T T T                                                     
! ARD 29   F F/F F F                                                     
! ARD 30   / 10 20 30                                                    
! ARD 31   1 2 3 4                                                       
! ARD 32   5 6 7 8                                                       
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 370                        
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: irvi
      integer :: nuvi
      real :: dvs
      integer :: ivcorr
      real :: rvcorr
      integer :: ivtnum
      integer :: ivi
      real :: avs
      real :: aavs
      integer :: ivcomp
      integer :: jvi
      integer :: kvi
      integer :: lvi
      real :: bvs
      real :: cvs
      real :: bbvs
      real :: ccvs
      real :: go
      real :: to
      integer :: iivi
      integer :: kkvi
      integer :: jjvi
      integer :: mvi
        integer, dimension(1:3) :: j1i
        logical :: avb
        logical :: bvb
        logical :: cvb
        logical :: dvb
        logical :: evb
        logical :: fvb
        logical :: gvb
        logical :: hvb
        character(len=2) :: a2vk
        character(len=4) :: a4vk
        character(len=4) :: b4vk
        character(len=6) :: a6vk
        character(len=6) :: b6vk
        character(len=8) :: a8vk
        character(len=9) :: a9vk
        character(len=15) :: a15vk
        character(len=21) :: a21vk
        character(len=6) :: cvnx06
        character(len=6) :: cvny06
        character(len=21) :: cvnx21
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
      irvi = i01                                                        
      nuvi = i02                                                        
      zprog = 'FM923'                                                   
      ivtotl = 27                                                       
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
! *****  REAL NUMBER APPROXIMATION CRITERIA                              
        dvs = 0.0001                                                    
        ivcorr=0                                                        
        rvcorr=0                                                        
! *****                                                                  
! *****  HEADING FOR SEGMENT 370                                         
        write(nuvi,37000)                                               
37000 format(/2x, " LSTDI1 - (370) " ,                                         " LIST DIRECTED INPUT FOR SUBSET DATA TYPES" //                3x," ANS REF. - 13.6  12.4" )                               
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
! T001*  TEST 1 - CARD 1    INTEGER                                      
           ivtnum = 1                                                   
        read(irvi, *) ivi                                               
! *****     TO DELETE TEST THE READ STATEMENTS MUST BE PERFORMED         
! *****     FIRST. THEN INCLUDE THE FOLLOWING 2 STATEMENTS               
! *****     IVDELE=IVDELE+1                                              
! *****     WRITE (NUVI,80000) IVTNUM                                    
! *****     AND COMMENT OUT REMAINING LINES UNTIL NEXT TEST              
           ivcorr=25                                                    
           if (ivi - 25) 20010,10010,20010                              
10010 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0011                                                   
20010 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80024) ivi                                       
           write (nuvi,80026) ivcorr                                    
 0011      continue                                                     
! T002*  TEST 2 - CARD 2    REAL                                         
           ivtnum = 2                                                   
        read(irvi, *) avs                                               
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           rvcorr=10.75                                                 
        aavs = avs - 10.75                                              
           if (aavs + .00005) 20020,10020,40020                         
40020 if (aavs - .00005) 10020,10020,20020                         
10020 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0021                                                   
20020 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80028) avs                                       
           write (nuvi,80030) rvcorr                                    
 0021      continue                                                     
! T003*  TEST 3 - CARD 3    REAL, EXPONENT                               
           ivtnum = 3                                                   
        read(irvi, *) avs                                               
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           rvcorr=128.75                                                
        aavs = avs - 128.75                                             
           if (aavs + .00005) 20030,10030,40030                         
40030 if (aavs - .00005) 10030,10030,20030                         
10030 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0031                                                   
20030 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80028) avs                                       
           write (nuvi,80030) rvcorr                                    
 0031      continue                                                     
! T004*  TEST 4 - CARD 4    LOGICAL                                      
           ivtnum = 4                                                   
        read(irvi, *) avb                                               
! *****     TO DELETE TEST SEE NOTES TEST 1                              
           ivcomp=0                                                     
           if (avb) ivcomp = 1                                          
           if (ivcomp - 1) 20040,10040,20040                            
10040 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0041                                                   
20040 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70040 format (" ",16x,"COMPUTED: " ,1x,l1)                         
           write (nuvi,70040) avb                                       
70041 format (" ",16x,"CORRECT:  " ," T")                          
           write (nuvi,70041)                                           
 0041      continue                                                     
! T005*  TEST 5 - CARD 5    CHARACTER                                    
           ivtnum = 5                                                   
        read(irvi, *) a6vk                                              
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           cvnx06='ABCDEF'                                              
           ivcomp=0                                                     
           if (a6vk  ==  'ABCDEF') ivcomp = 1                           
           if (ivcomp - 1) 20050,10050,20050                            
10050 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0051                                                   
20050 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a6vk                                      
           write (nuvi,80022) cvnx06                                    
 0051      continue                                                     
! T006*  TEST 6 - CARD 6    SEVERAL INTEGER                              
           ivtnum = 6                                                   
        read(irvi, *) ivi, jvi, kvi, lvi                                
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi - 10) 20060,40060,20060                              
40060 if (jvi - 15) 20060,40061,20060                              
40061 if (kvi - 22) 20060,40062,20060                              
40062 if (lvi - 40) 20060,10060,20060                              
10060 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0061                                                   
20060 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70060 format (" ",16x,"COMPUTED: " ,i5,3(2x,i5))                   
           write (nuvi,70060) ivi,jvi,kvi,lvi                           
70061 format (" ",16x,"CORRECT:  " ,                                      "   10",2x,"   15",2x,"   22",2x,"   40")                  
           write (nuvi,70061)                                           
 0061      continue                                                     
! T007*  TEST 7 - CARD 7    SEVERAL REAL                                 
           ivtnum = 7                                                   
        read(irvi, *) avs, bvs, cvs                                     
! *******   TO DELETE TEST SEE NOTES FOR TEST 1                          
        aavs = avs - 100.5                                              
        bbvs = bvs - 0.025                                              
        ccvs = cvs - (-162.5)                                           
           if (aavs + .00005) 20070,40071,40070                         
40070 if (aavs - .00005) 40071,40071,20070                         
40071 if (bbvs + .00005) 20070,40073,40072                         
40072 if (bbvs - .00005) 40073,40073,20070                         
40073 if (ccvs + .00005) 20070,10070,40074                         
40074 if (ccvs - .00005) 10070,10070,20070                         
10070 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0071                                                   
20070 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70071 format (" ",16x,"COMPUTED: " ,f6.2,2x,f6.4,2x,f7.2)          
           write (nuvi,70071) avs,bvs,cvs                               
70072 format (" ",16x,"CORRECT:  " ,"100.50",2x,"0.0250",               2x,"-162.50")                                                
           write(nuvi,70072)                                            
 0071      continue                                                     
! T008*  TEST 8 - CARD 8    SEVERAL LOGICAL                              
           ivtnum = 8                                                   
        read(irvi, *) avb, bvb, cvb, dvb, evb                           
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (avb .and. .not. bvb .and. .not. cvb .and. dvb .and.           .not. evb) goto 37008                                       
           ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70081 format (" ",16x,"COMPUTED: " ,l1,4(2x,l1))                   
           write (nuvi,70081) avb,bvb,cvb,dvb,evb                       
70082 format (" ",16x,"CORRECT:  " ,"T",2x,"F",2x,"F",2x,"T",2x,          "F")                                                       
           write (nuvi,70082)                                           
           goto 37010                                                  
37008 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37010 continue                                                     
! T009*  TEST 9 - CARD 9    SEVERAL CHARACTER STRINGS                    
           ivtnum = 9                                                   
        read(irvi, *) a2vk, a4vk, a6vk                                  
! *****     TO DELETE CODE SEE NOTES FOR TEST 1                          
           if (a2vk  ==  'AB' .and. a4vk  ==  'ABCD' .and.                   a6vk  ==  'ABCDEF') goto 37011                              
           ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70090 format (" ",16x,"COMPUTED: " ,a2,2x,a4,2x,a6)                
           write (nuvi,70090) a2vk,a4vk,a6vk                            
70091 format (" ",16x,"CORRECT:  " ,"AB",2x,"ABCD",2x,"ABCDEF")    
           write (nuvi,70091)                                           
           goto 37013                                                  
37011 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37013 continue                                                     
! T010*  TEST 10 - CARD 10    MIXED TYPES                                
           ivtnum = 10                                                  
        read(irvi, *) a6vk, avb, avs, ivi, bvs                          
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (a6vk  ==  '123456' .and. avb .and. avs  >=  (17.5 - dvs)      .and. avs  <=  (17.5 + dvs) .and. ivi  ==  -11 .and.              bvs  >=  (2.5 - dvs) .and. bvs  <=  (2.5 + dvs))                  goto 37014                                                  
           ivfail=ivfail+1                                              
70100 format (" ",2x,i3,4x," FAIL  ",32x,                               "COMPLEX IF - SEE SOURCE CODE" )                             
           write (nuvi,70100) ivtnum                                    
70101 format (" ",16x,"COMPUTED: " ,                                    a6,2x,l1,2x,f5.2,2x,i5,2x,e12.5)                             
           write (nuvi,70101) a6vk,avb,avs,ivi,bvs                      
70102 format (" ",16x,"CORRECT:  " ,                                    "123456  T  17.50    -11" ,2x," 0.25000E+01 OR .25000+001" ) 
           write (nuvi,70102)                                           
           goto 37016                                                  
37014 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37016 continue                                                     
! T011*  TEST 11 - CARD 11    MIXED TYPES SEPARATED BY COMMAS            
           ivtnum = 11                                                  
        read(irvi, *) ivi, a4vk, avb, avs                               
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi  ==  -5 .and. a4vk  ==  '2468' .and. avb .and.            avs  >=  (15.0 - dvs) .and. avs  <=  (15.0 + dvs))                goto 37017                                                  
           ivfail=ivfail+1                                              
70110 format (" ",2x,i3,4x," FAIL  ","MIXED DATA TYPES" ,16x,           "COMPLEX IF - SEE SOURCE CODE" )                             
           write (nuvi,70110) ivtnum                                    
70111 format (" ",16x,"COMPUTED: " ,                                    i5,2x,a4,2x,l1,2x,f5.2)                                      
           write (nuvi,70111) ivi,a4vk,avb,avs                          
70112 format (" ",16x,"CORRECT:  " ,                                    "   -5",2x,"2468",2x,"T",2x,"15.00")                         
           write (nuvi,70112)                                           
           goto 37019                                                  
37017 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37019 continue                                                     
! T012*  TEST 12 - CARD 12    MIXED TYPES, VARYING NUMBER OF             
! *****                               BLANKS SEPARATING VALUES           
            ivtnum = 12                                                 
        read(irvi, *) avb, a4vk, ivi, avs                               
! *****      TO DELETE TEST SEE NOTES FOR TEST 1                         
            if (.not. avb .and. a4vk  ==  'CHAR' .and. ivi  ==  -1 .and.      avs  >=  (0.25 - dvs) .and. avs  <=  (0.25 + dvs))                goto 37020                                                 
            ivfail=ivfail+1                                             
70120 format (" ",2x,i3,4x," FAIL  ","MIXED DATA TYPES" ,16x,             "COMPLEX IF - SEE SOURCE CODE" )                          
            write (nuvi,70120) ivtnum                                   
70121 format (" ",16x,"COMPUTED: " ,                                    l1,2x,a4,2x,i5,2x,f4.2)                                     
            write (nuvi,70121) avb,a4vk,ivi,avs                         
70122 format (" ",16x,"CORRECT:  " ,                                    "F  ","CHAR",2x,"   -5",2x,"0.25")                          
            write (nuvi,70122)                                          
            goto 37022                                                 
37020 continue                                                    
            ivpass=ivpass+1                                             
            write (nuvi,80002) ivtnum                                   
37022 continue                                                    
! T013*  TEST 13 - CARD 13    READ VALUES INTO ARRAY BY USING            
! *****                       AN IMPLICIT DO-LOOP                        
            ivtnum = 13                                                 
        read(irvi, *) (j1i(iivi), iivi=1,3)                             
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (j1i(1) - 5) 20130,40130,20130                            
40130 if (j1i(2) - 10) 20130,40131,20130                           
40131 if (j1i(3) - 15) 20130,10130,20130                           
10130 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0131                                                   
20130 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70130 format (" ",16x,"COMPUTED: " ,i5,2x,i5,2x,i5)                
           write (nuvi,70130) j1i(1),j1i(2),j1i(3)                      
70131 format (" ",16x,"CORRECT:  " ,                                      "    5",2x,"   10",2x,"   15")                             
           write (nuvi,70131)                                           
 0131      continue                                                     
! T014*  TEST 14 - CARDS 14-15    LIST EXTENDING OVER 2 RECORDS          
           ivtnum = 14                                                  
        read(irvi, *) avs, avb, bvb, ivi, a2vk, cvb, a4vk, b4vk, bvs    
! *****     TO DELETE CODE SEE NOTES FOR TEST 1                          
           if (avs  >=  (-1.25e1 - dvs) .and. avs  <=  (-1.25e1 + dvs)       .and. .not. avb .and. bvb .and.                                   ivi  ==  -6 .and. a2vk  ==  '-6' .and. .not. cvb .and.            a4vk  ==  'ZYXW' .and. b4vk  ==  'DCBA' .and.                     bvs  >=  (15.5 - dvs) .and. bvs  <=  (15.5 + dvs))                goto 37024                                                  
           ivfail=ivfail+1                                              
70140 format (" ",2x,i3,4x," FAIL  ","MIXED DATA TYPES" ,16x,           "COMPLEX IF - SEE SOURCE CODE" )                             
           write (nuvi,70140) ivtnum                                    
70141 format (" ",16x,"COMPUTED: " ,e12.5,2x,l1,2x,l1,2x,i5,            /27x,a2,2x,l1,2x,a4,2x,a4,2x,f5.2)                           
           write (nuvi,70141) avs,avb,bvb,ivi,a2vk,cvb,a4vk,b4vk,bvs    
70142 format (" ",16x,"CORRECT:  " ,                                    " -.12500E+01" ,2x,"F",2x,"T",2x,"   -6",                         /27x,"-6",2x,"F",2x,"ZYXW",2x,"DCBA",2x,"15.50")             
           write (nuvi,70142)                                           
           goto 37026                                                  
37024 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37026 continue                                                     
! T015*  TEST 15 - CARD 16    NULL VALUE REPRESENTED AS ,,               
           ivtnum = 15                                                  
        avs = 2.0                                                       
        read(irvi, *) a4vk, avs, ivi, avb                               
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (a4vk  ==  'ONE ' .and. avs  >=  (2.0 - dvs) .and.             avs  <=  (2.0 + dvs) .and. ivi  ==  3 .and. .not. avb)            goto 37027                                                  
           ivfail=ivfail+1                                              
           write (nuvi,70150) ivtnum                                    
70150 format (" ",2x,i3,4x," FAIL  ",32x,                               "COMPLEX IF - SEE SOURCE CODE" )                             
70151 format (" ",16x,"COMPUTED: " ,a4,2x,f4.1,2x,i5,2x,l1)        
           write (nuvi,70151) a4vk,avs,ivi,avb                          
70152 format (" ",16x,"CORRECT:  " ,                                    "ONE    2.0      3  F" )                                     
           write (nuvi,70152)                                           
           goto 37029                                                  
37027 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37029 continue                                                     
! T016*  TEST 16 - CARD 17    NULL VALUE REPRESENTED AS ' '              
           ivtnum = 16                                                  
        avb = .true.                                                    
        read(irvi, *) a4vk, ivi, avb, avs                               
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (a4vk  ==  'TWO ' .and. ivi  ==  2 .and. avb .and.             avs  >=  (2.0 - dvs) .and. avs  <=  (2.0 + dvs))                  goto 37030                                                  
           ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70160 format (" ",16x,"COMPUTED: " ,a4,2x,i5,2x,l1,2x,f4.1)        
           write (nuvi,70160) a4vk,ivi,avb,avs                          
70161 format (" ",16x,"CORRECT:  " ,                                    "TWO       2  T   2.0" )                                     
           write (nuvi,70161)                                           
           goto 37032                                                  
37030 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37032 continue                                                     
! T017*  TEST 17 - CARD 18    VARIOUS NULL REPRESENTATIONS               
           ivtnum = 17                                                  
        ivi = 2                                                         
        jvi = 6                                                         
        kvi = 10                                                        
        kkvi = 12                                                       
        read(irvi, *) ivi, iivi, jvi, jjvi, kvi, kkvi, lvi              
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi  ==  2 .and. iivi  ==  4 .and. jvi  ==  6 .and.           jjvi  ==  8 .and. kvi  ==  10 .and. kkvi  ==  12 .and.            lvi  ==  14) goto 37033                                     
           ivfail=ivfail+1                                              
           write (nuvi,70170) ivtnum                                    
70170 format (" ",2x,i3,4x," FAIL  ",32x,                               "COMPLEX IF - SEE SOURCE CODE" )                             
70171 format (" ",16x,"COMPUTED: " ,                                    i5,6(2x,i5))                                                 
           write (nuvi,70171) ivi,iivi,jvi,jjvi,kvi,kkvi,lvi            
70172 format (" ",16x,"CORRECT:  " ,                                    "    2      4      6      8     10     12     14" )          
           write (nuvi,70172)                                           
           goto 37035                                                  
37033 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37035 continue                                                     
! T018*  TEST 18 - CARD 19    NULL VALUE USING REPETITION FACTOR         
           ivtnum = 18                                                  
        ivi = 1                                                         
        avb = .true.                                                    
        avs = 1.0                                                       
        a4vk = 'TRUE'                                                   
        read (irvi, *) jvi, bvs, ivi, avb, avs, a4vk, b4vk, bvb         
! *****     TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (jvi  ==  5 .and. bvs  >=  (-2.5 - dvs) .and.                  bvs  <=  (-2.5 + dvs) .and. ivi  ==  1 .and. avb .and.            avs  >=  (1.0 - dvs) .and.  avs  <=  (1.0 + dvs) .and.            a4vk  ==  'TRUE' .and. b4vk  ==  'TEST' .and. .not. bvb)          goto 37036                                                  
           ivfail=ivfail+1                                              
           write (nuvi,70180) ivtnum                                    
70180 format (" ",2x,i3,4x," FAIL  ",32x,                               "COMPLEX IF - SEE SOURCE CODE" )                             
70181 format (" ",16x,"COMPUTED: " ,                                    i5,2x,f4.1,2x,i5,2x,l1,2x,f4.1,2x,a4,2x,                          a4,2x,l1)                                                    
           write (nuvi,70181) jvi,bvs,ivi,avb,avs,a4vk,b4vk,bvb         
70182 format (" ",16x,"CORRECT:  " ,                                    "    5",2x,"-2.5",2x,"    1",2x,"T",2x," 1.0",2x,"TRUE",2x,       "TEST",2x,"F")                                               
           write (nuvi,70182)                                           
           goto 37038                                                  
37036 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37038 continue                                                     
! T019*  TEST 19 - CARDS 20-21    TERMINATOR SLASH (/)                   
           ivtnum = 19                                                  
        read(irvi, *) ivi, jvi, kvi, lvi, mvi                           
        read(irvi, *) ivi, jvi, kvi, lvi, mvi                           
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi - 6) 20190,40190,20190                               
40190 if (jvi - 7) 20190,40191,20190                               
40191 if (kvi - 8) 20190,40192,20190                               
40192 if (lvi - 4) 20190,40193,20190                               
40193 if (mvi - 5) 20190,10190,20190                               
10190 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0191                                                   
20190 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70190 format (" ",16x,"COMPUTED: " ,i5,4(2x,i5))                   
           write (nuvi,70190) ivi,jvi,kvi,lvi,mvi                       
70191 format (" ",16x,"CORRECT:  " ,                                    "    6",2x,"    7",2x,"    8",2x,"    4",2x,"    5")         
           write (nuvi,70191)                                           
 0191      continue                                                     
! T020*  TEST 20 - CARD 22    VERIFY THAT BLANKS ARE NOT                 
! *****                       INTERPRETED AS ZEROS                       
           ivtnum = 20                                                  
        read(irvi, *) ivi, jvi, kvi                                     
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi - 12045) 20200,40200,20200                           
40200 if (jvi - 12) 20200,40201,20200                              
40201 if (kvi - 45) 20200,10200,20200                              
10200 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0201                                                   
20200 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70200 format (" ",16x,"COMPUTED: " ,i5,2x,i5,2x,i5)                
           write (nuvi,70200) ivi,jvi,kvi                               
70201 format (" ",16x,"CORRECT:  " ,                                    "12045",2x,"   12",2x,"   45")                               
           write (nuvi,70201)                                           
 0201      continue                                                     
! T021*  TEST 21 - CARDS 23-24    VERIFY THAT END-OF-RECORD IS           
! *****                                   TREATED AS A BLANK WHEN IT     
! *****                                   SEPARATES TWO INTEGERS         
           ivtnum = 21                                                  
        read(irvi, *) ivi, jvi, kvi                                     
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi - 12045) 20210,40210,20210                           
40210 if (jvi - 12) 20210,40211,20210                              
40211 if (kvi - 45) 20210,10210,20210                              
10210 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0211                                                   
20210 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70210 format (" ",16x,"COMPUTED: " ,i5,2x,i5,2x,i5)                
           write (nuvi,70210) ivi,jvi,kvi                               
70211 format (" ",16x,"CORRECT:  " ,                                    "12045",2x,"   12",2x,"   45")                               
           write (nuvi,70211)                                           
 0211      continue                                                     
! T023*  TEST 22 - CARD 25    QUOTES, BLANKS, COMMAS AND SLASHES         
! *****                       EMBEDDED IN CHARACTER STRINGS              
           ivtnum = 22                                                  
        read(irvi, *) a21vk                                             
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           cvnx21='CAN''T, AND/OR   WON''T'                             
           if (a21vk  ==  'CAN''T, AND/OR   WON''T') goto 37044        
           ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write (nuvi,80020) a21vk                                     
           write (nuvi,80022) cvnx21                                    
           goto 0231                                                   
37044 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
 0231      continue                                                     
! T024*  TEST 23 - CARD 26    CHARACTER STRINGS THAT ARE READ IN         
! *****                       VARIABLES OF DIFFERENT LENGTHS             
           ivtnum = 23                                                  
        read(irvi, *) a15vk, a8vk, a9vk                                 
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (a15vk  ==  '1234567890     ' .and.                            a8vk  ==  '12345678' .and.                                        a9vk  ==  '123456789') goto 37047                           
           ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70240 format (" ",16x,"COMPUTED: " ,a15,2x,a8,2x,a9)               
           write (nuvi,70240) a15vk,a8vk,a9vk                           
70241 format (" ",16x,"CORRECT:  " ,                                    "1234567890     " ,2x,"12345678",2x,"123456789")             
           write (nuvi,70241)                                           
           goto 37049                                                  
37047 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37049 continue                                                     
! T025*  TEST 24 - CARD 27    LOGICAL VALUES IN DIFFERENT                
! *****                               REPRESENTATIONS                    
           ivtnum = 24                                                  
        read(irvi, *) avb, bvb, cvb, dvb, evb, fvb, gvb, hvb            
! *****     TO DELETE CODE SEE NOTES FOR TEST 1                          
           if (avb .and. .not. bvb .and. .not. cvb .and. dvb .and.           .not. evb .and. .not. evb .and. gvb .and. hvb)                     goto 37050                                                 
           ivfail=ivfail+1                                              
           write (nuvi,70250) ivtnum                                    
70250 format (" ",2x,i3,4x," FAIL  ",32x,                               "COMPLEX IF - SEE SOURCE CODE" )                             
70251 format (" ",16x,"COMPUTED: " ,l1,7(2x,l1))                   
           write (nuvi,70251) avb,bvb,cvb,dvb,evb,fvb,gvb,hvb           
70252 format (" ",16x,"CORRECT:  " ,"T  F  F  T  F  F  T  T" )     
           write (nuvi,70252)                                           
           goto 37052                                                  
37050 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37052 continue                                                     
! T026*  TEST 25 - CARDS 28-29    SLASH TERMINATOR                       
           ivtnum = 25                                                  
        read(irvi, *) avb, bvb, cvb, dvb, evb                           
        read(irvi, *) avb, bvb, cvb, dvb, evb                           
! *****     TO DELETE CODE SEE NOTES FOR TEST 1                          
           if (.not. avb .and. .not. bvb .and. cvb .and.                     dvb .and. evb) goto 37053                                   
           ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70260 format (" ",16x,"COMPUTED: " , l1,4(2x,l1))                  
           write (nuvi,70260) avb,bvb,cvb,dvb,evb                       
70261 format (" ",16x,"CORRECT:  " ,"F  F  T  T  T" )              
           write (nuvi,70261)                                           
           goto 37055                                                  
37053 continue                                                     
           ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
37055 continue                                                     
! T027*  TEST 26 - CARD 30    SLASH TERMINATING IMPLIED-DO LOOP          
           ivtnum = 26                                                  
        j1i(1) = 1                                                      
        read(irvi,*) (j1i(ivi), ivi=1,3)                                
! ****      TO DELETE CODE SEE NOTES FOR TEST 1                          
           ivcorr=1                                                     
           if (j1i(1) - 1) 20270,10270,20270                            
10270 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0271                                                   
20270 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
           write(nuvi, 80024) j1i(1)                                    
           write (nuvi,80026) ivcorr                                    
 0271      continue                                                     
! T028*  TEST 27 - CARDS 31-32   SECOND READ SHOULD CAUSE VALUES         
! *****                                  TO BE READ FROM SECOND CARD     
           ivtnum = 27                                                  
        read(irvi,*) ivi, jvi                                           
        read(irvi,*) ivi, jvi                                           
! ****      TO DELETE TEST SEE NOTES FOR TEST 1                          
           if (ivi - 5) 20280,40280,20280                               
40280 if (jvi - 6) 20280,10280,20280                               
10280 ivpass=ivpass+1                                              
           write (nuvi,80002) ivtnum                                    
           goto 0281                                                   
20280 ivfail=ivfail+1                                              
           write (nuvi,80008) ivtnum                                    
70280 format (" ",16x,"COMPUTED: " ,i5,2x,i5)                      
           write (nuvi,70280) ivi,jvi                                   
70281 format (" ",16x,"CORRECT:  " ,"    5",2x,"    6")            
           write (nuvi,70281)                                           
 0281      continue                                                     
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
90009 format (" ",/," *",a5,"BEGIN*",12x,"TEST RESULTS   " ,a5,/)       
90010 format (" ",8x,"TEST DATE*TIME= " ,a17,"     COMPILER= " ,a20)    
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
! *****    END OF TEST SEGMENT 370                                       
        stop                                                            
        end program fm923
