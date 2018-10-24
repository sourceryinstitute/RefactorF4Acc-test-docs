      program fm801
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM801               YDINT - (155)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INTRINSIC FUNCTIONS DINT, DNINT, IDNINT           15.3   
! *****    TRUNCATION (SIGN OF A * LARGEST INTEGER LE ABS(A) )  (TABLE 5)
! *****                                                                  
! *****  GENERAL COMMENTS                                                
! *****          FLOAT FUNCTION ASSUMED WORKING                          
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
! *****    S P E C I F I C A T I O N S  SEGMENT 155                      
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
      real :: dvcorr
      integer :: inavi
      integer :: ivcorr
        double precision :: dnavd
        double precision :: dnbvd
        double precision :: dndvd
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
      ivtotl = 45                                                       
      zprog = 'FM801'                                                   
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
! *****    HEADER FOR SEGMENT 155                                        
        write (nuvi,15501)                                              
15501 format (" ", // 1x,"YDINT - (155) INTRINSIC FUNCTIONS--" //16x,           "DINT, DNINT, IDNINT (TYPE CONVERSION) "  //                      "  ANS REF. - 15.3" )                                   
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
! *****    TEST OF DINT                                                  
! *****                                                                  
        write(nuvi, 15502)                                              
15502 format(// 8x, "TEST OF DINT" )                                  
! T001*  TEST 1                                         THE VALUE ZERO   
           ivtnum = 1                                                   
        dnbvd = 0.0d0                                                   
        dnavd = dint(dnbvd)                                             
           if (dnavd + 5.0d-10) 20010, 10010, 40010                     
40010 if (dnavd - 5.0d-10) 10010, 10010, 20010                     
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0011      continue                                                     
! T002*  TEST 2                                       A VALUE IN (0,1)   
           ivtnum = 2                                                   
        dnbvd = 0.375d0                                                 
        dnavd = dint(dnbvd)                                             
           if (dnavd + 5.0d-10) 20020, 10020, 40020                     
40020 if (dnavd - 5.0d-10) 10020, 10020, 20020                     
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0021      continue                                                     
! T003*  TEST 3                                            THE VALUE 1   
           ivtnum = 3                                                   
        dnbvd = float(1)                                                
        dnavd = dint(dnbvd)                                             
           if (dnavd - 0.9999999995d0) 20030, 10030, 40030              
40030 if (dnavd - 1.000000001d0) 10030, 10030, 20030               
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0031      continue                                                     
! T004*  TEST 4                      AN INTEGRAL VALUE OTHER THAN 0, 1   
           ivtnum = 4                                                   
        dnbvd = float(6)                                                
        dnavd = dint(dnbvd)                                             
           if (dnavd - 5.999999997d0) 20040, 10040, 40040               
40040 if (dnavd - 6.000000003d0) 10040, 10040, 20040               
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           dvcorr = 6.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0041      continue                                                     
! T005*  TEST 5                                     A VALUE IN (X,X+1)   
           ivtnum = 5                                                   
        dnbvd = 0.375d1                                                 
        dnavd = dint(dnbvd)                                             
           if (dnavd - 2.999999998d0) 20050, 10050, 40050               
40050 if (dnavd - 3.000000002d0) 10050, 10050, 20050               
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           dvcorr = 0.3d1                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0051      continue                                                     
! T006*  TEST 6               A NEGATIVE VALUE WITH MAGNITUDE IN (0,1)   
           ivtnum = 6                                                   
        dnbvd = -0.375d0                                                
        dnavd = dint(dnbvd)                                             
           if (dnavd + 5.0d-10) 20060, 10060, 40060                     
40060 if (dnavd - 5.0d-10) 10060, 10060, 20060                     
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0061      continue                                                     
! T007*  TEST 7                                           THE VALUE -1   
           ivtnum = 7                                                   
        dnbvd = float(-1)                                               
        dnavd = dint(dnbvd)                                             
           if (dnavd + 1.000000001d0) 20070, 10070, 40070               
40070 if (dnavd + 0.9999999995d0) 10070, 10070, 20070              
10070 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           dvcorr = -1.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0071      continue                                                     
! T008*  TEST 8                              A NEGATIVE INTEGRAL VALUE   
           ivtnum = 8                                                   
        dnbvd = float(-6)                                               
        dnavd = dint(dnbvd)                                             
           if (dnavd + 6.000000003d0) 20080, 10080, 40080               
40080 if (dnavd + 5.999999997d0) 10080, 10080, 20080               
10080 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           dvcorr = -6.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0081      continue                                                     
! T009*  TEST 9             A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1)   
           ivtnum = 9                                                   
        dnbvd = -0.375d1                                                
        dnavd = dint(dnbvd)                                             
           if (dnavd + 3.000000002d0) 20090, 10090, 40090               
40090 if (dnavd + 2.999999998d0) 10090, 10090, 20090               
10090 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           dvcorr = -0.3d1                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0091      continue                                                     
! T010*  TEST 10                       ZERO PREFIXED WITH A MINUS SIGN   
           ivtnum = 10                                                  
        dnbvd = 0.0d0                                                   
        dnavd = dint(-dnbvd)                                            
           if (dnavd + 5.0d-10) 20100, 10100, 40100                     
40100 if (dnavd - 5.0d-10) 10100, 10100, 20100                     
10100 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0101      continue                                                     
! T011*  TEST 11        AN ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   
           ivtnum = 11                                                  
        dnbvd = 0.375d1                                                 
        dnavd = dint(dnbvd/0.375d0)                                     
           if (dnavd - 0.9000000000d1) 20110, 10110, 40110              
40110 if (dnavd - 1.000000001d1) 10110, 10110, 20110               
10110 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           dvcorr = 1.0d1                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0111      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TEST OF DNINT                                                 
! *****                                                                  
        write(nuvi, 15504)                                              
15504 format( // 8x, "TEST OF DNINT" )                                
! T012*  TEST 12                                        THE VALUE ZERO   
           ivtnum = 12                                                  
        dnbvd = 0.0d0                                                   
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 5.0d-10) 20120, 10120, 40120                     
40120 if (dnavd - 5.0d-10) 10120, 10120, 20120                     
10120 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0121      continue                                                     
! T013*  TEST 13                                     A VALUE IN (0,.5)   
           ivtnum = 13                                                  
        dnbvd = 0.25d0                                                  
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 5.0d-10) 20130, 10130, 40130                     
40130 if (dnavd - 5.0d-10) 10130, 10130, 20130                     
10130 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0131      continue                                                     
! T014*  TEST 14                                         THE VALUE 0.5   
           ivtnum = 14                                                  
        dnbvd = float(1) / float(2)                                     
        dnavd = dnint(dnbvd)                                            
           if (dnavd - 0.9999999995d0) 20140, 10140, 40140              
40140 if (dnavd - 1.000000001d0) 10140, 10140, 20140               
10140 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0141      continue                                                     
! T015*  TEST 15                                     A VALUE IN (.5,1)   
           ivtnum = 15                                                  
        dnbvd = 0.75d0                                                  
        dnavd = dnint(dnbvd)                                            
           if (dnavd - 0.9999999995d0) 20150, 10150, 40150              
40150 if (dnavd - 1.000000001d0) 10150, 10150, 20150               
10150 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0151      continue                                                     
! T016*  TEST 16                     AN INTEGRAL VALUE OTHER THAN 0, 1   
           ivtnum = 16                                                  
        dnbvd = float(5)                                                
        dnavd = dnint(dnbvd)                                            
           if (dnavd - 4.999999997d0) 20160, 10160, 40160               
40160 if (dnavd - 5.000000003d0) 10160, 10160, 20160               
10160 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           dvcorr = 5.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0161      continue                                                     
! T017*  TEST 17                                   A VALUE IN (X,X+.5)   
           ivtnum = 17                                                  
        dnbvd = 10.46875d0                                              
        dnavd = dnint(dnbvd)                                            
           if (dnavd - 9.999999995d0) 20170, 10170, 40170               
40170 if (dnavd - 10.00000001d0) 10170, 10170, 20170               
10170 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           dvcorr = 10.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0171      continue                                                     
! T018*  TEST 18                 A VALUE WITH FRACTIONAL COMPONENT 0.5   
           ivtnum = 18                                                  
        dnbvd = float(15) + float(1) / float(2)                         
        dnavd = dnint(dnbvd)                                            
           if (dnavd - 15.99999999d0) 20180, 10180, 40180               
40180 if (dnavd - 16.00000001d0) 10180, 10180, 20180               
10180 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           dvcorr = 16.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0181      continue                                                     
! T019*  TEST 19                                 A VALUE IN (X+.5,X+1)   
           ivtnum = 19                                                  
        dnbvd = 27.96875d0                                              
        dnavd = dnint(dnbvd)                                            
           if (dnavd - 27.99999998d0) 20190, 10190, 40190               
40190 if (dnavd - 28.00000002d0) 10190, 10190, 20190               
10190 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           dvcorr = 28.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0191      continue                                                     
! T020*  TEST 20             A NEGATIVE VALUE WITH MAGNITUDE IN (0,.5)   
           ivtnum = 20                                                  
        dnbvd = -0.25d0                                                 
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 5.0d-10) 20200, 10200, 40200                     
40200 if (dnavd - 5.0d-10) 10200, 10200, 20200                     
10200 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0201      continue                                                     
! T021*  TEST 21                                        THE VALUE -0.5   
           ivtnum = 21                                                  
        dnbvd = -float(1) / float(2)                                    
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 1.000000001d0) 20210, 10210, 40210               
40210 if (dnavd + 0.9999999995d0) 10210, 10210, 20210              
10210 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           dvcorr = -1.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0211      continue                                                     
! T022*  TEST 22             A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1)   
           ivtnum = 22                                                  
        dnbvd = -0.75d0                                                 
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 1.000000001d0) 20220, 10220, 40220               
40220 if (dnavd + 0.9999999995d0) 10220, 10220, 20220              
10220 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           dvcorr = -1.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0221      continue                                                     
! T023*  TEST 23                             A NEGATIVE INTEGRAL VALUE   
           ivtnum = 23                                                  
        dnbvd = -float(5)                                               
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 5.000000003d0) 20230, 10230, 40230               
40230 if (dnavd + 4.999999997d0) 10230, 10230, 20230               
10230 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           dvcorr = -5.0d0                                              
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0231      continue                                                     
! T024*  TEST 24           A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5)   
           ivtnum = 24                                                  
        dnbvd = -10.46875d0                                             
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 10.00000001d0) 20240, 10240, 40240               
40240 if (dnavd + 9.999999995d0) 10240, 10240, 20240               
10240 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           dvcorr = -10.0d0                                             
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0241      continue                                                     
! T025*  TEST 25        A NEGATIVE VALUE WITH FRACTIONAL COMPONENT 0.5   
           ivtnum = 25                                                  
        dnbvd = float(-15) - float(1) / float(2)                        
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 16.00000001d0) 20250, 10250, 40250               
40250 if (dnavd + 15.99999999d0) 10250, 10250, 20250               
10250 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           dvcorr = -16.0d0                                             
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0251      continue                                                     
! T026*  TEST 26         A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1)   
           ivtnum = 26                                                  
        dnbvd = -27.96875d0                                             
        dnavd = dnint(dnbvd)                                            
           if (dnavd + 28.00000002d0) 20260, 10260, 40260               
40260 if (dnavd + 27.99999998d0) 10260, 10260, 20260               
10260 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           dvcorr = -28.0d0                                             
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0261      continue                                                     
! T027*  TEST 27                       ZERO PREFIXED WITH A MINUS SIGN   
           ivtnum = 27                                                  
        dnbvd = 0.0d0                                                   
        dnavd = dnint(-dnbvd)                                           
           if (dnavd + 5.0d-10) 20270, 10270, 40270                     
40270 if (dnavd - 5.0d-10) 10270, 10270, 20270                     
10270 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           dvcorr = 0.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0271      continue                                                     
! T028*  TEST 28        AN ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   
           ivtnum = 28                                                  
        dnbvd = 8.00d0                                                  
        dndvd = 7.25d0                                                  
        dnavd = dnint(dnbvd - dndvd)                                    
           if (dnavd - 0.9999999995d0) 20280, 10280, 40280              
40280 if (dnavd - 1.000000001d0) 10280, 10280, 20280               
10280 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           dvcorr = 1.0d0                                               
           write (nuvi, 80031) ivtnum, dnavd, dvcorr                    
 0281      continue                                                     
! *****                                                                  
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TEST OF IDNINT                                                
! *****                                                                  
! *****                                                                  
        write(nuvi, 15506)                                              
15506 format( // 8x, "TEST OF IDNINT" )                               
! T029*  TEST 29                                      THE VALUE ZERO     
           ivtnum = 29                                                  
        dnbvd = 0.0d0                                                   
        inavi = idnint(dnbvd)                                           
           if (inavi - 0) 20290, 10290, 20290                           
10290 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0291      continue                                                     
! T030*  TEST 30                                     A VALUE IN (0.,5)   
           ivtnum = 30                                                  
        dnbvd = 0.25d0                                                  
        inavi = idnint(dnbvd)                                           
           if (inavi - 0) 20300, 10300, 20300                           
10300 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0301      continue                                                     
! T031*  TEST 31                                          THE VALUE 0.5  
           ivtnum = 31                                                  
        dnbvd = float(1) / float(2)                                     
        inavi = idnint(dnbvd)                                           
           if (inavi - 1) 20310, 10310, 20310                           
10310 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0311      continue                                                     
! T032*  TEST 32                                     A VALUE IN (.5,1)   
           ivtnum = 32                                                  
        dnbvd = 0.75d0                                                  
        inavi = idnint(dnbvd)                                           
           if (inavi - 1) 20320, 10320, 20320                           
10320 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0321      continue                                                     
! T033*  TEST 33                     AN INTEGRAL VALUE OTHER THAN 0, 1   
           ivtnum = 33                                                  
        dnbvd = float(5)                                                
        inavi = idnint(dnbvd)                                           
           if (inavi - 5) 20330, 10330, 20330                           
10330 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           ivcorr = 5                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0331      continue                                                     
! T034*  TEST 34                                     A VALUE IN (X,X+.5) 
           ivtnum = 34                                                  
        dnbvd = 10.46875d0                                              
        inavi = idnint(dnbvd)                                           
           if (inavi - 10) 20340, 10340, 20340                          
10340 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           ivcorr = 10                                                  
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0341      continue                                                     
! T035*  TEST 35                 A VALUE WITH FRACTIONAL COMPONENT 0.5   
           ivtnum = 35                                                  
        dnbvd = float(15) + float(1) / float(2)                         
        inavi = idnint(dnbvd)                                           
           if (inavi - 16) 20350, 10350, 20350                          
10350 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0351                                                   
20350 ivfail = ivfail + 1                                          
           ivcorr = 16                                                  
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0351      continue                                                     
! T036*  TEST 36                                 A VALUE IN (X+.5,X+1)   
           ivtnum = 36                                                  
        dnbvd = 27.96875d0                                              
        inavi = idnint(dnbvd)                                           
           if (inavi - 28) 20360, 10360, 20360                          
10360 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0361                                                   
20360 ivfail = ivfail + 1                                          
           ivcorr = 28                                                  
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0361      continue                                                     
! T037*  TEST 37             A NEGATIVE VALUE WITH MAGNITUDE IN (0,.5)   
           ivtnum = 37                                                  
        dnbvd = -0.25d0                                                 
        inavi = idnint(dnbvd)                                           
           if (inavi - 0) 20370, 10370, 20370                           
10370 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0371                                                   
20370 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0371      continue                                                     
! T038*  TEST 38                                        THE VALUE -0.5   
           ivtnum = 38                                                  
        dnbvd = -float(1) / float(2)                                    
        inavi = idnint(dnbvd)                                           
           if (inavi + 1) 20380, 10380, 20380                           
10380 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0381                                                   
20380 ivfail = ivfail + 1                                          
           ivcorr = -1                                                  
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0381      continue                                                     
! T039*  TEST 39             A NEGATIVE VALUE WITH MAGNITUDE IN (.5,1)   
           ivtnum = 39                                                  
        dnbvd = -0.75d0                                                 
        inavi = idnint(dnbvd)                                           
           if (inavi + 1) 20390, 10390, 20390                           
10390 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0391                                                   
20390 ivfail = ivfail + 1                                          
           ivcorr = -1                                                  
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0391      continue                                                     
! T040*  TEST 40                             A NEGATIVE INTEGRAL VALUE   
           ivtnum = 40                                                  
        dnbvd = -float(5)                                               
        inavi = idnint(dnbvd)                                           
           if (inavi + 5) 20400, 10400, 20400                           
10400 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0401                                                   
20400 ivfail = ivfail + 1                                          
           ivcorr = -5                                                  
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0401      continue                                                     
! T041*  TEST 41           A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+.5)   
           ivtnum = 41                                                  
        dnbvd = -10.46875d0                                             
        inavi = idnint(dnbvd)                                           
           if (inavi + 10) 20410, 10410, 20410                          
10410 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0411                                                   
20410 ivfail = ivfail + 1                                          
           ivcorr = -10                                                 
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0411      continue                                                     
! T042*  TEST 42        A NEGATIVE VALUE WITH FRACTIONAL COMPONENT 0.5   
           ivtnum = 42                                                  
        dnbvd = float(-15) - float(1) /float(2)                         
        inavi = idnint(dnbvd)                                           
           if (inavi + 16) 20420, 10420, 20420                          
10420 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0421                                                   
20420 ivfail = ivfail + 1                                          
           ivcorr = -16                                                 
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0421      continue                                                     
! T043*  TEST 43         A NEGATIVE VALUE WITH MAGNITUDE IN (X+.5,X+1)   
           ivtnum = 43                                                  
        dnbvd = -27.96875d0                                             
        inavi = idnint(dnbvd)                                           
           if (inavi + 28) 20430, 10430, 20430                          
10430 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0431                                                   
20430 ivfail = ivfail + 1                                          
           ivcorr = -28                                                 
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0431      continue                                                     
! T044*  TEST 44                       ZERO PREFIXED WITH A MINUS SIGN   
           ivtnum = 44                                                  
        dnbvd = 0.0d0                                                   
        inavi = idnint(-dnbvd)                                          
           if (inavi - 0) 20440, 10440, 20440                           
10440 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0441                                                   
20440 ivfail = ivfail + 1                                          
           ivcorr = 0                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0441      continue                                                     
! T045*  TEST 45        AN ARITHMETIC EXPRESSION PRESENTED TO FUNCTION   
           ivtnum = 45                                                  
        dnbvd = 8.00d0                                                  
        dndvd = 7.25d0                                                  
        inavi = idnint(dnbvd - dndvd)                                   
           if (inavi - 1) 20450, 10450, 20450                           
10450 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0451                                                   
20450 ivfail = ivfail + 1                                          
           ivcorr = 1                                                   
           write (nuvi, 80010) ivtnum, inavi, ivcorr                    
 0451      continue                                                     
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
! *****    END OF TEST SEGMENT 155                                       
        stop                                                            
        end program fm801
