      program fm715
!                                                                        
!      THIS ROUTINE TESTS CHARACTER EXPRESSIONS           ANS REF.       
!      AND CONCATENATION OPERATIONS USING                 6.2, 6.2.1,    
!      ASSIGNMENT STATEMENTS AND RELATIONAL               6.2.2, 6.2.2.2,
!      EXPRESSIONS.                                       6.6.5          
!                                                                        
!      THIS ROUTINE USES ROUTINES CF716-CF717 AS FUNCTION SUBPROGRAMS.   
!                                                                        
!      THE FUNCTION LEN IS ASSUMED WORKING.                              
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
      character(len=65) :: cvcomp
      character(len=65) :: cvcorr
! BE** ********************** BBCINITA **********************************
!                                                                        
      character(len=7) :: cvn001
      character(len=35) :: cvn002
      character(len=6), dimension(1:2,1:2) :: c2n001
      character(len=10) :: cf716! decl of func/sub in program
      character(len=2) :: cvn003
      character(len=2) :: cvn004
      character(len=2) :: cvd005
      character(len=2) :: cf717! decl of func/sub in program
      character(len=5), parameter :: cpn001='PQRST'
      character(len=10), parameter :: cpn002='EXPRESSION'
      character(len=*), parameter :: cpn003='NOW IS THE TIME FOR ALL GOOD MEN'
      data cvn001 / 'ONE+TWO' / 
      data cvn002 / 'THIS-IS-A-LONG-CHARACTER-STRING' / 
      data c2n001 / 'ABCDEF','GHIJKL','MNOPQR','STUVWX' / 
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
           zprog='FM715'                                                
           ivtotl =  34                                                 
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
!      TESTS 1-12 - CHARACTER EXPRESSIONS                                
!                                                                        
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER CONSTANT IN AN ASSIGNMENT STATEMENT                     
!                                                                        
           ivtnum =   1                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'CONSTANT'                                          
      cvcomp = 'CONSTANT'                                               
           if (cvcomp  ==  'CONSTANT') ivcomp = 1                       
           if (ivcomp - 1) 20010, 10010, 20010                          
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER CONSTANT IN AN IF STATEMENT                             
!                                                                        
           ivtnum =   2                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'RELATIONAL'                                             
      if (cvcomp == 'RELATIONAL') ivcomp = 1                            
40020 if (ivcomp - 1) 20020, 10020, 20020                          
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0021      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      SYMBOLIC NAME OF A CHARACTER CONSTANT IN AN ASSIGNMENT STATEMENT  
!                                                                        
           ivtnum =   3                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'PQRST'                                             
      cvcomp = cpn001                                                   
           if (cvcomp  ==  'PQRST') ivcomp = 1                          
           if (ivcomp - 1) 20030, 10030, 20030                          
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0031      continue                                                     
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      SYMBOLIC NAME OF A CHARACTER CONSTANT IN AN IF STATEMENT          
!                                                                        
           ivtnum =   4                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'EXPRESSION'                                             
      if (cvcomp == cpn002) ivcomp = 1                                  
40040 if (ivcomp - 1) 20040, 10040, 20040                          
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER VARIABLE IN AN ASSIGNMENT STATEMENT                     
!                                                                        
           ivtnum =   5                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'ONE+TWO'                                           
      cvcomp = cvn001                                                   
           if (cvcomp  ==  'ONE+TWO') ivcomp = 1                        
           if (ivcomp - 1) 20050, 10050, 20050                          
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0051      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER VARIABLE IN AN IF STATEMENT                             
!                                                                        
           ivtnum =   6                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'THIS-IS-A-LONG-CHARACTER-STRING'                        
      if (cvcomp == cvn002) ivcomp = 1                                  
40060 if (ivcomp - 1) 20060, 10060, 20060                          
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER ARRAY ELEMENT REFERENCE IN AN ASSIGNMENT STATEMENT      
!                                                                        
           ivtnum =   7                                                 
           cvcomp = ' '                                                 
           cvcorr = 'GHIJKL'                                            
           ivcomp = 0                                                   
      cvcomp = c2n001(2,1)                                              
           if (cvcomp  ==  'GHIJKL') ivcomp = 1                         
           if (ivcomp - 1) 20070, 10070, 20070                          
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0071      continue                                                     
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER ARRAY ELEMENT REFERENCE IN AN IF STATEMENT              
!                                                                        
           ivtnum =   8                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      cvcomp = 'MNOPQR'                                                 
      if (cvcomp == c2n001(1,2)) ivcomp = 1                             
40080 if (ivcomp - 1) 20080, 10080, 20080                          
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      SUBSTRING REFERENCE IN AN ASSIGNMENT STATEMENT                    
!                                                                        
           ivtnum =   9                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'CTER-STRIN'                                        
      cvcomp = cvn002(21:30)                                            
           if (cvcomp  ==  'CTER-STRIN') ivcomp = 1                     
           if (ivcomp - 1) 20090, 10090, 20090                          
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0091      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      SUBSTRING REFERENCE IN AN IF STATEMENT                            
!                                                                        
           ivtnum =  10                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'A-LONG-CHA'                                             
      if (cvcomp == cvn002(9:18)) ivcomp = 1                            
40100 if (ivcomp - 1) 20100, 10100, 20100                          
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0101      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER FUNCTION REFERENCE IN AN ASSIGNMENT STATEMENT           
!                                                                        
           ivtnum =  11                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'FIRST AID'                                         
      cvcomp  = cf716(1)
           if (cvcomp  ==  'FIRST AID') ivcomp = 1                      
           if (ivcomp - 1) 20110, 10110, 20110                          
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0111      continue                                                     
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER FUNCTION REFERENCE IN AN IF STATEMENT                   
!                                                                        
           ivtnum =  12                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'SECONDRATE'                                             
      if (cvcomp == cf716(2)) ivcomp  = 1
40120 if (ivcomp - 1) 20120, 10120, 20120                          
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0121      continue                                                     
!                                                                        
!      TESTS 13-30 CONCATENATION OPERATIONS                              
!                                                                        
!                                                                        
! T013*  TEST 013   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE TWO CHARACTER CONSTANTS IN AN ASSIGNMENT STATEMENT    
!                                                                        
           ivtnum =  13                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'ABCUVWXYZ'                                         
      cvcomp = 'ABC'//'UVWXYZ'                                          
           if (cvcomp  ==  'ABCUVWXYZ') ivcomp = 1                      
           if (ivcomp - 1) 20130, 10130, 20130                          
10130 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0131                                                   
20130 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0131      continue                                                     
!                                                                        
! T014*  TEST 014   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE TWO CHARACTER CONSTANTS IN AN IF STATEMENT            
!                                                                        
           ivtnum =  14                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'THIS-IS-IT'                                             
      if (cvcomp  == 'THIS-I'//'S-IT') ivcomp = 1                       
40140 if (ivcomp - 1) 20140, 10140, 20140                          
10140 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0141                                                   
20140 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0141      continue                                                     
!                                                                        
! T015*  TEST 015   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A SYMBOLIC NAME OF A CHARACTER CONSTANT WITH A LITERAL
!      STRING IN AN ASSIGNMENT STATEMENT                                 
!                                                                        
           ivtnum =  15                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'PQRSTUVWXYZ'                                       
      cvcomp = cpn001//'UVWXYZ'                                         
           if (cvcomp  ==  'PQRSTUVWXYZ') ivcomp = 1                    
           if (ivcomp - 1) 20150, 10150, 20150                          
10150 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0151                                                   
20150 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0151      continue                                                     
!                                                                        
! T016*  TEST 016   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A SYMBOLIC NAME OF A CHARACTER CONSTANT WITH A LITERAL
!      STRING IN AN IF STATEMENT                                         
!                                                                        
           ivtnum =  16                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      cvcomp = 'USEFUL-EXPRESSION'                                      
      if (cvcomp == 'USEFUL-'//cpn002) ivcomp = 1                       
40160 if (ivcomp - 1) 20160, 10160, 20160                          
10160 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0161                                                   
20160 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0161      continue                                                     
!                                                                        
! T017*  TEST 017   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER VARIABLE WITH A LITERAL STRING IN AN      
!                                       ASSIGNMENT STATEMENT             
!                                                                        
           ivtnum =  17                                                 
      cvcomp = ' '                                                      
      ivcomp = 0                                                        
           cvcorr = 'ONE+TWO+THREE'                                     
      cvcomp = cvn001//'+THREE'                                         
           if (cvcomp  ==  'ONE+TWO+THREE') ivcomp = 1                  
           if (ivcomp - 1) 20170, 10170, 20170                          
10170 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0171                                                   
20170 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0171      continue                                                     
!                                                                        
! T018*  TEST 018   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER VARIABLE WITH A LITERAL STRING IN AN      
!                                       IF STATEMENT                     
!                                                                        
           ivtnum =  18                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      cvcomp = 'ZERO+ONE+TWO'                                           
      if (cvcomp == 'ZERO+'//cvn001) ivcomp = 1                         
40180 if (ivcomp - 1) 20180, 10180, 20180                          
10180 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0181                                                   
20180 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0181      continue                                                     
!                                                                        
! T019*  TEST 019   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER ARRAY ELEMENT WITH A LITERAL STRING IN AN 
!                                            ASSIGNMENT STATEMENT        
!                                                                        
           ivtnum =  19                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'STUVWXYZ-END'                                      
      cvcomp = c2n001(2,2)//'YZ-END'                                    
           if (cvcomp  ==  'STUVWXYZ-END') ivcomp = 1                   
           if (ivcomp - 1) 20190, 10190, 20190                          
10190 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0191                                                   
20190 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0191      continue                                                     
!                                                                        
! T020*  TEST 020   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER ARRAY ELEMENT WITH A LITERAL STRING IN AN 
!                                            IF STATEMENT                
!                                                                        
           ivtnum =  20                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      cvcomp = 'BEGIN-ABCDEF'                                           
      if (cvcomp == 'BEGIN-'//c2n001(1,1)) ivcomp = 1                   
40200 if (ivcomp - 1) 20200, 10200, 20200                          
10200 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0201                                                   
20200 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0201      continue                                                     
!                                                                        
! T021*  TEST 021   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE SPECIAL CHARACTERS IN AN ASSIGNMENT STATEMENT         
!                                                                        
           ivtnum =  21                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = '=+-*/(),.$'':'                                     
      cvcomp = '=+-*/('//'),.$'':'                                      
           if (cvcomp  ==  '=+-*/(),.$'':') ivcomp = 1                  
           if (ivcomp - 1) 20210, 10210, 20210                          
10210 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0211                                                   
20210 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0211      continue                                                     
!                                                                        
! T022*  TEST 022   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE SPECIAL CHARACTERS IN AN IF STATEMENT                 
!                                                                        
           ivtnum =  22                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = '$X=(A/B+C):(-''D'')'                                    
      if (cvcomp == '$X=(A/'//'B+C):(-''D'')') ivcomp = 1               
40220 if (ivcomp - 1) 20220, 10220, 20220                          
10220 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0221                                                   
20220 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0221      continue                                                     
!                                                                        
!      TESTS 23-24 -  TESTS THE INTRINSIC FUNCTION LEN(E) WHERE THE      
!                     ARGUMENT IS A CHARACTER EXPRESSION                 
!                                                                        
!                                                                        
! T023*  TEST 023   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!                                                                        
           ivtnum =  23                                                 
           ivcomp = 0                                                   
           ivcorr = 15                                                  
      ivcomp = len(cvn001//'EIGHTEEN')                                  
40230 if (ivcomp - 15) 20230, 10230, 20230                         
10230 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0231                                                   
20230 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0231      continue                                                     
!                                                                        
! T024*  TEST 024   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!                                                                        
           ivtnum =  24                                                 
           ivcomp = 0                                                   
           ivcorr = 30                                                  
      ivcomp = len('THIS-IS-A-LITERAL-STRING'//c2n001(1,2))             
40240 if (ivcomp - 30) 20240, 10240, 20240                         
10240 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0241                                                   
20240 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0241      continue                                                     
!                                                                        
! T025*  TEST 025   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A SUBSTRING WITH A LITERAL STRING IN AN ASSIGNMENT    
!                                             STATEMENT                  
!                                                                        
           ivtnum =  25                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'IS-A-LONG-ARRAY'                                   
      cvcomp = cvn002(6:15)//'ARRAY'                                    
           if (cvcomp  ==  'IS-A-LONG-ARRAY') ivcomp = 1                
           if (ivcomp - 1) 20250, 10250, 20250                          
10250 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0251                                                   
20250 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0251      continue                                                     
!                                                                        
! T026*  TEST 026   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A SUBSTRING WITH A LITERAL STRING IN AN IF            
!                                             STATEMENT                  
!                                                                        
           ivtnum =  26                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'A-LONG-CHARTER-PLANE'                                   
      if (cvcomp == cvn002(9:19)//'TER-PLANE') ivcomp = 1               
40260 if (ivcomp - 1) 20260, 10260, 20260                          
10260 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0261                                                   
20260 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0261      continue                                                     
!                                                                        
! T027*  TEST 027   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER FUNCTION REFERENCE WITH A LITERAL STRING  
!                                                                        
           ivtnum =  27                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'THIRDCLASSMAIL'                                    
      cvcomp  = cf716(3)//'MAIL'
           if (cvcomp  ==  'THIRDCLASSMAIL') ivcomp = 1                 
           if (ivcomp - 1) 20270, 10270, 20270                          
10270 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0271                                                   
20270 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0271      continue                                                     
!                                                                        
! T028*  TEST 028   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER ARRAY ELEMENT WITH A CHARACTER FUNCTION RE
!                                                                        
           ivtnum =  28                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'MNOPQRFIRST AID'                                   
      cvcomp  = c2n001(1,2)//cf716(1)
           if (cvcomp  ==  'MNOPQRFIRST AID') ivcomp = 1                
           if (ivcomp - 1) 20280, 10280, 20280                          
10280 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0281                                                   
20280 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0281      continue                                                     
!                                                                        
! T029*  TEST 029   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A CHARACTER SUBSTRING WITH A CHARACTER FUNCTION REFERE
!                                                                        
           ivtnum =  29                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'G-CHARACSECONDRATE'                                
      cvcomp  = cvn002(14:21)//cf716(2)
           if (cvcomp  ==  'G-CHARACSECONDRATE') ivcomp = 1             
           if (ivcomp - 1) 20290, 10290, 20290                          
10290 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0291                                                   
20290 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0291      continue                                                     
!                                                                        
! T030*  TEST 030   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATIONS ON BOTH SIDES OF ".EQ." IN AN IF STATEMENT         
!                                                                        
           ivtnum =  30                                                 
           ivcomp = 0                                                   
           ivcorr = 1                                                   
      cvn002 = 'STTHIRDCLASS'                                           
      if (cpn001//cf716(3) == c2n001(1,2)(4:6)//cvn002) ivcomp  = 1
40300 if (ivcomp - 1) 20300, 10300, 20300                          
10300 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0301                                                   
20300 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0301      continue                                                     
!                                                                        
! T031*  TEST 031   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CONCATENATE A LITERAL WITH A SYMBOLIC NAME OF A CHARACTER CONSTANT
!      LENGTH IS SPECIFIED BY AN ASTERISK WITH A LITERAL                 
!                                                                        
           ivtnum =  31                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = 'NOW IS THE TIME FOR ALL GOOD MENTO COME TO THE AID OF THEIR PARTY'                                                        
      if (cvcomp == cpn003//'TO COME TO THE AID OF THEIR PARTY')           ivcomp = 1                                                     
40310 if (ivcomp - 1) 20310, 10310, 20310                          
10310 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0311                                                   
20310 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0311      continue                                                     
!                                                                        
! T032*  TEST 032   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!      CHARACTER EXPRESSION CONCATENATED WITH CHARACTER PRIMARY          
!                                                                        
           ivtnum =  32                                                 
           ivcomp = 0                                                   
           cvcomp = ' '                                                 
           cvcorr = ' '                                                 
           ivcorr = 1                                                   
      cvcomp = ('ONE'//'TWO')//'THREE'                                  
      cvcorr = 'ONE'//'TWO'//'THREE'                                    
      if (cvcomp == cvcorr) ivcomp = 1                                  
40320 if (ivcomp - 1) 20320, 10320, 20320                          
10320 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0321                                                   
20320 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0321      continue                                                     
!                                                                        
!      TESTS 33-34 - EVALUATION OF CHARACTER EXPRESSIONS                 
!      (PROCESSOR NEEDS TO EVALUATE ONLY AS MUCH OF THE CHARACTER        
!      EXPRESSION AS IS REQUIRED BY THE CONTEXT IN WHICH THE             
!      EXPRESSION APPEARS)                                               
!                                                                        
!                                                                        
! T033*  TEST 033   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!                                                                        
           ivtnum =  33                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'AB'                                                
      cvn003 = 'ABC'                                                    
      cvcomp = cvn003                                                   
           if (cvcomp  ==  'AB') ivcomp = 1                             
           if (ivcomp - 1) 20330, 10330, 20330                          
10330 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0331                                                   
20330 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0331      continue                                                     
!                                                                        
! T034*  TEST 034   ****  FCVS PROGRAM 715  ****                         
!                                                                        
!                                                                        
           ivtnum =  34                                                 
           cvcomp = ' '                                                 
           ivcomp = 0                                                   
           cvcorr = 'LO'                                                
      cvn004 = 'LONG'                                                   
      cvd005 = 'SHORT'                                                  
      cvn003  = cvn004//cf717(cvd005)
      cvcomp = cvn003                                                   
           if (cvcomp  ==  'LO') ivcomp = 1                             
           if (ivcomp - 1) 20340, 10340, 20340                          
10340 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0341                                                   
20340 ivfail = ivfail + 1                                          
           write (i02,80018) ivtnum, cvcomp, cvcorr                     
 0341      continue                                                     
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
90001 format (" ",56x,"FM715")                                          
90000 format (" ",50x,"END OF PROGRAM FM715" )                          
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
           end program fm715
      character*10 function cf716(ivd001)
      integer :: ivd001
      if (ivd001 - 2) 70010, 70020, 70030                               
70010 cf716 = 'FIRST AID'                                               
      return                                                            
70020 cf716 = 'SECONDRATE'                                              
      return                                                            
70030 cf716 = 'THIRDCLASS'                                              
      return                                                            
      end function cf716
      character*(*) function cf717(cvd001)
      character(len=*) :: cvd001
      cf717 = cvd001                                                    
      return                                                            
      end function cf717
