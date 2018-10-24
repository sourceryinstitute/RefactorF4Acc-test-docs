      program fm900
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM900               FMTRWF - (021)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REFS
! *****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.2
! *****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  
! *****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  
! *****    PROGRAM SEGMENTS FOR DOUBLE PRECISION AND COMPLEX             
! *****    DATA TYPES.                                                   
! *****  RESTRICTIONS OBSERVED                                   12.8.2  
! *****  *  ALL FORMAT STATEMENTS ARE LABELED                    13.1.1  
! *****  *  H AND X DESCRIPTORS ARE NEVER REPEATED               13.2.1  
! *****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               
! *****     W IS EQUAL TO OR GREATER THAN D                              
! *****  *  FIELD WIDTH IS NEVER ZERO                            13.2.1  
! *****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    
! *****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           
! *****     IN THE FORMAT SPECIFICATION                                  
! *****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS     13.3    
! *****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                    13.5.9  
! *****  *  FIELD WIDTH NEVER EXCEEDED BY OUTPUT                 13.5.9  
! *****  GENERAL COMMENTS                                                
! *****    PLUS SIGNS FOR INPUT FIELDS ARE USUALLY OMITTED       13.5.9  
! *****    FORMATTED WRITES WITHOUT AN I/O LIST (FORMAT          13.5.2  
! *****    STATEMENTS TEST H AND X DESCRIPTORS AND SLASH         13.5.3  
! *****    RECORD DIVIDERS)                                      13.5.4  
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
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 17 CARD IMAGES IN COL. 1 - 80 
! OL.      1----------------------------------------------------------61 
! ARD  1   1.05.522.066.633.123455.0789                                  
! ARD  2   123.00456.88 0.123E+01  +0.987+1 -0.2345+02 -0.6879E+2+0.7E+0 
! OL     62-----70                                                       
! ARD  2 3 0.4E+03                                                       
! OL.      1----------------------------------------------------------61 
! ARD  3    0.9876543E-04+0.1357913E-04                                  
! ARD  4   19.34+0.2468E+02   +.765+287.643.96 0.5407E+0243.96+0.5407E+0 
! OL.    62-------------78                                               
! ARD  4 243.96   0.5407+2                                               
! OL.      1----------------------------- ----------------------------61 
! ARD  5     +0.1D+06                                                    
! ARD  6   -0.334D-04   -.334-4 +0.7657654D00 0.12345678901D+10          
! ARD  7    +0.98765432109876D-1+0.98765432109876D-01    .98765432109876 
! OL.    62-66                                                           
! ARD  7 -1                                                              
! OL.      1----------------------------------------------------------61 
! ARD  8    -.555555542D+03  -0.555555542+3                              
! ARD  9     9.91.19.92.29.93.39.94.49.91.19.92.29.93.39.94.4            
! ARD 10   9.95.59.96.69.97.79.98.89.95.59.96.69.97.79.98.8              
! ARD 11   -0.99D+01-0.98D+01-0.97D+01-0.96D+01-0.99D+01 -.98D+01  -.97+ 
! OL.    62-------72                                                     
! ARD 11 01   -.96+1                                                     
! ARD 12     +0.99D+01 0.98D+01  +.97D01   +.96D1                        
! ARD 13             +0.99D+01 0.99D+01 0.99D+01+0.99D+01    .99D1       
! ARD 14   9.95.59.96.69.97.79.98.8                                      
! ARD 15   123.45678E2  1234.5678  123.45678  12.345678  1.2345678  .123 
! OL.    62-66                                                           
! ARD 15 45678                                                           
! OL.      1----------------------------------------------------------61 
! ARD 16    9876.5498.7654E2 9876.54   987.654864786D-486.4786E286.4786  
! OL.    62---------------80                                             
! ARD 16  8657.86D0  9876.54                                             
! OL.      1----------------------------------------------------------61 
! ARD 17    9.8765698.7654E2  9876.54  987.654864786D-386.4786E286.4786  
! OL.    62---------------80                                             
! ARD 17  8657.86D0  9876.54                                             
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 021                        
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
      integer :: irvi
      integer :: nuvi
      integer :: ivtnum
      double precision, dimension(1:5) :: dpa1d
      double precision, dimension(1:1,1:4,1:2) :: mca3d
      double precision :: zzdvd
      double precision, dimension(1:2,1:2) :: a2d
      double precision, dimension(1:2,1:2,1:2) :: a3d
      double precision, dimension(1:10) :: ac1d
      double precision, dimension(1:7,1:4) :: bc2d
      double precision :: dpavd
      double precision :: dpbvd
      complex :: bvc
      complex :: qavc
      complex :: chavc
      complex :: chbvc
      complex :: chcvc
      complex :: chdvc
      complex, dimension(1:32) :: ll1c
      complex, dimension(1:8,1:4) :: lm2c
      complex, dimension(1:12) :: a1c
      complex, dimension(1:2,1:2) :: a2c
      complex, dimension(1:2,1:2,1:2) :: b3c
      complex, dimension(1:8) :: b1c
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
! *****                                                                  
! BB** ********************** BBCINITA **********************************
! **** SPECIFICATION STATEMENTS                                          
! ****                                                                   
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
      ivtotl = 36                                                       
      zprog = 'FM900'                                                   
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
! *****    HEADER FORMAT STATEMENT                                       
      write (nuvi,02100)                                                
02100 format (" ",/1x,"FMTRWF - (021) FORMATTED I/O" //2x,                       "REFS - 12.9.5  13.3  13.5" )                            
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! *****    TESTS 1 THRU 11:                                              
! *****    FORMATTED READ AND WRITE STATEMENTS WITH COMPLEX  12.8.1      
! *****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST.      12.8.2      
! *****    E AND F CONVERSION ARE USED IN THE FORMAT         13.5.9.2.1-2
! *****    STATEMENTS. SOME FORMAT DESCRIPTORS ARE REPEATED  13.5.9.2.1  
! *****                                                                  
02101 format (/8x,"COMPLEX CONVERSION TEST" /)                          
      write (nuvi,02101)                                                
! *****  INPUT CARD  1                                                   
02102 format ( 2(f3.1) , 2(f4.1), 2(f7.4))                              
      read (irvi,02102) chavc, chbvc, a1c(2)                            
! *****  INPUT CARDS 2, 3                                                
02103 format ( 2f6.2, 2e10.3, 2e11.4, 2e8.1/ 2e14.7)                    
      read (irvi,02103) a2c(1,2), b3c(2,2,1), chcvc, a1c(1), chdvc      
! *****  INPUT CARD  4                                                   
02104 format (f5.2, e11.4, e10.3, f4.1, 3(f5.2,e11.4))                  
      read (irvi,02104) a2c(2,1), bvc, qavc, lm2c(1,2), ll1c(2)         
! T001*  TEST 1                                                          
           ivtnum = 1                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write (nuvi, 70010) chavc                                       
70010 format (26x,f3.1,2x,f3.1)                                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70011)                                          
70011 format (26x, "1.0  5.5")                                     
! T002*  TEST 2                                                          
           ivtnum = 2                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write (nuvi, 70020) chbvc                                       
70020 format (26x,f4.1,2x,f4.1)                                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70021)                                          
70021 format (26x,"22.0  66.6" )                                   
! T003*  TEST 3                                                          
           ivtnum = 3                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write (nuvi, 70030) a1c(2)                                      
70030 format (26x,f7.4,2x,f7.4)                                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70031)                                          
70031 format (26x,"33.1234  55.0789" )                             
! T004*  TEST 4                                                          
           ivtnum = 4                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write (nuvi, 70040) a2c(1,2)                                    
70040 format (26x,f6.2,2x,f6.2)                                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70041)                                          
70041 format (26x,"123.00  456.88" )                               
! T005*  TEST 5                                                          
           ivtnum = 5                                                   
           remrks = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   
           write (nuvi, 80004) ivtnum,remrks                            
           write (nuvi, 80020)                                          
        write (nuvi, 70050) b3c(2,2,1)                                  
70050 format (26x,e10.3,2x,e10.3)                                     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
70051 format (" ",16x,"CORRECT:  " ,22x,  "2 CORRECT ANSWERS POSSIBLE")                                                              
           write (nuvi, 70052)                                          
70052 format (26x,"+0.123E+01  +0.987E+01" /                                    26x,"+0.123+001  +0.987+001" )                       
! T006*  TEST 6                                                          
           ivtnum = 6                                                   
           remrks = 'LEADING ZERO OPTIONAL'                             
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70060) chcvc                                       
70060 format (26x,e11.4,2x,e11.4)                                     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
           write (nuvi, 70061)                                          
70061 format (26x,"-0.2345E+02  -0.6879E+02" /                                  26x,"-0.2345+002  -0.6879+002" )                     
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! T007*  TEST 7                                                          
           ivtnum = 7                                                   
           remrks = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70070) a1c(1)                                      
70070 format (26x,e8.1,2x,e8.1)                                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
           write (nuvi, 70071)                                          
70071 format (26x,"+0.7E+03  +0.4E+03" /                                        26x,"+0.7+003  +0.4+003" )                           
! T008*  TEST 8                                                          
           ivtnum = 8                                                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70080) chdvc                                       
70080 format (26x,e14.7,2x,e14.7)                                     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
           write (nuvi, 70081)                                          
70081 format (26x,"+0.9876543E-04  +0.1357913E-04" /                            26x,"+0.9876543-004  +0.1357913-004" )               
! T009*  TEST 9                                                          
           ivtnum = 9                                                   
           write (nuvi, 70090) ivtnum                                   
70090 format (" ",2x,i3,4x,"INSPECT",32x,  "LEADING PLUS SIGN/ZERO OPTIONAL"/" ",48x,"FOR THE SECOND NUMBER" )                       
           write (nuvi, 80020)                                          
        write (nuvi, 70091) a2c(2,1)                                    
70091 format (26x,f5.2,2x,e11.4)                                      
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
           write (nuvi, 70092)                                          
70092 format (26x,"19.34  +0.2468E+02" /                                        26x,"19.34  +0.2468+002" )                           
! T010*  TEST 10                                                         
           ivtnum = 10                                                  
           write (nuvi, 70100) ivtnum                                   
70100 format (" ",2x,i3,4x,"INSPECT",32x,  "LEADING PLUS SIGN/ZERO OPTIONAL"/" ",48x,"FOR THE FIRST NUMBER" )                        
           write (nuvi, 80020)                                          
        write (nuvi, 70101) bvc                                         
70101 format (26x,e10.3,2x,f4.1)                                      
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
           write (nuvi, 70102)                                          
70102 format (26x,"+0.765E+02  87.6" /                                          26x,"+0.765+002  87.6" )                             
! T011*  TEST 11                                                         
           ivtnum = 11                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70110)                                          
70110 format (" ",16x,"COMPUTED:",23x,"3 COMPUTED LINES EXPECTED" )
        write (nuvi,70111) qavc, lm2c(1,2), ll1c(2)                     
70111 format (3(26x,f7.2,e11.4/))                                     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70112)                                          
70112 format (" ",16x,"CORRECT:  " ,22x,  "EACH RESULT LINE SHOULD MATCH "/" ",48x,"EITHER ONE OF THE 2 POSSIBLE  " /                       " ",48x,"ANSWERS BELOW" )                                  
           write (nuvi, 70113)                                          
70113 format (26x," +43.96+0.5407E+02" /                                        26x," +43.96+0.5407+002" )                           
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TESTS 12 THRU 17:                                             
! *****    FORMATTED READ AND WRITE STATEMENTS WITH            12.8.1    
! *****    DOUBLE PRECISION VARIABLES IN AN I/O LIST.          12.8.2    
! *****    D CONVERSION IS USED IN THE FORMAT STATEMENTS.      13.5.9.2.2
! *****    SOME D FORMAT DESCRIPTORS ARE REPEATED. (FIELD      13.3      
! *****    WIDTH ALWAYS INCLUDES 6 EXTRA POSITIONS TO          13.5.9    
! *****    PROVIDE FOR SIGN, DECIMAL POINT AND EXPONENT        13.5.9.2  
! *****    AND 1 POSITION FOR OPTIONAL DIGIT ZERO BEFORE                 
! *****    THE DECIMAL POINT)                                            
! *****                                                                  
02109 format (/8x, "D CONVERSION TEST" /)                               
      write (nuvi,02109)                                                
! *****  INPUT CARD  5                                                   
02110 format ( 2x, d8.1)                                                
      read (irvi,02110) dpavd                                           
! *****  INPUT CARDS  6, 7, 8                                            
02111 format ( 2d10.3, d14.7, d18.11/ 3d21.14/ 2d16.9)                  
      read (irvi,02111) mca3d(1,2,2), ac1d(2), bc2d(3,1), ac1d(1),           zzdvd, ac1d(3), dpbvd, mca3d(1,2,1), bc2d(1,2)               
! T012*  TEST 12                                                         
           ivtnum = 12                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi,70120) dpavd                                        
70120 format (26x,d8.1)                                               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
70121 format (" ",16x,"CORRECT:  " ,22x,  "3 CORRECT ANSWERS POSSIBLE")                                                              
           write (nuvi, 70122)                                          
70122 format (26x,"+0.1D+06"/26x,"+0.1E+06"/26x,"+0.1+006")        
! T013*  TEST 13                                                         
           ivtnum = 13                                                  
           remrks = 'LEADING ZERO OPTIONAL'                             
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70130)                                          
70130 format (" ",16x,"COMPUTED:",23x,"2 COMPUTED LINES EXPECTED" )
        write (nuvi, 70131) mca3d(1,2,2), ac1d(2)                       
70131 format (26x,d10.3 / 26x,d10.3)                                  
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70132)                                          
70132 format (" ",16x,"CORRECT:  " ,22x,  "EACH RESULT LINE SHOULD MATCH "/" ",48x,"ONE OF THE 3 POSSIBLE ANSWERS " /                       " ",48x,"BELOW")                                           
           write (nuvi, 70133)                                          
70133 format(26x,"-0.334D-04" /26x,"-0.334E-04" /26x,"-0.334-004" )
! T014*  TEST 14                                                         
           ivtnum = 14                                                  
           remrks = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70140) bc2d(3,1)                                   
70140 format (26x,d14.7)                                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70141)                                          
70141 format (26x,"+0.7657654D+00" /                                            26x,"+0.7657654E+00" /                                            26x,"+0.7657654+000" )                               
! T015*  TEST 15                                                         
           ivtnum = 15                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70150) ac1d(1)                                     
70150 format (26x,d18.11)                                             
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70151)                                          
70151 format (26x,"+0.12345678901D+10" /                                        26x,"+0.12345678901E+10" /                                        26x,"+0.12345678901+010" )                           
! T016*  TEST 16                                                         
           ivtnum = 16                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70110)                                          
        write (nuvi, 70160) zzdvd,ac1d(3),dpbvd                         
70160 format (26x,d21.14 / 26x,d21.14 / 26x,d21.14)                   
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70132)                                          
           write (nuvi, 70161)                                          
70161 format (26x,"+0.98765432109876D-01" /                                     26x,"+0.98765432109876E-01" /                                     26x,"+0.98765432109876-001" )                        
! T017*  TEST 17                                                         
           ivtnum = 17                                                  
           remrks = 'LEADING ZERO OPTIONAL'                             
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70130)                                          
        write (nuvi, 70170) mca3d(1,2,1), bc2d(1,2)                     
70170 format (26x,d16.9 /26x,d16.9)                                   
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70132)                                          
           write (nuvi, 70171)                                          
70171 format (26x,"-0.555555542D+03" /                                          26x,"-0.555555542E+03" /                                          26x,"-0.555555542+003" )                             
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TESTS 18 THRU 22:                                             
! *****    FORMATTED READ AND WRITE STATEMENTS WITH ARRAY          12.8.1
! *****    NAMES OF ALL TYPES IN AN I/O LIST. THE NUMBER OF        12.8.2
! *****    ITEMS IN THE LIST IS VARIABLE. SOME FIELD               13.3  
! *****    DESCRIPTORS ARE REPEATED.                                     
! *****                                                                  
02114 format (/8x, "TEST UNSUBSCRIPTED ARRAY NAMES IN I/O LISTS " /)    
      write (nuvi,02114)                                                
! *****  INPUT CARDS  9, 10                                              
02115 format(2x,8(f3.1),8f3.1/8(2(f3.1)))                               
      read (irvi,02115) b1c,b3c                                         
! *****  INPUT CARDS  11, 12                                             
02116 format(4(d9.2),4d9.2/2x,4(d9.2))                                  
      read (irvi,02116) a3d, a2d                                        
! *****  INPUT CARDS  13, 14                                             
02117 format (2x,4(2x),5(d9.2)/4(2(f3.1)))                              
      read (irvi,02117)  dpa1d, a2c                                     
! T018*  TEST 18                                                         
           ivtnum = 18                                                  
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 70130)                                          
        write (nuvi,70180) b1c                                          
70180 format (26x,8(f3.1) / 26x,8(f3.1))                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70181)                                          
70181 format (" ",16x,"CORRECT:  " ,22x,  "EACH RESULT LINE SHOULD EQUAL")                                                           
           write (nuvi, 70182)                                          
70182 format (26x, "9.91.19.92.29.93.39.94.4" )                    
! T019*  TEST 19                                                         
           ivtnum = 19                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70130)                                          
        write (nuvi, 70190) a3d                                         
70190 format (26x,4(d9.2) / 26x,4(d9.2))                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70132)                                          
           write (nuvi, 70191)                                          
70191 format (26x,"-0.99D+01-0.98D+01-0.97D+01-0.96D+01" /                      26x,"-0.99E+01-0.98E+01-0.97E+01-0.96E+01" /                      26x,"-0.99+001-0.98+001-0.97+001-0.96+001" )         
! T020*  TEST 20                                                         
           ivtnum = 20                                                  
           remrks = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi,70200) a2d                                          
70200 format (26x,4(d9.2))                                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70201)                                          
70201 format (26x,"+0.99D+01+0.98D+01+0.97D+01+0.96D+01" /                      26x,"+0.99E+01+0.98E+01+0.97E+01+0.96E+01" /                      26x,"+0.99+001+0.98+001+0.97+001+0.96+001" )         
! T021*  TEST 21                                                         
           ivtnum = 21                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70210)                                          
70210 format (" ",16x,"COMPUTED:",23x,"5 COMPUTED LINES EXPECTED" )
        write (nuvi,70211) dpa1d                                        
70211 format (5(26x,d11.2/))                                          
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70132)                                          
           write (nuvi, 70212)                                          
70212 format (26x,"  +0.99D+01" /                                               26x,"  +0.99E+01" /                                               26x,"  +0.99+001" )                                  
! T022*  TEST 22                                                         
           ivtnum = 22                                                  
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 70110)                                          
        write (nuvi,70220) a2c, b3c                                     
70220 format (26x,8(f3.1) / 26x,8(f3.1) / 26x,8(f3.1))                
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70181)                                          
           write (nuvi, 70221)                                          
70221 format (26x,"9.95.59.96.69.97.79.98.8" )                     
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TESTS 23 THRU 30:                                             
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS            13.5.9
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH. (D AND              
! *****    F DESCRIPTORS ARE TESTED.)                                    
! *****                                                                  
02121 format (/8x, "LEADING BLANK INSERTION TEST" /)                    
      write (nuvi,02121)                                                
! T023*  TEST 23                                                         
           ivtnum = 23                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
70230 format (" ",48x,"LEADING BLANKS ARE REQUIRED" )              
           write (nuvi, 80020)                                          
        write (nuvi, 70231) ac1d(3)                                     
70231 format (26x,d9.1)                                               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70232)                                          
70232 format (26x," +0.1D+00"/26x," +0.1E+00"/26x," +0.1+000")     
! T024*  TEST 24                                                         
           ivtnum = 24                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70240) zzdvd                                       
70240 format (26x,d10.1)                                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70241)                                          
70241 format(26x,"  +0.1D+00" /26x,"  +0.1E+00" /26x,"  +0.1+000" )
! T025*  TEST 25                                                         
           ivtnum = 25                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70250) zzdvd                                       
70250 format (26x,d11.1)                                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70251)                                          
70251 format (26x,"   +0.1D+00" /                                               26x,"   +0.1E+00" /                                               26x,"   +0.1+000" )                                  
! T026*  TEST 26                                                         
           ivtnum = 26                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70260) zzdvd                                       
70260 format (26x,d12.1)                                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70261)                                          
70261 format (26x,"    +0.1D+00" /                                              26x,"    +0.1E+00" /                                              26x,"    +0.1+000" )                                 
! T027*  TEST 27                                                         
           ivtnum = 27                                                  
           remrks = 'LEADING PLUS OPTIONAL'                             
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70270) chavc                                       
70270 format (26x,2(f5.1))                                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70271)                                          
70271 format (26x," +1.0 +5.5" )                                   
! T028*  TEST 28                                                         
           ivtnum = 28                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70280) b3c(1,1,1)                                  
70280 format (26x,2(f6.1))                                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70281)                                          
70281 format (26x,"  +9.9  +5.5" )                                 
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! T029*  TEST 29                                                         
           ivtnum = 29                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70290) b3c(1,1,1)                                  
70290 format (26x,2(f7.1))                                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70291)                                          
70291 format (26x,"   +9.9   +5.5" )                               
! T030*  TEST 30                                                         
           ivtnum = 30                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70230)                                          
           write (nuvi, 80020)                                          
        write (nuvi, 70300) chavc                                       
70300 format (26x,2(f8.1))                                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70301)                                          
70301 format (26x,"    +1.0    +5.5" )                             
! *****    TESTS 31 THRU 32:                                             
! *****    FORMATS WITH G CONVERSIONS USING COMPLEX DATA       13.5.9.2.3
! *****                                                                  
! *****  INPUT CARD   15                                                 
02123 format(  3(g11.4), 3g11.4)                                        
      read (irvi,02123) ll1c(1), ll1c(2), ll1c(3)                       
02124 format (/8x,"G CONVERSION TEST" /)                                
      write (nuvi, 02124)                                               
! T031*  TEST 31                                                         
           ivtnum = 31                                                  
           remrks = 'LEADING PLUS SIGN/ZERO OPTIONAL'                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 70130)                                          
        write (nuvi, 70310) ll1c(1), ll1c(2), ll1c(3)                   
70310 format (26x,g14.4,4x,2g11.4 / 26x,g14.4,4x,2g11.4)              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70311)                                          
70311 format(/" ",16x,"CORRECT:  " ,22x,  "CORRESPONDING LINES MUST MATCH "   ,/" ",48x,"EITHER OF THE FOLLOWING TWO    "  ,                     /" ",48x,"CORRECT ANSWERS                "  /)        
           write (nuvi, 70312)                                          
70312 format (26x,"   +0.1235E+05     +1235.     +123.5" /                      26x,"    +12.35         +1.235    +0.1235" //                     26x,"   +0.1235+005     +1235.     +123.5" /                      26x,"    +12.35         +1.235    +0.1235" )         
! *****  INPUT CARD   16                                                 
! *****    TESTS 32 THRU 34:                                             
! *****    ON READ, BUT NOT ON WRITE                                     
! *****    SCALE FACTOR APPLIED TO F,E,D,G DESCRIPTORS           13.7.5.1
! *****                                                                  
02126 format(2pf8.3,-2pe9.4,f9.4,0pg9.4,d9.4,-2pe9.4,f9.4,d9.4,2pg9.4)  
      read(irvi,02126)bvc, chavc, bc2d(1,4), a1c(1), bc2d(2,1), dpavd   
02127 format(/8x, "SCALE FACTOR ON READ" /)                             
      write (nuvi, 02127)                                               
! T032*  TEST 32                                                         
           ivtnum = 32                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70320) bvc,chavc                                   
70320 format (26x,f12.4,e12.4,f12.2,f12.3)                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70051)                                          
           write (nuvi, 70321)                                          
70321 format (30x,"+98.7654 +0.9877E+04  +987654.00    +987.654" /              30x,"+98.7654 +0.9877+004  +987654.00    +987.654" ) 
! T033*  TEST 33                                                         
           ivtnum = 33                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70330) bc2d(1,4), a1c(1)                           
70330 format (26x,d12.4,e12.4,f12.3)                                  
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70331)                                          
70331 format (26x," +0.8648D-02 +0.8648E+04   +8647.860" /                      26x," +0.8648E-02 +0.8648E+04   +8647.860" /                      26x," +0.8648-002 +0.8648+004   +8647.860" )         
70332 format (" ",48x,"   OR")                                     
           write (nuvi,70332)                                           
70333 format (26x," +0.8648D-02 +0.8648E+04   +8647.859" /                      26x," +0.8648E-02 +0.8648E+04   +8647.859" /                      26x," +0.8648-002 +0.8648+004   +8647.859" )         
           write (nuvi,70333)                                           
! T034*  TEST 34                                                         
           ivtnum = 34                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70340) bc2d(2,1), dpavd                            
70340 format (26x,d12.4,g16.4)                                        
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70341)                                          
70341 format (26x," +0.8658D+04      +98.77" /                                  26x," +0.8658E+04      +98.77" /                                  26x," +0.8658+004      +98.77" )                     
! *****  ADVANCE TO TOP-OF-PAGE AND WRITE HEADER                         
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TESTS 35 AND 36:                                              
! *****    SCALE FACTOR APPLIED TO  F, E, D, G  DESCRIPTORS              
! *****    ON WRITE, BUT, NOT ON READ                                    
! *****                                                                  
! *****  INPUT CARD   17                                                 
02128 format(f8.2,e9.4,f9.2,g9.3,d9.0,e9.4,f9.4,d9.2,g9.4)              
      read(irvi,02128) chbvc, a2c(2,1), ac1d(4), chcvc, ac1d(5), dpbvd  
02129 format(/8x, "SCALE FACTOR ON WRITE" /)                            
      write (nuvi, 02129)                                               
! T035*  TEST 35                                                         
           ivtnum = 35                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write (nuvi, 70350) chbvc, a2c(2,1), ac1d(4)                    
70350 format (26x,2pf12.2,-2pe12.4,f12.4,1pg12.2,d12.4)               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70351)                                          
70351 format (28x, "   +987.66 +0.0099E+06    +98.7654   +9.88E+02 +8.6479D+02"/28x, "   +987.66 +0.0099E+06    +98.7654   +9.88E+02 +8.6479E+02"/28x, "   +987.66 +0.0099+006    +98.7654   +9.88+002 +8.6479+002")                                                     
70352 format (" ",48x,"   OR")                                     
           write (nuvi,70352)                                           
70353 format (28x, "   +987.66 +0.0099E+06    +98.76539  +9.88E+02 +8.6479D+02"/28x, "   +987.66 +0.0099E+06    +98.76539  +9.88E+02 +8.6479E+02"/28x, "   +987.66 +0.0099+006    +98.76539  +9.88+002 +8.6479+002")                                                     
           write (nuvi,70353)                                           
! T036*  TEST 36                                                         
           ivtnum = 36                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write(nuvi,70360) chcvc, ac1d(5), dpbvd                         
70360 format (26x,-2pe12.4,2pf12.2,1pd12.4,2pg16.4)                   
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70121)                                          
           write (nuvi, 70361)                                          
70361 format(27x, "+0.0086E+06    +8647.86 +8.6579D+03      +9877."           /27x, "+0.0086E+06    +8647.86 +8.6579E+03      +9877."          /27x,"+0.0086+006    +8647.86 +8.6579+003      +9877." )
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
! *****    END OF TEST SEGMENT 21                                        
      stop                                                              
      end program fm900
