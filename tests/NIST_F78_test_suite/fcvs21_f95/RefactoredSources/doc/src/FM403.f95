      program fm403
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM403               FMTRW - (020)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REFS
! *****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.2
! *****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  
! *****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  
! *****    PROGRAM SEGMENTS FOR INTEGER, REAL, AND LOGICAL               
! *****    DATA TYPES.                                                   
! *****  RESTRICTIONS OBSERVED                                   12.8.2  
! *****  *  ALL FORMAT STATEMENTS ARE LABELED                    13.1.1  
! *****  *  H DESCRIPTOR ARE NEVER REPEATED                      13.2.1  
! *****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               
! *****     W IS EQUAL TO OR GREATER THAN D                              
! *****  *  FIELD WIDTH IS NEVER ZERO                            13.2.1  
! *****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    
! *****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           
! *****     IN THE FORMAT SPECIFICATION                                  
! *****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS     13.3    
! *****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                    13.5.9  
! *****  *  FIELD WIDTH NEVER EXCEEDED BY OUTPUT                 13.5.9  
! *****  *  FOR I EDITING, EXTERNAL INPUT FIELDS ARE             13.5.9.1
! *****     INTEGER CONSTANTS                                            
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
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 27 CARD IMAGES IN COL. 1 - 80 
! OL.      1----------------------------------------------------------61 
! ARD  1     999                                                         
! ARD  2     555554444                                                   
! ARD  3     666  777777  8                                              
! ARD  4     333333111112222222255555444444444444                        
! ARD  5     7.7123456.7                                                 
! ARD  6     8.889.9997.123456                                           
! ARD  7     5.44446.5555533.133.133.133.1444.1                          
! ARD  8     5555.15555.1  66666.166666.1  44.22                         
! ARD  9     2.12.12.12.12.1666.3334.3334.3334.333                       
! ARD 10   -0.1E+01+0.22E-01 0.333E+02 0.4444E+03-0.55555E-03+0.666666E+ 
! OL.    62------------77                                                
! ARD 10 00+0.9876543E+12                                                
! OL.      1----------------------------------------------------------61 
! ARD 11   TABC                                                          
! ARD 12   FDEFFGHIT*+T1F/).TRUE..FALSE.                                 
! ARD 13     -9.9-9.9-9.9-9.9                                            
! ARD 14   9999999999                                                    
! ARD 15   .9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9.9        
! ARD 16   TFTFTFTFTF                                                    
! ARD 17     99999999                                                    
! ARD 18   9999999999999999TFFT9.99.99.99.99.9                           
! ARD 19        T   F         T    F                                     
! ARD 20     3334444.555550                                              
! ARD 21    9876.5498.7654E2 9876.54   987.654         86.4786E286.4786  
! ARD 22    9.8765698.7654E2  9876.54  987.654         86.4786E286.4786  
! ARD 23   122333544888611222                                            
! ARD 24   455666233444966111                                            
! ARD 25   788999377555899777                                            
! ARD 26   11112 334 559 880 11                                          
! ARD 27   6 778 995 441 222 00                                          
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 020                        
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
      integer :: jacvi
      integer :: kbcvi
      integer :: ihdvi
      integer :: lccvi
      integer :: iedvi
      integer :: igdvi
      integer :: kgvi
      integer :: mdcvi
      integer :: mrrvi
      integer :: necvi
      real :: acvs
      real :: cmavs
      real :: bcvs
      real :: cmbvs
      real :: cmcvs
      real :: ffcvs
      real :: ggcvs
      real :: hhcvs
      real :: ccvs
      real :: dcvs
      real :: avs
      real :: bvs
      real :: cvs
      real :: dvs
      real, dimension(1:33) :: ep1s
      real, dimension(1:5) :: cma1s
      integer, dimension(1:5) :: iac1i
      integer, dimension(1:2,1:7) :: iac2i
      integer, dimension(1:5) :: mca1i
      real, dimension(1:5) :: a1s
      real, dimension(1:2,1:2) :: a2s
      real, dimension(1:3,1:3,1:3) :: a3s
      real, dimension(1:25) :: ac1s
      real, dimension(1:5,1:6) :: ac2s
      integer, dimension(1:2,1:2) :: i2i
      integer, dimension(1:2,1:2,1:2) :: i3i
      integer, dimension(1:2,1:3,1:3) :: mca3i
      logical, dimension(1:7) :: mca1b
      logical, dimension(1:2) :: a1b
      logical, dimension(1:2,1:2) :: a2b
      logical, dimension(1:2,1:2,1:2) :: a3b
      logical :: avb
      logical :: cvb
      logical :: dvb
      logical :: mcbvb
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
! *****  I N P U T - O U T P U T  ASSIGNMENT STATEMENTS                  
      irvi = i01                                                        
      nuvi = i02                                                        
      ivtotl = 59                                                       
      zprog = 'FM403'                                                   
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
2000  format ( // 2x,"FMTRW - (020) FORMATTED DATA TRANSFER" //2x,      "SUBSET REFS - 12.9.5.2   13.3   13.5.9   " )                     
      write (nuvi,2000)                                                 
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! T001*  TEST 1 -  FORMAT WITH DIGITS 0-9 IN H FIELDS                    
      ivtnum = 1                                                        
      remrks = '2 COMPUTED LINES EXPECTED'                              
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70010)                                                 
70010 format (25x,"  10101010101010101010" ,"999999999","88888888"/27x, "7777777","666666","55555","4444","333","22","1")                 
      ivinsp = ivinsp + 1                                               
      write (i02,70011)                                                 
70011 format(" ",16x,"CORRECT:  " ,22x,"CORRESPONDING LINE MUST MATCH" )
      write (i02,70012)                                                 
70012 format (25x,'  1010101010101010101099999999988888888',                    /25x,'  7777777666666555554444333221         ')           
      ivtnum = 2                                                        
! T002*  TEST 2 -  FORMAT  CONTAINING ALL LETTERS (A-Z) IN H FIELDS AND  
! *****            A VARIABLE NUMBER OF BLANKS IN H AND X FIELDS         
      remrks = '9 COMPUTED LINES EXPECTED'                              
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70020)                                                 
70020 format(27x,"AAA",5x,"     ","BBB",10x,"CCC"/28x,"DDD",9x,"EEE"    ,"         ","FFF"/29x,"GGG",8x,"HHH","        ","III"/27x,"   "  ,"JJJ","       ","KKK",7x,"LLL"/31x,"MMM",6x,"NNN","      ","OOO"/ 32x,"PPP","     ","QQQ",5x,"RRR"/33x,"SSS",4x,"TTT","    ","UUU"/                                                             27x,  "       VVV   ","WWW",3x,"XXX"/37x,"YYY",3x,"ZZZ")               
      ivinsp = ivinsp + 1                                               
      write (i02,70011)                                                 
      write (i02,70021)                                                 
70021 format (27x,'AAA          BBB          CCC',                             /27x,' DDD         EEE         FFF ',                             /27x,'  GGG        HHH        III  ',                             /27x,'   JJJ       KKK       LLL   ',                             /27x,'    MMM      NNN      OOO    ',                             /27x,'     PPP     QQQ     RRR     ',                             /27x,'      SSS    TTT    UUU      ',                             /27x,'       VVV   WWW   XXX       ',                             /27x,'          YYY   ZZZ          ')                      
! T003*  TEST 3 - FORMAT CONTAINING H FIELD WITH ALL POSSIBLE            
! *****           SPECIAL CHARACTERS                                     
      ivtnum = 3                                                        
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70030)                                                 
70030 format (25x,"  = + - * / ( ) , . '" )                             
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70031)                                                 
70031 format (25x,  '  = + - * / ( ) , . ''')                           
! *****  FORMAT  TO TEST VERTICAL SPACING                                
! *****                                                       12.9.5.2.3 
! T004*  TEST 4 - FORMAT STATEMENT ENDING WITH ONE SLASH DESCRIPTOR      
      ivtnum = 4                                                        
      remrks = 'SLASH DESCRIPTOR'                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,70040)                                                 
70040 format(15x,                            "  FORMAT( '   SKIP 1 LINE'  /)" /)                                                          
      ivinsp = ivinsp + 1                                               
      write (i02,70041)                                                 
70041 format(17x,"ONE BLANK LINE SHOULD APPEAR ABOVE" )                 
!   ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             
      write (i02,90002)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! T005*  TEST 5 - FORMAT STATEMENT ENDING WITH TWO SLASH DESCRIPTORS     
      ivtnum = 5                                                        
      write (i02,80004) ivtnum                                          
      write (i02,70050)                                                 
70050 format(15x,"  FORMAT('   SKIP 2 LINES'   //)"  //)                
      ivinsp = ivinsp + 1                                               
      write (i02,70051)                                                 
70051 format(17x,"TWO BLANK LINES SHOULD APPEAR ABOVE" )                
! T006*  TEST 6 - FORMAT STATEMENT ENDING WITH THREE SLASH DESCRIPTORS   
      ivtnum = 6                                                        
      write (i02,80004) ivtnum                                          
      write (i02,70060)                                                 
70060 format(15x,"  FORMAT('   SKIP 3 LINES '  ///)"  ///)              
      ivinsp = ivinsp + 1                                               
      write (i02,70061)                                                 
70061 format(17x,"THREE BLANK LINES SHOULD APPEAR ABOVE" )              
! T007*  TEST 7 - FORMAT STATEMENT CONTAINING IMBEDDED SLASH DESCRIPTORS 
      ivtnum = 7                                                        
      remrks = 'IMBEDDED SLASHES'                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,70070)                                                 
70070 format( 17x,"1 BLANK LINE SHOULD APPEAR BELOW"  //                        17x,"2 BLANK LINES SHOULD APPEAR BELOW" ///                       17x,"3 BLANK LINES SHOULD APPEAR BELOW" / 3(/),                   17x,"0 BLANK LINES SHOULD APPEAR BELOW" /                         17x,"END IMBEDDED SLASHES TEST        " )                 
      ivinsp = ivinsp + 1                                               
! T008*  TEST 8 - FORMS CONTROL USING '0' FOR DOUBLE SPACING             
      ivtnum = 8                                                        
      remrks = 'DOUBLE SPACE'                                           
      write (i02,80004) ivtnum, remrks                                  
      write (i02,70080)                                                 
70080 format( 17x,"1 BLANK LINE SHOULD APPEAR BELOW " / "0",                    17x,"END DOUBLE SPACE TEST            " )                 
      ivinsp = ivinsp + 1                                               
! T009*  TEST 9 - FORMS CONTROL USING '+' FOR OVERPRINTING               
      ivtnum = 9                                                        
      remrks = 'OVERPRINT'                                              
      write (i02,80004) ivtnum, remrks                                  
      write (i02,70090)                                                 
70090 format(/17x,"!FIRST PRINT LINE!     OVER" ,/"+",                          17x,"                    P R I N T  !SECOND PRINT LINE!" )
      ivinsp = ivinsp + 1                                               
! T010*  TEST 10 - FORMS CONTROL USING '1' FOR PAGE EJECTION             
      ivtnum = 10                                                       
      remrks = 'PAGE ADVANCE'                                           
      write (i02,80004) ivtnum, remrks                                  
      write (i02,70100)                                                 
70100 format(/17x,"THIS SHOULD BE THE LAST LINE ON THIS PAGE" /,        "1                NEW PAGE:  END OF VERTICAL SPACING TESTS" )     
      ivinsp = ivinsp + 1                                               
!   WRITE PAGE HEADERS                                                   
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! *****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH INTEGER  12.8.1   
! *****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST. (THE    12.8.2   
! *****    NUMBER OF ITEMS IN THE LIST IS VARIABLE.) SOME       13.2.1   
! *****    FORMAT STATEMENTS CONTAIN REPEATED FIELDS.                    
! *****    FORMATS CONTAIN I EDIT DESCRIPTORS.                  13.5.9.1 
! *****    FIELD WIDTHS ARE FROM 1 TO 5 DIGITS.                 13.3     
! *****  INPUT CARD   1                                                  
2009  format (2x,i3)                                                    
      read (irvi,2009) jacvi                                            
! *****  INPUT CARD   2                                                  
2010  format (1x,i5,1x,i4)                                              
      read (irvi,2010) kbcvi, iac1i(1)                                  
! *****  INPUT CARD   3                                                  
2011  format (2x,i3,2x,3i2,2x,i1)                                       
      read (irvi,2011) iac2i(1,2), lccvi, iac1i(5), ihdvi, mca3i(1,2,3) 
! *****  INPUT CARD   4                                                  
2012  format (2x,2(i3),1(i5), 4i2 ,5i1,3 i4 )                           
      read (irvi,2012) mdcvi, iac2i(2,2), iac1i(4), necvi, iac1i(3),         iac2i(2,3), iac2i(2,1), mrrvi, igdvi, kgvi, iedvi, iac2i(1,1)     ,iac1i(2), iac2i(2,7), mca3i(2,1,3)                          
! T011*  TEST 11 - I CONVERSION                                          
      ivtnum = 11                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70110) jacvi                                           
70110 format (25x,i5)                                                   
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70111)                                                 
70111 format (25x,"  999")                                              
! T012*  TEST 12 - I CONVERSION                                          
      ivtnum = 12                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70120) kbcvi, iac1i(1)                                 
70120 format (26x,i5,1x,i4)                                             
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70121)                                                 
70121 format (26x," 5555 4444" )                                        
! T013*  TEST 13 - I CONVERSION                                          
      ivtnum = 13                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70130) iac2i(1,2),lccvi, iac1i(5), ihdvi, mca3i(1,2,3) 
70130 format (27x,i3,2x,3i2,2x,i1)                                      
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70131)                                                 
70131 format (27x,"666  777777  8" )                                    
! T014*  TEST 14 - I CONVERSION                                          
      ivtnum = 14                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70140)                                                 
      write (i02,70140) mdcvi, iac2i(2,2), iac1i(4), necvi, iac1i(3),        iac2i(2,3), iac2i(2,1), mrrvi, igdvi, kgvi, iedvi, iac2i(1,1)     ,iac1i(2), iac2i(2,7), mca3i(2,1,3)                          
70140 format (27x,2(i3),1(i5), 4i2 ,5i1,3 i4 )                          
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70141)                                                 
70141 format (27x,"333333111112222222255555444444444444" )              
! *****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH REAL       12.8.1 
! *****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST.(THE       12.8.2 
! *****    NUMBER OF ITEMS IN THE LIST IS VARIABLE.) ONLY F     13.5.9.2 
! *****    EDIT DESCRIPTORS ARE USED IN THE FORMAT            13.5.9.2.1 
! *****    STATEMENTS.  SOME F EDIT DESCRIPTORS ARE REPEATED.       13.3 
! *****    FIELD WIDTH ALWAYS CONTAINS 1 POSITION FOR DECIMAL PT.        
! *****    FIELD WIDTH IS FROM 1 TO 7 DIGITS. PLACEMENT OF               
! *****    DECIMAL POINT IS VARIABLE. SOME F FIELDS ARE                  
! *****    REPEATED                                                      
! *****  INPUT CARD   5                                                  
2018  format (2x,f3.1,f8.1)                                             
      read (irvi,2018) acvs, cmavs                                      
! *****  INPUT CARD   6                                                  
2019  format(2x,f4.2,f5.3,f8.6)                                         
      read (irvi,2019) a1s(2), bcvs, cmbvs                              
! *****  INPUT CARD   7                                                  
2020  format (2x,f6.4,f7.5,4f4.1,f5.1)                                  
      read (irvi,2020) hhcvs, cmcvs, ggcvs, ffcvs, a1s(1), ac1s(25),        ac2s(4,1)                                                     
! *****  INPUT CARD   8                                                  
2021  format (2x,2(f6.1),2x,2f7.1  ,2x,f5.2)                            
      read (irvi,2021) ac1s(18), ac1s(7), ac2s(4,4) , ac1s(8), ac1s(10) 
! *****  INPUT CARD   9                                                  
2022  format (2x,5(f3.1),f7.3,3f5.3  )                                  
      read (irvi,2022) ac2s(3,3) , ac2s(5,1), ccvs, ac1s(12), dcvs,         ac1s(13), ac1s(5), a3s(1,1,2), ac2s(3,5)                      
! T015*  TEST 15 - F CONVERSION                                          
      ivtnum = 15                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70150) acvs, cmavs                                     
70150 format (27x,f3.1,f8.1)                                            
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70151)                                                 
70151 format (27x,"7.7123456.7" )                                       
! T016*  TEST 16 - F CONVERSION                                          
      ivtnum = 16                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70160) a1s(2), bcvs, cmbvs                             
70160 format(27x,f4.2,f5.3,f8.6)                                        
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70161)                                                 
70161 format (27x,"8.889.9997.123456" )                                 
! T017*  TEST 17 - F CONVERSION                                          
      ivtnum = 17                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70170) hhcvs,cmcvs, ggcvs, ffcvs, a1s(1), ac1s(25)         ,ac2s(4,1)                                                    
70170 format (27x,f6.4,f7.5,4f4.1,f5.1)                                 
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70171)                                                 
70171 format (27x,"5.44446.5555533.133.133.133.1444.1" )                
! T018*  TEST 18 - F CONVERSION                                          
      ivtnum = 18                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70180) ac1s(18),ac1s(7), ac2s(4,4) , ac1s(8), ac1s(10) 
70180 format (27x,2(f6.1),2x,2f7.1  ,2x,f5.2)                           
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70181)                                                 
70181 format (27x,"5555.15555.1  66666.166666.1  44.22"  )              
! T019*  TEST 19 - F CONVERSION                                          
      ivtnum = 19                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70190) ac2s(3,3) , ac2s(5,1), ccvs, ac1s(12), dcvs,        ac1s(13), ac1s(5),  a3s(1,1,2), ac2s(3,5)                     
70190 format (27x,5(f3.1),f7.3,3f5.3  )                                
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70191)                                                 
70191 format (27x,"2.12.12.12.12.1666.3334.3334.3334.333" )             
! *****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH REAL       12.8.1 
! *****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST.           12.8.2 
! *****    E EDIT DESCRIPTORS ARE USED IN THE FORMAT            13.5.9.2 
! *****    STATEMENTS. SOME E EDIT DESCRIPTORS ARE REPEATED   13.5.9.2.2 
! *****    (FIELD WIDTH ALWAYS INCLUDES 6 EXTRA POSITIONS                
! *****    TO PROVIDE FOR SIGN, DECIMAL POINT AND EXPONENT.       13.5.9 
! *****    PROVISION IS ALWAYS MADE FOR THE DIGIT ZERO        13.5.9.2.1 
! *****    BEFORE THE DECIMAL POINT)                                     
! *****    THE NUMBER OF DECIMAL PLACES VARIES FROM 1                    
! *****    TO 7 DIGITS.                                                  
! *****  INPUT CARD  10                                                  
2029  format (e8.1,e9.2,e10.3,e11.4,e12.5,e13.6,e14.7)                  
      read (irvi,2029) avs, bvs, ep1s(5), ac2s(1,5), cvs, ac2s(5,4),          a3s(2,1,2)                                                  
! T020*  TEST 20 - E CONVERSION                                          
      ivtnum = 20                                                       
      remrks = 'LEADING PLUS SIGN/ZERO OPTIONAL'                        
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70200) avs, bvs                                        
70200 format (27x,e8.1,2x,e9.2)                                         
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
70201 format (" ",16x,"CORRECT:  " ,22x,"2 CORRECT ANSWERS POSSIBLE" )  
      write (i02,70202)                                                 
70202 format (27x,"-0.1E+01  +0.22E-01" /                                       27x,"-0.1+001  +0.22-001" )                               
!   ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             
      write (i02,90002)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! T021*  TEST 21 - E CONVERSION                                          
      ivtnum = 21                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70210) ep1s(5), ac2s(1,5)                              
70210 format (27x,e10.3,2x,e11.4)                                       
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70211)                                                 
70211 format (27x,"+0.333E+02  +0.4444E+03" /                                   27x,"+0.333+002  +0.4444+003" )                           
! T022*  TEST 22 - E CONVERSION                                          
      ivtnum = 22                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70220) cvs, ac2s(5,4)                                  
70220 format (27x,e12.5,2x,e13.6)                                       
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70221)                                                 
70221 format (27x,"-0.55555E-03  +0.666666E+00" /                               27x,"-0.55555-003  +0.666666+000" )                       
! T023*  TEST 23 - E CONVERSION                                          
      ivtnum = 23                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70230) a3s(2,1,2)                                      
70230 format (27x,e14.7)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70231)                                                 
70231 format (27x,"+0.9876543E+12" /                                            27x,"+0.9876543+012" )                                    
! *****  INPUT CARD   11                                                 
! *****    FORMATTED DATA TRANSFER I/O STATEMENTS WITH LOGICAL   12.8.2  
! *****    VARIABLES AND ARRAY ELEMENTS IN AN I/O LIST           13.5.10 
! *****    SOME L EDIT DESCRIPTORS ARE REPEATED.                         
! *****    L EDIT DESCRIPTORS ARE USED IN THE FORMAT STATEMENTS   13.2.1 
2033  format (l4)                                                       
      read (irvi,2033) a2b(2,1)                                         
! *****  INPUT CARD   12                                                 
2034  format ( 2l4, l3, l2, l3, l6, l7)                                 
      read (irvi,2034) mca1b(1), mcbvb, a2b(1,1), a3b(1,1,1), cvb,           dvb, a3b(1,2,1)                                              
! T024*  TEST 24 - L CONVERSION                                          
      ivtnum = 24                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70240) a2b(2,1), mca1b(1), mcbvb, a2b(1,1), a3b(1,1,1),     cvb, dvb, a3b(1,2,1)                                         
70240 format (24x, 3(l4), l3, l2, l3,                                     2(l1))                                                          
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70241)                                                 
70241 format (27x,"T   F   F  T T  FTF" )                               
! *****        FORMATTED DATA TRANSFER STATEMENTS WITH ARRAY    12.8.2   
! *****        NAMES OF SEVERAL TYPES IN AN I/O LIST. THE       12.9.5.2 
! *****        NUMBER OF ITEMS IN THE LIST IS VARIABLE. SOME    13.2.1   
! *****        EDIT DESCRIPTORS ARE REPEATED.                            
! *****        OPTIONAL COMMA BEFORE AND AFTER A SLASH                   
! *****  INPUT CARDS  13, 14                                             
2037  format(2x,4(f4.1)/5(i2))                                          
      read (irvi,2037) a2s, mca1i                                       
! *****  INPUT CARDS  15, 16                                             
2038  format(27(f2.1)/5(l1),5l1)                                        
      read (irvi,2038)  a3s, a1b, a3b                                   
! *****  INPUT CARDS  17, 18                                             
2039  format (2x,2(i2,i2),/,2(2(i2,i2)),2(l1,l1),2(f3.1,f3.1),f3.1)     
      read (irvi,2039) i2i, i3i, a2b, cma1s                             
! T025*  TEST 25 THRU 28 - UNSUBSCRIPTED ARRAY NAME IN I/O LISTS         
      write (i02,70250) a2s, mca1i, a3s, a1b                            
70250 format ("    25    INSPECT" /" ",16x,"COMPUTED: " /27x,4(f4.1)/   " ",16x,"CORRECT:  " /27x,"-9.9-9.9-9.9-9.9" / "    26    INSPECT"/" ",16x,"COMPUTED: " /27x,5(i2)/" ",16x,"CORRECT:  " /27x,       "9999999999" /"    27    INSPECT" ,32x,"LEADING PLUS SIGN/ZERO " ,"OPTIONAL"/" ",16x,"COMPUTED: " ,22x,"3 COMPUTED LINES EXPECTED"  /27x,3(3(f4.1))/27x,2(2(f4.1,f4.1)),f4.1/27x,9f4.1/" ",16x,       "CORRECT:  " ,22x,"EACH RESULT LINE SHOULD EQUAL" /                       27x," 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9" /                      "    28    INSPECT" /" ",16x,"COMPUTED: " /27x,2l1/               " ",16x,"CORRECT:  " /27x,"TF")                           
      ivinsp = ivinsp + 4                                               
!   ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             
      write (i02,90002)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! T029*  TEST 29 THRU 33 - UNSUBSCRIPTED ARRAY NAMES IN I/O LISTS        
      write (i02,70290) a3b, i2i, i3i, a2b, cma1s                       
70290 format ("    29    INSPECT" /" ",16x,"COMPUTED: " /27x,8(l1)/" ", 16x,"CORRECT:  " /27x,"TFTFTFTF"/"    30    INSPECT" /" ",16x,    "COMPUTED: " /27x,4(i2)/" ",16x,"CORRECT:  " /27x,"99999999"/      "    31    INSPECT"/" ",16x,"COMPUTED: " /27x,8(i2)/" ",16x,     "CORRECT:  " /27x,"9999999999999999" /"    32    INSPECT" /" ",   16x,"COMPUTED: " /27x,4(l1)/" ",16x,"CORRECT:  " /27x,"TFFT"/      "    33    INSPECT"/" ",16x,"COMPUTED: " /27x,5(f3.1)/                   " ",16x,"CORRECT:  " ,/,                                          27x,"9.99.99.99.99.9" )                                   
      ivinsp = ivinsp + 5                                               
! T034*  TEST 34 - FORMATTED DATA TRANSFER STATEMENT TO TEST     13.5.10 
! *****            THAT OPTIONAL BLANKS MAY PRECEDE A LOGICAL INPUT FIELD
! *****  INPUT CARD   19                                                 
70340 format ( l6, l4, l10, l5)                                         
      read (irvi,70340) avb, mca1b(2), a2b(1,2), a3b(2,1,2)             
      ivtnum = 34                                                       
      remrks = 'LEADING BLANKS ARE REQUIRED'                            
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70341) avb, mca1b(2), a2b(1,2), a3b(2,1,2)             
70341 format (27x,l6, l4, l10, l5)                                      
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70342)                                                 
70342 format (27x,"     T   F         T    F" )                         
! T035*  TEST 35                                                         
! *****    FORMATTED DATA TRANSFER TO TEST F EDIT DESCRIPTORS 13.5.9.2.1 
! *****    WHERE D IS EQUAL TO ZERO                                      
! *****  INPUT CARD   20                                                 
70350 format (2x, f3.0, f5.0, f5.5, f1.0)                               
      read (irvi,70350) avs, bvs, cvs, dvs                              
      ivtnum = 35                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70351) avs, bvs                                        
70351 format (27x,f4.0,4x,f5.0)                                         
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70352)                                                 
70352 format (27x,"333.",4x,"4444.")                                    
! T036*  TEST 36                                                         
! *****    FORMATTED DATA TRANSFER TO TEST F EDIT DESCRIPTORS 13.5.9.2.1 
! *****    WHERE W EQUALS D+1 AND WHERE D IS EQUAL TO ZERO        13.2.1 
      ivtnum = 36                                                       
      write (i02,80004) ivtnum                                          
      write (i02,80020)                                                 
      write (i02,70360) cvs, dvs                                        
70360 format (27x,f6.5,2x,f2.0)                                         
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70361)                                                 
70361 format (27x,".55555  0." )                                        
! T037*  TEST 37                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    I EDIT DESCRIPTORS                                            
      ivtnum = 37                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70370) mca3i(1,2,3)                                    
70370 format (27x,i3)                                                   
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70371)                                                 
70371 format (27x,"  8")                                                
! T038*  TEST 38                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    I EDIT DESCRIPTORS                                            
      ivtnum = 38                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70380) iac1i(3)                                        
70380 format (27x,i4)                                                   
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70381)                                                 
70381 format (27x,"  22")                                               
! T039*  TEST 39                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    I EDIT DESCRIPTORS                                            
      ivtnum = 39                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70390) necvi                                           
70390 format (27x,i5)                                                   
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70391)                                                 
70391 format (27x,"   22")                                              
!   ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             
      write (i02,90002)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! T040*  TEST 40                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    I EDIT DESCRIPTORS                                            
      ivtnum = 40                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70400) iac1i(3)                                        
70400 format (27x,i6)                                                   
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70401)                                                 
70401 format (27x,"    22")                                             
! T041*  TEST 41                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    I EDIT DESCRIPTORS                                            
      ivtnum = 41                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70410) iac2i(2,3)                                      
70410 format (27x,i7)                                                   
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70411)                                                 
70411 format (27x,"     22")                                            
! T042*  TEST 42                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    F EDIT DESCRIPTORS                                            
      ivtnum = 42                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70420) acvs                                            
70420 format (27x,f5.1)                                                 
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70421)                                                 
70421 format (27x,"  7.7")                                              
! T043*  TEST 43                                                         
! T043*  TEST 43 - FORMATTED WRITES TO TEST THAT LEADING BLANKS   13.5.9 
! *****            ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT      
! *****            PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE      
! *****            F EDIT DESCRIPTORS                                    
      ivtnum = 43                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70430) a1s(2)                                          
70430 format (27x,f7.2)                                                 
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70431)                                                 
70431 format (27x,"   8.88")                                            
! T044*  TEST 44 - FORMATTED WRITES TO TEST THAT LEADING BLANKS   13.5.9 
! *****            ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT      
! *****            PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE      
! *****            F EDIT DESCRIPTORS                                    
      ivtnum = 44                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70440) bcvs                                            
70440 format (27x,f9.3)                                                 
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70441)                                                 
70441 format (27x,"    9.999")                                          
! T045*  TEST 45                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    F EDIT DESCRIPTORS                                            
      ivtnum = 45                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70450) hhcvs                                           
70450 format (27x,f11.4)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70451)                                                 
70451 format (27x,"     5.4444" )                                       
! T046*  TEST 46                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    F EDIT DESCRIPTORS                                            
      ivtnum = 46                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70460) cmcvs                                           
70460 format (27x,f13.5)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70461)                                                 
70461 format (27x,"      6.55555" )                                     
! T047*  TEST 47                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    F EDIT DESCRIPTORS                                            
      ivtnum = 47                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70470) cmbvs                                           
70470 format (27x,f15.6)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,80022)                                                 
      write (i02,70471)                                                 
70471 format (27x,"       7.123456" )                                   
! T048*  TEST 48                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    E EDIT DESCRIPTORS                                            
      ivtnum = 48                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70480) dcvs                                            
70480 format (27x,e10.2)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70481)                                                 
70481 format (27x,"  0.21E+01" /                                                27x,"  0.21+001" )                                        
! *****    E EDIT DESCRIPTORS                                            
! T049*  TEST 49                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
      ivtnum = 49                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70490) ac1s(25)                                        
70490 format (27x,e12.3)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70491)                                                 
70491 format (27x,"   0.331E+02" /                                              27x,"   0.331+002" )                                      
!   ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             
      write (i02,90002)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! T050*  TEST 50                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
! *****    E EDIT DESCRIPTORS                                            
      ivtnum = 50                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70500) ac2s(4,1)                                       
70500 format (27x,e14.4)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70501)                                                 
70501 format (27x,"    0.4441E+03" /                                            27x,"    0.4441+003" )                                    
! *****    E EDIT DESCRIPTORS                                            
! T051*  TEST 51                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
      ivtnum = 51                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70510) ac1s(7)                                         
70510 format (27x,e16.5)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70511)                                                 
70511 format (27x,"     0.55551E+04" /                                          27x,"     0.55551+004" )                                  
! *****    E EDIT DESCRIPTORS                                            
! T052*  TEST 52                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
      ivtnum = 52                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70520) ac1s(8)                                         
70520 format (27x,e18.6)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70521)                                                 
70521 format (27x,"      0.666661E+05" /                                        27x,"      0.666661+005" )                                
! *****    E EDIT DESCRIPTORS                                            
! T053*  TEST 53                                                         
! *****    FORMATTED WRITES TO TEST THAT LEADING BLANKS           13.5.9 
! *****    ARE INSERTED IN THE OUTPUT FIELD WHEN THE OUTPUT              
! *****    PRODUCED IS SMALLER THAN THE FIELD WIDTH FOR THE              
      ivtnum = 53                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70530) cmavs                                           
70530 format (27x,e20.7)                                                
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70531)                                                 
70531 format (27x,"       0.1234567E+06" /                                      27x,"       0.1234567+006" )                              
2050  format(2pf8.3,-2pe9.4,f9.4,0pf9.4,9x,-2pe9.4,f9.4)                
! T054*  TEST 54                                                         
! *****    SCALE FACTOR APPLIED TO F AND E EDIT DESCRIPTORS              
! *****    ON READ, BUT NOT ON WRITE                                     
! *****  INPUT CARD   21                                                 
      read(irvi,2050)ep1s(16),ep1s(17),ep1s(18), ep1s(19),                 ep1s(20),ep1s(22)                                              
      ivtnum = 54                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70540) ep1s(16),ep1s(17),ep1s(18)                      
70540 format (27x,f12.4, e12.4, f12.2)                                  
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70541)                                                 
70541 format (27x,"     98.7654  0.9877E+04   987654.00" /                      27x,"              0.9877+004            " )              
! *****    ON READ, BUT NOT ON WRITE                                     
! T055*  TEST 55                                                         
! *****    SCALE FACTOR APPLIED TO F AND E EDIT DESCRIPTORS              
      ivtnum = 55                                                       
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70550) ep1s(19),ep1s(20),ep1s(22)                      
70550 format( 27x,f12.3, e12.4,f12.3 )                                  
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70551)                                                 
70552 format (" ",48x,"   OR")                                     
           write (i02,70552)                                            
70553 format (27x,"     987.654  0.8648E+04    8647.859" /              27x,"              0.8648+004            " )                 
           write (i02,70553)                                            
           write (i02,90004)                                            
70551 format (27x,"     987.654  0.8648E+04    8647.860" /                      27x,"              0.8648+004            " )              
2053  format(f8.2,e9.4,f9.2,f9.3,9x,e9.4,f9.4)                          
! T056*  TEST 56                                                         
! *****    SCALE FACTOR APPLIED TO  F AND E EDIT  DESCRIPTORS            
! *****    ON WRITE, BUT, NOT ON READ                                    
! *****  INPUT CARD   22                                                 
      read(irvi,2053) ac1s(1),ac1s(2),ac1s(3),ac1s(4),                    ac1s(20),ac1s(23)                                               
       ivtnum = 56                                                      
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70560) ac1s(1),ac1s(2),ac1s(3)                         
70560 format (27x,2pf12.2, -2pe12.4,f12.4)                              
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70561)                                                 
70561 format (27x,"      987.66  0.0099E+06     98.7654" /                      27x,"              0.0099+006            " )              
      ivtnum = 57                                                       
! T057*  TEST 57 - SCALE FACTOR APPLIED TO  F AND E EDIT  DESCRIPTORS    
! *****            ON WRITE, BUT, NOT ON READ                            
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write (i02,70570) ac1s(4), ac1s(20),ac1s(23)                      
70570 format (27x,1pe12.2,   -2pe12.4,  2pf12.2 )                       
      ivinsp = ivinsp + 1                                               
      write (i02,70201)                                                 
      write (i02,70571)                                                 
70571 format (27x,"    9.88E+02  0.0086E+06     8647.86" /                      27x,"    9.88+002  0.0086+006            " )              
2055  format( i1,i2,i3)                                                 
! T058*  TEST 58 - I/O FORMAT RESCAN                                     
! *****  INPUT CARDS  23, 24, 25                                         
      read(irvi,2055) i2i,iac1i                                         
      ivtnum = 58                                                       
      remrks = '3 COMPUTED LINES EXPECTED'                              
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write(i02,70580) i2i(1,1),i2i(2,1),i2i(1,2),i2i(2,2),iac1i        
70580 format (27x,i4,i5,i6)                                             
      ivinsp = ivinsp + 1                                               
      write (i02,70011)                                                 
      write (i02,70581)                                                 
70581 format (27x,"   1   22   333" /                                           27x,"   4   55   666" /                                           27x,"   7   88   999" )                                   
!   ADVANCE TO TOP-OF-PAGE AND WRITE HEADERS                             
      write (i02,90002)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
! *****  INPUT CARDS  26, 27                                             
2058  format(i4, 2(i1,1x,i2))                                           
      read( irvi,2058) i2i, iac1i                                       
! T059*  TEST 59 - I/O FORMAT RESCAN                                     
      ivtnum = 59                                                       
      remrks = '2 COMPUTED LINES EXPECTED'                              
      write (i02,80004) ivtnum, remrks                                  
      write (i02,80020)                                                 
      write( i02,70590) i2i(2,1),i2i(2,2),iac1i(2),iac1i(4)             
70590 format (27x,i4," **",1(27x,i4," ''",(i4," ((")))                  
      ivinsp = ivinsp + 1                                               
      write (i02,70011)                                                 
      write (i02,70591)                                                 
70591 format(27x,"   2 **",30x,"4 ''   6 ((" ,/                                27x,"   8 ''")                                             
! ****                                                                   
! BB** ********************** BBCSUM0  **********************************
! **** WRITE OUT TEST SUMMARY                                            
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
! *****    END OF TEST SEGMENT 020                                       
      stop                                                              
      end program fm403
