      program fm110
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM110               IOFMT - (350)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REFS
! *****    TO TEST ADDITIONAL FEATURES OF READ AND WRITE          12.8   
! *****    STATEMENTS, FORMATTED RECORDS AND FORMAT STATEMENTS    12.1.1 
! *****    FOR INTEGER AND REAL DATA TYPES                               
! *****  RESTRICTIONS OBSERVED                                           
! *****  *  ALL FORMAT STATEMENTS ARE LABELED                     13.1.1 
! *****  *  H AND X DESCRIPTORS ARE NEVER REPEATED                13.2.1 
! *****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               
! *****     W IS EQUAL TO OR GREATER THAN D                              
! *****  *  FIELD WIDTH IS NEVER ZERO                                    
! *****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE ITEM            13.3   
! *****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           
! *****     IN THE FORMAT SPECIFICATION                                  
! *****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS             
! *****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                     13.5.9 
! *****  *  AN H EDIT DESCRIPTOR IS NEVER USED ON INPUT           13.5.2 
! *****  *  IN THE INPUT FIELD, FOR THE IW EDIT DESCRIPTOR      13.5.9.1 
! *****     THE CHARACTER STRING MUST BE AN OPTIONALLY SIGNED            
! *****     INTEGER CONSTANT                                             
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
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 40 CARD IMAGES IN COL. 1 - 80 
!     COLS.        22  25  31  34-35  40-43  55  67  69  74-76           
! ARD  1            .   .   .     0.   E+00   +   +   .    E00           
!     COLS.        16  31  33  42-45  50  59-60                          
! ARD  2            +   +   .   D+00   .     D0                          
!     COLS. 1-----------14  18-----26  28-------38                       
! ARD 3     1.23456987654.  +1.234E-0  -98.7654E+0                       
!     COLS         1---5                                                 
! ARDS 4,5,6,7,8   12345                                                 
!     COLS.        1-3                                                   
! ARDS 9,10,11,12  1.1                                                   
!     COLS. 1------------------------------------------------------58    
! ARD 13    +0.339567E+02                                                
! ARD 14      + .339567+2                                                
! ARD 15     + 3.395670E1                                                
! ARD 16     0.96295134244D+04                                           
! ARD 17       .96295134244D04                                           
! ARD 18       0.96295134244+4                                           
! ARD 19       +.96295134244D4                                           
! ARD 20    31.23+0.14E+04+0.2D+02                                       
! ARD 21    31.23   .14D+4   +.2+2                                       
! ARD 22    -0.13579E+054444                                             
! ARD 23    4444                                                         
! ARD 24    4444                                                         
! ARD 25    4444                                                         
! ARD 26    4444                                                         
! ARD 27    -333 5.555+0.4545E-04                                        
! ARD 28    -6.666  .9989E+12                                            
! ARD 29    7.77-0.747E-02  +0.549E022                                   
! ARD 30    +0.662E-00  0.468-1011                                       
! ARD 31     0.59542D+04-44.6666-0.1234560000D-03                        
! ARD 32     54.9327-0.1395624534D+00                                    
! ARD 33    65432.1                                                      
! ARD 34    +0.848E+03    .848E3 + .1290D7+0.129D+07  0.412D21           
! ARD 35    22222222222222222222222222222222222222222222222222           
! ARD 36       -.987E0-0.987E+00   -.987D0                               
! ARD 37       5   5                                                     
! ARD 38        987654   8647.86   987.654                               
! ARD 39    1.2345E0  1.2345  1234.5                                     
! ARD 40    12345.                                                       
! ARD COLS. NOT MENTIONED ARE BLANK                                      
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 350                        
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
      integer :: jacvi
      real :: acvs
      real :: bcvs
      integer :: ivtnum
      real :: cmavs
      real :: cmbvs
      real :: cmevs
      real :: cmfvs
      real :: ffcvs
      real :: ggcvs
      integer :: kbcvi
      real :: cmgvs
      real :: avs
      real :: bvs
      real :: hhcvs
      real :: aavs
      real :: dcvs
      integer :: lccvi
      integer :: mdcvi
      real :: ccvs
      real :: cmcvs
      real :: cmdvs
      real :: ravs
      real :: rbvs
      real :: rcvs
      real :: rdvs
      real :: revs
      integer :: mrrvi
      integer :: necvi
      real :: cavs
      real :: cvs
      real :: bavs
      real :: dvs
      real, dimension(1:5) :: a1s
      real, dimension(1:2,1:2) :: a2s
      real, dimension(1:3,1:3,1:3) :: a3s
      real, dimension(1:25) :: ac1s
      real, dimension(1:5,1:6) :: ac2s
      integer, dimension(1:5) :: iac1i
      integer, dimension(1:2,1:7) :: iac2i
      real, dimension(1:33) :: ep1s
      integer, dimension(1:2,1:3,1:3) :: mca3i
      real :: mvs
!      CHARACTER*80 IDATA                                                
! ***** IDATA USED BY TEST 3 TO BYPASS CARDS 4-21 TO DELETE TEST         
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
           ivtotl = 11                                                  
           zprog='FM110'                                                
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
! *****    ALL VARIABLES AND ARRAY ELEMENTS USED IN THIS SEGMENT         
! *****    ARE FIRST SET TO A NON-ZERO VALUE                             
! *****                                                                  
! *****    HEADER FOR SEGMENT 350 WRITTEN                                
35000 format (//2x,"IOFMT - (350) ADDITIONAL FORMATTED I/O"  //16x,            "DATA TRANSFERS" ,//2x, "SUBSET REFS - 12.8   13." )      
      write (nuvi,35000)                                                
! *****                                                                  
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
      jacvi = 11111                                                     
      iac1i(1) = -2345                                                  
      iac2i(1,1) = 9999                                                 
      mca3i(1,1,1) = 2                                                  
      acvs = 1.2                                                        
      bcvs = -.34e-3                                                    
      a1s(1) = 34.56                                                    
      a1s(2) = 456.789e+02                                              
      a2s(1,1) = -7899.3                                                
      a2s(2,1) = +9876.543e-01                                          
      a3s(1,1,1) = .543                                                 
      a3s(2,1,1) = 4.33e+1                                              
      mvs = +2.22e+01                                                   
      a1s(3) = -.33456e-01                                              
      a2s(1,2) = 9987.76e+2                                             
      a3s(3,1,1) = 44.e-2                                               
! ****                                                                   
!                                                                        
! T001*  TEST 1                                                          
           ivtnum = 1                                                   
! ******                                                                 
! *****     TEST THAT BLANK INPUT FIELDS ARE TREATED AS ZERO      13.5.9 
! *****     I, E, and F EDIT DESCRIPTORS ARE TESTED                      
! *****     CARDS 1 AND 2                                                
! *****                                                                  
35001 format (4(i5), 4(f3.1), 4(f11.4)/ 4(e15.8))                     
      read (irvi,35001) jacvi, iac1i(1), iac2i(1,1), mca3i(1,1,1), acvs,  a1s(1), a2s(1,1), a3s(1,1,1), bcvs, a1s(2), a2s(2,1),             a3s(2,1,1), mvs, a1s(3), a2s(1,2), a3s(3,1,1)                   
! ****      TO DELETE TEST INSERT THE FOLLOWING CODE:                    
! ****      IVDELE=IVDELE+1                                              
! ****      WRITE (NUVI,80000) IVTNUM                                    
! ****      COMMENT OUT FOLLOWING CODE UNTIL NEXT TEST                   
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
70010 format (/49x,"THIS TEST CONTAINS 4 GROUPS" ,                      /49x,"ALL ANSWERS SHOULD BE ZERO" )                          
! **************************                                             
           write (nuvi,70010)                                           
35002 format (" ",16x,"COMPUTED: " ,22x,                                "4 COMPUTED LINES EXPECTED" ,4(/23x,i6),                       /17x,"COMPUTED: " ,22x,"4 COMPUTED LINES EXPECTED" ,              4(/23x,f8.1),/17x,"COMPUTED: " ,22x,                              "4 COMPUTED LINES EXPECTED" ,4(/23x,f12.5),                       /17x,"COMPUTED: " ,22x,"4 COMPUTED LINES EXPECTED" ,              4(/23x,e12.1))                                                  
      write (nuvi,35002) jacvi, iac1i(1), iac2i(1,1), mca3i(1,1,1),acvs,  a1s(1), a2s(1,1), a3s(1,1,1), bcvs, a1s(2), a2s(2,1),             a3s(2,1,1), mvs, a1s(3), a2s(1,2), a3s(3,1,1)                   
! *****                                                                  
! T002*  TEST 2                                                          
           ivtnum = 2                                                   
! *****     TEST THAT DECIMAL POINTS APPEARING IN INPUT FIELDS 13.5.9.2.1
! *****     OVERRIDE THE SPECIFICATIONS SUPPLIED BY E AND F              
! *****     EDIT  DESCRIPTORS                                            
70020 format (" ",48x,"THIS TEST CONTAINS 4 GROUPS" )              
        cmavs = 1.23456                                                 
        cmbvs = 987654.                                                 
        cmevs = 0.1234e+01                                              
        cmfvs = -0.987654e+02                                           
! *****  CARD 3                                                          
35004 format (2(f7.3), 2(e12.5))                                      
        read (irvi,35004) acvs, bcvs, ffcvs, ggcvs                      
35005 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,     2(/23x,f12.5),/17x,"CORRECT:  " ," 1.23456",                      //17x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,             2(/23x,f13.1),/17x,"CORRECT:  " ," 987654.0",                     //17x,"COMPUTED: "  ,22x,"2 COMPUTED LINES EXPECTED" ,            2(/23x,e15.4),/17x,"CORRECT:  " ," 0.1234E+01" ," OR ",           " .1234+001" ,//17x,"COMPUTED: " ,22x,                            "2 COMPUTED LINES EXPECTED" ,2(/23x,e17.6),                       /17x,"CORRECT:  " ,"-0.987654E+02" ," OR ","-.987654+002" )     
! ****      SEE TEST 1 TO DELETE TEST (ENTER CODE HERE)                  
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
           write (nuvi,70020)                                           
        write (nuvi,35005) cmavs, acvs, cmbvs, bcvs, cmevs, ffcvs,           cmfvs, ggcvs                                                 
           ivtnum=3                                                     
! *****                                                                  
! T003*  TEST 3                                                          
! *****     TEST COMPLETE FORMAT RESCAN                          13.3    
! *****     WHEN ADDITIONAL ITEMS REMAIN IN AN I/O LIST                  
! *****     AND THE LAST RIGHT PARENTHESIS HAS BEEN REACHED              
! *****     IN THE CORRESPONDING FORMAT STATEMENT                        
        jacvi = +12345                                                  
        kbcvi = 3                                                       
        cmavs = 1.1                                                     
        cmbvs = 1.23                                                    
        cmevs = 33.9567                                                 
        cmgvs = 1.4e+03                                                 
        avs = .962951e+4                                                
        bvs = 2.0e1                                                     
! *****  CARDS 4, 5, 6, 7, 8                                             
70030 format (/49x,"THIS TEST CONTAINS 5 GROUPS" )                 
! ***********************                                                
! ****      TO DELETE TEST 3 - CARDS 4 THRU 21 MUST BE BYPASS            
! ****      USE THE FOLLOWING CODE:                                      
! ****      IVDELE=IVDELE+1                                              
! ****      WRITE (NUVI,80000) IVTNUM                                    
! ****      DO 0031 IPASS=1,18                                           
! 0032      FORMAT (A80)                                                 
! ****      READ (IRVI,0032) IDATA                                       
! 0031      CONTINUE                                                     
! ****      COMMENT OUT REMAINING CODE UNTIL NEXT TEST                   
! *************************                                              
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
           write (nuvi,70030)                                           
35007 format (i5)                                                       
      read (irvi,35007) iac1i                                           
 3509 format (" ",16x,"COMPUTED: " ,22x,"6 COMPUTED LINES EXPECTED" )   
      write (nuvi,3509)                                                 
35009 format(23x,i10)                                                   
      write(nuvi,35009)jacvi,iac1i                                      
35008 format (" ",16x,"C0RRECT:  " ," 12345")                         
        write(nuvi,35008)                                               
! *****  CARDS 9, 10, 11, 12                                             
35010 format(f3.1)                                                    
        read (irvi,35010) a2s                                           
 3501   format (/17x,"COMPUTED: " ,22x,"5 COMPUTED LINES EXPECTED" )    
        write (nuvi,3501)                                               
35012 format(23x,f8.1)                                                
        write(nuvi,35012)cmavs,a2s                                      
35011 format (" ",16x,"C0RRECT:  " ," 1.1")                           
        write (nuvi,35011)                                              
! *****  CARDS 13, 14, 15                                                
35013 format (e13.6)                                                  
        read (irvi,35013) a1s(1), hhcvs, a1s(2)                         
 3504   format (/17x,"COMPUTED: " ,22x,"4 COMPUTED LINES EXPECTED" )    
        write (nuvi,3504)                                               
35015 format(23x,e17.6)                                               
        write(nuvi,35015) cmevs, a1s(1), hhcvs, a1s(2)                  
35014 format (" ",16x,"C0RRECT:  " ," 0.339567E+02" ," OR ",            " .339567+002" )                                                
        write (nuvi,35014)                                              
! *****  CARDS 16, 17, 18, 19 WITH D EXPONENTS                           
35016 format (f18.11/e18.11)                                          
        read (irvi,35016) a2s                                           
 3507   format (/17x,"COMPUTED: " ,22x,"5 COMPUTED LINES EXPECTED" )    
        write (nuvi,3507)                                               
35018 format (23x,e17.6)                                              
        write (nuvi,35018) avs, a2s                                     
35017 format (" ",16x,"CORRECT:  " ," 0.962951E+04" ,                   " OR "," .962951+004" )                                         
        write (nuvi,35017)                                              
! *****  CARDS 20, 21                                                    
35019 format (i1,f4.2,e9.2,f8.1)                                      
        read (irvi,35019) lccvi, dcvs, ac2s(5,6), a3s(1,2,2), mdcvi,         ffcvs, ggcvs, aavs                                           
70033 format (/17x,"COMPUTED: " ,22x,"3 COMPUTED LINES EXPECTED" )    
        write (nuvi,70033)                                              
35021 format (23x,i6, f6.2, e10.2, e9.1)                              
        write (nuvi,35021) kbcvi, cmbvs, cmgvs, bvs, lccvi, dcvs,             ac2s(5,6), a3s(1,2,2), mdcvi, ffcvs, ggcvs, aavs            
35020 format (" ",16x,"CORRECT:  " ,22x,                                "2 CORRECT ANSWERS POSSIBLE" ,                                    /28x,"3  1.23  0.14E+04  0.2E+02" ,                               /28x,"3  1.23  0.14+004  0.2+002" )                             
        write (nuvi,35020)                                              
! **********************************                                     
! T004*  TEST 4                                                          
           ivtnum=4                                                     
! *****                                                                  
! ************************************                                   
! *****   TEST THAT FORMAT CONTROL PASSES TO THE GROUP                   
! *****   ENCLOSED BY THE LAST PRECEDING RIGHT PARENTHESIS               
! *****   WHEN THE I/O LIST CONTAINS MORE ELEMENTS THAN                  
! *****   THE NUMBER OF DESCRIPTORS IN THE FORMAT STATEMENT              
! ***************************************                                
        jacvi = +4444                                                   
        kbcvi = -333                                                    
        lccvi = 22                                                      
        mdcvi = 11                                                      
        acvs = 5.555                                                    
        bcvs = -6.666                                                   
        ccvs = +7.77                                                    
        dcvs = 65432.1                                                  
        cmavs = -0.13579e+5                                             
        cmbvs = 0.4545e-04                                              
        cmcvs = 0.9989e12                                               
        cmdvs = -0.747e-2                                               
        cmevs = +0.549e+00                                              
        cmfvs = 0.662e-0                                                
        cmgvs = 0.468e-10                                               
        ravs = +59.542e02                                               
        rbvs = -0.01234560e-2                                           
        rcvs = -1395624534.e-10                                         
        rdvs = +129.e4                                                  
        revs = 4.12e+20                                                 
        ffcvs = -44.6666                                                
        ggcvs = +.549327e+2                                             
        hhcvs = 848.                                                    
        mvs = -.987                                                     
! ***** CARDS 22, 23, 24, 25, 26                                         
35022 format ( e12.5, (i4))                                           
! *****     SEE NOTES TEST1 & TEST 3 TO BYPASS TEST                      
! *****     CARDS 22 THRU 26 MUST BE BYPASSED                            
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
           write (nuvi,70040)                                           
        read (irvi,35022) a1s(2), iac1i                                 
70040 format (" ",48x,"THIS TEST CONTAINS 2 GROUPS" )              
35023 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,     2(/23x,e16.5),                                                  /17x,"CORRECT:  " ,"-0.13579E+05" ," OR "," -.13579+005" ,        //17x,"COMPUTED: " ,22x,"6 COMPUTED LINES EXPECTED" ,             /(23x,i9))                                                      
70041 format (" ",16x,"CORRECT:  " ," 4444")                          
      write (nuvi,35023) cmavs, a1s(2), jacvi, iac1i                    
      write (nuvi,70041)                                                
! T005*  TEST 5                                                          
! *****                                                                  
           ivtnum = 5                                                   
! *****     CARDS 27, 28                                                 
! *****     SEE NOTES TEST 1 & TEST 3 TO DELETE TEST                     
! *****     CARDS 27,28 SHOULD BE BYPASSED                               
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
           write (nuvi,70050)                                           
70050 format (" ",48x,"THIS TEST CONTAINS 5 GROUPS" )              
35025 format (i4, (f6.3), e11.4)                                        
      read (irvi,35025) mrrvi, ac1s(1), ep1s(1), a3s(1,1,1), ac2s(2,2)  
35026 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,   2(/23x,i8),/17x,"CORRECT:  " ,"-333",//17x,"COMPUTED: " ,          22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,f10.3),                  /17x,"CORRECT:  " ," 5.555",//17x,"COMPUTED: " ,                  22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,e15.4),                   /17x,"CORRECT:  " ," 0.4545E-04" ," OR ",".4545-004",//17x,       "COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,f10.3),    /17x,"CORRECT:  " ,"-6.666",//17x,                                 "COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,e15.4),    /17x,"CORRECT:  " ," 0.9989E+12" ," OR ",".9989+012")            
        write (nuvi,35026) kbcvi, mrrvi, acvs, ac1s(1), cmbvs, ep1s(1),       bcvs, a3s(1,1,1),cmcvs,ac2s(2,2)                            
           ivtnum = 6                                                   
! T006*  TEST 6                                                          
! *****     CARDS 29, 30                                                 
! *****     SEE NOTES TEST 1 & 3 TO DELETE TEST                          
! *****     CARDS 29 & 30 MUST BE BYPASSED                               
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
70060 format (" ",48x,"THIS TEST CONTAINS 7 GROUPS" )              
           write (nuvi,70060)                                           
35027 format (f4.2, (2(e10.3)), i2)                                   
        read (irvi,35027) a2s(2,2), a3s(2,1,1), ep1s(2), mca3i(1,1,1),       bvs, ac2s(2,1), necvi                                        
35028 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,   2(/23x,f9.2),/17x,"CORRECT:  " ," 7.77",//17x,"COMPUTED: " ,    22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,e14.3),/17x,"CORRECT:  " ,"-0.747E-02" ," OR ","-.747-002",//17x,"COMPUTED: " ,22x,         "2 COMPUTED LINES EXPECTED" ,2(/23x,e14.3),/17x,"CORRECT:  " ,    " 0.549E+00" ," OR ",".549+000",//17x,"COMPUTED: " ,22x,          "2 COMPUTED LINES EXPECTED" ,2(/23x,i7),/17x,"CORRECT:  " ," 22", //17x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,             2(/23x,e14.3), /17x,"CORRECT:  " ," 0.662E+00" ," OR ",".662+000")
75028 format (//17x,"COMPUTED: " ,22x,                                    "2 COMPUTED LINES EXPECTED" ,2(/23x,e14.3),                       /17x,"CORRECT:  " ," 0.468E-10" ," OR ",".468-010",              //17x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,i7),   /17x,"CORRECT:  " ," 11")                                       
        write (nuvi,35028) ccvs, a2s(2,2), cmdvs, a3s(2,1,1), cmevs,      ep1s(2), lccvi, mca3i(1,1,1), cmfvs, bvs                        
!                                                                        
        write (nuvi,75028) cmgvs,ac2s(2,1),mdcvi,necvi                  
!                                                                        
! T007*  TEST 7                                                          
           ivtnum = 7                                                   
! *****     CARDS 31, 32                                                 
! *****     SEE NOTES TEST 1 & TEST 3 TO DELETE TEST                     
! *****     CARDS 31,& 32 SHOULD BE BYPASSED                             
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
           write (nuvi,70070)                                           
70070 format (" ",48x,"THIS TEST CONTAINS 5 GROUPS" )              
35029 format (e12.5, (f8.4,  e17.10))                                 
        read (irvi,35029) cavs, ep1s(3), a1s(1), a2s(1,2), a2s(2,1)     
35030 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,    /(23x, e16.5))                                                 
70071 format (/17x,"CORRECT:  " ," 0.59542E+04" ," OR ",                ".59542+004" )                                                  
        write (nuvi,35030) ravs, cavs                                   
        write (nuvi,70071)                                              
35031 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,   2(/23x,f12.4),/17x,"CORRECT:  " ,"-44.6666",                      //17x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,             2(/23x,e17.6),                                                    /17x,"CORRECT:  " ,"-0.123456E-03" ," OR ","-.123456-003" ,       //17x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,             2(/23x,f12.4),/17x,"CORRECT:  " ," 54.9327",//17x,                "COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,/(23x,e17.6))    
!                                                                        
70072 format (/17x,"CORRECT:  " ,"-0.139562E+00" ," OR ",               "-.139562+000" )                                                
       write (nuvi,35031) ffcvs, ep1s(3), rbvs, a1s(1), ggcvs, a2s(1,2),  rcvs, a2s(2,1)                                                  
        write (nuvi,70072)                                              
! ****                                                                   
! T008*  TEST 8                                                          
           ivtnum = 8                                                   
! *****     CARDS 33, 34, 35, 36                                         
! *****     SEE NOTES TEST 1 & TEST 3 TO DELETE TEST                     
! *****     CARDS 33 THRU 36 SHOULD BE BYPASSED                          
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
           write (nuvi,70080)                                           
70080 format (" ",48x,"THIS TEST CONTAINS 5 GROUPS" )              
! *****     THIS READ CAUSES AN INPUT DATA CARD TO BE SKIPPED            
35032 format( f7.1, (/2(e10.3), 2(e10.3)), e10.3)                     
        read (irvi,35032)  cvs, a2s(2,1), a3s(1,2,2), a3s(1,1,1),         a3s(2,2,1), a2s(1,1), a3s(1,2,1), ep1s(4),a1s(2)                
35033 format (" ",16x,"COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,   2(/23x,f12.1),/17x,"CORRECT:  " ," 65432.1",//17x,                "COMPUTED: " ,22x,"3 COMPUTED LINES EXPECTED" ,3(/23x,e14.3),     /17x,"CORRECT:  " ," 0.848E+03" ," OR ",".848+003",//17x,         "COMPUTED: " ,22x,"3 COMPUTED LINES EXPECTED" ,3(/23x,e14.3),     /17x,"CORRECT:  " ," 0.129E+07" ," OR ",".129+007",//17x,         "COMPUTED: " ,22x,"2 COMPUTED LINES EXPECTED" ,2(/23x,e14.3),     /17x,"CORRECT:  " ," 0.412E+21" ," OR ",".412+021",//17x,         "COMPUTED: " ,22x,"4 COMPUTED LINES EXPECTED" ,4(/23x,e14.3),     /17x,"CORRECT:  " ,"-0.987E+00" ," OR ","-.987+000")            
        write (nuvi,35033) dcvs, cvs, hhcvs, a2s(2,1), a3s(1,2,2),rdvs,   a3s(1,1,1), a3s(2,2,1), revs, a2s(1,1),                           mvs, a3s(1,2,1), ep1s(4),a1s(2)                                 
! T009*  TEST 9                                                          
           ivtnum = 9                                                   
! *****     TEST FOR EMPTY FORMAT STATEMENT                              
! *****     SEE NOTES TEST 1 TO DELETE TEST                              
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
35034 format (" ",48x,"EMPTY FORMAT ( ) WRITE" ,                        //2x,"THE FOLLOWING LINE SHOULD BE BLANK" )                     
        write (nuvi,35034)                                              
35035 format ( )                                                      
        write (nuvi,35035)                                              
35036 format (2x,"  END EMPTY FORMAT TEST" )                          
        write (nuvi,35036)                                              
! *****  POSITION INPUT TO INSURE CORRECT RECORD FOR NEXT TESTS          
35037 if (mrrvi - 5) 35038, 35039, 35038                              
! *****     CARD 37                                                      
35038 read (irvi, 35025) mrrvi                                          
      goto 35037                                                       
35039 continue                                                          
! T010*  TEST 10                                                         
           ivtnum = 10                                                  
! *****                                                                  
! *****     ADDITIONAL  SCALE FACTOR ON INPUT-OUTPUT            13.5.7   
! *****     CARD 38                                                      
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
35040 format (1pe10.3, -1pe10.2, e10.3)                               
        read (irvi,35040) a1s(3), a1s(4), a1s(5)                        
! ****      SEE NOTES TEST 1 TO DELETE TEST (INSERT CODE HERE)           
35041 format (" ",16x,"COMPUTED: " ,                                    e12.3,     e12.4,      e12.4,                                     /17x,"CORRECT:  " ,22x,"2 CORRECT ANSWERS POSSIBLE" ,             /30x,"0.988E+02  0.8648E+05  0.9877E+04" ,                        /30x," .988+002   .8648+005   .9877+004" )                      
        write(nuvi, 35041) a1s(3), a1s(4), a1s(5)                       
! T011*  TEST 11                                                         
           ivtnum = 11                                                  
! *****     CARDS 39 & 40                                                
! *****     SCALE FACTOR HAS NO EFFECT ON FORMAT RESCAN OR F EDIT        
! *****     DESCRIPTOR WITH INPUT DATA CONTAINING AN EXPONENT            
        aavs = .087654                                                  
        bavs = .87654                                                   
35042 format (-1p,2f8.1, +1p, 2x,(f8.1))                              
        read (irvi, 35042) avs, bvs, cvs, dvs                           
! ****      SEE NOTES TEST 1 TO DELETE TEST                              
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
35043 format (" ",16x,"COMPUTED: " ,22x,                             "3 COMPUTED LINES EXPECTED" ,/25x,f8.4, f8.3, f8.2, f8.1, 1p,     /26x, f5.4, 3x, 2p, f5.3, +3p, " ", (23x,f6.2),3x)              
 5043      format (17x,"CORRECT:  " ,22x,"                          " ,   /25x,"  1.2345  12.345  123.45  1234.5" ,/24x,                    "  .8765   8.765                         87.65" /21x,             "  876.54")                                                     
        write (nuvi,35043) avs,bvs,cvs,dvs,aavs,aavs,aavs,bavs          
        write (nuvi,5043)                                               
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
! *****    END OF TEST SEGMENT 350                                       
      stop                                                              
      end program fm110
