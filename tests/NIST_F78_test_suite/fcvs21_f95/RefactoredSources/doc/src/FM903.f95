      program fm903
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM903               IOFMTF - (354)                             
! *****   THIS PROGRAM CALLS SUBROUTINE SN904                            
! ***********************************************************************
! *****  GENERAL PURPOSE                                        ANS REFS 
! *****    TO TEST ADDITIONAL FEATURES OF READ AND WRITE        12.8     
! *****    STATEMENTS, FORMATTED RECORDS AND FORMAT STATEMENTS  12.1.1   
! *****    DOUBLE PRECISION AND COMPLEX DATA TYPES.                      
! *****    TO TEST ALL FORMS OF CHARACTER EXPRESSIONS AS        13.1.2   
! *****    FORMAT SPECIFIERS.                                            
! *****  RESTRICTIONS OBSERVED                                           
! *****  *  H AND X DESCRIPTORS ARE NEVER REPEATED              13.2.1   
! *****  *  FOR W.D DESCRIPTORS, D IS ALWAYS SPECIFIED AND               
! *****     W IS EQUAL TO OR GREATER THAN D                              
! *****  *  FIELD WIDTH IS NEVER ZERO                                    
! *****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE ITEM          13.3     
! *****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           
! *****     IN THE FORMAT SPECIFICATION                                  
! *****  *  ITEMS IN I/O LIST CORRESPOND TO EDIT DESCRIPTORS             
! *****  *  NEGATIVE OUTPUT VALUES ARE SIGNED                   13.5.9   
! *****  *  AN H EDIT DESCRIPTOR IS NEVER USED ON INPUT         13.5.2   
! *****  *  IN THE INPUT FIELD, FOR THE IW EDIT DESCRIPTOR      13.5.9.1 
! *****     THE CHARACTER STRING MUST BE AN OPTIONALLY SIGNED            
! *****     INTEGER CONSTANT                                             
! *****  GENERAL COMMENTS                                                
! *****     PLUS SIGNS FOR INPUT FIELDS ARE USUALLY OMITTED     13.5.9   
! *****                                                                  
! *****     CALL SUBROUTINE SN904 (SEGMENT 790)                          
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
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 14 CARD IMAGES IN COL. 1 - 56 
! OL.      1-----------------------------------------------------56      
! ARD  1   333144446666225555                                            
! ARD  2   1234567890                                                    
! ARD  3   1234567890                                                    
! ARD  4   1234567890                                                    
! ARD  5   1234567890                                                    
! ARD  6    12345                                                        
! ARD  7    12345123.5123.45D-01 12345D+01                               
! ARD  8   12 345 678                                                    
! ARD  9       5-1111 3333-5555 7777-9999                                
! ARD 10   12345678901234567890123456781234567890123456789012345678      
! ARD 11   12345678901234123456789012341234567890123412345678901234      
! ARD 12   12345678901234123456789012341234567890123456789012345678      
! ARD 13   12345678901234567890123456781234567890123456789012345678      
! ARD 14   12345678901234123456789012341234567890123412345678901234      
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 354                        
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
      integer :: ivi
      integer :: jvi
      real :: avs
        integer, dimension(1:6) :: j1i
        integer, dimension(1:8) :: ia1i
        character(len=11) :: a11vk
        character(len=15), dimension(1:7) :: c151k
        character(len=19) :: a19vk
        character(len=25), dimension(1:6) :: c251k
        character(len=32) :: a32vk
        character(len=52) :: a52vk
        character(len=65) :: a65vk
        character(len=85) :: a85vk
        double precision :: avd
        double precision, dimension(1:4) :: a1d
        double precision, dimension(1:2,1:1,1:2,1:2) :: b4d
        complex :: avc
        complex :: bvc
        complex :: cvc
        complex, dimension(1:2,1:2) :: a2c
        external sn904                                                  
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
      ivtotl = 13                                                       
      zprog = 'FM903'                                                   
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
        write(nuvi, 35400)                                              
35400 format(" ",/ 1x, "IOFMTF - (354) ADDITIONAL FORMATTED" //1x,             "DATA TRANSFERS" ,//1x,                                           "ANS REF. - 12.9.5.2  13.1  13.5" )                      
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! *****    TEST THAT A FORMAT MAY BE A CHARACTER VARIABLE,     12.4.2(3) 
! *****    A CHARACTER EXPRESSION, A CHARACTER ARRAY, OR A     12.4.2(4) 
! *****    CHARACTER ARRAY ELEMENT.                            13.1.2    
! *****    NOTE THAT  THE LENGTH OF THE FORMAT MAY EXCEED THE            
! *****    LENGTH OF AN ARRAY ELEMENT IF THE FORMAT SPECIFIER            
! *****    IS AN ARRAY, BUT NOT IF THE SPECIFIER IS AN ARRAY ELEMENT.    
        write(nuvi, 35401)                                              
35401 format(/8x, "CHARACTER EXPRESSION AS FORMAT" /)                 
        a19vk = '(I3,I1,I4,I4,I2,I4)'                                   
! *****    CARD 1                                                        
        read(irvi, a19vk) j1i(3), j1i(1), j1i(4), j1i(6), j1i(2), j1i(5)
! T001*  TEST 1 - CHARACTER EXPRESSION AS FORMAT                         
           ivtnum = 1                                                   
           remrks = 'LEADING PLUS SIGN OPTIONAL'                        
           write (nuvi, 80004) ivtnum, remrks                           
        a65vk = '16X, "COMPUTED: "/26X,I1, 1X, I2, 1X, I3, 1X, I4, 1X,  I5, 1X, I6'                                                       
        a85vk = '16X, "CORRECT:  ",22X, "2 CORRECT ANSWERS POSSIBLE"/26X, "1 22 333 4444  5555   6666"'                                   
        write(nuvi, '(/1X,' // a65vk // '/1X,' // a85vk // ')') j1i     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70010)                                          
70010 format (26x,"1 22 333 4444 +5555  +6666" )                   
! T002*  TEST 2 - CHARACTER ARRAY AS FORMAT                              
           ivtnum = 2                                                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        c251k(1) = '(26X, I6, 1X, I5, 1X, I4,'                          
        c251k(2) = ' 1X, I3, 1X, I2, 1X, I1 /'                          
        c251k(3) = '17X,"CORRECT: ",22X, "2 C'                          
        c251k(4) = 'ORRECT ANSWERS POSSIBLE"/'                          
        c251k(5) = '26X,  "  6666  5555 4444 '                          
        c251k(6) = '333 22 1")'                                         
        write(nuvi, c251k) (j1i(7-ivi), ivi=1,6)                        
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70020)                                          
70020 format (26x," +6666 +5555 4444 333 22 1" )                   
! T003*  TEST 3 - CHARACTER ARRAY ELEMENT AS FORMAT                      
           ivtnum = 3                                                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
! *****                                                                  
        c151k(1) = '(I1,2X,I2)'                                         
        c151k(3) = '(2X,I3,1X,I4)'                                      
        c151k(5) = '(I5,T1,I1)'                                         
        c151k(7) = '(TR4,I2,TL2,I3)'                                    
! *****    CARDS 2-5                                                     
        do ivi = 1, 7, 2                                           
        read(irvi, c151k(ivi)) ia1i(ivi), ia1i(ivi+1)                   
  end do
        write(nuvi, 70030) ia1i                                         
70030 format (25x, 8(1x, i5))                                         
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70031)                                          
70031 format (" ",16x,"CORRECT:  " ,22x,"2 CORRECT ANSWERS POSSIBLE" )
           write (nuvi, 70032)                                          
70032 format(26x, '    1    45   345  7890 12345     1    56   567'/           26x, '   +1   +45  +345 +7890 12345    +1   +56  +567')  
        write(nuvi, 35404)                                              
! *****                                                                  
! *****    TEST ADDITIONAL INTEGER EDITING FEATURES.                     
! *****      - IW.M EDITING DESCRIPTOR                          13.5.9.1 
! *****    NOTE THAT IF M IS ZERO AND THE VALUE OF THE INTERNAL          
! *****    DATUM IS ZERO, THE OUTPUT FIELD CONSISTS OF ONLY BLANK        
! *****    CHARACTERS REGARDLESS OF THE SIGN CONTROL IN EFFECT.          
35404 format(/8x, "INTEGER EDITING AND OUT OF RANGE" /)               
! *****    CARD 6                                                        
        read(irvi, 35405) (ia1i(ivi), ivi=1,4)                          
35405 format(i6.6, t1, i6.4, tl6, i6.2, tl9, i6.0)                    
! T004*  TEST 4 - INTEGER EDITING                                        
           ivtnum = 4                                                   
           write (nuvi, 80004) ivtnum,remrks                            
           write (nuvi, 80020)                                          
        write(nuvi, 70040) (ia1i(ivi), ivi=1,4)                         
70040 format(25x, 4(1x, i6))                                          
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70031)                                          
           write (nuvi, 70041)                                          
70041 format(26x, " 12345  12345  12345  12345" /                              26x, "+12345 +12345 +12345 +12345" )                     
! T005*  TEST 5 - OUT OF RANGE                                           
           ivtnum = 5                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        jvi = 0                                                         
        ivi = 12                                                        
        write (nuvi, 70050) -ivi, ivi, ivi, ivi, ivi, jvi, jvi, jvi     
70050 format (26x, ss, i5.5, s, 1x, i5.5, ss, 1x, i5.3, 1x, i5.1,               1x, i5.0, 1x, "(", i5.0, ")", s, 1x, "(", i5.0, ")",              sp, 1x, "(", i5.0, ")")                                 
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70051)                                          
70051 format (26x, "***** 00012   012    12    12 (     ) (     )" ,            " (     )")                                             
!   ADVANCE TO TOP-OF PAGE AND WRITE HEADERS                             
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    TEST ADDITIONAL DOUBLE PRECISION EDITING FEATURES. 13.5.9.2   
! *****      - D.P. MAY BE READ, WRITTEN WITH F AND E         13.5.9.2.1 
! *****        EDIT DESCRIPTOR.                               13.5.9.2.2 
! *****        (D AND G FORMATS ARE TEST IN INTERNAL FILE SEGMENTS       
! *****        392 AND 393.)                                             
! *****      - FIELD WIDTH TOO SMALL ON F                     13.5.9(4)  
! *****      - EXPONENT WIDTH TOO SMALL ON EW.DE(E)           13.5.9(4)  
! *****      - IF SP AND FIELD TOO SMALL, THE PLUS IS NOT     13.5.9(5)  
! *****        OPTIONAL                                                  
        write(nuvi, 35408)                                              
35408 format(/8x,"DOUBLE PRECISION EDITING AND OUT OF RANGE" /)       
! *****    CARD 7                                                        
        read(irvi, 35409) b4d                                           
35409 format(1x, 2f5.2, f10.2, f10.5, tl40, 1x, 2e5.2, e10.2, e10.5e5)
! T006*  TEST 6 - DOUBLE PRECISION EDITING AND OUT OF RANGE              
           ivtnum = 6                                                   
           remrks = '2 COMPUTED LINES EXPECTED'                         
           write (nuvi, 80004) ivtnum,remrks                            
           write (nuvi, 80020)                                          
        b4d(2,1,2,2) = (b4d(2,1,2,2) * 10) ** 12                        
        write(nuvi, 70060) b4d                                          
70060 format(26x, sp, f6.2, ss, 1x, f5.4, 1x, f6.3, 1x, f6.4,               /26x,2p,e6.1,0p,2(5x,e10.5),5x,e9.5e1)                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70061)                                          
70061 format(/" ",16x,"CORRECT:  " ,22x,  "CORRESPONDING LINES MUST MATCH "    ,/" ",48x,"EITHER OF THE FOLLOWING TWO    "  ,                    /" ",48x,"CORRECT ANSWERS                "  )         
           write (nuvi, 70062)                                          
70062 format(26x,"****** ***** 12.345 1.2345" /26x,                            "******     .12350E+03     .12345E+02     *********" /           /26x,"****** ***** 12.345 1.2345" /26x,                            "******     .12350+003     .12345+002     *********" )
        write(nuvi, 35411)                                              
! *****                                                                  
! *****    TEST ADDITIONAL COMPLEX EDITING FEATURES.          13.5.9.2.4 
! *****      - FIELD WIDTH TOO SMALL ON F                     13.5.9(4)  
! *****      - EXPONENT WIDTH TOO SMALL ON EW.DE(E)           13.5.9(4)  
35411 format(/8x, "COMPLEX EDITING AND OUT OF RANGE" /)               
! T007*  TEST 7 - COMPLEX EDITING AND OUT OF RANGE                       
           ivtnum = 7                                                   
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        avc = (25.25, 75.75)                                            
        bvc = (0.25e+10, 0.75e+10)                                      
        write(nuvi, 70070) avc, avc, bvc, bvc                           
70070 format (26x, f7.2, 3x, f6.2, 3x, f5.2, 3x, f4.2,                          /26x, e8.2e3, 3x, e7.2e2, 2(4x, e6.2e1))                
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70061)                                          
           write (nuvi, 70071)                                          
70071 format (26x, "  25.25    75.75   25.25   ****" /                          25x, " .25E+010   .75E+10    ******    ******" //                 26x, " +25.25   +75.75   25.25   ****" /                          25x, " .25E+010   .75E+10    ******    ******" )     
! *****    -  TEST T, TL, TR EDIT DESCRIPTORS                   13.5.3.1 
! *****                                                                  
! *****    -  TEST BZ, BN EDIT DESCRIPTORS                      13.5.8   
        write(nuvi, 35414)                                              
35414 format(/8x,"BZ, BN, T, TL AND TR EDIT DESCRIPTOR" /)            
! T008*  TEST 8 - BZ, BN, T, TL, AND TR EDIT DESCRIPTOR                  
           ivtnum = 8                                                   
           remrks = 'LEADING PLUS SIGN OPTIONAL'                        
           write (nuvi, 80004) ivtnum,remrks                            
           write (nuvi, 80020)                                          
! *****    CARD 8                                                        
        read(irvi, 70080) avd, b4d(2,1,1,2), a2c(1,1), avc              
70080 format(bn, d5.2, bz, d5.2, tl40, 2f5.2, t1, tr1, tl1, bn, 2f5.1)
        write(nuvi, 70081) avd, b4d(2,1,1,2), a2c(1,1), avc             
70081 format (25x, 2f6.2, (((4(1x, f6.2)))))                          
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70031)                                          
           write (nuvi, 70082)                                          
70082 format(25x, tr26, " 123.40 567.80" , t25, "  12.34506.78" ,              1x, "120.34 506.78" //                                            25x, tr26, " 123.40 567.80" , t25, " +12.34506.78" ,              1x, "120.34 506.78" )                                 
! *****    SPECIFIER TO A SUBROUTINE.                                    
! *****                                                                  
! *****    PASS A CHARACTER CONSTANT, WHICH IS A LEGITIMATE FORMAT       
        write(nuvi, 35417)                                              
35417 format(/8x,"SUBROUTINE CALL" /)                                 
! T009*  TEST 9 - SUBROUTINE CALL                                        
           ivtnum = 9                                                   
           write (nuvi, 80004) ivtnum,remrks                            
! *****    CARD 9                                                        
        a11vk = '(I5, 6(I5))'                                           
        call sn904(a11vk,irvi,nuvi)

           ivinsp = ivinsp + 1                                          
! *****                                                                  
!   ADVANCE TO TOP-OF PAGE AND WRITE HEADERS                             
        write (nuvi, 90002)                                             
        write (nuvi, 90013)                                             
        write (nuvi, 90014)                                             
! *****                                                                  
! *****    -  TEST SS AND SP EDIT DESCRIPTORS.                    13.5.6 
! *****    -  TEST ALSO THAT A FORMAT SPECIFICATION MAY BE        13.1.2 
! *****       ALTERED BY A CHARACTER SUBSTRING SUBSTITUTION.       5.7   
        write(nuvi, 35419)                                              
35419 format(/8x,"SS AND SP EDIT DESCRIPTOR" /)                       
! T010*  TEST 10 - SS AND SP EDIT DESCRIPTORS                            
           ivtnum = 10                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        ivi = 12345                                                     
        avs = 25.25                                                     
        a1d(2) = 5.5d0                                                  
        a2c(2,1) = (3.0, 4.0)                                           
        a52vk = '(26X,SP,F5.1,SS,2X,F4.1,SP,(T40,I6,2X,F6.2,SS,F6.1))'  
        write(nuvi, a52vk) a2c(2,1), ivi, avs, a1d(2), ivi, avs, a1d(2) 
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70101)                                          
70101 format(/" ",16x,"CORRECT:  " ,22x,  "CORRESPONDING LINES MUST MATCH "    )                                                     
           write (nuvi, 70102)                                          
70102 format(26x,' +3.0   4.0  +12345  +25.25   5.5'                           /t40,' 12345   25.25   5.5')                          
! T011*  TEST 11 - FORMAT ALTERED BY CHARACTER SUBSTRING SUBSTITUTION    
           ivtnum = 11                                                  
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        a52vk(7:7) = 'S'                                                
        a52vk(14:15) = 'SP'                                             
        a52vk(26:26) = 'S'                                              
        a52vk(45:45) = 'P'                                              
        write(nuvi, a52vk) a2c(2,1), ivi, avs, a1d(2), ivi, avs, a1d(2) 
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70101)                                          
           write (nuvi, 70111)                                          
70111 format (26x,'  3.0  +4.0   12345   25.25  +5.5'                           /t40,'+12345  +25.25  +5.5')                         
        write(nuvi, 35422)                                              
! *****                                                                  
! *****    TEST A COLON EDIT DESCRIPTOR FOLLOWED BY A H-EDIT      13.5.5 
! *****    DESCRIPTOR TO SHOW THAT THE COLON EDIT DESCRIPTOR             
! *****    TERMINATED IF THERE ARE NO MORE ITEMS IN THE INPUT/OUTPUT LIST
35422 format(/8x,'COLON EDIT DESCRIPTOR'/)                            
! T012*  TEST 12                                                         
           ivtnum = 12                                                  
           remrks = '2 COMPUTED LINES EXPECTED'                         
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        a32vk = 'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHH'                      
        write(nuvi, 70120) a32vk, a32vk                                 
70120 format(26x, a32, :, 'IIIIJJJJ')                                 
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70101)                                          
           write (nuvi, 70121)                                          
70121 format(26x, 'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJ',                  /26x, 'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHH')             
           ivtnum = 13                                                  
! *****                                                                  
! *****    TEST THAT FW.D, EW.DE(E) AND GW.DE(E) MAY HAVE MORE DIGITS ON 
! *****    INPUT THAN THE PROCESSOR CAN HANDLE FOR D.P. AND COMPLEX      
! T013*  TEST 13 - LARGE FORMAT SIZE FOR D.P. AND COMPLEX                
           write (nuvi, 70131) ivtnum                                   
70131 format (/" ",2x,i3,4x,"INSPECT",32x,                                      'TEST SUCCESSFUL IF PROCESSOR IS '/" ",48x,                       'ABLE TO READ INPUT CARDS 10-14  '/" ",48x,                       'UNDER F, E, AND G FORMATS WHICH '/" ",48x,                       'HAVE  MORE  DIGITS  THAN  THE   '/" ",48x,                       'PROCESSOR CAN HANDLE FOR D. P.  '/" ",48x,                       'AND COMPLEX')                                       
           ivinsp = ivinsp + 1                                          
! *****    CARDS 10-14                                                   
        read(irvi, 70130) b4d(1,1,1,1), avd, avc, a2c(2,2), bvc,               (b4d(1,1,ivi,1),ivi=1,2), a1d(1), a2c(1,2), cvc            
70130 format(2f28.14, /2(e14.7e2, g14.14e1), /g14.0e3, e14.14e3,               e28.0e1, /2g28.14e2, /2(f14.0, f14.14) )                 
      ivtotn = ivpass + ivfail + ivdele + ivinsp                        
! *****                                                                  
! BB** ********************** BBCSUM0  **********************************
! **** WRITE OUT TEST SUMMARY                                            
! ****                                                                   
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
! *****    END OF TEST SEGMENT 354                                       
        stop                                                            
        end program fm903
        subroutine sn904(a0wvk,irwvi,nuwvi)
! *****                                                                  
! *****  S P E C I F I C A T I O N S   SEGMENT 790                       
! *****                                                                  
      integer :: irwvi
      integer :: nuwvi
      integer :: ivi
      integer :: jvi
        character(len=*) :: a0wvk
        character(len=130) :: a130vk
        integer, dimension(1:5) :: i1i
! *****                                                                  
! *****    TESTS THAT                                                    
! *****    - A FORMAT SPECIFIER MAY BE PASSED AS A CHARACTER      13.1.2 
! *****      CONSTANT TO A SUBROUTINE.                          15.6.2.3 
! *****    - A FORMAT SPECIFIER MAY BE DEFINED IN A DATA          13.1.2 
! *****      STATEMENT.                                              9.4 
! *****    - AN INPUT LIST MAY CONTAIN AN INTEGER THAT IS USED  12.8.2.3 
! *****      AS A SUBSCRIPT IN AN IMPLIED DO-LIST.                       
! *****    - AN OUTPUT LIST MAY CONTAIN AN EXPRESSION WITH AN   12.8.2.2 
! *****      INTRINISC FUNCTION.                                15.3     
! *****                                                                  
        data a130vk / '(16X, "COMPUTED: "/26X, 3I5/16X, "CORRECT:  ",22X, ''2 CORRECT ANSWERS POSSIBLE''/26X,'' 1111 3333-5555''/26X,''+1111+3333-5555'')' / 
        read(irwvi, a0wvk) ivi, (i1i(jvi),jvi=1,ivi)                    
        write(nuwvi, a130vk) iabs(i1i(1)), max0(i1i(2),i1i(5)), i1i(3)  
! *****                                                                  
        return                                                          
        end subroutine sn904
