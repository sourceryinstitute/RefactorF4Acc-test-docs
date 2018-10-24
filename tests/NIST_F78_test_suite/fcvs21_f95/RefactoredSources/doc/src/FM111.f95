      program fm111
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM111               IOFMTS - (353)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REFS
! *****    TO TEST ADDITIONAL FEATURES OF READ AND WRITE        12.8     
! *****    STATEMENTS, FORMATTED RECORDS AND FORMAT STATEMENTS  12.1.1   
! *****    FOR INTEGER AND REAL DATA TYPES                               
! *****    TO TEST CHARACTER CONSTANTS AS FORMAT SPECIFIERS.    13.1.2   
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
!   INPUT DATA TO THIS SEGMENT CONSISTS OF 8 CARD IMAGES IN COL. 1 - 39  
! OL.      1-------------------------------------------46                
! ARD  1   111 2 2 3 3. 3E-1  44 5 5 6 . 67 . 78 8. 8E-1                 
! ARD  2   9 9                                                           
! ARD  3   2345 1 34512 45123 51234                                      
! ARD  4   2345 1 34512 45123 51234                                      
! ARD  5                                                                 
! ARD  6   246801357912345678901234                                      
! ARD  7   .10203040506070809010E+0233.33                                
! ARD  8       1    2    3    4    5    6                                
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 353                        
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
      real :: avs
      integer :: ivi
      integer :: jvi
      integer :: kvi
      real :: bvs
      real :: cvs
        integer, dimension(1:2,1:2) :: i2i
        integer, dimension(1:2,1:2,1:2) :: i3i
        integer, dimension(1:1,1:2,1:3) :: j3i
        real, dimension(1:5) :: a1s
      character(len=80) :: idata
! *****                                                                  
! *****  I N P U T - O U T P U T TAPE ASSIGNMENT STATEMENTS              
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
! ***** TOTAL NUMBER OF EXPECTED TEST                                    
           ivtotl =4                                                    
           zprog='FM111'                                                
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
        write(nuvi, 35300)                                              
35300 format(/1x, "IOFMTS - (353) ADDITIONAL FORMATTED" /16x,                  "DATA TRANSFERS" ,//2x,                                           "SUBSET REFS 12.9.5.2  13.1  13.5" )                     
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
! *******************************************************                
! **** TO DELETE A TEST USED CODE SHOWN IN TEST 1       *                
! **** REPLACE THE DELETE COMMENT WITH DELETE CODE      *                
! *******************************************************                
! T001*  TEST 1                                                          
! *****     TEST VARIOUS COMBINATION OF BZ AND BN EDIT          13.5.8   
! *****     DESCRIPTORS, INCLUDING USING EACH AS A LEADING      13.5.9(1)
! *****     DESCRIPTOR, AND PRECEDING IW, EW.D, AND FW.D DESCRIPTORS.    
! *****     BN AND BZ HAVE NO EFFECT ON OUTPUT.                 13.5.8   
! *****     CARDS 1-2                                                    
! *****                                                                  
           ivtnum = 1                                                   
! ****      TO DELETE TEST 1 - CARDS 1 THRU 5 MUST BE BYPASS             
! ****      USE THE FOLLOWING CODE:                                      
! ****      IVDELE=IVDELE+1                                              
! ****      WRITE (NUVI,80000) IVTNUM                                    
! ****      DO 0031 IPASS=1,5                                            
! 0011      FORMAT (A80)                                                 
! **** READ (IRVI,0011) IDATA                                            
! 0031      CONTINUE                                                     
! ****      COMMENT OUT OUT FOLLOWING LINES UNTIL NEXT TEST              
! *************************                                              
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
        read(irvi, 35301)i2i(1,2), ivi, a1s(3), jvi, kvi, a1s(2), avs,         a1s(1), i2i(1,1)                                           
35301 format(bz,(2i4, e10.1, bn, 2i4, f5.2, bz, f5.2, bn, e10.1))     
        write(nuvi, 35302)i2i(1,2), ivi, a1s(3), jvi, kvi, a1s(2), avs,        a1s(1), i2i(1,1)                                           
! ****************************TEST 1 ********************                
35302 format (" ",10x,"COMPUTED:  " , 2i5, 1x, e10.5, bn, 2i5, f6.1,      bz, f6.2, bn, 1x, e8.3, i5)                                     
70010 format (" ",10x,"CORRECT:  " ,                                    "  1110 2020 .30303E-07   44   55   6.6 70.07" ,                  " .888E+01   99" )                                           
           write (nuvi,70010)                                           
! *****     CARDS 3-4                                                    
        read(irvi, 35303) i3i(1,2,1), a1s(3), avs, ivi, i2i(1,1),              jvi, bvs, a1s(2), (i3i(kvi,1,1), kvi=1,2)                  
35303 format(bz, (i5, f5.0, bn, f5.2, 2i5))                           
! *****************************************************************      
        write(nuvi, 35304) i3i(1,2,1), a1s(3), avs, ivi, i2i(1,1),             jvi, bvs, a1s(2), i3i(1,1,1), i3i(2,1,1)                   
35304 format ( /bn, 11x,"COMPUTED:  " , i5, f7.0, bz, 1x, f5.2,           2(1x,i4),i5, f7.0, bz, 1x, f5.2, 2(1x, i4))                     
70011 format (" ",10x,"CORRECT:  " ,                                    " 23450 10345. 12.45 1235 1234" ,                                 " 2345  1345. 12.45 1235 1234" )                             
           write (nuvi,70011)                                           
! ****      CARD 5                                                       
        cvs = -0.0044                                                   
        read(irvi, 35305) ivi, avs, a1s(2), jvi, bvs                    
35305 format(bz, i5, f5.1, bn, f5.1, i5, bz, f5.1, i5)                
! **************************************************************         
        write(nuvi, 35306) ivi, avs, a1s(2), jvi, bvs, cvs, cvs         
35306 format (/11x,"COMPUTED:  " ,                                        i5, 2(3x, f2.1), i5, 3x, e5.1e1, 3x, f2.1, 3x, e6.1e1)          
70012 format (" ",10x,"CORRECT:  " ,                                    "     0   .0   .0    0   .0E+0   .0   -.4E-2" /)             
           write (nuvi,70012)                                           
! *****                                                                  
! T002*  TEST 2                                                          
! *****    TEST CASES WHERE THE NUMBER OF CHARACTERS TO BE     13.5.9(3) 
! *****    OUTPUT EXCEEDS THE SPECIFIED OUTPUT FIELD WIDTH,              
! *****    OR AN EXPONENT EXCEEDS ITS SPECIFIED LENGTH.                  
! ***************************************                                
           ivtnum = 2                                                   
! *****     SEE NOTES TEST 1 TO DELETE TEST (NO READS REQUIRED)          
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
        avs = 0.12345e+10                                               
! *********************************************************              
        write(nuvi, 35307) avs, avs, avs, avs, avs                      
35307 format (" ",10x,"COMPUTED: " ,                                    e9.5e1, 1x, e10.5e2, 1x, e11.5e3,1x,e11.5e4,1x,e10.5)           
70020 format (" ",10x,"CORRECT:  " ,                                    "********* .12345E+10 .12345E+010 ***********" ,                  " .12345E+10" /)                                             
           write (nuvi,70020)                                           
! T003*  TEST 3                                                          
! *****                                                                  
! *****    -  TEST THAT FW.D AND EW.D MAY HAVE MORE DIGITS     13.5.9(2) 
! *****       ON INPUT THAN THE PROCESSOR CAN USE.                       
! *****    -  READ IN AN ARRAY USING AN IMPLIED DO-LOOP, AND   12.8.2.3  
! *****       AND TEST VALUE OF THE IMPLIED DO-PARAMETER.      11.10     
! *****    -  USE AS A FORMAT AN INTEGER VARIABLE WHOSE VALUE  10.3      
! *****       IS ASSIGNED USING AN ASSIGNMENT STATEMENT.       12.4(2)   
! *****    -  TEST THAT ON INPUT, THE X-EDIT DESCRIPTOR MAY    13.5.3    
! *****       SPECIFY A POSITION BEYOND COLUMN 80 IF THERE ARE           
! *****       NO MORE ITEMS IN THE I/O LIST.                             
! *****                                                                  
           ivtnum = 3                                                   
! *****     CARDS 6-7                                                    
! *****     SEE NOTES TEST 1 TO DELETE TEST                              
! *****     CARDS 6 & 7 MUST BE BYPASSED                                 
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
        assign 35308 to jvi                                             
35308 format(2f5.2, f14.0 / e25.20, f5.2, 51x)                        
        read(irvi, jvi) (a1s(ivi), ivi=1,5)                             
! *************************************************************          
        assign 35309 to jvi                                             
35309 format (11x,"COMPUTED: " ,i5,1x,f5.2)                           
70030 format (" ",10x,"CORRECT:  " ,                                    "    6 33.33" /)                                             
        write(nuvi, jvi) ivi, a1s(5)                                    
           write (nuvi,70030)                                           
! *****                                                                  
! T004*  TEST 4                                                          
! *****    -  TEST NESTING OF 3 LEVELS OF PARENTHESES WITHIN A           
! *****       FORMAT STATEMENT.                                          
! *****     -  TEST DIFFERENT FORMS OF CHARACTER CONSTANTS USED   12.4(2)
! *****        AS A FORMAT SPECIFIER, INCLUDING BLANKS BEFORE     13.1.2 
! *****        THE FIRST PARENTHESIS, AND CHARCTERS AFTER THE            
! *****        LAST PARENTHESIS.                                         
! *****     -  2 CONSECUTIVE APOSTROPHES IN A H-EDIT DESCRIPTOR   13.5.2 
! *****     CARD 8                                                       
           ivtnum = 4                                                   
! *****     SEE NOTES TEST 1 TO DELETE TEST                              
! *****     NO READS REQUIRED                                            
           ivinsp=ivinsp+1                                              
           write (nuvi,80004) ivtnum                                    
        read(irvi, '  (3(1(2(I5))))')                                         (((j3i(ivi,jvi,kvi),ivi=1,1),jvi=1,2),kvi=1,3)              
! ***************************************************                    
        write(nuvi,                                                       '(/11X, "COMPUTED: " ,4(2X, I3)  )JUNK')                          (j3i(1,2,ivi),ivi=1,3)                                          
           write (nuvi,                                                      '(11X,"CORRECT:  " ,  "    2    4    6") ')                  
        write (nuvi,                                                      '(/11X,"COMPUTED:"," ''THAT''S ALL FOR NOW''")')                
70040 format (11x,"CORRECT:  " ,                                        "'THAT'S ALL FOR NOW'" )                                     
           write (nuvi,70040)                                           
! *****                                                                  
 0041 continue                                                          
! *****    END OF TEST SEGMENT 353                                       
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
        end program fm111
