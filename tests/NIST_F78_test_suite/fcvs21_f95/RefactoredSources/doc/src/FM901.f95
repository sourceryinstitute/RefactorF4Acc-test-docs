      program fm901
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM901               AFMTF - (023)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REFS
! *****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.2
! *****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  
! *****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  
! *****    PROGRAM SEGMENTS FOR CHARACTER DATA TYPES.            4.8     
! *****    TO TEST READ AND WRITE OF SUBSTRINGS.                 5.7     
! *****                                                                  
! *****  RESTRICTIONS OBSERVED                                           
! *****  *  ALL FORMAT STATEMENTS ARE LABELED                    12.8.2  
! *****  *  H AND X DESCRIPTORS ARE NEVER REPEATED               13.1.1  
! *****  *  FIELD WIDTH IS NEVER ZERO                            13.5.11 
! *****  *  IF AN I/O LIST SPECIFIES AT LEAST ONE LIST ITEM      13.3    
! *****     AT LEAST ONE REPEATABLE EDIT DESCRIPTOR MUST EXIST           
! *****     IN THE FORMAT SPECIFICATION.                                 
! *****  *  ITEMS IN I/O LIST CORRESPOND TO FORMAT DESCRIPTORS   13.3    
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
!  INPUT DATA TO THIS SEG. CONSISTS OF 5 DATA CARD IMAGES IN COLS. 1 - 52
! OL.      1-------------------------------------------------52          
! ARD  1   XYZ123:45$'),.JKLABCDEF67890MNOPQRSTUVW =+-*/(GHI             
! ARD  2   ONEFIVENINEELEVENSEVENTHREE                                   
! ARD  3   SQUARE THE WORLD IN 40 NIGHTS                                 
! ARD  4   DAYS  80AROUND                                                
! ARD  5   TO XXXXX NOT TO XXXX-  THAT IS THE QUESTIONXXBE ORBE          
! *****                                                                  
! *****  S P E C I F I C A T I O N S   SEGMENT 023                       
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
        character(len=13) :: a13vk
        character(len=27) :: a27vk
        character(len=29) :: a29vk
        character(len=36) :: a36vk
        character(len=43) :: b43vk
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
      ivtotl = 4                                                        
      zprog = 'FM901'                                                   
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
! *****    HEADER FOR SEGMENT 23                                         
        write (nuvi,02300)                                              
02300 format(" ", /1x," AFMTF - (023) FORMATTED DATA TRANSFER" //              1x," USING A-CONVERSION WITH SUBSTRINGS" //1x,                    " REFS - 12.9.5.2  13.3  13.5.11" )                      
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
! *****    TEST THAT DATA MAY BE READ IN A SERIES OF SUBSTRINGS,      5.7
! *****    NOT NECESSARILY IN THE ORDER OF POSITION IN THE STRING, 12.8.2
! *****    AND CAN BE WRITTEN AS A CHARACTER STRING.              13.5.11
! *****    SHOW ALSO THAT THE FULL FORTRAN CHARACTER SET CAN BE READ  3.1
! *****    (INCLUDES $ AND :)                                            
! *****                                                                  
! *****    INPUT CARD 1                                                  
        read(irvi, 02301) a36vk(24:29), a13vk(13:13), a36vk(30:31),            a13vk(11:12), a13vk(8:10), a36vk(10:12), a36vk(:6),               a36vk(32:), a36vk(13:23), a13vk(1:7), a36vk(7:9)           
02301 format(a6, a1, 2a2, a3, a3, a6, a5, a11, a7, a3)                
! T001*  TEST 1                                                          
           ivtnum = 1                                                   
           remrks = '2 SETS OF 2 COMPUTED LINES     '                   
           write (nuvi, 80004) ivtnum, remrks                           
           remrks = 'EXPECTED                       '                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020)                                          
        write (nuvi, 70010) a36vk(1:6), a36vk(7:9), a36vk(10:12),              a36vk(13:23), a36vk(24:29), a36vk(30:31), a36vk(32:36),           a36vk, a13vk(:7), a13vk(8:10), a13vk(11:12), a13vk(13:),          a13vk                                                      
70010 format (26x,a6,2(a3),a11,a6,a2,a5/26x,a36//26x,a7,a3,a2,a1/               26x,a13)                                                
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70011)                                          
70011 format(" ",16x,"CORRECT:  " ,22x,  "CORRESPONDING LINE(S) MUST MATCH")                                                            
           write (nuvi, 70012)                                          
70012 format(26x,"ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890" /                   26x," =+-*/(),.$':" )                                    
        read(irvi, 02303) a27vk                                         
! *****                                                                  
! *****    TEST THAT A CHARACTER VARIABLE CAN BE OUTPUT AS SUBSTRINGS.   
! *****                                                           13.5.11
! *****    INPUT CARD 2                                                  
02303 format(a27)                                                     
! T002*  TEST 2                                                          
           ivtnum = 2                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write(nuvi, 70020) a27vk(1:3), a27vk(23:27), a27vk(4:7),                a27vk(18:22), a27vk(8:11), a27vk(12:17)                   
70020 format(26x,a3,a6,a5,a6,a5,a7)                                   
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70022)                                          
70022 format(26x,"ONE THREE FIVE SEVEN NINE ELEVEN" )              
! *****                                                                  
! *****    TEST THAT A SUBSTRING CAN BE READ IN, AND PARTIALLY REPLACE   
! *****    A PREVIOUSLY READ CHARACTER STRING.                    13.5.11
! *****    THIS SHOWS THAT THE LENGTH IS DERIVED FROM THE SUBSTRING,     
! *****    AND NOT THE CHARACTER VARIABLE LENGTH.                        
! *****                                                                  
! *****    INPUT CARDS 3-4                                               
        read(irvi, 02305)  a29vk, a29vk(24:29), a29vk(21:22), a29vk(1:6)
02305 format(a29/a,2a)                                                
! T003*  TEST 3                                                          
           ivtnum = 3                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write(nuvi, 70030) a29vk(1:3), a29vk(4:21), a29vk(22:29)        
70030 format (26x,3(a))                                               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70032)                                          
70032 format(25x," AROUND THE WORLD IN 80 DAYS  " )                
! *****                                                                  
! *****    SPECIFIED FIELD WIDTH IN A A-EDIT DESCRIPTOR                  
! *****    IS DIFFERENT FROM SUBSTRING LENGTH                            
! *****                                                                  
! *****    INPUT CARD 5                                                  
        read(irvi, 02307) b43vk, b43vk(4:8), b43vk(17:20)               
02307 format(a43, a7, a2)                                             
! T004*  TEST 4                                                          
           ivtnum = 4                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write (nuvi, 70040) b43vk(:)                                    
70040 format (26x,a20)                                                
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70042)                                          
70042 format(26x,"TO BE OR NOT TO BE  " )                          
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
! *****    END OF TEST SEGMENT 023                                       
        stop                                                            
        end program fm901
