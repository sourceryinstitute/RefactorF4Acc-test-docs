      program fm905
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM905                                                          
! *****                       LSTDO1 - (371)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST LIST DIRECTED OUTPUT ON                          13.6    
! *****    INTEGER, REAL, LOGICAL, AND CHARACTER DATA TYPES      12.4    
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
! *****  S P E C I F I C A T I O N S  SEGMENT 371                        
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
      integer :: ivi
      integer :: jvi
      integer :: kvi
      integer :: lvi
      integer :: mvi
      real :: avs
      real :: bvs
      real :: cvs
      real :: dvs
        logical, dimension(1:3) :: b1b
        logical :: avb
        character(len=5) :: a5vk
        character(len=9) :: a9vk
        character(len=33) :: a33vk
        character(len=82) :: a82vk
        character(len=5), dimension(1:4) :: a51k
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
      ivtotl = 10                                                       
      zprog = 'FM905'                                                   
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
! *****    HEADING FOR SEGMENT 371                                       
        write(nuvi,37100)                                               
37100 format(" ", /" LSTDO1 - (371) " ,                                        " LIST DIRECTED OUTPUT FOR SUBSET DATA TYPES" //                  " ANS REF. - 13.6  12.4" )                               
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
           write (nuvi, 70000)                                          
70000 format (" ",48x,"THE CORRECT LINE OF EACH TEST  " /                       " ",48x,"IS HOLLERITH INFORMATION.      " /                       " ",48x,"COLUMN SPACING,  LINE BREAKS,  " /                       " ",48x,"AND THE NUMBER OF DECIMAL      " /                       " ",48x,"PLACES FOR REAL NUMBERS ARE    " /                       " ",48x,"PROCESSOR DEPENDENT.           " /                       " ",48x,"EITHER E OR F FORMAT MAY BE    " /                       " ",48x,"USED FOR REAL NUMBERS.         " /)         
! T001*  TEST 1 - INTEGER                                                
           ivtnum = 1                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        ivi = 2                                                         
        write(nuvi, *) ivi                                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70011)                                          
70011 format (" ",6x,"2")                                          
! T002*  TEST 2 - SEVERAL INTEGERS                                       
           ivtnum = 2                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        ivi = 1                                                         
        jvi = 3                                                         
        kvi = 5                                                         
        lvi = 7                                                         
        mvi = 9                                                         
        write(nuvi, *) ivi, jvi, kvi, lvi, mvi                          
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70021)                                          
70021 format (" ",6x,"1  3  5  7  9" )                             
! T003*  TEST 3 - REAL                                                   
           ivtnum = 3                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        avs = 2.5                                                       
        write(nuvi, *) avs                                              
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70031)                                          
70031 format (" ",6x,"2.5")                                        
! T004*  TEST 4 - SEVERAL REALS                                          
           ivtnum = 4                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        avs = 0.25e-10                                                  
        bvs = 0.25                                                      
        cvs = 0.25e+3                                                   
        dvs = 0.25e+10                                                  
        write(nuvi, *) avs, bvs, cvs, dvs                               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70041)                                          
70041 format(" ",6x," 2.5E-11  0.25  250.0   2.5E+09" )            
! T005*  TEST 5 - IMPLIED-DO TO PRINT ARRAY OF LOGICALS                  
           ivtnum = 5                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        b1b(1) = .true.                                                 
        b1b(2) = .false.                                                
        b1b(3) = .true.                                                 
        write(nuvi, *) (b1b(ivi), ivi = 1,3)                            
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70051)                                          
70051 format(" ",6x,"T  F  T")                                     
! T006*  TEST 6 - LIST OF CHARACTER VALUES, USING ARRAY NAME             
           ivtnum = 6                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        a51k(1) = 'ONE  '                                               
        a51k(2) = 'TWO  '                                               
        a51k(3) = 'THREE'                                               
        a51k(4) = 'FOUR '                                               
        write(nuvi, *) a51k                                             
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70061)                                          
70061 format(" ",6x,"ONE  TWO  THREEFOUR " )                          
! T007*  TEST 7 - MIXED LIST                                             
           ivtnum = 7                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        ivi = -3                                                        
        avs = 15.25                                                     
        avb = .true.                                                    
        a5vk = 'HELLO'                                                  
        write(nuvi,*) ivi, avs, a5vk, avb                               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70071)                                          
70071 format(" ",6x,"-3  15.25  HELLO  T" )                        
! T008*  TEST 8 - CHARACTER CONSTANT CONTAINING EMBEDDED '               
           ivtnum = 8                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        a9vk = '5 O''CLOCK'                                             
        write(nuvi, *) a9vk                                             
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70081)                                          
70081 format(" ",6x,"5 O'CLOCK")                                   
! T009*  TEST 9 - CHARACTER CONSTANT SPILLING OVER RECORD BOUNDARY       
           ivtnum = 9                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        a5vk = 'SHORT'                                                  
        a33vk = 'THIS IS A LONGER CHARACTER STRING'                     
        a82vk = '123456789012345678901234567890123456789012345678901234567890123456789012'                                                
        write(nuvi, *) a5vk, a33vk, a82vk                               
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70091)                                          
70091 format(" ", "SHORT  THIS IS A LONGER CHARACTER STRING" ,                 " 123456789012345678901234567890123456789" /                      " ","012345678901234567890123456789012" )             
! T010*  TEST 10 - SEVERAL IDENTICAL VALUES                              
           ivtnum = 10                                                  
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        ivi = 5                                                         
        jvi = 5                                                         
        kvi = 5                                                         
        lvi = 5                                                         
        mvi = 5                                                         
        write(nuvi, *) ivi, jvi, kvi, lvi, mvi                          
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70101)                                          
70101 format(" ",6x,"5  5  5  5  5  OR  5*5" )                     
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
! *****    END OF TEST SEGMENT 371                                       
        stop                                                            
        end program fm905
