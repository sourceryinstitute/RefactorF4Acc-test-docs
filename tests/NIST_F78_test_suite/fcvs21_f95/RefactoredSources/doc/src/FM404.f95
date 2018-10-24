      program fm404
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM404               AFMTS - (022)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REFS
! *****    TO TEST SIMPLE FORMAT AND FORMATTED DATA              12.9.5.2
! *****    TRANSFER STATEMENTS IN EXTERNAL SEQUENTIAL I/O SO     13.1.1  
! *****    THAT THESE FEATURES MAY BE USED IN OTHER TEST         12.8.1  
! *****    PROGRAM SEGMENTS FOR CHARACTER DATA TYPES.            4.8     
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
!  INPUT DATA TO THIS SEG. CONSISTS OF 6 DATA CARD IMAGES IN COLS. 1 - 55
! OL.      1--------------------------------------------47               
! ARD  1   QRSTMNOPIJKLYZ127890ABCD3456EFGHUVWX/(),.' =+-*               
! ARD  2   AABABCABCDABCDEABCDEFWXYZWXYZWXYZWXYZWXYZWXYZ                 
! ARD  3   112123123412345123456                                         
! ARD  4   GGGGHHHHIIIIJJJJ                                              
! ARD  5   ----LLLL                                                      
! ARD  6   ....NNNN                                                      
! *****                                                                  
! *****  S P E C I F I C A T I O N S   SEGMENT 022                       
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
        character(len=1) :: a1vk
        character(len=2) :: a2vk
        character(len=3) :: a3vk
        character(len=4) :: a4vk
        character(len=4), dimension(1:6) :: a41k
        character(len=4), dimension(1:2,1:2,1:3) :: a43k
        character(len=5) :: a5vk
        character(len=6) :: a6vk
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
      ivtotl = 5                                                        
      zprog = 'FM404'                                                   
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
! *****    HEADER FOR SEGMENT 22                                         
        write (nuvi,02200)                                              
02200 format(" ", /1x," AFMTS - (022) FORMATTED DATA TRANSFER" //              1x," USING A-CONVERSION" //1x,                                    " SUBSET REFS - 12.9.5.2  13.3  13.5.11" )               
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
! *****    TESTS THAT ALL FORTRAN (SUBSET) CHARACTERS MAY BE READ.    3.1
! *****                                                                  
! *****    INPUT CARD 1                                                  
        read(irvi, 02201) a43k(1,1,1), a43k(1,1,2), a43k(1,1,3),               a43k(1,2,1), a43k(1,2,2), a43k(1,2,3), a43k(2,1,1),               a43k(2,1,2), a43k(2,1,3), a6vk, a5vk                       
02201 format(9a4, a6, a5)                                             
! T001*  TEST 1                                                          
           ivtnum = 1                                                   
           remrks = '2 COMPUTED LINES EXPECTED'                         
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write(nuvi, 70010) a43k(1,2,3), a43k(2,1,2), a43k(1,1,3),              a43k(1,1,2), a43k(1,1,1), a43k(2,1,3), a43k(1,2,1),               a43k(2,1,1), a43k(1,2,2), a5vk, a6vk                       
70010 format(26x,9a4/25x,a5,a6)                                       
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70011)                                          
70011 format(" ",16x,"CORRECT:  " ,22x,  "CORRESPONDING LINE(S) MUST MATCH")                                                            
           write (nuvi, 70012)                                          
70012 format(26x, "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890" /                     26x,"=+-*/(),.'" )                                    
! *****    INPUT CARD 2                                                  
! *****                                                                  
! *****    AW CONVERSION IS USED IN THE FORMAT STATEMENTS.         3.5.11
! *****    SOME FORMAT DESCRIPTORS ARE REPEATED.                         
! *****    THE FOLLOWING THREE CASES ARE USED FOR BOTH INPUT AND OUTPUT. 
! *****      INPUT FIELD WIDTH   =  CHARACTER VARIABLE LENGTH            
! *****      INPUT FIELD WIDTH   <  CHARACTER VARIABLE LENGTH            
! *****      INPUT FIELD WIDTH   >  CHARACTER VARIABLE LENGTH            
! *****                                                                  
        read(irvi, 02203) a41k(1), a41k(2), a41k(3), a41k(4), a41k(5),         a41k(6), a1vk, a2vk, a3vk, a4vk, a5vk, a6vk                
02203 format(a1, a2, 1a3, a4, a5, 1(a6), a4, 2a4, 3(a4))              
! T002*  TEST 2                                                          
           ivtnum = 2                                                   
           remrks = '2 COMPUTED LINES EXPECTED'                         
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write(nuvi, 70020) a41k(1), a41k(2), a41k(3), a41k(4), a41k(5),        a41k(6), a6vk, a5vk, a4vk, a3vk, a2vk, a1vk                
70020 format(26x,a4,a4,4a4/26x,a6,a5,a4,a3,a2,a1)                     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70011)                                          
           write (nuvi, 70022)                                          
70022 format(26x,"A   AB  ABC ABCDBCDECDEF" /                                  26x,"WXYZ  WXYZ WXYZXYZYZZ" )                         
           ivtnum = 3                                                   
! *****                                                                  
! T003*  TEST 3                                                          
           remrks = '2 COMPUTED LINES EXPECTED'                         
           write (nuvi, 80004) ivtnum, remrks                           
           write (nuvi, 80020)                                          
        write(nuvi, 70030) a41k(1), a41k(2), a41k(3), a41k(4), a41k(5),        a41k(6), a1vk, a2vk, a3vk, a4vk, a5vk, a6vk                
70030 format(26x,a1,a2,a3,a4,a5,a6/23x,4(a4),a4,a4)                   
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70011)                                          
           write (nuvi, 70032)                                          
70032 format(26x,"AABABCABCD BCDE  CDEF" /                                     26x,"Z  YZ XYZWXYZWXYZWXYZ" )                         
        read(irvi, 02206) a1vk, a2vk, a3vk, a4vk, a5vk, a6vk            
! *****                                                                  
! *****    A CONVERSION IS USED IN THE FORMAT STATEMENTS.          3.5.11
! *****    SOME FORMAT DESCRIPTORS ARE REPEATED.                         
! *****    READ WITH A-EDIT DESCRIPTOR, A STRING, FOLLOWED BY ANOTHER    
! *****    FIELD TO SHOW THAT THE POINTER PICKS UP THE NEXT FIELD        
! *****    FOLLOWING THE COUNT OF THE LENGTH OF THE DECLARED VARIABLE.   
! *****                                                                  
! *****    INPUT CARD 3                                                  
02206 format(a, 2a, 3(a))                                             
! T004*  TEST 4                                                          
           ivtnum = 4                                                   
           write (nuvi, 80004) ivtnum                                   
           write (nuvi, 80020)                                          
        write(nuvi, 70040) a1vk, a2vk, a3vk, a4vk, a5vk, a6vk           
70040 format(26x,6a)                                                  
           ivinsp = ivinsp + 1                                          
           write (nuvi, 80022)                                          
           write (nuvi, 70042)                                          
70042 format(26x,"112123123412345123456" )                         
! *****                                                                  
! *****    TEST THAT A SLASH ON INPUT CAUSES THE UNPROCESSED CHARACTERS  
! *****    TO BE SKIPPED.                                          13.5.4
! *****    ALSO TEST THAT AN APOSTROPHE MAY BE USED INSTEAD OF AN  13.5.1
! *****    H-EDIT DESCRIPTOR.                                      13.5.2
! *****                                                                  
! *****    INPUT CARD 4                                                  
        read(irvi, 02208) a41k(2), a41k(1), a41k(4), a41k(3)            
02208 format(4a4)                                                     
! *****    INPUT CARDS 5-6                                               
        read(irvi, 02209) a41k(2), a41k(4), a41k(3)                     
02209 format(a4 / 2a4)                                                
! T005*  TEST 5                                                          
           ivtnum = 5                                                   
           remrks = '2 IDENTICAL COMPUTED LINES     '                   
           write (nuvi, 80004) ivtnum, remrks                           
           remrks = 'EXPECTED                       '                   
           write (nuvi, 80050) remrks                                   
           write (nuvi, 80020)                                          
        write(nuvi, 70050) a41k(2), a41k(1), a41k(4), a41k(3)           
70050 format(26x,'----HHHH....NNNN'/26x,3(a4),a4)                     
           ivinsp = ivinsp + 1                                          
           write (nuvi, 70011)                                          
           write (nuvi, 70052)                                          
70052 format (26x,"----HHHH....NNNN" )                             
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
! *****    END OF TEST SEGMENT 022                                       
        stop                                                            
        end program fm404
