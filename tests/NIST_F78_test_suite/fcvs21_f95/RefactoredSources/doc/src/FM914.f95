      program fm914
! ***********************************************************************
! *****   FM914                                                          
! *****                       INQU1 - (430)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INQUIRE BY UNIT ON SEQUENTIAL, FORMATTED FILES   12.10.3 
! *****                                                                  
! *****    THE TESTS IN THIS UNIT ARE ONLY PERFORMED ON A                
! *****    UNIT THAT IS CONNECTED FOR SEQUENTIAL, FORMATTED ACCESS       
! *****    (ANS REF. 12.2.4.1 AND 12.9.5.2)                              
! *****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             
! *****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       
! ***********************************************************************
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
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: i08
      integer :: nuvi
      integer :: imvi
      integer :: ivtnum
      integer :: jvi
      integer :: kvi
      real :: go
      real :: to
        logical :: avb
        logical :: bvb
        character(len=10) :: b10vk
        character(len=10) :: c10vk
        character(len=11) :: e11vk
        character(len=10) :: f10vk
        character(len=10) :: h10vk
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
! *****                                                                  
!      I08 CONTAINS THE UNIT NUMBER FOR A SEQUENTIAL FORMATTED FILE.     
      i08 = 14                                                          
! X080   REPLACED BY FEXEC X-080 CONTROL CARD (SEQ. FILE UNIT NUMBER).   
!      SPECIFYING I08 = NN OVERRIDES THE DEFAULT I08 = 14.               
! *****                                                                  
! *****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    
! *****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            
! *****    SEQUENTIAL, FORMATTED FILE.                                   
! *****                                                                  
      nuvi = i02                                                        
      imvi = i08                                                        
      zprog = 'FM914'                                                   
      ivtotl = 1                                                        
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
        write(nuvi,43000)                                               
43000 format(" ", / " INQU1 - (430) INQUIRE BY UNIT" //                        " SEQUENTIAL FORMATTED FILE, CONNECTED BY OPEN" //                " ANS REF. - 12.10.3" )                                  
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
! *****    OPEN FILE                                                     
! *****                                                                  
        open(unit=imvi, access='SEQUENTIAL', form='FORMATTED',                 blank='NULL')                                              
           ivtnum = 1                                                   
! *****                                                                  
! T001*  TEST 1 - FIRST INQUIRE (AFTER OPEN)                             
        inquire(unit=imvi, exist=avb, opened=bvb, number=jvi,                     access=b10vk, sequential=c10vk, form=e11vk,                       formatted=f10vk, blank=h10vk, err=20011, iostat=kvi)    
        if (kvi  /=  0) goto 20010                                     
        if (.not. avb) goto 20010                                      
        if (.not. bvb) goto 20010                                      
        if (jvi  /=  imvi) goto 20010                                  
        if (b10vk  /=  'SEQUENTIAL') goto 20010                        
        if (c10vk  /=  'YES') goto 20010                               
        if (e11vk  /=  'FORMATTED') goto 20010                         
        if (f10vk  /=  'YES' ) goto 20010                              
        if (h10vk  /=  'NULL') goto 20010                              
           write (nuvi, 80002) ivtnum                                   
           ivpass = ivpass + 1                                          
           goto 0011                                                   
20011 continue                                                     
           write (nuvi, 20021) ivtnum                                   
20021 format (" ",2x,i3,4x," FAIL",12x,                                 "ERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)" /)          
           goto 20012                                                  
20010 continue                                                     
           write (nuvi, 20020) ivtnum                                   
20020 format(" ",2x,i3,4x," FAIL",12x,                                  "ERROR IN AN INQUIRE SPECIFIER" /)                           
20012 continue                                                     
           ivfail = ivfail + 1                                          
           write (nuvi, 20030) kvi,avb,bvb,jvi,b10vk,c10vk,e11vk,                                f10vk,h10vk                              
20030 format (" ",16x,"COMPUTED: " ,"IOSTAT=",i1,", EXIST=",l1,                 " ,OPENED=",l1,", NUMBER=",i4,","/                                " ",26x,"ACCESS=",a10,", SEQUENTIAL=" ,a3,", FORM=",              a9,","/" ",26x,"FORMATTED=" ,a3,", BLANK=",a4)       
           write (nuvi, 20040) imvi                                     
20040 format (" ",16x,"CORRECT:  " ,"IOSTAT=0, EXIST=T, " ,                     "OPENED=T, NUMBER=" ,i4,","/                                      " ",26x,"ACCESS=SEQUENTIAL, SEQUENTIAL=YES, FORM=" ,              "FORMATTED," /" ",26x,"FORMATTED=YES, BLANK=NULL" )  
 0011   continue                                                        
! *****                                                                  
        rewind imvi                                                     
        close(unit=imvi, status='DELETE')                               
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
! *****    END OF TEST SEGMENT 914                                       
        stop                                                            
        end program fm914
