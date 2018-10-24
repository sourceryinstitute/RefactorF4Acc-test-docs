      program fm921
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM921                                                          
! *****                       INQF4 - (441)                              
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                         ANS REF 
! *****    TEST INQUIRE BY FILE ON DIRECT, UNFORMATTED FILE      12.10.3 
! *****                                                                  
! *****    THE TESTS IN THE UNIT ARE ONLY PERFORMED ON A                 
! *****    FILE THAT IS CONNECTED FOR DIRECT, UNFORMATTED ACCESS         
! *****    (ANS REF. 12.2.4.2 AND 12.9.5.1)                              
! *****    THIS TEST PERFORMS AN EXPLICIT OPEN, AND PERFORMS             
! *****    A CLOSE WITH STATUS='DELETE' AT THE END OF THE SEGMENT.       
! *****    THIS SEGMENT TESTS THAT AN INQUIRE IS PERFORMED CORRECTLY     
! *****    BEFORE READING OR WRITING TO THE FILE, AFTER WRITING TO       
! *****    THE FILE, AND AFTER READING FROM THE FILE.                    
! *****                                                                  
! *****  NOTE:                                                           
! *****    AN INQUIRE STATEMENT IS NEEDED TO TEST THE READ AND           
! *****    WRITE OF MORE THAN A SINGLE RECORD AT A TIME, IN ORDER TO     
! *****    DETERMINE THAT THE RECORD NUMBER IS ADVANCED THE CORRECT      
! *****    NUMBER (ONE MORE THAN THE RECORD NUMBER LAST READ OR WRITTEN).
! *****    THIS TEST WILL BE PERFORMED IN THE SEGMENTS WHICH TEST        
! *****    DIRECT ACCESS FILES - DIRAF3 (412).                           
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
      integer :: i10
      integer :: mvi
      integer :: nuvi
      integer :: iovi
      integer :: ivtnum
      integer :: jvi
      integer :: kvi
      integer :: lvi
      integer :: ivi
      real :: go
      real :: to
        logical :: avb
        logical :: bvb
        character(len=10) :: b10vk
        character(len=10) :: d10vk
        character(len=11) :: e11vk
        character(len=10) :: g10vk
! ***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   
! X20   REPLACED BY FEXEC X-20  CONTROL CARD.  X-20  IS FOR REPLACING    
        character(len=15) :: cdir
        character(len=15) :: cseq
!       THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-100     
!       (PROGRAM VARIABLE CDIR) IF NOT VALID FOR THE PROCESSOR.          
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
! *****    THE FOLLOWING STATEMENT MUST BE CHANGED IF                    
! *****    THE UNIT GIVEN IS NOT CAPABLE OF BEING OPENED AS A            
! *****    DIRECT, UNFORMATTED FILE.                                     
! *****                                                                  
!      I10 CONTAINS THE UNIT NUMBER FOR A DIRECT, UNFORMATTED FILE.      
      i10 = 24                                                          
! X100   REPLACED BY FEXEC X-100 CONTROL CARD (DIR. FILE UNIT NUMBER).   
!      SPECIFYING I10 = NN OVERRIDES THE DEFAULT I10 = 24.               
! *****                                                                  
! *****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             
! *****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               
! *****  UNFORMATTED FILE.                                               
! *****                                                                  
!      CDIR CONTAINS THE FILE NAME FOR UNIT I10.                         
      cdir = '        DIRFILE'                                          
!                                                                        
! X201   REPLACED BY FEXEC X-201 CONTROL CARD.  CX201 IS FOR SYSTEMS     
!      REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    
!      X-100 THAN THE DEFAULT CDIR = '        DIRFILE'.                  
! *****                                                                  
! *****    THE FOLLOWING STATEMENT MUST BE CHANGED IF 40 IS              
! *****    NOT A VALID RECORD LENGTH.                                    
      mvi = 40                                                          
! *****                                                                  
      nuvi = i02                                                        
      iovi = i10                                                        
      zprog = 'FM921'                                                   
      ivtotl = 3                                                        
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
        write(nuvi,44100)                                               
44100 format(" ", / " INQF4 - (441) INQUIRE BY FILE" //                        " DIRECT ACCESS UNFORMATTED FILE" //                              " ANS REF. - 12.10.3" )                                  
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
        open(file=cdir, unit=iovi, access='DIRECT', recl=mvi,                  form='UNFORMATTED')                                        
           ivtnum = 1                                                   
! *****                                                                  
! T001*  TEST 1 -  FIRST INQUIRE (AFTER OPEN)                            
        inquire(file=cdir, exist=avb, opened=bvb, number=jvi,                     access=b10vk, direct=d10vk, recl=kvi, nextrec=lvi,                form=e11vk, unformatted=g10vk, err=20014, iostat=ivi)   
! *****                                                                  
        if (ivi  /=  0) goto 20010                                     
        if (.not. avb) goto 20010                                      
        if (.not. bvb) goto 20010                                      
        if (jvi  /=  iovi) goto 20010                                  
        if (b10vk  /=  'DIRECT') goto 20010                            
        if (d10vk  /=  'YES') goto 20010                               
        if (kvi  /=  mvi) goto 20010                                   
        if (lvi  /=  1) goto 20010                                     
        if (e11vk  /=  'UNFORMATTED') goto 20010                       
        if (g10vk  /=  'YES' ) goto 20010                              
           write (nuvi, 80002) ivtnum                                   
           ivpass = ivpass + 1                                          
           goto 0011                                                   
20014 continue                                                     
           write (nuvi, 20015) ivtnum                                   
20015 format (" ",2x,i3,4x," FAIL",12x,                                 "ERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)" /)          
           goto 20016                                                  
20010 continue                                                     
           write (nuvi, 20011) ivtnum                                   
20011 format(" ",2x,i3,4x," FAIL",12x,                                  "ERROR IN AN INQUIRE SPECIFIER" /)                           
20016 ivfail = ivfail + 1                                          
           write (nuvi, 20012) ivi,avb,bvb,jvi,b10vk,d10vk,kvi,                                  lvi,e11vk,g10vk                          
20012 format (" ",16x,"COMPUTED: " ,"IOSTAT=",i1,", EXIST=",l1,                 " ,OPENED=",l1,", NUMBER=",i4,","/                                " ",26x,"ACCESS=",a6,", DIRECT=",a3,", RECL=",                    i4,","/" ",26x,"NEXTREC=",i4,", FORM=",                           a11,","/" ",26x,"UNFORMATTED=" ,a3)                  
           write (nuvi, 20013) iovi,mvi                                 
20013 format (" ",16x,"CORRECT:  " ,"IOSTAT=0, EXIST=T, " ,                     "OPENED=T, NUMBER=" ,i4,","/                                      " ",26x,"ACCESS=DIRECT, DIRECT=YES, RECL=" ,                      i4,","/" ",26x,"NEXTREC=   1, FORM=UNFORMATTED," /                " ",26x,"UNFORMATTED=YES" )                          
 0011   continue                                                        
! *****                                                                  
! *****    WRITE A RECORD TO FILE                                        
44103 write(iovi, rec=1) jvi                                          
! *****                                                                  
! T002*  TEST 2 -  SECOND INQUIRE (AFTER WRITE)                          
           ivtnum = 2                                                   
! *****    THIS INQUIRE ONLY TESTS THE DIRECT, RECL, AND NEXTREC         
! *****    AS THE OTHER SPECIFIERS HAVE BEEN PREVIOUSLY TESTED           
        inquire(file=cdir, direct=d10vk, recl=kvi, nextrec=lvi,                   err=20024, iostat=ivi)                                  
! *****                                                                  
        if (ivi  /=  0) goto 20020                                     
        if (d10vk  /=  'YES') goto 20020                               
        if (kvi  /=  mvi) goto 20020                                   
        if (lvi  /=  2) goto 20020                                     
           write (nuvi, 80002) ivtnum                                   
           ivpass = ivpass + 1                                          
           goto 0021                                                   
20024 continue                                                     
           write (nuvi, 20025) ivtnum                                   
20025 format (" ",2x,i3,4x," FAIL",12x,                                 "ERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)" /)          
           goto 20026                                                  
20020 continue                                                     
           write (nuvi, 20021) ivtnum                                   
20021 format(" ",2x,i3,4x," FAIL",12x,                                  "ERROR IN AN INQUIRE SPECIFIER" /)                           
20026 ivfail = ivfail + 1                                          
           write (nuvi, 20022) ivi,d10vk,kvi,lvi                        
20022 format (" ",16x,"COMPUTED: " ,"IOSTAT=",i1,", DIRECT=",a3,                " ,RECL=",i4,", NEXTREC=" ,i4)                       
           write (nuvi, 20023) mvi                                      
20023 format (" ",16x,"CORRECT:  " ,"IOSTAT=0, DIRECT=YES" ,                    " ,RECL=",i4,", NEXTREC=   2" )                      
 0021   continue                                                        
! *****                                                                  
! *****    READ A RECORD FROM FILE                                       
44106 read(iovi, rec=1) jvi                                           
! *****                                                                  
! T003*  TEST 3 - THIRD INQUIRE (AFTER READ)                             
           ivtnum = 3                                                   
! *****    THIS INQUIRE ONLY TESTS THE DIRECT, RECL, AND NEXTREC         
! *****    AS THE OTHER SPECIFIERS HAVE BEEN PREVIOUSLY TESTED           
        inquire(file=cdir, direct=d10vk, recl=kvi, nextrec=lvi,                   err=20034, iostat=ivi)                                  
! *****                                                                  
        if (ivi  /=  0) goto 20030                                     
        if (d10vk  /=  'YES') goto 20030                               
        if (kvi  /=  mvi) goto 20030                                   
        if (lvi  /=  2) goto 20030                                     
           write (nuvi, 80002) ivtnum                                   
           ivpass = ivpass + 1                                          
           goto 0031                                                   
20034 continue                                                     
           write (nuvi, 20035) ivtnum                                   
20035 format (" ",2x,i3,4x," FAIL",12x,                                 "ERROR IN EXECUTION OF INQUIRE STATEMENT (ERR=)" /)          
           goto 20036                                                  
20030 continue                                                     
           write (nuvi, 20031) ivtnum                                   
20031 format(" ",2x,i3,4x," FAIL",12x,                                  "ERROR IN AN INQUIRE SPECIFIER" /)                           
20036 ivfail = ivfail + 1                                          
           write (nuvi, 20032) ivi,d10vk,kvi,lvi                        
20032 format (" ",16x,"COMPUTED: " ,"IOSTAT=",i1,", DIRECT=",a3,                " ,RECL=",i4,", NEXTREC=" ,i4)                       
           write (nuvi, 20033) mvi                                      
20033 format (" ",16x,"CORRECT:  " ,"IOSTAT=0, DIRECT=YES" ,                    " ,RECL=",i4,", NEXTREC=   2" )                      
 0031   continue                                                        
! *****                                                                  
        close(unit=iovi, status='DELETE')                               
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
! *****    END OF TEST SEGMENT 441                                       
        stop                                                            
        end program fm921
