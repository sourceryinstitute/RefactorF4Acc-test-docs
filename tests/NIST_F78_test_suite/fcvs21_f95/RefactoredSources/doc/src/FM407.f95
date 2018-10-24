      program fm407
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM407                                                          
! *****                       DIRAF1 - (410)                             
! *****   THIS PROGRAM CALLS SUBROUTINE SN408                            
! ***********************************************************************
! *****  TESTING OF DIRECT ACCESS FILES                        SUBSET REF
! *****          UNFORMATED RECORDS ONLY                         12.10.1 
! *****                                                                  
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
! *****  S P E C I F I C A T I O N S  SEGMENT 410                        
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
      integer :: nuvi
      integer :: iuvi
      integer :: ivi
      real :: avs
      integer :: ivtnum
      integer :: ivcomp
      real :: bvs
      integer :: kvi
      real :: go
      real :: to
      integer :: jvi
        integer, dimension(1:10) :: l1i
        integer, dimension(1:10) :: k1i
        integer, dimension(1:10) :: m1i
        real, dimension(1:10) :: f1s
        real, dimension(1:10) :: g1s
        character(len=4) :: a4vk
        character(len=4) :: b4vk
        character(len=4), dimension(1:10) :: a41k
        character(len=4), dimension(1:10) :: b41k
        logical :: avb
        logical :: bvb
        logical, dimension(1:10) :: c1b
        logical, dimension(1:10) :: d1b
! ***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   
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
! *****                                                                  
      nuvi = i02                                                        
      ivtotl = 4                                                        
      zprog = 'FM407'                                                   
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
! *****                                  FILE NUMBER ASSIGNMENT          
      iuvi = i10                                                        
! *****                                                                  
! *****    HEADER FOR SEGMENT 410                                        
       write(nuvi,41000)                                                
41000 format(" ",/ " DIRAF1 - (410) DIRECT ACCESS UNFORMATTED FILE" //           " SUBSET REF. - 12.10.1" )                              
! ****                                                                   
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! *****                                                                  
        write (nuvi, 41099)                                             
41099 format (" ",48x,"EACH TEST READS 10 RECORDS AND " /                       " ",48x,"EACH RECORD IS CHECKED, I.E.,  " /                       " ",48x,"THERE ARE 10 SUBTESTS MADE FOR " /                       " ",48x,"EACH TEST                      " )             
! *****                                                                  
        call sn408(l1i,k1i,m1i,f1s,g1s,c1b,d1b,a41k,b41k)

! *****                                                                  
        open(iuvi, access='DIRECT',recl=132)                            
! *****                      WRITE 10 RECORDS IN SEQUENCE, REC = 1 TO 10 
        do ivi = 1, 10                                            
        avs = f1s (ivi)                                                 
        a4vk = a41k (ivi)                                               
        avb = c1b (ivi)                                                 
        write(iuvi, rec= ivi) ivi, avs, a4vk, avb                       
         end do
! T001*  TEST 1                         READ RECORDS 1 TO 10 IN SEQUENCE 
           ivtnum = 1                                                   
           ivcomp = 0                                                   
        do ivi = 1, 10                                            
        read(iuvi, rec = ivi) kvi, bvs, b4vk, bvb                       
        if (ivi  /=  kvi) goto 20010                                      !Break
        if (b4vk  /=  a41k(ivi)) goto 20010                               !Break
        if ((bvb .and. .not. c1b(ivi)) .or.                                   (.not. bvb .and. c1b(ivi))) goto 20010                        !Break
        if (bvs  /=  f1s(ivi)) goto 20010                                !Break
        goto 41002                                                     
20010 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, ivi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, ivi, f1s(ivi),                               a41k(ivi), c1b(ivi)                      
70010 format (" ",2x,i3,4x," FAIL ON REC " ,i2)                    
70020 format (" ",16x,"COMPUTED: " ,i2,1x,f5.2,1x,a4,1x,l1/                     " ",16x,"CORRECT:  " ,i2,1x,f5.2,1x,a4,1x,l1)        
41002 continue                                                     
      end do
           if (ivcomp - 0) 0011, 10010, 0011                            
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0011      continue                                                     
! T002*  TEST 2            READ RECORDS NOT IN SEQUENCE OF RECORD NUMBER 
           ivtnum = 2                                                   
           ivcomp = 0                                                   
        do ivi = 1, 10                                            
        jvi = l1i(ivi)                                                  
        read(iuvi, rec = jvi) kvi, bvs, b4vk, bvb                       
        if (kvi  /=  jvi) goto 20020                                      !Break
        if (b4vk  /=  a41k(jvi)) goto 20020                               !Break
        if ((bvb .and. .not. c1b(jvi)) .or.                                   (.not. bvb .and. c1b(jvi))) goto 20020                        !Break
        if (bvs  /=  f1s(jvi)) goto 20020                                 !Break
        goto 41013                                                     
20020 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, jvi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, jvi, f1s(jvi),                               a41k(jvi), c1b(jvi)                      
41013 continue                                                        
      end do
           if (ivcomp - 0) 0021, 10020, 0021                            
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0021      continue                                                     
! *****                   WRITE RECORDS NOT IN SEQUENCE OF RECORD NUMBER 
41014 do ivi = 1, 10                                            
        jvi = k1i (ivi)                                                 
        avs = g1s (jvi)                                                 
        a4vk = b41k (jvi)                                               
        avb = d1b (jvi)                                                 
        write(iuvi, rec= jvi) avb, a4vk, jvi, avs                       
         end do
! T003*  TEST 3                READ RECORDS IN SEQUENCE OF RECORD NUMBER 
           ivtnum = 3                                                   
           ivcomp = 0                                                   
        do ivi = 1, 10                                            
        read(iuvi, rec = ivi) bvb, b4vk, jvi, bvs                       
        if (jvi  /=  ivi) goto 20030                                      !Break
        if (b4vk  /=  b41k(ivi)) goto 20030                               !Break
        if ((bvb .and. .not. d1b(ivi)) .or.                                   (.not. bvb .and. d1b(ivi))) goto 20030                        !Break
        if (bvs  /=  g1s(jvi)) goto 20030                                 !Break
        goto 41016                                                     
20030 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, ivi                              
           write (nuvi, 70020) jvi, bvs, b4vk, bvb, ivi, g1s(ivi),                               b41k(ivi), d1b(ivi)                      
41016 continue                                                        
      end do
           if (ivcomp -0) 0031, 10030, 0031                             
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0031      continue                                                     
! T004*  TEST 4               READ RECORDS IN A DIFFERENT ORDER SEQUENCE 
           ivtnum = 4                                                   
           ivcomp = 0                                                   
        do ivi = 1, 10                                            
        jvi = m1i(ivi)                                                  
        read(iuvi, rec = jvi) bvb, b4vk, kvi, bvs                       
        if (kvi  /=  jvi) goto 20040                                      !Break
        if (b4vk  /=  b41k(jvi)) goto 20040                               !Break
        if ((bvb .and. .not. d1b(jvi)) .or.                                   (.not. bvb .and. d1b(jvi))) goto 20040                        !Break
        if (bvs  /=  g1s(jvi)) goto 20040                                 !Break
           goto 41018                                                  
20040 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, jvi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, jvi, g1s(jvi),                               b41k(jvi), d1b(jvi)                      
41018 continue                                                        
      end do
           if (ivcomp - 0) 0041, 10040, 0041                            
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0041      continue                                                     
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
! *****    END OF TEST SEGMENT 410                                       
      stop                                                              
      end program fm407
        subroutine sn408(lw1i,kw1i,mw1i,fw1s,gw1s,cw1b,dw1b,a4w1k,b4w1k)
      integer :: ivi
        integer, dimension(1:10) :: lt1i
        integer, dimension(1:10) :: lw1i
        integer, dimension(1:10) :: kt1i
        integer, dimension(1:10) :: kw1i
        integer, dimension(1:10) :: mt1i
        integer, dimension(1:10) :: mw1i
! *****                                                                  
! *****  SUBROUTINE USED WITH SEGMENT FM408        TO SUPPLY VALUES      
! *****  TO ARRAYS THRU THE DUMMY ARGUMENT LIST                          
! *****                                                                  
        real, dimension(1:10) :: ft1s
        real, dimension(1:10) :: fw1s
        real, dimension(1:10) :: gt1s
        real, dimension(1:10) :: gw1s
        logical, dimension(1:10) :: ct1b
        logical, dimension(1:10) :: cw1b
        logical, dimension(1:10) :: dt1b
        logical, dimension(1:10) :: dw1b
        character(len=4), dimension(1:10) :: a4t1k
        character(len=4), dimension(1:10) :: a4w1k
        character(len=4), dimension(1:10) :: b4t1k
        character(len=4), dimension(1:10) :: b4w1k
! *****                                                                  
        data lt1i / 2,4,1,3,10,8,9,6,7,5 / 
        data kt1i / 9,10,1,3,2,5,8,4,7,6 / 
        data mt1i / 10,1,3,4,7,6,8,5,2,9 / 
        data ft1s / 1.0,2.0,3.0,4.0,5.0,6.5,7.1,8.2,9.9,10.0 / 
        data gt1s / 2.34,2.3,1.9,2.3,9.9,1.1,8.8,7.6,2.3,10.1 / 
        data a4t1k / 'AAAA','BBBB','CCCC','DDDD','EDFG','JLKD','CDFE','LKJH','JHGF','LLLL' / 
        data b4t1k / 'HDFK','LKJH','ASDF','LKJH','XMNC','ALXM','IEOW','IERU','DJNC','DJAL' / 
        data ct1b / .true.,.false.,.true.,.true.,.true.,.false.,.false.,.true.,.true.,.false. / 
        data dt1b / .false.,.false.,.false.,.true.,.false.,.false.,.true.,.true.,.false.,.true. / 
! *****                                                                  
        do ivi = 1, 10                                               
        lw1i(ivi) = lt1i(ivi)                                           
        kw1i(ivi) = kt1i(ivi)                                           
        mw1i(ivi) = mt1i(ivi)                                           
        fw1s(ivi) = ft1s(ivi)                                           
        gw1s(ivi) = gt1s(ivi)                                           
        cw1b(ivi) = ct1b(ivi)                                           
        dw1b(ivi) = dt1b(ivi)                                           
        a4w1k(ivi) = a4t1k(ivi)                                         
        b4w1k(ivi) = b4t1k(ivi)                                         
         end do
! *****                                                                  
        return                                                          
        end subroutine sn408
