      program fm910
! ***********************************************************************
! *****   FM910                                                          
! *****                       DIRAF2 - (411)                             
! *****   THIS PROGRAM CALLS SUBROUTINE SN911 IN FILE FM911              
! ***********************************************************************
! *****  TESTING OF DIRECT ACCESS FILES                         ANS REF  
! *****          UNFORMATTED WITH BOTH SEQUENTIAL AND DIRECT     12.5    
! *****          ACCESS TO THE SAME FILE                                 
! *****          NAMED FILE AND SCRATCH FILE                             
! *****                                                                  
! *****          USES SUBROUTINE SN911                                   
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
! *****  S P E C I F I C A T I O N S  SEGMENT 910                        
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
      integer :: i11
      integer :: nuvi
      integer :: imvi
      integer :: kmvi
      integer :: mmvi
      integer :: ivi
      real :: avs
      integer :: ivtnum
      integer :: ivcomp
      real :: bvs
      integer :: kvi
      integer :: jvi
        integer, dimension(1:10) :: l1i
        integer, dimension(1:15) :: n1i
        real, dimension(1:10) :: f1s
        real, dimension(1:15) :: h1s
        character(len=4) :: a4vk
        character(len=4) :: b4vk
        character(len=4) :: d4vk
        character(len=4), dimension(1:10) :: a41k
        character(len=4), dimension(1:15) :: c41k
        logical :: avb
        logical :: bvb
        logical, dimension(1:10) :: c1b
        logical, dimension(1:15) :: e1b
        double precision :: avd
        double precision :: bvd
        double precision, dimension(1:10) :: d1d
        double precision, dimension(1:15) :: b1d
        complex :: avc
        complex :: bvc
        complex, dimension(1:10) :: c1c
        complex, dimension(1:15) :: d1c
! *****                                                                  
! ***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   
! X20   REPLACED BY FEXEC X-20  CONTROL CARD.  X-20  IS FOR REPLACING    
        character(len=15) :: cdir
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
! *****    THE FOLLOWING STATEMENT MUST BE CHANGED IF THE                
! *****    UNITS GIVEN ARE NOT CAPABLE OF BEING OPENED AS SPECIFIED.     
! *****                                                                  
!      I10 CONTAINS THE UNIT NUMBER FOR A NAMED DIRECT ACCESS FILE.      
      i10 = 24                                                          
! X100   REPLACED BY FEXEC X-100 CONTROL CARD (DIR. FILE UNIT NUMBER).   
!      SPECIFYING I10 = NN OVERRIDES THE DEFAULT I10 = 24.               
! *****                                                                  
!      I11 CONTAINS THE UNIT NUMBER FOR A SCRATCH DIRECT ACCESS FILE.    
      i11 = 25                                                          
! X110   REPLACED BY FEXEC X-110 CONTROL CARD (DIR. FILE UNIT NUMBER).   
!      SPECIFYING I11 = NN OVERRIDES THE DEFAULT I11 = 25.               
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
! *****                          FILE NUMBER AND NAME ASSIGNMENT         
      nuvi = i02                                                        
      imvi = i10                                                        
      kmvi = i11                                                        
      ivtotl = 6                                                        
      zprog = 'FM910'                                                   
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
! *****    HEADER FOR SEGMENT 910                                        
        write(nuvi,41100)                                               
41100 format(" ",/" DIRAF2 - (411) DIRECT ACCESS UNFORMATTED FILE" //           " WITH OPTION TO OPEN AS A SEQUENTIAL FILE" //                    " ANS REF. - 12.5" )                                    
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
! *****                                  INITIALIZE DATA                 
        call sn911(l1i,n1i,f1s,h1s,c1b,e1b,d1d,b1d,c1c,d1c,a41k,c41k)

        mmvi = 0                                                        
! *****                                                                  
        open(file=cdir, unit=imvi, access='DIRECT',recl=132,                       status='NEW')                                          
! *****                          WRITE DIRECT FILE IN SEQUENTIAL ORDER   
        do ivi = 1,10                                             
        avs = f1s (ivi)                                                 
        a4vk = a41k (ivi)                                               
        avb = c1b (ivi)                                                 
        avd = d1d (ivi)                                                 
        avc = c1c (ivi)                                                 
        write(unit=imvi, rec= ivi) ivi, avs, a4vk, avb, avd, avc        
         end do
! *****                        CHECK TO SEE IF IT CAN BE OPEN SEQUENTIAL 
        inquire(unit=imvi,sequential=d4vk)                              
        close(unit=imvi)                                                
        if(d4vk  ==  'YES ') goto 41103                                 
        write(nuvi,41102)                                               
41102 format(" ",48x,"TESTS 2 THRU 6 ARE EXPECTED TO " /                       " ",48x,"EXECUTE                        " /                       " ",48x,"TEST 1 IS OPTIONAL AND IS NOT  " /                       " ",48x,"EXECUTED IF DIRECT ACCESS      " /                       " ",48x,"FILE CANNOT BE REOPENED AS     " /                       " ",48x,"A SEQUENTIAL FILE              " )              
        goto 41119                                                      
! T001*  TEST 1                          READ IT SEQUENTIALY             
41103 ivtnum = 1                                                   
           ivcomp = 0                                                   
        open(file=cdir, unit=imvi, access='SEQUENTIAL', status='OLD',         form='UNFORMATTED')                                         
        rewind(unit=imvi)                                               
        do ivi = 1, 10                                            
        read(unit=imvi) kvi, bvs, b4vk, bvb, bvd, bvc                   
        if (ivi  /=  kvi) goto 20010                                      !Break
        if (bvs  <  f1s(ivi) .or. bvs  >  f1s(ivi)) goto 20010          !Break
        if (b4vk  /=  a41k(ivi)) goto 20010                               !Break
        if ((bvb .and. .not. c1b(ivi)) .or.                                   (.not. bvb .and. c1b(ivi))) goto 20010                        !Break
        if (bvd  <  d1d(ivi) .or. bvd  >  d1d(ivi)) goto 20010          !Break
        if ((real(bvc)  <  real(c1c(ivi))) .or. (real(bvc)  >             real(c1c(ivi))) .or. (aimag(bvc)  <  aimag(c1c(ivi)))            .or. (aimag(bvc)  >  aimag(c1c(ivi)))) goto 20010              !Break
           goto 41104                                                  
20010 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, ivi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, bvd, bvc, ivi,                               f1s(ivi), a41k(ivi), c1b(ivi), d1d(ivi),                          c1c(ivi)                                 
70010 format (" ",2x,i3,4x," FAIL ON REC " ,i2)                    
70020 format (" ",16x,"COMPUTED: " ,i2,1x,f5.2,1x,a4,1x,l1,1x,                                       d10.3,1x,"(",f6.3,", ",f6.3,")"/             " ",16x,"CORRECT:  " ,i2,1x,f5.2,1x,a4,1x,l1,1x,                                       d10.3,1x,"(",f6.3,", ",f6.3,")")
41104 continue                                                        
      end do
           if (ivcomp - 0) 0011, 10010, 0011                            
10010 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0011      continue                                                     
! *****                                                                  
41118 close(unit=imvi)                                                
! T002*  TEST 2                             REOPEN AS DIRECT FILE,       
! *****                                  AND READ IN SEQUENTIAL ORDER    
41119 ivtnum = 2                                                   
           ivcomp = 0                                                   
! *****                                                                  
        open(file=cdir, unit=imvi, access='DIRECT', status='OLD',              recl=132)                                                  
        do ivi = 1, 10                                            
        read(unit=imvi, rec = ivi) kvi, bvs, b4vk, bvb, bvd, bvc        
        if (ivi  /=  kvi) goto 20020                                      !Break
        if (bvs  <  f1s(ivi) .or. bvs  >  f1s(ivi)) goto 20020          !Break
        if (b4vk  /=  a41k(ivi)) goto 20020                               !Break
        if ((bvb .and. .not. c1b(ivi)) .or.                                   (.not. bvb .and. c1b(ivi))) goto 20020                        !Break
        if (bvd  <  d1d(ivi) .or. bvd  >  d1d(ivi)) goto 20020          !Break
        if ((real(bvc)  <  real(c1c(ivi))) .or. (real(bvc)  >             real(c1c(ivi))) .or. (aimag(bvc)  <  aimag(c1c(ivi)))            .or. (aimag(bvc)  >  aimag(c1c(ivi)))) goto 20020              !Break
           goto 41120                                                  
20020 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, ivi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, bvd, bvc, ivi,                               f1s(ivi), a41k(ivi), c1b(ivi), d1d(ivi),                          c1c(ivi)                                 
41120 continue                                                        
      end do
           if (ivcomp - 0) 0021, 10020, 0021                            
10020 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0021      continue                                                     
! *****                                                                  
41121 close(unit=imvi)                                                
! T003*  TEST 3                                  READ IT AS DIRECT       
! *****                                      FILE IN NONSEQUENTIAL ORDER 
           ivtnum = 3                                                   
           ivcomp = 0                                                   
! *****                                                                  
        open(file=cdir, unit=imvi, access='DIRECT', status='OLD',              recl=132)                                                  
        do ivi = 1, 10                                            
        jvi = l1i(ivi)                                                  
        read(unit=imvi, rec = jvi) kvi, bvs, b4vk, bvb, bvd, bvc        
        if (kvi  /=  jvi) goto 20030                                      !Break
        if (bvs  <  f1s(jvi) .or. bvs  >  f1s(jvi)) goto 20030          !Break
        if (b4vk  /=  a41k(jvi)) goto 20030                               !Break
        if ((bvb .and. .not. c1b(jvi)) .or.                                   (.not. bvb .and. c1b(jvi))) goto 20030                        !Break
        if (bvd  <  d1d(jvi) .or. bvd  >  d1d(jvi)) goto 20030          !Break
        if ((real(bvc)  <  real(c1c(jvi))) .or. (real(bvc)  >             real(c1c(jvi))) .or. (aimag(bvc)  <  aimag(c1c(jvi)))            .or. (aimag(bvc)  >  aimag(c1c(jvi)))) goto 20030              !Break
           goto 41122                                                  
20030 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, jvi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, bvd, bvc, jvi,                               f1s(jvi), a41k(jvi), c1b(jvi), d1d(jvi),                          c1c(jvi)                                 
41122 continue                                                        
      end do
           if (ivcomp - 0) 0031, 10030, 0031                            
10030 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0031      continue                                                     
! *****                                                                  
41123 open(unit=kmvi, access='DIRECT', recl=80, status='SCRATCH')     
! *****                                                                  
! T004*  TEST 4                  CHECK RECL AND NEXTREC ON SCRATCH FILE  
           ivtnum = 4                                                   
        inquire(unit=kmvi,recl=ivi,nextrec=kvi)                         
        if (ivi  /=  80) goto 20040                                     
        if (kvi  /=  1) goto 20040                                      
10040 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (nuvi, 70030) ivtnum                                   
           write (nuvi, 70040) ivi, kvi                                 
70030 format (" ",2x,i3,4x," FAIL ON RECL AND/OR NEXTREC" )        
70040 format (" ",16x,"COMPUTED:  RECL=" ,i4,", NEXTREC=" ,i4/                  " ",16x,"CORRECT:   RECL=  80, NEXTREC=   1" )       
 0041      continue                                                     
! *****                                                                  
! *****                                 WRITE DIRECT ACCESS              
! *****                          SCRATCH FILE IN NONSEQUENTIAL ORDER     
        do ivi = 1,15                                             
        jvi = n1i (ivi)                                                 
        avs = h1s (jvi)                                                 
        a4vk = c41k (jvi)                                               
        avb = e1b (jvi)                                                 
        avc = d1c(jvi)                                                  
        avd = b1d(jvi)                                                  
        write(unit=kmvi, rec= jvi) avb, avc, a4vk, jvi, avd, avs        
         end do
! T005*  TEST 5                  CHECK DIRECT ACCESS SCRATCH FILE        
! *****                        BY READING IT IN NONSEQUENTIAL ORDER      
           ivtnum = 5                                                   
           ivcomp = 0                                                   
        mmvi = -1                                                       
        do ivi = 15,1,-1                                          
        jvi = n1i (ivi)                                                 
        read(unit=kmvi, rec = jvi) bvb, bvc, b4vk, kvi, bvd, bvs        
        if (kvi  /=  jvi) goto 20050                                      !Break
        if (bvs  <  h1s(jvi) .or. bvs  >  h1s(jvi)) goto 20050          !Break
        if (b4vk  /=  c41k(jvi)) goto 20050                               !Break
        if ((bvb .and. .not. e1b(jvi)) .or.                                   (.not. bvb .and. e1b(jvi))) goto 20050                        !Break
        if (bvd  <  b1d(jvi) .or. bvd  >  b1d(jvi)) goto 20050          !Break
        if ((real(bvc)  <  real(d1c(jvi))) .or. (real(bvc)  >             real(d1c(jvi))) .or. (aimag(bvc)  <  aimag(d1c(jvi)))            .or. (aimag(bvc)  >  aimag(d1c(jvi)))) goto 20050              !Break
           goto 41127                                                  
20050 ivcomp = ivcomp + 1                                          
           if (ivcomp  <=  1) ivfail = ivfail + 1                       
           write (nuvi, 70010) ivtnum, jvi                              
           write (nuvi, 70020) kvi, bvs, b4vk, bvb, bvd, bvc, jvi,                               h1s(jvi), c41k(jvi), e1b(jvi), b1d(jvi),                          d1c(jvi)                                 
41127 continue                                                        
      end do
           if (ivcomp - 0) 0051, 10050, 0051                            
10050 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
 0051      continue                                                     
! *****                                                                  
! T006*  TEST 6                     CHECK RECL AND NEXTREC AFTER READING 
           ivtnum = 6                                                   
        inquire(unit=kmvi,recl=ivi,nextrec=kvi)                         
        if (ivi  /=  80) goto 20060                                     
        if (kvi  /=  6) goto 20060                                      
10060 ivpass = ivpass + 1                                          
           write (nuvi, 80002) ivtnum                                   
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (nuvi, 70050) ivtnum                                   
           write (nuvi, 70060) ivi, kvi                                 
70050 format (" ",2x,i3,4x," FAIL ON RECL AND/OR NEXTREC" )        
70060 format (" ",16x,"COMPUTED:  RECL=" ,i4,", NEXTREC=" ,i4/                  " ",16x,"CORRECT:   RECL=  80, NEXTREC=   6" )       
 0061      continue                                                     
! *****                                                                  
           close (unit=imvi,status='DELETE')                            
! *****                                                                  
! ****                                                                   
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
! *****    END OF TEST SEGMENT 910                                       
      stop                                                              
      end program fm910
        subroutine sn911(lw1i,nw1i,fw1s,hw1s,cw1b,ew1b,dw1d,bw1d,cw1c,dw1c,a4w1k,c4w1k)
      integer :: ivi
        integer, dimension(1:10) :: lw1i
        integer, dimension(1:10) :: lt1i
        integer, dimension(1:15) :: nt1i
        integer, dimension(1:15) :: nw1i
! *****                                                                  
! *****  SUBROUTINE USED WITH SEGMENT DIRAF2 (411) TO SUPPLY VALUES      
! *****  TO ARRAYS THRU THE DUMMY ARGUMENT LIST                          
! *****                                                                  
        real, dimension(1:10) :: ft1s
        real, dimension(1:10) :: fw1s
        real, dimension(1:15) :: ht1s
        real, dimension(1:15) :: hw1s
        logical, dimension(1:10) :: ct1b
        logical, dimension(1:10) :: cw1b
        logical, dimension(1:15) :: et1b
        logical, dimension(1:15) :: ew1b
        double precision, dimension(1:10) :: dt1d
        double precision, dimension(1:10) :: dw1d
        double precision, dimension(1:15) :: bt1d
        double precision, dimension(1:15) :: bw1d
        complex, dimension(1:10) :: cw1c
        complex, dimension(1:10) :: ct1c
        complex, dimension(1:15) :: dw1c
        complex, dimension(1:15) :: dt1c
        character(len=4), dimension(1:10) :: a4t1k
        character(len=4), dimension(1:10) :: a4w1k
        character(len=4), dimension(1:15) :: c4t1k
        character(len=4), dimension(1:15) :: c4w1k
! *****                                                                  
        data lt1i / 2,3,1,3,10,8,9,6,7,5 / 
        data nt1i / 5,7,3,9,4,11,8,13,14,12,6,10,2,15,1 / 
        data ft1s / 1.0,2.0,3.0,4.0,5.0,6.5,7.1,8.2,9.9,10.0 / 
        data ht1s / 2.34,2.3,1.9,2.3,9.9,1.1,8.8,7.6,2.3,10.1,3.4,5.60,34.9,3.48,23.8 / 
        data a4t1k / 'AAAA','BBBB','CCCC','DDDD','EDFG','JLKD','CDFE','LKJH','JHGF','LLLL' / 
        data c4t1k / 'HDFK','LKJH','ASDF','LKJH','XMNC','ALXM','IEOW','IERU','DJNC','DJAL','KDFJ','ABCD','ASDF','GHJK','QWER' / 
        data ct1b / .true.,.false.,.true.,.true.,.true.,.false.,.false.,.true.,.true.,.false. / 
        data et1b / .false.,.false.,.false.,.true.,.false.,.false.,.true.,.true.,.false.,.true.,.true.,.true.,.false.,.true.,.false. / 
        data dt1d / 1.23d1,2.34d1,3.45d3,4.56d4,5.602d0,34.35d1,2.34d1,398.0d0,3.49d-1,0.99d1 / 
        data bt1d / 3.45d1,34.5d0,34.5d4,2.93d3,0.09d-2,3.4d-1,34.0d1,85.0d1,3.968d0,3.48d1,39.3d4,0.09d3,389.098d1,483.98d0,3456.0d-4 / 
        data ct1c / (1.2,3.4),(9.8,34.5),(3.4,34.9),(9.0,34.9),(2.3,3.9),(3.98,8.9),(3.112,3.4),(8.0,1.2),(2.56,2.1),(3.4,4.5) / 
        data dt1c / (2.3,3.9),(3.98,8.9),(3.112,3.4),(8.0,1.2),(2.56,2.1),(3.4,4.5),(3.4,34.9),(9.0,34.9),(1.2,3.4),(9.8,34.5),(3.4,34.9),(9.0,34.9),(3.112,3.4),(8.0,1.2),(3.112,3.4) / 
        do ivi = 1, 10                                               
! *****                                                                  
        lw1i(ivi) = lt1i(ivi)                                           
        fw1s(ivi) = ft1s(ivi)                                           
        cw1b(ivi) = ct1b(ivi)                                           
        dw1d(ivi) = dt1d(ivi)                                           
        cw1c(ivi) = ct1c(ivi)                                           
        a4w1k(ivi) = a4t1k(ivi)                                         
         end do
! *****                                                                  
        do ivi = 1, 15                                                
        nw1i(ivi) = nt1i(ivi)                                           
        hw1s(ivi) = ht1s(ivi)                                           
        ew1b(ivi) = et1b(ivi)                                           
        bw1d(ivi) = bt1d(ivi)                                           
        dw1c(ivi) = dt1c(ivi)                                           
        c4w1k(ivi) = c4t1k(ivi)                                         
         end do
! *****                                                                  
        return                                                          
        end subroutine sn911
