      program fm912
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM912                                                          
! *****                       DIRAF3 - (412)                             
! *****   THIS PROGRAM CALLS SUBROUTINE SN913 IN FILE FM913              
! ***********************************************************************
! *****  TESTING OF DIRECT ACCESS FILES                         ANS REF  
! *****          FORMATTED, WITH BOTH SEQUENTIAL AND DIRECT       12.5   
! *****          ACCESS TO THE SAME FILE                                 
! *****                                                                  
! *****          USES SUBROUTINE SN913    FAQ                            
! *****                                                                  
! *****  S P E C I F I C A T I O N S  SEGMENT 412                        
! ***********************************************************************
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: i13
      integer :: nuvi
      integer :: kuvi
      real :: cvs
      integer :: ivtnum
      integer :: ivi
      integer :: kvi
      real :: go
      real :: to
      real :: avs
      real :: bvs
      integer :: iswt
      real :: dvs
      real :: gvs
      integer :: lvi
      real :: evs
      real :: fvs
      integer :: nvi
      integer :: jvi
        real, dimension(1:10) :: f1s
        real, dimension(1:10) :: g1s
        character(len=20) :: a20vk
        character(len=20) :: b20vk
        character(len=20) :: c20vk
        character(len=20), dimension(1:10) :: a201k
        character(len=20), dimension(1:10) :: b201k
        character(len=47) :: a47vk
        character(len=47) :: b47vk
        character(len=47) :: c47vk
        character(len=51) :: a51vk
        character(len=12) :: a12vk
        character(len=120) :: a120vk
        character(len=120) :: b120vk
        character(len=1) :: a1vk
        character(len=4) :: a4vk
        character(len=31) :: remk
        character(len=31) :: remk1
        character(len=31) :: remk2
        character(len=31) :: remk3
        character(len=31) :: remk4
        character(len=31) :: remk5
        character(len=31) :: remk45
        logical :: avb
        logical :: bvb
        logical :: cvb
        logical, dimension(1:10) :: c1b
        logical, dimension(1:10) :: d1b
        double precision :: avd
        double precision :: bvd
        double precision :: cvd
        double precision :: dvd
        double precision, dimension(1:10) :: d1d
        double precision, dimension(1:15) :: b1d
! *****                                                                  
! ***** BELOW CHARACTER STATEMENT ESTABLISHES THE FILE NAME VARIABLES.   
! X20   REPLACED BY FEXEC X-20  CONTROL CARD.  X-20  IS FOR REPLACING    
        character(len=15) :: cdir
!       THE CHARACTER STATEMENT FOR FILE NAMES ASSOCIATED WITH X-130     
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
!      I13 CONTAINS THE UNIT NUMBER FOR A NAMED DIRECT ACCESS FILE.      
      i13 = 24                                                          
! X130   REPLACED BY FEXEC X-130 CONTROL CARD (DIR. FILE UNIT NUMBER).   
!      SPECIFYING I13 = NN OVERRIDES THE DEFAULT I13 = 24.               
!                                                                        
! *****  THE FOLLOWING STATEMENT MUST BE CHANGED IF THE NAME             
! *****  GIVEN IS NOT A VALID FILE SPECIFIER FOR A DIRECT,               
! *****  FORMATTED FILE.                                                 
! *****                                                                  
!      CDIR CONTAINS THE FILE NAME FOR UNIT I13.                         
      cdir = '        DIRFILE'                                          
!                                                                        
! X201   REPLACED BY FEXEC X-201 CONTROL CARD.  CX201 IS FOR SYSTEMS     
!      REQUIRING A DIFFERENT FILE SPECIFIER FOR FILES ASSOCIATED WITH    
!      X-130 THAN THE DEFAULT CDIR = '        DIRFILE'.                  
!                                                                        
! *****                          FILE NUMBER AND NAME ASSIGNMENT         
        nuvi = i02                                                      
        kuvi = i13                                                      
        ivtotl = 26                                                     
        zprog = 'FM912'                                                 
! *****                                                                  
! *****  FILE NUMBER AND NAME ASSIGNMENT                                 
! *****                                                                  
        remk1='RECORD 1 - ERR PATH TAKEN'                               
        remk2='RECORD 2 - ERR PATH TAKEN'                               
        remk3='RECORD 3 - ERR PATH TAKEN'                               
        remk4='RECORD 4 - ERR PATH TAKEN'                               
        remk5='RECORD 5 - ERR PATH TAKEN'                               
        remk45='RECORD 4 + 5 - ERR PATH TAKEN'                          
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
        write(nuvi,41200)                                               
41200 format( " ",/"  DIRAF3 - (412) DIRECT ACCESS FORMATTED FILE" /            "  WITH OPTION TO OPEN AS A SEQUENTIAL FILE" /                    "  ANS REF. - 12.5" )                                   
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
! *****  PLUS OR MINUS VALUES                                            
! *****                                                                  
        cvs = 0.0001                                                    
        cvd = 0.0001d0                                                  
! *****                                                                  
! *****  INITIALIZE DATA ARRAYS                                          
! *****                                                                  
        call sn913(f1s,g1s,c1b,d1b,d1d,b1d,a201k,b201k)

! *****                                                                  
! *****  OPEN DIRECT ACCESS FILE - STATUS=NEW                            
! *****                                                                  
        open(file=cdir, unit=kuvi, access='DIRECT',recl=120,                        form='FORMATTED',status='NEW')                        
        ivtnum=1                                                        
! *****                                                                  
! T001*  TEST 1 - CHECKS RECL AND NEXTREC                                
! *****           FOR JUST OPENED DIRECT ACCESS FILE                     
! *****                                                                  
        inquire(unit=kuvi, recl=ivi, nextrec=kvi)                       
        if (ivi  /=  120) goto 33020                                   
        if (kvi  /=  1) goto 33020                                     
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33030                                                     
33020 remk='ERROR IN INQUIRE'                                         
        write(nuvi,55010)ivtnum,remk                                    
55010 format(" ","TEST ",i3,1x," FAIL",34x,a31)                       
        ivfail=ivfail+1                                                 
        write(nuvi,55020)ivi,kvi                                        
55020 format(" ",/,11x,"COMPUTED:  RECL=" ,i6,5x,"NEXTREC=",i6)       
        write(nuvi,55030)                                               
55030 format(" ",10x,"CORRECT:   RECL=   120" ,5x,"NEXTREC=     1" /) 
! *****                                                                  
! T002*  TEST 2 - WRITES RECORD 1                                        
! *****                                                                  
33030 ivtnum=2                                                        
        ivi = 1                                                         
        avs = f1s (ivi)                                                 
        bvs = f1s(ivi + 1)                                              
        a20vk = a201k (ivi)                                             
        avb = c1b (ivi)                                                 
        avd = d1d (ivi)                                                 
        write(unit=kuvi,rec=1,fmt=41204,err=33040) ivi, avs, bvs, avd,                                               avb, a20vk           
41204 format(i5, f10.5, e14.6, d14.8, l10, a20, 35x, ' LAST RECORD')  
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33050                                                     
33040 write(nuvi,55010)ivtnum,remk1                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T003*  TEST 3 - WRITES RECORD 2                                        
! *****                                                                  
33050 ivtnum=3                                                        
        ivi = ivi + 1                                                   
        avs = f1s (ivi)                                                 
        bvs = f1s(ivi + 1)                                              
        a20vk = a201k (ivi)                                             
        avb = c1b (ivi)                                                 
        avd = d1d (ivi)                                                 
        write(unit=kuvi,rec=2,fmt=41205,err=33060) bvs, avd, ivi, avs,                                               avb, a20vk           
41205 format(e12.6, d15.7, i4, f11.5, l2, a25, 30x, ' LASTS RECORD')  
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33070                                                     
33060 write(nuvi,55010)ivtnum,remk2                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T004*  TEST 4 - WRITES RECORD 3                                        
! *****                                                                  
33070 ivtnum=4                                                        
        ivi = ivi + 1                                                   
        avs = f1s (ivi)                                                 
        bvs = f1s(ivi + 1)                                              
        a20vk = a201k (ivi)                                             
        avb = c1b (ivi)                                                 
        avd = d1d (ivi)                                                 
        write(unit=kuvi,rec=3,fmt=41206,err=33080) ivi, bvs, avs, avd,                                               avb, a20vk           
41206 format(i5, f10.5, e14.6, d14.8, l10, a20, 30x, 'THE LAST REC')  
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33090                                                     
33080 write(nuvi,55010)ivtnum,remk3                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T005*  TEST 5 - WRITES RECORDS 4 AND 5 WITH ONE WRITE                  
! *****                                                                  
33090 ivtnum=5                                                        
        ivi = ivi + 1                                                   
        avs = f1s (ivi)                                                 
        bvs = f1s(ivi + 1)                                              
        a20vk = a201k (ivi)                                             
        avb = c1b (ivi)                                                 
        avd = d1d (ivi)                                                 
        write(unit=kuvi,rec=4,fmt=41207,err=33100) ivi, avs, avd, avb,                       a20vk, bvs, bvs, avd, avb, ivi, avs, a20vk   
41207 format(i5, f10.5, d14.8, l10, a20, e14.6, 35x, 'NEXT TO LAST',/          e12.6, d15.7, l2, i4, f11.5, a25, 30x, 'THE END')        
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33290                                                     
33100 write(nuvi,55010)ivtnum,remk45                                  
        ivfail=ivfail+1                                                 
! *****                                                                  
! T006*  TEST 6 - CHECK RECL AND NEXTREC ON OPENED FILE                  
! *****                                                                  
33290 ivtnum=6                                                        
        inquire(unit=kuvi, recl=ivi, nextrec=kvi)                       
        if (ivi  /=  120)goto 33300                                    
        if(kvi  /=  6)goto 33300                                       
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33110                                                     
33300 remk='ERROR IN INQUIRE'                                         
        write(nuvi,55010)ivtnum,remk                                    
        ivfail=ivfail+1                                                 
        write(nuvi,55020)ivi,kvi                                        
        write(nuvi,55040)                                               
55040 format(" ",10x,"CORRECT:   RECL=   120" ,5x,"NEXTREC=     6" /) 
! *****                                                                  
! T007*  TEST 7 - READS RECORD 1                                         
! *****                                                                  
33110 ivtnum=7                                                        
        ivi = 1                                                         
        read(unit=kuvi,rec=ivi,fmt=41210,err=33120) kvi, avs, bvs, avd,                                               avb, a20vk, a47vk   
41210 format(i5, f10.5, e14.6, d14.8, l10, a20, a47)                  
        iswt=1                                                          
        goto 33220                                                     
33120 write(nuvi,55010)ivtnum,remk1                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T008*  TEST 8 - READS RECORD 2                                         
! *****                                                                  
33130 ivtnum=8                                                        
        ivi = 2                                                         
        read(unit=kuvi,rec=ivi,fmt=41238,err=33140) bvs, avd, kvi, avs,                                               avb, a20vk, a51vk   
41238 format(e12.6, d15.7, i4, f11.5, l2, a25, a51)                   
        iswt=2                                                          
        goto 33230                                                     
33140 write(nuvi,55010)ivtnum,remk2                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T009*  TEST 9 - READS RECORD 3                                         
! *****                                                                  
33150 ivtnum=9                                                        
        ivi = 3                                                         
        read(unit=kuvi,rec=ivi,fmt=41210,err=33160) lvi, dvs, gvs, bvd,                                               bvb, b20vk, b47vk   
        iswt=3                                                          
        goto 33240                                                     
33160 write(nuvi,55010)ivtnum,remk3                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T010*  TEST 10 - READS RECORD 4                                        
! *****                                                                  
33170 ivtnum=10                                                       
        ivi = 4                                                         
        read(unit=kuvi,rec=ivi,fmt=41241,err=33180) nvi, evs, dvd, cvb,                                               c20vk, fvs, c47vk   
41241 format(i5, f10.5, d14.8, l10, a20, e14.6, a47)                  
        iswt=4                                                          
        goto 33250                                                     
33180 write(nuvi,55010)ivtnum,remk4                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T011*  TEST 11 - READS RECORD 5                                        
! *****                                                                  
33190 ivtnum=11                                                       
        ivi = 5                                                         
        jvi = 4                                                         
        read(unit=kuvi,rec=ivi,fmt=41218,err=33200) bvs, avd, avb, kvi,                                               avs, a20vk, a51vk   
41218 format(e12.6, d15.7, l2, i4, f11.5, a25, a51)                   
        iswt=5                                                          
        goto 33260                                                     
33200 write(nuvi,55010)ivtnum,remk5                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T012*  TEST 12 - OVERWRITES RECORD 3                                   
! *****                                                                  
33210 ivtnum=12                                                       
        ivi = 3                                                         
        avs = g1s (ivi)                                                 
        bvs = g1s(ivi + 1)                                              
        a20vk = b201k (ivi)                                             
        avb = d1b (ivi)                                                 
        avd = b1d (ivi)                                                 
        write(unit=kuvi,rec=3,fmt=41251,err=33310) ivi, avs, bvs, avd,                                               a20vk, avb           
41251 format(i5, f11.5, e13.6, d14.8, a20, l10, 35x, 'NEW  RECORD ')  
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33320                                                     
33310 write(nuvi,55010)ivtnum,remk3                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T013*  TEST 13 - OVERWRITES RECORD 5                                   
! *****                                                                  
33320 ivtnum=13                                                       
        ivi = 5                                                         
        avs = g1s (ivi)                                                 
        bvs = g1s(ivi - 1)                                              
        a20vk = b201k (ivi)                                             
        avb = d1b (ivi)                                                 
        avd = b1d (ivi)                                                 
        write(unit=kuvi,rec=5,fmt=41252,err=33330) avs, ivi, a20vk, avd,                                             bvs, avb             
41252 format(f10.5, i5, a20, d14.8, e14.6, l10, 35x, 'STOP  RECORD')  
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33340                                                     
33330 write(nuvi,55010)ivtnum,remk5                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! *****  CLOSE AND REOPEN DIRECT ACCESS FILE                             
! *****                                                                  
33340 close(unit=kuvi)                                                
        open(file=cdir, unit=kuvi, access='DIRECT',status='OLD',               form='FORMATTED',recl=120)                                 
! *****                                                                  
! *****                                                                  
! T014*  TEST 14 - READS RECORD 4                                        
        ivtnum=14                                                       
        ivi = 4                                                         
        read(unit=kuvi,rec=ivi,fmt=41241,err=33350) nvi, evs, dvd, cvb,                                               c20vk, fvs, c47vk   
        iswt=6                                                          
        goto 33250                                                     
33350 write(nuvi,55010)ivtnum,remk4                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T015*  TEST 15 - READS THE CHANGED RECORD 5                            
! *****                                                                  
33360 ivtnum=15                                                       
        ivi = 5                                                         
        read(unit=kuvi,rec=ivi,fmt=41254,err=33370) avs, kvi, a20vk,                                                  avd, bvs, avb, a47vk
41254 format(f10.5, i5, a20, d14.8, e14.6, l10, a47)                  
        iswt=7                                                          
        if (kvi  /=  ivi) goto 41221                                    
        if (avs  <  g1s(ivi)-cvs .or. avs  >  g1s(ivi)+cvs) goto 41223
        if (bvs < g1s(ivi-1)-cvs .or. bvs > g1s(ivi-1)+cvs) goto 41225
        if (a20vk  /=  b201k(ivi)) goto 41229                           
        if ((avb .and. .not. d1b(ivi)) .or.                                   (.not. avb .and. d1b(ivi))) goto 41233                      
        if (avd  <  b1d(ivi)-cvd .or. avd  >  b1d(ivi)+cvd) goto 41227
        if (a47vk  /=                                                     '                                   STOP  RECORD') goto 41231   
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33380                                                     
33370 write(nuvi,55010)ivtnum,remk5                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T016*  TEST 16 - READS RECORD 2                                        
! *****                                                                  
33380 ivtnum=16                                                       
        ivi = 2                                                         
        read(unit=kuvi,rec=ivi,fmt=41238,err=33390) bvs, avd, kvi, avs,                                               avb, a20vk, a51vk   
        iswt=8                                                          
        goto 33230                                                     
33390 write(nuvi,55010)ivtnum,remk2                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T017*  TEST 17 - READS THE CHANGED RECORD 3                            
! *****                                                                  
33400 ivtnum=17                                                       
        ivi = 3                                                         
        read(unit=kuvi,rec=3,fmt=41256,err=33410) kvi, avs, bvs, avd,                                               a20vk, avb, a47vk     
41256 format(i5, f11.5, e13.6, d14.8, a20, l10, a47)                  
        iswt=9                                                          
        if (kvi  /=  ivi) goto 41221                                    
        if (avs  <  g1s(ivi)-cvs .or. avs  >  g1s(ivi)+cvs) goto 41223
        if (bvs < g1s(ivi+1)-cvs .or. bvs > g1s(ivi+1)+cvs) goto 41225
        if (a20vk  /=  b201k(ivi)) goto 41229                           
        if ((avb .and. .not. d1b(ivi)) .or.                                   (.not. avb .and. d1b(ivi))) goto 41233                      
        if (avd  <  b1d(ivi)-cvd .or. avd  >  b1d(ivi)+cvd) goto 41227
        if (a47vk  /=                                                     '                                   NEW  RECORD ') goto 41231   
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33420                                                     
33410 write(nuvi,55010)ivtnum,remk3                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T018*  TEST 18 - READS RECORD 1                                        
! *****                                                                  
33420 ivtnum=18                                                       
        ivi = 1                                                         
        read(unit=kuvi,rec=ivi,fmt=41210,err=33430) kvi, avs, bvs, avd,                                               avb, a20vk, a47vk   
        iswt=10                                                         
        goto 33220                                                     
33430 write(nuvi,55010)ivtnum,remk1                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T019*  TEST 19 - OVERWRITES RECORD 4                                   
! *****                                                                  
33440 ivtnum=19                                                       
41258 ivi = 4                                                         
        kvi = ivi + 1                                                   
        avs = f1s (ivi)                                                 
        bvs = f1s(ivi + 1)                                              
        evs = f1s(ivi) + 2.34                                           
        avd = d1d (ivi)                                                 
        write(unit=kuvi,rec=4,fmt=41259,err=33450) ivi, kvi, avs, bvs,                                               evs, avd             
41259 format(i5, i5.3, f10.5, e14.6, e20.1e4, d14.8)                  
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33460                                                     
33450 write(nuvi,55010)ivtnum,remk4                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T020*  TEST 20 - OVERWRITES RECORDS 1, 2, AND 3                        
! *****                                                                  
33460 ivtnum=20                                                       
        ivi = 1                                                         
        a1vk = 'A'                                                      
        a4vk = a201k (ivi) (1:4)                                        
        avb = c1b (ivi)                                                 
        avd = d1d (ivi)                                                 
        bvd = d1d (ivi) + 3.234d2                                       
        write(unit=kuvi,rec=1,fmt=41260,err=33470) avd, bvd, avb, a1vk,                                              a4vk                 
41260 format(g14.8, g20.2e4, l2, a, a4, 'TSAL DROCER',//,                      "HOLLERITH " , t15, 'ONE', 10x, tl5, 'TWO', tr5,                  'THREE', :, 'LAST')                                      
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33480                                                     
33470 write(nuvi,55010)ivtnum,remk1                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T021*  TEST 21 - OVERWRITES RECORD 5                                   
! *****                                                                  
33480 ivtnum=21                                                       
        ivi = 5                                                         
        bvs = f1s(ivi - 1)                                              
        avd = b1d (4)                                                   
        write(unit=kuvi,rec=5,fmt=41261,err=33490) ivi, bvs, ivi, avd   
41261 format(sp, i5, s, f10.5, ss, i5, 3pe14.6e2)                     
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33500                                                     
33490 write(nuvi,55010)ivtnum,remk5                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! *****  CLOSE AND REOPEN DIRECT ACCESS FILE                             
! *****                                                                  
33500 close(unit=kuvi)                                                
        open(file=cdir, unit=kuvi, access='DIRECT',status='OLD',               form='FORMATTED',recl=120)                                 
! *****                                                                  
! *****                                                                  
! T022*  TEST 22 - READS RECORD 1                                        
        ivtnum=22                                                       
        ivi = 1                                                         
        read(unit=kuvi,rec=ivi,fmt=41262,err=33510) avd, a20vk, avb,                                                  a1vk, a4vk, a12vk   
41262 format(g14.8, a20, l2, a, a4, a12)                              
        iswt=1                                                          
        if (avd  <  d1d(ivi)-cvd .or. avd  >  d1d(ivi)+cvd) goto 41277
        if (a20vk(12:20)  /=  '.34E+0003') goto 41279                   
        if ((a1vk  /=  'A') .or.                                             (a4vk  /=  a201k(ivi)(1:4)) .or.                                  (a12vk  /=  'TSAL DROCER')) goto 41279                       
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33520                                                     
33510 write(nuvi,55010)ivtnum,remk1                                   
        ivfail=ivfail+1                                                 
! *****                                                  RECORD # 4      
! T023*  TEST 23 - READS RECORD 4                                        
! *****                                                                  
33520 ivtnum=23                                                       
        ivi = 4                                                         
        read(unit=kuvi,rec=ivi,fmt=41266,err=33530) kvi, a20vk, avs,                                                  bvs, b20vk, avd     
41266 format(i5, a5, f10.5, e14.6, a20, d14.8)                        
        iswt=2                                                          
        if (a20vk(3:5)  /=  '005') goto 41293                           
        if ((avs  <  f1s(ivi)-cvs .or. avs  >  f1s(ivi)+cvs) .or.          (bvs < f1s(ivi+1)-cvs .or. bvs > f1s(ivi+1)+cvs) .or.           (b20vk(13:20)  /=  '.6E+0001')) goto 41293                   
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33540                                                     
33530 write(nuvi,55010)ivtnum,remk4                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T024*  TEST 24 - READS RECORD 2   TESTS FOR BLANK RECORD               
! *****                                                                  
33540 ivtnum=24                                                       
        b120vk = ' '                                                    
        ivi = 2                                                         
        read(unit=kuvi,rec=ivi,fmt=41269,err=33550) a120vk              
41269 format(a120)                                                    
        iswt=3                                                          
        if (a120vk  /=  b120vk) goto 41281                              
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33560                                                     
33550 write(nuvi,55010)ivtnum,remk2                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T025*  TEST 25 - READS RECORD 5                                        
! *****                                                                  
33560 ivtnum=25                                                       
        ivi = 5                                                         
        read(unit=kuvi,rec=ivi,fmt=41271,err=33570) a20vk(1:5), avs,                                                  b20vk, c20vk        
41271 format(a5, f10.5, bz, a5, bn, a20)                              
        iswt=4                                                          
        if (a20vk(1:5)  /=  '   +5') goto 41283                         
        if (b20vk(1:5)  /=  '    5') goto 41285                         
        if (c20vk(1:14)  /=  '  625.0000E-03') goto 41287               
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33580                                                     
33570 write(nuvi,55010)ivtnum,remk5                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! T026*  TEST 26 - READS RECORD 3                                        
! *****                                                                  
33580 ivtnum=26                                                       
        ivi = 3                                                         
        read(unit=kuvi,rec=ivi,fmt=41275,err=33590) a120vk              
41275 format(a120)                                                    
        iswt=5                                                          
        if (a120vk(1:10)  /=  'HOLLERITH') goto 41289                   
        if (a120vk(11:40)  /=                                              '    ONE     TWO     THREE     ') goto 41291                   
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33600                                                     
33590 write(nuvi,55010)ivtnum,remk3                                   
        ivfail=ivfail+1                                                 
! *****                                                                  
! *****  CLOSE DIRECT ACCESS FILE                                        
! *****                                                                  
33600 close(unit=kuvi,status='DELETE')                                
        goto 33610                                                     
! *****                                                                  
! *****  CHECKING RECORD 1                                               
! *****                                                                  
33220 if (kvi  /=  ivi) goto 41221                                    
        if (avs  <  f1s(ivi)-cvs .or. avs  >  f1s(ivi)+cvs) goto 41223
        if (bvs < f1s(ivi+1)-cvs .or. bvs > f1s(ivi+1)+cvs) goto 41225
        if (a20vk  /=  a201k(ivi)) goto 41229                           
        if (a47vk  /=                                                     '                                    LAST RECORD') goto 41231   
        if ((avb .and. .not. c1b(ivi)) .or.                                   (.not. avb .and. c1b(ivi))) goto 41233                      
        if (avd  <  d1d(ivi)-cvd .or. avd  >  d1d(ivi)+cvd) goto 41227
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        if (iswt  ==  10)goto 33440                                    
        goto 33130                                                     
41221 write(nuvi,41222)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
41223 write(nuvi,41224)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
41225 write(nuvi,41226)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
41227 write(nuvi,41228)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
41229 write(nuvi,41230)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
41231 write(nuvi,41232)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
41233 write(nuvi,41234)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to (33130,33150,33170,33190,33210,33360,33380,33400,                  33420,33440)iswt                                         
! *****                                                                  
! *****                                                                  
! *****  CHECKING RECORD 2                                               
33230 if (kvi  /=  ivi) goto 41221                                    
        if (avs  <  f1s(ivi)-cvs .or. avs  >  f1s(ivi)+cvs) goto 41223
        if (bvs < f1s(ivi+1)-cvs .or. bvs > f1s(ivi+1)+cvs) goto 41225
        if (a20vk  /=  a201k(ivi)) goto 41229                           
        if ((avb .and. .not. c1b(ivi)) .or.                                   (.not. avb .and. c1b(ivi))) goto 41233                      
        if (avd  <  d1d(ivi)-cvd .or. avd  >  d1d(ivi)+cvd) goto 41227
        if (a51vk  /=                                                    '                               LASTS RECORD        ')goto 41231 
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        if (iswt  ==  8)goto 33400                                     
        goto 33150                                                     
! *****                                                                  
! *****  CHECKING RECORD 3                                               
! *****                                                                  
33240 if (lvi  /=  ivi) goto 41221                                    
        if (gvs  <  f1s(ivi)-cvs .or. gvs  >  f1s(ivi)+cvs) goto 41223
        if (dvs < f1s(ivi+1)-cvs .or. dvs > f1s(ivi+1)+cvs) goto 41225
        if (b20vk  /=  a201k(ivi)) goto 41229                           
        if ((bvb .and. .not. c1b(ivi)) .or.                                   (.not. bvb .and. c1b(ivi))) goto 41233                      
        if (bvd  <  d1d(ivi)-cvd .or. bvd  >  d1d(ivi)+cvd) goto 41227
        if (b47vk  /=                                                     '                              THE LAST REC     ') goto 41231   
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33170                                                     
! *****                                                                  
! *****  CHECKING RECORD 4                                               
! *****                                                                  
33250 if (nvi  /=  ivi) goto 41221                                    
        if (evs  <  f1s(ivi)-cvs .or. evs  >  f1s(ivi)+cvs) goto 41223
        if (fvs < f1s(ivi+1)-cvs .or. fvs > f1s(ivi+1)+cvs) goto 41225
        if (c20vk  /=  a201k(ivi)) goto 41229                           
        if ((cvb .and. .not. c1b(ivi)) .or.                                   (.not. cvb .and. c1b(ivi))) goto 41233                      
        if (dvd  <  d1d(ivi)-cvd .or. dvd  >  d1d(ivi)+cvd) goto 41227
        if (c47vk  /=                                                     '                                   NEXT TO LAST') goto 41231   
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        if (iswt  ==  6)goto 33360                                     
        goto 33190                                                     
! *****                                                                  
! *****  CHECKING RECORD 5                                               
! *****                                                                  
33260 if (kvi  /=  jvi) goto 41221                                    
        if (avs  <  f1s(jvi)-cvs .or. avs  >  f1s(jvi)+cvs) goto 41223
        if (bvs < f1s(jvi+1)-cvs .or. bvs > f1s(jvi+1)+cvs) goto 41225
        if (a20vk  /=  a201k(jvi)) goto 41229                           
        if ((avb .and. .not. c1b(jvi)) .or.                                   (.not. avb .and. c1b(jvi))) goto 41233                      
        if (avd  <  d1d(jvi)-cvd .or. avd  >  d1d(jvi)+cvd) goto 41227
        if (a51vk  /=                                                    '                              THE END              ') goto 41231
        write(nuvi,80002)ivtnum                                         
        ivpass=ivpass+1                                                 
        goto 33210                                                     
! *****                                                                  
! *****                                                                  
! *****                                                                  
41277 write(nuvi,41278)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41279 write(nuvi,41280)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41281 write(nuvi,41282)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41283 write(nuvi,41284)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41285 write(nuvi,41286)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41287 write(nuvi,41288)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41289 write(nuvi,41290)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41291 write(nuvi,41292)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
41293 write(nuvi,41294)ivtnum,ivi                                     
        ivfail=ivfail+1                                                 
        go to(33520,33540,33560,33580,33600)iswt                        
! *****                                                                  
! *****                                                                  
! *****                                                                  
41222 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON I FORMAT" )                                       
41224 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON F FORMAT" )                                       
41226 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON E FORMAT" )                                       
41228 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON D FORMAT" )                                       
41230 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON A FORMAT" )                                       
41232 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON X AND ' FORMAT" )                                 
41234 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON L FORMAT" )                                       
41278 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON GW.D FORMAT" )                                    
41280 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON GW.DEN FORMAT" )                                  
41282 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON BLANK RECORD " )                                  
41284 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON SP FORMAT    " )                                  
41286 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON BZ OR SS FORMAT" )                                
41288 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON NP FORMAT    " )                                  
41290 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON H FORMAT     " )                                  
41292 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON TR, TLC, T FORMAT" )                              
41294 format(" ","TEST ",i3,"  FAIL",34x,"RECORD",i2,                          " - ON IN.N FORMAT  " )                                  
! *****                                                                  
! *****                                                                  
! *****  END OF TEST SEGMENT 412                                         
33610 continue                                                        
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
        stop                                                            
        end program fm912
        subroutine sn913(fw1s,gw1s,cw1b,dw1b,dw1d,bw1d,a20w1k,b20w1k)
      integer :: ivi
        real, dimension(1:5) :: ft1s
        real, dimension(1:5) :: fw1s
        real, dimension(1:5) :: gt1s
        real, dimension(1:5) :: gw1s
! *****                                                                  
! *****  SUBROUTINE USED WITH SEGMENT DIRAF3 (412) TO SUPPLY VALUES      
! *****  TO ARRAYS THRU THE DUMMY ARGUMENT LIST                          
! *****                                                                  
        logical, dimension(1:5) :: ct1b
        logical, dimension(1:5) :: cw1b
        logical, dimension(1:5) :: dt1b
        logical, dimension(1:5) :: dw1b
        double precision, dimension(1:5) :: dt1d
        double precision, dimension(1:5) :: dw1d
        double precision, dimension(1:5) :: bt1d
        double precision, dimension(1:5) :: bw1d
        character(len=20), dimension(1:5) :: a20t1k
        character(len=20), dimension(1:5) :: a20w1k
        character(len=20), dimension(1:5) :: b20t1k
        character(len=20), dimension(1:5) :: b20w1k
        data ft1s / 1.0,2.0,3.0,4.0,5.0 / 
        data gt1s / 1.2,2.3,3.5,4.45,45.0 / 
        data a20t1k / 'AAAALKJHGFASERTYUIOP','KDJFLKJEOITMNV E CJF','CDFEJHFKLM CNB FHGDC','LKJHNHBJMVK,FIJ NVHD','JHGFKDJJSLDKFJDKJFSL' / 
        data b20t1k / 'AAAALSDEFCASERTYUIOP','KDDFFEJEOITMNV E CJF','CDFEJHFKLM     DHGDC','L...NHBJMVK,FIJ NVHD','LKJHDNMVHNEUYHBDGHCJ' / 
        data ct1b / .true.,.false.,.true.,.true.,.false. / 
        data dt1b / .false.,.true.,.false.,.true.,.true. / 
        data dt1d / 1.23d1,2.34d1,3.45d3,5.602d3,5.602d0 / 
        data bt1d / 23.1d1,34.1d1,23.45d3,.625d0,109.384d0 / 
! *****                                                                  
! *****                                                                  
! *****                                                                  
        do ivi = 1, 5                                                
        fw1s(ivi) = ft1s(ivi)                                           
        gw1s(ivi) = gt1s(ivi)                                           
        cw1b(ivi) = ct1b(ivi)                                           
        dw1b(ivi) = dt1b(ivi)                                           
        dw1d(ivi) = dt1d(ivi)                                           
        bw1d(ivi) = bt1d(ivi)                                           
        a20w1k(ivi) = a20t1k(ivi)                                       
        b20w1k(ivi) = b20t1k(ivi)                                       
         end do
! *****                                                                  
! *****                                                                  
! *****                                                                  
        return                                                          
        end subroutine sn913
