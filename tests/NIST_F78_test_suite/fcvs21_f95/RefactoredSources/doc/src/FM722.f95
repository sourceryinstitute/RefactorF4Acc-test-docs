      program fm722
!                                                                        
!      *************************************************************     
!      THE FULL LANGUAGE SET ALLOWS DATA TYPES TO BE DECLARED DOUBLE     
!      PRECISION AND COMPLEX.                                            
!      (NIST TEST/PROGRAM IDENTIFICATION S04AF-2P)                       
!      *************************************************************     
!      REFERENCES.                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!            X3.9-1978                                                   
!                                                                        
!         SECTION 4  DATA TYPES AND CONSTANTS                            
!           PARAGRAPHS:                                                  
!                                                                        
!           4.1                                                          
!           4.1.2                                                        
!                                                                        
!         SECTION 8  SPECIFICATION STATEMENTS                            
!           PARAGRAPHS:                                                  
!           8.4.1                                                        
!           8.6                                                          
!                                                                        
!           TEST DATA TYPES DOUBLE PRECISION AND COMPLEX USING:          
!                                                                        
!             TYP V [,V1]                                                
!                                                                        
!             TYP = DOUBLE PRECISION OR COMPLEX                          
!               V = VARIABLE NAME, ARRAY NAME, ARRAY DECLARATOR,         
!                   SYMBOLIC NAME OF A CONSTANT, FUNCTION NAME,          
!                   OR DUMMY PROCEDURE NAME                              
!                                                                        
!      FM722 USES FUNCTIONS DF723, ZF724 AND SUBROUTINE SN725            
!      ****************************************************************  
!                                                                        
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
!                                                                        
! BB** ********************** BBCINITA **********************************
! **** SPECIFICATION STATEMENTS                                          
! ****                                                                   
      double precision  :: dvc006
      doubleprecision :: dsn001
      doubleprecision :: dvn003
      doubleprecision :: dvn004
      doubleprecision :: dsn006
      doubleprecision :: dvn008
      doubleprecision :: dvn007
      complex :: zsn001
      real :: rvn001
      real :: rvn002
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: ivinsp
      integer :: ivtotl
      integer :: ivtotn
      integer :: iczero
      integer :: i01
      integer :: i02
      integer :: ivtnum
      doubleprecision :: dvcomp
      doubleprecision :: dvcorr
      doubleprecision :: dvn009
      doubleprecision :: dvn010
      complex :: zvcomp
      complex :: zvcorr
      complex :: zvn001
      complex :: zvn002
      real :: rvn004
      real :: rvn005
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
! BE** ********************** BBCINITA **********************************
      double precision :: nvcomp
      double precision :: df723! decl of func/sub in program
      complex, dimension(1:2) :: i2n002
      complex :: zf724! decl of func/sub in program
      real, dimension(1:2) :: r2nn02
      equivalence (zvcomp,r2nn02)                                       
      real, parameter :: dpn001=5.834d6
      integer, parameter :: ipn001=2
      real, parameter :: dcn004=1.456d3
      complex, parameter :: icp001=(3.2,2.3)
      doubleprecision, dimension(1:ipn001) :: d2n001
      external df723,zf724                                              
      data d2n001(1),d2n001(2) / ipn001*dcn004 / 
      data i2n002(1),i2n002(2) / ipn001*(3.2,2.3) / 
      dsn001(dvn003,dvn004) = dvn003 + dvn004                           
      dsn006(dvn007,dvn008) = (dsn001(dvn007,dvn007) + dvn008)          
      zsn001(rvn001,rvn002) = cmplx(rvn001,rvn002) +                    cmplx(rvn002,rvn002)                                              
      data zvers,zversd,zdate / 'VERSION 2.1  ','93/10/21*21.02.00','*NO DATE*TIME' / 
      data zcompl,zname,ztape / '*NONE SPECIFIED*','*NO COMPANY NAME*','*NO TAPE*' / 
      data zproj,ztaped,zprog / '*NO PROJECT*','*NO TAPE DATE','XXXXX' / 
      data remrks / '                               ' / 
!                                                                        
!                                                                        
! BB** ********************** BBCINITB **********************************
! **** INITIALIZE SECTION                                                
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
           zprog='FM722'                                                
           ivtotl =  12                                                 
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
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 001 IS DESIGNED TO TEST A DOUBLE PRECISION CONSTANT     
!           VALUE SET WITH PARAMETER STATEMENT                           
!                                                                        
           ivtnum =   1                                                 
        dvcomp=0.0d0                                                    
        dvcomp=dpn001                                                   
        dvcorr=5.834d6                                                  
           if  (dpn001 - 5.833999997d6) 20010,10010,40010               
40010 if  (dpn001 - 5.834000003d6) 10010,10010,20010               
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 002 IS DESIGNED TO TEST A DOUBLE PRECISION VARIABLE     
!                                                                        
           ivtnum =   2                                                 
        dvcomp=0.0d0                                                    
        nvcomp=.1212345d2                                               
        dvcomp=nvcomp                                                   
        dvcorr=.1212345d2                                               
           if  (nvcomp - .1212344999d2) 20020,40021,40020               
40020 if  (nvcomp - .1212345001d2) 40021,40021,20020               
40021 dvcomp = dvcomp + .1212345d2                                    
        dvcorr=.2424690d2                                               
           if  (dvcomp - .2424689998d2) 20020,10020,40022               
40022 if  (dvcomp - .2424690002d2) 10020,10020,20020               
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0021      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 003 A DOUBLE PRECISION ARRAY                            
!                                                                        
           ivtnum =   3                                                 
        dvcomp=0.0d0                                                    
        dvcorr=2.912d3                                                  
        dvcomp=d2n001(1) + d2n001(2)                                    
           if  (dvcomp - 2.911999998d3) 20030,10030,40030               
40030 if  (dvcomp - 2.912000002d3) 10030,10030,20030               
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0031      continue                                                     
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 004 IS DESIGNED TO TEST A DOUBLE PRECISION FUNCTION     
!           DF723                                                        
!                                                                        
           ivtnum =   4                                                 
        dvcomp=0.0d0                                                    
        dvn009=.1211d2                                                  
        dvcomp = df723(dvn009)
        dvcorr=1.001211d4                                               
           if  (dvcomp - 1.001210999d4) 20040,10040,40040               
40040 if  (dvcomp - 1.001211001d4) 10040,10040,20040               
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 005 IS DESIGNED TO TEST A DOUBLE PRECISION DUMMY        
!           PROCEDURE (DF723 USED AS DUMMY ARGUMENT FOR SUBROUTINE       
!           FS528                                                        
!                                                                        
           ivtnum =   5                                                 
        dvcomp=0.0d0                                                    
        dvcorr=1200000.0d-2                                             
        dvn009=0.0d0                                                    
        dvn009=10d2                                                     
        call sn725(df723,dvn009,dvc006)

        dvcomp=dvc006                                                   
           if  (dvcomp - .1199999999d5) 20050,10050,40050               
40050 if  (dvcomp - .1200000001d5) 10050,10050,20050               
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0051      continue                                                     
!                                                                        
! T006*  TEST 006   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 006 DOUBLE PRECISION FUNCTION NAME USING                
!           STATEMENT FUNCTION STATEMENT                                 
!                                                                        
           ivtnum =   6                                                 
        dvcomp=0.0d0                                                    
        dvcorr=20d2                                                     
        dvn009=10d2                                                     
        dvn010=10d2                                                     
        dvcomp=dsn001(dvn009,dvn010)                                    
           if  (dvcomp - 19.99999999d2) 20060,10060,40060               
40060 if  (dvcomp - 20.00000001d2) 10060,10060,20060               
10060 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0061                                                   
20060 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0061      continue                                                     
!                                                                        
! T007*  TEST 007   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 007 DOUBLE PRECISION FUNCTION NAME USED IN              
!           A STATEMENT FUNCTION STATEMENT AS A DUMMY ARGUMENT           
!                                                                        
           ivtnum =   7                                                 
        dvcomp=0.0d0                                                    
        dvcorr=30d2                                                     
        dvn009=10d2                                                     
        dvn010=10d2                                                     
        dvcomp=dsn006(dvn009,dvn010)                                    
           if  (dvcomp - 29.99999998d2) 20070,10070,40070               
40070 if  (dvcomp - 30.00000002d2) 10070,10070,20070               
10070 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0071                                                   
20070 ivfail = ivfail + 1                                          
           write (i02,80031) ivtnum, dvcomp, dvcorr                     
 0071      continue                                                     
!                                                                        
!           THE FOLLOWING GROUP OF TESTS ARE DESIGNED TO                 
!           TEST COMPLEX DATA TYPES                                      
!                                                                        
!                                                                        
! T008*  TEST 008   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 008 DATA TYPE CAN BE A COMPLEX VARIABLE                 
!                                                                        
           ivtnum =   8                                                 
        zvcomp=(0.0, 0.0)                                               
        zvcorr=(1.0, 1.0)                                               
        zvn001=(6.5, 2.2)                                               
        zvn002=(5.5, 1.2)                                               
        zvcomp=zvn001-zvn002                                            
           if  (r2nn02(1) - 0.9995) 20080,40081,40080                   
40080 if  (r2nn02(1) - 1.0001) 40081,40081,20080                   
40081 if  (r2nn02(2) - 0.9995) 20080,10080,40082                   
40082 if  (r2nn02(2) - 1.0001) 10080,10080,20080                   
10080 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0081                                                   
20080 ivfail = ivfail + 1                                          
           write (i02,80045) ivtnum, zvcomp, zvcorr                     
 0081      continue                                                     
!                                                                        
! T009*  TEST 009   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 009 COMPLEX CONSTANT                                    
!                                                                        
           ivtnum =   9                                                 
        zvcomp=(0.0, 0.0)                                               
        zvcorr=(6.4, 4.6)                                               
        zvcomp=icp001+icp001                                            
           if  (r2nn02(1) - 6.3996) 20090,10090,40090                   
40090 if  (r2nn02(1) - 6.4004) 40091,40091,20090                   
40091 if  (r2nn02(2) - 4.5997) 20090,10090,40092                   
40092 if  (r2nn02(2) - 4.6003) 10090,10090,20090                   
10090 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0091                                                   
20090 ivfail = ivfail + 1                                          
           write (i02,80045) ivtnum, zvcomp, zvcorr                     
 0091      continue                                                     
!                                                                        
! T010*  TEST 010   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 010 COMPLEX ARRAY                                       
!                                                                        
           ivtnum =  10                                                 
        zvcomp=(0.0, 0.0)                                               
        zvcorr=(6.4, 4.6)                                               
        zvcomp=i2n002(1)+i2n002(2)                                      
           if  (r2nn02(1) - 6.3996) 20100,10100,40100                   
40100 if  (r2nn02(1) - 6.4004) 40101,40101,20100                   
40101 if  (r2nn02(2) - 4.5997) 20100,10100,40102                   
40102 if  (r2nn02(2) - 4.6003) 10100,10100,20100                   
10100 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0101                                                   
20100 ivfail = ivfail + 1                                          
           write (i02,80045) ivtnum, zvcomp, zvcorr                     
 0101      continue                                                     
!                                                                        
! T011*  TEST 011   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 011    COMPLEX FUNCTION NAME (USING STATEMENT FUNCTION) 
!           FUNCTION NAME CAN BE COMPLEX                                 
!                                                                        
           ivtnum =  11                                                 
        zvcorr=(3.0, 4.0)                                               
        zvcomp=(0.0, 0.0)                                               
        rvn004=1.0                                                      
        rvn005=2.0                                                      
        zvcomp=(zsn001(rvn004,rvn005))                                  
           if  (r2nn02(1) - 2.9998) 20110,10110,40110                   
40110 if  (r2nn02(1) - 3.0002) 40111,40111,20110                   
40111 if  (r2nn02(2) - 3.9998) 20110,10110,40112                   
40112 if  (r2nn02(2) - 4.0002) 10110,10110,20110                   
10110 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0111                                                   
20110 ivfail = ivfail + 1                                          
           write (i02,80045) ivtnum, zvcomp, zvcorr                     
 0111      continue                                                     
!                                                                        
! T012*  TEST 012   ****  FCVS PROGRAM 722  ****                         
!                                                                        
!           TEST 012 TEST COMPLEX FUNCTION NAME IN A FUNCTION SUBPROGRAM 
!                                                                        
           ivtnum =  12                                                 
        zvcorr=(3.0, 4.0)                                               
        zvcomp=(0.0, 0.0)                                               
        rvn004=1.0                                                      
        rvn005=2.0                                                      
        zvcomp = zf724(rvn004,rvn005)
           if  (r2nn02(1) - 2.9998) 20120,10120,40120                   
40120 if  (r2nn02(1) - 3.0002) 40121,40121,20120                   
40121 if  (r2nn02(2) - 3.9998) 20120,10120,40122                   
40122 if  (r2nn02(2) - 4.0002) 10120,10120,20120                   
10120 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0121                                                   
20120 ivfail = ivfail + 1                                          
           write (i02,80045) ivtnum, zvcomp, zvcorr                     
 0121      continue                                                     
!                                                                        
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
90001 format (" ",56x,"FM722")                                          
90000 format (" ",50x,"END OF PROGRAM FM722" )                          
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
           end program fm722
      double precision function df723(dvn008)
      double precision  :: dvn008
!           THIS FUNCTION IS USED BY PROGRAM FM722 TO TEST               
!           DOUBLE PRECISION FUNCTIONS                                   
        df723=dvn008 + 100d2                                            
        return                                                          
        end function df723
      complex function zf724(rvn006,rvn007)
      real :: rvn006
      real :: rvn007
!           THIS FUNCTION IS USED BY PROGRAM FM722 TO TEST               
!           COMPLEX FUNCTION NAME                                        
        zf724= cmplx(rvn006,rvn007) + cmplx(rvn007,rvn007)              
        return                                                          
        end function zf724
      subroutine sn725(dtint,dvn008,dvc006)
      double precision  :: dvc006
      double precision , external :: dtint
      double precision  :: dvn008
!           THIS ROUTINE IS USED BY PROGRAM FM722                        
!           TO TEST A DOUBLE PRECISION FUNCTION NAME USED AS AN          
!           ACTUAL ARGUMENT                                              
        dvc006=dtint(dvn008) + 10d2                                     
        return                                                          
        end subroutine sn725
