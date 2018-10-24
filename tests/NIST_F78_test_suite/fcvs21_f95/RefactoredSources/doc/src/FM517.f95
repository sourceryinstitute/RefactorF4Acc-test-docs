      program fm517
!                                                                        
!      THIS PROGRAM TESTS THE RETURN STATEMENT              ANS REF.     
!                        RETURN E                           15.8.1       
!      IN SUBROUTINE SUBPROGRAMS.  E IS AN ARITHMETIC       15.8.3       
!      EXPRESSION WHOSE VALUE INDICATES WHERE CONTROL                    
!      WILL BE RETURNED TO.                                              
!                                                                        
!      THIS ROUTINE USES SUBROUTINE SUBPROGRAMS SN518                    
!      AND SN519                                                         
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
! BB** ********************** BBCINITA **********************************
! **** SPECIFICATION STATEMENTS                                          
! ****                                                                   
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
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivn001
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
!                                                                        
!                                                                        
!                                                                        
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
           zprog = 'FM517'                                              
           ivtotl = 5                                                   
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
!      TESTS 1 AND 2 TEST RETURN CONTROL PROCESSING IN THE EXECUTION     
!      OF A SUBROUTINE SUBPROGRAM WHICH PROVIDES ALTERNATE RETURN        
!                                                                        
! T001*  TEST 001   ****  FCVS PROGRAM 517  ****                         
!                                                                        
           ivtnum = 1                                                   
           ivcomp = 0                                                   
           ivcorr =     3                                               
      ivn001 = 2                                                        
      call sn518(ivn001,*0012,*0013)

      ivcomp = 1                                                        
      goto 0014                                                        
0012  continue                                                          
      ivcomp = 2                                                        
      goto 0014                                                        
0013  continue                                                          
      ivcomp = 3                                                        
0014  continue                                                          
40010 if (ivcomp -     3) 20010, 10010, 20010                      
10010 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0011                                                   
20010 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0011      continue                                                     
!                                                                        
! T002*  TEST 002   ****  FCVS PROGRAM 517  ****                         
!                                                                        
           ivtnum = 2                                                   
           ivcomp = 0                                                   
           ivcorr =     5                                               
      call sn519(7,*0022,*0023)

      ivcomp = 1                                                        
      goto 0024                                                        
0022  continue                                                          
      ivcomp = 3                                                        
      goto 0024                                                        
0023  continue                                                          
      ivcomp = 5                                                        
0024  continue                                                          
40020 if (ivcomp -     5) 20020, 10020, 20020                      
10020 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0021                                                   
20020 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0021      continue                                                     
!                                                                        
! T003*  TEST 003   ****  FCVS PROGRAM 517  ****                         
!      TEST 003 TESTS THE "RETURN E" STATEMENT WHERE E HAS VALUE         
!      LESS THAN ONE                                                     
!                                                                        
           ivtnum = 3                                                   
           ivcomp = 0                                                   
           ivcorr =    -2                                               
      call sn518(-3,*0032,*0033)

      ivcomp = -2                                                       
      goto 0034                                                        
0032  continue                                                          
      ivcomp = -4                                                       
      goto 0034                                                        
0033  continue                                                          
      ivcomp = -6                                                       
0034  continue                                                          
40030 if (ivcomp +     2) 20030, 10030, 20030                      
10030 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0031                                                   
20030 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0031      continue                                                     
!                                                                        
! T004*  TEST 004   ****  FCVS PROGRAM 517  ****                         
!      TEST 004 TESTS THE "RETURN E" STATEMENT WHERE E HAS VALUE         
!      GREATER THAN THE NUMBER OF ASTERISKS IN A SUBROUTINE STATEMENT    
!                                                                        
           ivtnum = 4                                                   
           ivcomp = 0                                                   
           ivcorr =     7                                               
      call sn518(3,*0042,*0043)

      ivcomp = 7                                                        
      goto 0044                                                        
0042  continue                                                          
      ivcomp = 9                                                        
      goto 0044                                                        
0043  continue                                                          
      ivcomp = 11                                                       
0044  continue                                                          
40040 if (ivcomp -     7) 20040, 10040, 20040                      
10040 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0041                                                   
20040 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0041      continue                                                     
!                                                                        
! T005*  TEST 005   ****  FCVS PROGRAM 517  ****                         
!      TEST 005 TESTS THE "RETURN E" STATEMENT WHERE E HAS VALUE         
!      GREATER THAN THE NUMBER OF ASTERISKS IN AN ENTRY STATEMENT        
!                                                                        
           ivtnum = 5                                                   
           ivcomp = 0                                                   
           ivcorr =   -10                                               
      call en872(9,*0052,*0053)

      ivcomp = -10                                                      
      goto 0054                                                        
0052  continue                                                          
      ivcomp = 3                                                        
      goto 0054                                                        
0053  continue                                                          
      ivcomp = 11                                                       
0054  continue                                                          
40050 if (ivcomp +    10) 20050, 10050, 20050                      
10050 ivpass = ivpass + 1                                          
           write (i02,80002) ivtnum                                     
           goto 0051                                                   
20050 ivfail = ivfail + 1                                          
           write (i02,80010) ivtnum, ivcomp, ivcorr                     
 0051      continue                                                     
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
90001 format (" ",56x,"FM517")                                          
90000 format (" ",50x,"END OF PROGRAM FM517" )                          
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
      end program fm517
      subroutine sn518(ivd001,*,*)
      integer :: ivd001
      return ivd001                                                     
      end subroutine sn518
      subroutine sn519(ivd001,*,*)
      integer :: ivd001
      return (ivd001 - 2*(ivd001/2) + 1)                                
      entry en872(ivd002,*,*)
      return (ivd002 - 3)                                               
      end subroutine sn519
