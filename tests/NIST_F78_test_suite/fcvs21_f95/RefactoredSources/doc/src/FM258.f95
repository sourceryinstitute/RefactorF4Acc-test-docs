      program fm258
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM258                                                          
! *****                       BLKIF1 - (300)                             
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                      SUBSET REF 
! *****    TEST BLOCK IF STATEMENTS                          11.6 - 11.9 
! *****    SIMPLE TESTS OF IF (E) THEN,ELSE,ELSEIF,ENDIF                 
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
      integer :: nuvi
      integer :: ivtnum
      integer :: jvi
      integer :: kvi
      integer :: lvi
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
           zprog='FM258'                                                
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
! ***** TOTAL NUMBER OF EXPECTED TEST                                    
        ivtotl=8                                                        
! *****    HEADER FOR SEGMENT 300                                        
        write(nuvi,30000)                                               
30000 format(/1x," BLKIF1 - (300) BLOCK IF - SIMPLE TEST" //                       "  SUBSET REF.  11.6 - 11.9" )                       
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
           write (nuvi,30025)                                           
! T001*  TEST 1                  IF (E) THEN .*. ELSE .*. ENDIF          
           ivtnum = 1                                                   
           ivinsp=ivinsp+1                                              
           write(nuvi,80004) ivtnum                                     
        jvi = 0                                                         
30001 jvi = jvi + 1                                                   
        if (jvi  ==  2) then                                            
                kvi = 2                                                 
           else                                                         
                kvi = 1                                                 
         endif                                                          
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30001,30002), jvi                                          
30002 continue                                                        
! T002*  TEST 2                          IF (E) THEN .*. ENDIF           
           ivtnum = 2                                                   
           ivinsp=ivinsp+1                                              
           write(nuvi,80004) ivtnum                                     
        jvi = 0                                                         
        kvi = 1                                                         
30003 jvi = jvi + 1                                                   
        if (jvi  ==  2) then                                            
                kvi = 2                                                 
         endif                                                          
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30003,30004), jvi                                          
30004 continue                                                        
! T003*  TEST 3                  IF (E) THEN ... ELSE .*. ENDIF          
           ivtnum = 3                                                   
           ivinsp=ivinsp+1                                              
           write(nuvi,80004) ivtnum                                     
        jvi = 0                                                         
        kvi = 1                                                         
30005 jvi = jvi + 1                                                   
        if (jvi  ==  1) then                                            
           else                                                         
            kvi = 2                                                     
         endif                                                          
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30005,30006), jvi                                          
30006 continue                                                        
! T004*  TEST 4       IF (E) THEN .*. ELSEIF .*. ELSE .*. ENDIF          
           ivtnum = 4                                                   
           ivinsp=ivinsp+1                                              
           write(nuvi,80004) ivtnum                                     
        jvi = 0                                                         
30007 jvi = jvi + 1                                                   
        if (jvi  ==  1) then                                            
                kvi = 1                                                 
           elseif (jvi .eq. 2) then                                     
                   kvi = 2                                              
                else                                                    
                   kvi = 3                                              
         endif                                                          
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30007,30007,30008), jvi                                    
30008 continue                                                        
! T005*  TEST 5      IF (E) THEN .*. ELSEIF .*. ENDIF                    
          ivtnum = 5                                                    
          ivinsp=ivinsp+1                                               
          write(nuvi,80004) ivtnum                                      
        jvi = 0                                                         
        kvi = 1                                                         
30009 jvi = jvi + 1                                                   
        if (jvi  >  2) then                                            
                kvi = 3                                                 
         elseif (jvi .eq. 2) then                                       
                kvi = 2                                                 
        endif                                                           
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30009,30009,30010), jvi                                    
30010 continue                                                        
! T006*  TEST 6      IF (E) THEN .*. ELSEIF ... ELSE .*. ENDIF           
          ivtnum = 6                                                    
          ivinsp=ivinsp+1                                               
          write(nuvi,80004) ivtnum                                      
        jvi = 0                                                         
        kvi = 1                                                         
30011 jvi = jvi + 1                                                   
        if ( jvi  >  2) then                                           
                kvi = 3                                                 
        elseif (jvi .eq. 1) then                                        
                  else                                                  
                        kvi = 2                                         
        endif                                                           
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30011,30011,30012), jvi                                    
30012 continue                                                        
! T007*  TEST 7      IF (E) THEN ... ELSEIF .*. ELSE .*. ENDIF           
           ivtnum = 7                                                   
           ivinsp=ivinsp+1                                              
           write(nuvi,80004) ivtnum                                     
        jvi = 0                                                         
        kvi = 1                                                         
30013 jvi = jvi + 1                                                   
        if (jvi  ==  1) then                                            
            elseif (jvi .lt. 3) then                                    
                    kvi = 2                                             
                else                                                    
                    kvi = 3                                             
        endif                                                           
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30013,30013,30014), jvi                                    
30014 continue                                                        
! T008*  TEST 8      IF (E) THEN .*. ELSEIF .*. ELSEIF .*. ENDIF         
           ivtnum = 8                                                   
           ivinsp=ivinsp+1                                              
           write(nuvi,80004) ivtnum                                     
        jvi = 0                                                         
30015 jvi = jvi + 1                                                   
        kvi = 4                                                         
        if ( jvi  ==  1) then                                           
                kvi = 1                                                 
            elseif (jvi .eq. 2) then                                    
                        kvi = 2                                         
            elseif (jvi .lt. 4) then                                    
                        kvi = 3                                         
        endif                                                           
        lvi = jvi - kvi                                                 
        write(nuvi,30018) lvi                                           
        goto(30015,30015,30015,30016), jvi                              
! *****                                                                  
30016 continue                                                        
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
30026 format ("1", 26x,i1)                                            
30018 format(" ",26x,i10)                                             
30025 format(/49x,"TESTS 1-3 (2 COMPUTED RESULTS)" ,                      /49x,"TESTS 4-7 (3 COMPUTED RESULTS)" ,                           /49x,"TEST  8   (4 COMPUTED RESULTS)" ,                           /49x,"ALL ANSWERS SHOULD BE ZERO" )                           
! *****    END OF TEST SEGMENT 300                                       
      stop                                                              
      end program fm258
