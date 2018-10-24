      program fm353
! ***********************************************************************
! *****  FORTRAN 77                                                      
! *****   FM353               XINT - (150)                               
! *****                                                                  
! ***********************************************************************
! *****  GENERAL PURPOSE                                       SUBSET REF
! *****    TEST INTRINSIC FUNCTION - IFIX - (CONVERSION FROM      15.3   
! *****    REAL TO INTEGER)                                     (TABLE 5)
! *****    TEST INTRINSIC FUNCTION - INT - (TRUNCATION -- SIGN           
! *****    OF A * LARGEST INTEGER LE ABS(A) )                            
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
      real :: racvs
      integer :: iaavi
      integer :: iabvi
      integer :: iadvi
      integer :: iaevi
      real :: raavs
      integer :: iafvi
      real :: rabvs
      integer :: iacvi
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
        ivtotl = 14                                                     
      zprog='FM353'                                                     
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
! *****    HEADER FOR SEGMENT 150                                        
        write (nuvi,15001)                                              
15001 format  (" ",/ 2x,"XINT - (150) INTRINSIC FUNCTIONS--"  /17x,              " IFIX, INT (TYPE CONVERSION)" / 2x,                              "SUBSET REF. - 15.3" )                                 
! *****                                                                  
! BB** ********************** BBCHED0B **********************************
! **** WRITE DETAIL REPORT HEADERS                                       
! ****                                                                   
      write (i02,90004)                                                 
      write (i02,90004)                                                 
      write (i02,90013)                                                 
      write (i02,90014)                                                 
      write (i02,90015) ivtotl                                          
! BE** ********************** BBCHED0B **********************************
15003 format(1x,2x,i3,4x,"INSPECT",5x, i5, 5x, i5)                    
15004 format( /48x," BELOW ANSWERS SHOULD BE ZERO " /49x,                     "FOR TEST SEGMENT TO PASS " )                             
15005 format (49x,"- EACH TEST HAS TWO PARTS." )                      
        write (nuvi, 15005)                                             
        write(nuvi, 15004)                                              
        write(nuvi,15002)                                               
15002 format (23x, " IFIX", 5x,  " INT ")                             
! *****                                                                  
! T001*    TEST   1                                       THE VALUE ZERO 
        ivtnum =   1                                                    
        racvs = 0.0                                                     
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 0                                               
        iaevi = iabvi - 0                                               
        write(nuvi,15003)  ivtnum,iadvi, iaevi                          
! T002*    TEST   2                                     A VALUE IN (0,1) 
        ivtnum =   2                                                    
        racvs = 0.375                                                   
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 0                                               
        iaevi = iabvi - 0                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T003*    TEST   3                                        THE VALUE ONE 
        ivtnum =   3                                                    
        racvs = 1.00001                                                 
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 1                                               
        iaevi = iabvi - 1                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T004*    TEST   4                  AN INTEGRAL VALUE OTHER THAN 0 OR 1 
        ivtnum =   4                                                    
        racvs = 6.00001                                                 
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 6                                               
        iaevi = iabvi - 6                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T005*    TEST   5                                   A VALUE IN (X,X+1) 
        ivtnum =   5                                                    
        racvs = 3.75                                                    
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 3                                               
        iaevi = iabvi - 3                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T006*    TEST   6             A NEGATIVE VALUE WITH MAGNITUDE IN (0,1) 
        ivtnum =   6                                                    
        racvs = -0.375                                                  
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 0                                               
        iaevi = iabvi - 0                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T007*    TEST   7                                         THE VALUE -1 
        ivtnum =   7                                                    
        racvs = -1.00001                                                
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi + 1                                               
        iaevi = iabvi + 1                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T008*    TEST   8                            A NEGATIVE INTEGRAL VALUE 
        ivtnum =   8                                                    
        racvs = -6.00001                                                
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi + 6                                               
        iaevi = iabvi + 6                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T009*    TEST   9           A NEGATIVE VALUE WITH MAGNITUDE IN (X,X+1) 
        ivtnum =   9                                                    
        racvs = -3.75                                                   
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi + 3                                               
        iaevi = iabvi + 3                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T010*    TEST  10                      ZERO PREFIXED WITH A MINUS SIGN 
        ivtnum =  10                                                    
        racvs = 0                                                       
        iaavi = ifix(-racvs)                                            
        iabvi = int(-racvs)                                             
        iadvi = iaavi - 0                                               
        iaevi = iabvi - 0                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T011*    TEST 011           IFIX, INT USED IN AN ARITHMETIC EXPRESSION 
        ivtnum = 011                                                    
        raavs = 3.75                                                    
        iafvi = 3                                                       
        iaavi = 25 + iafvi * ifix(raavs)                                
        iabvi = 25 + iafvi * int(raavs)                                 
        iadvi = iaavi - 34                                              
        iaevi = iabvi - 34                                              
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T012*    TEST  12      AN ARITHMETIC EXPRESSION PRESENTED TO IFIX, INT 
        ivtnum =  12                                                    
        raavs = 25.5                                                    
        rabvs = 12.25                                                   
        iaavi = ifix(raavs - rabvs)                                     
        iabvi = int(raavs - rabvs)                                      
        iadvi = iaavi - 13                                              
        iaevi = iabvi - 13                                              
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T013*    TEST  13        COMPARE AUTOMATIC TYPE CONVERSION TO EXPLICIT 
        ivtnum =  13                                                    
        raavs = 11.75                                                   
        rabvs = 12.625                                                  
        iaavi = ifix(raavs + rabvs)                                     
        iabvi = int(raavs + rabvs)                                      
        iacvi = raavs + rabvs                                           
        iadvi = iaavi - iacvi                                           
        iaevi = iabvi - iacvi                                           
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! T014*    TEST  14                            ARGUMENT OF LOW MAGNITUDE 
        ivtnum =  14                                                    
        racvs = -3.05923e-33                                            
        iaavi = ifix(racvs)                                             
        iabvi = int(racvs)                                              
        iadvi = iaavi - 0                                               
        iaevi = iabvi - 0                                               
        write(nuvi,15003)  ivtnum, iadvi, iaevi                         
! *****                                                                  
! *****                                                                  
        ivinsp = 14                                                     
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
! *****    END OF TEST SEGMENT 150                                       
        stop                                                            
        end program fm353
