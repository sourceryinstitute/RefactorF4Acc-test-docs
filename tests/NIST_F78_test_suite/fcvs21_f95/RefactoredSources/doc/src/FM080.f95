      program fm080
!      COMMENT SECTION                                                   
!                                                                        
!      FM080                                                             
!                                                                        
!          THIS ROUTINE CONTAINS EXTERNAL FUNCTION REFERENCE TESTS.      
!      THE FUNCTION SUBPROGRAMS CALLED BY THIS ROUTINE ARE FF081,        
!      FF082 AND FF083.  THE FUNCTION SUBPROGRAMS ARE DEFINED AS         
!      FF081 = INTEGER, FF082 = REAL, FF083 = IMPLICIT REAL.             
!      THE FUNCTION SUBPROGRAM DUMMY ARGUMENTS MUST AGREE IN ORDER,      
!      NUMBER AND TYPE WITH THE CORRESPONDING ACTUAL ARGUMENTS OF THE    
!      MAIN PROGRAM.     THE ARGUMENTS OF THE FUNCTION SUBPROGRAMS WILL  
!      CORRESPOND TO ACTUAL ARGUMENT LIST REFERENCES OF VARIABLE-NAME,   
!      ARRAY-NAME, ARRAY-ELEMENT-NAME AND EXPRESSION RESPECTIVELY.       
!                                                                        
!          THIS ROUTINE WILL TEST THE VALUE OF THE FUNCTION AND THE      
!      FUNCTION ARGUMENTS RETURNED FOLLOWING THE FUNCTION REFERENCE CALL.
!                                                                        
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 2.6, ARRAY                                             
!         SECTION 15.5.2, REFERENCING EXTERNAL FUNCTIONS                 
!         SECTION 17.2, EVENTS THAT CAUSE ENTITIES TO BECOME DEFINED     
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon0a
      integer :: ivon02
      integer :: ivcorr
      integer :: ivcomp
      real :: rvon0a
      real :: rvcorr
      real :: rvcomp
      real :: rvon01
      integer :: ivon01
      real :: rvon02
      real :: rvon03
      integer :: ivon03
      integer, dimension(1:5) :: iadn1a
      integer, dimension(1:4,1:4) :: iadn2a
      real, dimension(1:3,1:6,1:3) :: radn3a
      real, dimension(1:10) :: radn1a
      integer, dimension(1:3,1:4,1:5) :: iadn3a
      integer :: ff081
      real :: ff082
!                                                                        
!       **********************************************************       
!                                                                        
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN NATIONAL STANDARD  
!      PROGRAMMING LANGUAGE FORTRAN X3.9-1978, HAS BEEN DEVELOPED BY THE 
!      FEDERAL COBOL COMPILER TESTING SERVICE.  THE FORTRAN COMPILER     
!      VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT ROUTINES, THEIR RELATED
!      DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT ROUTINE IS A FORTRAN   
!      PROGRAM, SUBPROGRAM OR FUNCTION WHICH INCLUDES TESTS OF SPECIFIC  
!      LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING THE RESULT 
!      OF EXECUTING THESE TESTS.                                         
!                                                                        
!          THIS PARTICULAR PROGRAM/SUBPROGRAM/FUNCTION CONTAINS FEATURES 
!      FOUND ONLY IN THE SUBSET AS DEFINED IN X3.9-1978.                 
!                                                                        
!          SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO -             
!                                                                        
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!       **********************************************************       
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION                                            
!                                                                        
!      INITIALIZE CONSTANTS                                              
!       **************                                                   
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER.         
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER.             
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD. 
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD. 
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD. 
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021    THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD. 
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass=0                                                          
      ivfail=0                                                          
      ivdele=0                                                          
      iczero=0                                                          
!                                                                        
!      WRITE PAGE HEADERS                                                
      write (i02,90000)                                                 
      write (i02,90001)                                                 
      write (i02,90002)                                                 
      write (i02, 90002)                                                
      write (i02,90003)                                                 
      write (i02,90002)                                                 
      write (i02,90004)                                                 
      write (i02,90002)                                                 
      write (i02,90011)                                                 
      write (i02,90002)                                                 
      write (i02,90002)                                                 
      write (i02,90005)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!      EXTERNAL FUNCTION REFERENCE  -  FUNCTION SUBPROGRAM DEFINED AS    
!                                      INTEGER (FF081)                   
!                                                                        
 6741 continue                                                          
      ivtnum = 674                                                      
!                                                                        
!          TEST 674 THROUGH 679 TEST THE FUNCTION AND ARGUMENT VALUES    
!      FROM REFERENCE OF FUNCTION FF081.  FUNCTION SUBPROGRAM FF081 IS   
!      DEFINED AS INTEGER.                                               
!                                                                        
!      **** TEST 674 ****                                                
!                                                                        
!      TEST 674 TESTS THE FUNCTION VALUE RETURNED FROM FUNCTION FF081    
!                                                                        
      if (iczero) 36740,6740,36740                                      
 6740 continue                                                          
      ivon0a        = 0                                                 
      ivon02        = 2                                                 
      iadn1a (3)    = 8                                                 
      iadn1a (2)    = 4                                                 
      iadn2a (1,3)  =10                                                 
      ivon0a  = ff081(ivon02,iadn1a,iadn2a,999)
      goto 46740                                                       
36740 ivdele =  ivdele + 1                                              
      write (i02,80003) ivtnum                                          
      if (iczero) 46740,6751,46740                                      
46740 if (ivon0a - 1015) 26740,16740,26740                              
16740 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6751                                                        
26740 ivfail = ivfail + 1                                               
      ivcorr = 1015                                                     
      ivcomp = ivon0a                                                   
      write  (i02,80004) ivtnum, ivcomp, ivcorr                         
 6751 continue                                                          
      ivtnum = 675                                                      
!                                                                        
!      ****  TEST 675  ****                                              
!                                                                        
!          TEST 675 TESTS THE RETURN VALUE OF VARIABLE-NAME ARGUMENT     
!      IVON02.   VALUE OF IVON02 SHOULD BE 4.                            
!                                                                        
      if (iczero) 36750,6750,36750                                      
 6750 continue                                                          
      goto 46750                                                       
36750 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46750,6761,46750                                      
46750 if (ivon02 - 4) 26750,16750,26750                                 
16750 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6761                                                        
26750 ivfail = ivfail + 1                                               
      ivcorr = 4                                                        
      ivcomp = ivon02                                                   
      write  (i02,80004) ivtnum, ivcomp, ivcorr                         
 6761 continue                                                          
      ivtnum = 676                                                      
!                                                                        
!      ****  TEST 676  ****                                              
!                                                                        
!          TEST 676 TESTS THE RETURN VALUE OF ARRAY-NAME ARGUMENT        
!      IADN1A.  IADN1A (2) IS INCREMENTED BY 40 IN FUNCTION SUBPROGRAM   
!      AND SHOULD RETURN A VALUE OF 44.                                  
!                                                                        
      if (iczero) 36760,6760,36760                                      
 6760 continue                                                          
      goto 46760                                                       
36760 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46760,6771,46760                                      
46760 if (iadn1a (2) - 44) 26760,16760,26760                            
16760 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6771                                                        
26760 ivfail = ivfail + 1                                               
      ivcorr = 44                                                       
      ivcomp = iadn1a (2)                                               
      write  (i02,80004) ivtnum, ivcomp, ivcorr                         
 6771 continue                                                          
      ivtnum = 677                                                      
!                                                                        
!      ****  TEST 677  ****                                              
!                                                                        
!         TEST 677 TESTS THE RETURN VALUE OF ARRAY-NAME ARGUMENT IADN1A. 
!      IADN1A (3) WAS NOT MODIFFED    BY FUNCTION SUBPROGRAM AND SHOULD  
!      HAVE A VALUE OF 8                                                 
!                                                                        
      if (iczero) 36770,6770,36770                                      
 6770 continue                                                          
      goto 46770                                                       
36770 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46770,6781,46770                                      
46770 if (iadn1a (3) - 8) 26770,16770,26770                             
16770 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6781                                                        
26770 ivfail = ivfail + 1                                               
      ivcorr = 8                                                        
      ivcomp = iadn1a (3)                                               
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6781 continue                                                          
      ivtnum = 678                                                      
!                                                                        
!      ****  TEST 678  ****                                              
!                                                                        
!          TEST 678 TESTS THE RETURN VALUE OF ARRAY-ELEMENT-NAME         
!      IADN2A (1,3).  IADN2A (1,3) WAS INCREMENTED BY 70 IN THE FUNCTION 
!      SUBPROGRAM AND SHOULD CONTAIN A VALUE OF 80.                      
!                                                                        
      if (iczero) 36780,6780,36780                                      
 6780 continue                                                          
      goto 46780                                                       
36780 ivdele = ivdele + 1                                               
      write  (i02,80003) ivtnum                                         
      if (iczero) 46780,6791,46780                                      
46780 if (iadn2a (1,3) - 80) 26780,16780,26780                          
16780 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6791                                                        
26780 ivfail = ivfail + 1                                               
      ivcorr = 80                                                       
      ivcomp = iadn2a (1,3)                                             
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6791 continue                                                          
      ivtnum = 679                                                      
!                                                                        
!      ****  TEST 679  ****                                              
!                                                                        
!          TEST 679  TESTS THE VALUE OF INTEGER FUNCTION ASSIGNED        
!      TO A REAL VARIABLE.                                               
!                                                                        
      if (iczero) 36790,6790,36790                                      
 6790 continue                                                          
      rvon0a        = 0.0                                               
      ivon02        = 2                                                 
      iadn1a (2)    = 4                                                 
      iadn2a (1,3)  = 10                                                
      rvon0a  = ff081(ivon02,iadn1a,iadn2a,999)
      goto 46790                                                       
36790 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46790,6801,46790                                      
46790 if (rvon0a - 1014.5) 26790,16790,46791                            
46791 if (rvon0a - 1015.5) 16790,16790,26790                            
16790 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6801                                                        
26790 ivfail = ivfail + 1                                               
      rvcorr = 1015.0                                                   
      rvcomp = rvon0a                                                   
      write  (i02,80005) ivtnum, rvcomp, rvcorr                         
 6801 continue                                                          
      ivtnum = 680                                                      
!                                                                        
!      EXTERNAL FUNCTION REFERENCE - FUNCTION SUBPROGRAM FF082 DEFINED AS
!                                    REAL                                
!                                                                        
!          TESTS 680 THRU 685  TESTS THE FUNCTION AND ARGUMENT VALUES    
!      FROM THE FUNCTION REFERENCE TO SUBPROGRAM FF082. THE FUNCTION     
!      SUBPROGRAM IS DEFINED AS REAL.                                    
!                                                                        
!      ****  TEST 680  ***                                               
!                                                                        
!          TEST  680  TESTS THE VALUE OF THE FUNCTION FF082. VALUE OF    
!      FUNCTION SHOULD BE 339.0.                                         
!                                                                        
      if  (iczero) 36800,6800,36800                                     
 6800 continue                                                          
      rvon01        =  2.0                                              
      radn3a (2,5,2) = 100.0                                            
      radn1a (5)   = 210.5                                              
      rvon0a       = 0.0                                                
      rvon0a  = ff082(rvon01,radn3a,radn1a,26.5)
      goto 46800                                                       
36800 ivdele = ivdele + 1                                               
      write (i02, 80003) ivtnum                                         
      if (iczero) 46800,6811,46800                                      
46800 if (rvon0a - 338.5) 26800,16800,46801                             
46801 if (rvon0a - 339.5) 16800,16800,26800                             
16800 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6811                                                        
26800 ivfail = ivfail + 1                                               
      rvcorr = 339.0                                                    
      rvcomp = rvon0a                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 6811 continue                                                          
      ivtnum = 681                                                      
!                                                                        
!      **** TEST 681  ****                                               
!                                                                        
!          TEST 681 TESTS THE VALUE OF THE VARIABLE-NAME ARGUMENT RVON01 
!      FOLLOWING THE FUNCTION REFERENCE.  VALUE OF RVON01 SHOULD BE 8.4. 
!                                                                        
      if (iczero) 36810,6810,36810                                      
 6810 continue                                                          
      goto 46810                                                       
36810 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46810,6821,46810                                      
46810 if (rvon01 - 8.395) 26810,16810,46811                             
46811 if (rvon01 - 8.405) 16810,16810,26810                             
16810 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6821                                                        
26810 ivfail = ivfail + 1                                               
      rvcorr = 8.4                                                      
      rvcomp = rvon01                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 6821 continue                                                          
      ivtnum = 682                                                      
!                                                                        
!      ****  TEST 682  ****                                              
!                                                                        
!          TEST 682 TESTS THE VALUE OF THE ARRAY-NAME ARGUMENT RADN3A    
!      FOLLOWING THE FUNCTION REFERENCE. RADN3A (2,5,2) WAS INITIALIZED  
!      IN MAIN PROGRAM AND INCREMENTED IN SUBPROGRAM. VALUE OF RADN3A    
!      (2,5,2) SHOULD BE 112.2.                                          
!                                                                        
      if (iczero) 36820,6820,36820                                      
 6820 continue                                                          
      goto 46820                                                       
36820 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46820,6831,46820                                      
46820 if (radn3a (2,5,2) - 111.7) 26820,16820,46821                     
46821 if (radn3a (2,5,2) - 112.7) 16820,16820,26820                     
16820 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6831                                                        
26820 ivfail = ivfail + 1                                               
      rvcorr = 112.2                                                    
      rvcomp = radn3a (2,5,2)                                           
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 6831 continue                                                          
      ivtnum = 683                                                      
!                                                                        
!      ****  TEST 683  ****                                              
!                                                                        
!          TEST 683 TESTS  THE VALUE OF THE ARRAY-NAME ARGUMENT RADN3A   
!      FOLLOWING THE FUNCTION REFERENCE.  RADN3A (1,2,1) WAS INITIALIZED 
!      IN THE SUBPROGRAM. THE VALUE OF RADN3A (1,2,1) SHOULD BE 612.2.   
!                                                                        
      if (iczero) 36830,6830,36830                                      
 6830 continue                                                          
      goto 46830                                                       
36830 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46830,6841,46830                                      
46830 if (radn3a (1,2,1) - 611.7) 26830,16830,46831                     
46831 if (radn3a (1,2,1) - 612.7) 16830,16830,26830                     
16830 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6841                                                        
26830 ivfail = ivfail + 1                                               
      rvcorr = 612.2                                                    
      rvcomp = radn3a (1,2,1)                                           
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 6841 continue                                                          
      ivtnum = 684                                                      
!                                                                        
!      ****  TEST 684  ****                                              
!                                                                        
!          TEST 684 TESTS THE VALUE OF THE ARRAY-ELEMENT-NAME ARGUMENT   
!      RADN1A FOLLOWING THE FUNCTION REFERENCE. RADN1A (5) WAS           
!      INITIALIZED IN THE MAIN PROGRAM AND INCREMENTED BY 18.8 IN THE    
!      FUNCTION SUBPROGRAM.  THE VALUE OF RADN1A SHOULD BE 229.3.        
!                                                                        
      if (iczero) 36840,6840,36840                                      
 6840 continue                                                          
      goto 46840                                                       
36840 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46840,6851,46840                                      
46840 if (radn1a (5) - 228.8) 26840,16840,46841                         
46841 if (radn1a (5) - 229.8) 16840,16840,26840                         
16840 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6851                                                        
26840 ivfail = ivfail + 1                                               
      rvcorr = 229.3                                                    
      rvcomp = radn1a (5)                                               
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 6851 continue                                                          
      ivtnum = 685                                                      
!                                                                        
!      **** TEST 685 ****                                                
!                                                                        
!          TEST 685  TESTS THE RESULTANT VALUE WHERE THE FUNCTION        
!      SUBPROGRAM IS DEFINED AS REAL AND THE VARIABLE TO WHICH THE       
!      FUNCTION VALUE IS ASSIGNED IN THE MAIN PROGRAM IS DEFINED AS      
!      INTEGER.                                                          
!                                                                        
      if (iczero) 36850,6850,36850                                      
 6850 continue                                                          
      rvon01   = 4.0                                                    
      radn3a (2,5,2) = 200.0                                            
      radn1a (5) = 2.85                                                 
      ivon0a = 0.0                                                      
      ivon0a  = ff082(rvon01,radn3a,radn1a,102.68)
      goto 46850                                                       
36850 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46850,6861,46850                                      
46850 if (ivon0a - 309)    26850,16850,26850                            
16850 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6861                                                        
26850 ivfail = ivfail + 1                                               
      ivcorr = 309                                                      
      ivcomp = ivon0a                                                   
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6861 continue                                                          
      ivtnum = 686                                                      
!                                                                        
!          TESTS 686 THRU 690 TESTS THE FUNCTION AND ARGUMENT VALUES     
!      FROM THE EXTERNAL FUNCTION REFERENCE TO SUBPROGRAM FF083. THE     
!      FUNCTION SUBPROGRAM IS AN IMPLICIT DEFINITION OF REAL.            
!                                                                        
!      *****  TEST 686  *****                                            
!                                                                        
!          TEST 686 TESTS THE VALUE OF FUNCTION FF082. THE VALUE OF THE  
!      FUNCTION SHOULD BE 921.8.                                         
!                                                                        
      if (iczero) 36860,6860,36860                                      
 6860 continue                                                          
!                                                                        
!                                                                        
      ivon01 =  826                                                     
      iadn2a (1,1) = 77                                                 
      iadn3a (2,3,4) =  10                                              
      rvon02 = 4.4                                                      
      rvon03 = 0.0                                                      
!                                                                        
      rvon03  = ff083(ivon01,iadn2a,iadn3a,rvon02*2.0)
      goto 46860                                                       
36860 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46860,6871,46860                                      
46860 if (rvon03 - 921.3) 26860,16860,46861                             
46861 if (rvon03 - 922.3) 16860,16860,26860                             
16860 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6871                                                        
26860 ivfail = ivfail + 1                                               
      rvcorr = 921.8                                                    
      rvcomp = rvon03                                                   
      write (i02,80005) ivtnum, rvcomp, rvcorr                          
 6871 continue                                                          
      ivtnum = 687                                                      
!                                                                        
!      ****  TEST  687  *****                                            
!                                                                        
!          TEST 687 TESTS THE VALUE OF THE VARIABLE-NAME ARGUMENT IVON01 
!      FOLLOWING THE FUNCTION REFERENCE. THE VALUE OF IVON01 SHOULD BE   
!      836.                                                              
!                                                                        
      if (iczero) 36870,6870,36870                                      
 6870 continue                                                          
      goto 46870                                                       
36870 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46870,6881,46870                                      
46870 if (ivon01 - 836) 26870,16870,26870                               
16870 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6881                                                        
26870 ivfail = ivfail + 1                                               
      ivcorr = 836                                                      
      ivcomp = ivon01                                                   
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6881 continue                                                          
      ivtnum = 688                                                      
!                                                                        
!      ****  TEST 688  *****                                             
!                                                                        
!          TEST 688 TESTS THE VALUE OF THE ARRAY-NAME ARGUMENT IADN2A    
!      FOLLOWING THE FUNCTION REFERENCE. THE ACTUAL ARGUMENT WAS         
!      INITIALIZED IN THE MAIN PROGRAM AND IS INCREMENTED IN THE         
!      SUBPROGRAM. THE VALUE OF IADN2A (1,1) SHOULD BE 97.               
!                                                                        
      if (iczero) 36880,6880,36880                                      
 6880 continue                                                          
      goto 46880                                                       
36880 ivdele = ivdele + 1                                               
      write  (i02,80003) ivtnum                                         
      if (iczero) 46880,6880,46880                                      
46880 if (iadn2a (1,1) - 97) 26880,16880,26880                          
16880 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6891                                                        
26880 ivfail = ivfail + 1                                               
      ivcorr = 97                                                       
      ivcomp = iadn2a (1,1)                                             
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6891 continue                                                          
      ivtnum = 689                                                      
!                                                                        
!      **** TEST 689 ****                                                
!                                                                        
!          TEST 689 TESTS THE VALUE OF THE ARRAY-ELEMENT-NAME ARGUMENT   
!      IADN3A FOLLOWING THE FUNCTION REFERENCE.  IADN3A (2,3,4)          
!      WAS INTIALIZED IN THE MAIN PROGRAM AND INCREMENTED BY 40 IN THE   
!      FUNCTION SUBPROGRAM. THE VALUE OF IADN3A SHOULD BE 50.            
!                                                                        
      if (iczero) 36890,6890,36890                                      
 6890 continue                                                          
      goto 46890                                                       
36890 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46890,6901,46890                                      
46890 if (iadn3a (2,3,4) - 50) 26890,16890,26890                        
16890 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6901                                                        
26890 ivfail = ivfail + 1                                               
      ivcorr = 50                                                       
      ivcomp = iadn3a (2,3,4)                                           
      write (i02,80004) ivtnum,ivcomp,ivcorr                            
 6901 continue                                                          
      ivtnum = 690                                                      
!                                                                        
!      **** TEST 690  ****                                               
!                                                                        
!          TEST  690 TESTS THE RESULTANT VALUE WHERE THE FUNCTION        
!      SUBPROGRAM IS IMPLICITY DEFINED AS REAL AND THE VARIABLE          
!      TO WHICH THE FUNCTION VALUE IS ASSIGNED IN THE MAIN PROGRAM       
!      IS DEFINED AS INTEGER. THE VALUE OF IVON03 SHOULD BE 329.         
!                                                                        
      if (iczero) 36900,6900,36900                                      
 6900 continue                                                          
      ivon01 =   226                                                    
      iadn2a (1,1) = 66                                                 
      iadn3a (2,3,4) = 20                                               
      rvon02 = 8.8                                                      
      ivon03 = 0                                                        
!                                                                        
      ivon03  = ff083(ivon01,iadn2a,iadn3a,rvon02*2.0)
!                                                                        
      goto 46900                                                       
36900 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 46900,6911,46900                                      
46900 if (ivon03 - 329) 26900,16900,26900                               
16900 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 6911                                                        
26900 ivfail = ivfail + 1                                               
      ivcorr = 329                                                      
      ivcomp = ivon03                                                   
      write (i02,80004) ivtnum, ivcomp, ivcorr                          
 6911 continue                                                          
!                                                                        
!      WRITE PAGE FOOTINGS AND RUN SUMMARIES                             
99999 continue                                                          
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90002)                                                 
      write (i02,90002)                                                 
      write (i02,90007)                                                 
      write (i02,90002)                                                 
      write (i02,90008)  ivfail                                         
      write (i02,90009) ivpass                                          
      write (i02,90010) ivdele                                          
!                                                                        
!                                                                        
!      TERMINATE ROUTINE EXECUTION                                       
      stop                                                              
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
90000 format ("1")                                                      
90002 format (" ")                                                      
90001 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90003 format (" ",21x,"VERSION 2.1" )                                   
90004 format (" ",10x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )        
90005 format (" ",5x,"TEST",5x,"PASS/FAIL", 5x,"COMPUTED",8x,"CORRECT") 
90006 format (" ",5x,"----------------------------------------------" ) 
90011 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARIES                               
90008 format (" ",15x,i5," ERRORS ENCOUNTERED" )                        
90009 format (" ",15x,i5," TESTS PASSED" )                              
90010 format (" ",15x,i5," TESTS DELETED" )                             
!                                                                        
!      FORMAT STATEMENTS FOR TEST RESULTS                                
80001 format (" ",4x,i5,7x,"PASS")                                      
80002 format (" ",4x,i5,7x,"FAIL")                                      
80003 format (" ",4x,i5,7x,"DELETED")                                   
80004 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80005 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
!                                                                        
90007 format (" ",20x,"END OF PROGRAM FM080" )                          
      end program fm080
      integer function ff081(idon01,iddn10,iddn20,idon02)
!                                                                        
!      COMMENT SECTION                                                   
!                                                                        
!      FF081                                                             
!                                                                        
!          THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM080. 
!      THE FUNCTION DUMMY ARGUMENTS IDON01, IDDN10 AND IDDN20 ARE        
!      INCREMENTED BY 2, 40 AND 70 RESPECTIVELY BEFORE CONTROL IS        
!      RETURNED TO THE CALLING PROGRAM.  VALUE OF THE FUNCTION WILL BE   
!      THE SUM OF THE ACTUAL ARGUMENTS AS PASSED FROM CALLING PROGRAM.   
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.5.1, FUNCTION SUBPROGRAM AND FUNCTION STATEMENT     
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!          FUNCTION SUBPROGRAM                                           
!                                                                        
      integer :: idon01
      integer :: idon02
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      integer :: ivon04
      integer, dimension(1:5) :: iddn10
      integer, dimension(1:4,1:4) :: iddn20
      ivon01 = idon01                                                   
      ivon02 = iddn10(2)                                                
      ivon03 = iddn20(1,3)                                              
      ivon04 = idon02                                                   
!                                                                        
      ff081  = ivon01 + ivon02 + ivon03 + ivon04                        
      idon01 = ivon01 + 2                                               
      iddn10 (2) = ivon02   + 40                                        
      iddn20 (1,3) = ivon03 + 70                                        
      iddn10 (4) = ivon02 + 40                                          
      return                                                            
      end function ff081
      real function ff082(rdon01,rddn3a,rddn1a,rdon02)
      real :: rdon01
      real :: rdon02
      real :: rvon01
      real :: rvon02
      real :: rvon03
      real :: rvon04
      real, dimension(1:3,1:6,1:3) :: rddn3a
      real, dimension(1:10) :: rddn1a
!                                                                        
!      COMMENT SECTION                                                   
!                                                                        
!      FF082                                                             
!                                                                        
!          THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM080. 
!      THE FUNCTION DUMMY ARGUMENTS RDON01, RDDN3A, AND RDDN1A ARE       
!      INCREMENTED BY 6.4, 12.2 AND 18.8 RESPECTIVELY BEFORE CONTROL IS  
!      RETURNED TO THE MAIN PROGRAM.  VALUE OF THE FUNCTION WILL BE      
!      THE SUM OF THE ACTUAL ARGUMENTS AS PASSED TO THE SUBPROGRAM.      
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 15.5.1, FUNCTION SUBPROGRAM AND FUNCTION STATEMENT     
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!           FUNCTION SUBPROGRAM                                          
!                                                                        
      rvon01 = rdon01                                                   
      rvon02 = rddn3a (2,5,2)                                           
      rvon03 = rddn1a (5)                                               
      rvon04 = rdon02                                                   
!                                                                        
      ff082 = rvon01 + rvon02 + rvon03  + rvon04                        
!                                                                        
      rdon01 =     rvon01 + 6.4                                         
      rddn3a (2,5,2) = rvon02 + 12.2                                    
      rddn1a (5)     = rvon03 + 18.8                                    
      rddn3a (1,2,1) =  600.0 + 12.2                                    
      return                                                            
      end function ff082
      function ff083(idon01,iddn2a,iddn3a,rdon02)
      integer :: idon01
      real :: rdon02
      integer :: ivon01
      integer :: ivon02
      integer :: ivon03
      real :: rvon04
      real :: rvon05
      integer, dimension(1:2,1:2) :: iddn2a
      integer, dimension(1:3,1:4,1:5) :: iddn3a
!                                                                        
!      COMMENT SECTION                                                   
!                                                                        
!      FF083                                                             
!                                                                        
!          THIS FUNCTION SUBPROGRAM IS CALLED BY THE MAIN PROGRAM FM080. 
!      THE TYPE DECLARATION IS IMPLICIT REAL.                            
!      THE FUNCTION DUMMY ARGUMENTS ARE BOTH INTEGER AND REAL. DUMMY     
!      ARGUMENTS IDON01, IDDN2A AND IDDN3A ARE INCREMENTED BY 10, 20 AND 
!      40 RESPECTIVELY BEFORE CONTROL IS RETURNED TO THE MAIN PROGRAM.   
!      THE VALUE OF THE FUNCTION RETURNED TO THE REFERENCING PROGRAM     
!      WILL BE THE SUM OF THE ACTUAL ARGUMENTS AS PASSED TO THE          
!      SUBPROGRAM FF083.                                                 
!          DUMMY ARGUMENT IDDN2A CORRESPONDS TO AN ARRAY-NAME IN THE     
!      ACTUAL ARGUMENT OF THE MAIN PROGRAM.  DUMMY ARGUMENT IDDN3A       
!      CORRESPONDS TO AN ARRAY-ELEMENT-NAME IN THE ACTUAL ARGUMENT OF THE
!      MAIN PROGRAM.  DUMMY ARGUMENT IDON02  CORRESPONDS TO AN EXPRESSION
!      CONTAINING VARIABLES,ARITHMETIC OPERATORS AND CONSTANTS IN THE    
!      ACTUAL ARGUMENT OF THE MAIN PROGRAM.                              
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 4.1.2, TYPE RULES FOR DATA AND PROCEDURE IDENTIFIERS   
!         SECTION 15.5.1, FUNCTION SUBPROGRAM                            
!                                                                        
!      TEST SECTION                                                      
!                                                                        
!           FUNCTION SUBPROGRAM                                          
!                                                                        
      ivon01 = idon01                                                   
      ivon02 = iddn2a (1,1)                                             
      ivon03 = iddn3a (2,3,4)                                           
      rvon04 = rdon02                                                   
!                                                                        
      rvon05 = ivon01 + ivon02 + ivon03                                 
      ff083 = rvon05 + rvon04                                           
!                                                                        
      idon01 = ivon01 + 10                                              
      iddn2a (1,1) = ivon02 + 20                                        
      iddn3a (2,3,4) = ivon03 + 40                                      
!                                                                        
      return                                                            
      end function ff083
