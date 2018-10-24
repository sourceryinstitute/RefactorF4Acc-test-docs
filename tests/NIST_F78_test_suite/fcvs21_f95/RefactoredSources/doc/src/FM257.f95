      program fm257
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      integer :: ivcorr
      integer :: ivon01
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE IS A TEST OF THE PAUSE AND STOP STATEMENTS.  THESE
!      STATEMENTS CAN NOW BE FOLLOWED BY A STRING OF NOT MORE THAN FIVE  
!      DIGITS, OR A CHARACTER CONSTANT.                                  
!                                                                        
!      REFERENCES                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1978                                                 
!         SECTION 11.12,      STOP STATEMENT                             
!         SECTION 11.13,      PAUSE STATEMENT                            
!                                                                        
!         FM015 - TESTS THE STOP AND PAUSE STATEMENTS USING AN OCTAL     
!                 DIGIT STRING OF LENGTH FROM ONE TO FIVE.               
!                                                                        
!                                                                        
!                                                                        
!      ******************************************************************
!          A COMPILER VALIDATION SYSTEM FOR THE FORTRAN LANGUAGE         
!      BASED ON SPECIFICATIONS AS DEFINED IN AMERICAN STANDARD FORTRAN   
!      X3.9-1978, HAS BEEN DEVELOPED BY THE DEPARTMENT OF THE NAVY.  THE 
!      FORTRAN COMPILER VALIDATION SYSTEM (FCVS) CONSISTS OF AUDIT       
!      ROUTINES, THEIR RELATED DATA, AND AN EXECUTIVE SYSTEM.  EACH AUDIT
!      ROUTINE IS A FORTRAN PROGRAM OR SUBPROGRAM WHICH INCLUDES TESTS   
!      OF SPECIFIC LANGUAGE ELEMENTS AND SUPPORTING PROCEDURES INDICATING
!      THE RESULT OF EXECUTING THESE TESTS.                              
!                                                                        
!      THIS PARTICULAR PROGRAM OR SUBPROGRAM CONTAINS ONLY FEATURES      
!      FOUND IN THE SUBSET LEVEL OF THE STANDARD.                        
!                                                                        
!            SUGGESTIONS AND COMMENTS SHOULD BE FORWARDED TO             
!               NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY           
!                    SOFTWARE STANDARDS VALIDATION GROUP                 
!                           BUILDING 225  RM A266                        
!                          GAITHERSBURG, MD  20899                       
!      ******************************************************************
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!                                                                        
!      INITIALIZATION SECTION.                                           
!                                                                        
!      INITIALIZE CONSTANTS                                              
!      ********************                                              
!      I01 CONTAINS THE LOGICAL UNIT NUMBER FOR THE CARD READER          
      i01 = 5                                                           
!      I02 CONTAINS THE LOGICAL UNIT NUMBER FOR THE PRINTER              
      i02 = 6                                                           
!      SYSTEM ENVIRONMENT SECTION                                        
!                                                                        
! X010     THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-010 CONTROL CARD.
!      THE CX010 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I01 = 5      
!      (UNIT NUMBER FOR CARD READER).                                    
! X011      THIS CARD IS REPLACED BY CONTENTS OF FEXEC X-011 CONTROL CARD
!      THE CX011 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX010 ABOVE.         
!                                                                        
! X020     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-020 CONTROL CARD.
!      THE CX020 CARD IS FOR OVERRIDING THE PROGRAM DEFAULT I02 = 6      
!      (UNIT NUMBER FOR PRINTER).                                        
! X021     THIS CARD IS PEPLACED BY CONTENTS OF FEXEC X-021 CONTROL CARD.
!      THE CX021 CARD IS FOR SYSTEMS WHICH REQUIRE ADDITIONAL            
!      FORTRAN STATEMENTS FOR FILES ASSOCIATED WITH CX020 ABOVE.         
!                                                                        
      ivpass = 0                                                        
      ivfail = 0                                                        
      ivdele = 0                                                        
      iczero = 0                                                        
!                                                                        
!      WRITE OUT PAGE HEADERS                                            
!                                                                        
      write (i02,90002)                                                 
      write (i02,90006)                                                 
      write (i02,90008)                                                 
      write (i02,90004)                                                 
      write (i02,90010)                                                 
      write (i02,90004)                                                 
      write (i02,90016)                                                 
      write (i02,90001)                                                 
      write (i02,90004)                                                 
      write (i02,90012)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
!                                                                        
!                                                                        
!                                                                        
!         THE FOLLOWING SERIES OF TESTS CHECK THE VARIOUS FORMS OF THE   
!      PAUSE STATEMENT.  IN EACH CASE THE WORD PAUSE (FOLLOWED BY A      
!      STRING OF CHARACTERS AS NOTED IN EACH TEST DESCRIPTION), SHOULD BE
!      DISPLAYED ON THE OPERATORS CONSOLE.  FOR EACH TEST THE OPERATOR   
!      NEED ONLY DO WHATEVER IS NESSARY TO TELL THE SYSTEM TO CONTINUE   
!      THE EXECUTION OF THE ROUTINE.  THE STRING FORMS ARE AS DESCRIBED  
!      IN SECTION 11.13.                                                 
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 257  -  TEST 001  ****                         
!                                                                        
!         TEST 001 CHECKS THE PAUSE STATEMENT THAT IS NOT FOLLOWED BY    
!      A STRING OF ANYTHING EXCEPT BLANKS.  ONLY THE WORD PAUSE SHOULD   
!      BE DISPLAYED.                                                     
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      pause                                                             
!                                                                        
!      ***** THESE CARDS INITIALIZE IVCOMP AND IVCORR FOR THE NEXT       
!            FIVE TESTS EVEN THOUGH THEY ONLY APPEAR IN THE FAIL CODE    
!            OF THE BOILERPLATE.*****                                    
      ivcomp = 1                                                        
      ivcorr = 1                                                        
!                                                                        
!                                                                        
40010 if ( iczero )  20010, 10010, 20010                                
30010 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10010, 0021, 20010                                    
10010 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0021                                                        
20010 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0021 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 257  -  TEST 002  ****                         
!                                                                        
!         TEST 002 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY A SINGLE    
!      CHARACTER ZERO (0).                                               
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      pause 0                                                           
40020 if ( iczero )  20020, 10020, 20020                                
30020 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10020, 0031, 20020                                    
10020 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0031                                                        
20020 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0031 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 257  -  TEST 003  ****                         
!                                                                        
!         TEST 003 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY A STRING OF 
!      FIVE ZEROS (00000).                                               
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      pause 00000                                                       
40030 if ( iczero )  20030, 10030, 20030                                
30030 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10030, 0041, 20030                                    
10030 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0041                                                        
20030 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0041 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 257  -  TEST 004  ****                         
!                                                                        
!         TEST 004 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY THE STRING  
!      OF FIVE CHARACTERS  19283.                                        
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      pause  19283                                                      
40040 if ( iczero )  20040, 10040, 20040                                
30040 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10040, 0051, 20040                                    
10040 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0051                                                        
20040 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0051 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 257  -  TEST 005  ****                         
!                                                                        
!         TEST 005 SHOULD DISPLAY THE WORD PAUSE FOLLOWED BY THE STRING  
!      OF FOUR NINES  (9999).                                            
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      pause 9999                                                        
40050 if ( iczero )  20050, 10050, 20050                                
30050 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10050, 0061, 20050                                    
10050 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0061                                                        
20050 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0061 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 257  -  TEST 006  ****                         
!                                                                        
!         TEST 006 IS FOR THE STOP STATEMENT - SECTION 11.12.            
!      SINCE THE STOP STATEMENT CAN ONLY BE EXECUTED ONCE IN A PROGRAM   
!      UNIT, VARIOUS FORMATS OF THE STOP STATEMENT WILL BE CHECKED FOR   
!      SYNTAX ONLY BY THE USE OF A COMPUTED GO TO STATEMENT.             
!                                                                        
!         ONCE THE STOP STATEMENT HAS BEEN EXECUTED, THEN THE ROUTINE    
!      FM257 SHOULD NO LONGER EXECUTE.  ANY CONTINUATION IS CONSIDERED AS
!      A FAILURE OF THIS TEST.                                           
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivon01 = 6                                                        
      go to ( 0062, 0063, 0064, 0065, 0066, 0067, 40060 ), ivon01       
!                                                                        
 0062 stop 0                                                            
 0063 stop 00000                                                        
 0064 stop 12345                                                        
 0065 stop 9999                                                         
 0066 stop 'IMA 1'                                                      
 0067 stop 'P ASS'                                                      
!                                                                        
!         **** THE TEST FAILS IF IT GOES BEYOND THE STOP STATEMENTS  ****
!                                                                        
40060 if ( iczero )  10060, 20060, 10060                                
!      ***** NOTE THAT THE NORMAL PASS-10060 AND FAIL-20060 LABELS       
!            ARE REVERSED BECAUSE IF THE LOGIC EXECUTES THIS STATEMENT   
!            THEN THE STOP STATEMENT FAILS TO EXECUTE CORRECTLY. *****   
30060 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10060, 0071, 20060                                    
10060 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0071                                                        
20060 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0071 continue                                                          
!                                                                        
!                                                                        
!      WRITE OUT TEST SUMMARY                                            
!                                                                        
      write (i02,90004)                                                 
      write (i02,90014)                                                 
      write (i02,90004)                                                 
      write (i02,90000)                                                 
      write (i02,90004)                                                 
      write (i02,90020) ivfail                                          
      write (i02,90022) ivpass                                          
      write (i02,90024) ivdele                                          
      stop                                                              
90001 format (" ",24x,"FM257")                                          
90000 format (" ",20x,"END OF PROGRAM FM257" )                          
!                                                                        
!      FORMATS FOR TEST DETAIL LINES                                     
!                                                                        
80000 format (" ",4x,i5,6x,"DELETED")                                   
80002 format (" ",4x,i5,7x,"PASS")                                      
80010 format (" ",4x,i5,7x,"FAIL",10x,i6,9x,i6)                         
80012 format (" ",4x,i5,7x,"FAIL",4x,e12.5,3x,e12.5)                    
80018 format (" ",4x,i5,7x,"FAIL",2x,a14,1x,a14)                        
!                                                                        
!      FORMAT STATEMENTS FOR PAGE HEADERS                                
!                                                                        
90002 format ("1")                                                      
90004 format (" ")                                                      
90006 format (" ",10x,"FORTRAN COMPILER VALIDATION SYSTEM" )            
90008 format (" ",21x,"VERSION 2.1" )                                   
90010 format (" ",8x,"FOR OFFICIAL USE ONLY - COPYRIGHT 1978" )         
90012 format (" ",5x,"TEST",5x,"PASS/FAIL",5x,"COMPUTED",8x,"CORRECT")  
90014 format (" ",5x,"----------------------------------------------" ) 
90016 format (" ",18x,"SUBSET LEVEL TEST" )                             
!                                                                        
!      FORMAT STATEMENTS FOR RUN SUMMARY                                 
!                                                                        
90020 format (" ",19x,i5," TESTS FAILED" )                              
90022 format (" ",19x,i5," TESTS PASSED" )                              
90024 format (" ",19x,i5," TESTS DELETED" )                             
      end program fm257
