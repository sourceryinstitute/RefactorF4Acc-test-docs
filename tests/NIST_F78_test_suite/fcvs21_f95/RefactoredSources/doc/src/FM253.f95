      program fm253
!                                                                        
!                                                                        
!                                                                        
!         THIS ROUTINE IS A TEST OF THE IF-BLOCK.  TESTS WITHIN THIS     
!      ROUTINE ARE FOR THE SYNTAX OF THE BASIC IF ( )  THEN  THROUGH     
!      END IF BLOCK STRUCTURE.                                           
!                                                                        
!         THERE IS ALSO A SERIES OF TESTS TO CHECK THE HIERARCHY AND     
!      ORDER OF EVALUATION IN EXPRESSIONS THAT CONTAIN A COMBINATION OF  
!      ARITHMETIC, RELATIONAL, AND LOGICAL OPERATORS.                    
!                                                                        
!      REFERENCES                                                        
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!              X3.9-1978                                                 
!         SECTION 11.6,       BLOCK IF STATEMENT                         
!         SECTION 11.6.1,     IF-LEVEL                                   
!         SECTION 11.6.2,     IF-BLOCK                                   
!         SECTION 11.6.3,     EXECUTION OF A BLOCK IF STATEMENT          
!                                                                        
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
      logical  :: lfis01
      logical  :: l
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivcomp
      real :: true
      integer :: ivcorr
      integer :: ivon01
      logical  :: lvon01
      integer :: ivon03
      integer :: ivon04
      integer :: ivon05
      integer :: ivon02
      integer :: j
      real :: false
      logical  :: lvon02
      logical  :: lvon03
      real :: go
      real :: to
      integer :: ivon06
      integer :: ivon07
      integer :: ivon08
      integer :: ivon09
      integer :: ivon10
      integer :: ivon11
      integer :: ivon12
      integer :: ivon13
      integer :: ivon14
      integer :: ivon15
      integer :: ivon16
      integer :: ivon17
      integer :: ivon18
      integer :: ivon19
      integer :: ivon20
      integer :: ivon21
      integer :: ivon22
      integer :: ivon23
      integer :: ivon24
      integer :: ivon25
      integer :: ivon26
      integer :: ivon27
      integer :: ivon28
      integer :: ivon29
      integer :: ivon30
      integer :: ivon31
      integer :: ivon32
      integer :: ivon33
      integer :: ivon34
      integer :: ivon35
      integer :: ivon36
      integer :: ivon37
      integer :: ivon38
      integer :: ivon39
      integer :: ivon40
      logical, dimension(1:2) :: ladn11
      logical :: lvtn01
      logical :: lvtn02
      logical, dimension(1:2) :: latn11
      data ladn11 / .true.,.false. / 
!                                                                        
!                                                                        
!      **** LOGICAL STATEMENT FUNCTION REFERENCED IN TEST 20 ****        
!                                                                        
      lfis01 ( l ) = .not. l                                            
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
!      ****  FCVS PROGRAM 253  -  TEST 001  ****                         
!                                                                        
!         TEST 001 USES A VERY SIMPLE BLOCK IF STATEMENT.  THE EXPRESSION
!      WITHIN THE PARENTHESES IS THE LOGICAL CONSTANT  .TRUE.  AND THE   
!      EXECUTABLE STATEMENT WITHIN THE IF-BLOCK OF LEVEL ONE IS AN       
!      INTEGER ARITHMETIC ASSIGNMENT STATEMENT. SINCE THE LOGICAL        
!      EXPRESSION IS TRUE, THEN THE INTEGER ASSIGNMENT STATEMENT ( TRUE  
!      PATH ) SHOULD BE EXECUTED.                                        
!                                                                        
!         THIS IS A SYNTAX CHECK FOR THE BLOCK IF STATEMENT.  SHOULD A   
!      COMPILER NOT BE ABLE TO ACCEPT THE SYNTAX OF THIS BASIC TEST,     
!      THEN ROUTINES FM253, THRU FMXXX NEED NOT BE RUN.                  
!                                                                        
!                                                                        
      ivtnum =   1                                                      
      if (iczero) 30010, 0010, 30010                                    
 0010 continue                                                          
      ivcomp = 0                                                        
      if ( .true. ) then                                                
           ivcomp = 1                                                   
      end if                                                            
      ivcorr = 1                                                        
40010 if ( ivcomp - 1 )  20010, 10010, 20010                            
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
!      ****  FCVS PROGRAM 253  -  TEST 002  ****                         
!                                                                        
!         TEST 002 USES A LOGICAL VARIABLE SET .FALSE. AS THE LOGICAL    
!      EXPRESSION IN THE BLOCK IF STATEMENT.   BECAUSE THE EXPRESSION    
!      IS FALSE, THE IF-BLOCK (WHICH IS AN INTEGER ARITHMETIC ASSIGNMENT 
!      STATEMENT AND A LOGICAL ASSIGNMENT STATEMENT) SHOULD NOT BE       
!      EXECUTED.                                                         
!                                                                        
!                                                                        
      ivtnum =   2                                                      
      if (iczero) 30020, 0020, 30020                                    
 0020 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 1                                                        
      lvon01 = .false.                                                  
      lvtn01 = .false.                                                  
      if ( lvon01 )  then                                               
           ivon01 = 0                                                   
           lvtn01 = .true.                                              
      end if                                                            
      ivcorr = 1                                                        
40020 if ( ivon01  ==  1 )  ivcomp = 1                                  
40021 if ( ivcomp - 1 )  20020, 10020, 20020                            
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
!      ****  FCVS PROGRAM 253  -  TEST 003  ****                         
!                                                                        
!         TEST 003 IS A BLOCK IF STATEMENT WITH AN EMPTY IF-BLOCK.  THE  
!      LOGICAL EXPRESSION IS A LOGICAL ARRAY ELEMENT REFERENCE SET TO    
!      .TRUE.  SECTION 11.6.2 STATES THAT,  AN IF-BLOCK MAY BE EMPTY.    
!      BECAUSE THE LOGICAL EXPRESSION IS TRUE, THE IF-BLOCK SHOULD BE    
!      EXECUTED.  IF THE VALUE OF THE EXPRESSION IS TRUE AND THE IF-BLOCK
!      IS EMPTY, CONTROL IS TRANSFERRED TO THE NEXT END IF STATEMENT     
!      THAT HAS THE SAME IF-LEVEL AS THE BLOCK IF STATEMENT ACCORDING    
!      TO SECTION 11.6.3. IN THIS TEST THE EMPTY IF-BLOCK IS OF LEVEL ONE
!                                                                        
!                                                                        
      ivtnum =   3                                                      
      if (iczero) 30030, 0030, 30030                                    
 0030 continue                                                          
      ivcomp = 0                                                        
      latn11(1) = .true.                                                
      if ( latn11(1) )  then                                            
      end if                                                            
      ivcomp = 1                                                        
      ivcorr = 1                                                        
40030 if ( ivcomp - 1 )  20030, 10030, 20030                            
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
!      ****  FCVS PROGRAM 253  -  TEST 004  ****                         
!                                                                        
!         TEST 004 IS LIKE THE PREVIOUS TEST USING A LOGICAL ARRAY       
!      ELEMENT REFERENCE AS THE LOGICAL EXPRESSION OF THE BLOCK IF       
!      STATEMENT THAT HAS AN EMPTY IF-BLOCK STRUCTURE OF LEVEL ONE.      
!      IN THIS TEST THE LOGICAL EXPRESSION IS FALSE SO CONTROL SHOULD    
!      BE TRANSFERRED TO THE END IF STATEMENT THAT HAS THE SAME IF-LEVEL 
!      AS THE BLOCK IF STATEMENT ACCORDING TO SECTION 11.6.3.            
!                                                                        
!      THE LOGICAL ARRAY ELEMENT REFERENCE  LADN11(2) IS SET TO .FALSE.  
!      IN THE DATA STATEMENT AS FOLLOWS                                  
!                                                                        
!                   DATA LADN11/.TRUE., .FALSE./                         
!                                                                        
!                                                                        
      ivtnum =   4                                                      
      if (iczero) 30040, 0040, 30040                                    
 0040 continue                                                          
      ivcomp = 0                                                        
      if ( ladn11(2) )  then                                            
      end if                                                            
      ivcomp = 1                                                        
      ivcorr = 1                                                        
40040 if ( ivcomp - 1 )  20040, 10040, 20040                            
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
!                                                                        
!         THE NEXT FOUR TESTS ARE FOR A BLOCK IF STRUCTURE OF LEVEL      
!      TWO IN THE INNERMOST IF-BLOCK.  THIS STRUCTURE IS SHOWN BELOW -   
!                                                                        
!                   IF ( E1 )  THEN                                      
!                        IF-BLOCK 1                                      
!                        IF ( E2 )  THEN                                 
!                             IF-BLOCK 2                                 
!                        END IF                                          
!                   END IF                                               
!      TESTS WILL USE THE FOUR COMBINATIONS OF TRUE AND FALSE FOR E1 AND 
!      E2 RESPECTIVELY TO TEST THE TRANSFER OF CONTROL AS DESCRIBED      
!      IN SECTION 11.6.3.                                                
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 005  ****                         
!                                                                        
!         TEST 005 USES A FALSE VALUE FOR E1 AND A FALSE VALUE FOR E2.   
!      CONTROL SHOULD BE TRANSFERRED TO THE END IF STATEMENT OF LEVEL 1  
!      WHICH MEANS IF-BLOCK 1 AND IF-BLOCK 2 SHOULD NOT BE EXECUTED.     
!                                                                        
!                                                                        
      ivtnum =   5                                                      
      if (iczero) 30050, 0050, 30050                                    
 0050 continue                                                          
      ivcomp = 1                                                        
      ladn11(2) = .false.                                               
      if ( 76  <  3 )  then                                            
           ivcomp = ivcomp * 2                                          
           if ( ( ladn11(2) ) )  then                                   
                ivcomp = ivcomp * 3                                     
           end if                                                       
      end if                                                            
      ivcorr = 1                                                        
40051 if ( ivcomp - 1 )  20050, 10050, 20050                            
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
!      ****  FCVS PROGRAM 253  -  TEST 006  ****                         
!                                                                        
!         TEST 006 USES A FALSE VALUE FOR E1 AND A TRUE VALUE FOR E2.    
!      CONTROL SHOULD BE TRANSFERRED TO THE END IF STATEMENT OF LEVEL 1  
!      WHICH MEANS IF-BLOCK 1 AND IF-BLOCK 2 SHOULD NOT BE EXECUTED.     
!                                                                        
!                                                                        
      ivtnum =   6                                                      
      if (iczero) 30060, 0060, 30060                                    
 0060 continue                                                          
      ivcomp = 1                                                        
      ivon03 = 32767                                                    
      lvtn01 = .true.                                                   
      lvon01 = .true.                                                   
      if ( .not. lvtn01 )  then                                         
           ivcomp = ivcomp * 2                                          
           if ( lvon01 .and. ivon03  >=  587 )  then                    
                ivcomp = ivcomp * 3                                     
           end if                                                       
      end if                                                            
      ivcorr = 1                                                        
40061 if ( ivcomp - 1 )  20060, 10060, 20060                            
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
!      ****  FCVS PROGRAM 253  -  TEST 007  ****                         
!                                                                        
!         TEST 007 USES A TRUE VALUE FOR E1 AND A FALSE VALUE FOR E2.    
!      IF-BLOCK 1 SHOULD BE EXECUTED, BUT IF-BLOCK 2 SHOULD NOT BE       
!      EXECUTED.                                                         
!                                                                        
!         IF-BLOCK 1 ALSO CONTAINS AN UNCONDITIONAL GO TO AND A CONTINUE 
!      STATEMENT WHICH SHOULD BOTH BE EXECUTED.                          
!                                                                        
!                                                                        
      ivtnum =   7                                                      
      if (iczero) 30070, 0070, 30070                                    
 0070 continue                                                          
      ivcomp = 1                                                        
      ivon03 = 587                                                      
      ivon04 = 3                                                        
      latn11(1) = .true.                                                
      latn11(2) = .false.                                               
      if ( (latn11(1)) .or. ((7 * ivon04)  ==  21) )  then              
           ivcomp = ivcomp * 2                                          
           goto 0072                                                   
 0072      continue                                                     
           if ( 7  >  ivon03 .or. latn11(2) )  then                    
                ivcomp = ivcomp * 3                                     
           end if                                                       
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 2 = 1 * 2        ****    
!                                                                        
      ivcorr = 2                                                        
40070 if ( ivcomp - 2 )  20070, 10070, 20070                            
30070 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10070, 0081, 20070                                    
10070 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0081                                                        
20070 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0081 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 008  ****                         
!                                                                        
!         TEST 008 USES A TRUE VALUE FOR E1 AND A TRUE VALUE FOR E2.     
!      BOTH IF-BLOCK 1 AND IF-BLOCK 2 SHOULD BE EXECUTED.                
!                                                                        
!         IF-BLOCK 1 CONTAINS AN ASSIGN STATEMENT PLUS AN ASSIGNED GO TO 
!      STATEMENT WHICH SHOULD BOTH BE EXECUTED.                          
!                                                                        
!                                                                        
      ivtnum =   8                                                      
      if (iczero) 30080, 0080, 30080                                    
 0080 continue                                                          
      ivcomp = 1                                                        
      ivon04 = 4                                                        
      ivon05 = 2                                                        
      lvon01 = .false.                                                  
      lvtn01 = lvon01                                                   
!                                                                        
      if ( ivon04 - 1  <=  6 .and. 7  >=  5 / ivon05 )  then            
           ivcomp = ivcomp * 2                                          
           assign 0083 to i                                             
           goto 0084                                                   
 0082      ivcomp = ivcomp * 3                                          
           goto 0085                                                   
 0083      ivcomp = ivcomp * 5                                          
           goto 0085                                                   
 0084      go to i, ( 0082, 0083 )                                      
 0085      if ( .not. ( lvtn01 ) )  then                                
                ivcomp = ivcomp * 7                                     
           end if                                                       
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 70 = 1 * 2 * 5 * 7   ****
!                                                                        
      ivcorr = 70                                                       
40080 if ( ivcomp - 70 )  20080, 10080, 20080                           
30080 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10080, 0091, 20080                                    
10080 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0091                                                        
20080 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0091 continue                                                          
!                                                                        
!         THE NEXT FIVE TESTS ARE FOR A BLOCK IF STRUCTURE OF LEVEL      
!      THREE IN THE INNERMOST IF-BLOCK.  THIS STRUCTURE IS SHOWN BELOW - 
!                                                                        
!              IF ( E1 )  THEN                                           
!                   IF-BLOCK 1                                           
!                   IF ( E2 )  THEN                                      
!                        IF-BLOCK 2                                      
!                        IF ( E3 )  THEN                                 
!                             IF-BLOCK 3                                 
!                        END IF                                          
!                   END IF                                               
!              END IF                                                    
!                                                                        
!      THE FIVE TESTS WILL USE THE FOLLOWING COMBINATIONS OF TRUE AND    
!      FALSE FOR E1, E2, AND E3 AS SHOWN BELOW -                         
!         TEST NUMBER    9   10   11   12   13                           
!              E1        T    T    T    T    F                           
!              E2        T    T    F    F    T                           
!              E3        T    F    T    F    T                           
!                                                                        
!      CONTROL SHOULD BE AS DESCRIBED IN SECTION 11.6.3.                 
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 009  ****                         
!                                                                        
!         TEST 009 HAS E1, E2, AND E3 AS TRUE.  IF-BLOCK 1, 2, AND 3     
!      SHOULD BE EXECUTED.  IF-BLOCK 1 HAS A COMPUTED GO TO STATEMENT    
!      WHICH SHOULD BE EXECUTED.                                         
!                                                                        
!                                                                        
      ivtnum =   9                                                      
      if (iczero) 30090, 0090, 30090                                    
 0090 continue                                                          
      ivcomp = 1                                                        
      ivon01 = 4                                                        
      ivon02 = 3                                                        
!                                                                        
      if ( .not. ivon01  ==  3 .or. .not. ivon02  ==  4 )  then         
           ivcomp = ivcomp * 2                                          
           j = 2                                                        
           goto 0095                                                   
 0092      ivcomp = ivcomp * 3                                          
           goto 0096                                                   
 0093      ivcomp = ivcomp * 5                                          
           goto 0096                                                   
 0094      ivcomp = ivcomp * 7                                          
           goto 0096                                                   
 0095      go to ( 0092, 0093, 0094 ), j                                
 0096      if ( ivon01  ==  4 .and. ivon02  /=  2 )  then               
                ivcomp = ivcomp * 11                                    
                if ( ivon01  ==  4 .and. .not. ivon02  ==  2 )  then    
                     ivcomp = ivcomp * 13                               
                end if                                                  
           end if                                                       
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 1430 = 1*2*5*11*13   ****
!                                                                        
      ivcorr = 1430                                                     
40090 if ( ivcomp - 1430 )  20090, 10090, 20090                         
30090 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10090, 0101, 20090                                    
10090 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0101                                                        
20090 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0101 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 010  ****                         
!                                                                        
!                                                                        
!         TEST 010 HAS E1 AND E2 AS TRUE.  E3 IS FALSE.  IF-BLOCK 1 HAS  
!      A LOGICAL IF STATEMENT WHICH SHOULD BE EXECUTED BY TAKING THE     
!      TRUE PATH.  IF-BLOCK 2 HAS AN ARITHMETIC IF STATEMENT WITH THE    
!      VALUE INSIDE THE PARENTHESIS EQUAL TO ZERO.  IF-BLOCK 3 SHOULD NOT
!      BE EXECUTED.                                                      
!                                                                        
!                                                                        
      ivtnum =  10                                                      
      if (iczero) 30100, 0100, 30100                                    
 0100 continue                                                          
      ivcomp = 1                                                        
      ivon01 = +3                                                       
      lvon01 = .false.                                                  
!                                                                        
      if ( .not. lvon01 .and. .true. .or. .true. .and. .not. lvon01 )   then                                                              
           ivcomp = ivcomp * 2                                          
           if ( 3  <=  ivon01 )  ivcomp = ivcomp * 3                    
           if ( .not.(lvon01.and..true.).or.(.true..and..not.lvon01) )       then                                                         
                if ( 3 - ivon01 )  0103, 0102, 0103                     
 0102           ivcomp = ivcomp * 5                                     
                goto 0104                                              
 0103           ivcomp = ivcomp * 7                                     
 0104           continue                                                
                if ( .not.(.not.(lvon01.and..true.)).or..false..and.              .not.lvon01 )  then                                     
                     ivcomp = ivcomp * 11                               
                end if                                                  
           end if                                                       
      end if                                                            
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 30 = 1 * 2 * 3 * 5   ****
!                                                                        
      ivcorr = 30                                                       
40100 if ( ivcomp - 30 )  20100, 10100, 20100                           
30100 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10100, 0111, 20100                                    
10100 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0111                                                        
20100 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0111 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 011  ****                         
!                                                                        
!                                                                        
!         TEST 011 HAS E1 AND E3 AS TRUE.  E2 IS FALSE.  ONLY IF-BLOCK 1 
!      SHOULD BE EXECUTED.  THIS SET OF BLOCK IF STATEMENTS HAS INTEGER  
!      ASSIGNMENT STATEMENTS BETWEEN THE END IF STATEMENTS.  A CHECK IS  
!      MADE TO DETERMINE IF THESE STATEMENTS HAVE BEEN EXECUTED.         
!                                                                        
!                                                                        
      ivtnum =  11                                                      
      if (iczero) 30110, 0110, 30110                                    
 0110 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      lvon03 = .true.                                                   
!                                                                        
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                end if                                                  
                ivcomp = ivcomp * 7                                     
           end if                                                       
           ivcomp = ivcomp * 11                                         
      end if                                                            
      ivcomp = ivcomp * 13                                              
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 286 = 1*2*11*13      ****
!                                                                        
      ivcorr = 286                                                      
40110 if ( ivcomp - 286 )  20110, 10110, 20110                          
30110 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10110, 0121, 20110                                    
10110 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0121                                                        
20110 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0121 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 012  ****                         
!                                                                        
!                                                                        
!         TEST 012 HAS E1 AS TRUE.  E2 AND E3 ARE FALSE.  ONLY IF-BLOCK 1
!      SHOULD BE EXECUTED.  INTEGER ASSIGNMENT STATEMENTS ARE USED TO    
!      DETERMINE THE FLOW OF LOGIC THROUGH THE BLOCK IF STRUCTURE.       
!                                                                        
!                                                                        
      ivtnum =  12                                                      
      if (iczero) 30120, 0120, 30120                                    
 0120 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .true.                                                   
      lvon02 = .false.                                                  
      lvon03 = .false.                                                  
!                                                                        
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                end if                                                  
                ivcomp = ivcomp * 7                                     
           end if                                                       
           ivcomp = ivcomp * 11                                         
      end if                                                            
      ivcomp = ivcomp * 13                                              
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 286 = 1*2*11*13      ****
!                                                                        
      ivcorr = 286                                                      
40120 if ( ivcomp - 286 )  20120, 10120, 20120                          
30120 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10120, 0131, 20120                                    
10120 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0131                                                        
20120 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0131 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 013  ****                         
!                                                                        
!                                                                        
!         TEST 013 HAS E1 FALSE.  E2 AND E3 ARE TRUE.  NONE OF THE IF-   
!      BLOCKS SHOULD BE EXECUTED.  INTEGER ASSIGNMENT STATEMENTS ARE     
!      USED TO TRACE THE FLOW OF LOGIC THROUGH THE BLOCK IF STRUCTURE.   
!                                                                        
!                                                                        
      ivtnum =  13                                                      
      if (iczero) 30130, 0130, 30130                                    
 0130 continue                                                          
      ivcomp = 1                                                        
      lvon01 = .false.                                                  
      lvon02 = .true.                                                   
      lvon03 = .true.                                                   
!                                                                        
      if ( lvon01 )  then                                               
           ivcomp = ivcomp * 2                                          
           if ( lvon02 )  then                                          
                ivcomp = ivcomp * 3                                     
                if ( lvon03 )  then                                     
                     ivcomp = ivcomp * 5                                
                end if                                                  
                ivcomp = ivcomp * 7                                     
           end if                                                       
           ivcomp = ivcomp * 11                                         
      end if                                                            
      ivcomp = ivcomp * 13                                              
!                                                                        
!         **** IVCOMP IS DETERMINED BY IVCOMP = 13 = 1 * 13          ****
!                                                                        
      ivcorr = 13                                                       
40130 if ( ivcomp - 13 )  20130, 10130, 20130                           
30130 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10130, 0141, 20130                                    
10130 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0141                                                        
20130 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0141 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 014  ****                         
!                                                                        
!         TEST 014 IS TO CHECK FOR PROPER TRANSFER OF CONTROL USING      
!      LOGICAL IF STATEMENTS WITHIN IF-BLOCKS AND BRANCHING TO THE       
!      OUTERMOST IF-LEVEL FROM THE INNERMOST IF-LEVEL IN A CONTROLLED    
!      LOOP.  THE INNERMOST IF-LEVEL SHOULD BE EXECUTED 10 TIMES.  A     
!      LOGICAL IF STATEMENT IS USED IN EACH OF THE IF-LEVELS IN CASE     
!      THE EXECUTION LOGIC BRANCHES INCORRECTLY.  THIS SHOULD PREVENT    
!      AN INFINITE LOOP DURING THE EXECUTION OF THIS ROUTINE.            
!                                                                        
!                                                                        
      ivtnum =  14                                                      
      if (iczero) 30140, 0140, 30140                                    
 0140 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 0                                                        
      ivon02 = 0                                                        
      ivon03 = 0                                                        
 0142 if ( ivon03  <  10 )  then                                       
           ivon01 = ivon01 + 1                                          
           if ( ivon01  >  11 )  goto 0143                            
           if ( ivon03  <  10 )  then                                  
                ivon02 = ivon02 + 1                                     
                if ( ivon02  >  11 )  goto 0143                       
                if ( ivon03  <  10 )  then                             
                     ivon03 = ivon03 + 1                                
                     if ( ivon03  >  11 )  goto 0143                  
                     if ( ivon03  <=  10 )  goto 0142                  
                end if                                                  
           end if                                                       
      end if                                                            
 0143 continue                                                          
      ivcomp = ivon01                                                   
      ivcorr = 10                                                       
40140 if ( ivcomp - 10 ) 20140, 10140, 20140                            
30140 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10140, 0151, 20140                                    
10140 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0151                                                        
20140 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0151 continue                                                          
!                                                                        
!         THE NEXT TWO TESTS ARE TO CHECK THE COUNTERS IN IF-LEVEL 2 AND 
!      IF-LEVEL 3 RESPECTIVELY IN THE PREVIOUS TEST.                     
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 015  ****                         
!                                                                        
!      TEST 015 CHECKS THAT THE INTEGER COUNTER IN IF-LEVEL 2 IN THE     
!      PREVIOUS TEST IS EQUAL TO TEN (10).                               
!                                                                        
!                                                                        
      ivtnum =  15                                                      
      if (iczero) 30150, 0150, 30150                                    
 0150 continue                                                          
      ivcomp = ivon02                                                   
      ivcorr = 10                                                       
40150 if ( ivcomp - 10 ) 20150, 10150, 20150                            
30150 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10150, 0161, 20150                                    
10150 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0161                                                        
20150 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0161 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 016  ****                         
!                                                                        
!         TEST 016 CHECKS THAT THE INTEGER COUNTER IN IF-LEVEL 3 IN THE  
!      PREVIOUS TEST IS EQUAL TO TEN (10).                               
!                                                                        
!                                                                        
      ivtnum =  16                                                      
      if (iczero) 30160, 0160, 30160                                    
 0160 continue                                                          
      ivcomp = ivon03                                                   
      ivcorr = 10                                                       
40160 if ( ivcomp - 10 )  20160, 10160, 20160                           
30160 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10160, 0171, 20160                                    
10160 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0171                                                        
20160 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0171 continue                                                          
!                                                                        
!         THE NEXT THREE TESTS ARE SIMILAR TO THE PREVIOUS THREE TESTS   
!      IN THAT THEY TEST THE TRANSFER OF CONTROL WITHIN A THREE LEVEL    
!      BLOCK IF STRUCTURE.  EACH OF THE IF-LEVELS ARE EXECUTED AS IF THEY
!      WERE A LOOP USING LOGICAL IF STATEMENTS WITHIN EACH IF-LEVEL.     
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 017  ****                         
!                                                                        
!         TEST 017 CHECKS THAT THE VALUE OF THE INTEGER COUNTER IVON04 IN
!      IF-LEVEL 1 IN THIS TEST EQUALS 10.                                
!                                                                        
!                                                                        
      ivtnum =  17                                                      
      if (iczero) 30170, 0170, 30170                                    
 0170 continue                                                          
      ivcomp = 0                                                        
      ivon01 = 0                                                        
      ivon02 = 0                                                        
      ivon03 = 0                                                        
      ivon04 = 0                                                        
      ivon05 = 0                                                        
      ivon06 = 0                                                        
!                                                                        
 0172 if ( ivon01  <  10 )  then                                       
           ivon01 = ivon01 + 1                                          
           ivon04 = ivon04 + 1                                          
           if ( ivon01  >  11 )  goto 0175                            
 0173      if ( ivon02  <  10 )  then                                  
                ivon02 = ivon02 + 1                                     
                ivon05 = ivon05 + 1                                     
               if ( ivon02  >  11 )  goto 0175                        
 0174           if ( ivon03  <  10 )  then                             
                     ivon03 = ivon03 + 1                                
                     ivon06 = ivon06 + 1                                
                     if ( ivon03  >  11 )  goto 0175                  
                     if ( ivon03  <=  10 )  goto 0174                  
                end if                                                  
                ivon03 = 0                                              
                if ( ivon02  <=  10 )  goto 0173                       
           end if                                                       
           ivon02 = 0                                                   
           if ( ivon01  <=  10 )  goto 0172                            
      end if                                                            
 0175 continue                                                          
      ivcomp = ivon04                                                   
      ivcorr = 10                                                       
40170 if ( ivcomp - 10 )  20170, 10170, 20170                           
30170 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10170, 0181, 20170                                    
10170 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0181                                                        
20170 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0181 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 018  ****                         
!                                                                        
!         TEST 018 CHECKS THAT THE VALUE OF THE INTEGER COUNTER IVON05 IN
!      IF-LEVEL 2 OF THE PREVIOUS TEST EQUALS 100.                       
!                                                                        
!                                                                        
      ivtnum =  18                                                      
      if (iczero) 30180, 0180, 30180                                    
 0180 continue                                                          
      ivcomp = ivon05                                                   
      ivcorr = 100                                                      
40180 if ( ivcomp - 100 )  20180, 10180, 20180                          
30180 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10180, 0191, 20180                                    
10180 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0191                                                        
20180 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0191 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 019  ****                         
!                                                                        
!         TEST 019 CHECKS THAT THE VALUE OF THE INTEGER COUNTER IVON06 IN
!      IF-LEVEL 3 OF THE PREVIOUS TEST EQUALS 1000.                      
!                                                                        
!                                                                        
      ivtnum =  19                                                      
      if (iczero) 30190, 0190, 30190                                    
 0190 continue                                                          
      ivcomp = ivon06                                                   
      ivcorr = 1000                                                     
40190 if ( ivcomp - 1000 )  20190, 10190, 20190                         
30190 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10190, 0201, 20190                                    
10190 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0201                                                        
20190 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0201 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 020  ****                         
!                                                                        
!         TEST 020 USES A LOGICAL STATEMENT FUNCTION  LFIS01(L) AS       
!      THE LOGICAL EXPRESSION IN A BLOCK IF STRUCTURE.  THE LOGICAL      
!      STATEMENT FUNCTION TAKES THE LOGICAL COMPLEMENT OF THE LOGICAL    
!      VALUE SUPPLIED.  THE VALUE OF .FALSE. IS SUPPLIED AND THE LOGICAL 
!      VALUE OF .TRUE. SHOULD BE RETURNED AS THE LOGICAL FUNCTION        
!      REFERENCE.  THE IF-BLOCK OF LEVEL ONE SHOULD BE EXECUTED.         
!                                                                        
!                                                                        
      ivtnum =  20                                                      
      if (iczero) 30200, 0200, 30200                                    
 0200 continue                                                          
      ivcomp = 0                                                        
      lvon01 = .false.                                                  
      if ( lfis01( lvon01 ) )  then                                     
           ivcomp = 1                                                   
      end if                                                            
      ivcorr = 1                                                        
40200 if ( ivcomp - 1 )  20200, 10200, 20200                            
30200 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10200, 0211, 20200                                    
10200 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0211                                                        
20200 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0211 continue                                                          
!                                                                        
!                                                                        
!         THE FOLLOWING SERIES OF TESTS ARE TO CHECK THE PRECEDENCE OF   
!      OPERATORS.  THESE INCLUDE ARITHMETIC, RELATIONAL, AND LOGICAL     
!      OPERATORS.  ARITHMETIC OPERATORS ARE CHECKED FIRST FROM THE       
!      EVALUATION OF CERTAIN ARITHMETIC EXPRESSIONS THAT USE ONLY INTEGER
!      VALUES IN THE COMPUTATIONS.  ALL INTERMEDIATE AND FINAL VALUES ARE
!      LESS THAN 32767.  AFTER EACH OF THE STATEMENTS IS TESTED BY ITSELF
!      THEN THE RELATIONAL OPERATORS ARE TESTED USING THE INTEGER VALUES 
!      OBTAINED IN EACH OF THE ARITHMETIC EXPRESSIONS.  IN THIS TEST THE 
!      RELATIONAL EXPRESSIONS ARE COMBINED WITH LOGICAL OPERATORS TO     
!      PRODUCE A LOGICAL EXPRESSION.  FINALLY THE ENTIRE SET OF SIX (6)  
!      ARITHMETIC , RELATIONAL, AND LOGICAL EXPRESSIONS IS COMBINED INTO 
!      ONE LOGICAL IF STATEMENT.                                         
!                                                                        
!                                                                        
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 021  ****                         
!                                                                        
!         TEST 021 CHECKS THE ORDER OF EVALUATION WHEN AN ARITHMETIC     
!      EXPRESSION HAS PARENTHESES AND A SERIES OF EXPONENTIATION.  THE   
!      ORDER OF EVALUATION IS SHOWN BELOW -                              
!                                                                        
!         1 + 2 * ( 4 - 2 ) ** 2 ** 3 - 4 / 2                            
!         1 + 2 * ( 2 ) ** 2 ** 3 - 4 / 2                                
!         1 + 2 * 2 ** 8 - 4 / 2                                         
!         1 + 2 * 256 - 4 / 2                                            
!         1 + 512 - 2                                                    
!         513 - 2                                                        
!         511                                                            
!                                                                        
      ivtnum =  21                                                      
      if (iczero) 30210, 0210, 30210                                    
 0210 continue                                                          
      ivon01 = 1                                                        
      ivon02 = 2                                                        
      ivon03 = 4                                                        
      ivon04 = 2                                                        
      ivon05 = 4                                                        
      ivon06 = 2                                                        
      ivcomp = ivon01 + ivon02 * ( ivon03 - ivon04 ) ** 2 ** 3 - ivon05 / ivon06                                                 
      ivcorr = 511                                                      
40210 if ( ivcomp - 511 )  20210, 10210, 20210                          
30210 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10210, 0221, 20210                                    
10210 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0221                                                        
20210 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0221 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 022  ****                         
!                                                                        
!         TEST 022 IS A SERIES OF DIVISIONS FOLLOWED BY A SERIES OF      
!      MULTIPLICATIONS ALL WITHOUT ANY PARENTHESES.                      
!                                                                        
!         16 / 2 / 2 / 2 * 4 * 8                                         
!         8 / 2 / 2 * 4 * 8                                              
!         4 / 2 * 4 * 8                                                  
!         2 * 4 * 8                                                      
!         8 * 8                                                          
!         64                                                             
!                                                                        
!                                                                        
      ivtnum =  22                                                      
      if (iczero) 30220, 0220, 30220                                    
 0220 continue                                                          
      ivon07 = 16                                                       
      ivon08 = 2                                                        
      ivon09 = 2                                                        
      ivon10 = 2                                                        
      ivon11 = 4                                                        
      ivon12 = 8                                                        
      ivcomp = ivon07 / ivon08 / ivon09 / ivon10 * ivon11 * ivon12      
      ivcorr = 64                                                       
40220 if ( ivcomp - 64 ) 20220, 10220, 20220                            
30220 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10220, 0231, 20220                                    
10220 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0231                                                        
20220 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0231 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 023  ****                         
!                                                                        
!         TEST 023 HAS ONE SUBTRACTION IMBEDDED IN A SERIES OF ADDITIONS 
!      WITHOUT ANY PARENTHESES.                                          
!                                                                        
!         3 + 4 - 1 + 5                                                  
!         7 - 1 + 5                                                      
!         6 + 5                                                          
!         11                                                             
!                                                                        
!                                                                        
      ivtnum =  23                                                      
      if (iczero) 30230, 0230, 30230                                    
 0230 continue                                                          
      ivon13 = 3                                                        
      ivon14 = 4                                                        
      ivon15 = 1                                                        
      ivon16 = 5                                                        
      ivcomp = ivon13 + ivon14 - ivon15 + ivon16                        
      ivcorr = 11                                                       
40230 if ( ivcomp - 11 )  20230, 10230, 20230                           
30230 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10230, 0241, 20230                                    
10230 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0241                                                        
20230 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0241 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 024  ****                         
!                                                                        
!         TEST 024 HAS ADDITION, SUBTRACTION, MULTIPLICATION, DIVISION,  
!      AND EXPONENTIATION WITHOUT PARENTHESES.                           
!                                                                        
!         4 + 4 - 6 * 3 / 3 ** 2                                         
!         4 + 4 - 6 * 3 / 9                                              
!         4 + 4 - 18 / 9                                                 
!         4 + 4 - 2                                                      
!         8 - 2                                                          
!         6                                                              
!                                                                        
!                                                                        
      ivtnum =  24                                                      
      if (iczero) 30240, 0240, 30240                                    
 0240 continue                                                          
      ivon17 = 4                                                        
      ivon18 = 4                                                        
      ivon19 = 6                                                        
      ivon20 = 3                                                        
      ivon21 = 3                                                        
      ivon22 = 2                                                        
      ivcomp = ivon17 + ivon18 - ivon19 * ivon20 / ivon21 ** ivon22     
      ivcorr = 6                                                        
40240 if ( ivcomp - 6 )  20240, 10240, 20240                            
30240 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10240, 0251, 20240                                    
10240 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0251                                                        
20240 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0251 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 025  ****                         
!                                                                        
!         TEST 025 IS LIKE TEST NUMBER 021 EXCEPT THAT THE PARENTHESES   
!      HAVE BEEN REMOVED.  THE INTEGER VALUES USED AS INPUT ARE THE SAME.
!      REMOVAL OF THE PARENTHESES CHANGES THE ORDER OF EVALUATION SO THE 
!      FINAL INTEGER RESULT IS DIFFERENT.                                
!                                                                        
!         1 + 2 * 4 - 2 ** 2 ** 3 - 4 / 2                                
!         1 + 2 * 4 - 2 ** 8 - 4 / 2                                     
!         1 + 2 * 4 - 256 - 4 / 2                                        
!         1 + 8 - 256 - 2                                                
!         9 - 256 - 2                                                    
!         -247 - 2                                                       
!         -249                                                           
!                                                                        
      ivtnum =  25                                                      
      if (iczero) 30250, 0250, 30250                                    
 0250 continue                                                          
      ivon23 = 1                                                        
      ivon24 = 2                                                        
      ivon25 = 4                                                        
      ivon26 = 2                                                        
      ivon27 = 4                                                        
      ivon28 = 2                                                        
      ivcomp = ivon23 + ivon24 * ivon25 - ivon26 ** 2 ** 3 - ivon27     / ivon28                                                
      ivcorr = -249                                                     
40250 if ( ivcomp + 249 )  20250, 10250, 20250                          
30250 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10250, 0261, 20250                                    
10250 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0261                                                        
20250 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0261 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 026  ****                         
!                                                                        
!         TEST 026 IS JUST LIKE TEST NUMBER 022 EXCEPT THAT PARENTHESES  
!      HAVE BEEN ADDED.  ALTHOUGH THE INTEGER VALUES ARE THE SAME, THE   
!      PARENTHESES CHANGE THE ORDER OF EVALUATION SO THAT THE FINAL      
!      INTEGER RESULT IS DIFFERENT.                                      
!                                                                        
!         16 / ( 2 / 2 ) / 2 * ( 4 * 8 )                                 
!         16 / ( 1 ) / 2 * ( 32 )                                        
!         16 / 2 * 32                                                    
!         8 * 32                                                         
!         256                                                            
!                                                                        
      ivtnum =  26                                                      
      if (iczero) 30260, 0260, 30260                                    
 0260 continue                                                          
      ivon29 = 16                                                       
      ivon30 = 2                                                        
      ivon31 = 2                                                        
      ivon32 = 2                                                        
      ivon33 = 4                                                        
      ivon34 = 8                                                        
      ivcomp = ivon29 / ( ivon30 / ivon31 ) / ivon32 * ( ivon33 *                 ivon34 )                                                
      ivcorr = 256                                                      
40260 if ( ivcomp - 256 )  20260, 10260, 20260                          
30260 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10260, 0271, 20260                                    
10260 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0271                                                        
20260 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0271 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 027  ****                         
!                                                                        
!         TEST 027 COMBINES THE INTEGER RESULTS OBTAINED IN THE PREVIOUS 
!      SIX TESTS AND USES RELATIONAL AND LOGICAL OPERATORS IN ONE        
!      LOGICAL EXPRESSION.  RELATIONAL EXPRESSIONS ARE EVALUATED FIRST   
!      FOLLOWED BY THE LOGICAL OPERATORS .NOT. , .AND., AND .OR. IN THAT 
!      ORDER.                                                            
!                                                                        
!         511 .LT. 64 .OR. .NOT. 11 .LE. 6 .AND. -249 .LE. 256           
!         F .OR. .NOT. F .AND. T                                         
!         F .OR. T .AND. T                                               
!         F .OR. T                                                       
!         T                                                              
!                                                                        
!                                                                        
      ivtnum =  27                                                      
      if (iczero) 30270, 0270, 30270                                    
 0270 continue                                                          
      ivon35 = 511                                                      
      ivon36 = 64                                                       
      ivon37 = 11                                                       
      ivon38 = 6                                                        
      ivon39 = -249                                                     
      ivon40 = 256                                                      
      ivcomp = 0                                                        
      lvon01 = ivon35 .lt. ivon36 .or. .not. ivon37 .le. ivon38 .and.             ivon39 .le. ivon40                                      
      if ( lvon01 )  ivcomp = 1                                         
      ivcorr = 1                                                        
40270 if ( ivcomp - 1 )  20270, 10270, 20270                            
30270 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10270, 0281, 20270                                    
10270 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0281                                                        
20270 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0281 continue                                                          
!                                                                        
!      ****  FCVS PROGRAM 253  -  TEST 028  ****                         
!                                                                        
!         TEST 028 IS THE BIGGIE.  IT COMBINES ALL OF THE INTEGER VALUES 
!      AND RESULTS IN THE PREVIOUS SEVEN (7) TESTS.  IF THERE WERE ANY   
!      ERRORS IN ANY OF THE PREVIOUS SEVEN TESTS, THEN THIS TEST SHOULD  
!      ALSO FAIL.                                                        
!                                                                        
!                                                                        
      ivtnum =  28                                                      
      if (iczero) 30280, 0280, 30280                                    
 0280 continue                                                          
      ivcomp = 0                                                        
      if ( ivon01 + ivon02 * ( ivon03 - ivon04 ) ** 2 ** 3 - ivon05 /   ivon06  <  ivon07 / ivon08 / ivon09 / ivon10 * ivon11 * ivon12   .or. .not. ivon13 + ivon14 - ivon15 + ivon16  <=  ivon17 + ivon18 - ivon19 * ivon20 / ivon21 ** ivon22 .and. ivon23 + ivon24 *      ivon25 - ivon26 ** 2 ** 3 - ivon27 / ivon28  <=  ivon29 / ( ivon30/ ivon31 ) / ivon32 * ( ivon33 * ivon34 ) )  ivcomp = 1          
      ivcorr = 1                                                        
40280 if ( ivcomp - 1 )  20280, 10280, 20280                            
30280 ivdele = ivdele + 1                                               
      write (i02,80000) ivtnum                                          
      if (iczero) 10280, 0291, 20280                                    
10280 ivpass = ivpass + 1                                               
      write (i02,80002) ivtnum                                          
      goto 0291                                                        
20280 ivfail = ivfail + 1                                               
      write (i02,80010) ivtnum, ivcomp, ivcorr                          
 0291 continue                                                          
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
90001 format (" ",24x,"FM253")                                          
90000 format (" ",20x,"END OF PROGRAM FM253" )                          
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
      end program fm253
