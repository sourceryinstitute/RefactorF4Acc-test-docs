      program fm090
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      real :: a
      real :: b
      real :: c
      real :: d
      real :: e
      real :: f
      real :: g
      real :: h
      integer :: i
      integer :: j
      integer :: k
      integer :: l
      integer :: m
      integer :: n
      real :: o
      real :: p
      real :: q
      real :: r
      real :: s
      real :: t
      real :: u
      real :: v
      real :: w
      real :: x
      real :: y
      real :: z
      real :: aaaaaa
      real :: bbbbb
      real :: cccc
      real :: ddd
      real :: ee
      real :: f0
      real :: g12
      real :: h345
      integer :: i6789
      integer :: j01234
      integer :: k56789
      integer :: l2l2l2
      integer :: m3m3m3
      integer :: n40
      real :: omy
      integer :: ipmh
      real :: goto1
      integer :: if3
      real :: do3
      real :: callfl
      real :: typei
      real :: true
      real :: false
      integer :: ivcomp
      integer :: ivcorr
      integer :: iace11
      integer :: iace21
      integer :: iace31
      integer :: iacn11
      integer :: iadn11
      integer :: iate31
      real :: race11
      real :: race21
      real :: racn31
      real :: rade31
      integer :: ivte69
      integer :: ivon78
      real :: rvtnaz
      real :: rvoez9
      integer :: icte96
      integer :: icon84
      real :: rcon48
      real :: rcte54
      integer :: idony4
      integer :: idoeb6
      real :: rdon46
      integer :: ifons3
      real :: rfon77
      integer :: ivon01
!      COMMENT SECTION.                                                  
!      WV: VARIANT of FM010, removed spaces to make it sane              
!       FM090                                                            
!                                                                        
!              THIS ROUTINE TESTS REFERENCE FORMAT OF FORTRAN STATEMENTS 
!      AND STATEMENT NUMBERS.  THE USE OF THE BLANK CHARACTER IS TESTED  
!      BOTH WITHIN THE STATEMENT NUMBER FIELD AND WITHIN THE FORTRAN     
!      STATEMENTS THEMSELVES.  LEADING ZERO IS TESTED FOR STATEMENTS AND 
!      INTEGER CONSTANTS.  VARIABLE NAMES WHICH LOOK VERY MUCH LIKE      
!      FORTRAN RESERVED WORDS ARE TESTED IN ARITHMETIC ASSIGNMENT        
!      STATEMENTS.  NAMING CONVENTIONS USED THROUGHOUT THE FCVS ARE      
!      TESTED ALSO IN ARITHMETIC ASSIGNMENT STATEMENTS.                  
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 2.5, VARIABLES                                         
!         SECTION 3.1.6, BLANK CHARACTER                                 
!         SECTION 3.2.2, INITIAL LINES                                   
!         SECTION 3.4, STATEMENT LABELS                                  
!                                                                        
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
 1001 continue                                                          
      ivtnum = 100                                                      
!                                                                        
!       ****  TEST  100  ****                                            
!                                                                        
!      TEST 100  -  TO CHECK THE VARIOUS COMBINATIONS OF FORMING VARIABLE
!            NAMES.  THESE ARE ACTUALLY SYMBOLIC NAMES (ANSI X3.9-1978   
!            SECTION 2.2).  THIS IS BASICALLY A SYNTAX CHECK USING A     
!            COMBINATION OF FROM ONE TO SIX ALPHANUMERIC CHARACTERS WITH 
!            THE FIRST CHARACTER ALWAYS ALPHABETIC.  REFERENCE FORMAT IS 
!            ALSO CHECKED BY HAVING EACH ASSIGNMENT STATEMENT AN INITIAL 
!            LINE (SECTION 3.2.2).  THIS MEANS ZERO MAY APPEAR IN COLUMN 
!            SIX WITHOUT EFFECT, THAT LINES MAY BEGIN ANYWHERE FROM      
!            COLUMN SEVEN TO COLUMN 72, AND BLANKS MAY BE USED FREELY    
!            WITHOUT MEANING (3.1.6 BLANK CHARACTERS).                   
!                                                                        
      if (iczero) 31000, 1000, 31000                                    
 1000 continue                                                          
      a=1.                                                              
      b =2.                                                             
      c =3.                                                             
      d   =4.                                                           
      e     =5.                                                         
      f      =6.                                                        
      g                      =                   7.                     
                                        h=8.                            
                                                                     i=9
      j  =  10                                                          
          k        =          11                                        
      l                                 =                             12
      m=13                                                              
      n=14                                                              
      o=15.                                                             
      p=16.                                                             
      q=17.                                                             
      r=18.                                                             
      s=19.                                                             
      t=20.                                                             
      u=21.                                                             
      v=22.                                                             
      w=23.                                                             
      x=24.                                                             
      y=25.                                                             
      z=26.                                                             
      aaaaaa=27.                                                        
      bbbbb=28.                                                         
      cccc=29.                                                          
      ddd=30                                                            
      ee=31.                                                            
      f0=32.                                                            
      g12=33.                                                           
      h345 = 34.                                                        
      i6789 = 35                                                        
      j01234 = 36                                                       
      k56789=37                                                         
       l2l2l2 =38                                                       
        m3m3m3   =                                                   39 
         n40        =                   40                              
          omy    =           41.                                        
      ipmh =           42                                               
      goto1 = 43.                                                       
      if3  = 44                                                         
      do3  =   53.                                                      
      callfl  =62.                                                      
      typei  = 63.                                                      
      true   =71.                                                       
      false  = 72.                                                      
      goto 41000                                                       
31000 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41000, 1011, 41000                                    
41000 if (ipmh - 42) 21000,11000,21000                                  
11000 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1011                                                        
21000 ivfail = ivfail + 1                                               
      ivcomp = ipmh                                                     
      ivcorr = 42                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1011 continue                                                          
      ivtnum = 101                                                      
!                                                                        
!       ****  TEST  101  ****                                            
!      TEST 101  -  CHECKS THE FCVS NAMING CONVENTIONS FOR INTEGER AND   
!            REAL VARIABLES IN ASSIGNMENT STATEMENTS: VARIABLE = CONSTANT
!            BASICALLY A SYNTAX CHECK ON SIX CHARACTER VARIABLE NAMES.   
!                                                                        
      if (iczero) 31010, 1010, 31010                                    
 1010 continue                                                          
      iace11 = 1                                                        
      iace21 = 2                                                        
      iace31 = 3                                                        
      iacn11 = 4                                                        
      iadn11 = 5                                                        
      iate31 = 6                                                        
      race11 = 7.                                                       
      race21 = 8.                                                       
      racn31 = 9.                                                       
      rade31 = 10.                                                      
      ivte69 = 11                                                       
      ivon78 = 12                                                       
      rvtnaz = 13.                                                      
      rvoez9 = 14.                                                      
      icte96 = 15                                                       
      icon84 = 16                                                       
      rcon48 = 17.                                                      
      rcte54 = 18.                                                      
      idony4 = 19                                                       
      idoeb6 = 20                                                       
      rdon46 = 21.                                                      
      ifons3 = 22                                                       
      rfon77 = 23.                                                      
      goto 41010                                                       
31010 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41010, 1021, 41010                                    
41010 if (ivte69 - 11) 21010,11010,21010                                
11010 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1021                                                        
21010 ivfail = ivfail + 1                                               
      ivcomp = ivte69                                                   
      ivcorr = 11                                                       
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1021 continue                                                          
      ivtnum = 102                                                      
!                                                                        
!       ****  TEST  102  ****                                            
!      TEST 102  -  REFERENCE FORMAT CHECK ON STATEMENT LABELS (SECTION  
!            3.4). THESE ARE NON-ZERO INTEGERS, FROM 1 TO 5 DIGITS,      
!            MAY BEGIN ANYWHERE FROM COLS. 1 TO 5, AND LEADING ZEROS ARE 
!            NOT SIGNIFICANT.  BLANKS WILL BE IMBEDDED IN SOME OF THE    
!            STATEMENT LABELS AND THESE SHOULD HAVE NO EFFECT.  THE      
!            CONTINUE STATEMENT (SECTION 11.11) IS USED FOR THIS TEST.   
!            A BASIC FCVS ASSUMPTION IS THAT THE LOGIC WILL FALL THRU A  
!            SERIES OF CONTINUE STATEMENTS (NORMAL EXECUTION SEQUENCE).  
!                                                                        
      if (iczero) 31020, 1020, 31020                                    
 1020 continue                                                          
1     continue                                                          
 2    continue                                                          
  3   continue                                                          
   4  continue                                                          
    5 continue                                                          
06    continue                                                          
 007  continue                                                          
 0008 continue                                                          
00009 continue                                                          
 010  continue                                                          
   11 continue                                                          
  012 continue                                                          
  013 continue                                                          
 0014 continue                                                          
 015  continue                                                          
 0016 continue                                                          
100   continue                                                          
101   continue                                                          
102   ivon01 = 1                                                        
103   continue                                                          
 104  continue                                                          
0105  continue                                                          
0106  continue                                                          
0107  continue                                                          
00108 continue                                                          
111   continue                                                          
1111  continue                                                          
  99  continue                                                          
999   continue                                                          
9999  continue                                                          
      goto 41020                                                       
31020 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41020, 1031, 41020                                    
41020 if (ivon01 - 1) 21020,11020,21020                                 
11020 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1031                                                        
21020 ivfail = ivfail + 1                                               
      ivcomp = ivon01                                                   
      ivcorr = 1                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1031 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM090" )                          
      end program fm090
