      program fm012
      integer :: i01
      integer :: i02
      integer :: ivpass
      integer :: ivfail
      integer :: ivdele
      integer :: iczero
      integer :: ivtnum
      integer :: ivon01
      integer :: i
      integer :: ivcomp
      integer :: ivcorr
      integer :: j
      integer :: k
      integer :: l
      integer :: m
      integer :: n
      integer :: i1
      integer :: i2
      integer :: i3
      integer :: i4
      integer :: i5
!                                                                        
!      COMMENT SECTION.                                                  
!                                                                        
!      FM012                                                             
!                                                                        
!              THIS ROUTINE TESTS THE FORTRAN DO - STATEMENT FROM ITS    
!      SIMPLIST FORMAT TO THE MORE ABBREVIATED FORMS.  VARIOUS INCREMENTS
!      ARE USED AND BRANCHING BY VARIOUS METHODS IS TESTED FOR PASSING   
!      CONTROL OUT OF THE DO RANGE AND RETURNING (EXTENDED RANGE).       
!      NESTED DO STATEMENTS USING VARIOUS TERMINATING STATEMENTS ARE ALSO
!      TESTED BY THIS ROUTINE.                                           
!                                                                        
!       REFERENCES                                                       
!         AMERICAN NATIONAL STANDARD PROGRAMMING LANGUAGE FORTRAN,       
!               X3.9-1978                                                
!                                                                        
!         SECTION 11.10, DO STATEMENT                                    
!         SECTION 11.10.3, EXECUTES A DO LOOP                            
!         SECTION 11.11, CONTINUE STATEMENT                              
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
      ivtnum = 110                                                      
!                                                                        
!      TEST 110  -  DO STATEMENT WITH THE COMPLETE FORMAT, INCREMENT OF 1
!            THE LOOP SHOULD BE EXECUTED TEN (10) TIMES THUS THE LOOP    
!            COUNTER SHOULD HAVE A VALUE OF TEN AT THE COMPLETION OF THE 
!            DO-LOOP.                                                    
!                                                                        
!                                                                        
      if (iczero) 31100, 1100, 31100                                    
 1100 continue                                                          
      ivon01=0                                                          
      do i=1,10,1                                                  
      ivon01=ivon01+1                                                   
  end do
      goto 41100                                                       
31100 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41100, 1111, 41100                                    
41100 if(ivon01-10) 21100,11100,21100                                   
11100 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1111                                                        
21100 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=10                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1111 continue                                                          
      ivtnum = 111                                                      
!                                                                        
!      TEST 111  -  SAME DO TEST AS IN TEST 110 EXCEPT THAT NO INCREMENT 
!            IS GIVEN.  THE INCREMENT SHOULD BE 1 AND THE LOOP PERFORMED 
!            TEN (10) TIMES AS BEFORE.                                   
!                                                                        
!                                                                        
      if (iczero) 31110, 1110, 31110                                    
 1110 continue                                                          
      ivon01=0                                                          
      do j=1,10                                                    
      ivon01=ivon01+1                                                   
  end do
      goto 41110                                                       
31110 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41110, 1121, 41110                                    
41110 if(ivon01-10)  21110, 11110, 21110                                
11110 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1121                                                        
21110 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=10                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1121 continue                                                          
      ivtnum = 112                                                      
!                                                                        
!      TEST 112  -  DO STATEMENT WITH AN INCREMENT OTHER THAN ONE (1).   
!            THE DO - LOOP SHOULD BE EXECUTED FIVE (5) TIMES THUS        
!            THE VALUE OF THE LOOP COUNTER SHOULD BE FIVE (5) AT THE     
!            END OF THE DO - LOOP.                                       
!                                                                        
!                                                                        
      if (iczero) 31120, 1120, 31120                                    
 1120 continue                                                          
      ivon01=0                                                          
      do k = 1, 10, 2                                              
      ivon01=ivon01+1                                                   
  end do
      goto 41120                                                       
31120 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41120, 1131, 41120                                    
41120 if (ivon01 - 5 )  21120, 11120, 21120                             
11120 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1131                                                        
21120 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=5                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1131 continue                                                          
      ivtnum = 113                                                      
!                                                                        
!      TEST 113  -  DO STATEMENT WITH THE INITIAL VALUE EQUAL TO THE     
!            TERMINAL VALUE.  THE DO - LOOP SHOULD BE EXECUTED ONE (1)   
!            TIME THUS THE VALUE OF THE LOOP COUNTER SHOULD BE ONE (1).  
!                                                                        
!                                                                        
      if (iczero) 31130, 1130, 31130                                    
 1130 continue                                                          
      ivon01=0                                                          
      do l = 2, 2                                                  
      ivon01=ivon01+1                                                   
  end do
      goto 41130                                                       
31130 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41130, 1141, 41130                                    
41130 if ( ivon01 - 1 )  21130, 11130, 21130                            
11130 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1141                                                        
21130 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1141 continue                                                          
      ivtnum = 114                                                      
!                                                                        
!      TEST 114  -  THIS TESTS THE UNCONDITIONAL BRANCH OUT OF THE       
!            RANGE OF THE DO USING THE GO TO STATEMENT.  THE DO INDEX    
!            SHOULD RETAIN THE VALUE IT HAD WHEN THE UNCONDITIONAL BRANCH
!            WAS MADE.  SINCE THE DO LOOP ONLY CONTAINS AN UNCONDITIONAL 
!            BRANCH, THE VALUE OF THE DO INDEX SHOULD BE ITS INITIAL     
!            VALUE.  IN THIS CASE THE VALUE SHOULD BE ONE (1).           
!            SEE SECTION 11.10.                                          
!                                                                        
!                                                                        
      if (iczero) 31140, 1140, 31140                                    
 1140 continue                                                          
      do m=1,10                                                    
      goto 1143                                                          !Break
  end do
 1143 continue                                                          
      goto 41140                                                       
31140 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41140, 1151, 41140                                    
41140 if ( m - 1 )  21140, 11140, 21140                                 
11140 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1151                                                        
21140 ivfail = ivfail + 1                                               
      ivcomp=m                                                          
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1151 continue                                                          
      ivtnum = 115                                                      
!                                                                        
!      TEST 115  -  THIS TEST IS SIMILAR TO TEST 114 IN THAT THE DO      
!            RANGE HAS ONLY AN UNCONDITIONAL BRANCH OUTSIDE OF THE RANGE.
!            THE DO INDEX SHOULD AGAIN RETAIN ITS VALUE, IN THIS CASE    
!            ITS INITIAL VALUE OF ONE (1).                               
!            SEE SECTION 11.10.                                          
!                                                                        
!                                                                        
      if (iczero) 31150, 1150, 31150                                    
 1150 continue                                                          
      do n = 1, 10                                                 
      if ( n - 1 )  1152, 1153, 1152                                    
 1152 end do
 1153 continue                                                          
      goto 41150                                                       
31150 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41150, 1161, 41150                                    
41150 if (n - 1 )  21150, 11150, 21150                                  
11150 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1161                                                        
21150 ivfail = ivfail + 1                                               
      ivcomp=n                                                          
      ivcorr=1                                                          
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1161 continue                                                          
      ivtnum = 116                                                      
!                                                                        
!      TEST 116  -  THIS IS A TEST OF A NEST OF TWO DO RANGES.  TWO      
!            SEPARATE CONTINUE STATEMENTS ARE USED AS TERMINAL STATEMENTS
!            FOR THE TWO RESPECTIVE DO RANGES.  THE OUTER LOOP SHOULD BE 
!            PERFORMED TEN (10) TIMES AND THE INNER LOOP SHOULD BE       
!            PERFORMED TWICE FOR EACH EXECUTION OF THE OUTER LOOP.  THE  
!            LOOP COUNTER SHOULD HAVE A VALUE OF TWENTY (20) SINCE IT    
!            IS INCREMENTED IN THE INNER DO - LOOP.                      
!            SEE SECTION 11.10.3.                                        
!                                                                        
!                                                                        
      if (iczero) 31160, 1160, 31160                                    
 1160 continue                                                          
      ivon01=0                                                          
      do i=1,10,1                                                  
      do j=1,2,1                                                   
      ivon01=ivon01+1                                                   
  end do
  end do
      goto 41160                                                       
31160 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41160, 1171, 41160                                    
41160 if ( ivon01 - 20 )  21160, 11160, 21160                           
11160 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1171                                                        
21160 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=20                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1171 continue                                                          
      ivtnum = 117                                                      
!                                                                        
!      TEST 117  -  THIS IS BASICALLY THE SAME AS TEST 116 EXCEPT THAT   
!            ONLY ONE CONTINUE STATEMENT IS USED AS THE TERMINATING      
!            STATEMENT FOR BOTH OF THE DO RANGES.  THE VALUE OF THE      
!            LOOP COUNTER SHOULD AGAIN BE TWENTY (20).                   
!                                                                        
!                                                                        
      if (iczero) 31170, 1170, 31170                                    
 1170 continue                                                          
      ivon01=0                                                          
      do k=1,10,1                                                  
      do l=1,2,1                                                   
      ivon01=ivon01+1                                                   
  end do
      end do
      goto 41170                                                       
31170 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41170, 1181, 41170                                    
41170 if (ivon01 - 20 )  21170, 11170, 21170                            
11170 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1181                                                        
21170 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=20                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1181 continue                                                          
      ivtnum = 118                                                      
!                                                                        
!      TEST 118  -  THIS IS BASICALLY THE SAME TEST AS 116 EXCEPT        
!            THAT THE LOOP COUNTER INCREMENT IS THE TERMINATING STATEMENT
!            OF BOTH OF THE DO RANGES.  THE VALUE OF THE LOOP COUNTER    
!            SHOULD BE TWENTY (20), BUT THE NUMBER OF EXECUTIONS OF      
!            THE OUTER LOOP IS NOW TWO (2) AND THE INNER LOOP EXECUTES   
!            TEN (10) TIMES FOR EVERY EXECUTION OF THE OUTER LOOP.       
!                                                                        
!                                                                        
      if (iczero) 31180, 1180, 31180                                    
 1180 continue                                                          
      ivon01=0                                                          
      do m=1,2,1                                                   
      do n=1,10,1                                                  
 1182 ivon01 = ivon01 + 1                                               
      end do
      end do
      goto 41180                                                       
31180 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41180, 1191, 41180                                    
41180 if (ivon01 - 20 )  21180, 11180, 21180                            
11180 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1191                                                        
21180 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=20                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1191 continue                                                          
      ivtnum = 119                                                      
!                                                                        
!      TEST 119  -  THIS IS A TEST OF AN UNCONDITIONAL BRANCH OUT OF A   
!            NESTED DO RANGE QUITE LIKE TEST 114.  THE LOOP COUNTER      
!            SHOULD ONLY BE INCREMENTED ON THE OUTER LOOP RANGE SO       
!             THE FINAL VALUE OF THE LOOP COUNTER SHOULD BE TEN (10).    
!                                                                        
!                                                                        
      if (iczero) 31190, 1190, 31190                                    
 1190 continue                                                          
      ivon01=0                                                          
      do i=1,10,1                                                  
      do j=1,2,1                                                   
!                                                                        
!      THE FOLLOWING STATEMENT IS TO ELIMINATE THE DEAD CODE PRODUCED    
!          BY THE STATEMENT   GO TO 1194.                                
!                                                                        
      if ( iczero )  1193, 1192, 1193                                   
!                                                                        
 1192  goto 1194                                                       
 1193 ivon01 = ivon01 + 1                                               
      end do
 1194 ivon01 = ivon01 + 1                                               
      end do
      goto 41190                                                       
31190 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41190, 1201, 41190                                    
41190 if ( ivon01 - 10 )  21190, 11190, 21190                           
11190 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1201                                                        
21190 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=10                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1201 continue                                                          
      ivtnum = 120                                                      
!                                                                        
!      TEST 120  -  THIS IS BASICALLY THE SAME TEST AS TEST 119 EXCEPT   
!            THAT AN IF STATEMENT IS USED TO BRANCH OUT OF THE INNER LOOP
!            WITHOUT INCREMENTING THE LOOP COUNTER.  THE VALUE OF THE    
!            LOOP COUNTER SHOULD AGAIN BE TEN (10).                      
!                                                                        
!                                                                        
      if (iczero) 31200, 1200, 31200                                    
 1200 continue                                                          
      ivon01=0                                                          
      do i=1,10,1                                                  
      do j=1,2,1                                                   
      if ( j - 1 )  1203, 1203, 1202                                    
 1202 ivon01 = ivon01 + 1                                               
      end do
 1203 ivon01 = ivon01 + 1                                               
      end do
      goto 41200                                                       
31200 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41200, 1211, 41200                                    
41200 if ( ivon01 - 10 )  21200, 11200, 21200                           
11200 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1211                                                        
21200 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=10                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1211 continue                                                          
      ivtnum = 121                                                      
!                                                                        
!      TEST 121  -  THIS IS A TEST OF DO NESTS WITHIN DO NESTS.  THE     
!            LOOP COUNTER SHOULD HAVE A FINAL VALUE OF EIGHTY-FOUR (84). 
!                                                                        
!                                                                        
      if (iczero) 31210, 1210, 31210                                    
 1210 continue                                                          
      ivon01=0                                                          
      do i1=1,2,1                                                  
      do i2=1,3,1                                                  
      do i3=1,4,1                                                  
      ivon01=ivon01+1                                                   
  end do
  end do
      do i4=1,5,1                                                  
      do i5=1,6,1                                                  
      ivon01=ivon01+1                                                   
  end do
  end do
  end do
      goto 41210                                                       
31210 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41210, 1221, 41210                                    
41210 if ( ivon01 - 84 )  21210, 11210, 21210                           
11210 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1221                                                        
21210 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=84                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1221 continue                                                          
      ivtnum = 122                                                      
!                                                                        
!      TEST 122  -  THIS IS AGAIN A TEST OF DO NESTS BUT COMBINED WITH   
!            ARITHMETIC IF STATEMENT BRANCHES WITHIN THE DO RANGE.  THE  
!            FINAL LOOP COUNTER VALUE SHOULD BE EIGHTEEN (18).           
!                                                                        
!                                                                        
      if (iczero) 31220, 1220, 31220                                    
 1220 continue                                                          
      ivon01=0                                                          
      do i1=1,3,1                                                  
      do i2=1,4,1                                                  
      if ( i2 - 3 )  1222, 1224, 1224                                   
 1222 ivon01 = ivon01 + 1                                               
  end do
 1224 do i3=1,5,1                                                  
      if ( i3 - 3 )  1225, 1225, 1227                                   
 1225 ivon01 = ivon01 + 1                                               
  end do
 1227 continue                                                          
  end do
      goto 41220                                                       
31220 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41220, 1231, 41220                                    
41220 if ( ivon01 - 15 )  21220, 11220, 21220                           
11220 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1231                                                        
21220 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=15                                                         
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1231 continue                                                          
      ivtnum = 124                                                      
!                                                                        
!      TEST 124  -  THIS IS A TEST OF A TRIPLE NESTED DO RANGE WITH      
!            AN UNCONDITIONAL GO TO STATEMENT BRANCH IN THE INNERMOST    
!            NESTED DO TO THE COMMON TERMINAL STATEMENT.  THE FINAL      
!            LOOP COUNTER VALUE SHOULD BE ONE HUNDRED AND FORTY-TWO (142)
!            THE INITIAL VALUE OF THE INNERMOST DO RANGE IS TWO (2).     
!                                                                        
!                                                                        
      if (iczero) 31240, 1240, 31240                                    
 1240 continue                                                          
      ivon01=0                                                          
      do i2=1,5,1                                                  
      do i3=2,8,1                                                  
      do i1=1,4,1                                                  
      ivon01=ivon01+1                                                   
      goto 1242                                                        
 1242 continue                                                          
      end do
      end do
      end do
      goto 41240                                                       
31240 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41240, 1251, 41240                                    
41240 if ( ivon01 - 140 )  21240, 11240, 21240                          
11240 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1251                                                        
21240 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=140                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1251 continue                                                          
      ivtnum = 125                                                      
!                                                                        
!      TEST 125  -  THIS IS BASICALLY THE SAME AS TEST 124 EXCEPT THAT   
!            AN ARITHMETIC IF BRANCH IS USED INSTEAD OF THE GO TO        
!            STATEMENT FOR THE BRANCH TO THE TERMINAL STATEMENT COMMON   
!            TO ALL THREE OF THE DO RANGES.                              
!            THE FINAL VALUE OF THE LOOP COUNTER SHOULD BE ONE           
!            HUNDRED AND FORTY (140).                                    
!                                                                        
!                                                                        
      if (iczero) 31250, 1250, 31250                                    
 1250 continue                                                          
      ivon01=0                                                          
      do i1=1,4,1                                                  
      do i2=1,5,1                                                  
      do i3=2,8,1                                                  
      ivon01=ivon01+1                                                   
      if ( i3 - 9 ) 1252, 1252, 1253                                    
 1252 end do
      end do
      end do
 1253 continue                                                          
      goto 41250                                                       
31250 ivdele = ivdele + 1                                               
      write (i02,80003) ivtnum                                          
      if (iczero) 41250, 1261, 41250                                    
41250 if ( ivon01 - 140 )  21250, 11250, 21250                          
11250 ivpass = ivpass + 1                                               
      write (i02,80001) ivtnum                                          
      goto 1261                                                        
21250 ivfail = ivfail + 1                                               
      ivcomp=ivon01                                                     
      ivcorr=140                                                        
      write (i02,80004) ivtnum, ivcomp ,ivcorr                          
 1261 continue                                                          
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
90007 format (" ",20x,"END OF PROGRAM FM012" )                          
      end program fm012
