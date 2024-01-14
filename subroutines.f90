! Subroutines for Adventure


SUBROUTINE SPEAK(IT)
   implicit none
   integer, intent(in) :: IT

   COMMON RTEXT,LLINE
   integer :: RTEXT(100), LLINE(1000,22)

   integer :: KKT, JJT

   KKT=RTEXT(IT)
   IF (KKT == 0) RETURN
999 PRINT 998, (LLINE(KKT,JJT),JJT=3,LLINE(KKT,2))
998 FORMAT(20A4)
   KKT=KKT+1
   IF (LLINE(KKT-1,1) /= 0) GOTO 999
   PRINT 996
996 FORMAT(/)
   RETURN
END


SUBROUTINE GETIN(two_words, word1, word2, D)
   implicit none
   logical, intent(out):: two_words
   character(len=5), intent(out) :: word1, word2, D

   character(len=100) :: LINE
   integer :: A(5)
   integer :: j

   READ(*,1) LINE
1  FORMAT(A)
   DO  J=1,LEN(LINE)
      IF (LINE(J:J) >= 'a' .AND. LINE(J:J) <= 'z') LINE(J:J)=CHAR(ICHAR(LINE(J:J))-ICHAR('a')+ICHAR('A'))
   END DO
   two_words = .false.

   word1=LINE(1:5)
   D=LINE(6:10)
   J=INDEX(word1,' ')
   IF (J /= 0) word1(J:)=' '
   J=INDEX(D,' ')
   IF (J /= 0) D(J:)=' '
   J=INDEX(LINE,' ')
   IF (J > 2) word2=LINE(J+1:)
   IF (J > 2 .AND. word2 /= ' ') two_words = .true.
   RETURN
END


SUBROUTINE YES(X,Y,Z,YEA)
   implicit none
   integer, intent(in) :: X, Y, Z
   integer, intent(in out) :: YEA

   logical :: junk
   character(len=5) :: word1, junc

   call SPEAK(X)
   call GETIN(junk, word1, junc, junc)
   if (word1 == 'NO' .OR. word1 == 'N') then
      if (Z /= 0) CALL SPEAK(Z)
      return
   else
      YEA=1
      if (Y /= 0) CALL SPEAK(Y)
      return
   end if
END
