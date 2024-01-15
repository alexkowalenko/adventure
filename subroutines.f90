! Subroutines for Adventure


SUBROUTINE SPEAK(index)
   implicit none
   integer, intent(in) :: index

   COMMON RTEXT,LLINE
   integer :: RTEXT(100), LLINE(1000,22)

   integer :: K, J

   K = RTEXT(index)
   IF (K == 0) RETURN
   DO
      PRINT '(20A4)', (LLINE(K,J), J=3, LLINE(K,2))
      K = K+1
      IF (LLINE(K-1,1) == 0) EXIT
   END DO
   PRINT '(/)'
END SUBROUTINE SPEAK


SUBROUTINE GET_INPUT(two_words, word1, word2, D)
   implicit none
   logical, intent(out):: two_words
   character(len=5), intent(out) :: word1, word2, D

   integer, parameter :: lower_a =  ICHAR('a')
   integer, parameter :: upper_a =  ICHAR('A')
   character :: c

   character(len=100) :: LINE
   integer :: A(5)
   integer :: j

   READ(*,1) LINE
1  FORMAT(A)

   ! Convert to uppcase
   DO  J=1,LEN(LINE)
      c = LINE(J:J)
      IF (c >= 'a' .AND. c <= 'z') LINE(J:J) = CHAR(ICHAR(c) - lower_a + upper_a)
   END DO

   ! Seperate out two words if present
   two_words = .false.

   word1=LINE(1:5)
   D=LINE(6:10)
   J=INDEX(word1,' ')
   IF (J /= 0) word1(J:) = ' '
   J = INDEX(D,' ')
   IF (J /= 0) D(J:) = ' '
   J = INDEX(LINE,' ')
   IF (J > 2) word2 = LINE(J+1:)
   IF (J > 2 .AND. word2 /= ' ') two_words = .true.
END SUBROUTINE GET_INPUT


SUBROUTINE YES(X,Y,Z, yesno)
   implicit none
   integer, intent(in) :: X, Y, Z
   logical, intent(out) :: yesno

   logical :: junk
   character(len=5) :: word1, junc

   yesno = .false.
   call SPEAK(X)
   call GET_INPUT(junk, word1, junc, junc)
   if (word1 == 'NO' .OR. word1 == 'N') then
      if (Z /= 0) CALL SPEAK(Z)
      return
   else
      yesno = .true.
      if (Y /= 0) CALL SPEAK(Y)
      return
   end if
END SUBROUTINE YES
