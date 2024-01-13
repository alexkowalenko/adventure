! Subroutines for Adventure


SUBROUTINE SPEAK(IT)
   IMPLICIT INTEGER(A-Z)
   integer, intent(in) :: IT

   COMMON RTEXT,LLINE
   DIMENSION RTEXT(100),LLINE(1000,22)

   KKT=RTEXT(IT)
   IF(KKT.EQ.0)RETURN
999 PRINT 998, (LLINE(KKT,JJT),JJT=3,LLINE(KKT,2))
998 FORMAT(20A4)
   KKT=KKT+1
   IF(LLINE(KKT-1,1).NE.0) GOTO 999
   PRINT 996
996 FORMAT(/)
   RETURN
END


SUBROUTINE GETIN(TWOW,B,C,D)
   IMPLICIT INTEGER(A-Z)
   integer, intent(out):: TWOW
   CHARACTER, intent(out):: B*5,C*5,D*5

   CHARACTER LINE*80
   DIMENSION A(5)

   READ(*,1) LINE
1  FORMAT(A)
   DO  J=1,LEN(LINE)
      IF(LINE(J:J).GE.'a'.AND.LINE(J:J).LE.'z')LINE(J:J)=CHAR(ICHAR(LINE(J:J))-ICHAR('a')+ICHAR('A'))
   END DO
   TWOW=0
!     S=0
!     B=A(1)
!     DO 2 J=1,4
!     DO 2 K=1,5
!     MASK1="774000000000
!     IF(K.NE.1) MASK1="177*M2(K)
!     IF(((A(J).XOR."201004020100).AND.MASK1).EQ.0)GOTO 3
!     IF(S.EQ.0) GOTO 2
!     TWOW=1
!     CALL SHIFT(A(J),7*(K-1),XX)
!     CALL SHIFT(A(J+1),7*(K-6),YY)
!     MASK=-M2(6-K)
!     C=(XX.AND.MASK)+(YY.AND.(-2-MASK))
!     GOTO 4
!3     IF(S.EQ.1) GOTO 2
!     S=1
!     IF(J.EQ.1) B=(B.AND.-M2(K)).OR.("201004020100.AND.
!    1 (-M2(K).XOR.-1))
!2     CONTINUE
!4     D=A(2)
   B=LINE(1:5)
   D=LINE(6:10)
   J=INDEX(B,' ')
   IF(J.NE.0) B(J:)=' '
   J=INDEX(D,' ')
   IF(J.NE.0) D(J:)=' '
   J=INDEX(LINE,' ')
   IF(J.GT.2) C=LINE(J+1:)
   IF(J.GT.2 .AND. C.NE.' ') TWOW=1
   RETURN
END

SUBROUTINE YES(X,Y,Z,YEA)
   IMPLICIT NONE
   integer, intent(in) :: X, Y, Z
   integer, intent(in out) :: YEA

   INTEGER :: JUNK
   CHARACTER*5 IA1,IB1
   CHARACTER*5 JUNC

   CALL SPEAK(X)
   CALL GETIN(JUNK,IA1,JUNC,IB1)
   IF(IA1.EQ.'NO'.OR.IA1.EQ.'N') GOTO 1
   YEA=1
   IF(Y.NE.0) CALL SPEAK(Y)
   RETURN
1  YEA=0
   IF(Z.NE.0) CALL SPEAK(Z)
   RETURN
END



SUBROUTINE SHIFT (VAL,DIST,RES)
!     SHIFT VAL BY |DIST| BITS RIGHT (IF DIST < 0) OR LEFT (IF DIST > 0)
   implicit none
   integer, intent(in) :: val, dist
   integer, intent(out) :: res

   integer :: MR1, ML1, ML2, MSG, IDIST, I, J

   DATA MR1 /Z'7FFFFFFF'/
   DATA ML1 /Z'40000000'/
   DATA ML2 /Z'3FFFFFFF'/
   DATA MSG /Z'80000000'/

   RES=VAL
   if( DIST .lt. 0) then
      IDIST=-DIST
      do I=1,IDIST
         J = 0
         if (RES .LT. .0) J = MSG
!11    RES = ((RES.AND."377777777777)/2) + J
         RES = IOR(IAND(RES,MR1)/2,J)
      end do
   else if (dist .eq. 0) then
      return
   else
      do I=1,DIST
         J = 0
!     IF ((RES.AND."200000000000).NE.0) J="400000000000
         IF (IAND(RES,ML1).NE.0) J=MSG
!31    RES = (RES.AND."177777777777)*2 + J
         RES = IOR(IAND(RES,ML2)*2,J)
      end do
   end if
   return
END
