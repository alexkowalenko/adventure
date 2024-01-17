! Subroutines for Adventure

MODULE Subroutines

    implicit none

    private

    public :: SPEAK, GET_INPUT, YES
    public :: random

CONTAINS

    SUBROUTINE SPEAK(index)
        implicit none
        integer, intent(in) :: index

        ! Globals
        include 'common.f90'

        integer :: K

        K = RTEXT(index)
        IF (K == 0) RETURN
        DO
            PRINT '(A)', text_lines(K)
            K = K+1
            IF (text_lines_index(K-1) == 0) EXIT
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
1       FORMAT(A)

        ! Convert to uppcase
        DO  J=1,LEN(LINE)
            c = LINE(J:J)
            IF (c >= 'a' .AND. c <= 'z') LINE(J:J) = CHAR(ICHAR(c) - lower_a + upper_a)
        END DO

        ! Separate out two words if present
        two_words = .FALSE.

        word1=LINE(1:5)
        D=LINE(6:10)
        J=INDEX(word1,' ')
        IF (J /= 0) word1(J:) = ' '
        J = INDEX(D,' ')
        IF (J /= 0) D(J:) = ' '
        J = INDEX(LINE,' ')
        IF (J > 2) word2 = LINE(J+1:)
        IF (J > 2 .AND. word2 /= ' ') two_words = .TRUE.
    END SUBROUTINE GET_INPUT


    SUBROUTINE YES(msg, yes_msg, no_msg, yesno)
        implicit none
        integer, intent(in) :: msg, yes_msg, no_msg
        logical, intent(out) :: yesno

        logical :: junk
        character(len=5) :: word1, junc

        yesno = .false.
        call SPEAK(msg)
        call GET_INPUT(junk, word1, junc, junc)
        if (word1 == 'NO' .OR. word1 == 'N') then
            if (no_msg /= 0) CALL SPEAK(no_msg)
            return
        else
            yesno = .true.
            if (yes_msg /= 0) CALL SPEAK(yes_msg)
            return
        end if
    END SUBROUTINE YES


    real function random()
        implicit none

        real, save :: r
        call random_number(r)
        random = r
    end function random

END MODULE Subroutines
