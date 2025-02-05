! ADVENTURES
PROGRAM ADVENTURE

    USE Subroutines

    IMPLICIT NONE

    ! Globals
    include 'common.f90'

    ! Vocab tables
    integer, parameter :: MAX_WORDS = 1000
    integer :: vocab_key(MAX_WORDS) ! Key words types
    character(len=5) :: vocabulary(MAX_WORDS) ! Key words synonyms, multiple text words map to the same key

    integer :: JSPKT(16), IPLT(20), IFIXT(20)

    integer :: IOBJ(300),ICHAIN(100),IPLACE(100),IFIXED(100),COND(300),PROP(100),ABB(300),LTEXT(300),STEXT(300), &
        KEY(300),TRAVEL(1000),TK(25),BTEXT(200),DTRAV(20)

    integer, parameter :: MAX_OBJECTS = 10
    integer :: DSEEN(MAX_OBJECTS), DLOC(MAX_OBJECTS), ODLOC(MAX_OBJECTS)

    logical :: print_location = .false. ! Print out location numbers

    integer :: I
    integer :: KEYS, LAMP, GRATE, ROD, BIRD,NUGGET, SNAKE, FOOD, WATER, AXE
    integer :: IDWARF, IFIRST, IWEST, ILONG, IDETAL, IDARK

    logical :: setup = .false.

    ! Intialise data
    CALL INITIALISE()

    !READ THE PARAMETERS
    IF(setup) GOTO 1
    setup = .true.
    ! MAX_OBJECTS here only
    KEYS=1
    LAMP=2
    GRATE=3
    ROD=5
    BIRD=7
    NUGGET=10
    SNAKE=11
    FOOD=19
    WATER=20
    AXE=21

    STEXT = [(0, i = 1, 300)]
    LTEXT = [(0, i = 1, 300)]
    BTEXT = [(0, i = 1, 200)]
    RTEXT = [(0, i = 1, 100)]

    ! Read data file
    CALL READ_DATAFILE()

1   CALL PLAY_GAME()

CONTAINS

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE INITIALISE()
        implicit none

        ! Input strings
        character(len=5) :: argument

        JSPKT = [24,29,0,31,0,31,38,38,42,42,43,46,77,71,73,75]
        IPLT = [3,3,8,10,11,14,13,9,15,18,19,17,27,28,29,30,0,0,3,3]
        IFIXT = [0,0,1,0,0,1,0,1,1,0,1,1,0,0,0,0,0,0,0,0]
        DTRAV = [36,28,19,30,62,60,41,27,17,15,19,28,36,300,300,0,0,0,0,0]

        KEY = [(0, i = 1, 300)]
        ! Objects
        DSEEN = [(0, i = 1,MAX_OBJECTS)]
        DLOC = [(0, i = 1, MAX_OBJECTS)]
        ODLOC = [(0, i = 1, MAX_OBJECTS)]

        ! PARSE COMMAND LINE ARGS
        DO I = 1, command_argument_count()
            CALL GETARG(I, argument)
            IF (argument == '-LOC' .OR. argument == '-loc') print_location = .TRUE.
        END DO
    END SUBROUTINE INITIALISE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE READ_DATAFILE()
        implicit none

        integer :: IOStatus, IKIND, JKIND, LKIND
        integer :: keywords_index, L

        character(len=4) :: CHRS =  '    '
        integer :: BLNK
        EQUIVALENCE (BLNK, CHRS)

        I = 1
        OPEN(1, FILE='ADVENTURE.DAT', ACTION="READ", IOSTAT=IOStatus)
        IF (IOStatus /= 0) STOP 'BAD DATA FILE'

        ! Main loop to read file
        DO
            READ(1, '(I9)') IKIND

            SELECT CASE (IKIND)

              CASE (1,2,5,6)
                ! Read text, type = 1, 2, 5, 6

1004            READ(1,'(I9,A80)') JKIND, text_lines(I)
                IF (JKIND == -1) CYCLE

                text_lines_index(I) = 0
                IF (IKIND .EQ. 6) GOTO 1023
                IF (IKIND .EQ. 5) GOTO 1011
                IF (IKIND .EQ. 1) GOTO 1008
                IF (STEXT(JKIND) .NE. 0) GOTO 1009
                STEXT(JKIND)=I
                GOTO 1010

                ! Full text descriptions, type 1
1008            IF (LTEXT(JKIND).NE.0) GOTO 1009
                LTEXT(JKIND)=I
                GOTO 1010
1009            text_lines_index(I-1)=I
1010            I=I+1
                IF (I /= MAX_TEXT) GOTO 1004
                STOP 'TOO MANY LINES'

1011            IF (JKIND.LT.200) GOTO 1012
                IF (BTEXT(JKIND-100) .NE. 0) GOTO 1009
                BTEXT(JKIND-100)=I
                BTEXT(JKIND-200)=I
                GOTO 1010
1012            IF (BTEXT(JKIND) .NE. 0) GOTO 1009
                BTEXT(JKIND)=I
                GOTO 1010

1023            IF (RTEXT(JKIND) .NE. 0) GOTO 1009
                RTEXT(JKIND)=I
                GOTO 1010

              CASE (3)
                ! Read type 3

                I=1
1014            READ(1,1015) JKIND,LKIND,(TK(L),L=1,10)
                !1015  FORMAT(12G)
1015            FORMAT(12I10)
                IF(JKIND .EQ. -1) CYCLE
                IF(KEY(JKIND) .NE. 0) GOTO 1016
                KEY(JKIND)=I
                GOTO 1017
1016            TRAVEL(I-1)=-TRAVEL(I-1)
1017            DO L=1,10
                    IF(TK(L) .EQ. 0) GOTO 1019
                    TRAVEL(I)=LKIND*1024+TK(L)
                    I=I+1
                    IF(I .EQ. 1000) STOP
                END DO
1019            TRAVEL(I-1)=-TRAVEL(I-1)
                GOTO 1014

              CASE(4)
                ! Read vocab, type 4
                ! vocab_key word key (integer)
                ! vocabulary = word synonyms (character*5)
                DO keywords_index = 1, MAX_WORDS
                    READ(1,1021) vocab_key(keywords_index), vocabulary(keywords_index)
1021                FORMAT(I9,A5)
                    IF (vocab_key(keywords_index) == -1) EXIT
                END DO
                IF (keywords_index >= MAX_WORDS)  STOP 'TOO MANY WORDS'
                CYCLE

              CASE DEFAULT
                EXIT
            END SELECT
        END DO

        CLOSE(1)
        RETURN
    END SUBROUTINE READ_DATAFILE

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE INITIALISE2()
        implicit none

        integer :: KTEM

        IPLACE = [(0, i=1,100)]
        IFIXED = [(0, i=1,100)]
        ICHAIN = [(0, i=1,100)]

        DO I=1,20
            IPLACE(I) = IPLT(I)
            IFIXED(I) = IFIXT(I)
        END DO

        COND = [(0, i=1,300)]
        ABB = [(0, i=1,300)]
        IOBJ = [(0, i=1,300)]

        DO I=1,10
            COND(I) = 1
        END DO
        COND(16) = 2
        COND(20) = 2
        COND(21) = 2
        COND(22) = 2
        COND(23) = 2
        COND(24) = 2
        COND(25) = 2
        COND(26) = 2
        COND(31) = 2
        COND(32) = 2
        COND(79) = 2

        DO I=1,100
            KTEM = IPLACE(I)
            IF (KTEM == 0) GOTO 1107
            IF (IOBJ(KTEM) /= 0) GOTO 1104
            IOBJ(KTEM) = I
            GOTO 1107
1104        KTEM = IOBJ(KTEM)
1105        IF (ICHAIN(KTEM) /= 0) GOTO 1106
            ICHAIN(KTEM) = I
            GOTO 1107
1106        KTEM = ICHAIN(KTEM)
            GOTO 1105
1107    END DO

        IDWARF = 0
        IFIRST = 1
        IWEST = 0
        ILONG = 1
        IDETAL = 0
    END SUBROUTINE INITIALISE2

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE PLAY_GAME()
        implicit NONE

        ! Input strings
        character(len=5) :: word1
        character(len=5) :: B, word2

        integer :: J, K, KK, KQ, L, LL, LOLD, IL, ILK, TEMP, ITEMP
        integer :: location
        integer :: ATTACK, number_dwarfs, STICK, JSPK, verb_index
        integer :: LTRUBL, JOBJ
        integer :: ID, IID

        logical :: two_words, yesno

1100    CALL INITIALISE2()
        PRINT *,'INIT DONE'
        CALL random_init(.TRUE., .TRUE.)

        ! Start
        ! Do you want instructions?
        CALL YES(65,1,0,yesno)

        ! First room
        L=1
        location = 1
2       DO I=1,3
            IF (ODLOC(I) .NE. L .OR. DSEEN(I) .EQ. 0) EXIT
            L = location
            CALL SPEAK(2)
            EXIT
        END DO
        location = L

        ! Debug location
        IF (print_location) PRINT '(A,1x,i2)','location:',location

        ! DWARF STUFF
        IF (IDWARF .NE. 0) GOTO 60
        IF (location .EQ. 15) IDWARF=1
        GOTO 71
60      IF (IDWARF .NE. 1)GOTO 63
        IF (random() .GT. 0.05) GOTO 71
        IDWARF=2
        DO I=1,3
            DLOC(I)=0
            ODLOC(I)=0
            DSEEN(I)=0
        END DO
        CALL SPEAK(3)
        ICHAIN(AXE)=IOBJ(location)
        IOBJ(location)=AXE
        IPLACE(AXE)=location
        GOTO 71

63      IDWARF=IDWARF+1
        ATTACK=0
        number_dwarfs = 0
        STICK=0
        DO I=1,3
            IF (2*I+IDWARF .LE. 8) GOTO 66
            IF (2*I+IDWARF .GT. 23 .AND. DSEEN(I) .EQ. 0) GOTO 66
            ODLOC(I)=DLOC(I)
            IF (DSEEN(I) .NE. 0 .AND. location .GT. 14) GOTO 65
            DLOC(I)=DTRAV(I*2+IDWARF-8)
            DSEEN(I)=0
            IF (DLOC(I) .NE. location .AND. ODLOC(I) .NE. location) GOTO 66
65          DSEEN(I)=1
            DLOC(I)=location
            number_dwarfs = number_dwarfs + 1
            IF (ODLOC(I) .NE. DLOC(I)) GOTO 66
            ATTACK=ATTACK+1
            IF (random() .LT. 0.1) STICK=STICK+1
66      END DO

        IF (number_dwarfs .EQ. 0) GOTO 71
        IF (number_dwarfs .EQ. 1) THEN
            CALL SPEAK(4) ! THERE IS A THREATENING LITTLE DWARF IN THE ROOM WITH YOU!
        ELSE
            PRINT 67,number_dwarfs
67          FORMAT(' THERE ARE ',I2,' THREATENING LITTLE DWARVES IN THE ROOM WITH YOU.',/)
        END IF

        IF (ATTACK .EQ. 0) GOTO 71
        IF (ATTACK .EQ. 1) GOTO 79
        PRINT 78,ATTACK
78      FORMAT(' ',I2,' OF THEM THROW KNIVES AT YOU!',/)
        GOTO 81
79      CALL SPEAK(5) ! ONE SHARP NASTY KNIFE IS THROWN AT YOU!
        CALL SPEAK(52+STICK) !IT MISSES, IT GETS YOU!
        GOTO(71,83)(STICK+1)

81      IF (STICK .EQ. 0) GOTO 69
        IF (STICK .EQ. 1) GOTO 82
        PRINT 68,STICK
68      FORMAT(' ',I2,' OF THEM GET YOU.',/)
        GOTO 83
82      CALL SPEAK(6) ! HE GETS YOU!
        !83    PAUSE 'GAMES OVER'
        !     GOTO 71
83      CONTINUE
        GOTO 31
69      CALL SPEAK(7) ! NONE OF THEM HIT YOU!

        ! PLACE DESCRIPTOR

71      KK=STEXT(L)
        IF (ABB(L) .EQ. 0 .OR. KK .EQ. 0) KK=LTEXT(L)
        IF (KK .EQ. 0) GOTO 7
        DO
            PRINT '(A)', text_lines(KK)
            KK=KK+1
            IF (text_lines_index(KK-1) == 0) EXIT
        END DO
        PRINT '(/)'
7       IF (COND(L).EQ.2) GOTO 8
        IF (location.EQ.33.AND.random().LT.0.25) CALL SPEAK(8)
        J=L
        GOTO 2000

        ! GO GET A NEW LOCATION

8       KK=KEY(location)
        IF (KK .EQ. 0) GOTO 19
        IF (K .EQ. 57) GOTO 32
        IF (K .EQ. 67) GOTO 40
        IF (K .EQ. 8) GOTO 12
        LOLD=L
9       LL=TRAVEL(KK)
        IF (LL .LT. 0) LL=-LL
        IF (1 .EQ. MOD(LL,1024)) GOTO 10
        IF (K .EQ. MOD(LL,1024)) GOTO 10
        IF (TRAVEL(KK).LT.0) GOTO 11
        KK=KK+1
        GOTO 9
12      TEMP=LOLD
        LOLD=L
        L=TEMP
        GOTO 21
10      L=LL/1024
        GOTO 21
11      JSPK=12
        IF (K .GE. 43 .AND. K .LE. 46) JSPK=9
        IF (K .EQ. 29 .OR. K .EQ. 30) JSPK=9
        IF (K .EQ. 7 .OR. K .EQ. 8 .OR. K .EQ. 36 .OR. K .EQ. 37 .OR. K .EQ. 68) JSPK=10
        IF (K .EQ. 11 .OR. K .EQ. 19) JSPK=11
        IF (verb_index .EQ. 1) JSPK=59
        IF (K .EQ. 48) JSPK=42
        IF (K .EQ. 17) JSPK=80
        CALL SPEAK(JSPK)
        GOTO 2
19      CALL SPEAK(13)
        L=location
        IF (IFIRST .EQ. 0) CALL SPEAK(14)
21      IF (L .LT. 300) GOTO 2
        IL=L-300+1
        !     GOTO(22,23,24,25,26,31,27,28,29,30,33,34,36,37)IL
        GOTO(22,23,24,25,26,31,27,28,29,30,33,34,36,37,39) IL
        PRINT '(a,1x,i2,1x,a,1x,i3,a)','**No special motion',IL-1,'from',location,'- hope this is OK...'
        GOTO 2

22      L=6
        IF (random() .GT. 0.5) L=5
        GOTO 2

23      L=23
        IF (PROP(GRATE) .NE. 0) L=9
        GOTO 2

24      L=9
        IF (PROP(GRATE) .NE. 0) L=8
        GOTO 2

25      L=20
        IF (IPLACE(NUGGET) .NE. -1) L=15
        GOTO 2

26      L=22
        IF (IPLACE(NUGGET) .NE. -1) L=14
        GOTO 2

27      L=27
        IF (PROP(12) .EQ. 0) L=31
        GOTO 2

28      L=28
        IF (PROP(SNAKE) .EQ. 0) L=32
        GOTO 2

29      L=29
        IF (PROP(SNAKE) .EQ. 0) L=32
        GOTO 2

30      L=30
        IF (PROP(SNAKE) .EQ. 0) L=32
        GOTO 2

        ! Finish Game
31      CALL YES(81,54,0, yesno)
        IF (.not. yesno) STOP ! End of game
        GOTO 1100 ! Restart

32      IF (IDETAL .LT. 3) CALL SPEAK(15) !SORRY, BUT I AM NOT ALLOWED TO GIVE MORE DETAIL.
        IDETAL=IDETAL+1
        L=location
        ABB(L)=0
        GOTO 2

33      L=8
        IF (PROP(GRATE) .EQ. 0) L=9
        GOTO 2

34      IF (random() .GT. 0.2) GOTO 35
        L=68
        GOTO 2

35      L=65
38      CALL SPEAK(56)
        GOTO 2

36      IF (random() .GT. 0.2) GOTO 35
        L=39
        IF(random() .GT. 0.5) L=70
        GOTO 2

37      L=66
        IF (random() .GT. 0.4) GOTO 38
        L=71
        IF (random() .GT. 0.25) L=72
        GOTO 2

39      L=66
        IF (random() .GT. 0.2)GOTO 38
        L=77
        GOTO 2

40      IF (location .LT. 8) CALL SPEAK(57)
        IF (location .GE. 8) CALL SPEAK(58)
        L=location
        GOTO 2

        ! DO NEXT INPUT

2000    LTRUBL=0
        location=J
        ABB(J)=MOD((ABB(J)+1),5)
        IDARK=0
        IF (MOD(COND(J),2) .EQ. 1)  GOTO 2003
        IF ((IPLACE(2) .NE. J) .AND. (IPLACE(2) .NE. -1)) GOTO 2001
        IF (PROP(2) .EQ. 1) GOTO 2003
2001    CALL SPEAK(16)
        IDARK=1


2003    I=IOBJ(J)
2004    IF (I .EQ. 0) GOTO 2011
        IF (((I .EQ. 6) .OR. (I .EQ. 9)) .AND. (IPLACE(10) .EQ. -1)) GOTO 2008
        ILK=I
        IF (PROP(I) .NE. 0) ILK=I+100
        KK=BTEXT(ILK)
        IF (KK .EQ. 0) GOTO 2008
2005    PRINT '(A)', text_lines(KK)
        KK=KK+1
        IF (text_lines_index(KK-1) .NE. 0) GOTO 2005
        !     TYPE 2007
        PRINT '(/)'
2008    I=ICHAIN(I)
        GOTO 2004


        ! K=1 MEANS ANY INPUT


2012    word1 = word2
        B = ' '
        two_words = .false.
        GOTO 2021

2009    K=54
2010    JSPK=K

        ! Response
5200    CALL SPEAK(JSPK)

2011    verb_index=0
        JOBJ=0
        two_words = .false.

        ! Loop for input
2020    CALL GET_INPUT(two_words, word1, word2, B)
        K=70
        IF (word1 .EQ.'ENTER' .AND. (word2 .EQ. 'STREA' .OR. word2 .EQ. 'WATER')) GOTO 2010
        IF (word1 .EQ.'ENTER' .AND. two_words .NEQV. .false.) GOTO 2012
2021    IF (word1 .NE.'WEST') GOTO 2023
        IWEST=IWEST+1
        IF (IWEST .NE. 10) GOTO 2023
        CALL SPEAK(17)

        ! Search for word
2023    DO I = 1, MAX_WORDS
            IF (vocab_key(I) == -1) GOTO 3000 ! Unknown word
            IF (vocabulary(I) == word1) EXIT ! Found it
        END DO

        K = MOD(vocab_key(I), MAX_WORDS)
        KQ = vocab_key(I) / MAX_WORDS + 1
        GOTO (5014,5000,2026,2010) KQ
        STOP 'NO NO'
2026    verb_index=K
        JSPK=JSPKT(verb_index)
        IF (two_words .NEQV. .false.) GOTO 2028
        IF (JOBJ .EQ. 0) GOTO 2036
2027    GOTO(9000,5066,3000,5031,2009,5031,9404,9406,5081,5200,5200,5300,5506,5502,5504,5505)verb_index
        STOP 'ERROR 5'


2028    word1 = word2
        B=' '
        two_words = .false.
        GOTO 2023

        ! Unknown word
3000    JSPK=60 !I DON'T KNOW THAT WORD.
        IF (random() .GT. 0.8) JSPK=61 ! WHAT?
        IF (random() .GT. 0.8) JSPK=13 ! I DON'T UNDERSTAND THAT!
        CALL SPEAK(JSPK)

        LTRUBL=LTRUBL+1
        IF (LTRUBL .NE. 3) GOTO 2020
        IF (J .NE. 13 .OR. IPLACE(7) .NE. 13 .OR. IPLACE(5) .NE. -1) GOTO 2032
        CALL YES(18,19,54, yesno)
        GOTO 2033
2032    IF (J .NE. 19 .OR. PROP(11) .NE. 0 .OR. IPLACE(7) .EQ. -1) GOTO 2034
        CALL YES(20,21,54, yesno)
        GOTO 2033
2034    IF (J .NE. 8 .OR. PROP(GRATE) .NE. 0) GOTO 2035
        CALL YES(62,63,54, yesno)
2033    IF (.not. yesno) GOTO 2011
        GOTO 2020
2035    IF (IPLACE(5) .NE. J .AND. IPLACE(5) .NE. -1) GOTO 2020
        IF (JOBJ .NE. 5) GOTO 2020
        CALL SPEAK(22)
        GOTO 2020


2036    GOTO(2037,5062,5062,9403,2009,9403,9404,9406,5062,5062,5200,5300,5062,5062,5062,5062)verb_index
        STOP 'OOPS'
2037    IF ((IOBJ(J).EQ.0).OR.(ICHAIN(IOBJ(J)).NE.0)) GOTO 5062
        DO I=1,3
            IF (DSEEN(I).NE.0) GOTO 5062
        END DO
        JOBJ=IOBJ(J)
        GOTO 2027
5062    IF (B .NE. ' ') GOTO 5333
        PRINT 5063, word1
5063    FORMAT('  ',A5,' WHAT?',/)
        GOTO 2020

5333    PRINT 5334, word1, B
5334    FORMAT(' ',2A5,' WHAT?',/)
        GOTO 2020
5014    IF (IDARK.EQ.0) GOTO 8

        IF (random().GT.0.25) GOTO 8
        CALL SPEAK(23)
        GOTO 31
        !     PAUSE 'GAME IS OVER'
        !     GOTO 2011



5000    JOBJ=K
        IF (two_words .NEQV. .false.) GOTO 2028
        IF ((J .EQ. IPLACE(K)) .OR. (IPLACE(K) .EQ. -1)) GOTO 5004
        IF (K .NE. GRATE) GOTO 502
        IF ((J .EQ. 1) .OR. (J .EQ. 4) .OR. (J .EQ. 7)) GOTO 5098
        IF ((J .GT. 9) .AND. (J .LT. 15)) GOTO 5097
502     IF (B .NE.' ') GOTO 5316
        PRINT 5005, word1
5005    FORMAT(' I SEE NO ',A5,' HERE.',/)
        GOTO 2011
5316    PRINT 5317, word1, B
5317    FORMAT(' I SEE NO ',2A5,' HERE.'/)
        GOTO 2011
5098    K=49
        GOTO 5014
5097    K=50
        GOTO 5014
5004    JOBJ=K
        IF (verb_index .NE. 0) GOTO 2027


        IF(B .NE. ' ') GOTO 5314
        PRINT 5001, word1
5001    FORMAT(' WHAT DO YOU WANT TO DO WITH THE ',A5,'?',/)
        GOTO 2020
5314    PRINT 5315, word1, B
5315    FORMAT(' WHAT DO YOU WANT TO DO WITH THE ',2A5,'?',/)
        GOTO 2020

        ! CARRY
9000    IF (JOBJ .EQ. 18) GOTO 2009
        IF (IPLACE(JOBJ) .NE. J) GOTO 5200
        IF (IFIXED(JOBJ) .EQ. 0) GOTO 9002
        CALL SPEAK(25)
        GOTO 2011
9002    IF (JOBJ .NE. BIRD) GOTO 9004
        IF (IPLACE(ROD) .NE. -1) GOTO 9003
        CALL SPEAK(26)
        GOTO 2011
9003    IF ((IPLACE(4) .EQ. -1) .OR. (IPLACE(4) .EQ. J)) GOTO 9004
        CALL SPEAK(27)
        GOTO 2011
9004    IPLACE(JOBJ)=-1
9005    IF (IOBJ(J) .NE. JOBJ) GOTO 9006
        IOBJ(J)=ICHAIN(JOBJ)
        GOTO 2009
9006    ITEMP=IOBJ(J)
9007    IF (ICHAIN(ITEMP) .EQ. (JOBJ)) GOTO 9008
        ITEMP=ICHAIN(ITEMP)
        GOTO 9007
9008    ICHAIN(ITEMP)=ICHAIN(JOBJ)
        GOTO 2009


        ! LOCK, UNLOCK, NO OBJECT YET
9403    IF ((J .EQ. 8) .OR. (J .EQ. 9)) GOTO 5105
        CALL SPEAK(28)
        GOTO 2011
5105    JOBJ=GRATE
        GOTO 2027

        ! DISCARD OBJECT
5066    IF (JOBJ .EQ. 18) GOTO 2009
        IF (IPLACE(JOBJ) .NE. -1) GOTO 5200
        IF ((JOBJ .NE. BIRD) .OR. (J .NE. 19) .OR. (PROP(11) .EQ. 1)) GOTO 9401
        CALL SPEAK(30)
        PROP(11)=1
5160    ICHAIN(JOBJ)=IOBJ(J)
        IOBJ(J)=JOBJ
        IPLACE(JOBJ)=J
        GOTO 2011

9401    CALL SPEAK(54)
        GOTO 5160

        ! LOCK,UNLOCK OBJECT
5031    IF(IPLACE(KEYS) .NE. -1 .AND. IPLACE(KEYS) .NE. J) GOTO 5200
        IF(JOBJ.NE.4) GOTO 5102
        CALL SPEAK(32)
        GOTO 2011
5102    IF(JOBJ .NE. KEYS) GOTO 5104
        CALL SPEAK(55)
        GOTO 2011
5104    IF(JOBJ .EQ. GRATE) GOTO 5107
        CALL SPEAK(33)
        GOTO 2011
5107    IF(verb_index .EQ. 4) GOTO 5033
        IF(PROP(GRATE) .NE. 0) GOTO 5034
        CALL SPEAK(34)
        GOTO 2011
5034    CALL SPEAK(35)
        PROP(GRATE)=0
        PROP(8)=0
        GOTO 2011
5033    IF(PROP(GRATE) .EQ. 0) GOTO 5109
        CALL SPEAK(36)
        GOTO 2011
5109    CALL SPEAK(37)
        PROP(GRATE)=1
        PROP(8)=1
        GOTO 2011

        ! LIGHT LAMP
9404    IF((IPLACE(2) .NE. J) .AND. (IPLACE(2) .NE. -1)) GOTO 5200
        PROP(2)=1
        IDARK=0
        CALL SPEAK(39)
        GOTO 2011

        ! LAMP OFF
9406    IF((IPLACE(2) .NE. J) .AND. (IPLACE(2) .NE. -1)) GOTO 5200
        PROP(2)=0
        CALL SPEAK(40)
        GOTO 2011

        ! STRIKE
5081    IF(JOBJ .NE. 12) GOTO 5200
        PROP(12)=1
        GOTO 2003

        ! ATTACK
5300    DO ID=1,3
            IID=ID
            IF (DSEEN(ID).NE.0) GOTO 5307
        END DO
        IF (JOBJ .EQ. 0) GOTO 5062
        IF (JOBJ .EQ. SNAKE) GOTO 5200
        IF (JOBJ .EQ. BIRD) GOTO 5302
        CALL SPEAK(44) ! THERE IS NOTHING HERE TO ATTACK.
        GOTO 2011
5302    CALL SPEAK(45) ! THE LITTLE BIRD IS NOW DEAD. ITS BODY DISAPPEARS.
        IPLACE(JOBJ)=300
        GOTO 9005

5307    IF (random() .GT. 0.4) GOTO 5309
        DSEEN(IID)=0
        ODLOC(IID)=0
        DLOC(IID)=0
        CALL SPEAK(47) ! YOU KILLED A LITTLE DWARF.
        GOTO 5311
5309    CALL SPEAK(48) ! YOU ATTACK A LITTLE DWARF, BUT HE DODGES OUT OF THE WAY.
5311    K=21
        GOTO 5014

        ! EAT
5502    IF ((IPLACE(FOOD) .NE. J .AND. IPLACE(FOOD) .NE. -1) .OR. PROP(FOOD) .NE. 0 .OR. JOBJ .NE. FOOD) GOTO 5200
        PROP(FOOD)=1
        JSPK=72 ! EATEN!
        GOTO 5200

        ! DRINK
5504    IF ((IPLACE(WATER) .NE. J .AND. IPLACE(WATER) .NE. -1) .OR. PROP(WATER) .NE. 0 .OR. JOBJ .NE. WATER) GOTO 5200
        PROP(WATER)=1
        JSPK=74 !THE BOTTLE OF WATER IS NOW EMPTY.
        GOTO 5200

        ! RUB
5505    IF (JOBJ /= LAMP) JSPK=76 !PECULIAR.  NOTHING UNEXPECTED HAPPENS.
        GOTO 5200

        ! POUR
5506    IF (JOBJ /= WATER) JSPK=78 !YOU CAN'T POUR THAT.
        PROP(WATER)=1
        GOTO 5200

    END SUBROUTINE PLAY_GAME

END PROGRAM ADVENTURE
