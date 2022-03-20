       PROGRAM MAIN
       CALL MMAIN
       WRITE (6,10)
    10 FORMAT ('CALLING RANDOM PROGRAM')
       CALL RDMAIN
       STOP
       END

!----------------------------------------------------------------------
       BLOCK DATA
       COMMON /TABB/TKSIG(257)
       REAL :: TKSIG
       DATA TKSIG   /15.0,10.0,20.0,30.0,40.0,50.0,70.0,100.0,         &
       150.0,300.0,500.0,1000.0,0.0,0.0,0.0,0.0,15.0,6.0,10.0,14.0,    &
       18.0,22.0,30.0,40.0,60.0,80.0,100.0,120.0,140.0,160.0,180.0,    &
       240.,2.954,2.69,2.515,2.295,2.162,1.972,1.82,1.622,1.514,1.413, &
       1.349,1.289,1.259,1.231,1.162,3.8,3.59,3.31,3.02,2.82,2.514,    &
       2.19,1.884,1.719,1.597,1.48,1.413,1.35,1.303,1.202,4.22,4.075,  &
       3.8,3.51,3.315,2.95,2.632,2.19,1.998,1.82,1.68,1.604,1.513,     &
       1.446,1.319,4.46,4.36,4.14,3.8,3.635,3.275,2.884,2.51,2.29,     &
       2.02,1.862,1.74,1.7,1.64,1.48,4.68,4.63,4.36,4.15,3.98,3.55,    &
       3.16,2.755,2.483,2.24,2.09,1.95,1.862,1.78,1.62,5.012,5.065,    &
       4.9,4.67,4.46,4.07,3.63,3.16,2.85,2.63,2.4,2.24,2.09,1.995,1.8, &
       5.346, 5.52,5.5,5.37,5.14,4.67,4.26,3.72,3.35,3.02,2.755,       &
       2.632,2.514,2.345,2.09,5.624,6.026,6.05,6.03,5.89,5.63,5.14,    &
       4.52,4.07,3.645,3.39,3.125,2.92,2.758,2.458,6.026,6.457,6.748,  &
       6.903,6.919,6.839,6.607,5.95,5.37,4.9,4.56,4.26,4.07,3.8,3.315, &
       6.096,6.684,6.919,7.228,7.328,7.345,7.245,6.887,6.457,5.95,     &
       5.63,5.25,5.01,4.74,4.16,6.166,6.839,7.145,7.413,7.586,7.727,   &
       7.763,7.586,7.379,7.079,6.808,6.562,6.309,6.03,5.31,60*0.0/
       END

!----------------------------------------------------------------------
      SUBROUTINE MMAIN
      COMMON X, Y
      COMMON CSUM, CYCLSM, MAXN, DY, STSMXM, STSMNM
      COMMON /TABB/TKSIG
      REAL :: YAW(40), CYBT(40),CYBT0(40)

      REAL :: X(3958), Y(3958), CDMGM(40), F(40), AM(40),             &
      DELTAY(25), DELT1(25), DELT2(25), DELT3(25), DELT4(25),         &
      DELT5(25), DELT6(25), DELY1(40), DELY11(40), DY(40,25),         &
      AMIDY(25), CDAMG(25), P(40), YMAX(25), YMIN(25),                &
      CUMM(25), T(40), TAB1(25), TAB2(25), TAB3(25),                  &
      TAB4(25), TAB5(25), TAB6(25), ARNO1(40), SGMAX1(40), ARNO2(40), &
      SGMAX2(40), ARNO3(40), SGMAX3(40), ABR(40), VELOS(40),          &
      SLOPE(40), AKSIG(40), WT(40), P1(40), AK1(40), P2(40), AK2(40), &
      STSMXM(40,25), STSMNM(40,25), CYCLSM(40,25), TABL1(25),         &
      TABL2(25), TABL3(25), TABL4(25), TABL5(25), TABL6(25),          &
      TABL7(25), TABL8(25), TABL9(25)

      REAL :: CSUM(40,25), TBLM2(434), TBLI2(1542), CYC(25), DMAGEM(25)
      REAL :: ABC(40), QMAX(1000), QMIN(1000), JX(1000), JI(1000)
      REAL :: DATAIN(13), SCLTRB(40), TKSIG(257), SIG(40), SIGULT

      REAL :: TBLSN(257), TBLLD(31), AC, CBART, AST, WAREA
      REAL :: AL1, AL2, AL3, AL4, AL5, AL6, ALIFE, AMGT, ARGUMT

      INTEGER :: M3(40), N1FLAG(40), M5(40), N(40), I4, ICASE, IEND
      INTEGER :: MAXN(40), N2(40), IA(40), K1(40), ISTRES(40), N6(40)
      INTEGER :: IRR, IW1, IW2, IW3, IW4, IW5, KEND, L1


      EQUIVALENCE (IEND, X(1)), (KEND, X(2)), (I4, X(3)),              &
      (SIGULT, X(4)), (WAREA, X(5)), (ISTRES, X(6)), (DELT1, X(46)),   &
      (TAB1, X(71)), (TAB2, X(96)), (TAB3, X(121)), (TAB4, X(146)),    &
      (TAB5, X(171)), (AC, X(196)), (IW1, X(197)), (IW2, X(198)),      &
      (IW3, X(199)), (IW4, X(200)), (IRR, X(201)), (ICASE, X(202)),    &
      (IW5, X(203)), (TAB6, X(206)), (DELT2, X(231)),                  &
      (DELY1, X(256)), (DELY11, X(296)), (ARNO1, X(336)),              &
      (ARNO2, X(376)), (ARNO3, X(416)), (SGMAX1, X(456)),              &
      (SGMAX2, X(496)), (SGMAX3, X(536)), (T, X(576)),                 &
      (AKSIG, X(616)), (SLOPE, X(656)), (VELOS, X(696)), (WT,          &
      X(736)), (P1, X(776)), (TBLM2, X(853))
      EQUIVALENCE (P2, X(1473)), (AK1, X(1513)), (AK2, X(1553)),       &
      (ABR, X(1593)), (IA, X(1633)), (M3, X(1673)), (SIG, X(1713)),    &
      (AM, X(1753)), (N, X(1793)), (NEND, X(1833)), (AL6, X(1850)),    &
      (AL5, X(1851)), (AL4, X(1852)), (AL3, X(1853)), (AL2, X(1854)),  &
      (AL1, X(1855)), (TBLI2, X(1856)), (M5, X(3398)),                 &
      (N1FLAG, X(3438)), (F, X(3478)), (N6, X(3518)), (N2, X(3558)),   &
      (P, X(3598)), (SCLTRB, X(3638)), (TABL1, X(3678)),               &
      (TABL2, X(3703)), (TABL3, X(3728)), (TABL4, X(3753)),            &
      (TABL5, X(3778)), (TABL6, X(3803)), (TABL7, X(1287)),            &
      (TABL8, X(1312)),(TABL9, X(1337)), (DELT3, X(1362)),             &
      (DELT4, X(1387))
      EQUIVALENCE (DELT5, X(1412)), (DELT6, X(1437)), (L1, X(1462))
      EQUIVALENCE (CBART, X(3837)), (AST, X(3838)), (YAW, X(3839)),    &
                  (CYBT, X(3879)), (CYBT0,X(3919))

      REAL :: JSUM, ISUM, NEND, JT, A, B, AX, CUMDMG, CUMXN, CYF, D, ST
      REAL :: DMG, FOUR, OUTPUT, PAR, R1, RHOO, RHO1, Q, STSMAX, STSMIN
      REAL :: TCDMGM, VAR, WLOAD, XARG, XARGMN, YARG, YARGMN, WLPRNT
      REAL :: XN, ACUMM1, ACUMM2

      INTEGER :: I, I10, I1V, I2, ICALL, ICARD, ICAS, II, ILINE, INTPER
      INTEGER :: J, IOUT, IREAD, IREF, ISETTB, ITAB, JEND, JJEND, JOUT
      INTEGER :: K, K4, K5, L, M, LEVEL, LINENO, M1, M6, M2, M20, N3958
      INTEGER :: N8, NSEG, NSEGNM
      DATA L/0/,M/0/,JOUT/0/,JEND/0/,IOUT/0/,ILINE/0/,ICALL/0/,I2/0/

      X=0.0
      Y=0.0
      DELTAY=0.0
      CUMM=0.0
      CSUM=0.0
      CYCLSM = 0.0

      READ (5,10) IREAD, ICARD
      IF (IREAD .NE. 2) THEN
         WRITE (6,30)
         LINENO = 0
         DO K = 1, ICARD
            LINENO = LINENO + 1
            IF (LINENO .GT. 50) THEN
               WRITE (6,30)
               LINENO = 0
            ELSE
               READ (5,20) (DATAIN(J), J = 1,13)
               WRITE (6,40) (DATAIN(J), J = 1,13)
            END IF
         END DO
      END IF

      II = 0
   85 N3958 = 3958
      CALL NPUT1A (X(1), N3958, Y(1), II, IREF, ICAS, 0)
!***  WRITE REFERENCE RUN, CASE NO., AND SEGMENTS ON TAPE FOR
!***  SPECTRUM LOADING RANDOM SEQUIENCE GENERATION PROGRAM
      WRITE (3) IRR, ICASE, IEND
      WLPRNT = 0.0
      IF (IW5 .NE. 2) THEN
         CALL PRINT
         WLPRNT = 1.0
      END IF
      B = 0.0
      K1(1) = 0
      TCDMGM = 0.0
  120 DO I = 1, IEND
         cumm=0.0
         TBLSN = 0.0
         TBLLD = 0.0
         INTPER = 0
         IF (ABS(B-1.0) .GT. 1E-6) THEN
            AX = 0.0
            CDMGM (I) = 0.0
            JEND = N(I)
            K = JEND - 1
            Q = 1.0
            !SELECTION IS MADE WHETHER OR NOT TO USE THE MULTIPLYING FACTOR F(I)
            IF (L1 .GT. 1) THEN
               D = 1.0
            ELSE
               D = F(I)
            END IF

            IF ((M3(I) .LE. 9) .OR. (M3(I) .GE. 13)) THEN
               !LOAD SPECTRUM INPUT FORMAT IS SELECTED
               DO J = 1, JEND
                  M6 = M5(I)
                  !CALCULATE THE INCREMETAL RESPONSE DELTAY
                  SELECT CASE (M6)
                     CASE (1)
                        DELTAY(J) = DELT1(J)
                     CASE (2)
                        DELTAY(J) = DELT2(J)
                     CASE (3)
                        DELTAY(J) = DELT3(J)
                     CASE (4)
                        DELTAY(J) = DELT4(J)
                     CASE (5)
                        DELTAY(J) = DELT5(J)
                     CASE (6)
                        DELTAY(J) = DELT6(J)
                     CASE (7)
                        DELTAY(J) = DELY1(I) + DELY11(I) * (Q - 1.0)
                     CASE DEFAULT
                        DELTAY(J) = DELT1(J)
                  END SELECT
                  Q = Q +1.0
                  DY(I,J) = DELTAY(J)
               END DO

               ! ESTABLISH MAX AND MIN RESPONSE VALUES AT MIDPOINTS BETWEEN
               ! SUCCESSIVE DELTA Y VALUES
               DO J = 1, K
                  AMIDY(J) = (DELTAY(J) + DELTAY(J + 1))/2.0
                  IF(IW3 .NE. 1) P(I) = 0.0
                  IF (N1FLAG(I).LE.2) THEN
                     YMAX(J) = D*(AM(I) + AMIDY(J)) + P(I)
                     IF (N1FLAG(I) .NE. 2) YMIN(J) = D*AM(I) + P(I)
                     IF (N1FLAG(I) .EQ. 1) CYCLE
                  ELSE
                     YMAX(J) = D*AM(I) + P(I)
                  END IF
                  YMIN(J) = D*(AM(I) - AMIDY(J)) + P(I)
               END DO
            END IF

            IF (M3(I) .GE. 13) THEN
               RHOO = 0.002378
               RHO1 = SIG(I) * RHOO
               IF (M3(I) .NE. 14 .AND. M3(I) .NE. 15) THEN
                  VAR = 32.2 * AC * SLOPE(I) * RHO1
                  WLOAD = 2.0 * WT(I)/WAREA
                  FOUR = 4.0 * (WLOAD/VAR)
                  PAR = FOUR + (6.28 / SLOPE(I))
                  R1 = FOUR/PAR
                  XARG = SCLTRB(I) / AC
                  YARG = PAR
                  XARGMN = TKSIG(18)
                  YARGMN = TKSIG(2)
                  IF (XARG .LT. XARGMN) THEN
                     IF (YARG .LT. YARGMN) THEN
                        WRITE (6,340) XARG, YARG, NSEG
                     ELSE
                        WRITE (6,360) XARG, NSEG
                     END IF
                  END IF
                  NSEG = I + 50
                  LEVEL = J
                  CALL TWOVIN(XARG, YARG, TKSIG, OUTPUT, NSEG,LEVEL)
                  !write(10,*)'1tw',XARG,YARG,TKSIG,OUTPUT,NSEG,LEVEL
                  AKSIG(I) = OUTPUT
                  AKSIG(I) = R1 * AKSIG(I)
               END IF
            END IF

            DO J = 1, JEND
!              CALCULATE THE CUMULATIVE CYCLES GIVEN VALUES OF DELTA Y
               M1 = M3(I)
               SELECT CASE (M1)
               CASE (1)
                  CUMM(J) = T(I) * TAB1(J)
               CASE (2)
                  CUMM(J) = T(I) * TAB2(J)
               CASE (3)
                  CUMM(J) = T(I) * TAB3(J)
               CASE (4)
                  CUMM(J) = T(I) * TAB4(J)
               CASE (5)
                  CUMM(J) = T(I) * TAB5(J)
               CASE (6)
                  CUMM(J) = T(I) * TAB6(J)
               CASE (7)
                  CUMM(J) = (ARNO1(I)*EXP(-DELTAY(J)**2/     &
                            (2.0*(SGMAX1(I))**2))            &
                          +  ARNO2(I)*EXP(-DELTAY(J)**2/     &
                            (2.0*(SGMAX2(I))**2))            &
                          +  ARNO3(I)*EXP(-DELTAY(J)**2/     &
                            (2.0*(SGMAX3(I))**2))) *  T(I)
               CASE (8,9,13,14,15)
                  IF (M1.EQ.8 .OR. M1.EQ.13) ABR(I) =                &
                    (VELOS(I)*SLOPE(I)*WAREA*AKSIG(I))/(498.0*WT(I))
                  IF (M1.EQ.14 .OR. M1.EQ.15) THEN
                     AMGT = (2*WT(I)/(RHO1*CBART*32.2*SLOPE(I)*AST)* &
                     (YAW(I)/WT(I))) / SCLTRB(I)**2
                     AKSIG(I) = 0.88 * AMGT /(5.3 + AMGT)
                  END IF
                  IF (M3(I).EQ.14) ABR(I) = VELOS(I)*WAREA*AKSIG(I)  &
                     /(498.*WT(I))*(CYBT(I) + CYBT0(I))
                  IF (M3(I) .EQ. 15)                                 &
                     ABR(I) = (VELOS(I)*AST*SLOPE(I)/498.)*AKSIG(I)

                  IF (DELTAY(J)/(AK1(I)*ABR(I)).GE.24) THEN
                     ACUMM1 = 0.0
                  ELSE
                     ACUMM1 = EXP(-DELTAY(J)/(AK1(I)*ABR(I)))
                  END IF
                  IF (DELTAY(J)/(AK2(I)*ABR(I)).GE.24) THEN
                     ACUMM2 = 0.0
                  ELSE
                     ACUMM2 = EXP(-DELTAY(J)/(AK2(I)*ABR(I)))
                  END IF
                  CUMM(J) = (ARNO1(I)*P1(I)*ACUMM1 +                  &
                             ARNO2(I)*P2(I)*ACUMM2) * T(I)
                  !CUMM(J) = (ARNO1(I)*P1(I)*EXP(-DELTAY(J)/(AK1(I)*ABR(I))) &
                  !        +  ARNO2(I)*P2(I)*EXP(-DELTAY(J)/ (AK2(I)*ABR(I)))) * T(I)
               CASE (10)
                  STSMXM(I,J) = TABL1(J)
                  STSMNM(I,J) = TABL2(J)
                  CYCLSM(I,J) = TABL3(J)
                  K = JEND
                  AX = 1.0
                  CYCLE
               CASE (11)
                  STSMXM(I,J) = TABL4(J)
                  STSMNM(I,J) = TABL5(J)
                  CYCLSM(I,J) = TABL6(J)
                  K = JEND
                  AX = 1.0
                  CYCLE
               CASE (12)
                  STSMXM(I,J) = TABL7(J)
                  STSMNM(I,J) = TABL8(J)
                  CYCLSM(I,J) = TABL9(J)
                  K = JEND
                  AX = 1.0
                  CYCLE
               CASE DEFAULT
                  CUMM(J) = T(I) * TAB1(J)
               END SELECT
               CSUM(I,J) = CUMM(J)
            END DO
            K1(I) = K
            IF (ABS(AX-1.0) .GT. 1E-6) THEN
!              CALCULATE CYCLES FOR Y MAX AND Y MIN.
               DO J = 1, K
                  CYCLSM(I,J) = CUMM(J) - CUMM(J+1)
               END DO
            END IF
         END IF

         DO J = 1, K
            IF(ABS(B-1.0) .GT. 1E-6) THEN
               IF (ABS(AX-1.0) .GT. 1E-6) THEN
                  !SELECT WHETHER TO ENTER OR NOT TO ENTER THE STRESS TABLES.
                  !SELECTION IS MADE BY ISTRES FLAG.
                  IF (ISTRES(I) .GE. 1) THEN
                     DO I1V=1,2
                        IF (I1V.EQ.1) THEN
                           ARGUMT = YMAX(J)
                        ELSE
                           ARGUMT = YMIN(J)
                        END IF
                        M2 = ISTRES(I)
                        M2 = (31 * M2) - 30
                        DO ITAB = 1, 31
                           M20 = ITAB + M2 - 1
                           TBLLD(ITAB) = TBLM2(M20)
                        END DO
                        !SUBROUTINE ONEVAR- GIVEN A VALUE OF RESPONSE Y, INTERPOLATE IN
                        !STRESS TABLES FOR A VALUE OF STRESS
                        NSEGNM = I
                        CALL ONEVAR (ARGUMT, TBLLD, OUTPUT, NSEGNM)
                        IF (I1V.EQ.1) THEN
                           STSMXM(I,J) = OUTPUT
                        ELSE
                           STSMNM(I,J) = OUTPUT
                        END IF
                     END DO
                  ELSE
                     !WHEN STRESS TABLES ARE NOT USED, SET RESPONSE Y = STRESS
                     STSMXM(I,J) = YMAX(J)
                     STSMNM(I,J) = YMIN(J)
                  END IF
                  !TEST TO ESTABLISH TRUE MAX AND MIN STRESS VALUES.
                  !ALGEBRAICALLY, MAX STRESS GREATER THAN MIN STRESS.
                  STSMAX=AMAX1(STSMXM(I,J),STSMNM(I,J))
                  STSMIN=AMIN1(STSMXM(I,J),STSMNM(I,J))
                  STSMXM(I,J)=STSMAX
                  STSMNM(I,J)=STSMIN
                  IF (STSMXM (I,J) .LE. 0.0) THEN
  690                DMAGEM(J) = 0.0
                     CYC(J) = 0.0
                     CDAMG(J) = 0.0
                     IF (ABS(B-1.0) .LE. 1E-6) GO TO 1540
                     IF (INTPER .EQ. 0) CYCLE
                     IF (YARG .GE. YARGMN) CYCLE
                     NSEG = I
                     LEVEL = J
                     WRITE (6,710) XARG, YARG, NSEG, LEVEL
                     INTPER = 0
                     CYCLE
                  END IF
               END IF
               !FORM INTERPOLATING ARGUMENTS TO CALULATE CYCLES TO FAILURE
               !FROM S-N DATA
               SELECT CASE (IA(I))
                  CASE (:6)
                     XARG = (STSMXM(I,J) / SIGULT)
                     YARG = (STSMNM(I,J) / STSMXM (I,J))
                     I2 = IA(I)
                  CASE (7:12)
                     XARG = (STSMXM(I,J) / SIGULT)
                     YARG = (STSMXM(I,J)+STSMNM(I,J)) /(2.0*SIGULT)
                     I2= IA(I) - 6
                  CASE (13:18)
                     XARG = (STSMXM(I,J)-STSMNM(I,J)) /(2.0*SIGULT)
                     YARG = (STSMXM(I,J)+STSMNM(I,J)) /(2.0*SIGULT)
                     I2= IA(I) - 12
                  CASE (19:)
                     XARG = (STSMXM(I,J) / SIGULT)
                     YARG = (STSMNM(I,J) / SIGULT)
                     I2= IA(I) - 18
               END SELECT
               ICALL = I2
            END IF

            I2 = (257 *I2) - 256
            DO ISETTB = 1, 257
               I10 = ISETTB +I2 -1
               TBLSN(ISETTB) = TBLI2(I10)
            END DO
            XARGMN = TBLSN(18)
            YARGMN = TBLSN(2) - 0.001
            IF (XARG .LT. XARGMN) THEN
               INTPER = 1
               GO TO 690
            END IF
            NSEG = 1
            LEVEL = J
            IF (ABS(B-1.0) .LE. 1E-6) NSEG = 50
            ! SUBROUTINE TWOVIN - LINEAR - QUADRATIC INTERPOLATION OF S-N DATA.
            ! GIVEN THE INTERPOLATING VALUES XARG AND YARG, INTERPOLATE FOR A
            ! VALUE OF CYCLES TO FAILURE.
            CALL TWOVIN (XARG, YARG, TBLSN, OUTPUT, NSEG, LEVEL)
            !write(10,*)'2tw',XARG,YARG,TBLSN,OUTPUT,NSEG,LEVEL
            SELECT CASE (ICALL)
               CASE (1)
               ALIFE = AL1
               CASE (2)
               ALIFE = AL2
               CASE (3)
               ALIFE = AL3
               CASE (4)
               ALIFE = AL4
               CASE (5)
               ALIFE = AL5
               CASE (6)
               ALIFE = AL6
               CASE DEFAULT
               ALIFE = AL1
            END SELECT
            IF (ABS(B-1.0) .LE. 1E-6) GO TO 1530
            ALIFE = ALOG10(ALIFE)
            IF (OUTPUT .LT. ALIFE) THEN
               CYC(J) = 10** OUTPUT
            ELSE
               GOTO 690
            END IF
            !FORM THE RATIO DAMAGE = CYCLES EXPERIENCED AT A GIVEN RESPONSE
            !LEVEL / CYCLES TO FAILURE AT THE RESPONSE LEVEL.
            DMAGEM(J) = CYCLSM(I,J) / CYC(J)

            !SUM THE DAMAGE DUE TO EAH LOAD INCREMET WITHIN ONE SEGMENT.
            CDMGM(I) = CDMGM(I) + DMAGEM(J)
            CDAMG(J) = CDMGM(I)
         END DO

         !SUM THE DAMAGE OF ALL SEGMENTS.
         TCDMGM = TCDMGM + CDMGM(I)

         !WRITE TAPE FOR SPECTRUM LOADING RANDOM SEQUENCE GENERATION PROGRAM
         JJEND = JEND - 1
         IF ((M3(I) .GT. 9) .AND. (M3(I) .LT. 13)) JJEND = JEND
         WRITE(1)JJEND,(STSMXM(I,J),STSMNM(I,J),CYCLSM(I,J), J=1,JJEND)

         IF (IW2 .EQ. 2) CYCLE
         WLPRNT = 1.0
         IF (IW5 .LT. 2 .OR. I.GT.1)  WRITE (6,100) IRR, ICASE
         WRITE (6,970) I
         WRITE (6,980)
         WRITE (6,990)
         DO J = 1, JEND
            IF ((M3(I) .LE. 9) .OR. (M3(I) .GE. 13))                  &
               WRITE (6,1000) J, DELTAY(J), CUMM(J)
            IF (J .EQ. JEND) CYCLE
            IF ((M3(I).GT.9).AND.(M3(I).LT.13).OR.M3(I).GT.15) THEN
               WRITE(6,1020) J,STSMXM(I,J),STSMNM(I,J),CYCLSM(I,J),   &
                           CYC(J), DMAGEM(J), CDAMG(J)
            ELSE
               WRITE (6,1040) STSMXM(I,J), STSMNM(I,J), CYCLSM(I,J),  &
                           CYC(J), DMAGEM(J), CDAMG(J)
            END IF
         END DO
         IF ((M3(I) .EQ. 8) .OR. (M3(I) .EQ. 9)) WRITE (6,1060)       &
             AKSIG(I), ABR(I)
         IF ((M3(I) .GE. 13) .AND. (M3(I) .LE. 15)) WRITE (6,1060)    &
             AKSIG(I), ABR(I)
         ABC(I) = TCDMGM
      END DO

      IF (I4 .NE. 0) THEN
         CUMDMG = 0.0
         CUMXN = 0.0
         JSUM = 0
         ISUM = 0
         I = 0
         J = 0
         M = 0
         K = 0
         K4 = 0
         K5 = 0
         DO I = 1, KEND
            N8=K1(I)
            DO J = 1, N8
               IF (N6(I) .LT. 1) CYCLE
               IF (N6(I) .GT. 1) THEN
                  K = K5 + 1
                  QMAX(K) = STSMXM(I,J)
                  JX(K) = CYCLSM(I,J)
                  K5 = K
               ELSE
                  K = K4 + 1
                  QMIN(K) = STSMNM(I,J)
                  JI(K) = CYCLSM(I,J)
                  K4 = K
               END IF
            END DO
         END DO

         IF (IW1 .NE.2) THEN
            WLPRNT = 1.0
            WRITE (6,100) IRR, ICASE
            WRITE (6,1170)
            WRITE (6,1180)
            WRITE (6,1190)
         END IF
         IF (K4 .LE. K5) THEN
            K = K5
            ILINE = 0
         ELSE
            K = K4
            ILINE = 0
         END IF
         DO I = 1, K
            IF (JSUM .LT. NEND) THEN
               !SORT MAX ARRAY INTO DESCENDING ORDER.
               DO J = I, K5
                  IF (QMAX(I) .GE. QMAX(J)) CYCLE
                  ST = QMAX(I)
                  QMAX(I) = QMAX(J)
                  QMAX(J) = ST
                  JT = JX(I)
                  JX(I) = JX(J)
                  JX(J) = JT
                  JOUT = I
               END DO
               JSUM = JSUM + JX(I)
               IF (JSUM .GE. NEND) THEN
                  JX(JOUT) = JX(JOUT) - (JSUM - NEND)
                  JSUM = JSUM - (JSUM - NEND)
               END IF
               IF (IW1 .NE. 2) THEN
                  WRITE (6,1270) QMAX(I), JX(I), JSUM
                  IF (K4 .LE. K5) THEN
                     ILINE = ILINE + 1
                     IF (ILINE .GE. 28) THEN
                        ILINE = 0
                        WRITE (6,100) IRR, ICASE
                        WRITE (6,1170)
                        WRITE (6,1190)
                     END IF
                  END IF
               END IF
            END IF
            IF (ISUM .GE. NEND) CYCLE
            !SORT MIN ARRAY INTO ASCENDING ORDER
            DO J = I, K4
               IF (QMIN(I) .LE. QMIN(J)) CYCLE
               ST = QMIN(I)
               QMIN(I) = QMIN(J)
               QMIN(J) = ST
               JT = JI(I)
               JI(I) = JI(J)
               JI(J) = JT
               IOUT = I
            END DO
            ISUM = ISUM + JI(I)
            IF (ISUM .GE. NEND) THEN
               JI(IOUT) = JI(IOUT) - (ISUM - NEND)
               ISUM = ISUM - (ISUM - NEND)
            END IF
            IF (IW2 .EQ. 2) CYCLE
            WRITE (6,1350) QMIN(I), JI(I), ISUM
            IF (K4 .LE. K5) CYCLE
            ILINE = ILINE + 1
            IF (ILINE .GE. 28) THEN
               ILINE = 0
               WRITE (6,100) IRR, ICASE
               WRITE (6,1170)
               WRITE (6,1190)
            END IF
         END DO
         L = 1
         M = 1
         !FORM INTERPOLATING ARGUMENT TO CALCULATE CYCLES TO FAILURE
         !FROM S-N DATA
         IF (IW1 .NE. 2) THEN
            ILINE = 0
            WRITE (6,100) IRR, ICASE
            WRITE (6,1410)
            WRITE (6,1420)
         END IF

         DO WHILE (.TRUE.)
            SELECT CASE (I4)
               CASE (:6)
                  XARG = QMAX(L) / SIGULT
                  YARG = QMIN(M) / QMAX(L)
               CASE (7:12)
                  XARG = QMAX(L) / SIGULT
                  YARG = (QMAX(L) + QMIN(M)) / (2.0*SIGULT)
               CASE (13:18)
                  XARG = (QMAX(L) - QMIN(M)) / (2.0*SIGULT)
                  YARG = (QMAX(L) + QMIN(M)) / (2.0*SIGULT)
               CASE (19:)
                  XARG = QMAX(L) / SIGULT
                  YARG = QMIN(M) / SIGULT
            END SELECT
            IF (I4 .LT. 7) I2 = I4
            IF ((I4 .GT. 6) .AND. (I4 .LT. 13)) I2 = (I4 -6)
            IF ((I4 .GT. 12) .AND. (I4 .LT. 19)) I2 = (I4 -12)
            IF (I4 .GT. 18) I2 = (I4 -18)
            ICALL = I2
            IF (JX(L) .LT. JI(M)) THEN
               JI(M) = JI(M) - JX(L)
               XN = JX(L)
               A = 1.0
            ELSE
               JX(L) = JX(L) - JI(M)
               XN = JI(M)
               A = 0.0
            END IF
            CUMXN = CUMXN + XN
            B = 1.0
            GO TO 120

 1530       ALIFE = ALOG10(ALIFE)
            IF (OUTPUT .GE. ALIFE) THEN
 1540          DMG = 0.0
               CYF = 0.0
            END IF
            IF (YARG .LT. YARGMN) THEN
               IF (OUTPUT.GE.ALIFE) WRITE (6,1550) XARG, YARG
               CYF = 10.0 ** OUTPUT
               DMG = XN / CYF
            END IF
            CUMDMG = CUMDMG + DMG

            IF (IW1 .NE. 2) THEN
              WRITE(6,1580)QMAX(L),QMIN(M),XN,CUMXN,YARG,CYF,DMG,CUMDMG
               ILINE = ILINE + 1
               IF (ILINE .GE. 54) THEN
                  ILINE = 0
                  WRITE (6,100) IRR, ICASE
                  WRITE (6,1410)
                  WRITE (6,1420)
               END IF
            END IF
            IF ((L .EQ. JOUT) .AND. (M .EQ. IOUT)) EXIT
            IF ((ABS(JX(L)) .LE. 1E-6) .AND. (ABS(A) .LE. 1E-6)) L = L + 1
            IF (ABS(A-1.0) .LE. 1E-6) L = L + 1
            IF (ABS(A) .LE. 1E-6) M = M + 1
         END DO

         !CALCULATION OF TOTAL DAMAGE INCLUDING GAG
         TCDMGM = TCDMGM + CUMDMG
      END IF
      IF (IW4 .NE. 2) THEN
         CALL SPECSM
         WLPRNT = 1.0
      END IF
      IF (ABS(WLPRNT) .GT. 1E-5) THEN
         WRITE (6,100) IRR, ICASE
         WRITE (6, 1630)
         WRITE (6,1640)
      ELSE
         WRITE (6,1630)
         WRITE (6,1660)
      END IF
      WRITE (6,1680) (I, CDMGM(I), ABC(I), I = 1, IEND)
      IF (I4 .NE. 0) WRITE (6,1690) CUMDMG, TCDMGM
      IF (II .EQ. 1) GO TO 85  ! PSV 90 -> 85

      RETURN
   10 FORMAT (I3, I3)
!  20 FORMAT (13A6)  ! PSV
   20 FORMAT (13A4)  ! PSV
   30 FORMAT ('1')
   40 FORMAT (13A4)
  100 FORMAT ('REFERENCE RUN NO.', I6, 4X, 'CASE NO', I6)
  340 FORMAT (                                                     &
      'GUST ALLEV. INTRP. ERROR X IS TOO SMALL. X ='               &
       ,E14.6,1X,'Y IS TOO SMALL. Y =',E14.6, 2X,                  &
      'SEG =', I2)
  360 FORMAT (                                                     &
      'GUST ALLEV. INTRP. ERROR, X IS TOO SMALL.'                  &
      ' X =',E14.6, 2X, 'SEG = ', I2)
  710 FORMAT ('SN, INTP ERROR X IS TOO SMALL X = ',E14.6, 1X,      &
      'Y IS TOO SMALL Y = ',E14.6, 1X, 'SEG =', I2, 1X,            &
      'LOAD LEVEL =', I2, 1X, 'DAMAGE SET = 0.0')
  970 FORMAT(4X, 'SEGMENT =', I2)
  980 FORMAT ('--------------------------SPECTRUM-------------'    &
      '-------------------', 2X, '------------DAMAGE CALCULATION'  &
      '------------')
  990 FORMAT (5X, 1HJ, 3X, 'DELTA Y', 3X,                          &
      'CUMULATIVE CYCLES', 2X, 'MAX STRESS', 2X 'MIN STRESS',      &
      8X, 'CYCLES', 10X, 'ALLOWABLE', 8X, 'DAMAGE', 2X,            &
      'CUM DAMAGE')
 1000 FORMAT (1H, 4X, I2, F13.3,1X,F16.4)
 1020 FORMAT (1H , 4X, I2, 32X, F10.0, 2X, F10.0, 2X, F16.4,       &
              2X, F16.0, 1X, F11.7, 1X, F11.7)
 1040 FORMAT (38X, F10.0, 2X, F10.0, 2X, F16.4, 2X, F16.0,         &
      1X, F11.7, 1X, F11.7)
 1060 FORMAT ( 5X, 'GUST ALLEVIATION FACTOR = ',F9.6, 5X,          &
      'A-BAR = ', F16.6)
 1170 FORMAT ('MAX AND MIN STRESSES AND CYCLE ARRAYS FORMED FOR'   &
              'THE DEFINITION OF THE GAG CYCLES')
 1180 FORMAT ('ARRAYS ARE FROMED FROM SEGMENTS AS SPECIFIED BY'    &
              'FLAG = N6.')
 1190 FORMAT (4X, 'MAX STRESS', 10X, 'CYCLES', 10X, 'CUM CYCLES',  &
      10X, 'MIN STRESS', 10X, 'CYCLES', 10X, 'CUM CYCLES')
 1270    FORMAT (4X, F10.0, 4X, F16.4, 3X, F16.4)
 1350 FORMAT (60X, F10.0, 4X, F16.4, 3X, F16.4)
 1410 FORMAT (' GAG CYCLE SPECTRUM AND DAMAGE CALCULATION')
 1420 FORMAT ( 5X, 'MAX', 7X, 'MIN STRESS', 9X, 'CYCLES',          &
      9X, 'CUM CYCLES', 9X, 'R', 8X, 'ALLOWABLE', 7X, 'DAMAGE',    &
      4X, 'CUM DAMAGE')
 1550 FORMAT (' SN ITERP. ERROR. X IS TOO SMALL. X = E14.6',       &
      1X, 'Y IS TOO SMALL. Y = E14.6, 1X, DAMAGE IS SET = 0.0',    &
      1X, '(GAG SEGMENT)')
 1580 FORMAT (2X, F15.0, 2X, F15.0, 2X, F16.4, 2X, F16.4, 1X,F7.3, &
      2X, F16.0, 1X, F11.7, 1X, F11.7)
 1630 FORMAT ('INDIVIDUAL SEGMENT AND TOTAL DAMAGE SUMMARY')
 1640 FORMAT ( 8X, 'SEG.', 7X, 'DAMAGE', 11X, 'TOTAL')
 1660 FORMAT ( 8X, 'SEG.', 7X, 'DAMAGE', 11X, 'TOTAL')
 1680 FORMAT (9X, I2, F16.7, F16.7)
 1690 FORMAT (9X, 'GAG', F16.7, F16.7)
      END

!----------------------------------------------------------------------
      SUBROUTINE NPUT1A (CASE, NCASE, RAREA, IENTRY, IREF, ICAS, KP)
!
!       THIS SUBROUTINE READS DATA CARDS
!
!       CASE   = OUTPUT = A LINER ARRAY CONTAINING THE DATA
!                        READ FROM CARDS
!       NCASE  = IN      THE UPPER LIMIT OF THE CASE ARRAY
!
!       RRAREA = IN    = A REFERENCE RUN ARRAY (OF THE SAME LENGTH
!                        AS CASE). NOT USED NOW
!
!       IENTRY  = I-OUT = ENTRY FLAG (INTEGER)
!                       =  0  INITITAL ENTRY  (INPUT)
!                       =  1  NORMAL OUTPUT FLAG (OUTPUT)
!                       = -1  LAST CASE FLAG (OUTPUT).
!
!       IREF    = OUTPUT = REFERENCE RUN NUMBER (INTEGER) TAKEN FROM
!                         INPUT CARDS

!       ICAS    = OUTPUT = CASE NUMBER (INTEGER) TAKEN FROM DATA CARDS
!
!       KP      = INPUT  = OPTIONAL PRINT CODE (INTEGER)
!                           IF KP IS NOT IN ARGUMENT LIST, IT IS
!                           TREATED AS IF KP = 0
!
!       FORTRAN UNIT 5 IS READ (FOR DATA)
!       FORTRAN UNIT 6 FOR WRITE
!
!      CARD FORMAT IS
!            COLUMN 1    MUST CONTAIN I OR F TYPE OF DATA (INTEGER OF FLOAT)
!       THEN 4 NUMBERS FOLLOW IN FREE FORMAT
!            LOC      IMTEGER LOCATION FIELDS
!            DATA     DATA FIELD (FLOAT IF COLUMN 1 CONTAINS "F"
!                                 AND INTEGER IF COLUMN 1 CONTAINS "I" )
!            REFNO    INTEGER REFERENCE RUN NUMBER (NOT USED NOW)
!            CASENO   INTEGER CASE NUMBER
!
      REAL :: CASE(NCASE), RAREA(NCASE)
      INTEGER NCASE, IENTRY, IREF, ICAS, KP
      CHARACTER*80 LINE
      CHARACTER*1 TYPE
      INTEGER :: LOC, N, REFNO, CASENO, RDSTAT
      REAL :: AN
      EQUIVALENCE (AN,N)
      SAVE TYPE, LOC, N, REFNO, CASENO, AN

      CASE=0.0
      RAREA = 0.0
      IF (IENTRY.NE.0) THEN
         ICAS=CASENO
         IREF=REFNO
         CASE(LOC) = AN
         WRITE (6,30) IREF, ICAS
      END IF

      DO WHILE (.TRUE.)
         READ(5,'(A80)', IOSTAT=RDSTAT) LINE
         IF (RDSTAT .LT. 0) THEN
            WRITE(6,*) '?--- INPUT FILE ERROR: END OF FILE REACHED'
            STOP 'Abnormal termination'
         END IF
         IF (RDSTAT .GT. 0) THEN
            WRITE(6,*) '?--- INPUT FILE ERROR: WRONG DATA'
            STOP 'Abnormal termination'
         END IF

         TYPE = LINE(1:1)
         IF (TYPE.NE.'I' .AND. TYPE.NE.'F') THEN
            WRITE(6,*) '?--- INPUT FILE ERROR: UNKNOWN DATA TYPE'
            STOP 'Abnormal termination'
         END IF
         IF (TYPE=='I') THEN
            READ(LINE(2:),*, IOSTAT=RDSTAT) LOC, N, REFNO, CASENO
         ELSE
            READ(LINE(2:),*, IOSTAT=RDSTAT) LOC, AN, REFNO, CASENO
         END IF
         IF (RDSTAT .NE. 0) THEN
            WRITE(6,*) '?--- INPUT FILE ERROR: WRONG DATA'
            STOP 'Abnormal termination'
         END IF
         IF (IENTRY.EQ.0) THEN
            ICAS=CASENO
            IREF=REFNO
            IENTRY=1
            WRITE (6,30) IREF, ICAS
         END IF

         IF (CASENO.EQ.999) THEN
            IENTRY=-1
            RETURN
         END IF

         IF (CASENO.NE.ICAS) RETURN

         CASE(LOC) = AN
      END DO

      RETURN
   30 FORMAT ('1REFERENCE RUN NO.', I3, 4X, 'CASE NO.', I4)
      END

!----------------------------------------------------------------------
      SUBROUTINE PRINT
      COMMON X, Y
      REAL :: X(3958), Y(3958), SIG(40), SCLTRB(40), F(40)
      REAL :: YAW(40), CYBT(40), CYBT0(40)
      INTEGER :: M3(40), M5(40), ISTRES(40), IA(40), N1FLAG(40)
      INTEGER :: N6(40), N2(40), N(40)
      REAL :: P(40),  AM(40), DELY1(40), DELY11(40)
      REAL :: ARNO1(40), ARNO2(40), ARNO3(40)
      REAL :: SGMAX1(40), SGMAX2(40), SGMAX3(40), AKSIG(40)
      REAL :: SLOPE(40), VELOS(40), WT(40), P1(40), P2(40)
      REAL :: AK1(40), AK2(40), ABR(40), TBLM2(434), TBLI2(1542)
      REAL :: DELT1(25), DELT2(25), DELT3(25)
      REAL :: DELT4(25), DELT5(25), DELT6(25)
      REAL :: TAB1(25),TAB2(25),TAB3(25),TAB4(25),TAB5(25),TAB6(25)
      REAL :: TABL1(25), TABL2(25), TABL3(25), TABL4(25), TABL5(25)
      REAL :: TABL6(25), TABL7(25), TABL8(25), TABL9(25), T(40)
      REAL :: AC, AL1,AL2,AL3,AL4,AL5,AL6, AST, CBART, SIGULT, WAREA
      INTEGER :: IW1, IW2, IW3, IW4, IW5
      INTEGER :: IEND, KEND, I4, IRR, ICASE, NEND, L1
      EQUIVALENCE (IEND, X(1)), (KEND, X(2)), (I4, X(3)),             &
      (SIGULT, X(4)), (WAREA, X(5)), (ISTRES, X(6)),                  &
      (DELT1, X(46)), (TAB1, X(71)), (TAB2, X(96)),                   &
      (TAB3, X(121)),  (TAB4, X(146)), (TAB5, X(171)),                &
      (AC, X(196)), (IW1, X(197)), (IW2, X(198)), (IW3, X(199)),      &
      (IW4, X(200)),    (IRR, X(201)), (ICASE, X(202)),               &
      (IW5, X(203)), (TAB6, X(206)), (DELT2, X(231)),                 &
      (DELY1, X(256)), (DELY11, X(296)), (ARNO1, X(336)),             &
      (ARNO2, X(376)), (ARNO3, X(416)), (SGMAX1, X(456)),             &
      (SGMAX2, X(496)), (SGMAX3, X(536)), (T, X(576)),                &
      (AKSIG, X(616)), (SLOPE, X(656)), (VELOS, X(696)),              &
      (WT, X(736)), (P1, X(776)), (TBLM2, X(853))
      EQUIVALENCE (P2, X(1473)), (AK1, X(1513)), (AK2, X(1553)),      &
      (ABR, X(1593)), (IA,  X(1633)), (M3, X(1673)),                  &
      (SIG, X(1713)), (AM,  X(1753)), (N, X(1793)),                   &
      (NEND, X(1833)), (AL6, X(1850)), (AL5,  X(1851)),               &
      (AL4, X(1852)), (AL3, X(1853)), (AL2, X(1854)),                 &
      (AL1, X(1855)), (TBLI2, X(1856)), (M5, X(3398)),                &
      (N1FLAG, X(3438)), (F, X(3478)), (N6, X(3518)),                 &
      (N2, X(3558)),    (P, X(3598)), (SCLTRB, X(3638)),              &
      (TABL1, X(3678)), (TABL2, X(3703)), (TABL3, X(3728)),           &
      (TABL4, X(3753)), (TABL5, X(3778)), (TABL6, X(3803)),           &
      (TABL7,   X(1287)), (TABL8, X(1312)), (TABL9, X(1337)),         &
      (DELT3,   X(1362)), (DELT4, X(1387))
      EQUIVALENCE (DELT5, X(1412)), (DELT6, X(1437)),                 &
                  (L1, X(1462))
      EQUIVALENCE (CBART, X(3837)), (AST, X(3838)),                   &
                  (YAW, X(3839)), (CYBT, X(3879)),                    &
                  (CYBT0, X(3919))
      INTEGER :: I, J, I2, ICALL, A5, B5, C5, D5, E5, F5, G5

      WRITE (6,310)

      IF (ALL(M3(1:IEND).GE.14) .AND. ALL(M3(1:IEND).LE.15)) THEN
         WRITE (6,25)
         WRITE (6,35) IEND,KEND,I4,SIGULT, WAREA, AC,FLOAT(NEND),     &
         L1, IW1, IW2, IW3, IW4, IW5, CBART, AST
      ELSE
         WRITE (6,20)
         WRITE (6,30) IEND, KEND, I4,SIGULT,WAREA,AC,float(NEND),     &
                      L1, IW1, IW2, IW3, IW4, IW5
      END IF

      WRITE (6,40)
      WRITE (6,50) (I, M3(I), M5(I), ISTRES(I), IA(I), N1FLAG(I),     &
                    P(I), N6(I), N2(I), AM(I), F(I), DELY1(I),        &
                    DELY11(I), I = 1, IEND)
      WRITE (6,10) IRR, ICASE
      WRITE (6,310)
      WRITE (6,60)
      WRITE (6,70) (I, ARNO1(I), ARNO2(I), ARNO3(I), SGMAX1(I),       &
                    SGMAX2(I), SGMAX3(I), I = 1, IEND)
      WRITE (6,10) IRR, ICASE
      WRITE (6,310)
      WRITE (6,80)
      WRITE (6,90) (I, AKSIG(I), SLOPE(I), VELOS(I), WT(I), P1(I),    &
                   P2(I), AK1(I), AK2(I), ABR(I), T(I), N(I),         &
                   I = 1, IEND)

      IF (ANY(M3(1:IEND).GE.13 .AND. M3(1:IEND).LE.15)) THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         IF(ALL(M3(1:IEND).GE.14).AND.ALL(M3(1:IEND).LE.15)) THEN
            WRITE (6,105)
            WRITE (6,115) (I, SIG(I), SCLTRB(I),CYBT(I),CYBT0(I),     &
            YAW(I), I = 1, IEND)
         ELSE
            WRITE (6,100)
            WRITE (6,110) (I, SIG(I), SCLTRB(I), I = 1, IEND)
         END IF
      END IF

      IF (ANY(M5(1:IEND).GT.0 .AND. M5(1:IEND).LT.7)) THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,130)
         WRITE (6,160)
         WRITE (6,170)(J, DELT1(J), DELT2(J), DELT3(J), DELT4(J),     &
         DELT5(J), DELT6(J), J = 1, 25)
      END IF

      IF (ANY(M3(1:IEND).GT.0 .AND. M3(1:IEND).LT.7)) THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,130)
         WRITE (6,180)
         WRITE (6,190) (J,TAB1(J),TAB2(J),TAB3(J),TAB4(J),TAB5(J),    &
         TAB6(J), J = 1, 25)
      END IF

      IF (ANY(M3(1:IEND).GT.9 .AND. M3(1:IEND).LT.12)) THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,130)
         WRITE (6,210)
         WRITE (6,220)(J, TABL1(J), TABL2(J), TABL3(J), TABL4(J),     &
         TABL5(J), TABL6(J), J = 1, 25)
      END IF

      IF (ANY(M3(1:IEND).EQ.12)) THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,130)
         WRITE (6,230)
         WRITE (6,240) (J, TABL7(J), TABL8(J), TABL9(J), J=1,25)
      END IF

      IF(ANY(ISTRES(1:IEND).GT.0 .AND. ISTRES(1:IEND).LT.9))THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,130)
         WRITE (6,320)
         WRITE (6,120)
         WRITE (6,140) (J, TBLM2(J), TBLM2(J+31), TBLM2(J+62),        &
                        TBLM2(J+93), TBLM2(J+124), TBLM2(J+155),      &
                        TBLM2(J+186), TBLM2(J+217), J = 1, 31)
      END IF

      IF(ANY(ISTRES(1:IEND).GT.8 .AND. ISTRES(1:IEND).LT.15))THEN
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,130)
         WRITE (6,320)
         WRITE (6,200)
         WRITE (6,140) (J, TBLM2(J), TBLM2(J+248), TBLM2(J+279),      &
                        TBLM2(J+310), TBLM2(J+341), TBLM2(J+372),     &
                        TBLM2(J+403), J = 1, 31)
      END IF

      A5 = 0
      B5 = 0
      C5 = 0
      D5 = 0
      E5 = 0
      F5 = 0
      G5 = 0
      DO WHILE(.TRUE.)
         DO I = 1, IEND
            IF ( IA(I).LT.7) I2 = IA(I)
            IF ((IA(I).GT.6) .AND. (IA(I).LT.13)) I2 = IA(I)-6
            IF ((IA(I).GT.12) .AND. (IA(I).LT.19)) I2 = IA(I)-12
            IF ( IA(I).GT.18) I2 = IA(I) -18
            ICALL = I2
            SELECT CASE (ICALL)
               CASE (:1,7:)
                  IF (A5 .NE. 1) THEN
                    WRITE (6,10)  IRR, ICASE
                    WRITE (6,310)
                    WRITE (6,250) ICALL, IA(I)
                    WRITE (6,260) TBLI2(1), TBLI2(17), AL1
                    WRITE (6,270)
                    WRITE (6,280)(TBLI2(J),TBLI2(J+16),TBLI2(J+31),   &
                    TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),  &
                    TBLI2(J+106), TBLI2(J+121), J = 2, 16)
                    WRITE (6,290)
                    WRITE (6,300)(TBLI2(J+136),TBLI2(J+151),          &
                    TBLI2(J+166),TBLI2(J+181), TBLI2(J+196),          &
                    TBLI2(J+211),TBLI2(J+226),TBLI2(J+241), J=2,16)
                    A5 = 1
                  END IF
               CASE (2)
                  IF (B5 .NE. 1) THEN
                    WRITE (6,10) IRR, ICASE
                    WRITE (6,310)
                    WRITE (6,250) ICALL, IA(I)
                    WRITE (6,260) TBLI2(258), TBLI2(274), AL2
                    WRITE (6,270)
                    WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),  &
                    TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),  &
                    TBLI2(J+106), TBLI2(J+121), J = 259, 273)
                    WRITE (6,290)
                    WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),       &
                    TBLI2(J+166),TBLI2(J+181), TBLI2(J+196),          &
                    TBLI2(J+211),TBLI2(J+226),                        &
                    TBLI2(J+241),J=259,273)
                    B5 = 1
                  END IF
               CASE (3)
                  IF (C5 .NE. 1) THEN
                    WRITE (6,10) IRR, ICASE
                    WRITE (6,310)
                    WRITE (6,250) ICALL, IA(I)
                    WRITE (6,260) TBLI2(515), TBLI2(531), AL3
                    WRITE (6,270)
                    WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),  &
                    TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),  &
                    TBLI2(J+106), TBLI2(J+121), J = 516, 530)
                    WRITE (6,290)
                    WRITE (6,300)(TBLI2(J+136), TBLI2(J+151),         &
                    TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),         &
                    TBLI2(J+211), TBLI2(J+226),                       &
                    TBLI2(J+241), J = 516, 530)
                    C5 = 1
                  END IF
               CASE (4)
                  IF (D5 .NE. 1) THEN
                    WRITE (6,10) IRR, ICASE
                    WRITE (6,310)
                    WRITE (6,250) ICALL, IA(I)
                    WRITE (6,260) TBLI2(772), TBLI2(788), AL4
                    WRITE (6,270)
                    WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),  &
                    TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),  &
                    TBLI2(J+106), TBLI2(J+121), J = 773, 787)
                    WRITE (6,290)
                    WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),       &
                    TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),         &
                    TBLI2(J+211), TBLI2(J+226),                       &
                    TBLI2(J+241), J = 773, 787)
                    D5 = 1
                  END IF
               CASE (5)
                  IF (E5 .NE. 1) THEN
                    WRITE (6,10) IRR, ICASE
                    WRITE (6,310)
                    WRITE (6,250) ICALL, IA(I)
                    WRITE (6,260) TBLI2(1029), TBLI2(1045), AL6
                    WRITE (6,270)
                    WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),  &
                    TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),  &
                    TBLI2(J+106), TBLI2(J+121), J = 1030, 1044)
                    WRITE (6,290)
                    WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),       &
                    TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),         &
                    TBLI2(J+211), TBLI2(J+226),                       &
                    TBLI2(J+241), J = 1030, 1044)
                    E5 = 1
                  END IF
               CASE (6)
                  IF (F5 .NE. 1) THEN
                    WRITE (6,10) IRR, ICASE
                    WRITE (6,310)
                    WRITE (6,250) ICALL, IA(I)
                    WRITE (6,260) TBLI2(1286), TBLI2(1302), AL2
                    WRITE (6,270)
                    WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),  &
                    TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),  &
                    TBLI2(J+106), TBLI2(J+121), J = 1287, 1301)
                    WRITE (6,290)
                    WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),       &
                    TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),         &
                    TBLI2(J+211), TBLI2(J+226),                       &
                    TBLI2(J+241), J = 1287, 1301)
                    F5 = 1
                  END IF
               END SELECT
            IF (G5 .EQ. 1) RETURN
         END DO
         IF (I4 .EQ. 0) RETURN
         IA(I) = I4
         G5 = 1
      END DO
      RETURN
   10 FORMAT ('REFERNCE RUN NO. ', I6, 4X, 'CASE NO. ', I6)
   20 FORMAT (4X, 'IEND', 1X,'KEND', 1X, 'I4', 4X, 'S-ULT.', 3X,      &
      '---S---',3X,'C-BAR-',3X,'---NEND---',2X,'L1', 2X, 'IW1',       &
      2X, 'IW2', 2X, 'IW3', 2X, 'IW4', 2X, 'IW5')
   25 FORMAT (4X, 'IEND', 1X, 'KEND', 1X, 'I4', 4X, 'S-ULT.', 3X,     &
               '---S---', 3X, 'C-BAR-', 3X, '---NEND---', 3X, 'L1',   &
               3X, 'IW1', 2X, 'IW2', 2X, 'IW3', 2X, 'IW4', 2X,        &
               'IW5', 2X, 'V.T.CHORD', 'V.T.AREA.')
   30 FORMAT (5X, I2, 3X, I2, 2X, I2, F12.3, F8.2, 2X, F7.2,          &
              3X, F10.0, 3X, I1, 3X, I1, 4X, I1, 4X, I1, 4X,          &
              I1, 4X, I1)
   35 FORMAT(5X, I2, 3X, I2, 2X, I2, F12.3, F8.2, 2X, F7.2,           &
             3X, F10.0, 3X, I1, 3X, I1, 4X, I1, 4X, I1, 4X, I1,       &
             4X, I1, 3X, F8.2, 2X, F8.2)
   40 FORMAT(2X, 'SEG.', 1X, 'M3', 2X, 'M5', 2X, 'ISTRES', 2X,        &
      'IA', 2X, 'IFLAG', 1X, 'H------P------', 2X, 'N6', 2X, 'N2',    &
       2X, '-----AM-----', 3X, '--------F--------', 10X,              &
      'DELTA Y1', 10X, 'DELTA Y11')
   50 FORMAT (3X, I2, 2X, I2, 2X, I2, 4X, I2, 4X, I2, 4X, I1, 4X,     &
              F13.0, 2X, I2, 2X, I2, 2X, F13.0, 3X, F17.4, 3X,        &
              F17.4, 3X, F17.4)
   60 FORMAT ('SEG.', 1X, 'H---N SUB 01---', 2X '---N SUB 02---',     &
      2X, '---N SUB 03---', 2X, '--SIG DY1--', 2X, '--SIG DY2--',     &
      2X, '--SIG DY3--')
   70 FORMAT (1X, I2, 2X, F14.4, 2X, F14.4, 2X, F14.4, 2X, F11.4,     &
              2X, F11.4, 2X, F11.4)
   80 FORMAT (2X, 'SEG.', 3X, 'KSIGMA', 3X, 'SLOPE', 5X, 'VE', 4X,    &
      '-----W----', 6X, 'P1', 10X, 'P2', 9X, 'B1', 7X, 'B2', 10X,     &
      'A-BAR', 5X, '-------T------', 4X, 'N')
   90 FORMAT (3X, I2, 3X, F8.5, 1X, F6.2, 2X, F7.2, 2X, F10.0, 2X,    &
              F10.6, 2X, F10.6, 2X, F7.3, 2X, F7.3, F16.6, 2X,        &
              F14.3, 3X, I2)
  100 FORMAT (3X, 'SEG.', 4X, 'AIR DENSITY RATIO', 5X,                &
      'SCALE OF TURBULENCE')
  105 FORMAT (3X, 'SEG.', 4X, 'AIR DENSITY RATIO', 5X,                &
      'SCALE OF TURBULENCE', 11X, 'SFC - T*', 'SFC - TO', 5X,         &
      '    IYAW   ')
  110 FORMAT (4X, I2, 9X, F9.5, 14X, F10.3)
  115 FORMAT (4X, I2, 9X,F 9.5,14X,F10.3,14X,F8.4,6X,F8.4,8X,         &
              F16.0)
  120 FORMAT (4X, 'LL', 4X, 'STRESS TBL1', 2X, 'STRESS TBL2', 2X,     &
      'STRESS TBL3', 2X, 'STRESS TBL4', 2X, 'STRESS TBL5', 2X,        &
      'STRESS TBL6', 2X, 'STRESS TBL7', 2X, 'STRESS TBL8')
  130 FORMAT (1H0)
  140 FORMAT (4X, I2, 3X, F13.2, 1X, F13.2, F13.2, F13.2 F13.2,       &
              F13.2, F13.2, F13.2)
  160 FORMAT (4X, 'LL', 4X, 'DELTA Y--TABLE 1', 2X,                   &
      'DELTA Y--TABLE 2', 2X, 'DELTA Y--TABLE 3', 2X,                 &
      'DELTA Y--TABLE 4', 2X, 'DELTA Y--TABLE 5', 2X,                 &
      'DELTA Y--TABLE 6')
  170 FORMAT (4X, I2, F18.3, F18.3, F18.3, F18.3, F18.3, F18.3)
  180 FORMAT (4X, 'LL', 4X, 'CUM CYCLES TBL 1', 2X                    &
      'CUM CYCLES TBL 2', 2X, 'CUM CYCLES TBL 3', 2X                  &
      'CUM CYCLES TBL 4', 2X, 'CUM CYCLES TBL 5', 2X,                 &
      'CUM CYCLES TBL 6')
  190 FORMAT (4X I2, F19.3, F18.3, F18.3, F18.3, F18.3, F18.3)
  200 FORMAT (4X, 'LL', 4X, 'STRESS TBL 9', 2X, 'STRESS TBL 10',      &
      1X, 'STRESS TBL 11', 1X, 'STRESS TBL 12', 1X,                   &
      'STRESS TBL 13', 1X, 'STRESS TBL 14', 1X, 'STRESS TBL 15',      &
      1X, 'STRESS TBL 16')
  210 FORMAT (4X, 'LL', 4X, 'MAX STRESS(1)', 2X, 'MIN STRESS(1)',     &
      6X, 'CYCLES(1)', 5X, 'MAX STRESS(2)', 2X, 'MIN STRESS(2)',      &
      6X, 'CYCLES(2)')
  220 FORMAT (4X, I2, F17.0, F15.0, F18.0, F15.0, F15.0, F18.0)
  230 FORMAT (4X, 'LL', 4X, 'MAX STRESS(3)', 2X,                      &
      'MIN STRESS(3)', 6X,'CYCLES(3)')
  240 FORMAT (4X, I2, F17.0, F15.0, F18.0)
  250 FORMAT (2X, 'S-N TABLE = ',I2, 2X, 'IA AND/OR I4 = ',I2)
  260 FORMAT (4X, 'NO. OF Y ENTRIES = ',F4.0, 4X,                     &
       'NO. OF X ENTRIES = ',F4.0, 2X, 'MAX CYCLES TO FAILURE = ',    &
        F12.0)
  270 FORMAT (7X, 'Y', 12X, 'X', 10X, 'Y1,X', 9X, 'Y2,X', 9X,         &
      'Y3,X', 9X, 'Y4,X', 9X, 'Y5,X', 9X, 'Y6,X', 9X, 'Y7,X')
  280 FORMAT (1X,F12.3, F14.4, F13.0, F13.0, F13.0,                   &
      F13.0, F13.0, F13.0, F13.0)
  290 FORMAT (8X, 'Y8,X', 10X, 'Y9,X', 9X, 'Y10,0', 9X, 'Y11,X',      &
      9X, 'Y12,X', 9X, 'Y13,X', 9X, 'Y14,X', 9X, 'Y15,X')
  300 FORMAT (4X, F12.0, 2X, F12.0, 2X, F12.0, 2X, F12.0, 2X,         &
      F12.0, 2X, F12.0, 2X, F12.0, 2X, F12.0)
  310 FORMAT ('THE FOLLOWING DATA IS INPUT DATA')
  320 FORMAT ('STRESS TABLES, S = X = F(Y). LL 1 = NO. OF Y '         &
      'ENTRIES LL 2 - 16 = Y VALUES, LL 17-31 = X VALUES')
      END

!----------------------------------------------------------------------
      SUBROUTINE SPECSM
      COMMON X, Y
      COMMON CSUM, CYCLSM, MAXN, DY, STSMXM, STSMNM
      REAL :: X(3958), Y(3958)
      REAL :: CSUM(40,25), CYCLSM(40,25), DY(40,25), STSMXM(40,25), STSMNM(40,25)
      INTEGER :: N2(40), MAXN(40), M3(40), N(40), IEND, IRR, ICASE

      EQUIVALENCE (IEND, X(1)), (IRR, X(201)), (ICASE, X(202))
      EQUIVALENCE (M3, X(1673)), (N, X(1793)), (N2, X(3558))

      INTEGER :: I, J, L, L5, M, M9, M10, K, JEND, KJ

      I = 1
      L = 0
      L5 = 0
      M = 1
      M9 = 0
      M10 = 0
   20 K = 0
      IF (N2(I) .EQ. 0)  THEN
         I=I+1
         M=0
      ELSE
         L = L + 1
         L5 = L5 + 1
         N(L) = N(I)
         MAXN(L) = N(I)
         M3(L) = M3(I)
         JEND = N(L)
         DO J = 1, JEND
             K = K + 1
             DY(L,K) = DY(I,J)
             CSUM(L,K) = CSUM(I,J)
             IF ((M3(I) .GT. 9) .AND. (M3(I) .LT. 13)) CSUM(L,K) = CYCLSM(I,J)
         END DO
      END IF

      DO WHILE (I .LE. IEND)
         M = M + 1
         M10 = 0
         IF (N2(I).EQ.0 .OR. M.GT.IEND) THEN
            I=I+1
            M=0
            CYCLE
         END IF

         IF (N2(I) .NE. N2(M)) CYCLE
         IF (I .EQ. M) GOTO 20
         IF (I .GT. M) THEN
            I=I+1
            M=0
            CYCLE
         END IF
         K = 0

         IF (N(L) .LT. N(M)) THEN
            N(L) = N(M)
            M9 = 1
            IF (MAXN(L) .LT. N(M)) THEN
               MAXN(L) = N(M)
               M10 = 1
            END IF
         ELSE
            IF (N(L) .GT. N(M)) THEN
               N(L) = N(M)
               M9 = 2
            END IF
         END IF

         JEND = N(L)
         DO J = 1, JEND
            K = K + 1
            IF ((M9 .EQ. 1) .AND. (M10 .EQ. 1)) DY(L,K) = DY(M,J)
            CSUM(L,K) = CSUM(L,K) + CSUM(M,J)
            IF ((M3(I).GT.9).AND.(M3(I).LT.13)) CSUM(L,K)=CSUM(L,K)+CYCLSM(M,J)
         END DO
      END DO

      DO L = 1, L5
         IF ((M3(L) .LT. 10) .OR. (M3(L) .GT. 12)) THEN
            WRITE (6,10) IRR, ICASE
            WRITE (6,140)
            WRITE (6,180)
         ELSE
            WRITE (6,10) IRR, ICASE
            WRITE (6,140)
            WRITE (6,150)
         END IF
         IF (N(L) .LT. MAXN(L)) N(L) = MAXN(L)
         JEND = N(L)
         KJ = N(L) -1
         DO K = 1, KJ
            CYCLSM(L,K) = CSUM(L,K) - CSUM(L,K+1)
         END DO
         DO K = 1, JEND
            IF ((M3(L) .GT. 9) .AND. (M3(L) .LT. 13)) THEN
               WRITE(6,230)L,K,STSMXM(L,K),STSMNM(L,K),CSUM(L,K)
            ELSE
               WRITE (6,210) L, K, DY(L,K), CSUM(L,K)
               IF (K .EQ. JEND) CYCLE
               WRITE (6,250) CYCLSM(L,K)
            END IF
         END DO
      END DO

      CSUM = 0.0

      RETURN
   10 FORMAT ('REFERENCE RUN NO.', I6, 4X, 'CASE NO.', I6)
  140 FORMAT ('THE FOLLOWING DATA IS THE SUMMATION OF THE'            &
      'SPECTRA FOR THE SEGMENTS SPECIFIED BY FLAG L = N2')
  150 FORMAT (3X, 'FL', 3X, 'LL', 5X, 'MAX STRESS', 5X,               &
      'MIN STRESS', 12X, 'CYCLES')
  180 FORMAT (3X, 'FL', 3X, 'LL', 5X, 'DELTA Y', 5X,                  &
      'CUMMULATIVE CYCLES', 10X, 'CYCLES')
  210 FORMAT (2X, I2, 3X, I2, 2X, F13.3, 3X, F16.4)
  230 FORMAT (1H0, 2X, I2, 3X, I2, 4X, F11.0, 4X, F11.0, 7X,F16.4)
  250 FORMAT (46X, F16.4)
      END

!----------------------------------------------------------------------
      SUBROUTINE ONEVAR (ARGUMT, TABLE, OUTPUT, NSEGNM)
!     ONEVAR IS A INTERPOLATION ROUTINE - ONE FUNCTION OF ONE
!     VARIABLE, I.E. X=F(Y) - LINEAR OR QUADRATIC.
!     ARGUMEENTS OF THE SUBROUTINE ARE AS FOLLOWS:
!       ARGUMT  = INPUT INTERPOLATION ARGUMENT (Y)
!       NXDIR   = TYPE OF INTEROLATION, 1 FOR LINEAR, 2 FOR QUAD.
!       TABLE   = SET OF Y VALUES FOLLOWED BY THE X VALUES
!       OUTPUT  = INTERPOLATED VALUE OF X = F(Y)
!       NER     = ERROR CODE
!                 1 = OK, INTERPOLATION SUCCESSFUL.
!                 2 = OFF CHART LOW END, MIN. VAL. SUBSTITUTED
!                 3 = OFF CHART HIGH, MAX. VAL. SUBSTITUTED
!                 4 = NO. OF X ENTRIES IS NOT 2 TO 15 (IF NXDIR
!                     IS 1). OR, IT IS NOT 3 TO 15 (IF NXDIR) IS 2).
!                 5 = Y ENTRIES NOT IN ASCENDING ORDER.
      IMPLICIT NONE
      REAL TABLE(31), ARGUMT, OUTPUT
      INTEGER NSEGNM, NOENTR, NXDIR, NER, I
      NOENTR = INT(TABLE(1) + 0.49999)
      NXDIR = 1
      NER = 1
      IF (NOENTR.LT.2 .OR. NOENTR.GT.15) THEN
         WRITE (6,330)
         NER=4
         RETURN
      END IF
      IF (ANY(TABLE(2:NOENTR).GE.TABLE(3:NOENTR+1))) THEN
         WRITE (6,350)
         NER=5
         RETURN
      END IF
      IF (ARGUMT.LT. TABLE(2)) THEN
         WRITE (6,280) ARGUMT
         NER=2
         OUTPUT = TABLE(2 + 15)
         RETURN
      END IF
      IF (ARGUMT.GT. TABLE(NOENTR+1)) THEN
         WRITE (6,310) ARGUMT
         NER=3
         OUTPUT = TABLE(NOENTR+1 + 15)
         RETURN
      END IF
      I = MINLOC(TABLE(2:NOENTR+1),1,                                &
          MASK=(TABLE(2:NOENTR+1).GT.ARGUMT))
      OUTPUT = TABLE(I+15) + (ARGUMT-TABLE(I))*                      &
               (TABLE(I+16)-TABLE(I+15))/(TABLE(I+1)-TABLE(I))
      RETURN
  280 FORMAT('ONEVAR INTEROLATION ERROR. Y IS TOO SMALL. Y = ',E14.6)
  310 FORMAT('ONEVAR INTEROLATION ERROR. Y IS TOO LARGE. Y = ',E14.6)
  330 FORMAT ('ONEVAR INTERP. ERROR. THE NO. OF Y ENTRIES IS',       &
              ' EITHER TOO SMALL OR TOO LARGE.')
  350 FORMAT ('ONEVAR INTERP. ERROR. THE NO. OF Y ENTRIES ARE',      &
              ' NOT IN ASCENDING ORDER.')
      END

!----------------------------------------------------------------------
      SUBROUTINE TWOVIN (XARG, YARG, TABLE, OUTPUT, NSEG, LEVEL)

!     ARUGMENTS OF THE SUBROUTINE ARE AS FOLLOWS:
!     XARG   = INPUT INTERPOLATION ARGUMENT (X)
!     YARG   = INPUT INTERPOLATION ARGUMENT (Y)
!     TABLE  = SET OF VALUES. SEE DESRIPTION OF THIS
!     OUTPUT = INTERPOLATED VALUE OF Z = F(X,Y)
      REAL :: XARG, YARG, TABLE(257), OUTPUT
      INTEGER :: NSEG, LEVEL
      INTEGER :: J5, K, NER, NER2, NER5, NXDIR, NXENTR, NYENTR, JK
      INTEGER :: IY, IX, IBOUND, IN
      REAL :: CYINT, CKINT, AN1, AN2, AN3, AN4, AN5, AN6, AN7
      REAL :: BX, CX, DX, EX, FX, GX, HX, XARGMX

      IX=0
      IY=0
      J5 = 0
      K = 0
      NER = 0
      NER2 = 0
      NER5 = 0
      NXDIR = 2
      NXENTR = INT(TABLE(17) + 0.499999)
      NYENTR = INT(TABLE(1) + 0.499999)
      DO JK = 1,NYENTR
         IY = JK
         IF (IY.GT.1) THEN
            IF (IY .LT.  NYENTR) THEN
               IF (TABLE(IY+1) .GT. TABLE(IY)) THEN
                  IF (TABLE (IY + 1) .LT. YARG) THEN
                     CYINT = (YARG - TABLE(IY + 1))             &
                             / (TABLE(IY +1) - TABLE(IY))
                     IF (CYINT .LT. 0.001) EXIT
                     CYCLE
                  END IF
                  EXIT
               END IF
               NER = 9
               WRITE (6,750)
               RETURN
            END IF
            IF (TABLE(IY+1) .GT. YARG) EXIT
            IF (TABLE(IY+1) .LT. YARG) THEN
               NER = 3
               NER2 = 13
            END IF
            J5 = 1
            IY = IY + 1
            EXIT
         END IF
         IF (TABLE(IY + 1) .GT. YARG) THEN
            NER = 2
            NER2 = 12
         END IF
         IF (TABLE(IY + 1) .GE. YARG) THEN
            IY = IY + 1
            J5 = 1
            EXIT
         END IF
         CYINT = (YARG - TABLE(IY + 1)) / (TABLE(IY+2) - TABLE(IY+1))
         IF (CYINT .LT. 0.001) THEN
            IY = IY + 1
            J5 = 1
            EXIT
         END IF
      END DO

      DO 460 JK = 1,NXENTR
         IX = JK
         IF (IX -1) 420,420,320
  320    IF (IX - NXENTR) 330,390,390
  330    IF (TABLE(IX + 17) - TABLE(IX + 16))340,340,450
  340    NER = 9
         GO TO 740
  350    NER = 5       ! corrected by PSV
         NER5 = 13     !
  360    IX = IX + 1
         IF (J5 .EQ. 1) GO TO 470
         GO TO 480
  370    CKINT = ABS((TABLE(IX+17)-XARG) / (TABLE(IX+18)-TABLE(IX+17)))
         IF (CKINT - 0.50)480,480,460
  390    IF (TABLE(IX+17) - XARG)350,360,400
  400    CKINT = ABS((TABLE(IX+17)-XARG)/(TABLE(IX+17)-TABLE(IX+16)))
         IF(CKINT - 0.50)410,410,350
  410    IX = IX - 1
         goto 480 !psv
  420    IF (TABLE(IX+17) - XARG) 460,460,430
  430    NER = 4
         IX = IX + 1
         IF (J5 .EQ. 1) GO TO 470
         GO TO 480
  450    IF (TABLE (IX+17) - XARG) 370,370,480
  460 CONTINUE

  470 IN = 15 * IY + 1 + IX
      OUTPUT = ALOG10(TABLE(IN))
      GO TO 710
  480 IN = 15 * IY + 1 + IX
      AN1 = ALOG10(TABLE(IN))
      IBOUND = (NXENTR - 1)
      IF (IX .GT. NXENTR) GO TO 510
      AN2 = ALOG10(TABLE(IN + 1))
      IF ((IX .EQ. NXENTR) .AND. (J5 .EQ. 1)) GO TO 500
      IF ((IX .EQ. NXENTR) .AND. (J5 .EQ. 0)) GO TO 490
      AN3 = ALOG10(TABLE(IN + 2))
      IF (IY .GT. NYENTR) GO TO 500
      AN5 = ALOG10(TABLE(IN + 17))
      AN6 = ALOG10(TABLE(IN + 16))
      IF (IX .EQ. IBOUND) GO TO 510
      AN4 = ALOG10(TABLE(IN + 18))
      GO TO 510
  490 AN6 = ALOG10(TABLE(IN + 16))
      GO TO 510
  500 AN4 = AN2
      AN5 = AN2
      AN6 = AN1
  510 IF (NXDIR - 1) 520,520,600
  520 IF (J5 - 1) 540,530,540
  530 BX = 0.0
      GO TO 550
  540 BX = ((YARG - TABLE(IY)) / (TABLE(IY + 1) - TABLE(IY)))
  550 IF (IX - NXENTR) 570,570,560
  560 OUTPUT=ALOG10(TABLE(IN))+BX*(ALOG10(TABLE(IN+15))-ALOG10(TABLE(IN)))
      GO TO 710
  570 XARGMX = TABLE(NXENTR + 17)
      IF (TABLE(IX + 17) - XARGMX) 590,580,580
  580 TABLE(IX + 18) = TABLE(IX + 17)
      TABLE(IN + 17) = TABLE(IN + 16)
  590 CX = XARG - (TABLE(IX + 16) + BX*(TABLE(IX+17) - TABLE(IX+16)))
      DX = ALOG10(TABLE(IN + 1)) + BX * (ALOG10(TABLE(IN + 17)) -     &
      ALOG10(TABLE(IN + 1)))
      EX = ALOG10(TABLE(IN)) + BX * (ALOG10(TABLE(IN + 16)) -         &
      ALOG10(TABLE(IN)))
      FX = TABLE(IX + 17) - TABLE(IX + 16) + BX * (TABLE(IX + 18)     &
           - 2.0 * TABLE (IX + 17) + TABLE(IX + 16))
      IF (ABS(FX) .LT. 1E-6) FX = 1.0
      OUTPUT = ALOG10(TABLE(IN)) + BX * (ALOG10(TABLE(IN + 16)) -     &
      ALOG10(TABLE(IN))) + (((CX) * (DX - EX)) / (FX))
      GO TO 710
  600 IF (J5 - 1) 620,610,620
  610 BX = 0.0
      GO TO 630
  620 BX = ((YARG - TABLE(IY)) / (TABLE(IY + 1) - TABLE(IY)))
  630 IF (IX - NXENTR) 650,650,640
  640 AN7 = ALOG10(TABLE(IN + 15))
      OUTPUT = AN1 + BX * (AN7 - AN1)
      GO TO 710
  650 IBOUND = NXENTR - 1
      IF (IX .EQ. IBOUND) GO TO 670
      XARGMX = TABLE(NXENTR + 17)
      IF (TABLE(IX + 17) - XARGMX) 680,660,660
  660 TABLE(IX + 18) = TABLE(IX + 17)
      AN3 = AN2
      AN5 = AN2
  670 TABLE(IX + 19) = TABLE(IX + 17)
      IF (IX .EQ. IBOUND) TABLE(IX + 19) = TABLE(IX + 18)
      AN4 = AN5
  680 CX = TABLE (IX + 18) + BX * (TABLE(IX + 19) -                   &
           TABLE (IX + 18))
      DX = TABLE (IX + 17) + BX * (TABLE(IX + 18) -                   &
           TABLE (IX + 17))
      EX = TABLE (IX + 16) + BX * (TABLE(IX + 17) -                   &
           TABLE (IX + 16))
      FX = AN3 + BX * (AN4 - AN3)
      GX = AN2 + BX * (AN5 - AN2)
      HX = AN1 + BX * (AN6 - AN1)
      IF (( IX .GE. IBOUND) .AND. ((BX * 1.001) .GE. 1.0))            &
         GO TO 700
      OUTPUT = (HX * (((XARG - DX) * (XARG - CX)) / ((EX - DX) *      &
      (EX - CX)))) + (GX * (((XARG - EX) * (XARG - CX)) /             &
      ((DX - EX) * (DX - CX)))) + (FX * (((XARG - EX) *               &
      (XARG - DX)) / ((CX - EX) * (CX - DX))))
      GO TO 710
  700 OUTPUT = GX + (HX - GX) * ((DX - XARG) / (DX - EX))
  710 IF (NSEG .EQ. 50) GO TO 720
      IF (NSEG .GT. 50) GO TO 730
      IF (NER .EQ. 2) WRITE(6,10) YARG, NSEG, LEVEL
      IF (NER .EQ. 3) WRITE(6,20) YARG, NSEG, LEVEL
      IF ((NER.EQ.5) .AND. (NER2.EQ.0)) WRITE(6,30) XARG,NSEG, LEVEL
      IF ((NER2.EQ.12) .AND. (NER5.EQ.13)) WRITE(6,40)XARG,YARG,      &
         NSEG, LEVEL
      IF ((NER2.EQ.13) .AND. (NER5.EQ.13)) WRITE(6,50)XARG,YARG,      &
         NSEG, LEVEL
      GO TO 760
  720 IF (NER .EQ. 2) WRITE(6,60) YARG, XARG
      IF (NER .EQ. 3) WRITE(6,70) YARG, XARG
      IF ((NER .EQ. 5) .AND. (NER2 .EQ. 0)) WRITE(6,80) XARG,         &
         NSEG, LEVEL
      IF ((NER2 .EQ. 12) .AND. (NER5 .EQ. 13)) WRITE(6,90) XARG,      &
         YARG
      IF ((NER2 .EQ. 13) .AND. (NER5 .EQ. 13)) WRITE(6,100) XARG,     &
         YARG
      GO TO 760
  730 NSEG = NSEG - 50
      IF (NER .EQ. 2) WRITE (6,110) YARG, NSEG
      IF (NER .EQ. 3) WRITE (6,120) YARG, NSEG
      IF ((NER .EQ. 5) .AND. (NER2 .EQ. 0)) WRITE(6,130) XARG, NSEG
      IF ((NER2 .EQ. 12) .AND. (NER5 .EQ. 13)) WRITE(6,140) XARG,     &
         YARG, NSEG
      IF ((NER2 .EQ. 13) .AND. (NER5 .EQ. 13)) WRITE(6,150) XARG,     &
         YARG, NSEG
      GO TO 760
  740 WRITE (6,750)
  760 RETURN
   10 FORMAT ('SN INTERP. ERROR. Y IS TOO SMALL. Y = ',E14.6,2X,   &
      'SEG = ', I3, 2X, 'LOAD LEVEL =', I3)
   20 FORMAT ('SN INTERP. ERROR. Y IS TOO LARGE. Y = ',E14.6,2X,   &
      'SEG = ', I3, 2X, 'LOAD LEVEL = ', I3)
   30 FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,   &
      'SEG = ', I3, 2X, 'LOAD LEVEL = ', I3, 2X)
   40 FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,   &
      'Y IS TOO SMALL. Y = ',E14.6, 2X, 'SEG = ', I3, 2X,          &
      'LOAD LEVEL = ', I3)
   50 FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,   &
      'Y IS TOO LARGE. Y = ',E14.6, 2X, 'SEG = ', I3, 2X,          &
      'LOAD LEVEL = ', I3)
   60 FORMAT ('SN INTERP. ERROR. Y IS TOO SMALL. Y = ',E14.6,2X,   &
      'X = ',E14.6, 2X, '(GAG SEGMENT)')
   70 FORMAT ('SN INTERP. ERROR. Y IS TOO LARGE. Y = ',E14.6,2X,   &
      'X = ',E14.6, 2X, '(GAG SEGMENT)')
   80 FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,   &
      'Y = ',E14.6, 2X, '(GAG SEGMENT)')
   90 FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,   &
      'Y = ',E14.6, 2X, '(GAG SEGMENT)')
  100 FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,   &
      'Y IS TOO LARGE. Y = ',E14.6, 2X, '(GAG SEGMENT)')
  110 FORMAT ('GUST ALLEV. INTRP. ERROR. Y IS TOO SMALL',          &
      'Y = ',E14.6, 2X, 'SEG =', I3)
  120 FORMAT ('GUST ALLEV. INTRP. ERROR. Y IS TOO LARGE',          &
      'Y = ',E14.6, 2X, 'SEG =', I3)
  130 FORMAT ('GUST ALLEV. INTRP. ERROR. X IS TOO LARGE',          &
      'X = ',E14.6, 2X, 'SEG =', I3)
  140 FORMAT ('GUST ALLEV. INTRP. ERROR. X IS TOO LARGE',          &
      'X = ',E14.6, 2X,'Y IS TOO SMALL. Y = ',E14.6,1X,'SEG =',2I3)
  150 FORMAT ('GUST ALLEV. INTRP. ERROR. X IS TOO LARGE',          &
      'X = ',E14.6,2X,'Y IS TOO LARGE. Y = ',E14.6,1X,'SEG =', 2I3)
  750 FORMAT ('TWOVIN INTERPOLATION ERROR. EITHER THE'             &
       'X OR THE Y ENTRIES ARE NOT IN ASCENDING ORDER.')
      END

!----------------------------------------------------------------------
      SUBROUTINE RDMAIN
!***  MAIN PROGRAM FOR THE SPECTRUM LOADING SEQUENCE GENERATION
!***  PROGRAM.  IT READS AND PRINTS THE TITLE AND SOME OF THE
!***  CONSTANTS USED IN THE PROGRAM.
!***  NOTE - THE SIZE OF THE DYNAMIC WORKING ARRAY IS SET BY THE
!***         DIMENSIONS OF A AND BY NSIZE.  THROUGH OUT THE
!***         SPECTRUM LOADING SEQUENCE GENERATION PROGRAM, CORE
!***         ALLOCATION IS DONE THROUGH IMPLIED EQUIVALENCES.
!***  SUBROUTINES CALLED - ERROR, INPUTF, NEWPG
      REAL :: A(28000)
      INTEGER :: N(28000)
      EQUIVALENCE (A(1), N(1))
      COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI,  &
             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP,     &
             ELIMV, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS,     &
             MAXHP, KF, KC, A
      INTEGER :: NPG, NSIZE, LEFT, IERR, IRS, IUIL, NPI, NRAN
      INTEGER :: IVP, IFI, NXY, IPFS, NPSS, IPTF, IAFS, NEXT, NOW
      INTEGER :: IFRS, MAXHP, KF, KC
      REAL :: CLIP, CLIV, FACTOR, ELIMP, ELIMV
      INTEGER :: I, IPI, IRAN, KVP, NFT, NST
      CHARACTER*4 :: BLANK, TITLE
      DATA BLANK/'    '/
      N=0
      NSIZE = 28000
      NEXT = 1
      NOW = 1
      TITLE = BLANK
      READ (5,'(20A4)') (TITLE(I), I = 1,20)
      NPG = 1
      CALL NEWPG
      IERR = 0
      READ (5,*) NFT, IRS, IFRS, IUIL, IPI, IRAN, KVP, IFI, NXY,    &
                 IPFS, NPSS, IPTF, IAFS, MAXHP, KF, KC
      WRITE (6,30) NFT, IRS, IFRS, IUIL, IPI, IRAN, KVP, IFI, NXY,  &
                   IPFS, NPSS, IPTF, IAFS, MAXHP
      WRITE (6,40) KF, KC
      NRAN = IABS(IRAN)
      NPI = IABS(IPI)
      IVP = IABS(KVP)
      READ (5,*) CLIP, CLIV, FACTOR, ELIMP
      WRITE (6,50) CLIP, CLIV, FACTOR, ELIMP
      LEFT = NSIZE - 2 * NFT
      IF (LEFT .LT. 0) CALL ERROR (1, LEFT, NSIZE, NFT)

      REWIND IUIL
      CALL INPUTF(NFT, N(1), N(NFT+1), N(2*NFT+1), NST, IRAN, IPI, KVP)

      RETURN
   30 FORMAT (                                                      &
      15X, 'NUMBER OF FLIGHT TYPES IS.......................', I5/  &
      15X, 'VALLEY/PEAK COUPLING IS..(1=A6PA, 2=RANDOM).....', I5/  &
      15X, 'FLIGHT SEQUENCE IS..(C=RANDOM...................', I5/  &
      15X, '..........(N=INPUT FLIGHT SEQUENCE, NSETS)......', /    &
      15X, 'FORTRAN UNIT NUMBER IS..........................', I5/  &
      15X, 'NUMBER OF INPUT PEAK LEVELS IS..................', I5/  &
      15X, 'NUMBER OF INPUT RANGE LEVELS IS.................', I5/  &
      15X, 'NUMBER OF INPUT VALLEY/PEAK RATIOS IS...........', I5/  &
      15X, 'SAVE SPECTRUM ON MAGNETIC TAPE..................', I5/  &
      15X, '..........................(0 = NO, 3 = YES).....', /    &
      15X, 'NUMBER OF POINTS OF THE RANGE VS R INPUT CURVE '        &
           'IS....', I5/                                            &
      15X, 'PRINT FLIGHT SEQUENCE (-1 = NO, 0 = ALL, N = '          &
           'NUMBER).', I5/                                          &
      15X, 'NUMBER OF SPECTRUM SUMMATIONS TO BE PRINTED ....', I5/  &
      15X, 'PROGRAM TERMINATION FLAG (0 = NORMAL, ..........', /    &
      15X, '..................M = STOP AFTER M FLIGHTS......', I5/  &
      15X, 'ALTERNATE FLIGHT SEQUENCE FLAG..................', I5/  &
      15X, '.(0 = REANDOM, 1=LOW-HI, 2=HI-LOW, 3=LO-HI-LO)....', /  &
      15X, 'NUMBER OF HIGHEST PEAKS PER FLIGHT TO BE PRINTED.',I5)
   40 FORMAT (                                                      &
      15X, 'STARTING VALUE FOR THE GENERATION OF RANDOM.......',/   &
      15X, '.FLIGHT NUMBERS (0 = DEFAULT DEFINED AS 11111).', I5/   &
      15X, 'STARTING VALUE FOR THE RANDOM CYCLE GENERATION.', I5/   &
      15X, '.........(0 = DEFAULT DEFINED AS 12345)...........')
   50 FORMAT (                                                      &
       15X,'PEAK CLIPPING VALUE IS.............................',   &
       F10.0 / 15X, 'VALLEY CLIPPING VALUE IS....................'  &
       '.....', F10.0 / 15X, 'MULTIPLICATION FACTOR IS..........'   &
       '...............', F10.5 / 15X, 'CYCLE ELIMINATION PEAK'     &
       'VALUE IS.................', F10.0//)
      END

      SUBROUTINE INPUTF (NFT, NF, NS, A, NST, IRAN, IPI, KVP)
!***  THIS SUBROUTINE READS IN THE NUMBER OF FLIGHTS
!***  AND THE NUMBER OF SEGMENTS IN EACH FLIGHT TYPE.
!***  IT ALSO SETS UP SOME IMPLIED EQUIVALENCES TO
!***  THE A ARRAY.
!***  SUBROUTINES CALLED ERROR, INF1F2

      INTEGER :: NFT, NF(NFT), NS(NFT), NST, IRAN, IPI, KVP
      REAL :: A(*)

      COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI, &
             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP,    &
             ELIMV, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS

      INTEGER :: NPG, TITLE, NSIZE, LEFT, IERR, IRS, IUIL, NPI, NRAN
      INTEGER :: IVP, IFI, NXY, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS
      REAL :: CLIP, CLIV, FACTOR, ELIMP, ELIMV

      INTEGER :: MPI, MRAN, MVP, MXY, MISS, MF1, MF2, MFRS, MN
      INTEGER :: NF1ST, NF2ST, NTF

!***  READ IN THE NUMBER OF FLIGHTS IN EACH FLIGHT TYPE
      READ (5,*) NF
!***  READ IN THE NUMBER OF SEGMENTS IN EACH FLIGHT TYPE
      READ (5,*) NS

!***  COMPUTE THE TOTAL NUMBER OF FLIGHTS
!***  AND THE TOTAL NUMBER OF SEGMENTS.
      NST = SUM(NS(1:NFT))
      NTF = SUM(NF(1:NFT))

!***  CALCULATE THE STARTING POIT FOR EACH ARRAY WITHIN THE A ARRAY.
      MPI = 1
      MRAN = MPI + NPI
      MVP = MRAN + NRAN
      MXY = MVP + IVP
      MISS = MXY + 2 * NXY
      MF1 = MISS + NPSS
      MF2 = MF1 + NST
      MFRS = MF2 + NST
      MN = MFRS + (2 * IFRS)
      LEFT = LEFT - (2*(NST+NXY+IFRS) + IVP + NRAN + NPI + NPSS)
      IF (LEFT .LT. 0) CALL ERROR (1, LEFT, NSIZE, NST)
      CALL INF1F2 (NST, NFT, NS, A(MF1), A(MF2), A(MN), NF1ST,   &
                   NF2ST, NTF, NF, A(MPI), A(MRAN), A(MVP),      &
                   A(MXY), IRAN, IPI, KVP, A(MISS), A(MFRS))
      RETURN
      END

      SUBROUTINE INF1F2 (NST, NFT, NS, IF1, IF2, N, NF1ST, NF2ST, NTF, NF, &
                         PI, RAN, VP, XY, IRAN, IPI, KVP, ISS, NFRS)
!***  THIS SUBROUTINE READS AND PRINTS THE REMAINDER OF THE  *****
!***  INPUT DATA.  IT SETS UP THE CORE STORAGE REQUIRED FOR  *****
!***  THE CALLS TO SUBROUTINES INMMN, GENFL, AND GENAFS.     *****
!***  SUBROUTINES CALLED - ERROR, GENAFS, GENFL, INMMN,      *****
!***                       NEWPG, OPENMS, WTAPE              *****
      INTEGER :: NST, NFT, NF1ST, NF2ST, NTF, IRAN, IPI, KVP, ISS (*)
      INTEGER :: NS(NFT), IF1(NST), IF2(NST), N(*), NF(NFT), NFRS(2,1)
      REAL :: PI(*), RAN(*), VP(*), XY(2,*)
      INTEGER :: NPG, TITLE, NSIZE, LEFT, IERR, IRS, IUIL, NPI, NRAN
      INTEGER :: IVP, IFI, NXY, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS
      REAL :: CLIP, CLIV, FACTOR, ELIMP, ELIMV
      INTEGER :: MAXHP
      COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI, &
             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP,    &
             ELIMV, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS, MAXHP
      INTEGER :: LINE, I, J
      INTEGER :: MAFS, MAX, MBX, MCY, MFF, MFLG, MAXSS, MHPEAK, MIDP
      INTEGER :: MIN, MINDEX, MIRR, MJTN, MMN, MMS, MPMAX, MPR, MRR
      INTEGER :: MREC, MS2, ND, NNPI, NNRAN, NRMAX, NS1, NS2
      INTEGER :: IRR, ICASE, ISEG, IS1, IS2, ISP, JLEFT, JSS, M3
      REAL :: DEL
      DATA ISP/0/

      LINE = 60

      IF (IPI .LE.0) THEN
!***     READ IN AND PRINT ALL PEAK LEVELS(IPI) 0
         READ (5,*) (PI(I), I = 1, NPI)
      ELSE
!***     READ IN AND PRINT FIRST AND LAST PEAK LEVELS.
!***     LET THE PROGRAM COMPUTE EVENLY SPACED VALUES.
         READ (5,*) PI(1), PI(NPI)
         ND = NPI - 1
         DEL = (PI(NPI) - PI(1)) / ND
         DO i = 2, ND
            PI(I) = PI(I-1) + DEL
         END DO
      END IF
      WRITE (6,60) (PI(I), I = 1, NPI)

      IF (IRAN .LE. 0) THEN
!***     READ IN AND PRINT ALL RANGE LEVELS (IRAN) 0)
         READ (5,*) (RAN(I), I = 1, NRAN)
      ELSE
!***     READ IN AND PRINT FIRST AND LAST RANGE LEVELS.
!***     LET THE PROGRAM COMPUTE EVENLY SPACED VALUES.
         READ (5,*) RAN(1), RAN(NRAN)
         ND = NRAN - 1
         DEL = (RAN(NRAN) - RAN (1)) / ND
         DO I = 2,ND
            RAN(I) = RAN(I-1) + DEL
         END DO
      END IF
      WRITE (6,110) (RAN(I), I = 1, NRAN)

      IF (KVP .LE. 0) THEN
!***     READ IN AND PRINT ALL TEH VALLEY/PEAK RETIOS (KVP) 0)
         READ (5,*) (VP(I), I = 1, IVP)
      ELSE
!***     READ IN AND PRING FIRT AND LAST VALLEY/PEAK RATIOS.
!***     LET THE PROGRAM COMPUTE EVELY SPACED VALUES.
         READ (5,*) VP(1), VP(IVP)
         ND = IVP - 1
         DEL = (VP(IVP) - VP(1)) / ND
         DO I = 2, ND
            VP(I) = VP(I-1) + DEL
         END DO
      END IF
      WRITE (6,150) (VP(I), I = 1, IVP)
      IF (NXY .NE. 0) THEN
!***     READ IN AND PRINT THE VALLEY/PEAK RATIO VS RANGE CURVE ****
         READ (5,*) ((XY(I,J), I = 1,2), J = 1, NXY)
         WRITE (6,160) ((XY(I,J), I = 1,2), J = 1, NXY)
      END IF
      IF (NPSS .NE. 0) THEN
!***     READ IN AND PRINT NUMBER OF FLIGHTS                *******
!***     AFTER WHICH A SPECTRUM SUMMATION IS TO BE PRINTED  *******
         READ (5,*) (ISS(I), I = 1, NPSS)
         WRITE (6,180) (ISS(I), I = 1, NPSS)
      END IF

      REWIND 3
!***  READ IN AND PRINT A6PA REFERENCDE RUN, CASE NUMBER,  ******
!***  AND SEGMENTS FROM TAPE UNIT 3.                       ******
      IF (LINE .GE. 55) THEN
         CALL NEWPG
         LINE = 4
      END IF
      IS2 = 0
      JSS = 0
      NST = 0
      WRITE (6,210)
      LINE = LINE + 2
      DO I = 1, NFT
         IS1 = IS2 + 1
         NST = NST + NS(I)
         IF (NST .EQ. JSS) THEN
            IS2 = ISEG
            WRITE (6,250) I, IRR, ICASE, IS1, IS2
            LINE = LINE + 1
            IS2 = 0
            CYCLE
         END IF

         IF (NST .GE. JSS) THEN
            DO WHILE (.TRUE.)
               READ (3) IRR, ICASE, ISEG
               ISP = JSS
               JSS = JSS + ISEG
               IF (NST .LT. JSS) EXIT
               IS2 = ISEG
               WRITE (6,250) I, IRR, ICASE, IS1, IS2
               LINE = LINE + 1
               IF (NST .EQ. JSS) EXIT
            END DO
            IF (NST.EQ.JSS) THEN
               IS2 = 0
               CYCLE
            END IF
         END IF
         IS2 = NST - ISP
         WRITE (6,250) I, IRR, ICASE, IS1, IS2
         LINE = LINE + 1
      END DO
      NF1ST = 0
      NF2ST = 0
      IS1 = 1
      IS2 = 0
!***  READ IN AND PRINT F1 AND F2 SEGMENTS              ******
      IF (LINE .GE. 50) THEN
         CALL NEWPG
         LINE = 4
      END IF
      DO I = 1, NFT
         IS2 = IS2 + NS(I)
         write(6,*) ' 1932'
         READ (5,*) (IF1(J), J = IS1, IS2)
         READ (5,*) (IF2(J), J = IS1, IS2)
         WRITE (6,280) I, NF(I), NS(I)
         WRITE (6,290) (IF1(J), J = IS1, IS2)
         WRITE (6,300) (IF2(J), J = IS1, IS2)
         MAX = 0
         MBX = 0
         DO J = IS1, IS2
            IF (IF1(J) .GT. MAX) MAX = IF1(J)
            IF (IF2(J) .GT. MBX) MBX = IF2(J)
         END DO
         NF1ST = NF1ST + MAX
         NF2ST = NF2ST + MBX
         LINE = LINE + 4
         IF (LINE.GE.51) THEN
            LINE = 4
            CALL NEWPG
         END IF
         IS1 = IS1 + NS(I)
      END DO

      IF (IFRS .NE. 0) THEN
!***     READ IN USER SPECIFIED FLIGHT SEQUENCE            *******
         READ (5,*) ((NFRS(I,J), I = 1,2), J = 1, IFRS)
         WRITE (6,350) ((NFRS(I,J), I = 1,2), J = 1, IFRS)
      END IF

!***  CALCULATE THE STARTING POINT WITHIN N ARRAY        *******
!***  (WHICH IS REALLY THE A ARRAY) FOR EACH ARRAY       *******
!***  USED IN THE SUBROUTINE INMNN.                      *******
      M3 = (LEFT - NST) / 3
      MAX = 1
      MIN = MAX + M3
      MCY = MIN + M3
      MS2 = MCY + M3
      CALL INMMN (NFT, NS, IF1, IF2, NST, N(MAX), N(MIN),       &
                  N(MCY), NF1ST, NF2ST, MMN, M3, NS1, NS2, NTF, &
                  NF, N(MS2))
      DO I =1, MMN
         N(MMN + I) = N(MIN + I - 1)
         N(2 * MMN + I) = N(MCY + I -1)
      END DO

!***  CALCULATE THE STARTING POINT WITHIN THE N ARRAY    *******
!***  (WHICH IS REALLY THE A ARRAY) FOR EACH ARRAY       *******
!***  USE IN THE SUBROUTINE GENFL.                       *******
      MHPEAK = (2 + IRS) * MMN + 1
      MIDP = MHPEAK  + MAXHP
      MPMAX = MIDP + MAXHP
      MCY = MPMAX
      MINDEX = MCY
      MJTN = MINDEX
      MFF = MJTN

      IF (IAFS .NE. 0) THEN
         MCY = MPMAX + NTF
         MINDEX = MCY + NTF
         MJTN = MINDEX + (NTF + 1)
         MFF = MJTN + NTF
         CALL OPENMS (4, N(MINDEX), (NTF+1), 0)
      END IF

      MRR = MFF + 2 * NFT
      NNRAN = NRAN + 1
      NNPI = NPI + 1
      MPR = MRR + (IVP + 2) * NNRAN
      MIRR = MPR + (IVP + 2) * NNPI
      NRMAX = MAX0(NNRAN,NNPI)
      MMS = MIRR + (IVP + 2) * NRMAX
      JLEFT = LEFT
      LEFT = LEFT - MMS
      IF (LEFT .LT. 0) CALL ERROR (1,LEFT,NSIZE,MMS)
      MAXSS = LEFT / 2
      IF ((IFI .NE. 0) .AND. (IAFS .NE. 0)) REWIND IFI
!***  CALCULATE THE FLIGHT SEQENCE                       *******
      CALL GENFL (NF, NFT, NTF, NS, NST, N(1), N(MMN+1),        &
                  N(2*MMN+1), MMN, IF2, N(MMS), MAXSS, PI, RAN, &
                  VP, XY, N(MRR), NNRAN, N(MPR), NNPI, N(MIRR), &
                  NRMAX, ISS, N(MPMAX), N(MCY), N(MFF), NFRS,   &
                  N(MHPEAK), N(MIDP), N(MJTN))

!***  CALCULATE THE STARTING POINT WITHIN THE N ARRAY    *******
!***  (WHICH IS REALLY THE A ARRAY) FOR EACH ARRAY       *******
!***  USE IN THE SUBROUTINE GENAFAS.                     *******
      MREC = MIRR + (IVP + 2) * NRMAX
      MAFS = MREC + NTF
      MFLG = MAFS + NTF
      MMS = MFLG + NTF
      LEFT = JLEFT
      LEFT = LEFT - MMS
      IF (LEFT .LT. 0) CALL ERROR (1,LEFT,NSIZE,MMS)
      IF (IAFS .EQ. 0) RETURN
!***  GENERATE THE ALTERNATE FLIGHT SEQUENCE            *******
      CALL GENAFS (N(MCY), N(MPMAX), N(MREC), N(MAFS), N(MFLG),    &
                   NTF, N(MMS), N(MRR), NNRAN, N(MPR), NNPI,       &
                   N(MIRR), NRMAX, PI, RAN, VP, N(MFF), ISS, NFT,  &
                   N(MHPEAK), N(MIDP), N(MJTN))
      IF (IFI .EQ. 0) RETURN
!***  SAVE THE ALTERNATE FLIGHT SEQUENCE ON MAGNETIC TAPE ******
      MMS = MFLG
      CALL WTAPE (N(MREC), N(MCY), N(MMS), NTF, NTF)
      RETURN
   60 FORMAT (5X, 'PEAK LEVELS FOR SPECTRUM SUMATION' / (10F12.0))
  110 FORMAT (5X, 'INPUT RANGE LEVELS FOR SPECTRUM SUMMATION' /   &
      (10F12.0))
  150 FORMAT (5X, 'INPUT VALLEY/PEAK RATIOS FOR SPECTRUM '        &
              'SUMMATION'/ 5X, 18F7.3)
  160 FORMAT (5X, 'INPUT VALLEY/PEAK RATIO VS RANGE CURVE FOR'    &
                  'RANGE TRUNCATION'/ 8X, 5(F7.3, F10.0, 4X) /    &
                   8X, 5(F7.3, F10.0, 4X))
  180 FORMAT (5X, 'INPUT FLIGHT NUMBERS FOR SPECTRUM SUMMATION '  &
                  'PRINT'/ (5X, 20I6))
  210 FORMAT (4X,'FLIGHT',6X,'RR',5X,'CASE',5X,'A6PA SEGMENTS'/)
  250 FORMAT (I8, 4X, I6, 2X, I6, 6X, 2I6)
  280 FORMAT (4X, 'FLIGHT TYPE', I5, ' HAS', I6, ' FLIGHTS AND',  &
              I5, ' A6PA SEGMENTS')
  290 FORMAT (8X, 'F1 SEGMENTS', 35I3)
  300 FORMAT (8X, 'F2 SEGMENTS', 35I3)
  350 FORMAT (4X, 'INPUT FLIGHT SEQUENCE' / (1X, 8(14I6, 5X)))
      END

!----------------------------------------------------------------------
      SUBROUTINE INMMN (NFT , NS, IF1, IF2, NST, SMAX, SMIN, NCY, &
                 NF1ST, NF2ST, MMN, M3, NS1, NS2, NTF, NF, IS2)
!***  THIS SUBROUTINE ETS UP THE CORE STORAGE               ****
!***  REQUIRED FOR THE CALLS TO SUBROUTINE AMMN             ****
!***  SUBROUTINES CALLED - AMMN, ERROR                      ****
      INTEGER :: NS(NFT), IF1(NST), IF2(NST), NCY(M3), NF(NFT),IS2(NST)
      REAL :: SMAX(M3), SMIN(M3)
      INTEGER :: NFT, NST, NF1ST, NF2ST, MMN, M3, NS1, NS2, NTF
      INTEGER :: I, JF1, JF2, JS1, JS2, NSS, NNM

      JF1 = 1
      JF2 = 1
      JS1 = 1
      JS2 = 1
      MMN = 1
!***  GET THE MAXIMUM STRESSES, MINIMUM STRESES, AND NUMBER *****
!***  OF CYCLES FOR EACH FLIGHT TYPE.  ALSO DO COMBINING OF *****
!***  SEGMENTS AND CYCLES.
      DO I = 1, NFT
         NSS = NS(I)
         CALL AMMN (NSS, IF1(JF1), IF2(JF2), IF1(JS1), IS2(JS2), &
                    NS1, NS2, SMAX(MMN), SMIN(MMN), NCY(MMN),    &
                    M3-MMN, NNM, NF(I), NCY(MMN), I)
         JF1 = JF1 + NSS
         JF2 = JF2 + NSS
         JS1 = JS1 + NS1
         JS2 = JS2 + NS2
         NS(I) = JS2 - 1 + (NST+1) * MMN
         MMN = MMN + NNM
         IF (MMN .GE. M3) CALL ERROR (3,MMN,M3,I)
      END DO

      MMN = MMN -1
      JS2 = JS2 -1
!***  AFTER PERFORMING THE COMBINING OF SEGMETS,          *******
!***  RECREATE THE F2 ARRAY.                              *******
      DO I = 1,JS2
         IF2(I) = IS2(I)
      END DO

      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE AMMN (NSS, IF1, IF2, IS1, IS2, NS1, NS2, SMAX, SMIN, &
                    NCY, MM, MMN, NF, BCY, IDFT)
!***  THIS SUBROUTINE PERFORMS THE COMBINING OF EGMENTS      ***
!***  BASED ON THE USER INPUT ARRAYS, F1 AND F2, PRODUCING   ***
!***  NEW MAXIMUM STRESS, MINIMUM STRESS AND NUMBER OF CYCLE ***
!***  ARRAYS.                                                ***
      COMMON SKIP(25), IUIL
      REAL :: SKIP
      INTEGER :: IUIL
      INTEGER :: IF1(NSS), IF2(NSS), IS1(*), IS2(*), NCY(MM)
      REAL :: SMAX(MM), SMIN(MM),  BCY(MM)
      REAL :: AMAX(25), AMIN(25), ACY(25)
      INTEGER :: NSS,  NS1, NS2, MM, MMN, NF, IDFT
      INTEGER :: JS1, I, J, K, K1, K2, KF1, KMMN, IA, IAA, IS
      INTEGER :: KK, KQ, KQ1, KKQ, KQQ, MAX, MIN, LINE, L
      INTEGER :: KS1, NDEL, NSS1
      REAL :: AA, AAA, BAA, AT

      AMAX=0     !
      AMIN=0     ! PSV
      ACY =0     !
      BCY =0     !

      JS1 = 0
      MMN = 0
!***  SUMMATION OF CYCLES                               ********
!***  F1 COMBINING OF SEGMENTS                          ********
      DO I = 1,NSS
!***     READ IN NUMBER OF GROUPS, PEAK VALUE, VALLEY      ********
!***     NUMBER OF CYCLES SEQUENCE FROM UTILITY TAPE       ********
         READ (IUIL) KMMN, (AMAX(K), AMIN(K), ACY(K), K = 1, KMMN)
         KF1 = IF1(I)
         IF (KF1 .NE. 0) THEN
            IF (KF1 .GT. JS1) THEN
               JS1 = JS1 + 1
               DO J = 1, KMMN
                  MMN = MMN + 1
                  SMAX(MMN) = AMAX(J)
                  SMIN(MMN) = AMIN(J)
                  BCY(MMN) = ACY(J)
               END DO
               IS1(JS1) = MMN
               CYCLE
            END IF
            K2 = IS1(KF1)
            IF (KF1 .EQ. 1) K1 = 1
            IF (KF1 .NE. 1) K1 = IS1(KF1-1) + 1
            K = 1
            DO J = K1,K2
               BCY(J) = BCY(J) + ACY(K)
               K = K + 1
            END DO
         END IF
         IF2(I) = 0
      END DO
      NS1 = JS1
      K1 = 1
      L = 1
!***  ROUND OFF CYCLES TO NEAREST INTEGER AND ELIMINATE *******
!***  CYCLES THAT ARE LESS THAN 0.5                     *******
      DO I = 1,NS1
         K2 = IS1(I)
         DO J = K1,K2
            NCY(L) = INT(BCY(J) + 0.5000001)
            SMAX(L) = SMAX(J)
            SMIN(L) = SMIN(J)
            IF (NCY(L) .GT. 0) L = L + 1
         END DO
         K1 = K2 + 1
         IF (L .LE. 1) CALL ERROR (7, I, NSS, K2)
         IS1(I) = L - 1
      END DO
      MMN = L - 1
!***  MOVE DOW TO THE END OF THE ARRAYS SO THAT F2      *******
!***  COMBINING OF SEGMENTS CAN BE DONE                 *******
      K = MMN
      J = MM
      DO I = 1, MMN
         SMAX(J) = SMAX(K)
         SMIN(J) = SMIN(K)
         NCY(J) = NCY(K)
         J = J - 1
         K = K - 1
      END DO
      NDEL = J - K
      KS1 = 1
!***  F2 COMBINING OF SEGMENTS
      NSS1 = NSS + 1
      DO I = 1, NSS
         IS2(I) = 0
         KKQ = K
         KQ = KKQ + 1
         KF1 = 0
         DO J = 1, NSS1
            IF (J .NE. NSS1) THEN
               IF (IF2(J) .NE. 0) KF1 = KF1 + 1
               IF (IF2(J) .NE. I) CYCLE       ! PSV: 1 -> I
            ELSE
               IF (KKQ .EQ. K) GO TO 220
               KQQ = KQ + IS2(I) -1
               KK = 0
               DO L = KQ, KQQ
                  KK = KK + NCY(L)
               END DO
               AA = (KK-1) / NF
               IA = INT(AA)
               NCY(K+1) = (IA+1) * NF - KK
               IF (NCY(K+1) .NE. 0) THEN
!****             FORMULATE FICTITIOUS LOAD LEVEL
                  K = K + 1
                  IS2(I) = IS2(I) + 1
                  MAX = 0
                  MIN = 0
                  KQ1 = KQ + 1
                  IF (KQ1 .GT. KQQ) THEN
                     SMIN(KQQ+1) = SMIN(KQ)
                     SMAX(KQQ+1) = SMIN(KQ)
                  ELSE
                     IF (ABS(SMAX(KQ1)-SMAX(KQ)) .GT. 1E-6) MAX = 1
                     IF (ABS(SMIN(KQ1)-SMIN(KQ)) .GT. 1E-6) MIN = 1
                     IF (MIN .EQ. 0) THEN
                        SMIN(KQQ+1) = SMIN(KQ)
                        SMAX(KQQ+1) = SMIN(KQ)
                     ELSE
                        IF (MAX .EQ. 0) THEN
                           SMAX(KQQ+1) = SMAX(KQ)
                           SMIN(KQQ+1) = SMIN(KQ)
                        ELSE
                           SMAX(KQQ+1) = (SMAX(KQ) + SMIN(KQ)) / 2.0
                           SMIN(KQQ+1) = SMAX(KQQ+1)
                        END IF
                     END IF
                  END IF
               END IF

!***           WRITE (2,10) I,KQ,KQQ,K1,K,MAX,MIN,IA,IS2(I-1),(NCY(L), L-KQ,K)
!***           WRITE (2,20) (SMAX(L), SMIN(L), L = KQ,KK)
               CYCLE
            END IF
            K2 = IS1(KF1) + NDEL
            IF (KF1 .EQ. 1) K1 = 1 + NDEL
            IF (KF1 .NE. 1) K1 = IS1(KF1-1) + 1 + NDEL
            DO L = K1,K2
               K = K + 1
               SMAX(K) = SMAX(L)
               SMIN(K) = SMIN(L)
               BCY(K) = BCY(L)
               NCY(K) = NCY(L)
            END DO
            IS2(I) = IS2(I) + K2 - K1 + 1
         END DO
      END DO
      I = NSS1
  220 NS2 = I -1
      MMN = K
      DO I = 2,NS2
         IS2(I) = IS2(I) + IS2(I-1)
      END DO
      LINE = 60
      IS = 1
      IAA = 0
      IA = 0
      BAA = 0.0
      AAA = 0.0
!**   PRINT OUT AFTER SEGMENT COMBINING               *********
      DO I = 1,MMN
         AT = NCY(I)
         AA = AT/NF
         AAA = AAA + AA
         IA = IA + NCY(I)
         IF (LINE .GE. 49) THEN
            CALL NEWPG
            WRITE (6,240) IDFT
            LINE = 0
         END IF
         WRITE (6,260) IS, SMIN(I), SMAX(I), NCY(I), AA
         LINE = LINE + 1
         IF (IS2(IS) .GT. I) CYCLE
         IAA = IA - IAA
         BAA = AAA - BAA
         WRITE (6,280) IAA,BAA
         LINE = LINE + 2
         IS = IS + 1
         IAA = IA
         BAA = AAA
      END DO
      WRITE (6,280) IA, AAA

      RETURN
  240 FORMAT (4X, 'FLIGHT TYPE', I4, 7X, 'F2', 9X, 'MIN.', 7X, &
                 'MAX.', / , 25X, 'SEGMENT', 5X 'STRESS', 5X,  &
                 'STRESS', 6X, 'CYCLES', 6X, 'CYCLES/FLIGHT',/)
  260 FORMAT (24X, I4, F14.0, F11.0, I12, 5X, F12.4)
  280 FORMAT (53X, I12, 5X, F12.4, /)
      END

!----------------------------------------------------------------------
      SUBROUTINE GENFL(NF,NFT,NTF,NS,NST,SMAX,SMIN,NCY,NMM,IS2,    &
                       SMM, MAXSS, PI, RAN, VP, XY, RR, NNRAN, PR, &
                       NNPI, IRR, NRMAX, ISS, PMAX, MCY, NFF, IPF, &
                       HPEAK, IDPEAK, JTN)
!***  THIS SUBROUTINE GENERATES THE SEQUENCE OF FLIGHTS.  ******
!***  DEPENDING UPON INPUT, THE SEQUENCE CAN BE RANDOMLY  ******
!***  GENERATED OR USER SPECIFIED.                        ******
!***  SUBROUTINES CALLED - DISTRD, GENCY, PRNTSS          ******
      INTEGER :: NF(NFT), NFT, NTF, NS(NFT), NST, NCY(NMM,*), NMM
      INTEGER :: IS2(NST), MAXSS, RR(NNRAN,*), NNRAN, PR(NNPI,*)
      INTEGER :: NNPI, IRR(NRMAX,1), NRMAX, ISS(*), MCY(NTF)
      INTEGER :: NFF(2,NFT), IPF(2,1), IDPEAK(*), JTN(NTF)
      REAL :: SMAX(NMM), SMIN(NMM), SMM(2,MAXSS), PI(*), RAN(*)
      REAL :: VP(*), XY(2,*), PMAX(NTF), HPEAK(*)

      COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI,    &
             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP,       &
             ELIMV, IPFS, IPSS, IPTF, IAFS, NEXT, NOW, IFRS,       &
             MAXHP, IKF, IKC
      INTEGER :: NPG, TITLE, NSIZE, LEFT, IERR, IRS, IUIL, NPI
      INTEGER :: NRAN, IVP, IFI, NXY, IPFS, IPSS, IPTF, IAFS, NEXT
      INTEGER :: NOW, IFRS, MAXHP, IKF, IKC
      REAL :: CLIP, CLIV, FACTOR, ELIMP, ELIMV

      INTEGER :: IJJ(6), KF, KC, NPRNT, NPSS, NPTF, IS, KLINE, NFT2
      INTEGER :: I, IIVP, NST1, I1, I2, JPF, II, III, IJF, JF
      INTEGER :: JS2, K, KCY, MMN, NSS
      REAL :: AMAX, HP, TMAX

      KF = 11111
      IF (IKF .NE. 0) KF = IKF
      KC = 12345
      IF (IKC .NE. 0) KC = IKC
      NPRNT = IPFS
      IF (IPFS .EQ. 0) NPRNT = NTF
      NPSS = ISS(1)
      IF (IPSS .EQ. 0) NPSS = NTF
      NPTF = IPTF
      IF (IPTF .EQ. 0) NPTF = NTF
      IS = 1
      KLINE = 60
      NFT2 = 2 * NFT
!***  ZERO OUT THE TABLES USED IN THE SPECTRUM SUMMATION
!     DO 40 I = 1,NFT2
!  40 NFF(I,1) = 0
      NFF = 0
      IJJ = 0
      IF (IRS .NE. 1) NCY(:,2) = NCY(:,1)
      IIVP = IVP + 2
      RR(:,1:IIVP)=0
      PR(:,1:IIVP)=0

      IF (MAXHP .NE. 0) THEN
!***     ZERO OUT HIGHEST PEAK ARRAYS
         HPEAK(1:MAXHP) = 0.0
         IDPEAK(1:MAXHP) = 0
      END IF

      SMM(1,1) = -1.E20
      SMM(2,1) = 1.E20
      NST1 = NST + 1
      I1 = 2
      I2 = 1
      JPF = 1
      DO II = 1, NTF
         IF (IFRS .NE. 0) THEN
!***        THE SEQUENCE OF FLIGHT NUMBERS IS SPECIFIED BY INPUT
            JF = IPF (1,JPF)
            IPF(2,JPF) = IPF(2,JPF) -1
            IF (IPF(2,JPF) .EQ. 0) JPF = JPF + 1
         ELSE
!***        THE SEQUENCE NUMBER OF FLIGHT NUMBERS IS RADOMLY GENERATED
            CALL DISTRD (NF, NFT, KF, JF)
         END IF
         NFF(1,JF) = NFF(1,JF) + 1
         MMN = NS(JF) / NST1
         IF (JF .NE. 1) JS2 = NS(JF-1) - NST1*(NS(JF-1)/NST1) + 1
         IF (JF .EQ. 1) JS2 = 1
         NSS = NS(JF) - MMN * NST1 - JS2 + 1

!***     WRITE (2,10) I1, NMM, IRS
!***     WRITE (2,10) NSS,JF,MMN,NST1,JS2,JPF,NF,NS,NFT,NTF,MAXSS
         IF (II .EQ. NTF) I2 = 0

!***     GENERATE CYCLE SEQUENCE
         CALL GENCY(IS2(JS2), SMAX(MMN), SMIN(MMN), NCY(MMN,1),    &
                    NF(JF), SMM(1,1), MAXSS, NSS, NCY(MMN,2),      &
                    PI, RAN, VP, XY, RR, NNRAN, PR, NNPI, KC, I1,  &
                    I2, IJJ, II, JF, NPRNT, KLINE, AMAX, KCY, NPTF)

         IF (IAFS .NE. 0) THEN
!******     SAVE THE HIGHEST PEAK AND NUMBER OF CYCLES FOR EACH FLIGHT
!******     TO BE USED FOR THE ALTERNATE FLIGHT SEQUENCE
            PMAX(II) = AMAX    !  PSV
            MCY(II) = 2 * KCY  !  I1 -> II
            JTN(II) = JF       !
         ELSE
            IF (MAXHP.NE.0) THEN
!******        DETERMINE AND SAVE THE HIGHEST PEAK AND CORRESPONDING
!******        FLIGHT NUMBER FOR MAXHP (SPECIFIED BY INPUT) NUMBER
!******        OF FLIGHTS
               HP = AMAX
               III = II   !PSV I1 ->II
               DO I = 1,MAXHP
                  IF (HP .LE. HPEAK(I)) CYCLE
                  DO K = I,MAXHP   !PSV  1 -> I
                     TMAX = HPEAK(K)
                     HPEAK(K) = HP
                     HP=TMAX
                     IJF = IDPEAK(K)
                     IDPEAK(K) = III
                     III = IJF
                  END DO
               END DO
            END IF
         END IF
         I1 = 1
         NFF(2,JF) = NFF(2,JF) + KCY
         NF(JF) = NF(JF) -1
         IF (.NOT.((II .NE. NPSS) .OR. (IAFS .NE. 0))) THEN
!***        WRITE (2,10) IJJ
!***        WRITE (2,10) MCY
!***        WRITE (2,20) PMAX
!***        IF NO ALTERNATE FLIGHT SEQUENCE IS DESIRED THEN
!***        PRINT THE SPECTRUM SUMMATION TABLES
            CALL PRNTSS (RR, NNRAN, PR, NNPI, IRR, NRMAX, IIVP, RAN, &
                 VP, PI, IS, NFF, ISS, II, NTF, NFT, HPEAK, IDPEAK)
         END IF
         IF (II .EQ. NPTF) EXIT
      END DO

      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE GENAFS (MCY, PMAX, LREC, AFS, IFLAG, NTF, SMM,    &
                         RR, NNRAN, PR, NNPI, IRR, NRMAX, PI, RAN, &
                         VP, NFF, ISS, NFT, HPEAK, IDPEAK, JTN)
!***  THIS SUBROUTINE RECORDS THE RANDOM FLIGHT SEQUENCE ******
!***  INTO LO-HI, HI-LO, LO-HI-LO FLIGHT SEQUENCE BASED  ******
!***  UPON THE MAXIMUM STRESS PER FLIGHT.  IT PERFORMS A ******
!***  TYPE 1 EDIT BEFORE DOING A SPECTRUM SUMMATION      ******
!***  SUBROUTINES CALLED - NEWPG, PRNTSS, REED, REDIT1, SPSUM  ******
      INTEGER :: MCY(NTF), LREC(NTF), IFLAG(NTF), NTF, NNRAN, NNPI, NRMAX
      INTEGER :: IRR(NRMAX,1), NFF(*), ISS(*), NFT, IDPEAK(*), JTN(NTF)
      INTEGER :: RR(NNRAN,*), PR(NNPI,*)
      REAL :: PMAX(NTF), AFS(NTF), SMM(2,1)
      REAL :: PI(*), RAN(*), VP(*), HPEAK(*)
      COMMON ISKP(28),IVP, JSKP(7),IPFS, IPSS, IPTF,IAFS,FSKP(3),MAXHP
      INTEGER :: ISKP, IVP, JSKP, IPFS, IPSS, IPTF, IAFS, MAXHP
      REAL :: FSKP
      INTEGER :: N1(3), IADD(3), TITLE(5), IJJ(6)
      REAL :: FORM(3,3), EPS, PHI, HP, TMAX
      INTEGER :: NPRNT, IS, INO, II, III, NPSS, IIVP, I, J, IFL, JF
      INTEGER :: LINE, KLINE, N2, N3, N4, J1, J2, JJ, K, NCY, NPTF
      DATA IFL/0/

      NPRNT = IPFS
      IF (IPFS .EQ. 0) NPRNT = NTF
      EPS = 1.0
      N1(1) = NTF + 1
      N1(2) = 0
      N1(3) = (NTF/2) + 1
      IADD(1) = -1
      IADD(2) = 1
      IADD(3) = -1
      INO = IADD(IAFS)
      IS = N1(IAFS)
      NPRNT = IPFS
      IF (IPFS .EQ. 0) NPRNT = NTF
      NPSS = ISS(1)
      IF (NPSS .EQ. 0) NPSS = NTF
      NPTF = IPTF
      IF (IPTF .EQ. 0) NPTF = NTF

      IIVP = IVP + 2
      RR(:,1:IIVP)=0
      PR(:,1:IIVP)=0
      IFLAG = 0
!***  REORDER THE FLIGHT SEQUENCE INTO LO-HI, HI-LO, OR LO-HI-LO
!***  SEQUENCE DEPENDING UPON TEH IAFS FLAG SPECIFIED BY INPUT
      DO II = 1,NTF
         PHI = -1.E20
         DO I = 1,NTF
            IF (PMAX(I) .LT. PHI) CYCLE
            IF (IFLAG(I) .GT.0) CYCLE
            PHI = PMAX(I)
            IFL = I
         END DO
         IF (IAFS .EQ. 3) THEN
            IADD(3) = -1 * IADD(3)
            INO = IADD(3) * (II -1)
         END IF
         IS = IS + INO
         AFS(IS) = PHI
         LREC(IS) = IFL
         IFLAG(IFL) = 1
!*****   FIND THE HIGHEST STRESSES AND ASSOCIATED FLIGHT NUMBERS
         IF (MAXHP .EQ. 0) CYCLE
         HP = PHI
         III = IS
         DO JJ = 1, MAXHP
            IF (HP .LE. HPEAK(JJ)) CYCLE
            DO K = JJ,MAXHP
               TMAX = HPEAK(K)
               HPEAK(K) = HP
               HP = TMAX
               JF = IDPEAK(K)
               IDPEAK(K) = III
               III = JF
            END DO
         END DO
      END DO
!*****PRINT THE NEW ORDER OF FLIGHTS AND THE LARGEST STRESS
!*****PER FLIGHT
      LINE = 50
      DO I = 1,NTF,5
         N4 = 1
         N2 = I + 4
         IF (N2 .GT. NTF) N2 = NTF
         N3 = N2 - N4 + 1
         LINE = LINE + 1
         IF ( LINE .GE. 45) THEN
            CALL NEWPG
            WRITE (6,100) (FORM(J,IAFS), J = 1,3)
            LINE = 0
            WRITE (6,110) ((TITLE(K), K=1, 5), J = 1, N3)
         END IF
         WRITE (6,130) (LREC(J), AFS(J), J = N4, N2)
      END DO
      IS = 1
      KLINE = 60
      DO I = 1,NTF
         NCY = MCY(LREC(I))
         CALL REED (SMM, LREC(I), NCY)
!***     REPEAT AN EDIT 1 BETWEEN FLIGHTS BECAUSE OF REORDERING
         NCY = NCY / 2
         CALL REDIT1 (SMM, IJJ, NCY, EPS)
!***     SPECTRUM SUMMATION
         CALL SPSUM (SMM, 1, NCY, PMAX(I), VP, RAN, PI, RR,   &
                     NNRAN, PR, NNPI)                            ! PSV I -> 1
         IF (I .LE. NPRNT) THEN
            IF (KLINE .GE. 53) THEN
               CALL NEWPG
               KLINE = 5
            END IF
            WRITE (6,170) I, LREC(I), JTN(LREC(I)), NCY
            KLINE = KLINE + 2
!***        PRINT THE NEW FLIGHT NUMBER, THE OLD FLIGHT NUMBER
!***        FLIGHT TYPE NUMBER, NUMBER OF CYCLES AND THE
!***        SEQUENCE OF MAXIMUM AND MINIMUM STRESES.
            DO JJ = 1, NCY, 5
               J1 = JJ
               J2 = J1 + 4
               IF (J2 .GT. NCY) J2 = NCY
               IF (KLINE .GE. 55) THEN
                  CALL NEWPG
                  KLINE = 5
               END IF
               WRITE (6,190) ((SMM(K,J), K = 1,2), J = J1,J2)
               KLINE = KLINE + 1
            END DO
         END IF
         IF (I .EQ. NPSS) THEN
!***        PRINT THE SPECTRUM SUMMATION
!***        PRINT THE SPECTRUM SUMMATION
            CALL PRNTSS (RR, NNRAN, PR, NNPI, IRR, NRMAX,    &
                         IIVP, RAN, VP, PI, IS, NFF, ISS, I, &
                         NTF, NFT,HPEAK, IDPEAK)
         END IF
         IF (I .EQ. NPTF) EXIT
      END DO

      RETURN
  100 FORMAT (15X, 3A4, 'ORDER OF FLIGHTS(NEAREST PEAK /FLIGHT)',//)
  110 FORMAT (5(5A4,4X))
  130 FORMAT (5(I6, F13.2, 5X))
  170 FORMAT ('NEW FLIGHT NUMBER',I5,'OLD FLIGHT NUMBER', I5,    &
              '  IS TYPE NUMBER', I3,'   NUMBER OF CYCLES', I7,  &
              '  SEQUENCE FLOWS ')
  190       FORMAT (10F12.2)
      END

!----------------------------------------------------------------------
      SUBROUTINE GENCY (IS2, SMAX, SMIN, NCY, NF, SMM, MAXSS,   &
                        NSS, KCY, PI, RAN, VP, XY, RR, NNRAN,   &
                        PR, NNPI, KC, I1, I2, IJJ, II, JF,      &
                        NPRNT, KLINE, PMAX, MCY, INTF)
!***  THS SUBROUTINE GENERATES THE CYCLE SEQUENCE FOR THE GIVEN ***
!***  FLIGHT.  IT THEN PERFORMS ALL EDITING OF THE CYCLES. IF   ***
!***  A SAVE TAPE IS SPECIFIED AND NO ALTERNATE FLIGHT SEQUENCE ***
!***  IS DESIRED, THEN THE CYCLE SEQUENCE WRITTEN DIRECTLY ONTO ***
!***  AN OUTPUT TAPE.  OTHERWISE THE CYCLE SEQUENCE IS WRITTEN  ***
!***  ONTO TEMPORARY MASS STORAGE.                              ***
!***  SUBROUTINES CALLED - DISTRD, ERROR, NEWPG, REDIT1,        ***
!***                       RITE, SPSUM, WTAPE                   ***

      INTEGER :: IS2(NSS), NCY(*), NF, MAXSS, NSS, KCY(*), NNRAN
      INTEGER :: NNPI, KC, I1, I2, IJJ(*), II, JF, NPRNT, KLINE
      INTEGER :: RR(NNRAN,*), PR(NNPI,*), MCY, INTF
      REAL :: SMAX(*), SMIN(*), SMM(2,MAXSS), PI(*), RAN(*), VP(*)
      REAL :: XY(2,*), PMAX

      COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL,    &
             NPI, NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR,    &
             ELIMP, ELIMV, IPFS, IPSS, IPTF, IAFS
      INTEGER :: NPG, TITLE, NSIZE, LEFT, IERR, IRS, IUIL, NPI
      INTEGER :: NRAN, IVP, IFI, NXY, IPFS, IPSS, IPTF, IAFS
      REAL :: CLIP, CLIV, FACTOR, ELIMP, ELIMV

      INTEGER :: IMM, KMM, LMM, K0, K1, K2, K3, NERR5, NCS
      INTEGER :: I, J, K, I3, J1, J2, IC, JC, JJ, JMM, LCY
      REAL :: TEMP, R, RANGE, RQ, EPS

      IMM = 1
      KMM = 1
      K1 = 1
      K0 = 0
      IF (II .EQ. 1) NERR5 = 0
      EPS = 1.
      PMAX = -1.E20
      DO I = 1, NSS
         K2 = IS2(I)
         NCS = 0
         K3 = K2 - K0
         LMM = IMM
         DO J = K1,K2
            NCS = NCS + NCY(J)
         END DO
         LCY = NCS / NF
         DO J = 1,LCY
!*****      IF PAIRED VALLEY PEAK COUPLING (IRS=1) IS SPECIFIED,
!*****      FIND THE RANDOM CYCLE. IF INDIVIDUAL VALLEY PEAK
!*****      COUPLING (IRS = 2) IS SPECIFIED, FIDN THE RANDOM PEAK.
            CALL DISTRD (NCY(K1), K3, KC, IC)
            IMM = IMM + 1
            IF (IMM .GE. MAXSS) THEN
               CALL ERROR (4, IMM, NSIZE, JF)
               STOP 7002
            END IF
            SMM (1,IMM) = SMIN(IC+K0)
            NCY(IC+K0) = NCY(IC+K0) -1
            IF (IRS .NE. 2) THEN
               SMM(2,IMM) = SMAX(IC+K0)
            ELSE
!*****         IF INDIVIDUAL VALLEY PEAK COUPLING (IRS=2) IS
!*****         SPECIFIED, FIND THE RANDOM VALLEY.
               CALL DISTRD (KCY(K1), K3, KC, JC)
               SMM(2,IMM) = SMAX(JC+K0)
               KCY(JC+K0) = KCY(JC+K0) -1
            END IF
!*****      EDIT 1
!*****      CHECK TO SEE IF MINIMU IS REALLY A VALLEY AND NOT AN
!*****      INTERMEDIATE POINT ON THE WAY TO A PEAK
            IF (ABS(SMM(1,IMM-1)-SMM(2,IMM-1)) .LT. 1E-6) THEN
               IF (SMM(1,IMM-1).GT.SMM(1,IMM)) SMM(1,IMM-1)=SMM(1,IMM)
               SMM(2,IMM-1) = SMM(2,IMM)
               IMM = IMM - 1
               IJJ(2) = IJJ(2) + 1
               CYCLE
            END IF
            IF (SMM(1,IMM) .GE. SMM(2,IMM-1)) THEN
               IJJ(1) = IJJ(1) + 1
               SMM (2,IMM-1) = SMM(2,IMM)
               IMM = IMM - 1
            END IF
            !IF ((ABS(SMM(1,IMM) - SMM(2,IMM)).LT.EPS ) .OR. &
            !    (ABS(SMM(1,IMM) - SMM(2,IMM)).LT.1E-10)) THEN
            IF ((ABS(SMM(1,IMM) - SMM(2,IMM)).LT.EPS )) THEN
!*****         DROP OUT FICTITIOUS LOAD LEVELS
               SMM(1,IMM) = SMM(2,IMM)
               IF ((SMM(2,IMM) .LT. SMM(2,IMM-1))) CYCLE
               SMM(2,IMM-1) = SMM(2,IMM)
               IMM = IMM - 1
               IJJ(3) = IJJ(3) + 1
            ELSE
               IF (SMM(1,IMM) .LT. SMM(2,IMM)) CYCLE
!*****         VALLEY GREATER THAN PEAK IS IN THIS SEGMENT
!*****         INTERCHANGE VALLEY AND PEAK
               TEMP = SMM(1,IMM)
               SMM(1,IMM) = SMM(2,IMM)
               SMM(2,IMM) = TEMP
            END IF
         END DO
!***     WRITE (2,10) (IJJ(J), J = 1,3)
         K0 = K2
         K1 = K0 + 1
      END DO
!***  EDIT 2
      KMM = IMM
      IMM = I1 - 1
      DO JMM = I1,KMM
         IMM = IMM + 1
         SMM(1,IMM) = SMM(1,JMM)
         SMM(2,IMM) = SMM(2,JMM)
!***     ELIMINATE RANGES BELOW AN INPUT RANGE VERSUS R CURVE
         RANGE = SMM(2,IMM) - SMM(1,IMM)
         R = SMM(1,IMM) / SMM(2,IMM)
!***     WRITE (2,10) JMM, IMM, I1, KMM
!***     WRITE (2,20) SMM(1,IMM), SMM(2,IMM) RANGE, R
         IF (NXY .GT. 1) THEN
            IF (XY(1,1) .GT. R) THEN
               IF (NERR5 .LT. 6) CALL ERROR (5, II, IMM, 1)
               NERR5 = NERR5 + 1
               K = 2
            ELSE
               IF (ALL(XY(1,2:NXY) .LT. R)) THEN
                  IF (NERR5 .LT. 6) CALL ERROR (5, II, IMM, 2)
                  K = NXY
               ELSE
                  K=MINLOC(XY(1,1:NXY),1,MASK=(XY(1,1:NXY).GE.R))
               END IF
            END IF

            RQ = XY(2,K) + (R-XY(1,K-1)) * (XY(2,K) - &
                 XY(2,K-1)) / (XY(1,K) - XY(1,K-1))

            IF (RANGE .LE. RQ) THEN
               IF (.NOT.((IMM.EQ.1) .OR. (SMM(2,IMM).LE.SMM(2,IMM-1)))) THEN
                  SMM(2,IMM-1) = SMM(2,IMM)
               ELSE
                  IF(((JMM+1).LE.KMM).AND.(SMM(1,IMM).LT.SMM(1,JMM+1))) THEN
                     SMM(1,JMM+1) = SMM(1,IMM)
                  END IF
               END IF
               IJJ(4) = IJJ(4) + 1
               IMM = IMM -1
               IJJ(6) = IJJ(6) + 1
               CYCLE
            END IF
         END IF

!*****   ELIMINATRE PEAKS BY INPUT VALUES
!*****   CLIP PEAKS AND/OR VALLEY BY INPUT VALUES
         IF (SMM(2,IMM) .GT. ELIMP) THEN
            IMM = IMM -1
            IJJ(6) = IJJ(6) + 1
            CYCLE
         END IF

         IF (SMM(2,IMM) .GE. CLIP) THEN
            IF (SMM(1,IMM) .GE. CLIP) THEN
!*****         IF BOTH VALLEY AND PEAK ABOE PAK CLIPPING ELIMINATE CYCLE
!*****         OR IF BOTH VALLEY AND PEAK BELOW VALLEY CLIPPING ELIMINATE CYCLE
               IMM = IMM -1
               IJJ(5) = IJJ(5) + 1
               CYCLE
            END IF
            SMM(2,IMM) = CLIP
         END IF

         IF (SMM(1,IMM) .GT. CLIV) CYCLE
         IF (SMM(2,IMM) .LT. CLIV) THEN
            IMM = IMM -1
            IJJ(5) = IJJ(5) + 1
         ELSE
            SMM(1,IMM) = CLIV
         END IF
      END DO

      MCY = IMM -1
      IF (MCY .EQ. 0) THEN
         WRITE (6,30) I1, JF, MCY
         CALL ERROR (6, II, JF, MCY)
         RETURN
      END IF

!***  REPEAT A TYPE 1 EDIT
      CALL REDIT1 (SMM, IJJ, IMM, EPS)
      MCY = IMM - 1
      IF (MCY .EQ. 0) THEN
         WRITE (6,30) I1, JF, MCY
         CALL ERROR (6, II, JF, MCY)
         RETURN
      END IF

!***  MULTIPLICATION FACTOR
      DO I = 1,IMM
      DO J = 1,2
         SMM(J,I) = SMM(J,I) * FACTOR
      END DO
      END DO
      I3 = IMM - I2

!***  SPECTRUM SUMMATION
      CALL SPSUM (SMM, I1, I3, PMAX, VP, RAN, PI, RR,  &
                  NNRAN, PR, NNPI)

      IF (IAFS .NE. 0) THEN
!***     WRITE CYCLE SEQUENCE ONTO TEMPORARY MASS STORAGE
         CALL RITE (SMM(1,2), II, (2*MCY))
      ELSE
!***     IF NO ALTERNATE FLIGHT SEQUENCE I DESIRED, BUT A SAVE
!***     TAPE IS SPECIFIED, THEN WRITE THE CYCLE SEQUENCE
!***     DIRECTLY ONT THE OUTPUT TAPE.
         IF (IFI .NE. 0) CALL WTAPE (II, (IMM-I2), SMM, INTF, I1)
         IF (II .LE. NPRNT) THEN
            IF (KLINE .GE. 53) THEN
               CALL NEWPG
               KLINE = 5
            END IF
            WRITE (6,30) II, JF, MCY
            KLINE = KLINE + 2
            DO JJ = 2,IMM,5
               J1 = JJ
               J2 = J1+4
               IF (J2 .GT. IMM) J2 = IMM
               IF (KLINE .GE. 55) THEN
                  CALL NEWPG
                  KLINE = 5
               END IF
               WRITE (6,330) ((SMM(I,J), I=1,2), J = J1, J2)
               KLINE = KLINE + 1
            END DO
         END IF
      END IF
!***  ELIMINATE THE MULTIPLICATION FACTOR WHEN CARRYING THE *****
!***  LAST CYCLE OVER INTO THE NEXT FLIGHT                  *****
      SMM(1,1) = SMM(1,IMM) / FACTOR
      SMM(2,1) = SMM(2,IMM) / FACTOR
      RETURN
   30 FORMAT ('  FLIGHT NUMBER', I4, '  IS TUPE NUMBER', I3,     &
              '    NUMBER OF CYCLES', I7,'  SEQUENCE FOLLOWS ')
  330 FORMAT (10F12.2)
      END

!----------------------------------------------------------------------
      SUBROUTINE DISTRD (N, M, K, L)
!***  THIS SUBROUTINE USES A REANDOM NUMBER GENERATOR       *******
!***  TO SELECT A FLIGHT NUMBER OR A CYCLE DEPENDING        *******
!***  UPON THE CONTENTS OF TEH INPUT ARRAY.  THE SEQUENCE   *******
!***  OF FLIGHTS OR CYCLES IS ESTABLISHED BY SELECTING      *******
!***  WITHOUT REPLACING A CHOSEN NUMBER.                    *******
!***  SUBROUTINES CALLED - RANIC                            *******
      INTEGER :: N(M), M, K, L, NS, I, LR, MS
      REAL :: R

      IF (M .EQ. 0) RETURN
      NS = 0
      DO I = 1, M
         NS = NS + N(I)
      END DO
      CALL RANIC (K, R)
      LR = INT(NS*R) + 1
      MS = 0
      DO I = 1, M
         MS = MS + N(I)
         L = I
         IF (MS .GE. LR) EXIT
      END DO
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE RANIC (K, R)
!***  THIS SUBROUTINE GENERATES A PSEUDO RANDOM NUMBER, WHICH   ***
!***  LIES BETWEEN 0 AND 1 INCLUSIVE, SUCCESSIVE ENTRIES WILL   ***
!***  YIELD A SERIES OF NUMBER WHICH CONFORM TO A UNIFORM       ***
!***  DISRIBUTION. THE SERIES REPEATS AFTER APPROXIMATELY       ***
!***  10**6 NUMBERS
!
!     K - I/O = GENRATING INTEGER ARGUMENT. K MUST BE
!               INITIALIZED TO ANY NON-ZERO VALUE.  THEREAFTER
!               K IS MODIFIED BY THE SUBROUTINE AND SHOULD NOT
!               BE CHANGED BY THE USER.
!     R = O   = THE GENERATED RANDOM NUMBER

      INTEGER :: K, IMAX
      REAL :: R
      SAVE IMAX
      DATA IMAX /2147483647/

      K = K * 2051
      IF (K .LT. 0) K = K + IMAX + 1
      K = MOD(K,4194304)
      R = FLOAT(K) / 4194304.
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE REED (A, MAT, NSIZE)
!***  THIS SUBROUTINE READS AND ENTIRE SEQUENCE OF
!***  MAXIMUM AND MINIMUM STRESSES FOR ONE FLIGHT
!***  FROM TEMPORARY MASS STORAGE.
!***  SUBROUTINES CALLED - READMS
      INTEGER :: NSIZE, MAT
      REAL :: A(NSIZE)
      CALL READMS (4, A, NSIZE, MAT)
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE NEWPG
!***  THE SUBROUTINE PRINTS OUT A HEADING AT THE TOP OF *******
!***  EACH NEW PAGE. THE HEADING INCLUDES CONSECUTIVE   *******
!***  PAGE NUMBERING AND THE USER INPUT TITLE.          *******
!***  SUBROUTINES CALLED - NONE                         *******
      COMMON NPG, TITLE(20)
      INTEGER :: NPG, TITLE
      WRITE (6,10) NPG, TITLE
   10 FORMAT ('SPECTRUM LOADING SEQUENCE GENERATION PROGRAM',  &
             15X, 'PAGE', I5//5X, 'JOB TITLE  ', 20A4//)
      NPG = NPG + 1
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE REDIT1 (SMM, IJJ, IMM, EPS)
!***  THIS SUBROUTINE PERFORMS A REPEAT OF A TYPE 1 EDIT
!***  SUBROUTINES CALLED - NONE
      REAL :: SMM(2,*), EPS, TEMP
      INTEGER :: IJJ(*), IMM, KMM, JMM

      KMM = IMM
      IMM = 1
      DO JMM = 2,KMM
         IMM = IMM + 1
         SMM(1,IMM) = SMM(1,JMM)
         SMM(2,IMM) = SMM(2,JMM)

!***     CHECK TO SEE OF MINIMM US REALLY A VALLEY AND NOT AN
!***     INTERMEDIATE POINT ON THE WAY TO A PEAK
         IF (ABS(SMM(1,IMM-1)-SMM(2,IMM-1)).LE.1E-6) THEN
            IF (SMM(1,IMM-1) .GT. SMM(1,IMM)) SMM(1,IMM-1) = SMM(1,IMM)
            SMM(2,IMM-1) = SMM(2,IMM)
            IMM = IMM - 1
            IJJ(2) = IJJ(2) + 1
            CYCLE
         END IF

         IF (SMM(1,IMM) .GE. SMM(2,IMM-1)) THEN
            IJJ(1) = IJJ(1) + 1
            SMM(2,IMM-1) = SMM(2,IMM)
            IMM = IMM - 1
         END IF

         IF (ABS(SMM(1,IMM)-SMM(2,IMM)).LT.EPS) THEN
!***        DROP OUT FICTITIOUS LOAD LEVELS
            SMM(1,IMM) = SMM(2,IMM)
            IF (SMM(2,IMM) .LT. SMM(2,IMM-1)) CYCLE
            SMM(2,IMM-1) = SMM(2,IMM)
            IMM = IMM - 1
            IJJ(3) = IJJ(3) + 1
         ELSE
            IF (SMM(1,IMM)  .GT. SMM(2,IMM)) THEN
!***           VALLEY GREATER THAN PEAK IN THIS SEGMENT
!***           INTERCHANGE VALLEY AND PEAK
               TEMP = SMM(1,IMM)
               SMM(1,IMM) = SMM(2,IMM)
               SMM(2,IMM) = TEMP
            END IF
         END IF
      END DO
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE SPSUM(SMM, I1,I3, PMAX,VP,RAN, PI, RR,NNRAN, PR,NNPI)
!***  THIS SUBROUTINE GENERATES THE TABLES REQUIRED FOR
!***  THE SPECTRUM SUMMATION PRINT OUT OF RANGE VS.
!***  VALLEY/PEAK RATIO AND PEAK VS. VALLEY/PEAK RATIO
!***  SUBROUTINES CALLED - NONE
      REAL :: SMM(2,*),  PMAX, VP(*), RAN(*), PI(*)
      INTEGER :: I1, I3, RR(NNRAN,*), NNRAN, PR(NNPI,*), NNPI

      COMMON ISKIP(26), NPI, NRAN, IVP
      INTEGER :: ISKIP, NPI, NRAN, IVP

      INTEGER :: KMM, K, L, M
      REAL :: RANGE, R

      DO KMM = I1,I3
         IF (SMM(2,KMM) .GT. PMAX) PMAX = SMM(2,KMM)
         RANGE = SMM(2,KMM) - SMM(1,KMM)
         R = SMM(1,KMM) / SMM(2,KMM)
!***     TEST RATIO (MIN.MAX) AGAINST INPUT VALLEY/PEAK RATIO
         K=MINLOC(VP(1:IVP),1,MASK=(VP(1:IVP).GE.R))
         IF (K.EQ.0) K = IVP + 1
!***     TEST RANGE AGAINST INPUT RANGE INTERVALS
         L=MINLOC(RAN(1:NRAN),1,MASK=(RAN(1:NRAN).GE.RANGE))
         IF (L.EQ.0) L = NRAN + 1
         RR(L,K) = RR(L,K) + 1
!***     TEST MAXIMUM STRESS AGAINST INPUT PEAK INTERVALS
         M=MINLOC(PI(1:NPI),1,MASK=(PI(1:NPI).GE.SMM(2,KMM)))
         IF (M.EQ.0) M = NPI + 1
         PR(M,K) = PR(M,K) + 1
      END DO
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE PRNTSS (RR, NNRAN, PR, NNPI, IRR, NRMAX,     &
                         IIVP, RAN, VP, PI, IS, NFF, ISS,     &
                         II, NTF, NFT, PMAX, MFT)
!***  THIS SUBROUTINE PRINTS OUT THE TWO SPECTRUM SUMMATION   *****
!***  TABLES, RANGE VERSUS VALLEY/PEAK RATIO, AND PEAK VERSUS *****
!***  PEAKS AND THEIR CORRESPONDING FLIGHT NUMBER.            *****
!***  SUBROUTINES CALLED - NEW PG                             *****

      INTEGER :: RR(NNRAN,*), NNRAN, PR(NNPI,*), NNPI, IIVP, IS, II
      INTEGER :: IRR(NRMAX,*), NRMAX, NFF(2,*), ISS(*), NTF,NFT,MFT(*)
      REAL :: RAN(*), VP(*), PI(*), PMAX(*)

      COMMON ISKIP(26), NPI, NRAN, IVP, SKIP(8), IPSS, SSKIP(5), MAXHP
      INTEGER :: ISKIP, NPI, NRAN, IVP, IPSS, MAXHP
      REAL :: SKIP, SSKIP

      CHARACTER*4 :: TITLE1(6), TITLE2(6), TITLE3(2), TITLE4(4)
      INTEGER :: FORM(4), FMAT(6), I, J, IVP1, IVP2, IVP3
      INTEGER :: LINE, JLINE, ITAB, K, KK, M1, M2, M3


      DATA TITLE1/'FLIG','HT  ','NUMB','ER  ',' NUM','BER '/
      DATA TITLE2/' TYP','E   ','FLIG','HTS ',' CYC','LES '/
      DATA TITLE3/'TOTA','L   '/
      DATA TITLE4/'PEAK','   F','LIGH','T   '/
      !DATA TITLE1/4HFLIG,4HHT  ,4HNUMB,4HER  ,4H NUM,4HBER /
      !DATA TITLE2/4H TYP,4HE   ,4HFLIG,4HHTS ,4H CYC,4HLES /
      !DATA TITLE3/4HTOTA,4HL   /
      !DATA TITLE4/4HPEAK,4H   F,4HLIGH,4HT   /

!***  PRINT THE RANGE VERSUS VALLEY/PEAK RATIO********************
      IVP3 = IIVP - 1
      DO I = 1,NNRAN
      DO J = 1,IVP3
         RR(I,IIVP) = RR(I,IIVP) + RR(I,J)
      END DO
      END DO

      IRR (NNRAN,1:IIVP) = RR(1,1:IIVP)

      DO I = 1,NRAN
      DO J = 1,IIVP
         IRR(NNRAN -I,J) = IRR(NNRAN-I+1,J) + RR(I+1,J)
      END DO
      END DO

      DO I = 1,NRAN
      DO J = 1,IIVP
         IRR(I+1,J) = IRR(I,J) - RR(I,J)
      END DO
      END DO

      LINE = 50
      IVP1 = 1
  120 IVP2 = IVP1 + 5
      IF (IVP2 .GT. IIVP) IVP2 = IIVP
      IVP3 = IVP2
      IF (IVP2 .GT. IVP) IVP3 = IVP

      IF (LINE .GE. 45) THEN
         LINE = 0
         CALL NEWPG
         WRITE (6,10) II, IRR(1,IIVP)
      END IF

      WRITE (6,20) (VP(J), J=IVP1,IVP3)
      IF (IVP2 .EQ. IIVP) THEN
         ITAB = IVP2 -IVP1 + 1
         FORM(2) = FMAT(ITAB)
         !WRITE (6,FORM) TITLE3
         WRITE (6,'(2A4)') TITLE3
      END IF

      WRITE (6,30)
      WRITE (6,50) RAN(1), (RR(1,J),IRR(1,J),J = IVP1,IVP2)
      LINE=LINE+4
      DO I=2,NRAN
         WRITE(6,40)RAN(I-1),RAN(I),(RR(I,J),IRR(I,J),J=IVP1,IVP2)
         LINE=LINE+1
         IF ((LINE.LT.48).OR.(I.EQ.NRAN)) CYCLE
         LINE=0
         CALL NEWPG
         WRITE (6,10) II,IRR(1,IIVP)
         WRITE (6,20) (VP(J),J=IVP1,IVP3)
         IF(IVP2.EQ.IIVP) WRITE (6,'(2A4)') TITLE3
         WRITE (6,30)
      END DO
      WRITE(6,60)RAN(NRAN),(RR(NNRAN,J),IRR(NNRAN,J),J=IVP1,IVP2)

!***  PRINT THE PEAK VERUS VALLEY/PEAK RATIO *********************
      IVP1 = IVP2 + 1
      IF (IVP1 .LE. IIVP) GO TO 120
      IVP3 = IIVP -1
      DO I = 1,NNPI
      DO J = 1,IVP3  !PSV IVP2?
         PR(I,IIVP) = PR(I,IIVP) + PR(I,J)
      END DO
      END DO

      DO J = 1,IIVP
         IRR(NNPI,J) = PR(1,J)
      END DO

      DO I = 1,NPI
      DO J = 1,IIVP
         IRR(NNPI-I,J) = IRR(NNPI-I+1,J) + PR(I+1,J)
      END DO
      END DO

      DO I = 1,NPI
      DO J = 1,IIVP
         IRR(I+1,J) = IRR(I,J) - PR(I,J)   ! ? PSV RR?
      END DO
      END DO

      IF (LINE .GE. 35) THEN
         LINE = 0
         CALL NEWPG
         WRITE (6,10) II, IRR(1,IIVP)
      END IF

      IVP1 = 1
  210 IVP2 = IVP1 + 5
      IF (IVP2 .GT. IIVP) IVP2 = IIVP
      IVP3 = IVP2
      IF (IVP2 .GT. IVP) IVP3 = IVP
      IF (LINE .GE. 45) THEN
         LINE = 0
         CALL NEWPG
         WRITE (6,10) II, IRR(1,IIVP)
      END IF
      WRITE (6,20) (VP(J), J=IVP1, IVP3)
      IF (IVP2 .EQ. IIVP) THEN
         ITAB = IVP2 - IVP1 + 1
         FORM(2) = FMAT(ITAB)
         !WRITE (6,FORM) TITLE3
         WRITE (6,'(2A4)') TITLE3
      END IF

      WRITE (6,70)
      WRITE (6,50) PI(1),(PR(1,J),IRR(1,J), J=IVP1,IVP2)
      LINE = LINE + 4
      DO I = 2,NPI
         WRITE(6,40) PI(I-1),PI(I),(PR(I,J),IRR(I,J),J=IVP1,IVP2)
         LINE = LINE + 1
         IF ((LINE .LT. 48) .OR. (I .EQ. NPI)) CYCLE
         LINE = 0
         CALL NEWPG
         WRITE (6,10) II, IRR(1,IIVP)
         WRITE (6,20) (VP(J), J=IVP1, IVP3)
         IF (IVP2 .EQ. IIVP) WRITE (6,'(2A4)') TITLE3
         !WRITE (6,FORM) TITLE3
         WRITE (6,70)
      END DO

      WRITE (6,60) PI(NPI),(PR(NNPI,J),IRR(NNPI,J),J=IVP1,IVP2)
      IVP1 = IVP2 + 1
      IF (IVP1 .LE. IIVP) GO TO 210
      JLINE = 50
      DO J = 1,NFT,4
         M1 = J
         M2 = J + 3
         IF (M2 .GT. NFT) M2 = NFT
         M3 = M2 - M1 + 1
         IF (JLINE .GE. 45) THEN
            CALL NEWPG
            WRITE (6,10) II, IRR(1,IIVP)
            WRITE (6,260) ((TITLE1(K), K = 1,6), KK = 1,M3)
            WRITE (6,260) ((TITLE2(K), K = 1,6), KK = 1,M3)
            JLINE = 0
            END IF
         WRITE (6,280) (K,(NFF(I,K), I = 1,2), K = M1,M2)
         JLINE = JLINE + 1
      END DO

!***  PRINT THE HIGHEST PEAKS AND THEIR CORRESPONDING  *******
!***  FLIGHT NUMBER                                    *******
      IF (MAXHP .NE. 0) THEN
         M3 = 6
         IF (M3 .GT. MAXHP) M3 = MAXHP
         IF (JLINE .GE. 45) THEN
            CALL NEWPG
            JLINE = 0
         END IF
         WRITE (6,320) MAXHP
         WRITE (6,330) ((TITLE4(K), K = 1,4), KK = 1,M3)
         JLINE = JLINE + 3

         DO J = 1,MAXHP,6
            M1 = J
            M2 = J + 5
            IF (M2 .GT. MAXHP) M2 = MAXHP
            WRITE (6,340) (PMAX(K), MFT(K), K = M1,M2)
            JLINE = JLINE + 1
         END DO
      END IF

      IF (IPSS .NE. IS) THEN
         IS = IS + 1
         IPSS = ISS(IS)          ! PSV NPSS ->IPSS
         !KLINE = 60
         RR(1:NNRAN,IIVP) = 0
         PR(1:NNPI,IIVP) = 0
      END IF

      RETURN

   10 FORMAT (30X, 'SPECTRUM SUMMATION FOR A TOTAL OF',  &
              I5, ' FLIGHTS AND', I7, ' CYCLES')
   20 FORMAT (2X, 'VALLEY.PEAK RATIO ', 6X, 6F16.2)
   30 FORMAT (14X, 'RANGE ')
   40 FORMAT (9X, 2F7.0, 7(2I7,2X))
   50 FORMAT (1X, 'BELOW OR EQUAL ', F7.0, 7(2I7,2X))
   60 FORMAT (9X, ' ABOVE ', F7.0, 7(2I7,2X))
   70 FORMAT (15X, 'PEAK ')
  260 FORMAT (4(6A4,5X))
  280 FORMAT (4(I5, 2I9, 6X))
  320 FORMAT (//, 10X, 'THE', I4, ' HIGHEST PEAKS')
  330 FORMAT ( 4X, 6(4A4,5X))
  340 FORMAT (6(F8.2,I8,5X))
      END

!----------------------------------------------------------------------
      SUBROUTINE ERROR (I, J, K, L)
!***  THIS SUBROUTINE PRINTS OUT THE ERROR MESSAGES  ***********
!***  SUBROUTINES CALLED - NONE

      INTEGER :: I, J, K, L

      COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI,     &
             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP, ELIMV

      INTEGER :: NPG,TITLE,NSIZE,LEFT,IERR,IRS,IUIL,NPI,NRAN,IVP,IFI,NXY
      REAL :: CLIP, CLIV, FACTOR, ELIMP, ELIMV

      IERR = IERR + 1
      WRITE (6,10) IERR, I, J, K, L
      IF (IERR .GT. 900) STOP 701
      RETURN
   10 FORMAT ('ERROR NUMBER',I6,' IS TYPE',I4,' AND INVOLVES',3I7)
      END

!----------------------------------------------------------------------
      SUBROUTINE RITE (A, MAT, NSIZE)
!***  THIS SUBROUTINE WRITE AND ENTIRE SEQUENCE    ***************
!***  OF MAXIMUM AND MINIMUM STRESSES FOR ONE      ***************
!***  FLIGHT ONTO TEMPORARY STORAGE.               ***************
!***  SUBROUTINES CALLED - WRITEMS                 ***************
      REAL :: A(NSIZE)
      INTEGER :: MAT, NSIZE
      CALL WRITEMS (4, A, NSIZE, MAT, -1)
      RETURN
      END

!----------------------------------------------------------------------
      SUBROUTINE WTAPE (LREC, MCY, SMM, NTF, I1)
!***  THIS SUBROUTINE WRITES THE FLIGHT NUMBER,    ***************
!***  THE NUMBER OF CYCLES IN THE FLIGHT, AND THE  ***************
!***  MAXIMUM AND MINIMUM STRESSES FOR THE FLIGHT  ***************
!***  ON THE OUTPUT TAPE.                          ***************

      INTEGER :: LREC(NTF), MCY(NTF), NTF, I1, M
      REAL :: SMM(2,*)

      COMMON ISKIP(29), IFI, ISK(9), IAFS
      INTEGER :: ISKIP, IFI, ISK,    IAFS
      INTEGER I, J, ICY, JCY, IFL, MAT

      IF (IAFS .NE. 0) THEN
!***     WIRE IF ALTERNATE FLIGHT SEQUENCE IS SPECIFIED ***********
         WRITE (IFI) NTF
         DO I = 1, NTF
            MAT = LREC(I)
            ICY = MCY(LREC(I))
            CALL REED (SMM, MAT, ICY)
            ICY = ICY / 2
            WRITE (IFI) I, ICY,((SMM(M,J), M = 1,2), J = 1,ICY)
!***        WRITE (6,10) I, ICY,((SMM(M,J), M = 1,2), J = 1,ICY)
         END DO
      ELSE
         IFL = LREC(1)
         IF (IFL .EQ. 1) WRITE (IFI) NTF
         ICY = MCY(1)
         JCY = ICY -I1 + 1
         WRITE (IFI) IFL, JCY, ((SMM(M,J), M = 1,2), J = I1,ICY)
!***     WRITE (6,10) IFL, JCY,((SMM(M,J), M = 1,2), J = I1,ICY)
      END IF
      RETURN
!  10 FORMAT ('WTAPE', 2I5, 10X, 8F12.2, / (10F13.2))
      END

!----------------------------------------------------------------------
      SUBROUTINE OPENMS(IU, NARR, NFT1, K)
      INTEGER :: IU, NARR(*), NFT1, K
      OPEN (IU, FILE='tape.dat', ACCESS='DIRECT', RECL=40000, &
            FORM='UNFORMATTED', ERR=30 )
      RETURN
  30  write(6,*) 'OPENMS error open'
      RETURN
      end

!----------------------------------------------------------------------
      SUBROUTINE READMS(IU, ARR, NA, MAT)
      INTEGER :: IU, NA, MAT
      REAL :: ARR(*)
      READ (IU, REC=MAT, ERR=50) ARR(1:NA)
      RETURN
  50  write(6,*) 'READMS error read'
      RETURN
      end

!----------------------------------------------------------------------
      SUBROUTINE WRITEMS(IU, ARR, NA, MAT, IR)
      INTEGER :: IU, NA, MAT, IR
      REAL :: ARR(*)
      WRITE (IU, REC=MAT, ERR=50 ) ARR(1:NA)
      RETURN
  50  write(6,*) 'WRITEMS error write'
      RETURN
      end
