
      PROGRAM MAIN 
C        REWIND TAPE 1
         CALL MMAIN
         WRITE (6,10)
   10    FORMAT ('CALLING RANDOM PROGRAM')
C        REWIND TAPE 3
            close(10)
         CALL RDMAIN
         STOP
      END

      SUBROUTINE MMAIN
         COMMON X, YR
         COMMON CSUM, CYCLSM, MAXN, CY, STSMXM, STSMNM
         COMMON /TABB/TKSIG
C***  DOUBLE PRECISION DATA
         DIMENSION YAW(40), CYBT(40),CYBT0(40)
         DIMENSION X(3958), Y(3958), CDMGM(40), N(40), F(40), AM(40),
     1   M5(40), DELTAY(25), DELT1(25), DELT2(25), DELT3(25), DELT4(25),
     2   DELT5(25), DELT6(25), DELY1(40), DELY11(40), DY(40,25),
     3   AMIDY(25), CDAMG(25), P(40), N1FLAG(40), YMAX(25), YMIN(25),
     4   M3(40), CUMM(25), T(40), TAB1(25), TAB2(25), TAB3(25), 
     5   TAB4(25), TAB5(25), TAB6(25), ARNO1(40), SGMAX1(40), ARNO2(40),
     6   SGMAX2(40), ARNO3(40), SGMAX3(40), ABR(40), VELOS(40), 
     7   SLOPE(40), AKSIG(40), WT(40), P1(40), AK1(40), P2(40), AK2(40),
     8   STSMXM(40,25), STSMNM(40,25), CYCLSM(40,25), TABL1(25), 
     9   TABL2(25), TABL3(25), TABL4(25), TABL5(25), TABL6(25), 
     A   TABL7(25), TABL8(25), TABL9(25)
         DIMENSION CSUM(40,25), K1(40), ISTRES(40), TBLM2(434),
     1   TBLI2(1542), IA(40), CYC(25), DMAGEM(25), ABC(40), N2(40), 
     2   N6(40), QMAX(1000), QMIN(1000), JX(1000), JI(1000), MAXN(40),
     3   DATAIN(13), SCLTRB(40), TKSIG(257), SIG(40)
         DIMENSION TBLSN(257), TBLLD(31)
         dimension cumm1(25), cumm2(25), cumm3(25)
         EQUIVALENCE (IEND, X(1)), (KEND, X(2)), (I4, X(3)),
     1   (SIGULT, X(4)), (WAREA, X(5)), (ISTRES, X(6)), (DELT1, X(46)),
     2   (TAB1, X(71)), (TAB2, X(96)), (TAB3, X(121)), (TAB4, X(146)),
     3   (TAB5, X(171)), (AC, X(196)), (IW1, X(197)), (IW2, X(198)),
     4   (IW3, X(199)), (IW4, X(200)), (IRR, X(201)), (ICASE, X(202)),
     5   (IW5, X(203)), (TAB6, X(206)), (DELT2, X(231)), 
     6   (DELY1, X(256)), (DELY11, X(296)), (ARNO1, X(336)), 
     7   (ARNO2, X(376)), (ARNO3, X(416)), (SGMAX1, X(456)), 
     8   (SGMAX2, X(496)), (SGMAX3, X(536)), (T, X(576)), 
     9   (AKSIG, X(616)), (SLOPE, X(656)), (VELOS, X(696)), (WT, 
     A   X(736)), (P1, X(776)), (TBLM2, X(853))
         EQUIVALENCE (P2, X(1473)), (AK1, X(1513)), (AK2, X(1553)),
     1   (ABR, X(1593)), (IA, X(1633)), (M3, X(1673)), (SIG, X(1713)),
     2   (AM, X(1753)), (N, X(1793)), (NEND, X(1833)), (AL6, X(1850)),
     3   (AL5, X(1851)), (AL4, X(1852)), (AL3, X(1853)), (AL2, X(1854)),
     4   (AL1, X(1855)), (TBLI2, X(1856)), (M5, X(3398)),
     5   (N1FLAG, X(3438)), (F, X(3478)), (N6, X(3518)), (N2, X(3558)),
     6   (P, X(3598)), (SCLTRB, X(3638)), (TABL1, X(3678)),
     7   (TABL2, X(3703)), (TABL3, X(3728)), (TABL4, X(3753)),
     8   (TABL5, X(3778)), (TABL6, X(3803)), (TABL7, X(1287)),
     9   (TABL8, X(1312)),(TABL9, X(1337)), (DELT3, X(1362)),
     A   (DELT4, X(1387))
         EQUIVALENCE (DELT5, X(1412)), (DELT6, X(1437)), (L1, X(1462))
         EQUIVALENCE (CBART, X(3837)), (AST, X(3838)), (YAW, X(3839)),
     1               (CYBT, X(3879)), (CYBTO,X(3919))
         REAL JX, JT, JSUM, ISLM, NEND, JI
         X=0.0
         Y=0.0
         DELTAY=0.0
         CUMM=0.0
         CUMM1=0.0
         CUMM2=0.0
         CUMM3=0.0
         DO K = 1,25
            DO L = 1,40
               CSUM(L,K) = 0.0
               CYCLSM(L,K) = 0.0
            END DO
         END DO
         READ (5,10) IREAD, ICARD
         IF (IREAD .NE . 2) THEN
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

   85    II = 0
   90    N3958 = 3958
         CALL NPUT1A (X(1), N3958, Y(1), II, IREF, ICAS, 0)
C***     WRITE REFERENCE RUN, CASE NO., AND SEGMENTS ON TAPE FOR
C***     SPECTRUM LOADING RANDOM SEQUIENCE GENERATION PROGRAM
         WRITE (3) IRR, ICASE, IEND
         WLPRNT = 0.0
         IF (IW5 .NE. 2) THEN
            CALL PRINT
            WLPRNT = 1.0
         END IF
         B = 0.0
         K1(1) = 0
         TCDMGM = 0.0
  120    DO I = 1, IEND
            cumm=0.0
            DO KJ = 1, 257
               TBLSN (KJ) = 0.0
            END DO
            DO KJ = 1, 31
               TBLLD (KJ) = 0.0
            END DO
            INTPER = 0
            IF (B .NE. 1.0) THEN
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
                     write(10,*)'1tw',XARG,YARG,TKSIG,OUTPUT,NSEG,LEVEL
                     AKSIG(I) = OUTPUT
                     AKSIG(I) = R1 * AKSIG(I)
                  END IF
               END IF
               
               DO J = 1, JEND
C                 CALCULATE THE CUMULATIVE CYCLES GIVEN VALUES OF DELTA Y
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
                     CUMM(J) = (ARNO1(I)*EXP(-DELTAY(J)**2/
     1                         (2.0*(SGMAX1(I))**2))
     2                       +  ARNO2(I)*EXP(-DELTAY(J)**2/
     3                         (2.0*(SGMAX2(I))**2))
     4                       +  ARNO3(I)*EXP(-DELTAY(J)**2/
     5                         (2.0*(SGMAX3(I))**2))) *  T(I)
                  CASE (8,9,13,14,15)
                     IF (M1.EQ.8 .OR. M1.EQ.13) ABR(I) = 
     1                 (VELOS(I)*SLOPE(I)*WAREA*AKSIG(I))/(498.0*WT(I))
                     IF (M1.EQ.14 .OR. M1.EQ.15) THEN
                        AMGT = (2*WT(I)/(RHO1*CBART*32.2*SLOPE(I)*AST)*
     1                  (YAW(I)/WT(I))) / SCLTRB(I)**2
                        AKSIG(I) = 0.88 * AMGT /(5.3 + AMGT)
                     END IF
                     IF (M3(I).EQ.14) ABR(I) = VELOS(I)*WAREA*AKSIG(I)
     1                  /(498.*WT(I))*(CYBT(I) + CYBT0(I))
                     IF (M3(I) .EQ. 15) 
     1                  ABR(I) = (VELOS(I)*AST*SLOPE(I)/498.)*AKSIG(I)
                     CUMM(J) = (ARNO1(I)*P1(I)*EXP(-DELTAY(J)/
     1                         (AK1(I)*ABR(I)))
     2                         +  ARNO2(I)*P2(I)*EXP(-DELTAY(J)/
     3                         (AK2(I)*ABR(I)))) * T(I)
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
               IF (AX .NE. 1.0) THEN
C                 CALCULATE CYCLES FOR Y MAX AND Y MIN.
                  DO J = 1, K
                     CYCLSM(I,J) = CUMM(J) - CUMM(J+1)
                  END DO
               END IF
            END IF

            DO J = 1, K
               IF(B .NE. 1.0) THEN
                  IF (AX .NE. 1.0) THEN
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
  690                   DMAGEM(J) = 0.0
                        CYC(J) = 0.0
                        CDAMG(J) = 0.0
                        IF (B .EQ. 1.0) GO TO 1540
                        IF (INTPER . EQ. 0) CYCLE
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
               IF (B .EQ. 1.0) NSEG = 50
               ! SUBROUTINE TWOVIN - LINEAR - QUADRATIC INTERPOLATION OF S-N DATA.
               ! GIVEN THE INTERPOLATING VALUES XARG AND YARG, INTERPOLATE FOR A
               ! VALUE OF CYCLES TO FAILURE.
               CALL TWOVIN (XARG, YARG, TBLSN, OUTPUT, NSEG, LEVEL)
               write(10,*)'2tw',XARG,YARG,TBLSN,OUTPUT,NSEG,LEVEL
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
               IF (B .EQ. 1.0) GO TO 1530
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

            !WRITE TAP FOR SPECTRUM LOADING RANDOM SEQUENCE GENERATION PROGRAM
            JJEND = JEND - 1
            IF ((M3(I) .GT. 9) .AND. (M3(I) .LT. 13)) JJEND = JEND
            WRITE (1)JJEND, (STSMXM(I,J), STSMNM(I,J), CYCLSM(I,J), 
     1             J = 1, JJEND)

            IF (IW2 .EQ. 2) CYCLE
            WLPRNT = 1.0
            IF (IW5 .LT. 2 .OR. I.GT.1)  WRITE (6,100) IRR, ICASE
            WRITE (6,970) I
            WRITE (6,980)
            WRITE (6,990)
            DO J = 1, JEND
               IF ((M3(I) .LE. 9) .OR. (M3(I) .GE. 13))
     1            WRITE (6,1000) J, DELTAY(J), CUMM(J)
               IF (J . EQ. JEND) CYCLE
               IF ((M3(I).GT.9).AND.(M3(I).LT.13).OR.M3(I).GT.15) THEN
                  WRITE(6,1020) J,STSMXM(I,J),STSMNM(I,J),CYCLSM(I,J),
     1                        CYC(J), DMAGEM(J), CDAMG(J)
               ELSE
                  WRITE (6,1040) STSMXM(I,J), STSMNM(I,J), CYCLSM(I,J), 
     1                        CYC(J), DMAGEM(J), CDAMG(J)
               END IF
            END DO
            IF ((M3(I) .EQ. 8) .OR. (M3(I) .EQ. 9)) WRITE (6,1060) 
     1          AKSIG(I), ABR(I)
            IF ((M3(I) .GE. 13) .AND. (M3(I) .LE. 15)) WRITE (6,1060)
     1          AKSIG(I), ABR(I)
            ABC(I) = TCDMGM
         END DO

         IF (I4 .NE. 0) THEN
            CUMDMG = 0.0
            CUMXN = 0.0
            JSUM = 0
            TSUM = 0
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
               IF ((I4 .GT. 12) . AND. (I4 .LT. 19)) I2 = (I4 -12)
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
               
 1530          ALIFE = ALOG10(ALIFE)
               IF (OUTPUT .GE. ALIFE) THEN
 1540             DMG = 0.0
                  CYF = 0.0
               END IF
               IF (YARG .LT. YARGMN) THEN
                  IF (OUTPUT.GE.ALIFE) WRITE (6,1550) XARG, YARG
                  CYF = 10.0 ** OUTPUT
                  DMG = XN / CYF
               END IF
               CUMDMG = CUMDMG + DMG
               
               IF (IW1 .NE. 2) THEN
                  WRITE(6,1580)
     1            QMAX(L),QMIN(M),XN,CUMXN,YART,CYF,DMG,CUMDMG
                  ILINE = ILINE + 1
                  IF (ILINE .GE. 54) THEN
                     ILINE = 0
                     WRITE (6,100) IRR, ICASE
                     WRITE (6,1410)
                     WRITE (6,1420)
                  END IF
               END IF
               IF ((L .EQ. JOUT) .AND. (M .EQ. IOUT)) EXIT
               IF ((JX(L) .EQ. 0.0) .AND. (A .EQ. 0.0)) L = L + 1
               IF (A .EQ. 1.0) L = L + 1
               IF (A .EQ. 0.0) M = M + 1
            END DO
            
            !CALCULATION OF TOTAL DAMAGE INCLUDING GAG
            TCDMGM = TCDMGM + CUMDMG
         END IF
         IF (IW4 .NE. 2) THEN
            CALL SPECSM
            WLPRNT = 1.0
         END IF
         IF (WLPRNT .NE. 0.0) THEN
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
   10    FORMAT (I3, I3)
C  20    FORMAT (13A6)  ! PSV 
   20    FORMAT (13A4)  ! PSV
   30    FORMAT ('1')
   40    FORMAT (13A4)
  100    FORMAT ('REFERENCE RUN NO.', I6, 4X, 'CASE NO', I6)
  340    FORMAT (
     1   'GUST ALLEV. INTRP. ERROR X IS TOO SMALL. X ='
     2    ,E14.6,1X,'Y IS TOO SMALL. Y =',E14.6, 2X,
     3   'SEG =', I2)
  360    FORMAT (
     1   'GUST ALLEV. INTRP. ERROR, X IS TOO SMALL.'
     2   ' X =',E14.6, 2X, 'SEG = ', I2)
  710    FORMAT ('SN, INTP ERROR X IS TOO SMALL X = ',E14.6, 1X,
     1   'Y IS TOO SMALL Y = ',E14.6, 1X, 'SEG =', I2, 1X,
     2   'LOAD LEVEL =', I2, 1X, 'DAMAGE SET = 0.0')
  970    FORMAT(4X, 'SEGMENT =', I2)
  980    FORMAT ('--------------------------SPECTRUM-------------'
     1   '-------------------', 2X, '------------DAMAGE CALCULATION'
     2   '------------')
  990    FORMAT (5X, 1HJ, 3X, 'DELTA Y', 3X,
     1   'CUMULATIVE CYCLES', 2X, 'MAX STRESS', 2X 'MIN STRESS',
     2   8X, 'CYCLES', 10X, 'ALLOWABLE', 8X, 'DAMAGE', 2X,
     3   'CUM DAMAGE')
 1000    FORMAT (1H, 4X, I2, F13.3,1X,F16.4)
 1020    FORMAT (1H , 4X, I2, 32X, F10.0, 2X, F10.0, 2X, F16.4, 
     1           2X, F16.0, 1X, F11.7, 1X, F11.7)
 1040    FORMAT (38X, F10.0, 2X, F10.0, 2X, F16.4, 2X, F16.0,
     1   1X, F11.7, 1X, F11.7)
 1060    FORMAT ( 5X, 'GUST ALLEVIATION FACTOR = ',F9.6, 5X,
     1   'A-BAR = ', F16.6)
 1170    FORMAT ('MAX AND MIN STRESSES AND CYCLE ARRAYS FORMED FOR'
     1           'THE DEFINITION OF THE GAG CYCLES')
 1180    FORMAT ('ARRAYS ARE FROMED FROM SEGMENTS AS SPECIFIED BY'
     1           'FLAG = N6.')
 1190    FORMAT (4X, 'MAX STRESS', 10X, 'CYCLES', 10X, 'CUM CYCLES',
     1   10X, 'MIN STRESS', 10X, 'CYCLES', 10X, 'CUM CYCLES')
 1270       FORMAT (4X, F10.0, 4X, F16.4, 3X, F16.4)
 1350    FORMAT (60X, F10.0, 4X, F16.4, 3X, F16.4)
 1410    FORMAT (' GAG CYCLE SPECTRUM AND DAMAGE CALCULATION')
 1420    FORMAT ( 5X, 'MAX', 7X, 'MIN STRESS', 9X, 'CYCLES',
     1   9X, 'CUM CYCLES', 9X, 'R', 8X, 'ALLOWABLE', 7X, 'DAMAGE',
     2   4X, 'CUM DAMAGE')
 1550    FORMAT (' SN ITERP. ERROR. X IS TOO SMALL. X = E14.6',
     1   1X, 'Y IS TOO SMALL. Y = E14.6, 1X, DAMAGE IS SET = 0.0', 
     2   1X, '(GAG SEGMENT)')
 1580    FORMAT (2X, F15.0, 2X, F15.0, 2X, F16.4, 2X, F16.4, 1X, F7.3,
     1   2X, F16.0, 1X, F11.7, 1X, F11.7)
 1630    FORMAT ('INDIVIDUAL SEGMENT AND TOTAL DAMAGE SUMMARY')
 1640    FORMAT ( 8X, 'SEG.', 7X, 'DAMAGE', 11X, 'TOTAL')
 1660    FORMAT ( 8X, 'SEG.', 7X, 'DAMAGE', 11X, 'TOTAL')
 1680    FORMAT (9X, I2, F16.7, F16.7)
 1690    FORMAT (9X, 'GAG', F16.7, F16.7)
      END

      BLOCK DATA
         COMMON /TABB/TKSIG
         DIMENSION TKSIG(257)
         DATA TKSIG   /15.0,10.0,20.0,30.0,40.0,50.0,70.0,100.0,
     1   150.0,300.0,500.0,1000.0,0.0,0.0,0.0,0.0,15.0,6.0,10.0,14.0,  
     2   18.0,22.0,30.0,40.0,60.0,80.0,100.0,120.0,140.0,160.0,180.0,   
     3   240.,2.954,2.69,2.515,2.295,2.162,1.972,1.82,1.622,1.514,1.413, 
     4   1.349,1.289,1.259,1.231,1.162,3.8,3.59,3.31,3.02,2.82,2.514,  
     5   2.19,1.884,1.719,1.597,1.48,1.413,1.35,1.303,1.202,4.22,4.075, ! PSV 1.363 -> 1.303
     6   3.8,3.51,3.315,2.95,2.632,2.19,1.998,1.82,1.68,1.604,1.513, 
     7   1.446,1.319,4.46,4.36,4.14,3.8,3.635,3.275,2.884,2.51,2.29,  
     8   2.02,1.862,1.74,1.7,1.64,1.48,4.68,4.63,4.36,4.15,3.98,3.55, 
     9   3.16,2.755,2.483,2.24,2.09,1.95,1.862,1.78,1.62,5.012,5.065,  
     A   4.9,4.67,4.46,4.07,3.63,3.16,2.85,2.63,2.4,2.24,2.09,1.995,1.8,
     B   5.346, 5.52,5.5,5.37,5.14,4.67,4.26,3.72,3.35,3.02,2.755,
     C   2.632,2.514,2.345,2.09,5.624,6.026,6.05,6.03,5.89,5.63,5.14,
     D   4.52,4.07,3.645,3.39,3.125,2.92,2.758,2.458,6.026,6.457,6.748,  ! PSV 6.748 -> 6.746
     E   6.903,6.919,6.839,6.607,5.95,5.37,4.9,4.56,4.26,4.07,3.8,3.315,
     F   6.096,6.684,6.919,7.228,7.328,7.345,7.245,6.887,6.457,5.95,
     G   5.63,5.25,5.01,4.74,4.16,6.166,6.839,7.145,7.413,7.586,7.727,
     H   7.763,7.586,7.379,7.079,6.808,6.562,6.309,6.03,5.31,60*0.0/
      END



         SUBROUTINE NPUT1A (CASE, NCASE, RAREA, IENTRY, IREF, ICAS, KP)
C
C     NPUT1A - STANDARD DATA INPUT
C
C     THIS SUBROUTINE READS A STANDARD DATA FORM (VAIRABLE NUMBER OF
C       CARDS WITH UP TO FOUR VALUES PER CARD) AND STORES VALUES
C       (INTEGER AND/OR REAL) INTO AN ARBITRARY LENGTH ARRAY.  A
C       PROCEDURE FOR EITHER REPLACIING OR UPDATING REFERENCE RUN
C       ARRAYS IS ALSO PROVIDED.

C       CASE   = OUTPUT = A LINER ARRAY (SEE Y) CONTAINING THE DATA
C                        READ FROM CARDS
C       NCASE  = IN-OUT = THE UPPER LIMIT OF THE CASE ARRAY

C       RRAREA = OUT-IN = A REFERENCE RUN ARRAY (OF THE SAME LENGTH
C                        AS CASE) IF RRAREA IS NOT EQUIVALENT TO
C                        CASE.  THE SUBROUTINE SAVES DATA IN THIS
C                        AREA BETWEEN CALLS.  IF RRAREA IS
C                        EQUIVALENCED TO CASE, THEN RRAREA IS NOT
C                        USED AND THERE CONNOT BE ANY REFERENCE RUN
C                        DATA

C       IENTRY  = I-OUT = ENTRY FLAG (INTEGER)
C                       =  0  INITITAL ENTRY ONLY (INPUT)
C                       =  2  NEW ENTRY ONLY (INPUT) TO CHANGE CASE,
C                                Y, AND/OR RRAREA.  NO INFORMATION
C                                IS SAVED FROM PREVIOUS CASE OR
C                                RAREA ARRAYS.
C                       =  3  OVERLAY RRAREA (INPUT). PERMITS
C                               OVERLAYING OF REFERENCE RUN DATA
C                       =  1  NORMAL OUTPUT FLAG (OUTPUT)
C                       = -1  LAST CASE FLAG (OUTPUT).  THE NEXT
C                               ENTRY TO THE SUBROUTINE WILL
C                               TERMINATE YOUR JOB.

C       IREF    = OUTPUT = REFERENCE RUN NUMBER (INTEGER) TAKEN FROM
C                         INPUT CARDS

C       ICAS    = OUTPUT = CASE NUMBER (INTEGER) TAKEN FROM DATA CARDS
C
C       KP      = INPUT  = OPTIONAL PRINT CODE (INTEGER)
C                           IF KP IS NOT IN ARGUMENT LIST, IT IS
C                           TREATED AS IF KP = 0
C                        = C   EJECT PAGE FOR EACH NEW DATA CASE
C                        = L   SINGLE SPACE FORE EACH NEW DATA CASE
C                        = ANYTHING ELSE - NO PRINTING AT ALL

C       THIS ROUTINE USES ZZL1 AND ARGQ
C                       THE IABS AND ISIGN FUNCTIONS

C       FORTRAN UNIT 5 IS READ (FOR DATA)
C       FORTRAN UNIT 6 MAY BE WRITE (SEE INPUT PARAMETER KP) WITH
C         REFERENCE RUN AND CASE NUMBERS AT THE TOP OF A NEW PAGE (OR
C         AFTER ONE BLANK LINE) AFTER EACH EXECUTED CALL STATEMENT.
C         ERROR MESSAGES MAY ALSO BE WRITTEN.

C      CARD FORM IS
C            CC  1    MUST CONTAIN A 1
C            CC  2-6, 18-22, 34-38, 50-54,    LOCATION FIELDS
C            CC  7-15, 23-31, 39-47, 55-63    FRACTION FIELDS
C            CC  16-17, 32-33, 48-49, 64-65   EXPONENT FIELDS
C            CC  66-68  UNDUSED
C            CC  69-70  REFERENCE RUN NUMBER
C            CC  71-73  CASE NUMBER
C            CC  74-80  UNDUSED

            INTEGER CASENO, CASNUM, ENTRY1, REFNO, REFNUM

            LOGICAL NER1, NER2, NER3, REFRUN

            DOUBLE PRECISION A
            character(80) line

            DIMENSION CASE(1), RRAREA(1)
            DIMENSION ITB(21), ITN(20), IE1(4), IE2(4), IS(4), IV(4), 
     1                L(4)

            EQUIVALENCE (AN,N)
            !SAVE CASENO, CASNUM, REFNO, REFNUM, REFRUN

C           DATA ITB/'0','1','2','3','4','5','6','7','8','9','-',
C    1               'J','K','L','M','N','O','P','Q','R',' '/
            DATA ITB/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H ,
     1               1HJ,1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR,1H /
            DATA ITN/ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
     1               -0,-1,-2,-3,-4,-5,-6,-7,-8,-9/
            DATA IX/1HX /

C   10       FORMAT (I1, 4(I5, A1, I8, 2A1), 3X, I2, I3)
   20       FORMAT ('ER', I2, 5X I1, 4(I5, A1, I8, 2A1), 3X, I2, I3)
   30       FORMAT (A1, 'REFERENCE RUN NO.', I3, 4X, 'CASE NO.', I4)
   40       FORMAT ('NO MORE INPUT1 DATA...JOB TERMINATED BY INPUT1.')

            ENTRY1 = IENTRY
            NER1 = .FALSE.
            NER2 = .FALSE.
            NER3 = .FALSE.
            REFRUN = .FALSE.   ! PSV
            CASNUM = 0  ! PSV
            CASENO = 0  ! PSV
            REFNO = 0  ! PSV
            REFNUM = 0  ! PSV
            write(7,*) 'Start'
C     SET PRINT FOR PAGE EJECT, CHECK NUMBER OF ARGUMENTS
            KKKK = 2

            IF (KP .EQ. 0) GO TO 50

C     PAGE EJECT NOT WANTED. SET PRINT FOR SINGLE SPACE, CHECK KP
            KKKK = 21
            IF (KP .NE. 1) KKKK = 50

C     TEST ENTRY FLAG
   50       IF (ENTRY1 .EQ. -1) GO TO 470
            write(7,*) 'Aft 50'
            NER = 0
            IF (ENTRY1 .EQ. 2) GO TO 60
            IF (ENTRY1 .NE. 0) GO TO 200
   60       REFRUN = .FALSE.
            write(7,*) 'Aft 60'

            IF (NCASE .GT. 0) REFRUN = .TRUE.
            NLL=IABS(NCASE)
            IF (ENTRY1 .EQ. 2) GO TO 190

   70       read(5,'(a80)') line
            if (line(1:1)=='I') then
               read(line(2:),*) loc,N,REFNO, CASENO
            else
               read(line(2:),*) loc,AN,REFNO, CASENO
            end if
            !write(10,*) 'Line: ',line
            !write(10,*) loc,N,AN,REFNO, CASENO
!            READ (5,10) IC1, (L(I), IS(I), IV(I), IE1(I), IE2(I),
!     1                  I=1, 4), REFNO, CASENO
!            WRITE(7,*) IC1, (L(I), IS(I), IV(I), IE1(I), IE2(I),
!     1                  I=1, 4), REFNO, CASENO
            write(7,*) 'Aft 70 LOC',LOC
            IF ((REFNO .EQ. 99) .AND. (CASENO .EQ. 999)) NER3 = .TRUE.
            IF (ENTRY1 .EQ. 0) GO TO 190
            IF (CASNUM .NE. 0) GO TO 80
            IF (CASENO .NE. 0) GO TO 140
            IF ( REFNUM .NE. REFNO) GO TO 460
            GO TO 110

C     TEST FOR END OF CURRENT CASE
   80       write(7,*) '80 CASNUM,CASENO',CASNUM,CASENO
            IF (CASNUM .EQ. CASENO) GO TO 170
            IF (NER3) ENTRY1 = -1

C     CHECK FOR PAST ERRORS

            IF (NER1 .OR. NER2) GO TO 100

C     PRINT (IF KP = 0 OR 1) OUTPUT TITLE AND EXIT FROM SUBROUTINE

            IF (KKKK .GT. 40) GO TO 90
            write(7,*) 'bef 90'
            WRITE (6,30) ITB(KKKK), REFNUM, CASNUM
   90       IREF = REFNUM
            ICAS = CASNUM
            IENTRY = ENTRY1
            RETURN

C     RESET ERROR FLAG1 AND TEST FRO RESETTING ERROR FLAG2

  100       IF (NER2 .AND. (REFNO - REFNUM) .NE. 0) NER2 = .FALSE.
            NER1 = .FALSE.
            write(7,*) 'Aft 100'
            GO TO 50

C     TEST FOR REFERENCE RUN DATA AND ARRAY

  110       IF (REFNO .EQ. 0) GO TO 120
            write(7,*) 'Aft 110'
            IF (.NOT. REFRUN) GO TO 440
            GO TO 150

C     SET CASE ARRAY TO ZERO (NO REFERENCE RUN DATA)

  120       DO 130 I = 1, NLL
  130       CASE(I) = 0.0
            write(7,*) 'Aft 120'
            REFNUM = 0.0
            GO TO 180

C     MOVE REFERENCE RUN ARRAY INTO CASE ARRAY

  140       write(7,*) '140'
            CASNUM = CASENO
  150       DO 160 I = 1, NLL
  160       CASE(I) = RRAREA(I)

  170       write(7,*) '170 REFNUM, REFNO', REFNUM, REFNO
            IF (REFNUM .NE. REFNO) GO TO 440
  180       CASNUM = CASENO
            write(7,*) 'Aft 180'
            GO TO 220

C     INITIAL ENTRY (ENTRY1 = 0 OR 2)

  190       write(7,*) '190'
            ENTRY1 = 1
            REFNUM = -1
            CASNUM = -1

C     TEST REFRENCE RUN AND CASE NUMBERS (NORMAL REENTRY)

  200       IF (CASENO .NE. 0) GO TO 110
            write(7,*) 'Aft 200'
            IF (REFNO .EQ. 0) GO TO 450
            IF ( .NOT. REFRUN) GO TO 440

C     SET UP REFERENCE RUN AND CASE NUMBER OF NEW REFERENCE RUN

            REFNUM = REFNO
            CASNUM = 0
C     CHECK FOR OVERLAY FLAN (AND IF NOT, SET REFERENCE RUN ARRAY = 0)

            IF (ENTRY1 .EQ. 3) GO TO 220
            DO 210 I = 1,NLL
  210       RRAREA(I) = 0.0
            write(7,*) 'Aft 210'

C     RESET ENTRY FLAG AND TEST COLUMN ONE OF DATA CARD

  220       ENTRY1 = 1
            write(7,*) 'Aft 220'

C     CONVERT, CHECK, AND (IF CORRECT) STORE 4 ETS OF DATA FIELDS
               J = LOC

C     TEST LOCATION FOR VALID RANGE
               IF (J .LE. 0) GO TO 370
               IF (J .GT. NLL) GO TO 360


C     STORE ANSWER IN LOATION J OF REGERENCE RUN OR CASE ARRAYS
  330          write(7,*)'I,J,CASENO,AN ',I,J,CASENO, AN
99330          IF (CASENO .EQ. 0)  GO TO 340
               CASE(J) = AN
               GO TO 350
  340          RRAREA(J) = AN
            write(7,*) 'Aft 340'
  350       CONTINUE
            GO TO 70

C     SET ERROCODE
  360       NER = NER + 2
  370       NER = NER + 2
  380       NER = NER + 2
  390       NER = NER + 1
  400       NER = NER + 1
  410       NER = NER + 1
  420       NER = NER + 1
  430       NER = NER + 2
  440       NER = NER + 1
  450       NER = NER + 1
  460       NER = NER + 1
            NER1 = .TRUE.
            IF (REFRUN .AND. CASENO .EQ. 0) NER2 = .TRUE.

C     WRITE ERROR MESSAGES
            IF (NER3) GO TO 470
            WRITE (6,20) NER, IC1, (LOC, ' ', N, ' ',' ',
     1                   I = 1, 4), REFNO, CASENO
            NER = 0
            GO TO 70

C     TERMINATE WHEN ALL DATA HAS BEEN READ

  470       WRITE (6,40)
            RETURN
         END


         SUBROUTINE SPECSM
            COMMON X, Y
            COMMON CSUM, CYCLSM, MAXN, DY, STSMXM, STSMNM
            DIMENSION X(3958), Y(3958)
            DIMENSION CSUM(40,25), CYCLSM(40,25), N2(40), MAXN(40), 
     1                M3(40), DY(40,25), N(40), STSMXM(40,25), 
     2                STSMNM(40,25)
            EQUIVALENCE (IEND, X(1)), (IRR, X(201)), (ICASE, X(202))
            EQUIVALENCE (M3, X(1673)), (N, X(1793)), (N2, X(3558))

            I = 1
            L = 0
            L5 = 0
            M = 1
            M9 = 0
            M10 = 0
   20       K = 0
            IF (N2(I) .EQ. 0) GO TO 130
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
               IF ((M3(I) .GT. 9) .AND. (M3(I) .LT. 13)) 
     1            CSUM(L,K) = CYCLSM(I,J)
            END DO

            DO WHILE (I .LE. IEND)
               M = M + 1
               M10 = 0
               IF (N2(I).EQ.0 .OR. M.GT.IEND) GO TO 130
               IF (N2(I) .NE. N2(M)) CYCLE
               IF (I .EQ. M) GOTO 20
               IF (I .GT. M) GOTO 130
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
                  IF ((M3(I).GT.9).AND.(M3(I).LT.13))
     1               CSUM(L,K) = CSUM(L,K) + CYCLSM(M,J)
               END DO
               CYCLE
               
  130          I = I + 1
               M = 0
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
   10    FORMAT ('REFERENCE RUN NO.', I6, 4X, 'CASE NO.', I6)
  140    FORMAT ('THE FOLLOWING DATA IS THE SUMMATION OF THE' 
     1   'SPECTRA FOR THE SEGMENTS SPECIFIED BY FLAG L = N2')
  150    FORMAT (3X, 'FL', 3X, 'LL', 5X, 'MAX STRESS', 5X,
     1   'MIN STRESS', 12X, 'CYCLES')
  180    FORMAT (3X, 'FL', 3X, 'LL', 5X, 'DELTA Y', 5X,
     1   'CUMMULATIVE CYCLES', 10X, 'CYCLES')
  210    FORMAT (2X, I2, 3X, I2, 2X, F13.3, 3X, F16.4)
  230    FORMAT (1H0, 2X, I2, 3X, I2, 4X, F11.0, 4X, F11.0, 7X,
     1           F16.4)
  250    FORMAT (46X, F16.4)
         END

         SUBROUTINE PRINT
         COMMON X, Y
         DIMENSION X(3958), Y(3958), SIG(40), SCLTRB(40), F(40)
         DIMENSION YAW(40), CYBT(40), CYBT0(40)
         DIMENSION M3(40), M5(40), ISTRES(40), IA(40), N1FLAG(40),
     1   P(40), N6(40), N2(40), AM(40), DELY1(40), DELY11(40), 
     2   ARNO1(40), ARNO2(40), ARNO3(40),	SGMAX1(40), SGMAX2(40), 
     3   SGMAX3(40), AKSIG(40), SLOPE(40), VELOS(40), WT(40), P1(40),
     4   P2(40), AK1(40), AK2(40), ABR(40), N(40), TBLM2(434), 
     5   TBLI2(1542)
         DIMENSION DELT1(25), DELT2(25), DELT3(25),DELT4(25), 
     1   DELT5(25), DELT6(25), TAB1(25), TAB2(25), TAB3(25), 
     2   TAB4(25), TAB5(25), TAB6(25),	TABL1(25), TABL2(25), 
     3   TABL3(25), TABL4(25), TABL5(25), TABL6(25), TABL7(25),
     4   TABL8(25), TABL9(25), T(40)
         EQUIVALENCE (IEND, X(1)), (KEND, X(2)), (I4, X(3)),
     1   (SIGULT, X(4)), (WAREA, X(5)), (ISTRES, X(6)), 
     2   (DELT1, X(46)), (TAB1, X(71)), (TAB2, X(96)), 
     3   (TAB3, X(121)),  (TAB4, X(146)), (TAB5, X(171)), 
     4   (AC, X(196)), (IW1, X(197)), (IW2, X(198)), (IW3, X(199)),
     5   (IW4, X(200)),	(IRR, X(201)), (ICASE, X(202)), 
     6   (IW5,	X(203)), (TAB6,	X(206)), (DELT2, X(231)), 
     7   (DELY1, X(256)), (DELY11, X(296)), (ARNO1, X(336)), 
     8   (ARNO2, X(376)), (ARNO3, X(416)), (SGMAX1, X(456)), 
     9   (SGMAX2, X(496)), (SGMAX3, X(536)), (T, X(576)), 
     A   (AKSIG, X(616)), (SLOPE, X(656)), (VELOS, X(696)), 
     B   (WT, X(736)), (P1, X(776)), (TBLM2, X(853))
         EQUIVALENCE (P2, X(1473)), (AK1, X(1513)), (AK2, X(1553)),
     l   (ABR, X(1593)), (IA,	X(1633)), (M3, X(1673)), 
     2   (SIG, X(1713)), (AM,	X(1753)), (N, X(1793)), 
     3   (NEND, X(1833)), (AL6, X(1850)), (AL5,  X(1851)), 
     4   (AL4, X(1852)), (AL3, X(1853)), (AL2, X(1854)), 
     5   (AL1, X(1855)), (TBLI2, X(1856)), (M5, X(3398)), 
     6   (N1FLAG, X(3438)), (F, X(3478)), (N6, X(3518)), 
     7   (N2, X(3558)),	(P, X(3598)), (SCLTRB, X(3638)), 
     8   (TABL1, X(3678)), (TABL2, X(3703)), (TABL3, X(3728)), 
     9   (TABL4, X(3753)), (TABL5, X(3778)), (TABL6, X(3803)), 
     A   (TABL7,	X(1287)), (TABL8, X(1312)), (TABL9, X(1337)),	
     B   (DELT3,	X(1362)), (DELT4, X(1387))
         EQUIVALENCE (DELT5, X(1412)), (DELT6, X(1437)),
     1               (L1, X(1462))
         EQUIVALENCE (CBART, X(3837)), (AST, X(3838)), 
     1               (YAW, X(3839)), (CYBT, X(3879)), 
     2               (CYBT0, X(3919))
   10    FORMAT ('REFERNCE RUN NO. ', I6, 4X, 'CASE NO. ', I6)
   20    FORMAT (4X, 'IEND', 1X,'KEND', 1X, 'I4', 4X, 'S-ULT.', 3X,
     1   '---S---',3X,'C-BAR-',3X,'---NEND---',2X,'L1', 2X, 'IW1',
     2   2X, 'IW2', 2X, 'IW3', 2X, 'IW4', 2X, 'IW5')
   25    FORMAT (4X, 'IEND', 1X, 'KEND', 1X, 'I4', 4X, 'S-ULT.', 3X, 
     1            '---S---', 3X, 'C-BAR-', 3X, '---NEND---', 3X, 'L1',
     2            3X, 'IW1', 2X, 'IW2', 2X, 'IW3', 2X, 'IW4', 2X,
     3            'IW5', 2X, 'V.T.CHORD', 'V.T.AREA.') 
   30    FORMAT (5X, I2, 3X, I2, 2X, I2, F12.3, F8.2, 2X, F7.2,
     1           3X, F10.0, 3X, I1, 3X, I1, 4X, I1, 4X, I1, 4X, 
     2           I1, 4X, I1)
   35    FORMAT(5X, I2, 3X, I2, 2X, I2, F12.3, F8.2, 2X, F7.2,
     l          3X, F10.0, 3X, I1, 3X, I1, 4X, I1, 4X, I1, 4X, I1, 
     2          4X, I1, 3X, F8.2, 2X, F8.2)
   40    FORMAT(2X, 'SEG.', 1X, 'M3', 2X, 'M5', 2X, 'ISTRES', 2X, 
     1   'IA', 2X, 'IFLAG', 1X, 'H------P------', 2X, 'N6', 2X, 'N2',
     2    2X, '-----AM-----', 3X, '--------F--------', 10X, 
     3   'DELTA Y1', 10X, 'DELTA Y11')
   50    FORMAT (3X, I2, 2X, I2, 2X, I2, 4X, I2, 4X, I2, 4X, I1, 4X,
     1           F13.0, 2X, I2, 2X, I2, 2X, F13.0, 3X, F17.4, 3X, 
     2           F17.4, 3X, F17.4)
   60    FORMAT ('SEG.', 1X, 'H---N SUB 01---', 2X '---N SUB 02---',
     1   2X, '---N SUB 03---', 2X, '--SIG DY1--', 2X, '--SIG DY2--', 
     2   2X, '--SIG DY3--')
   70    FORMAT (1X, I2, 2X, F14.4, 2X, F14.4, 2X, F14.4, 2X, F11.4,
     1           2X, F11.4, 2X, F11.4)
   80    FORMAT (2X, 'SEG.', 3X, 'KSIGMA', 3X, 'SLOPE', 5X, 'VE', 4X,
     1   '-----W----', 6X, 'P1', 10X, 'P2', 9X, 'B1', 7X, 'B2', 10X,
     2   'A-BAR', 5X, '-------T------', 4X, 'N')
   90    FORMAT (3X, I2, 3X, F8.5, 1X, F6.2, 2X, F7.2, 2X, F10.0, 2X,
     1           F10.6, 2X, F10.6, 2X, F7.3, 2X, F7.3, F16.6, 2X, 
     2           F14.3, 3X, I2)
  100    FORMAT (3X, 'SEG.', 4X, 'AIR DENSITY RATIO', 5X,
     1   'SCALE OF TURBULENCE')
  105    FORMAT (3X, 'SEG.', 4X, 'AIR DENSITY RATIO', 5X,
     1   'SCALE OF TURBULENCE', 11X, 'SFC - T*', 'SFC - TO', 5X,
     2   '    IYAW   ')
  110    FORMAT (4X, I2, 9X, F9.5, 14X, F10.3)
  115    FORMAT (4X, I2, 9X,F 9.5,14X,F10.3,14X,F8.4,6X,F8.4,8X,
     1           F16.0)
  120    FORMAT (4X, 'LL', 4X, 'STRESS TBL1', 2X, 'STRESS TBL2', 2X,
     1   'STRESS TBL3', 2X, 'STRESS TBL4', 2X, 'STRESS TBL5', 2X,
     2   'STRESS TBL6', 2X, 'STRESS TBL7', 2X, 'STRESS TBL8')
  130    FORMAT (1H0)
  140    FORMAT (4X, I2, 3X, F13.2, 1X, F13.2, F13.2, F13.2 F13.2,
     1           F13.2, F13.2, F13.2)
  150    FORMAT (4X, I2, 3X, F13.2, 1X, F13.2, F13.2, F13.2, F13.2,
     1           F13.2)
  160    FORMAT (4X, 'LL', 4X, 'DELTA Y--TABLE 1', 2X,
     1   'DELTA Y--TABLE 2', 2X, 'DELTA Y--TABLE 3', 2X,
     2   'DELTA Y--TABLE 4', 2X, 'DELTA Y--TABLE 5', 2X,
     3   'DELTA Y--TABLE 6')
  170    FORMAT (4X, I2, F18.3, F18.3, F18.3, F18.3, F18.3, F18.3)
  180    FORMAT (4X, 'LL', 4X, 'CUM CYCLES TBL 1', 2X
     1   'CUM CYCLES TBL 2', 2X, 'CUM CYCLES TBL 3', 2X
     2   'CUM CYCLES TBL 4', 2X, 'CUM CYCLES TBL 5', 2X,
     3   'CUM CYCLES TBL 6')
  190    FORMAT (4X I2, F19.3, F18.3, F18.3, F18.3, F18.3, F18.3)
  200    FORMAT (4X, 'LL', 4X, 'STRESS TBL 9', 2X, 'STRESS TBL 10',
     1   1X, 'STRESS TBL 11', 1X, 'STRESS TBL 12', 1X, 
     2   'STRESS TBL 13', 1X, 'STRESS TBL 14', 1X, 'STRESS TBL 15', 
     3   1X, 'STRESS TBL 16')
  210    FORMAT (4X, 'LL', 4X, 'MAX STRESS(1)', 2X, 'MIN STRESS(1)',
     1   6X, 'CYCLES(1)', 5X, 'MAX STRESS(2)', 2X, 'MIN STRESS(2)', 
     2   6X, 'CYCLES(2)')
  220    FORMAT (4X, I2, F17.0, F15.0, F18.0, F15.0, F15.0, F18.0)
  230    FORMAT (4X, 'LL', 4X, 'MAX STRESS(3)', 2X, 
     1   'MIN STRESS(3)', 6X,'CYCLES(3)')
  240    FORMAT (4X, I2, F17.0, F15.0, F18.0)
  250    FORMAT (2X, 'S-N TABLE = ',I2, 2X, 'IA AND/OR I4 = ',I2)
  260    FORMAT (4X, 'NO. OF Y ENTRIES = ',F4.0, 4X,
     1    'NO. OF X ENTRIES = ',F4.0, 2X, 'MAX CYCLES TO FAILURE = ',
     2     F12.0)
  270    FORMAT (7X, 'Y', 12X, 'X', 10X, 'Y1,X', 9X, 'Y2,X', 9X,
     1   'Y3,X', 9X, 'Y4,X', 9X, 'Y5,X', 9X, 'Y6,X', 9X, 'Y7,X')
  280    FORMAT (1X,F12.3, F14.4, F13.0, F13.0, F13.0,
     1   F13.0, F13.0, F13.0, F13.0)
  290    FORMAT (8X, 'Y8,X', 10X, 'Y9,X', 9X, 'Y10,0', 9X, 'Y11,X',
     1   9X, 'Y12,X', 9X, 'Y13,X', 9X, 'Y14,X', 9X, 'Y15,X')
  300    FORMAT (4X, F12.0, 2X, F12.0, 2X, F12.0, 2X, F12.0, 2X, 
     1   F12.0, 2X, F12.0, 2X, F12.0, 2X, F12.0)
  310    FORMAT ('THE FOLLOWING DATA IS INPUT DATA')
  320    FORMAT ('STRESS TABLES, S = X = F(Y). LL 1 = NO. OF Y '
     1   'ENTRIES LL 2 - 16 = Y VALUES, LL 17-31 = X VALUES')

         WRITE (6,310)

         IF (ALL(M3(1:IEND).GE.14) .AND. ALL(M3(1:IEND).LE.15)) THEN
            WRITE (6,25)
            WRITE (6,35) IEND,KEND,I4,SIGULT, WAREA, AC,FLOAT(NEND), 
     1      L1, IW1, IW2, IW3, IW4, IW5, CBART, AST
         ELSE
            WRITE (6,20)
            WRITE (6,30) IEND, KEND, I4,SIGULT,WAREA,AC,float(NEND),
     1                   L1, IW1, IW2, IW3, IW4, IW5
         END IF

         WRITE (6,40)
         WRITE (6,50) (I, M3(I), M5(I), ISTRES(I), IA(I), N1FLAG(I), 
     1                 P(I), N6(I), N2(I), AM(I), F(I), DELY1(I), 
     2                 DELY11(I), I = 1, IEND)
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,60)
         WRITE (6,70) (I, ARNO1(I), ARNO2(I), ARNO3(I), SGMAX1(I),
     1                 SGMAX2(I), SGMAX3(I), I = 1, IEND)
         WRITE (6,10) IRR, ICASE
         WRITE (6,310)
         WRITE (6,80)
         WRITE (6,90) (I, AKSIG(I), SLOPE(I), VELOS(I), WT(I), P1(I), 
     1                P2(I), AK1(I), AK2(I), ABR(I), T(I), N(I), 
     2                I = 1, IEND)

         IF (ANY(M3(1:IEND).GE.13 .AND. M3(1:IEND).LE.15)) THEN
            WRITE (6,10) IRR, ICASE
            WRITE (6,310)
            IF(ALL(M3(1:IEND).GE.14).AND.ALL(M3(1:IEND).LE.15)) THEN
               WRITE (6,105)
               WRITE (6,115) (I, SIG(I), SCLTRB(I),CYBT(I),CYBT0(I),
     1         YAW(I), I = 1, IEND)
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
            WRITE (6,170)(J, DELT1(J), DELT2(J), DELT3(J), DELT4(J),
     1      DELT5(J), DELT6(J), J = 1, 25)
         END IF

         IF (ANY(M3(1:IEND).GT.0 .AND. M3(1:IEND).LT.7)) THEN
            WRITE (6,10) IRR, ICASE
            WRITE (6,310)
            WRITE (6,130)
            WRITE (6,180)
            WRITE (6,190) (J,TAB1(J),TAB2(J),TAB3(J),TAB4(J),TAB5(J),
     1      TAB6(J), J = 1, 25)
         END IF

         IF (ANY(M3(1:IEND).GT.9 .AND. M3(1:IEND).LT.12)) THEN
            WRITE (6,10) IRR, ICASE
            WRITE (6,310)
            WRITE (6,130)
            WRITE (6,210)
            WRITE (6,220)(J, TABL1(J), TABL2(J), TABL3(J), TABL4(J),
     1      TABL5(J), TABL6(J), J = 1, 25)
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
            WRITE (6,140) (J, TBLM2(J), TBLM2(J+31), TBLM2(J+62), 
     1                     TBLM2(J+93), TBLM2(J+124), TBLM2(J+155), 
     2                     TBLM2(J+186), TBLM2(J+217), J = 1, 31)
         END IF

         IF(ANY(ISTRES(1:IEND).GT.8 .AND. ISTRES(1:IEND).LT.15))THEN
            WRITE (6,10) IRR, ICASE
            WRITE (6,310)
            WRITE (6,130)
            WRITE (6,320)
            WRITE (6,200)
            WRITE (6,140) (J, TBLM2(J), TBLM2(J+248), TBLM2(J+279), 
     1                     TBLM2(J+310), TBLM2(J+341), TBLM2(J+372), 
     2                     TBLM2(J+403), J = 1, 31)
         END IF

         A5 = 0.0
         B5 = 0.0
         C5 = 0.0
         D5 = 0.0
         E5 = 0.0
         F5 = 0.0
         G5 = 0.0
         DO WHILE(.TRUE.)
            DO I = 1, IEND
               IF ( IA(I).LT.7) I2 = IA(I)
               IF ((IA(I).GT.6) .AND. (IA(I).LT.13)) I2 = IA(I)-6
               IF ((IA(I).GT.12) .AND. (IA(I).LT.19)) I2 = IA(I)-12
               IF ( IA(I).GT.18) I2 = IA(I) -18
               ICALL = I2
               SELECT CASE (ICALL)
                  CASE (:1,7:)
                     IF (A5 .NE. 1.0) THEN
                       WRITE (6,10)  IRR, ICASE
                       WRITE (6,310)
                       WRITE (6,250) ICALL, IA(I)
                       WRITE (6,260) TBLI2(1), TBLI2(17), AL1
                       WRITE (6,270)
                       WRITE (6,280)(TBLI2(J),TBLI2(J+16),TBLI2(J+31),
     1                 TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),
     2                 TBLI2(J+106), TBLI2(J+121), J = 2, 16)
                       WRITE (6,290)
                       WRITE (6,300)(TBLI2(J+136),TBLI2(J+151),
     1                 TBLI2(J+166),TBLI2(J+181), TBLI2(J+196),
     2                 TBLI2(J+211),TBLI2(J+226),TBLI2(J+241), J=2,16)
                       A5 = 1.0
                     END IF
                  CASE (2)
                     IF (B5 .NE. 1.0) THEN
                       WRITE (6,10) IRR, ICASE
                       WRITE (6,310)
                       WRITE (6,250) ICALL, IA(I)
                       WRITE (6,260) TBLI2(258), TBLI2(274), AL2
                       WRITE (6,270)
                       WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),
     1                 TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),
     2                 TBLI2(J+106), TBLI2(J+121), J = 259, 273)
                       WRITE (6,290)
                       WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),
     1                 TBLI2(J+166),TBLI2(J+181), TBLI2(J+196),
     2                 TBLI2(J+211),TBLI2(J+226),
     3                 TBLI2(J+241),J=259,273)
                       B5 = 1.0
                     END IF
                  CASE (3)
                     IF (C5 .NE. 1.0) THEN
                       WRITE (6,10) IRR, ICASE
                       WRITE (6,310)
                       WRITE (6,250) ICALL, IA(I)
                       WRITE (6,260) TBLI2(515), TBLI2(531), AL3
                       WRITE (6,270)
                       WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),
     1                 TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),
     2                 TBLI2(J+106), TBLI2(J+121), J = 516, 530)
                       WRITE (6,290)
                       WRITE (6,300)(TBLI2(J+136), TBLI2(J+151),
     1                 TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),
     2                 TBLI2(J+211), TBLI2(J+226),
     3                 TBLI2(J+241), J = 516, 530)
                       C5 = 1.0
                     END IF
                  CASE (4)
                     IF (D5 .NE. 1.0) THEN
                       WRITE (6,10) IRR, ICASE
                       WRITE (6,310)
                       WRITE (6,250) ICALL, IA(I)
                       WRITE (6,260) TBLI2(772), TBLI2(788), AL4
                       WRITE (6,270)
                       WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),
     1                 TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),
     2                 TBLI2(J+106), TBLI2(J+121), J = 773, 787)
                       WRITE (6,290)
                       WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),
     1                 TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),
     2                 TBLI2(J+211), TBLI2(J+226),
     3                 TBLI2(J+241), J = 773, 787)
                       D5 = 1.0
                     END IF
                  CASE (5)
                     IF (E5 .NE. 1.0) THEN
                       WRITE (6,10) IRR, ICASE
                       WRITE (6,310)
                       WRITE (6,250) ICALL, IA(I)
                       WRITE (6,260) TBLI2(1029), TBLI2(1045), AL6
                       WRITE (6,270)
                       WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),
     1                 TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),
     2                 TBLI2(J+106), TBLI2(J+121), J = 1030, 1044)
                       WRITE (6,290)
                       WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),
     1                 TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),
     2                 TBLI2(J+211), TBLI2(J+226),
     3                 TBLI2(J+241), J = 1030, 1044)
                       E5 = 1.0
                     END IF
                  CASE (6)
                     IF (F5 .NE. 1.0) THEN
                       WRITE (6,10) IRR, ICASE
                       WRITE (6,310)
                       WRITE (6,250) ICALL, IA(I)
                       WRITE (6,260) TBLI2(1286), TBLI2(1302), AL2
                       WRITE (6,270)
                       WRITE (6,280) (TBLI2(J),TBLI2(J+16),TBLI2(J+31),
     1                 TBLI2(J+46),TBLI2(J+61),TBLI2(J+76),TBLI2(J+91),
     2                 TBLI2(J+106), TBLI2(J+121), J = 1287, 1301)
                       WRITE (6,290)
                       WRITE (6,300)  (TBLI2(J+136), TBLI2(J+151),
     1                 TBLI2(J+166), TBLI2(J+181), TBLI2(J+196),
     2                 TBLI2(J+211), TBLI2(J+226),
     3                 TBLI2(J+241), J = 1287, 1301)
                       F5 = 1.0
                     END IF
                  END SELECT
               IF (G5. EQ. 1.0) RETURN
            END DO
            IF (I4 .EQ. 0) RETURN
            IA(I) = I4
            G5 = 1.0
         END DO
         RETURN
         END

        SUBROUTINE ONEVAR (ARGUMT, TABLE, OUTPUT, NSEGNM)
C        ONEVAR IS A INTERPOLATION ROUTINE - ONE FUNCTION OF ONE
C        VARIABLE, I.E. X=F(Y) - LINEAR OR QUADRATIC.
C        ARGUMEENTS OF THE SUBROUTINE ARE AS FOLLOWS:
C          ARGUMT  = INPUT INTERPOLATION ARGUMENT (Y)
C          NXDIR   = TYPE OF INTEROLATION, 1 FOR LINEAR, 2 FOR QUAD.
C          TABLE   = SET OF Y VALUES FOLLOWED BY THE X VALUES
C          OUTPUT  = INTERPOLATED VALUE OF X = F(Y)
C          NER     = ERROR CODE
C                    1 = OK, INTERPOLATION SUCCESSFUL.
C                    2 = OFF CHART LOW END, MIN. VAL. SUBSTITUTED
C                    3 = OFF CHART HIGH, MAX. VAL. SUBSTITUTED
C                    4 = NO. OF X ENTRIES IS NOT 2 TO 15 (IF NXDIR
C                        IS 1). OR, IT IS NOT 3 TO 15 (IF NXDIR) IS 2).
C                    5 = Y ENTRIES NOT IN ASCENDING ORDER.
         IMPLICIT NONE
         REAL TABLE(31), ARGUMT, OUTPUT
         INTEGER NSEGNM, NOENTR, NXDIR, NER, I
         NOENTR = TABLE(1) + 0.5
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
         I=MINLOC(TABLE(2:NOENTR+1),1,
     1     MASK=(TABLE(2:NOENTR+1).GT.ARGUMT))
         OUTPUT = TABLE(I+15) + (ARGUMT - TABLE(I)) * (TABLE
     1   (I+16) - TABLE(I+15))/(TABLE(I+1) - TABLE(I))
         RETURN
  280    FORMAT ('ONEVAR INTEROLATION ERROR. Y IS TOO SMALL.',
     1           ' Y = ',E14.6)
  310    FORMAT ('ONEVAR INTEROLATION ERROR. Y IS TOO LARGE.',
     1           ' Y = ',E14.6)
  330    FORMAT ('ONEVAR INTERP. ERROR. THE NO. OF Y ENTRIES IS',
     1           ' EITHER TOO SMALL OR TOO LARGE.')
  350    FORMAT ('ONEVAR INTERP. ERROR. THE NO. OF Y ENTRIES ARE', 
     1           ' NOT IN ASCENDING ORDER.')
         END



      SUBROUTINE TWOVIN (XARG, YARG, TABLE, OUTPUT, NSEG, LEVEL)

C     ARUGMENTS OF THE SUBROUTINE ARE AS FOLLOWS:
C     XARG   = INPUT INTERPOLATION ARGUMENT (X)
C     YARG   = INPUT INTERPOLATION ARGUMENT (Y)
C     TABLE  = SET OF VALUES. SEE DESRIPTION OF THIS
C     OUTPUT = INTERPOLATED VALUE OF Z = F(X,Y)
         DIMENSION TABLE(257)
   10    FORMAT ('SN INTERP. ERROR. Y IS TOO SMALL. Y = ',E14.6,2X,
     1   'SEG = ', I3, 2X, 'LOAD LEVEL =', I3)
   20    FORMAT ('SN INTERP. ERROR. Y IS TOO LARGE. Y = ',E14.6,2X,
     1   'SEG = ', I3, 2X, 'LOAD LEVEL = ', I3)
   30    FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,
     1   'SEG = ', I3, 2X, 'LOAD LEVEL = ', I3, 2X)
   40    FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,
     1   'Y IS TOO SMALL. Y = ',E14.6, 2X, 'SEG = ', I3, 2X,
     2   'LOAD LEVEL = ', I3)
   50    FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,
     1   'Y IS TOO LARGE. Y = ',E14.6, 2X, 'SEG = ', I3, 2X,
     2   'LOAD LEVEL = ', I3)
   60    FORMAT ('SN INTERP. ERROR. Y IS TOO SMALL. Y = ',E14.6,2X,
     1   'X = ',E14.6, 2X, '(GAG SEGMENT)')
   70    FORMAT ('SN INTERP. ERROR. Y IS TOO LARGE. Y = ',E14.6,2X,
     1   'X = ',E14.6, 2X, '(GAG SEGMENT)')
   80    FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,
     1   'Y = ',E14.6, 2X, '(GAG SEGMENT)')
   90    FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,
     1   'Y = ',E14.6, 2X, '(GAG SEGMENT)')
  100    FORMAT ('SN INTERP. ERROR. X IS TOO LARGE. X = ',E14.6,2X,
     1   'Y IS TOO LARGE. Y = ',E14.6, 2X, '(GAG SEGMENT)')
  110    FORMAT ('GUST ALLEV. INTRP. ERROR. Y IS TOO SMALL',
     1   'Y = ',E14.6, 2X, 'SEG =', I3)
  120    FORMAT ('GUST ALLEV. INTRP. ERROR. Y IS TOO LARGE',
     1   'Y = ',E14.6, 2X, 'SEG =', I3)
  130    FORMAT ('GUST ALLEV. INTRP. ERROR. X IS TOO LARGE',
     1   'X = ',E14.6, 2X, 'SEG =', I3)
  140    FORMAT ('GUST ALLEV. INTRP. ERROR. X IS TOO LARGE',
     1   'X = ',E14.6, 2X,'Y IS TOO SMALL. Y = ',E14.6,1X,'SEG =', 
     2   I3)
  150    FORMAT ('GUST ALLEV. INTRP. ERROR. X IS TOO LARGE',
     1   'X = ',E14.6,2X,'Y IS TOO LARGE. Y = ',E14.6,1X,'SEG =', 
     2   I3)
         write(10,*) 'tw: ',XARG, YARG, TABLE, OUTPUT, NSEG, LEVEL
         J5 = 0
         K = 0
         NER = 0
         NER2 = 0
         NER5 = 0
         NXDIR = 2
         NXENTR = TABLE(17) + 0.5
         NYENTR = TABLE(1) + 0.5
  160    DO 300 JK = 1,NYENTR
            IY = JK
            IF (IY -1) 230,230,170
  170       IF (IY - NYENTR) 180,200,200
  180       IF (TABLE(IY+1) - TABLE(IY)) 190,190,260
  190       NER = 9
            GO TO 740
  200       IF (TABLE(IY+1) - YARG) 210,220,310
  210       NER = 3
            NER2 = 13
  220       J5 = 1
            IY = IY + 1
            GO TO 310
  230       IF (TABLE(IY + 1) - YARG) 270,250,240
  240       NER = 2
            NER2 = 12
  250       IY = IY + 1
            J5 = 1
            GO TO 310
  260       IF (TABLE (IY + 1) - YARG) 280,310,310
  270       CYINT = ((YARG - TABLE(IY + 1)) / (TABLE(IY + 2) - 
     1                TABLE(IY + 1)))
            IF (CYINT - 0.001) 250,300,300
  280       CYINT = ((YARG - TABLE(IY + 1)) / (TABLE(IY +1) - 
     1                TABLE(IY)))
  290       IF (CYINT - 0.001) 310,300,300
  300    CONTINUE
  310    DO 460 JK = 1,NXENTR
            IX = JK
            IF (IX -1) 420,420,320
  320       IF (IX - NXENTR) 330,390,390
  330       IF (TABLE(IX + 17) - TABLE(IX + 16))340,340,450
  340       NER = 9
            GO TO 740
  350       NER = 5       ! corrected by PSV
            NER5 = 13     !
  360       IX = IX + 1
            IF (J5 .EQ. 1) GO TO 470
            GO TO 480
  370       CKINT = ABS((TABLE(IX+17) - XARG) / (TABLE(IX+18) - 
     1              TABLE(IX+17)))
  380       IF (CKINT - 0.50)480,480,460
  390       IF (TABLE(IX+17) - XARG)350,360,400
  400       CKINT = ABS((TABLE(IX+17) - XARG)/(TABLE(IX+17) - 
     1      TABLE(IX+16)))
            IF(CKINT - 0.50)410,410,350
  410       IX = IX - 1
            goto 480 !psv
  420       IF (TABLE(IX+17) - XARG) 460,460,430
  430       NER = 4
  440       IX = IX + 1
            IF (J5 .EQ. 1) GO TO 470
            GO TO 480
  450       IF (TABLE (IX+17) - XARG) 370,370,480
  460    CONTINUE
  470    IN = 15 * IY + 1 + IX
         OUTPUT = ALOG10(TABLE(IN))
         write(9,*) 'TWOVIN 470', OUTPUT
         GO TO 710
  480    IN = 15 * IY + 1 + IX
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
  490    AN6 = ALOG10(TABLE(IN + 16))
         GO TO 510
  500    AN4 = AN2
         AN5 = AN2
         AN6 = AN1
  510    IF (NXDIR - 1) 520,520,600
  520    IF (J5 - 1) 540,530,540
  530    BX = 0.0
         GO TO 550
  540    BX = ((YARG - TABLE(IY)) / (TABLE(IY + 1) - TABLE(IY)))
  550    IF (IX - NXENTR) 570,570,560
  560    OUTPUT = ALOG10(TABLE(IN)) + BX * (ALOG10(TABLE(IN + 15)) -
     1   ALOG10(TABLE(IN)))
         write(9,*) 'TWOVIN 560', OUTPUT
         GO TO 710
  570    XARGMX = TABLE(NXENTR + 17)
         IF (TABLE(IX + 17) - XARGMX) 590,580,580
  580    TABLE(IX + 18) = TABLE(IX + 17)
         TABLE(IN + 17) = TABLE(IN + 16)
  590    CX = XARG - (TABLE(IX + 16) + BX * (TABLE(IX + 17) -
     1   TABLE(IX + 16)))
         DX = ALOG10(TABLE(IN + 1)) + BX * (ALOG10(TABLE(IN + 17)) -
     1   ALOG10(TABLE(IN + 1)))
         EX = ALOG10(TABLE(IN)) + BX * (ALOG10(TABLE(IN + 16)) -
     1   ALOG10(TABLE(IN)))
         FX = TABLE(IX + 17) - TABLE(IX + 16) + BX * (TABLE(IX + 18) 
     1        - 2.0 * TABLE (IX + 17) + TABLE(IX + 16))
         IF (FX .EQ. 0.0) FX = 1.0
         OUTPUT = ALOG10(TABLE(IN)) + BX * (ALOG10(TABLE(IN + 16)) -
     1   ALOG10(TABLE(IN))) + (((CX) * (DX - EX)) / (FX))
         write(9,*) 'TWOVIN 590', OUTPUT
         GO TO 710
  600    IF (J5 - 1) 620,610,620
  610    BX = 0.0
         GO TO 630
  620    BX = ((YARG - TABLE(IY)) / (TABLE(IY + 1) - TABLE(IY)))
  630    IF (IX - NXENTR) 650,650,640
  640    AN7 = ALOG10(TABLE(IN + 15))
         OUTPUT = AN1 + BX * (AN7 - AN1)
         GO TO 710
  650    IBOUND = NXENTR - 1
         IF (IX .EQ. IBOUND) GO TO 670
         XARGMX = TABLE(NXENTR + 17)
         IF (TABLE(IX + 17) - XARGMX) 680,660,660
  660    TABLE(IX + 18) = TABLE(IX + 17)
         AN3 = AN2
         AN5 = AN2
  670    TABLE(IX + 19) = TABLE(IX + 17)
         IF (IX .EQ. IBOUND) TABLE(IX + 19) = TABLE(IX + 18)
         AN4 = AN5
  680    CX = TABLE (IX + 18) + BX * (TABLE(IX + 19) - 
     1        TABLE (IX + 18))
         DX = TABLE (IX + 17) + BX * (TABLE(IX + 18) - 
     1        TABLE (IX + 17))
         EX = TABLE (IX + 16) + BX * (TABLE(IX + 17) - 
     1        TABLE (IX + 16))
         FX = AN3 + BX * (AN4 - AN3)
         GX = AN2 + BX * (AN5 - AN2)
         HX = AN1 + BX * (AN6 - AN1)
         IF (( IX .GE. IBOUND) .AND. ((BX * 1.001) .GE. 1.0)) 
     1      GO TO 700
  690    OUTPUT = (HX * (((XARG - DX) * (XARG - CX)) / ((EX - DX) *
     1   (EX - CX)))) + (GX * (((XARG - EX) * (XARG - CX)) / 
     2   ((DX - EX) * (DX - CX)))) + (FX * (((XARG - EX) * 
     3   (XARG - DX)) / ((CX - EX) * (CX - DX))))
         GO TO 710
  700    OUTPUT = GX + (HX - GX) * ((DX - XARG) / (DX - EX))
  710    IF (NSEG .EQ. 50) GO TO 720
         IF (NSEG .GT. 50) GO TO 730
         IF (NER .EQ. 2) WRITE(6,10) YARG, NSEG, LEVEL
         IF (NER .EQ. 3) WRITE(6,20) YARG, NSEG, LEVEL
         IF ((NER .EQ. 5) .AND. (NER2 .EQ. 0)) WRITE(6,30) XARG, 
     1      NSEG, LEVEL
         IF ((NER2.EQ.12) .AND. (NER5.EQ.13)) WRITE(6,40)XARG,YARG,
     1      NSEG, LEVEL
         IF ((NER2.EQ.13) .AND. (NER5.EQ.13)) WRITE(6,50)XARG,YARG,
     1      NSEG, LEVEL
         GO TO 760
  720    IF (NER .EQ. 2) WRITE(6,60) YARG, XARG
         IF (NER .EQ. 3) WRITE(6,70) YARG, XARG
         IF ((NER .EQ. 5) .AND. (NER2 .EQ. 0)) WRITE(6,80) XARG, 
     1      NSEG, LEVEL
         IF ((NER2 .EQ. 12) .AND. (NER5 .EQ. 13)) WRITE(6,90) XARG, 
     1      YARG
         IF ((NER2 .EQ. 13) .AND. (NER5 .EQ. 13)) WRITE(6,100) XARG, 
     1      YARG
         GO TO 760
  730    NSEG = NSEG - 50
         IF (NER .EQ. 2) WRITE (6,110) YARG, NSEG
         IF (NER .EQ. 3) WRITE (6,120) YARG, NSEG
         IF ((NER .EQ. 5) .AND. (NER2 .EQ. 0)) WRITE(6,130) XARG, 
     1      NSEG
         IF ((NER2 .EQ. 12) .AND. (NER5 .EQ. 13)) WRITE(6,140) XARG, 
     1      YARG, NSEG
         IF ((NER2 .EQ. 13) .AND. (NER5 .EQ. 13)) WRITE(6,150) XARG, 
     1      YARG, NSEG
         GO TO 760
  740    WRITE (6,750)
  750    FORMAT ('TWOVIN INTERPOLATION ERROR. EITHER THE'
     1    'X OR THE Y ENTRIES ARE NOT IN ASCENDING ORDER.')
  760    RETURN
      END


         SUBROUTINE RDMAIN
C*********  MAIN PROGRAM FOR THE SPECTRUM LOADING SEQUENCE GENERATION
C*********  PROGRAM.  IT READS AND PRINTS THE TITLE AND SOME OF THE
C*********  CONSTANTS USED IN THE PROGRAM.
C*********  NOTE - THE SIZE OF THE DYNAMIC WORKING ARRAY IS SET BY THE
C*********         DIMENSIONS OF A AND BY NSIZE.  THROUGH OUT THE
C*********         SPECTRUM LOADING SEQUENCE GENERATION PROGRAM, CORE
C*********         ALLOCATION IS DONE THROUGH IMPLIED EQUIVALENCES.
C*********  SUBROUTINES CALLED - ERROR, INPUTF, NEWPG
            DIMENSION A(28000), N(28000)
            EQUIVALENCE (A(1), N(1))
            COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI, 
     1             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP, 
     2             ELIMV, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS, 
     3             MAXHP, KF, KC, A
            !REAL N 
            N=0
            NSIZE = 28000
            NEXT = 1
            NOW = 1
            TITLE = BLANK
            READ (5,'(20A4)') (TITLE(I), I = 1,20)
            NPG = 1
            CALL NEWPG
            IERR = 0
            READ (5,*) NFT, IRS, IFRS, IUIL, IPI, IRAN, KVP, IFI, NXY, 
     1                 IPFS, NPSS, IPTF, IAFS, MAXHP, KF, KC
            WRITE (6,30) NFT, IRS, IFRS, IUIL, IPI, IRAN, KVP, IFI, NXY,
     1                   IPFS, NPSS, IPTF, IAFS, MAXHP
   30       FORMAT (
     X      15X, 'NUMBER OF FLIGHT TYPES IS.......................', I5/
     1      15X, 'VALLEY/PEAK COUPLING IS..(1=A6PA, 2=RANDOM).....', I5/
     F      15X, 'FLIGHT SEQUENCE IS..(C=RANDOM...................', I5/
     F      15X, '..........(N=INPUT FLIGHT SEQUENCE, NSETS)......', /
     2      15X, 'FORTRAN UNIT NUMBER IS..........................', I5/
     4      15X, 'NUMBER OF INPUT PEAK LEVELS IS..................', I5/
     5      15X, 'NUMBER OF INPUT RANGE LEVELS IS.................', I5/
     3      15X, 'NUMBER OF INPUT VALLEY/PEAK RATIOS IS...........', I5/
     6      15X, 'SAVE SPECTRUM ON MAGNETIC TAPE..................', I5/
     6      15X, '..........................(0 = NO, 3 = YES).....', /
     7      15X, 'NUMBER OF POINTS OF THE RANGE VS R INPUT CURVE '
     8           'IS....', I5/
     9      15X, 'PRINT FLIGHT SEQUENCE (-1 = NO, 0 = ALL, N = '
     A           'NUMBER).', I5/
     B      15X, 'NUMBER OF SPECTRUM SUMMATIONS TO BE PRINTED ....', I5/
     C      15X, 'PROGRAM TERMINATION FLAG (0 = NORMAL, ..........', /
     D      15X, '..................M = STOP AFTER M FLIGHTS......', I5/
     E      15X, 'ALTERNATE FLIGHT SEQUENCE FLAG..................', I5/
     F      15X, '.(0 = REANDOM, 1=LOW-HI, 2=HI-LOW, 3=LO-HI-LO)....', /
     G      15X, 'NUMBER OF HIGHEST PEAKS PER FLIGHT TO BE PRINTED.',I5)
            WRITE (6,40) KF, KC
   40       FORMAT (
     1       15X, 'STARTING VALUE FOR THE GENERATION OF RANDOM.......',/
     2       15X, '.FLIGHT NUMBERS (0 = DEFAULT DEFINED AS 11111).', I5/
     3       15X, 'STARTING VALUE FOR THE RANDOM CYCLE GENERATION.', I5/
     4       15X, '.........(0 = DEFAULT DEFINED AS 12345)...........')
            NRAN = IABS(IRAN)
            NPI = IABS(IPI)
            IVP = IABS(KVP)
            READ (5,*) CLIP, CLIV, FACTOR, ELIMP
            WRITE (6,50) CLIP, CLIV, FACTOR, ELIMP
   50       FORMAT (
     X       15X,'PEAK CLIPPING VALUE IS.............................', 
     1       F10.0 / 15X, 'VALLEY CLIPPING VALUE IS....................'
     2       '.....', F10.0 / 15X, 'MULTIPLICATION FACTOR IS..........'
     3       '...............', F10.5 / 15X, 'CYCLE ELIMINATION PEAK' 
     4       'VALUE IS.................', F10.0//)
            LEFT = NSIZE - 2 * NFT
            IF (LEFT .LT. 0) CALL ERROR (1, LEFT, NSIZE, NFT)
            rewind iuil ! PSV
            CALL INPUTF (NFT, N(1), N(NFT+1), N(2*NFT+1), NST, IRAN, 
     1                   IPI, KVP)
            RETURN
         END


         SUBROUTINE INPUTF (NFT, NF, NS, A, NST, IRAN, IPI, KVP)
C*********  THS SUBROUTINE READS IN THE NUMBER OF FLIGHTS
C*********  AND THE NUMBER OF SEGMENTS IN EACH FLIGHT TYPE.
C*********  IT ALSO SETS UP SOME IMPLIED EQUIVALENCES TO
C*********  THE A ARRAY.
C*********  SUBROUTINES CALLED -ERROR, INF1F2
            DIMENSION NF(NFT), NS(NFT), A(*)
            COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI, 
     1             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP, 
     2             ELIMV, IPFS, NPSS, IPTF, IAFS, NEXT, NOW, IFRS
C*********  READ IN THE NUMBER OF FLIGHTS IN EACH FLIGHT TYPE
            READ (5,*) NF
C*********  READ IN THE NUMBER OF SEGMENTS IN EACH FLIGHT TYPE
            READ (5,*) NS
   10       FORMAT (1X,15I7)
C*********  COMPUTE THE TOTAL NUMBER OF FLIGHTS
C*********      AND THE TOTAL NUMBER OF SEGMENTS.

            NST = SUM(NS(1:NFT))
            NTF = SUM(NF(1:NFT))
C*********  CALCULATE THE STARTING POIT FOR EACH ARRAY WITHIN
C*********  THE A ARRAY.
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
            CALL INF1F2 (NST, NFT, NS, A(MF1), A(MF2), A(MN), NF1ST, 
     1                   NF2ST, NTF, NF, A(MPI), A(MRAN), A(MVP), 
     2                   A(MXY), IRAN, IPI, KVP, A(MISS), A(MFRS))
            RETURN
         END


         SUBROUTINE NEWPG
C************  THE SUBROUTINE PRINTS OUT A HEADING AT THE TOP OF *******
C************  EACH NEW PAGE. THE HEADING INCLUDES CONSECUTIVE   *******
C************  PAGE NUMBERING AND THE USER INPUT TITLE.          *******
C************  SUBROUTINES CALLED - NONE                         *******
            COMMON NPG, TITLE(20)
            WRITE (6,10) NPG, TITLE
   10       FORMAT ('SPECTRUM LOADING SEQUENCE GENERATION PROGRAM',
     1             15X, 'PAGE', I5//5X, 'JOB TITLE  ', 20A4//)
            NPG = NPG + 1
            RETURN
         END



         SUBROUTINE INMMN (NFT , NS, IF1, IF2, NST, SMAX, SMIN, NCY,
     1                     NF1ST, NF2ST, MMN, M3, NS1, NS2, NTF, NF, 
     2                     IS2)
C***********  THIS SUBROUTINE ETS UP THE CORE STORAGE               ****
C***********  REQUIRED FOR THE CALLS TO SUBROUTINE AMMN             ****
C***********  SUBROUTINES CALLED - AMMN, ERROR                      ****
            DIMENSION NS(NFT), IF1(NST), IF2(NST), SMAX(M3), SMIN(M3), 
     1                NCY(M3)
            DIMENSION NF(NFT), IS2(NST)
   10       FORMAT (1X, 'IN_MMN', 15I7)
            JF1 = 1
            JF2 = 1
            JS1 = 1
            JS2 = 1
            MMN = 1
C**********  GET THE MAXIMUM STRESSES, MINIMUM STRESES, AND NUMBER *****
C**********  OF CYCLES FOR EACH FLIGHT TYPE.  ALSO DO COMBINING OF *****
C**********  SEGMENTS AND CYCLES.
            DO 20 I = 1, NFT
               NSS = NS(I)
               CALL AMMN (NSS, IF1(JF1), IF2(JF2), IF1(JS1), IS2(JS2), 
     1                    NS1, NS2, SMAX(MMN), SMIN(MMN), NCY(MMN), 
     2                    M3-MMN, NNM, NF(I), NCY(MMN), I)
               JF1 = JF1 + NSS
               JF2 = JF2 + NSS
               JS1 = JS1 + NS1
               JS2 = JS2 + NS2
               NS(I) = JS2 - 1 + (NST+1) * MMN
               WRITE(11,*)'I,NS(I) ',I, NS(I)
                !close(11)
                !open(unit=11, access='APPEND')
               MMN = MMN + NNM
               IF (MMN .GE. M3) CALL ERROR (3,MMN,M3,I)
   20       CONTINUE
            MMN = MMN -1
            JS2 = JS2 -1
   30       FORMAT ('IF MMN', 10F12.2)
C**********  AFTER PERFORMING THE COMBINING OF SEGMETS,          *******
C**********  RECREATE THE F2 ARRAY.                              *******
            DO 40 I = 1,JS2
   40       IF2(I) = IS2(I)
            RETURN
         END

         SUBROUTINE AMMN (NSS, IF1, IF2, IS1, IS2, NS1, NS2, SMAX, SMIN,
     1                    NCY, MM, MMN, NF, BCY, IDFT)
C**********  THIS SUBROUTINE PERFORMS THE COMBINING OF EGMENTS      ***
C**********  BASED ON THE USER INPUT ARRAYS, F1 AND F2, PRODUCING   ***
C**********  NEW MAXIMUM STRESS, MINIMUM STRESS AND NUMBER OF CYCLE ***
C**********  ARRAYS.                                                ***
            COMMON SKIP(25), IUIL
            DIMENSION IF1(NSS), IF2(NSS), IS1(*), IS2(*), SMAX(MM), 
     1                SMIN(MM), NCY(MM)
            DIMENSION AMAX(25), AMIN(25), ACY(25), BCY(MM)
   10       FORMAT (1X, 'AMMN ', 2I6, 19I4)
   20       FORMAT (1X, 'AMMN ', 6F12.2)

            AMAX=0     !
            AMIN=0     ! PSV
            ACY =0     !
            BCY =0     !

            JS1 = 0
            MMN = 0
C***********  SUMMATION OF CYCLES                               ********
C***********  F1 COMBINING OF SEGMENTS                          ********
            !rewind iuil ! PSV
            DO 80 I = 1,NSS
C***********  READ IN NUMBER OF GROUPS, PEAK VALUE, VALLEY      ********
C***********  NUMBER OF CYCLES SEQUENCE FROM UTILITY TAPE       ********
               READ (IUIL) KMMN, (AMAX(K), AMIN(K), ACY(K), K = 1, KMMN)
               write(15,*) 'idft ',idft, KMMN, 
     1         (AMAX(K), AMIN(K), ACY(K), K = 1, KMMN)
               KF1 = IF1(I)
               IF (KF1 .EQ. 0) GO TO 70
               IF (KF1 .LE. JS1) GO TO 50
               JS1 = JS1 + 1
               DO 30 J = 1, KMMN
                  MMN = MMN + 1
                  SMAX(MMN) = AMAX(J)
                  SMIN(MMN) = AMIN(J)
                  BCY(MMN) = ACY(J)
   30          CONTINUE
   40          IS1(JS1) = MMN
               GO TO 80
   50          K2 = IS1(KF1)
               IF (KF1 .EQ. 1) K1 = 1
               IF (KF1 .NE. 1) K1 = IS1(KF1-1) + 1
               K = 1
               DO 60 J = K1,K2
                  BCY(J) = BCY(J) + ACY(K)
   60          K = K + 1
   70          IF2(I) = 0
               WRITE(15,*) 'KF1 ', KF1
   80       CONTINUE
            NS1 = JS1
            K1 = 1
            L = 1
C************  ROUND OFF CYCLES TO NEAREST INTEGER AND ELIMINATE *******
C************  CYCLES THAT ARE LESS THAN 0.5                     *******
            DO 100 I = 1,NS1
               K2 = IS1(I)
               DO 90 J = K1,K2
                  NCY(L) = BCY(J) + 0.5000001
                  SMAX(L) = SMAX(J)
                  SMIN(L) = SMIN(J)
                  IF (NCY(L) .GT. 0) L = L + 1
   90          CONTINUE
               K1 = K2 + 1
               IF (L .LE. 1) CALL ERROR (7, I, NSS, K2)
  100       IS1(I) = L - 1
            MMN = L - 1
C************  MOVE DOW TO THE END OF THE ARRAYS SO THAT F2      *******
C************  COMBINING OF SEGMENTS CAN BE DONE                 *******
            K = MMN
            J = MM
            write(11,*) 'MMN,NSS ',MMN,NSS
            WRITE(17,*)'bef 110 ID, NCY',idft, (ncy(I17),I17=1,NSS)
            DO 110 I = 1, MMN
               SMAX(J) = SMAX(K)
               SMIN(J) = SMIN(K)
               NCY(J) = NCY(K)
               J = J - 1
  110       K = K - 1
            NDEL = J - K
            KS1 = 1
C************ F2 COMBINING OF SEGMENTS
            NSS1 = NSS + 1
            WRITE(17,*)'ID, NCY',idft, (ncy(I17),I17=1,NSS)
            DO 210 I = 1, NSS
               IS2(I) = 0
               KKQ = K
               KQ = KKQ + 1
               KF1 = 0
               DO 200 J = 1, NSS1
                  IF (J .EQ. NSS1) GO TO 120
                  IF (IF2(J) .NE. 0) KF1 = KF1 + 1
                  IF (IF2(J) .NE. I) GO TO 200       ! PSV: 1 -> I
                  GO TO 180
  120             IF (KKQ .EQ. K) GO TO 220
                  KQQ = KQ + IS2(I) -1
                  KK = 0
                  DO 130 L = KQ, KQQ
  130             KK = KK + NCY(L)
                  AA = (KK-1) / NF
                  IA = AA
                  NCY(K+1) = (IA+1) * NF - KK
                  IF (NCY(K+1) .EQ. 0) GO TO 170
C************ FORMULATE FICTITIOUS LOAD LEVEL
                  K = K + 1
                  IS2(I) = IS2(I) + 1
                  MAX = 0
                  MIN = 0
                  KQ1 = KQ + 1
                  IF (KQ1 .GT. KQQ) GO TO 140
                  IF (SMAX(KQ1) .NE. SMAX(KQ)) MAX = 1
                  IF (SMIN(KQ1) .NE. SMIN(KQ)) MIN = 1
                  IF (MIN. NE. 0) GO TO 150
  140             SMIN(KQQ+1) = SMIN(KQ)
                  SMAX(KQQ+1) = SMIN(KQ)
                  GO TO 170
  150             IF (MAX .NE. 0) GO TO 160
                  SMAX(KQQ+1) = SMAX(KQ)
                  SMIN(KQQ+1) = SMIN(KQ)
                  GO TO 170
  160             SMAX(KQQ+1) = (SMAX(KQ) + SMIN(KQ)) / 2.0
                  SMIN(KQQ+1) = SMAX(KQQ+1)
  170             CONTINUE
C***  WRITE (2,10) I,KQ,KQQ,K1,K,MAX,MIN,IA,IS2(I-1),(NCY(L), L-KQ,K)
C***  WRITE (2,20) (SMAX(L), SMIN(L), L = KQ,KK)
                  GO TO 200
  180             K2 = IS1(KF1) + NDEL
                  IF (KF1 .EQ. 1) K1 = 1 + NDEL
                  IF (KF1 .NE. 1) K1 = IS1(KF1-1) + 1 + NDEL
                  write(11,*) 'I, J, K1, K2 ',I, J, K1, K2
                  DO 190 L = K1,K2
                     K = K + 1
                     SMAX(K) = SMAX(L)
                     SMIN(K) = SMIN(L)
                     BCY(K) = BCY(L)
  190             NCY(K) = NCY(L)
                  IS2(I) = IS2(I) + K2 - K1 + 1
  200          CONTINUE
               write(17,*) 'ID, I, NCY',idft, I,NCY(I)
  210       CONTINUE
            I = NSS1
  220       NS2 = I -1
            MMN = K
            DO 230 I = 2,NS2
               IS2(I) = IS2(I) + IS2(I-1)
  230       CONTINUE
            WRITE(11,*)'NSS,IS2 ',NSS, (IS2(I17),I17=1,NSS)
            WRITE(17,*)'ID, NCY',idft, (ncy(I17),I17=1,NSS)
            !close(11)
            !open(unit=11, access='APPEND')
            LINE = 60
            IS = 1
            IAA = 0
            IA = 0
            BAA = 0.0
            AAA = 0.0
C***********   PRINT OUT AFTER SEGMENT COMBINING               *********
            DO 270 I = 1,MMN
               AT = NCY(I)
               AA = AT/NF
               AAA = AAA + AA
               IA = IA + NCY(I)
               IF (LINE .LT. 49) GO TO 250
               CALL NEWPG
               WRITE (6,240) IDFT
  240          FORMAT (4X, 'FLIGHT TYPE', I4, 7X, 'F2', 9X, 'MIN.', 7X,
     1                 'MAX.', / , 25X, 'SEGMENT', 5X 'STRESS', 5X, 
     2                 'STRESS', 6X, 'CYCLES', 6X, 'CYCLES/FLIGHT',/)
               LINE = 0
  250          WRITE (6,260) IS, SMIN(I), SMAX(I), NCY(I), AA
               LINE = LINE + 1
               IF (IS2(IS) .GT. I) GO TO 270
               IAA = IA - IAA
               BAA = AAA - BAA
               WRITE (6,280) IAA,BAA
               LINE = LINE + 2
               IS = IS + 1
               IAA = IA
               BAA = AAA
  260          FORMAT (24X, I4, F14.0, F11.0, I12, 5X, F12.4)
  270       CONTINUE
            WRITE (6,280) IA, AAA
  280       FORMAT (53X, I12, 5X, F12.4, /)
            RETURN
         END



         SUBROUTINE GENFL(NF,NFT,NTF,NS,NST,SMAX,SMIN,NCY,NMM,IS2,
     1                    SMM, MAXSS, PI, RAN, VP, XY, RR, NNRAN, PR,
     2                    NNPI, IRR, NRMAX, ISS, PMAX, MCY, NFF, IPF,
     3                    HPEAK, IDPEAK, JTN)
C***********  THIS SUBROUTINE GENERATES THE SEQUENCE OF FLIGHTS.  ******
C***********  DEPENDING UPON INPUT, THE SEQUENCE CAN BE RANDOMLY  ******
C***********  GENERATED OR USER SPECIFIED.                        ******
C***********  SUBROUTINES CALLED - DISTRD, GENCY, PRNTSS          ******
            COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, NPI, 
     1             NRAN, IVP, IFI, NXY, CLIP, CLIV, FACTOR, ELIMP, 
     2             ELIMV, IPFS, IPSS, IPTF, IAFS, NEXT, NOW, IFRS, 
     3             MAXHP, IKF, IKC
            INTEGER RR(NNRAN,1), PR(NNPI,1)
            DIMENSION NF(NFT), NS(NFT), SMAX(NMM), SMIN(NMM), IS2(NST),
     1                XY(2,1)
            DIMENSION SMM(2,MAXSS), NCY(NMM,*), PI(*), RAN(*), VP(*)
            DIMENSION PMAX(NTF), MCY(NTF), JTN(NTF), IRR(NRMAX,1), 
     1                IJJ(6), ISS(*)
            DIMENSION IPF(2,1), NFF(2,NFT), HPEAK(*), IDPEAK(*)
            WRITE(22,*)'NFT, NTF, NST, NMM, MAXSS ',
     1      NFT, NTF, NST, NMM, MAXSS
            WRITE(22,*)'NF ', NF(1:NFT)
            WRITE(22,*)'NS ', NS(1:NFT)
            WRITE(22,*)'SMAX ', SMAX(1:NMM)
            WRITE(22,*)'SMIN ', SMIN(1:NMM)
            WRITE(22,*)'IS2 ', IS2(1:NST)
            WRITE(22,*)'SMM ', SMM
   20       FORMAT (1X, 'GEN FL', 20I6)
   30       FORMAT (1X, 'GEN FL', 10F12.2)
            write(12,*) 'RR0',((RR(i8,j8), i8=1,nnran),j8=1,28)
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
            WRITE(11,*)'npss ',npss
            !close(11)
            !open(unit=11, access='APPEND')
C************  ZERO OUT THE TABLES USED IN THE SPECTRUM SUMMATION
            DO 40 I = 1,NFT2
   40       NFF(I,1) = 0
            WRITE(11,*)'NMM ',NMM
            !close(11)
            !open(unit=11, access='APPEND')
            DO 50 I = 1,6
   50       IJJ(I) = 0
            IF (IRS .EQ. 1) GO TO 70
            DO 60 I = 1,NMM
   60       NCY(I,2) = NCY(I,1)
   70       IIVP = IVP + 2
            DO 100 I = 1, IIVP
               DO 81 J = 1, NNRAN
   80          RR(J,I) = 0
   81          CONTINUE
               DO 91 J = 1, NNPI
   90          PR(J,I) = 0
   91          CONTINUE
  100       CONTINUE
            WRITE(11,*)'NNPI ',NNPI
            !close(11)
            !open(unit=11, access='APPEND')
            IF (MAXHP .EQ. 0) GO TO 120
C************ ZERO OUT HIGHEST PEAK ARRAYS
            DO 110 J = 1, MAXHP
               HPEAK(J) = 0.0
               IDPEAK(J) = 0
  110       CONTINUE
  120       SMM(1,1) = -1.E20
            SMM(2,1) = 1.E20
            NST1 = NST + 1
            I1 = 2
            I2 = 1
            JPF = 1
            WRITE(11,*)'MAXHP,NTF ',MAXHP,NTF
            !close(11)
            !open(unit=11, access='APPEND')
            DO 190 II = 1, NTF
               IF (IFRS .EQ. 0) GO TO 130
C************  THE SEQUENCE OF FLIGHT NUMBERS IS SPECIFIED BY INPUT
               JF = IPF (1,JPF)
               IPF(2,JPF) = IPF(2,JPF) -1
               IF (IPF(2,JPF) .EQ. 0) JPF = JPF + 1
               GO TO 140
C************  THE SEQUENCE NUMBER OF FLIGHT NUMBERS IS RADOMLY GENERATED
  130          CALL DISTRD (NF, NFT, KF, JF)
  140          NFF(1,JF) = NFF(1,JF) + 1
               MMN = NS(JF) / NST1
               IF (JF .NE. 1) JS2 = NS(JF-1) - NST1 * (NS(JF-1) / 
     1                              NST1) + 1
               IF (JF .EQ. 1) JS2 = 1
               NSS = NS(JF) - MMN * NST1 - JS2 + 1
C***  WRITE (2,10) I1, NMM, IRS
C***  WRITE (2,10) NSS,JF,MMN,NST1,JS2,JPF,NF,NS,NFT,NTF,MAXSS
               IF (II .EQ. NTF) I2 = 0
!            WRITE(11,*)'NS(JF),NST1,KF,JF,JS2,NSS ',NS(JF),NST1,KF,
!     1      JF,JS2,NSS
            !close(11)
            !open(unit=11, access='APPEND')
            write(12,*) 'RRA',((RR(i8,j8), i8=1,nnran),j8=1,28)
C************ GENERATE CYCLE SEQUENCE
               CALL GENCY (IS2(JS2), SMAX(MMN), SMIN(MMN), NCY(MMN,1), 
     1                     NF(JF), SMM(1,1), MAXSS, NSS, NCY(MMN,2), 
     2                     PI, RAN, VP, XY, RR, NNRAN, PR, NNPI, KC, I1,
     3                     I2, IJJ, II, JF, NPRNT, KLINE, AMAX, KCY, 
     4                     NPTF)
            WRITE(11,*)'IAFS ',IAFS
            write(12,*) 'RRB',((RR(i8,j8), i8=1,nnran),j8=1,28)
            !close(11)
            !open(unit=11, access='APPEND')
               IF (IAFS .EQ. 0) GO TO 145
C*********** SAVE THE HIGHEST PEAK AND NUMBER OF CYCLES FOR EACH FLIGHT
C*********** TO BE USED FOR THE ALTERNATE FLIGHT SEQUENCE
               PMAX(II) = AMAX    !  PSV
               MCY(II) = 2 * KCY  !  I1 -> II
               JTN(II) = JF       !
               GO TO 170
  145        IF (MAXHP.EQ.0) GO TO 170
C*********** DETERMINE AND SAVE THE HIGHEST PEAK AND CORRESPONDING
C*********** FLIGHT NUMBER FOR MAXHP (SPECIFIED BY INPUT) NUMBER
C*********** OF FLIGHTS
               HP = AMAX
               III = II   !PSV I1 ->II
               DO 160 I = 1,MAXHP
                  IF (HP .LE. HPEAK(I)) GO TO 160
                  DO 150 K = I,MAXHP   !PSV  1 -> I
                     TMAX = HPEAK(K)
                     HPEAK(K) = HP
                     HP=TMAX
                     IJF = IDPEAK(K)
                     IDPEAK(K) = III
                     III = IJF
  150             CONTINUE
  160          CONTINUE
  170          I1 = 1
               NFF(2,JF) = NFF(2,JF) + KCY
               NF(JF) = NF(JF) -1
            write(12,*) 'RRC',((RR(i8,j8), i8=1,nnran),j8=1,2)
            WRITE(11,*)'KCY, II, NPSS ',KCY, II, NPSS
            close(11)
            open(unit=11, access='APPEND')
                  IF ((II .NE. NPSS) .OR. (IAFS .NE. 0)) GO TO 180
C***  WRITE (2,10) IJJ
C***  WRITE (2,10) MCY
C***  WRITE (2,20) PMAX
C************  IF NO ALTERNATE FLIGHT SEQUENCE IS DESIRED THEN
C************  PRINT THE SPECTRUM SUMMATION TABLES
                  CALL PRNTSS (RR, NNRAN, PR, NNPI, IRR, NRMAX, IIVP, 
     1                         RAN, VP, PI, IS, NFF, ISS, II, NTF, NFT, 
     2                         HPEAK, IDPEAK)
  180             IF (II .EQ. NPTF) GO TO 200
  190       CONTINUE
  200       write(12,*) 'RR2',((RR(i8,j8), i8=1,nnran),j8=1,28)
            return
! 200       RETURN
         END



               SUBROUTINE GENAFS (MCY, PMAX, LREC, AFS, IFLAG, NTF, SMM, 
     1                            RR, NNRAN, PR, NNPI, IRR, NRMAX, PI, 
     2                            RAN, VP, NFF, ISS, NFT, HPEAK, IDPEAK,
     3                            JTN)
                  INTEGER RR(NNRAN,1), PR(NNPI,1), IRR(NRMAX,1)
C************  THIS SUBROUTINE RECORDS THE RANDOM FLIGHT SEQUENCE ******
C************  INTO LO-HI, HI-LO, LO-HI-LO FLIGHT SEQUENCE BASED  ******
C************  UPON THE MAXIMUM STRESS PER FLIGHT.  IT PERFORMS A ******
C************  TYPE 1 EDIT BEFORE DOING A SPECTRUM SUMMATION      ******
C********SUBROUTINES CALLED - NEWPG, PRNTSS, REED, REDIT1, SPSUM  ******
                  COMMON ISKIP(28), IVP, ISKP(7), IPFS, IPSS, IPTF, 
     1                   IAFS, SPA(3), MAXHP
                  DIMENSION MCY(NTF), PMAX(NTF), LREC(NTF), AFS(NTF), 
     1                      IFLAG(NTF), N1(3)
                  DIMENSION IADD(3), FORM(3,3), TITLE(5), SMM(2,1), 
     1                      IJJ(6), JTN(NTF)
                  DIMENSION PI(*), RAN(*), VP(*), NFF(*), ISS(*), 
     1                      HPEAK(*), IDPEAK(*)
                  

                  NPRNT = IPFS
                  IF (IPFS. EQ. 0) NPRNT = NTF
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
                  DO 30 I = 1, IIVP
                     DO 11 J = 1, NNRAN
   10                RR(J,I) = 0
   11                CONTINUE
                     DO 20 J = 1, NNPI
   20                PR(J,I) = 0
   21                CONTINUE
   30             CONTINUE
                  DO 40 I = 1,NTF
   40             IFLAG(I) = 0
C*********** REORDER THE FLIGHT SEQUENCE INTO LO-HI, HI-LO, OR LO-HI-LO
C*********** SEQUENCE DEPENDING UPON TEH IAFS FLAG SPECIFIED BY INPUT
                  DO 90 II = 1,NTF
                     PHI = -1.E20
                     DO 50 I = 1,NTF
                        IF (PMAX(I) .LT. PHI) GO TO 50
                        IF (IFLAG(I) .GT.0) GO TO 50
                        PHI = PMAX(I)
                        IFL = I
   50                CONTINUE
                     IF (IAFS .NE. 3) GO TO 60
                     IADD(3) = -1 * IADD(3)
                     INO = IADD(3) * (II -1)
   60                IS = IS + INO
                     AFS(IS) = PHI
                     LREC(IS) = IFL
                     IFLAG(IFL) = 1
C**********  FIND THE HIGHEST STRESSES AND ASSOCIATED FLIGHT NUMBERS
                     IF (MAXHP .EQ. 0) GO TO 90
                     HP = PHI
                     III = IS
                     DO 80 JJ = 1, MAXHP
                        IF (HP .LE. HPEAK(JJ)) GO TO 80
                        DO 70 K = JJ,MAXHP
                           TMAX = HPEAK(K)
                           HPEAK(K) = HP
                           HP = TMAX
                           JF = IDPEAK(K)
                           IDPEAK(K) = III
                           III = JF
   70                   CONTINUE
   80                CONTINUE
   90             CONTINUE
C********  PRINT THE NEW ORDER OF FLIGHTS AND THE LARGEST STRESS
C********  PER FLIGHT
                  LINE = 50
                  DO 140 I = 1,NTF,5
                     N4 = 1
                     N2 = I + 4
                     IF (N2 .GT. NTF) N2 = NTF
                     N3 = N2 - N4 + 1
                     LINE = LINE + 1
                     IF ( LINE .LT. 45) GO TO 120
                     CALL NEWPG
                     WRITE (6,100) (FORM(J,IAFS), J = 1,3)
  100                FORMAT (15X, 3A4, 'ORDER OF FLIGHTS(NEAREST PEAK /'
     1                      'FLIGHT)', //)
                     LINE = 0
                     WRITE (6,110) ((TITLE(K), K=1, 5), J = 1, N3)
  110                FORMAT (5(5A4,4X))
  120                WRITE (6,130) (LREC(J), AFS(J), J = N4, N2)
  130                FORMAT (5(I6, F13.2, 5X))
  140             CONTINUE
                  IS = 1
                  LKINE = 60
                  DO 230 I = 1,NTF
                     NCY = MCY(LREC(I))
                     CALL REED (SMM, LREC(I), NCY)
C**********  REPEAT AN EDIT 1 BETWEEN FLIGHTS BECAUSE OF REORDERING
                     NCY = NCY / 2
                     CALL REDIT1 (SMM, IJJ, NCY, EPS)
C**********  SPECTRUM SUMMATION
            write(12,*) 'RR0',((RR(j,i8), j=1,iivp),i8=1,nnran)
                     CALL SPSUM (SMM, 1, NCY, PMAX(I), VP, RAN, PI, RR, 
     1                           NNRAN, PR, NNPI)                            ! PSV I -> 1
            write(12,*) 'RR1',((RR(j,i8), j=1,iivp),i8=1,nnran)
  150                FORMAT ('GEN AFS', 4I5, 4F10.0)
                     IF ( I .GT. NPRNT) GO TO 210
                     IF (LKINE .LT. 53) GO TO 160
                     CALL NEWPG
                     KLINE = 5
  160                WRITE (6,170) I, LREC(I), JTN(LREC(I)), NCY
                     KLINE = KLINE + 2
C**********  PRINT THE NEW FLIGHT NUMBER, THE OLD FLIGHT NUMBER
C**********  FLIGHT TYPE NUMBER, NUMBER OF CYCLES AND THE
C**********  SEQUENCE OF MAXIMUM AND MINIMUM STRESES.
                     DO 200 JJ = 1, NCY, 5
                        J1 = JJ
                        J2 = J1 + 4
                        IF (J2 .GT. NCY) J2 = NCY
                        IF (KLINE .LT. 55) GO TO 180
                        CALL NEWPG
                        KLINE = 5
  170                   FORMAT ('NEW FLIGHT NUMBER', I5, 
     1                          'OLD FLIGHT NUMBER', I5,
     2                          '  IS TYPE NUMBER', I3, 
     3                          '   NUMBER OF CYCLES', I7,
     4                          '  SEQUENCE FLOWS ')
  180                   WRITE (6,190) ((SMM(K,J), K = 1,2), J = J1,J2)
  190                   FORMAT (10F12.2)
                        KLINE = KLINE + 1
  200                CONTINUE
  210                IF (I .NE. NPSS) GO TO 220
C********** PRINT THE SPECTRUM SUMMATION
C********** PRINT THE SPECTRUM SUMMATION
                     CALL PRNTSS (RR, NNRAN, PR, NNPI, IRR, NRMAX, 
     1                            IIVP, RAN, VP, PI, IS, NFF, ISS, I, 
     2                            NTF, NFT,HPEAK, IDPEAK)
  220                IF (I .EQ. NPTF) GO TO 240
  230             CONTINUE
  240             RETURN
               END



               SUBROUTINE GENCY (IS2, SMAX, SMIN, NCY, NF, SMM, MAXSS, 
     1                           NSS, KCY, PI, RAN, VP, XY, RR, NNRAN, 
     2                           PR, NNPI, KC, I1, I2, IJJ, II, JF, 
     3                           NPRNT, KLINE, PMAX, MCY, INTF)
C********* THS SUBROUTINE GENERATES THE CYCLE SEQUENCE FOR THE GIVEN ***
C********* FLIGHT.  IT THEN PERFORMS ALL EDITING OF THE CYCLES. IF   ***
C********* A SAVE TAPE IS SPECIFIED AND NO ALTERNATE FLIGHT SEQUENCE ***
C********* IS DESIRED, THEN THE CYCLE SEQUENCE WRITTEN DIRECTLY ONTO ***
C********* AN OUTPUT TAPE.  OTHERWISE THE CYCLE SEQUENCE IS WRITTEN  ***
C********* ONTO TEMPORARY MASS STORAGE.                              ***
C********* SUBROUTINES CALLED - DISTRD, ERROR, NEWPG, REDIT1,        ***
C*********                      RITE, SPSUM, WTAPE                   ***
                  COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, IUIL, 
     1                   NPI, NRAN, IVP, IF1, NXY, CLIP, CLIV, FACTOR, 
     2                   ELIMP, ELIMV, IPFS, IPSS, IPTF, IAFS
                  DIMENSION IS2(NSS), SMAX(*), SMIN(*), NCY(*), 
     1                      SMM(2,MAXSS)
                  DIMENSION KCY(*), IJJ(*)
                  INTEGER RR, PR
                  DIMENSION PI(*), RAN(*), VP(*), XY(2,*), RR(NNRAN,*)
                  DIMENSION PR(NNPI,1)
   10             FORMAT (1X, 'GEN CY', I3, 19I4)
   20             FORMAT (1X, 'GEN CY', 5F12.2)
   30             FORMAT ('  FLIGHT NUMBER', I4, '  IS TUPE NUMBER', I3,
     1                    '    NUMBER OF CYCLES', I7, 
     2                    '  SEQUENCE FOLLOWS ')
                  IMM = 1
                  KMM = 1
                  K1 = 1
                  K0 = 0
                  IF (II .EQ. 1) NERR5 = 0
                  EPS = 1.
                  PMAX = -1.E20
                  DO 150 I = 1,NSS
                     K2 = IS2(I)
                     NCS = 0
                     K3 = K2 - K0
                     LMM = IMM
                    write(11,*)'K1,K2 ',K1,K2
                        !close(11)
                        !open(unit=11, access='APPEND')
                     DO 40 J = K1,K2
   40                NCS = NCS + NCY(J)
                     LCY = NCS / NF
                     DO 140 J = 1,LCY
C*******  IF PAIRED VALLEY PEAK COUPLING (IRS=1) IS SPECIFIED,
C*******  FIND THE RANDOM CYCLE. IF INDIVIDUAL VALLEY PEAK
C*******  COUPLING (IRS = 2) IS SPECIFIED, FIDN THE RANDOM PEAK.
                        CALL DISTRD (NCY(K1), K3, KC, IC)
          !write(11,*)'NSS,LCY,NCY(K1),K3,K0,I0 ',NSS,LCY,NCY(K1),K3,K0,I0
                        !close(11)
                        !open(unit=11, access='APPEND')
                        IMM = IMM + 1
                        IF (IMM .LT. MAXSS) GO TO 50
                        CALL ERROR (4, IMM, NSIZE, JF)
                        STOP 7002
   50                   SMM (1,IMM) = SMIN(IC+K0)
                        NCY(IC+K0) = NCY(IC+K0) -1
                        IF (IRS .EQ. 2) GO TO 60
                        SMM(2,IMM) = SMAX(IC+K0)
                        GO TO 70
C********* IF INDIVIDUAL VALLEY PEAK COUPLING (IRS=2) IS
C********* SPECIFIED, FIND THE RANDOM VALLEY.
   60                   CALL DISTRD (KCY(K1), K3, KC, JC)
          !write(11,*)'KCY(K1),K3,K0,J0 ',KCY(K1),K3,K0,J0
                        !close(11)
                        !open(unit=11, access='APPEND')
                        SMM(2,IMM) = SMAX(JC+K0)
                        KCY(JC+K0) = KCY(JC+K0) -1
   70                   CONTINUE
          !write(11,*)'IMM,J0,K0 ',IMM,J0,K0
                        !close(11)
                        !open(unit=11, access='APPEND')
C********  EDIT 1
C********  CHECK TO SEE IF MINIMU IS REALLY A VALLEY AND NOT AN
C********  INTERMEDIATE POINT ON THE WAY TO A PEAK
                        IF (SMM(1,IMM-1) .NE. SMM(2,IMM-1)) GO TO 80
                        IF (SMM(1,IMM-1) .GT. SMM(1,IMM)) SMM(1,IMM-1) = 
     1                      SMM(1,IMM)
                        SMM(2,IMM-1) = SMM(2,IMM)
                        IMM = IMM - 1
                        IJJ(2) = IJJ(2) + 1
                        GO TO 140
   80                   IF (SMM(1,IMM) .LT. SMM(2,IMM-1)) GO TO 100
                        IJJ(1) = IJJ(1) + 1
   90                   FORMAT (1X, 4I5, 4F10.0)
                        SMM (2,IMM-1) = SMM(2,IMM)
                        IMM = IMM - 1
  100                   IF (ABS(SMM(1,IMM) - SMM(2,IMM)) .LT. EPS) 
     1                      GO TO 110
                        IF ((SMM(1,IMM) - SMM(2,IMM))) 140, 110, 130
C********* DROP OUT FICTITIOUS LOAD LEVELS
  110                   SMM(1,IMM) = SMM(2,IMM)
                        write(11,*)'110 SMM ',SMM(1,IMM)
                        !close(11)
                        !open(unit=11, access='APPEND')
                        IF ((SMM(2,IMM) .LT. SMM(2,IMM-1))) GO TO 140
                        SMM(2,IMM-1) = SMM(2,IMM)
  120                   IMM = IMM - 1
                        IJJ(3) = IJJ(3) + 1
                        GO TO 140
C*********  VALLEY GREATER THAN PEAK IS IN THIS SEGMENT
C*********  INTERCHANGE VALLEY AND PEAK
  130                   TEMP = SMM(1,IMM)
                        write(11,*)'130 TEMP ',TEMP
                        !close(11)
                        !open(unit=11, access='APPEND')

                        SMM(1,IMM) = SMM(2,IMM)
                        SMM(2,IMM) = TEMP
  140                CONTINUE
C***  WRITE (2,10) (IJJ(J), J = 1,3)
                     K0 = K2
                     K1 = K0 + 1
                        write(11,*)'NSS,I,IMM ',NSS,I,IMM
                        !close(11)
                        !open(unit=11, access='APPEND')
  150             CONTINUE
C**********  EDIT 2
                     KMM = IMM
                     IMM = I1 - 1
                        write(11,*)'I1 ',I1
                        !close(11)
                        !open(unit=11, access='APPEND')
                     DO 260 JMM = I1,KMM
                        IMM = IMM + 1
                        SMM(1,IMM) = SMM(1,JMM)
                        SMM(2,IMM) = SMM(2,JMM)
C*********** ELIMINATE RANGES BELOW AN INPUT RANGE VERSUS R CURVE
                        RANGE = SMM(2,IMM) - SMM(1,IMM)
                        R = SMM(1,IMM) / SMM(2,IMM)
C***  WRITE (2,10) JMM, IMM, I1, KMM
C***  WRITE (2,20) SMM(1,IMM), SMM(2,IMM) RANGE, R
                        IF (NXY .LE. 1) GO TO 220
                        IF (XY(1,1) .LE. R) GO TO 160
                        IF (NERR5 .LT. 6) CALL ERROR (5, II, IMM, 1)
                        NERR5 = NERR5 + 1
                        K = 2
                        GO TO 180
  160                   DO 170 K = 2, NXY
                           IF (XY(1,K) .GE. R) GO TO 180
  170                   CONTINUE
                        IF (NERR5 .LT. 6) CALL ERROR (5, II, IMM, 2)
                        K = NXY
  180                   RQ = XY(2,K) + (R-XY(1,K-1)) * (XY(2,K) - 
     1                       XY(2,K-1)) / (XY(1,K) - XY(1,K-1))
                        IF (RANGE .GT. RQ) GO TO 220
                        IF (IMM .EQ. 1) GO TO 190
                        IF (SMM(2,IMM) .LE. SMM(2,IMM-1)) GO TO 190
                        SMM(2,IMM-1) = SMM(2,IMM)
                        GO TO 200
  190                   IF ((JMM+1) .GT. KMM) GO TO 200
                        IF ((SMM(1,IMM) .GE. SMM(1,JMM+1))) GO TO 200
                        SMM(1,JMM+1) = SMM(1,IMM)
  200                   IJJ(4) = IJJ(4) + 1
  210                   IMM = IMM -1
                        IJJ(6) = IJJ(6) + 1
                        GO TO 260
C******** ELIMINATRE PEAKS BY INPUT VALUES
C******** CLIP PEAKS AND/OR VALLEY BY INPUT VALUES
  220                   IF (SMM(2,IMM) .GT. ELIMP) GO TO 210
                        IF (SMM(2,IMM) .LT. CLIP) GO TO 250
                        IF (SMM(1,IMM) .LT. CLIP) GO TO 240
C********** IF BOTH VALLEY AND PEAK ABOE PAK CLIPPING ELIMINATE CYCLE
C******OR IF BOTH VALLEY AND PEAK BELOW VALLEY CLIPPING ELIMINATE CYCLE
  230                   IMM = IMM -1
                        IJJ(5) = IJJ(5) + 1
                        GO TO 260
  240                   SMM(2,IMM) = CLIP
  250                   IF (SMM(1,IMM) .GT. CLIV) GO TO 260
                        IF (SMM(2,IMM) .LT. CLIV) GO TO 230
                        SMM(1,IMM) = CLIV
  260                CONTINUE
                     MCY = IMM -1
                     IF (MCY .EQ. 0) GO TO 350  ! PSV 340->350
C********** REPEAT A TYPE 1 EDIT
                     CALL REDIT1 (SMM, IJJ, IMM, EPS)
                     MCY = IMM - 1
                     IF (MCY . EQ. 0) GO TO 350
C********** MULTIPLICATION FACTOR
                     DO 270 I = 1,IMM
                        DO 270 J = 1,2
  270                SMM(J,I) = SMM(J,I) * FACTOR
  280                I3 = IMM - I2
C********** SPECTRUM SUMMATION
                     CALL SPSUM (SMM, I1, I3, PMAX, VP, RAN, PI, RR, 
     1                           NNRAN, PR, NNPI)
                     IF (IAFS .EQ. 0) GO TO 290
C********** WRITE CYCLE SEQUENCE ONTO TEMPORARY MASS STORAGE
                     CALL RITE (SMM(1,2), II, (2*MCY))
                     GO TO 340
C********** IF NO ALTERNATE FLIGHT SEQUENCE I DESIRED, BUT A SAVE
C********** TAPE IS SPECIFIED, THEN WRITE THE CYCLE SEQUENCE
C********** DIRECTLY ONT THE OUTPUT TAPE.
  290                IF (IFI .NE. 0) CALL WTAPE (II, (IMM-I2), SMM, 
     1                                             INTF, I1)
                     IF (II .GT. NPRNT) GO TO 340
                     IF (KLINE . LT. 53) GO TO 300
                     CALL NEWPG
                     KLINE = 5
  300                WRITE (6,30) II, JF, MCY
                     KLINE = KLINE + 2
                     DO 320 JJ = 2,IMM,5
                        J1 = JJ
                        J2 = J1+4
                        IF (J2 .GT. IMM) J2 = IMM
                        IF (KLINE .LT. 55) GO TO 310
                        CALL NEWPG
                        KLINE = 5
  310                   WRITE (6,330) ((SMM(I,J), I=1,2), J = J1, J2)
                        KLINE = KLINE + 1
  320                CONTINUE 
  330                FORMAT (10F12.2)
C*********** ELIMINATE THE MULTIPLICATION FACTOR WHEN CARRYING THE *****
C*********** LAST CYCLE OVER INTO THE NEXT FLIGHT                  *****
  340                SMM(1,1) = SMM(1,IMM) / FACTOR
                     SMM(2,1) = SMM(2,IMM) / FACTOR
                     RETURN
  350                WRITE (6,30) I1, JF, MCY
                     CALL ERROR (6, II, JF, MCY)
                     RETURN
                  END




                  SUBROUTINE DISTRD (N, M, K, L)
C********* THIS SUBROUTINE USES A REANDOM NUMBER GENERATOR       *******
C********* TO SELECT A FLIGHT NUMBER OR A CYCLE DEPENDING        *******
C********* UPON THE CONTENTS OF TEH INPUT ARRAY.  THE SEQUENCE   *******
C********* OF FLIGHTS OR CYCLES IS ESTABLISHED BY SELECTING      *******
C********* WITHOUT REPLACING A CHOSEN NUMBER.                    *******
C********* SUBROUTINES CALLED - RANIC                            *******
                     DIMENSION N(M)
   10                FORMAT (1X, I3, 19I4)
                     IF (M .EQ. 0) RETURN
                     NS = 0
                     DO 20 I = 1, M
                        NS = NS + N(I)
   20                CONTINUE
                     CALL RANIC (K, R)
                     LR = NS * R + 1
                     MS = 0
                     DO 30 I = 1, M
                        MS = MS + N(I)
                        L = I
                        IF (MS .GE. LR) GO TO 40
   30                CONTINUE
   40                CONTINUE
   50                FORMAT (1X, I12, F12.6, 12I4)
                     RETURN
                  END




                  SUBROUTINE RANIC1 (K0, R)
C******** THIS SUBROUTINE GENERATES A PSEUDO RANDOM NUMBER, WHICH   ***
C******** LIES BETWEEN 0 AND 1 INCLUSIVE, SUCCESSIVE ENTRIES WILL   ***
C******** YIELD A SERIES OF NUMBER WHICH CONFORM TO A UNIFORM       ***
C******** DISRIBUTIO.  THE SERIES REPEATS AFTER APPROXIMATELY       ***
C******** 10**6 NUMBERS
C
C            K - I/O = GENRATING INTEGER ARGUMENT. K MUST BE
C                      INITIALIZED TO ANY NON-ZERO VALUE.  THEREAFTER
C                      K IS MODIFIED BY THE SUBROUTINE AND SHOULD NOT
C                      BE CHANGED BY THE USER.
C            R = O   = THE GENERATED RANDOM NUMBER
                     common/cranic/ icall, k
                     data icall/0/
                     DATA IMAX /2147483647/
                     if (icall.eq.0) then
                        icall=1
                        k=k0
                     end if
                     K = K * 2051
                     IF (K .LT. 0) K = K + IMAX + 1
                     K = MOD(K,4194304)
                     R = FLOAT(K) / 4194304.
                     RETURN
                  END 


                  SUBROUTINE RANIC (K, R)
C******** THIS SUBROUTINE GENERATES A PSEUDO RANDOM NUMBER, WHICH   ***
C******** LIES BETWEEN 0 AND 1 INCLUSIVE, SUCCESSIVE ENTRIES WILL   ***
C******** YIELD A SERIES OF NUMBER WHICH CONFORM TO A UNIFORM       ***
C******** DISRIBUTIO.  THE SERIES REPEATS AFTER APPROXIMATELY       ***
C******** 10**6 NUMBERS
C
C            K - I/O = GENRATING INTEGER ARGUMENT. K MUST BE
C                      INITIALIZED TO ANY NON-ZERO VALUE.  THEREAFTER
C                      K IS MODIFIED BY THE SUBROUTINE AND SHOULD NOT
C                      BE CHANGED BY THE USER.
C            R = O   = THE GENERATED RANDOM NUMBER
                     DATA IMAX /2147483647/
                     K = K * 2051
                     IF (K .LT. 0) K = K + IMAX + 1
                     K = MOD(K,4194304)
                     R = FLOAT(K) / 4194304.
                     RETURN
                  END 


                  SUBROUTINE REED (A, MAT, NSIZE)
C**********  THIS SUBROUTINE READS AND ENTIRE SEQUENCE OF
C**********  MAXIMUM AND MINIMUM STRESSES FOR ONE FLIGHT
C**********  FROM TEMPORARY MASS STORAGE.
C**********  SUBROUTINES CALLED - READMS
                     DIMENSION A(NSIZE)
                     CALL READMS (4, A, NSIZE, MAT)
                     RETURN
                  END


                  SUBROUTINE REDIT1 (SMM, IJJ, IMM, EPS)
C********** THIS SUBROUTINE PERFORMS A REPEAT OF A TYPE 1 EDIT
C********** SUBROUTINES CALLED - NONE
                     DIMENSION SMM(2,*), IJJ(*)
                     KMM = IMM
                     IMM = 1
                     DO 60 JMM = 2,KMM
                        IMM = IMM + 1
                        SMM(1,IMM) = SMM(1,JMM)
                        SMM(2,IMM) = SMM(2,JMM)
C*********** CHECK TO SEE OF MINIMM US REALLY A VALLEY AND NOT AN
C*********** INTERMEDIATE POINT ON THE WAY TO A PEAK
                        IF (SMM(1,IMM-1) .NE. SMM(2,IMM-1)) GO TO 10
                        IF (SMM(1,IMM-1) .GT. SMM(1,IMM)) SMM(1,IMM-1)
     1                                        = SMM(1,IMM)
                        SMM(2,IMM-1) = SMM(2,IMM)
                        IMM = IMM - 1
                        IJJ(2) = IJJ(2) + 1
                        GO TO 60
   10                   IF (SMM(1,IMM) .LT. SMM(2,IMM-1)) GO TO 20
                        IJJ(1) = IJJ(1) + 1
                        SMM(2,IMM-1) = SMM(2,IMM)
                        IMM = IMM - 1
   20                   IF (ABS(SMM(1,IMM) - SMM(2,IMM)) .LT. EPS) 
     1                     GO TO 30
                        IF (SMM(1,IMM) - SMM(2,IMM)) 60, 30, 50
C********** DROP OUT FICTITIOUS LOAD LEVELS
   30                   SMM(1,IMM) = SMM(2,IMM)
                        IF (SMM(2,IMM) .LT. SMM(2,IMM-1)) GO TO 60
                        SMM(2,IMM-1) = SMM(2,IMM)
   40                   IMM = IMM - 1
                        IJJ(3) = IJJ(3) + 1
                        GO TO 60
C********** VALLEY GREATER THAN PEAK IN THIS SEGMENT
C********** INTERCHANGE VALLEY AND PEAK
   50                   TEMP = SMM(1,IMM)
                        SMM(1,IMM) = SMM(2,IMM)
                        SMM(2,IMM) = TEMP
   60                CONTINUE
                     RETURN
                  END



                  SUBROUTINE  SPSUM  (SMM, I1, I3, PMAX, VP, RAN, PI, 
     1                                RR, NNRAN, PR, NNPI)
C************* THIS SUBROUTINE GENERATES THE TABLES REQUIRED FOR
C************* THE SPECTRUM SUMMATION PRINT OUT OF RANGE VS.
C************* VALLEY/PEAK RATIO AND PEAK VS. VALLEY/PEAK RATIO
C********  SUBROUTINES CALLED - NONE
                     COMMON ISKIP(26), NPI, NRAN, IVP
                     DIMENSION SMM(2,*), RAN(*), VP(*), PI(*)
                     INTEGER RR(NNRAN,1), PR(NNPI,1)
            write(12,*) 'RR*',((RR(i8,j), i8=1,nnran), j=1,2)
                     DO 80 KMM = I1,I3
                        IF (SMM(2,KMM) .GT. PMAX) PMAX = SMM(2,KMM)
                        RANGE = SMM(2,KMM) - SMM(1,KMM)
                        R = SMM(1,KMM) / SMM(2,KMM)
C************* TEST RATIO (MIN.MAX) AGAINST INPUT VALLEY/PEAK RATIO
   10                   DO 20 K = 1,IVP
                           IF (VP(K) .GE. R) GO TO 30
   20                   CONTINUE
                        K = IVP + 1
C************* TEST RANGE AGAINST INPUT RANGE INTERVALS
   30                   DO 40 L = 1,NRAN
                           IF (RAN(L) .GE. RANGE ) GO TO 50
   40                   CONTINUE
                        L = NRAN + 1
   50                   RR(L,K) = RR(L,K) + 1
                        write(14,*) L,K
C************* TEST MAXIMUM STRESS AGAINST INPUT PEAK INTERVALS
                        DO 60 M = 1, NPI
                           IF (PI(M) .GE. SMM(2,KMM)) GO TO 70
   60                   CONTINUE
                        M = NPI + 1
   70                   PR(M,K) = PR(M,K) + 1
   80                CONTINUE
                     RETURN
                  END




         SUBROUTINE PRNTSS (RR, NNRAN, PR, NNPI, IRR, NRMAX, 
     1                      IIVP, RAN, VP, PI, IS, NFF, ISS, 
     2                      II, NTF, NFT, PMAX, MFT)
C********* THIS SUBROUTINE PRINTS OUT THE TWO SPECTRUM SUMMATION   *****
C********* TABLES, RANGE VERSUS VALLEY/PEAK RATIO, AND PEAK VERSUS *****
C********* PEAKS AND THEIR CORRESPONDING FLIGHT NUMBER.            *****
C*******SUBROUTINES CALLED - NEW PG                                *****
         COMMON ISKIP(26), NPI, NRAN, IVP, SKIP(8), IPSS, 
     1                    SSKIP(5), MAXHP
         INTEGER RR(NNRAN,1), PR (NNPI,1)
         DIMENSION RAN(*), VP(*), PI(*), IRR(NRMAX,*)
         DIMENSION NFF(2,1), TITLE1(6), TITLE2(6), 
     1             TITLE3(2), TITLE4(4)
         DIMENSION FORM(4), FMAT(6), ISS(*), PMAX(*), MFT(*)
         DATA TITLE1/4HFLIG,4HHT  ,4HNUMB,4HER  ,4H NUM,4HBER /
         DATA TITLE2/4H TYP,4HE   ,4HFLIG,4HHTS ,4H CYC,4HLES /
         DATA TITLE3/4HTOTA,4HL   /
         DATA TITLE4/4HPEAK,4H   F,4HLIGH,4HT   /
   10    FORMAT (30X, 'SPECTRUM SUMMATION FOR A TOTAL OF', 
     1           I5, ' FLIGHTS AND', I7, ' CYCLES')
   20    FORMAT (2X, 'VALLEY.PEAK RATIO ', 6X, 6F16.2)
   30    FORMAT (14X, 'RANGE ')
   40    FORMAT (9X, 2F7.0, 7(2I7,2X))
   50    FORMAT (1X, 'BELOW OR EQUAL ', F7.0, 7(2I7,2X))
   60    FORMAT (9X, ' ABOVE ', F7.0, 7(2I7,2X))
   70    FORMAT (15X, 'PEAK ')

C********** PRINT THE RANGE VERSUS VALLEY/PEAK RATIO********************
                     IVP3 = IIVP - 1
            write(12,*) 'RR ',((RR(j,i), j=1,nnran),i=1,iivp)
                     DO 80 I = 1,NNRAN
                        DO 80 J = 1,IVP3
   80                RR(I,IIVP) = RR(I,IIVP) + RR(I,J)
                     DO 90 J = 1,IIVP
   90                IRR (NNRAN,J) = RR(1,J)
                     DO 100 I = 1,NRAN
                        DO 100 J = 1,IIVP
  100                IRR(NNRAN -I,J) = IRR(NNRAN-I+1,J) + RR(I+1,J)
                     DO 110 I = 1,NRAN
                        DO 110 J = 1,IIVP
  110                IRR(I+1,J) = IRR(I,J) - RR(I,J)
                     LINE = 50
                     IVP1 = 1
  120                IVP2 = IVP1 + 5
                     IF (IVP2 .GT. IIVP) IVP2 = IIVP
                     IVP3 = IVP2
                     IF (IVP2 .GT. IVP) IVP3 = IVP
                     IF (LINE .LT. 45) GO TO 130
                     LINE = 0
                     CALL NEWPG
                     WRITE (6,10) II, IRR(1,IIVP)
  130                WRITE (6,20) (VP(J), J=IVP1,IVP3)
                     IF (IVP2 .NE. IIVP) GO TO 140
                     ITAB = IVP2 -IVP1 + 1
                     FORM(2) = FMAT(ITAB)
                     !WRITE (6,FORM) TITLE3
                     WRITE (6,'(2A4)') TITLE3
  140                WRITE (6,30)
  150                CONTINUE
                     WRITE (6,50) RAN(1), (RR(1,J),IRR(1,J), 
     1                            J = IVP1,IVP2)
                     LINE=LINE+4
                     write(12,*) 'iivp, ivp1, ivp2 ',iivp, ivp1, ivp2
                     DO I=2,NRAN
                        WRITE (6,40) RAN(I-1),RAN(I), 
     1                  (RR(I,J),IRR(I,J), J = IVP1,IVP2)
                        LINE=LINE+1
                        IF ((LINE.LT.48).OR.(I.EQ.NRAN)) CYCLE
                        LINE=0
                        CALL NEWPG
                        WRITE (6,10) II,IRR(1,IIVP)
                        WRITE (6,20) (VP(J),J=IVP1,IVP3)
                        IF(IVP2.EQ.IIVP) WRITE (6,'(2A4)') TITLE3
                        WRITE (6,30)
                     END DO
                     WRITE (6,60) RAN(NRAN), (RR(NNRAN,J),IRR(NNRAN,J), 
     1                            J = IVP1,IVP2)
C********** PRINT THE PEAK VERUS VALLEY/PEAK RATIO *********************
                     IVP1 = IVP2 + 1
                     IF (IVP1 .LE. IIVP) GO TO 120
                     IVP3 = IIVP -1
                     DO 160 I = 1,NNPI
                        DO 160 J = 1,IVP3
  160                PR(I,IIVP) = PR(1,IIVP) + PR(I,J)
                     DO 170 J = 1,IIVP
  170                IRR(NNPI,J) = PR(I,J)
                     DO 180 I = 1,NPI
                        DO 180 J = 1,IIVP
  180                IRR(NNPI-I,J) = IRR(NNPI-I+1,J) + PR(I+1,J)
                     DO 190 I = 1,NPI
                        DO 190 J = 1,IIVP
  190                IRR(I+1,J) = IRR(I,J) - PR(I,J)   ! ? PSV RR?
                     IF (LINE .LT. 35) GO TO 200
                     LINE = 0
                     CALL NEWPG
                     WRITE (6,10) II, IRR(1,IIVP)
  200                IVP1 = 1
  210                IVP2 = IVP1 + 5
                     IF (IVP2 .GT. IIVP) IVP2 = IIVP
                     IVP3 = IVP2
                     IF (IVP2 .GT. IVP) IVP3 = IVP
                     IF (LINE .LT. 45) GO TO 220
                     LINE = 0
                     CALL NEWPG
                     WRITE (6,10) II, IRR(1,IIVP)
  220                WRITE (6,20) (VP(J), J=IVP1, IVP3)
                     IF (IVP2 .NE. IIVP) GO TO 230
                     ITAB = IVP2 - IVP1 + 1
                     FORM(2) = FMAT(ITAB)
                     !WRITE (6,FORM) TITLE3
                     WRITE (6,'(2A4)') TITLE3
  230                WRITE (6,70)
                     WRITE (6,50) PI(1),(PR(1,J),IRR(1,J), J=IVP1,IVP2)
                     LINE = LINE + 4
                     DO 250 I = 2,NPI
                        WRITE (6,40) PI(I-1), PI(I), (PR(I,J), IRR(I,J), 
     1                               J=IVP1, IVP2)
                        LINE = LINE + 1
                        IF ((LINE .LT. 48) .OR. (I .EQ. NPI)) GO TO 250
                        LINE = 0
                        CALL NEWPG
                        WRITE (6,10) II, IRR(1,IIVP)
                        WRITE (6,20) (VP(J), J=IVP1, IVP3)
                        IF (IVP2 .NE. IIVP) GO TO 240
                        !WRITE (6,FORM) TITLE3
                        WRITE (6,'(2A4)') TITLE3
  240                   WRITE (6,70)
  250                CONTINUE
                     WRITE (6,60) PI(NPI), (PR(NNPI,J), IRR(NNPI,J), 
     1                            J=IVP1, IVP2)
                     IVP1 = IVP2 + 1
                     IF (IVP1 .LE. IIVP) GO TO 210
                     JLINE = 50
                     DO 290 J = 1,NFT,4
                        M1 = J
                        M2 = J + 3
                        IF (M2 .GT. NFT) M2 = NFT
                        M3 = M2 - M1 + 1
                        IF (JLINE .LT. 45) GO TO 270
                        CALL NEWPG
                        WRITE (6,10) II, IRR(1,IIVP)
                        WRITE (6,260) ((TITLE1(K), K = 1,6), KK = 1,M3)
                        WRITE (6,260) ((TITLE2(K), K = 1,6), KK = 1,M3)
  260                   FORMAT (4(6A4,5X))
                        JLINE = 0
  270                   WRITE (6,280) (K,(NFF(I,K), I = 1,2), K = M1,M2)
  280                   FORMAT (4(I5, 2I9, 6X))
                        JLINE = JLINE + 1
  290                CONTINUE
C************** PRINT THE HIGHEST PEAKS AND THEIR CORRESPONDING  *******
C************** FLIGHT NUMBER                                    *******
  300                IF (MAXHP .EQ. 0) GO TO 360
                     M3 = 6
                     IF (M3 .GT. MAXHP) M3 = MAXHP
                     IF (JLINE .LT. 45) GO TO 310
                     CALL NEWPG
                     JLINE = 0
  310                WRITE (6,320) MAXHP
  320                FORMAT (//, 10X, 'THE', I4, ' HIGHEST PEAKS')
                     WRITE (6,330) ((TITLE4(K), K = 1,4), KK = 1,M3)
  330                FORMAT ( 4X, 6(4A4,5X))
                     JLINE = JLINE + 3
                     DO 350 J = 1,MAXHP,6
                        M1 = J
                        M2 = J + 5
                        IF (M2 .GT. MAXHP) M2 = MAXHP
                        WRITE (6,340) (PMAX(K), MFT(K), K = M1,M2)
  340                   FORMAT (6(F8.2,I8,5X))
                        JLINE = JLINE + 1
  350                CONTINUE
  360                IF (IPSS .EQ. IS) GO TO 390
                     IS = IS + 1
                     NPSS = ISS(IS)
                     KLINE = 60
                     DO 371 J = 1,NNRAN
  370                RR(J,IIVP) = 0
  371                CONTINUE
                     DO 381 J = 1,NNPI
  380                PR(J,IIVP) = 0
  381                CONTINUE
  390                RETURN
                  END



                  SUBROUTINE ERROR (I, J, K, L)
C***********  THIS SUBROUTINE PRINTS OUT THE ERROR MESSAGES  ***********
C***********  SUBROUTINES CALLED - NONE
                     COMMON NPG, TITLE(20), NSIZE, LEFT, IERR, IRS, 
     1                      IUIL, NPI, NRAN, IVP, IFI, NXY, CLIP, CLIV, 
     2                      FACTOR, ELIMP, ELIMV
                     IEER = IERR + 1
                     WRITE (6,10) IERR, I, J, K, L
   10                FORMAT ('ERROR NUMBER', I6, ' IS TYPE', I4, 
     1                        ' AND INVOLVES', 3I7)
                     IF (IERR .GT. 900) STOP 701
                     RETURN
                  END




                  SUBROUTINE RITE (A, MAT, NSIZE)
C********** THIS SUBROUTINE WRITE AND ENTIRE SEQUENCE    ***************
C********** OF MAXIMUM AND MINIMUM STRESSES FOR ONE      ***************
C********** FLIGHT ONTO TEMPORARY STORAGE.               ***************
C****** SUBROUTINES CALLED - WRITEMS                     ***************
                     DIMENSION A(NSIZE)
                     CALL WRITEMS (4, A, NSIZE, MAT, -1)
                     RETURN
                  END

                  SUBROUTINE WTAPE (LREC, MCY, SMM, NTF, I1)
C********** THIS SUBROUTINE WRITES THE FLIGHT NUMBER,    ***************
C********** THE NUMBER OF CYCLES IN THE FLIGHT, AND THE  ***************
C********** MAXIMUM AND MINIMUM STRESSES FOR THE FLIGHT  ***************
C********** ON THE OUTPUT TAPE.                          ***************
                     COMMON ISKIP(29), IFI, ISK(9), IAFS
                     DIMENSION LREC(NTF), MCY(NTF), SMM(2,1)
                     IF (IAFS .EQ. 0) GO TO 30
C**********   WIRE IF ALTERNATE FLIGHT SEQUENCE IS SPECIFIED ***********
                     WRITE (IFI) NTF
                     DO 20 I = 1, NTF
                        MAT = LREC(I)
                        ICY = MCY(LREC(I))
                        CALL REED (SMM, MAT, ICY)
                        ICY = ICY / 2
                        WRITE (IFI) I, ICY,((SMM(M,J), M = 1,2), J = 1,
     1                               ICY)
C***                    WRITE (6,1001) I, ICY,((SMM(M,J), M = 1,2), 
C***                                   J = 1, ICY)
   10                   FORMAT ('WTAPE', 2I5, 10X, 8F12.2, / (10F13.2))
   20                CONTINUE
                     GO TO 40
   30                IFL = LREC(1)
                     IF (IFL .EQ. 1) WRITE (IFI) NTF
                     ICY = MCY(1)
                     JCY = ICY -I1 + 1
                     WRITE (IFI) IFL, JCY, ((SMM(M,J), M = 1,2), J = I1,
     1                               ICY)
C***                 WRITE (6,1001) IFL, JCY,((SMM(M,J), M = 1,2), 
C***                                   J = I1, ICY)
   40             RETURN
                END  

           SUBROUTINE OPENMS(IU, NARR, NFT1, K)
           DIMENSION NARR(1)
           OPEN( IU, FILE='tape.dat', ACCESS='DIRECT', RECL=40000,
     & 			FORM='UNFORMATTED', ERR=30 )
           RETURN
  30       write(6,*) 'error open'
           RETURN
           end

           SUBROUTINE READMS(IU, ARR, NA, MAT)
           DIMENSION ARR(1)
           READ( IU, REC=MAT, ERR=50 ) ARR(1:NA)
           RETURN
  50       write(6,*) 'error read'
           RETURN
           end


           SUBROUTINE WRITEMS(IU, ARR, NA, MAT, IR)
           DIMENSION ARR(1)
           WRITE( IU, REC=MAT, ERR=50 ) ARR(1:NA)
           RETURN
  50       write(6,*) 'error write'
           RETURN
           end












































