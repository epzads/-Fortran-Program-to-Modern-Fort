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
      !WRITE (11,*) 'VP ',(VP(I), I = 1, IVP)
      IF (NXY .NE. 0) THEN
!***     READ IN AND PRINT THE VALLEY/PEAK RATIO VS RANGE CURVE ****
         READ (5,*) ((XY(I,J), I = 1,2), J = 1, NXY)
         WRITE (6,160) ((XY(I,J), I = 1,2), J = 1, NXY)
         WRITE (11,*) 'XY ',((XY(I,J), I = 1,2), J = 1, NXY)
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
      !write(22,*)'MMN,MRR,MPR,MIRR,MPMAX,MCY,MFF,MHPEAK,MIDP,MJTN' &
      ! , MMN,MRR,MPR,MIRR,MPMAX,MCY,MFF,MHPEAK,MIDP,MJTN
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
         !WRITE(11,*)'I,NS(I) ',I, NS(I)
          !close(11)
          !open(unit=11, access='APPEND')
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
      !rewind iuil ! PSV
      DO I = 1,NSS
!***     READ IN NUMBER OF GROUPS, PEAK VALUE, VALLEY      ********
!***     NUMBER OF CYCLES SEQUENCE FROM UTILITY TAPE       ********
         READ (IUIL) KMMN, (AMAX(K), AMIN(K), ACY(K), K = 1, KMMN)
         !write(15,*) 'idft ',idft, KMMN,(AMAX(K), AMIN(K), ACY(K), K = 1, KMMN)
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
         WRITE(15,*) 'KF1 ', KF1
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
      !write(11,*) 'MMN,NSS ',MMN,NSS
      !WRITE(17,*)'bef 110 ID, NCY',idft, (ncy(I17),I17=1,NSS)
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
      !WRITE(17,*)'ID, NCY',idft, (ncy(I17),I17=1,NSS)
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
            write(11,*) 'I, J, K1, K2 ',I, J, K1, K2
            DO L = K1,K2
               K = K + 1
               SMAX(K) = SMAX(L)
               SMIN(K) = SMIN(L)
               BCY(K) = BCY(L)
               NCY(K) = NCY(L)
            END DO
            IS2(I) = IS2(I) + K2 - K1 + 1
         END DO
         !write(17,*) 'ID, I, NCY',idft, I,NCY(I)
      END DO  
      I = NSS1
  220 NS2 = I -1
      MMN = K
      DO I = 2,NS2
         IS2(I) = IS2(I) + IS2(I-1)
      END DO
      !WRITE(11,*)'NSS,IS2 ',NSS, (IS2(I17),I17=1,NSS)
      !WRITE(17,*)'ID, NCY',idft, (ncy(I17),I17=1,NSS)
      !close(11)
      !open(unit=11, access='APPEND')
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
         !write(12,*) 'RRC',((RR(i8,j8), i8=1,nnran),j8=1,2)
         !WRITE(11,*)'KCY, II, NPSS ',KCY, II, NPSS
         !close(11)
         !open(unit=11, access='APPEND')
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
         !write(11,*)'K1,K2 ',K1,K2
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
                  IF (II.EQ.1) THEN
                   write(24,*) IMM, I1
                  END IF
      KMM = IMM
      IMM = I1 - 1
      DO JMM = I1,KMM
         IMM = IMM + 1
         SMM(1,IMM) = SMM(1,JMM)
         SMM(2,IMM) = SMM(2,JMM)
!***     ELIMINATE RANGES BELOW AN INPUT RANGE VERSUS R CURVE
         RANGE = SMM(2,IMM) - SMM(1,IMM)
         R = SMM(1,IMM) / SMM(2,IMM)
                  IF (II.EQ.1) write(24,*) IMM, K, R
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
            IF (II.EQ.1) write(24,*) K,R,RQ
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

      !write(12,*) 'RR*',((RR(i8,j), i8=1,nnran), j=1,2)
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
         !write(14,*) L,K
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

      INTEGER :: TITLE1(6), TITLE2(6), TITLE3(2), TITLE4(4)
      INTEGER :: FORM(4), FMAT(6), I, J, IVP1, IVP2, IVP3
      INTEGER :: LINE, JLINE, ITAB, K, KK, M1, M2, M3


      DATA TITLE1/4HFLIG,4HHT  ,4HNUMB,4HER  ,4H NUM,4HBER /
      DATA TITLE2/4H TYP,4HE   ,4HFLIG,4HHTS ,4H CYC,4HLES /
      DATA TITLE3/4HTOTA,4HL   /
      DATA TITLE4/4HPEAK,4H   F,4HLIGH,4HT   /

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

      write(28,*) 'IRR ',((IRR(j,i), j=1,nnran),i=1,iivp)

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
         write(26,*) '1', II, IIVP, IRR(1,IIVP)
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
      !write(12,*) 'iivp, ivp1, ivp2 ',iivp, ivp1, ivp2
      DO I=2,NRAN
         WRITE(6,40)RAN(I-1),RAN(I),(RR(I,J),IRR(I,J),J=IVP1,IVP2)
         LINE=LINE+1
         IF ((LINE.LT.48).OR.(I.EQ.NRAN)) CYCLE
         LINE=0
         CALL NEWPG
         WRITE (6,10) II,IRR(1,IIVP)
         write(26,*) '2'
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
         write(26,*) '3'
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
         write(26,*) '4'
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
         write(26,*) '5'
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
         write(26,*) '6'
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
