         SUBROUTINE INF1F2 (NST, NFT, NS, IF1, IF2, N, NF1ST, NF2ST, NTF, NF, &
                            PI, RAN, VP, XY, IRAN, IPI, KVP, ISS, NFRS)
!******  THIS SUBROUTINE READS AND PRINTS THE REMAINDER OF THE  *****
!******  INPUT DATA.  IT SETS UP THE CORE STORAGE REQUIRED FOR  *****
!******  THE CALLS TO SUBROUTINES INMMN, GENFL, AND GENAFS.     *****
!******  SUBROUTINES CALLED - ERROR, GENAFS, GENFL, INMMN,      *****
!******                       NEWPG, OPENMS, WTAPE              *****
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
!******     READ IN AND PRINT ALL PEAK LEVELS(IPI) 0
            READ (5,*) (PI(I), I = 1, NPI)
         ELSE
!******     READ IN AND PRINT FIRST AND LAST PEAK LEVELS.
!******     LET THE PROGRAM COMPUTE EVENLY SPACED VALUES.
            READ (5,*) PI(1), PI(NPI)
            ND = NPI - 1
            DEL = (PI(NPI) - PI(1)) / ND
            DO i = 2, ND
               PI(I) = PI(I-1) + DEL
            END DO
         END IF
         WRITE (6,60) (PI(I), I = 1, NPI)

         IF (IRAN .GT. 0) GO TO 80
!******  READ IN AND PRINT ALL RANGE LEVELS (IRAN) 0)
   70    READ (5,*) (RAN(I), I = 1, NRAN)
         GO TO 100
!******  READ IN AND PRINT FIRST AND LAST RANGE LEVELS.
!******  LET THE PROGRAM COMPUTE EVENLY SPACED VALUES.
   80    READ (5,*) RAN(1), RAN(NRAN)
         ND = NRAN - 1
         DEL = (RAN(NRAN) - RAN (1)) / ND
         DO 90 I = 2,ND
   90    RAN(I) = RAN(I-1) + DEL
  100    WRITE (6,110) (RAN(I), I = 1, NRAN)
         IF (KVP .GT. 0) GO TO 120
!******  READ IN AND PRINT ALL TEH VALLEY/PEAK RETIOS (KVP) 0)
         READ (5,*) (VP(I), I = 1, IVP)
         GO TO 140
!******  READ IN AND PRING FIRT AND LAST VALLEY/PEAK RATIOS.
!******  LET THE PROGRAM COMPUTE EVELY SPACED VALUES.
  120    READ (5,*) VP(1), VP(IVP)
         ND = IVP - 1
         DEL = (VP(IVP) - VP(1)) / ND
         DO 130 i = 2, ND
  130    VP(I) = VP(I-1) + DEL
  140    WRITE (6,150) (VP(I), I = 1, IVP)
         WRITE (11,*) 'VP ',(VP(I), I = 1, IVP)
         IF (NXY .EQ. 0) GO TO 170
!******** READ IN AND PRINT THE VALLEY/PEAK RATIO VS RANGE CURVE ****
         READ (5,*) ((XY(I,J), I = 1,2), J = 1, NXY)
         WRITE (6,160) ((XY(I,J), I = 1,2), J = 1, NXY)
         WRITE (11,*) 'XY ',((XY(I,J), I = 1,2), J = 1, NXY)
  170    IF (NPSS .EQ. 0) GO TO 190
!******  READ IN AND PRINT NUMBER OF FLIGHTS                *******
!******  AFTER WHICH A SPECTRUM SUMMATION IS TO BE PRINTED  *******
         READ (5,*) (ISS(I), I = 1, NPSS)
         WRITE (6,180) (ISS(I), I = 1, NPSS)
  190    REWIND 3
!******  READ IN AND PRINT A6PA REFERENCDE RUN, CASE NUMBER,  ******
!******  AND SEGMENTS FROM TAPE UNIT 3.                       ******
         IF (LINE .LT. 55) GO TO 200
         CALL NEWPG
         LINE = 4
  200    IS2 = 0
         JSS = 0
         NST = 0
         WRITE (6,210)
         LINE = LINE + 2
         DO 260 I = 1, NFT
            IS1 = IS2 + 1
            NST = NST + NS(I)
            IF (NST .EQ. JSS) GO TO 230
            IF (NST .LT. JSS) GO TO 240
  220       READ (3) IRR, ICASE, ISEG
            ISP = JSS
            JSS = JSS + ISEG
            IF (NST .LT. JSS) GO TO 240
  230       IS2 = ISEG
            WRITE (6,250) I, IRR, ICASE, IS1, IS2
            LINE = LINE + 1
            IF (NST .NE. JSS) GO TO 220
            IS2 = 0
            GO TO 260
  240       IS2 = NST - ISP
            WRITE (6,250) I, IRR, ICASE, IS1, IS2
            LINE = LINE + 1
  260    CONTINUE
         NF1ST = 0
         NF2ST = 0
         IS1 = 1
         IS2 = 0
!**********  READ IN AND PRINT F1 AND F2 SEGMENTS              ******
         IF (LINE .LT. 50) GO TO 270
         CALL NEWPG
         LINE = 4
  270    DO 320 I = 1, NFT
            IS2 = IS2 + NS(I)
            write(6,*) ' 1932'
            READ (5,*) (IF1(J), J = IS1, IS2)
            READ (5,*) (IF2(J), J = IS1, IS2)
            WRITE (6,280) I, NF(I), NS(I)
            WRITE (6,290) (IF1(J), J = IS1, IS2)
            WRITE (6,300) (IF2(J), J = IS1, IS2)
            MAX = 0
            MBX = 0
            DO 310, J = IS1, IS2
               IF (IF1(J) .GT. MAX) MAX = IF1(J)
               IF (IF2(J) .GT. MBX) MBX = IF2(J)
  310       CONTINUE
            NF1ST = NF1ST + MAX
            NF2ST = NF2ST + MBX
            LINE = LINE + 4
            IF (LINE.LT.51) GO TO 320
            LINE = 4
            CALL NEWPG
  320    IS1 = IS1 + NS(I)

         IF (IFRS .EQ. 0) GO TO 360
!*********  READ IN USER SPECIFIED FLIGHT SEQUENCE            *******
         READ (5,*) ((NFRS(I,J), I = 1,2), J = 1, IFRS)
         WRITE (6,350) ((NFRS(I,J), I = 1,2), J = 1, IFRS)
!******* CALCULATE THE STARTING POINT WITHIN N ARRAY        *******
!******* (WHICH IS REALLY THE A ARRAY) FOR EACH ARRAY       *******
!******* USED IN THE SUBROUTINE INMNN.                      *******
  360    M3 = (LEFT - NST) / 3
         MAX = 1
         MIN = MAX + M3
         MCY = MIN + M3
         MS2 = MCY + M3
         CALL INMMN (NFT, NS, IF1, IF2, NST, N(MAX), N(MIN),       &
                     N(MCY), NF1ST, NF2ST, MMN, M3, NS1, NS2, NTF, &
                     NF, N(MS2))
         WRITE (11,*) 'LEFT, NST, M3',LEFT, NST, M3
         DO 370 I =1, MMN
            N(MMN + I) = N(MIN + I - 1)
  370    N(2 * MMN + I) = N(MCY + I -1)
         WRITE (11,*) 'MCY+MMN',MCY+MMN
         !close(11)
         !open(unit=11, access='APPEND')
!******* CALCULATE THE STARTING POINT WITHIN THE N ARRAY    *******
!******* (WHICH IS REALLY THE A ARRAY) FOR EACH ARRAY       *******
!******* USE IN THE SUBROUTINE GENFL.                       *******
         MHPEAK = (2 + IRS) * MMN + 1
         MIDP = MHPEAK  + MAXHP
         MPMAX = MIDP + MAXHP
         MCY = MPMAX
         MINDEX = MCY
         MJTN = MINDEX
         MFF = MJTN
         IF (IAFS .EQ. 0) GO TO 380    ! ?PSV
         MCY = MPMAX + NTF
         MINDEX = MCY + NTF
         MJTN = MINDEX + (NTF + 1)
         MFF = MJTN + NTF
         CALL OPENMS (4, N(MINDEX), (NTF+1), 0)
  380    MRR = MFF + 2 * NFT
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
          write(22,*)'MMN,MRR,MPR,MIRR,MPMAX,MCY,MFF,MHPEAK,MIDP,MJTN' &
           , MMN,MRR,MPR,MIRR,MPMAX,MCY,MFF,MHPEAK,MIDP,MJTN
         IF ((IFI .NE. 0) .AND. (IAFS .NE. 0)) REWIND IFI
!******* CALCULATE THE FLIGHT SEQENCE                       *******
         CALL GENFL (NF, NFT, NTF, NS, NST, N(1), N(MMN+1),        &
                     N(2*MMN+1), MMN, IF2, N(MMS), MAXSS, PI, RAN, &
                     VP, XY, N(MRR), NNRAN, N(MPR), NNPI, N(MIRR), &
                     NRMAX, ISS, N(MPMAX), N(MCY), N(MFF), NFRS,   &
                     N(MHPEAK), N(MIDP), N(MJTN))
         
!********* CALCULATE THE STARTING POINT WITHIN THE N ARRAY    *******
!********* (WHICH IS REALLY THE A ARRAY) FOR EACH ARRAY       *******
!********* USE IN THE SUBROUTINE GENAFAS.                     *******
         MREC = MIRR + (IVP + 2) * NRMAX
         MAFS = MREC + NTF
         MFLG = MAFS + NTF
         MMS = MFLG + NTF
         LEFT = JLEFT
         LEFT = LEFT - MMS
         WRITE(11,*)'iafs, LEFT ',iafs, LEFT
         close(11)
         open(unit=11,access='APPEND')
         IF (LEFT .LT. 0) CALL ERROR (1,LEFT,NSIZE,MMS)
         IF (IAFS .EQ. 0) RETURN
!*********  GENERATE THE ALTERNATE FLIGHT SEQUENCE            *******
         CALL GENAFS (N(MCY), N(MPMAX), N(MREC), N(MAFS), N(MFLG),    &
                      NTF, N(MMS), N(MRR), NNRAN, N(MPR), NNPI,       &
                      N(MIRR), NRMAX, PI, RAN, VP, N(MFF), ISS, NFT,  &
                      N(MHPEAK), N(MIDP), N(MJTN))
         IF (IFI .EQ. 0) RETURN
!********* SAVE THE ALTERNATE FLIGHT SEQUENCE ON MAGNETIC TAPE ******
         MMS = MFLG
         CALL WTAPE (N(MREC), N(MCY), N(MMS), NTF, NTF)
         RETURN
   10    FORMAT ('INF1F2',13I7)
   20    FORMAT ('INF1F2',10F12.2)
   60    FORMAT (5X, 'PEAK LEVELS FOR SPECTRUM SUMATION' / (10F12.0))
  110    FORMAT (5X, 'INPUT RANGE LEVELS FOR SPECTRUM SUMMATION' /   &
         (10F12.0))
  150    FORMAT (5X, 'INPUT VALLEY/PEAK RATIOS FOR SPECTRUM '        &
                 'SUMMATION'/ 5X, 18F7.3)
  160    FORMAT (5X, 'INPUT VALLEY/PEAK RATIO VS RANGE CURVE FOR'    &
                     'RANGE TRUNCATION'/ 8X, 5(F7.3, F10.0, 4X) /    &
                      8X, 5(F7.3, F10.0, 4X))
  180    FORMAT (5X, 'INPUT FLIGHT NUMBERS FOR SPECTRUM SUMMATION '  &
                     'PRINT'/ (5X, 20I6))
  210    FORMAT (4X,'FLIGHT',6X,'RR',5X,'CASE',5X,'A6PA SEGMENTS'/)
  250    FORMAT (I8, 4X, I6, 2X, I6, 6X, 2I6)
  280    FORMAT (4X, 'FLIGHT TYPE', I5, ' HAS', I6, ' FLIGHTS AND',  &
                 I5, ' A6PA SEGMENTS')
  290    FORMAT (8X, 'F1 SEGMENTS', 35I3)
  300    FORMAT (8X, 'F2 SEGMENTS', 35I3)
  330    FORMAT (1X, 5F12.2)
  340    FORMAT (1X, 'IN F1 F2', 20I6)
  350    FORMAT (4X, 'INPUT FLIGHT SEQUENCE' / (1X, 8(14I6, 5X)))
         END
