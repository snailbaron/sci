***************************************************************************   
*
*	VOICIN Version 52
*
***************************************************************************
*
*        Voicing Detection (VOICIN) makes voicing decisions for each half
*   frame of input speech.  Tentative voicing decisions are made two frames
*   in the future (2F) for each half frame.  These decisions are carried
*   through one frame in the future (1F) to the present (P) frame where
*   they are examined and smoothed, resulting in the final voicing
*   decisions for each half frame. 
*        The voicing parameter (signal measurement) column vector (VALUE)
*   is based on a rectangular window of speech samples determined by the
*   window placement algorithm.  The voicing parameter vector contains the
*   AMDF windowed maximum-to-minimum ratio, the zero crossing rate, energy
*   measures, reflection coefficients, and prediction gains.  The voicing
*   window is placed to avoid contamination of the voicing parameter vector
*   with speech onsets. 
*        The input signal is then classified as unvoiced (including
*   silence) or voiced.  This decision is made by a linear discriminant
*   function consisting of a dot product of the voicing decision
*   coefficient (VDC) row vector with the measurement column vector
*   (VALUE).  The VDC vector is 2-dimensional, each row vector is optimized
*   for a particular signal-to-noise ratio (SNR).  So, before the dot
*   product is performed, the SNR is estimated to select the appropriate
*   VDC vector. 
*        The smoothing algorithm is a modified median smoother.  The
*   voicing discriminant function is used by the smoother to determine how
*   strongly voiced or unvoiced a signal is.  The smoothing is further
*   modified if a speech onset and a voicing decision transition occur
*   within one half frame.  In this case, the voicing decision transition
*   is extended to the speech onset.  For transmission purposes, there are
*   constraints on the duration and transition of voicing decisions.  The
*   smoother takes these constraints into account. 
*        Finally, the energy estimates are updated along with the dither
*   threshold used to calculate the zero crossing rate (ZC).
*
*  Inputs:
*   VWIN      - Voicing window limits
*   INBUF     - Input speech buffer
*   LPBUF     - Low-pass filtered speech buffer
*   BUFLIM    - INBUF and LPBUF limits
*   HALF      - Present analysis half frame number
*   MINAMD    - Minimum value of the AMDF
*   MAXAMD    - Maximum value of the AMDF
*   MINTAU    - Pointer to the lag of the minimum AMDF value
*   IVRC(2)   - Inverse filter's RC's
*   OBOUND    - Onset boundary descriptions
*   AF        - The analysis frame number
*  Output:
*   VOIBUF(2,0:AF) - Buffer of voicing decisions
*  Internal:
*   QS        - Ratio of preemphasized to full-band energies
*   RC1       - First reflection coefficient
*   AR_B      - Product of the causal forward and reverse pitch prediction gains
*   AR_F      - Product of the noncausal forward and rev. pitch prediction gains
*   ZC        - Zero crossing rate
*   DITHER    - Zero crossing threshold level
*   MAXMIN    - AMDF's 1 octave windowed maximum-to-minimum ratio
*   MINPTR    - Location  of minimum AMDF value
*   NVDC      - Number of elements in each VDC vector
*   NVDCL     - Number of VDC vectors
*   VDCL      - SNR values corresponding to the set of VDC's
*   VDC       - 2-D voicing decision coefficient vector
*   VALUE(9)  - Voicing Parameters
*   VOICE(2,3)- History of LDA results
*   LBE       - Ratio of low-band instantaneous to average energies
*   FBE       - Ratio of full-band instantaneous to average energies
*   LBVE      - Low band voiced energy
*   LBUE      - Low band unvoiced energy
*   FBVE      - Full band voiced energy
*   FBUE      - Full band unvoiced energy
*   OFBUE     - Previous full-band unvoiced energy
*   OLBUE     - Previous low-band unvoiced energy
*   REF       - Reference energy for initialization and DITHER threshold
*   SNR       - Estimate of signal-to-noise ratio
*   SNR2      - Estimate of low-band signal-to-noise ratio
*   SNRL      - SNR level number
*   OT        - Onset transition present
*   VSTATE    - Decimal interpretation of binary voicing classifications
*   FIRST     - First call flag


	SUBROUTINE VOICIN( VWIN, INBUF, LPBUF, BUFLIM, HALF,
     1          MINAMD, MAXAMD, MINTAU, IVRC, OBOUND, VOIBUF, AF )

*	Global Variables:

	INCLUDE 'contrl.fh'
	INCLUDE 'vcomm.fh'

	REAL MINAMD, MAXAMD
	INTEGER VWIN(2), BUFLIM(4), HALF, MINTAU
	INTEGER AF, OBOUND(AF)
	REAL INBUF(BUFLIM(1):BUFLIM(2))
	REAL LPBUF(BUFLIM(3):BUFLIM(4))
	INTEGER VOIBUF(2,0:AF)
	INTEGER ZC, LBE, FBE
	INTEGER I, SNRL, VSTATE
	REAL DITHER, SNR, SNR2, IVRC(2)
	REAL MAXMIN, QS, RC1, AR_B
	REAL AR_F
	REAL VOICE(2,3)
	REAL VALUE(9)
	LOGICAL OT

*   Declare and initialize filters:

	INTEGER LBVE, LBUE, FBVE, FBUE, OFBUE, OLBUE, SFBUE, SLBUE
	INTEGER REF
	PARAMETER (REF = 3000)

	LOGICAL FIRST
	DATA FIRST /.TRUE./, DITHER/20/, NVDC/10/, NVDCL/5/
	DATA VDCL/600,450,300,200,6*0/

*   Voicing Decision Parameter vector (* denotes zero coefficient):
*
*	* MAXMIN
*	  LBE/LBVE
*	  ZC
*	  RC1
*	  QS
*	  IVRC2
*	  aR_B
*	  aR_F
*	* LOG(LBE/LBVE)

*   Define 2-D voicing decision coefficient vector according to the voicing
*   parameter order above.  Each row (VDC vector) is optimized for a specific
*   SNR.  The last element of the vector is the constant.

*	         E    ZC    RC1    Qs   IVRC2  aRb   aRf        c
	DATA VDC/
     1     0, 1714, -110,  334, -4096,  -654, 3752, 3769, 0,  1181,
     1     0,  874,  -97,  300, -4096, -1021, 2451, 2527, 0,  -500,
     1     0,  510,  -70,  250, -4096, -1270, 2194, 2491, 0, -1500,
     1     0,  500,  -10,  200, -4096, -1300, 2000, 2000, 0, -2000,
     1     0,  500,    0,    0, -4096, -1300, 2000, 2000, 0, -2500,
     1     50*0/

	IF (FIRST) THEN
	   LBVE = REF
	   FBVE = REF
	   FBUE = REF/16
	   OFBUE = REF/16
	   LBUE = REF/32
	   OLBUE = REF/32
	   SNR = 64*(FBVE/FBUE)
	   FIRST = .FALSE.
	END IF

*   The VOICE array contains the result of the linear discriminant function 
*   (analog values).  The VOIBUF array contains the hard-limited binary 
*   voicing decisions.  The VOICE and VOIBUF arrays, according to FORTRAN 
*   memory allocation, are addressed as:
*
*	   (half-frame number, future-frame number)
*
*	   |   Past    |  Present  |  Future1  |  Future2  |
*	   | 1,0 | 2,0 | 1,1 | 2,1 | 1,2 | 2,2 | 1,3 | 2,3 |  --->  time
*
*   Update linear discriminant function history each frame:

	IF (HALF .EQ. 1) THEN
	   VOICE(1,1)=VOICE(1,2)
	   VOICE(2,1)=VOICE(2,2)
	   VOICE(1,2)=VOICE(1,3)
	   VOICE(2,2)=VOICE(2,3)
	   MAXMIN = MAXAMD/MAX(MINAMD,1.)
	END IF

*   Calculate voicing parameters twice per frame:

	CALL VPARMS( VWIN, INBUF, LPBUF, BUFLIM, HALF, DITHER, MINTAU,
     1    ZC, LBE, FBE, QS, RC1, AR_B, AR_F )

*   Estimate signal-to-noise ratio to select the appropriate VDC vector.
*   The SNR is estimated as the running average of the ratio of the
*   running average full-band voiced energy to the running average
*   full-band unvoiced energy. SNR filter has gain of 63.

	SNR = NINT( 63*( SNR + FBVE/FLOAT(MAX(FBUE,1)) )/64.)
	SNR2 = (SNR*FBUE)/MAX(LBUE,1)

*   Quantize SNR to SNRL according to VDCL thresholds.

	SNRL = 1
	DO SNRL = 1, NVDCL-1
	   IF (SNR2 .GT. VDCL(SNRL)) GOTO 69
	END DO
*   	(Note:  SNRL = NVDCL here)
69	CONTINUE

*   Linear discriminant voicing parameters:
	VALUE(1) = MAXMIN
	VALUE(2) = FLOAT(LBE)/MAX(LBVE,1)
	VALUE(3) = ZC
	VALUE(4) = RC1
	VALUE(5) = QS
	VALUE(6) = IVRC(2)
	VALUE(7) = AR_B
	VALUE(8) = AR_F

*   Evaluation of linear discriminant function:

	VOICE(HALF,3) = VDC(10,SNRL)
	DO I = 1, 9
	   VOICE(HALF,3) = VOICE(HALF,3) + VDC(I,SNRL)*VALUE(I)
	END DO

*   Classify as voiced if discriminant > 0, otherwise unvoiced
*   Voicing decision for current half-frame:  1 = Voiced; 0 = Unvoiced

	IF (VOICE(HALF,3) .GT. 0.0) THEN
	   VOIBUF(HALF,3)=1
	ELSE
	   VOIBUF(HALF,3)=0
	END IF

*   Skip voicing decision smoothing in first half-frame:

	IF (HALF .EQ. 1) GOTO 99


*   Voicing decision smoothing rules (override of linear combination):
*
*	Unvoiced half-frames:  At least two in a row.
*	--------------------
*
*	Voiced half-frames:    At least two in a row in one frame.
*	-------------------    Otherwise at least three in a row.
*			       (Due to the way transition frames are encoded)
*
*	In many cases, the discriminant function determines how to smooth.
*	In the following chart, the decisions marked with a * may be overridden.
*
*   Voicing override of transitions at onsets:
*	If a V/UV or UV/V voicing decision transition occurs within one-half
*	frame of an onset bounding a voicing window, then the transition is
*	moved to occur at the onset.
*
*	P	1F
*	-----	-----
*	0   0   0   0
*	0   0   0*  1	(If there is an onset there)
*	0   0   1*  0*	(Based on 2F and discriminant distance)
*	0   0   1   1
*	0   1*  0   0	(Always)
*	0   1*  0*  1	(Based on discriminant distance)
*	0*  1   1   0*	(Based on past, 2F, and discriminant distance)
*	0   1*  1   1	(If there is an onset there)
*	1   0*  0   0	(If there is an onset there)
*	1   0   0   1
*	1   0*  1*  0	(Based on discriminant distance)
*	1   0*  1   1	(Always)
*	1   1   0   0
*	1   1   0*  1*	(Based on 2F and discriminant distance)
*	1   1   1*  0	(If there is an onset there)
*	1   1   1   1
*
*   Determine if there is an onset transition between P and 1F.
*   OT (Onset Transition) is true if there is an onset between 
*   P and 1F but not after 1F.

	OT = (AND(OBOUND(1), 2) .NE. 0 .OR. OBOUND(2) .EQ. 1)
     1      .AND. AND(OBOUND(3), 1) .EQ. 0

*   Multi-way dispatch on voicing decision history:

	VSTATE = VOIBUF(1,1)*8 + VOIBUF(2,1)*4 + VOIBUF(1,2)*2 + VOIBUF(2,2)
	GOTO (99,1,2,99,4,5,6,7,8,99,10,11,99,13,14,99) VSTATE+1

1	   IF (OT .AND. VOIBUF(1,3) .EQ. 1) VOIBUF(1,2) = 1
	GOTO 99

2	   IF (VOIBUF(1,3) .EQ. 0 .OR.
     1    VOICE(1,2) .LT. -VOICE(2,2)) THEN
	      VOIBUF(1,2) = 0
	   ELSE
	      VOIBUF(2,2) = 1
	   END IF
	GOTO 99

4	   VOIBUF(2,1) = 0
	GOTO 99

5	   IF (VOICE(2,1) .LT. -VOICE(1,2)) THEN
	      VOIBUF(2,1) = 0
	   ELSE
	      VOIBUF(1,2) = 1
	   END IF
	GOTO 99

*   VOIBUF(2,0) must be 0
6	   IF (VOIBUF(1,0) .EQ. 1 .OR.
     1         VOIBUF(1,3) .EQ. 1 .OR.
     1          VOICE(2,2) .GT. VOICE(1,1)) THEN
	      VOIBUF(2,2) = 1
	   ELSE
	      VOIBUF(1,1) = 1
	   END IF
	GOTO 99

7	   IF (OT) VOIBUF(2,1) = 0
	GOTO 99

8	   IF (OT) VOIBUF(2,1) = 1
	GOTO 99

10	   IF (VOICE(1,2) .LT. -VOICE(2,1)) THEN
	      VOIBUF(1,2) = 0
	   ELSE
	      VOIBUF(2,1) = 1
	   END IF
	GOTO 99

11	   VOIBUF(2,1) = 1
	GOTO 99

13	   IF (VOIBUF(1,3) .EQ. 0 .AND. VOICE(2,2) .LT. -VOICE(1,2)) THEN
	      VOIBUF(2,2) = 0
	   ELSE
	      VOIBUF(1,2) = 1
	   END IF
	GOTO 99

14	   IF (OT .AND. VOIBUF(1,3) .EQ. 0) VOIBUF(1,2) = 0
*	GOTO 99

99	CONTINUE

*   Now update parameters:
*   ----------------------
*
*   During unvoiced half-frames, update the low band and full band unvoiced
*   energy estimates (LBUE and FBUE) and also the zero crossing
*   threshold (DITHER).  (The input to the unvoiced energy filters is
*   restricted to be less than 10dB above the previous inputs of the
*   filters.)
*   During voiced half-frames, update the low-pass (LBVE) and all-pass 
*   (FBVE) voiced energy estimates.

	IF (VOIBUF(HALF,3) .EQ. 0) THEN
	   SFBUE = NINT(( 63*SFBUE + 8*MIN(FBE,3*OFBUE) )/64.)
	   FBUE = SFBUE/8
	   OFBUE = FBE
	   SLBUE = NINT(( 63*SLBUE + 8*MIN(LBE,3*OLBUE) )/64.)
	   LBUE = SLBUE/8
	   OLBUE = LBE
	ELSE
	   LBVE = NINT(( 63*LBVE + LBE )/64.)
	   FBVE = NINT(( 63*FBVE + FBE )/64.)
	END IF

*   Set dither threshold to yield proper zero crossing rates in the
*   presence of low frequency noise and low level signal input.
*   NOTE: The divisor is a function of REF, the expected energies.

	DITHER = MIN(MAX( 64*SQRT(FLOAT(LBUE*LBVE)) / REF,1.),20.)

*   Print Test Data

	IF( LISTL.GE.3 ) THEN
	   IF(HALF.EQ.1) WRITE(FDEBUG,930) VWIN,MINAMD,MAXAMD,MINTAU,IVRC
930	   FORMAT(' Voicing:VWIN     MINA     MAXA  MINTAU  IVRC1  IVRC2'/
     1    5X,2I4,2F9.1,I8,2F7.3/
     1    ' HALF  DISCR  MAX/MIN  LE/LVE   ZC    RC1     QS   IVRC2'
     1    '   aR_B   aR_F : DITH  LBE   FBE  LBVE  FBVE  LBUE  FBUE',
     1    ' SNR SNRL VS OT')
	   WRITE(FDEBUG,940) HALF, VOICE(HALF,3), (VALUE(I),I=1,8), DITHER,
     1    LBE, FBE, LBVE, FBVE, LBUE, FBUE,
     1    SNR, SNRL, VSTATE, OT
940	   FORMAT(1X,I3,':',F8.0,F9.1,F7.3,F7.2,5F7.3,F5.1,6I6,F5.1,2I3,L3)
	END IF

*   Voicing decisions are returned in VOIBUF.

	RETURN
	END
