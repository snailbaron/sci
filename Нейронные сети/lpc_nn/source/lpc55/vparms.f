**********************************************************************
*
*	VPARMS Version 50
*
**********************************************************************
*
*  Calculate voicing parameters:
*
* Inputs:
*  VWIN   - Voicing window limits
*  INBUF  - Input speech buffer
*  LPBUF  - Low pass filtered speech
*  BUFLIM - Array bounds for INBUF and LPBUF
*  HALF   - Half frame (1 or 2)
*  DITHER - Zero crossing threshold
*  MINTAU - Lag corresponding to minimum AMDF value (pitch estimate)
* Outputs:
*  ZC     - Zero crossing rate
*  LBE    - Low band energy (sum of magnitudes - SM)
*  FBE    - Full band energy (SM)
*  QS     - Ratio of 6 dB/oct preemphasized energy to full band energy
*  RC1    - First reflection coefficient
*  AR_B   - Product of the causal forward and reverse pitch
*           prediction gains
*  AR_F   - Product of the noncausal forward and reverse pitch
*           prediction gains
* Internal:
*  OLDSGN - Previous sign of dithered signal
*  VLEN   - Length of voicing window
*  START  - Lower address of current half of voicing window
*  STOP   - Upper address of current half of voicing window
*  E_0    - Energy of LPF speech (sum of squares - SS)
*  E_B    - Energy of LPF speech backward one pitch period (SS)
*  E_F    - Energy of LPF speech forward one pitch period (SS)
*  R_B    - Autocovariance of LPF speech backward one pitch period
*  R_F    - Autocovariance of LPF speech forward one pitch period
*  LP_RMS - Energy of LPF speech (sum of magnitudes - SM)
*  AP_RMS - Energy of all-pass speech (SM)
*  E_PRE  - Energy of 6dB preemphasized speech (SM)
*  E0AP   - Energy of all-pass speech (SS)
*

	SUBROUTINE VPARMS( VWIN, INBUF, LPBUF, BUFLIM, HALF, DITHER, MINTAU,
     1    ZC, LBE, FBE, QS, RC1, AR_B, AR_F )
	INTEGER BUFLIM(4), VWIN(2)
	REAL INBUF(BUFLIM(1):BUFLIM(2)), LPBUF(BUFLIM(3):BUFLIM(4))
	INTEGER HALF, ZC, LBE, FBE, MINTAU
	REAL DITHER, QS, RC1, AR_B
	REAL AR_F
	INTEGER I, VLEN, START, STOP
	REAL OLDSGN, E_0, E_B, R_B, LP_RMS, AP_RMS, E_PRE, E0AP
	REAL E_F, R_F

*   Calculate zero crossings (ZC) and several energy and correlation
*   measures on low band and full band speech.  Each measure is taken
*   over either the first or the second half of the voicing window,
*   depending on the variable HALF.

	LP_RMS = 0.
	AP_RMS = 0.
	E_PRE = 0.
	E0AP = 0.
	RC1 = 0.
	E_0 = 0.
	E_B = 0.
	E_F = 0.
	R_F = 0.
	R_B = 0.
	ZC = 0

	VLEN = VWIN(2) - VWIN(1) + 1
	START = VWIN(1) + (HALF-1)*VLEN/2 + 1
	STOP = START + VLEN/2 - 1
	OLDSGN = SIGN( 1., INBUF(START-1)-DITHER )
	DO I = START, STOP
	   LP_RMS = LP_RMS + ABS(LPBUF(I))
	   AP_RMS = AP_RMS + ABS(INBUF(I))
	   E_PRE = E_PRE + ABS(INBUF(I)-INBUF(I-1))
	   E0AP = E0AP + INBUF(I)**2
	   RC1 = RC1 + INBUF(I)*INBUF(I-1)
	   E_0 = E_0 + LPBUF(I)**2
	   E_B = E_B + LPBUF(I-MINTAU)**2
	   E_F = E_F + LPBUF(I+MINTAU)**2
	   R_F = R_F + LPBUF(I)*LPBUF(I+MINTAU)
	   R_B = R_B + LPBUF(I)*LPBUF(I-MINTAU)
	   IF( SIGN(1.,INBUF(I)+DITHER) .NE. OLDSGN ) THEN
	      ZC = ZC + 1
	      OLDSGN = -OLDSGN
	   END IF
	   DITHER = -DITHER
	END DO

*   Normalized short-term autocovariance coefficient at unit sample delay

	RC1 = RC1 / MAX(E0AP,1.)

*   Ratio of the energy of the first difference signal (6 dB/oct preemphasis)
*   to the energy of the full band signal

	QS = E_PRE / MAX(2.*AP_RMS,1.)

*   aR_b is the product of the forward and reverse prediction gains,
*   looking backward in time (the causal case).

	AR_B = (R_B / MAX(E_B,1.)) * (R_B / MAX(E_0,1.))

*   aR_f is the same as aR_b, but looking forward in time (non causal case).

	AR_F = (R_F / MAX(E_F,1.)) * (R_F / MAX(E_0,1.))

*   Normalize ZC, LBE, and FBE to old fixed window length of 180.
*   (The fraction 90/VLEN has a range of .58 to 1)

	ZC =       NINT( ZC*2     * (90./VLEN) )
	LBE = MIN( NINT( LP_RMS/4 * (90./VLEN) ), 32767 )
	FBE = MIN( NINT( AP_RMS/4 * (90./VLEN) ), 32767 )

	RETURN
	END
