***********************************************************************
*
*	TBDM Version 49
*
**********************************************************************
*
*  TURBO DIFMAG: Compute High Resolution Average Magnitude Difference Function
*
* Inputs:
*  SPEECH - Low pass filtered speech
*  LPITA  - Length of speech buffer
*  TAU    - Table of lags
*  LTAU   - Number of lag values to compute
* Outputs:
*  AMDF   - Average Magnitude Difference for each lag in TAU
*  MINPTR - Index of minimum AMDF value
*  MAXPTR - Index of maximum AMDF value within +/- 1/2 octave of min
*  MINTAU - Lag corresponding to minimum AMDF value

	SUBROUTINE TBDM( SPEECH, LPITA, TAU, LTAU, AMDF,
     1                  MINPTR, MAXPTR, MINTAU )
	INTEGER LPITA, LTAU, MINPTR, MAXPTR, MINTAU, MINAMD
	INTEGER I, PTR, TAU(LTAU), TAU2(6), LTAU2, MINP2, MAXP2
	REAL SPEECH(312), AMDF(LTAU), AMDF2(6)
*	REAL SPEECH(LPITA+TAU(LTAU)), AMDF(LTAU), AMDF2(6)
*   Stupid TOAST doesn't understand expressions

*   Compute full AMDF using log spaced lags, find coarse minimum

	CALL DIFMAG( SPEECH, LPITA, TAU, LTAU, TAU(LTAU),
     1    AMDF, MINPTR, MAXPTR )
	MINTAU = TAU(MINPTR)
	MINAMD = AMDF(MINPTR)

*   Build table containing all lags within +/- 3 of the AMDF minimum
*    excluding all that have already been computed

	LTAU2 = 0
	PTR = MINPTR - 2
	DO I = MAX(MINTAU-3,41), MIN(MINTAU+3,TAU(LTAU)-1)
	   DO WHILE( TAU(PTR).LT.I )
	      PTR = PTR + 1
	   END DO
	   IF( TAU(PTR).NE.I) THEN
	      LTAU2 = LTAU2 + 1
	      TAU2(LTAU2) = I
	   END IF
	END DO

*   Compute AMDF of the new lags, if there are any, and choose one
*    if it is better than the coarse minimum

	IF( LTAU2.GT.0 ) THEN
	   CALL DIFMAG( SPEECH, LPITA, TAU2, LTAU2, TAU(LTAU),
     1       AMDF2, MINP2, MAXP2 )
	   IF( AMDF2(MINP2).LT.MINAMD ) THEN
	      MINTAU = TAU2(MINP2)
	      MINAMD = AMDF2(MINP2)
	   END IF
	END IF

*   Check one octave up, if there are any lags not yet computed

	IF( MINTAU.GE.80 ) THEN
	   I = MINTAU/2
	   IF( AND(I,1).EQ.0 ) THEN
	      LTAU2 = 2
	      TAU2(1) = I-1
	      TAU2(2) = I+1
	   ELSE
	      LTAU2 = 1
	      TAU2(1) = I
	   END IF
	   CALL DIFMAG( SPEECH, LPITA, TAU2, LTAU2, TAU(LTAU),
     1       AMDF2, MINP2, MAXP2 )
	   IF( AMDF2(MINP2).LT.MINAMD ) THEN
	      MINTAU = TAU2(MINP2)
	      MINAMD = AMDF2(MINP2)
	      MINPTR = MINPTR - 20
	   END IF
	END IF

*   Force minimum of the AMDF array to the high resolution minimum

	AMDF(MINPTR) = MINAMD

*   Find maximum of AMDF within 1/2 octave of minimum

	MAXPTR = MAX(MINPTR-5,1)
	DO I = MAXPTR+1, MIN(MINPTR+5,LTAU)
	   IF( AMDF(I).GT.AMDF(MAXPTR) ) MAXPTR = I
	END DO

	RETURN
	END
