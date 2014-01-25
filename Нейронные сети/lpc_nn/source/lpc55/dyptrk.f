**********************************************************************
*
*	DYPTRK Version 52
*
**********************************************************************
*
*   Dynamic Pitch Tracker
*
*  Inputs:
*   AMDF   - Average Magnitude Difference Function array
*   LTAU   - Number of lags in AMDF
*   MINPTR - Location of minimum AMDF value
*   VOICE  - Voicing decision
*  Outputs:
*   PITCH  - Smoothed pitch value, 2 frames delayed
*   MIDX   - Initial estimate of current frame pitch
*  Compile time constant:
*   DEPTH  - Number of frames to trace back
*

	SUBROUTINE DYPTRK( AMDF, LTAU, MINPTR, VOICE, PITCH, MIDX )
	INCLUDE 'contrl.fh'
	INTEGER LTAU, MINPTR, VOICE, PITCH, MIDX
	REAL AMDF(LTAU), S(60), SBAR, MINSC, MAXSC, ALPHA
	INTEGER DEPTH
	PARAMETER (DEPTH=2)
	INTEGER P(60,DEPTH), PBAR, I, J, IPOINT, IPTR, PATH(DEPTH)
	REAL ALPHAX

*   Calculate the confidence factor ALPHA, used as a threshold slope in
*   SEESAW.  If unvoiced, set high slope so that every point in P array
*   is marked as a potential pitch frequency.  A scaled up version (ALPHAX)
*   is used to maintain arithmetic precision.

	IF( VOICE .EQ. 1 ) THEN
	   ALPHAX = .75*ALPHAX + AMDF(MINPTR)/2.
	ELSE
	   ALPHAX = (63./64.)*ALPHAX
	END IF
	ALPHA = ALPHAX/16
	IF( VOICE.EQ.0 .AND. ALPHAX.LT.128 ) ALPHA = 8

*  SEESAW: Construct a pitch pointer array and intermediate winner function
*   Left to right pass:

	IPTR = IPOINT+1
	P(1,IPTR) = 1
	I = 1
	PBAR = 1
	SBAR = S(1)
	DO I = 1,LTAU 
	   SBAR = SBAR + ALPHA
	   IF (SBAR.LT.S(I)) THEN
	      S(I) = SBAR
	      P(I,IPTR) = PBAR
	   ELSE
	      SBAR = S(I)
	      P(I,IPTR) = I
	      PBAR = I
	   END IF
	END DO

*   Right to left pass:

	I = PBAR-1
	SBAR = S(I+1)
	DO WHILE (I.GE.1)
	   SBAR = SBAR + ALPHA
	   IF (SBAR.LT.S(I)) THEN
	      S(I) = SBAR
	      P(I,IPTR) = PBAR
	   ELSE
	      PBAR = P(I,IPTR)
	      I = PBAR
	      SBAR = S(I)
	   END IF
	   I = I-1
	END DO

*   Update S using AMDF
*   Find maximum, minimum, and location of minimum

	S(1) = S(1) + AMDF(1)/2
	MINSC = S(1)
	MAXSC = MINSC
	MIDX = 1
	DO I = 2,LTAU
	   S(I) = S(I) + AMDF(I)/2
	   IF(S(I).GT.MAXSC) MAXSC = S(I)
	   IF(S(I).LT.MINSC) MIDX = I
	   IF(S(I).LT.MINSC) MINSC = S(I)
	END DO

*   Subtract MINSC from S to prevent overflow

	DO I = 1,LTAU
	   S(I) = S(I) - MINSC
	END DO
	MAXSC = MAXSC - MINSC

*   Use higher octave pitch if significant null there

	J = 0
	DO I = 20, 40, 10
	   IF (MIDX .GT. I) THEN
	      IF (S(MIDX-I) .LT. MAXSC/4) J = I
	   END IF
	END DO
	MIDX = MIDX - J

*   TRACE: look back two frames to find minimum cost pitch estimate

	J = IPOINT
	PITCH = MIDX
	DO I = 1,DEPTH
	   J = MOD(J,DEPTH) + 1
	   PITCH = P(PITCH,J)
	   PATH(I) = PITCH
	END DO
	IPOINT = MOD(IPOINT+DEPTH-1,DEPTH)

*   Print test data

	IF(LISTL.GE.3) THEN
	   IF(LISTL.GE.6) THEN
	      WRITE(FDEBUG,970) 'DYPTRACK array (P):',P
	      WRITE(FDEBUG,980) 'Pitch Winner Function (S):',S
	   END IF
	   WRITE(FDEBUG,950) IPOINT, MIDX, ALPHA, PITCH, PATH
950	   FORMAT(' Pitch: IPOINT  MIDX  ALPHA   PITCH     PATH'/
     1             5X,2I7,F7.0,I7,5X,10I4/)
970	   FORMAT(1X,A,100(/1X,20I6))
980	   FORMAT(1X,A,100(/1X,10F12.1))
	END IF

	RETURN
	END
