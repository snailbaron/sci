***********************************************************************
*
*	DIFMAG Version 49
*
**********************************************************************
*
*  Compute Average Magnitude Difference Function
*
* Inputs:
*  SPEECH - Low pass filtered speech
*  LPITA  - Length of speech buffer
*  TAU    - Table of lags
*  LTAU   - Number of lag values to compute
*  MAXLAG - Maximum possible lag value
* Outputs:
*  AMDF   - Average Magnitude Difference for each lag in TAU
*  MINPTR - Index of minimum AMDF value
*  MAXPTR - Index of maximum AMDF value

	SUBROUTINE DIFMAG( SPEECH, LPITA, TAU, LTAU, MAXLAG,
     1                    AMDF, MINPTR, MAXPTR )
	INTEGER LPITA, LTAU, MAXLAG, MINPTR, MAXPTR
	INTEGER TAU(LTAU), I, J, N1, N2
	REAL SPEECH(LPITA+MAXLAG), AMDF(LTAU), SUM

	MINPTR = 1
	MAXPTR = 1
	DO I = 1,LTAU
	   N1 = (MAXLAG-TAU(I))/2 + 1
	   N2 = N1 + LPITA - 1
	   SUM = 0.
	   DO J = N1,N2,4
	      SUM = SUM + ABS( SPEECH(J) - SPEECH(J+TAU(I)) )
	   END DO
	   AMDF(I) = SUM
	   IF( AMDF(I).LT.AMDF(MINPTR) ) MINPTR = I
	   IF( AMDF(I).GT.AMDF(MAXPTR) ) MAXPTR = I
	END DO
	RETURN
	END
