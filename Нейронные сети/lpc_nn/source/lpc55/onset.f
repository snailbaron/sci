*******************************************************************
*
*	ONSET Version 49
*
*******************************************************************
*
*	Floating point version
*

	SUBROUTINE ONSET( PEBUF, OSBUF, OSPTR, OSLEN,
     1                   SBUFL, SBUFH, LFRAME )
	INCLUDE 'config.fh'
	INTEGER OSLEN, SBUFL, SBUFH, LFRAME
	REAL PEBUF(SBUFL:SBUFH)
	INTEGER OSBUF(OSLEN), OSPTR

*   Detection of onsets in (or slightly preceding) the futuremost frame
*   of speech.

*   Arguments
*    PEBUF	Preemphasized speech
*    OSBUF	Buffer which holds sorted indexes of onsets (Modified)
*    OSPTR	Free pointer into OSBUF (Modified)

*   Parameters for onset detection algorithm:
*    L2		Threshold for filtered slope of FPC (function of L2WID!)
*    L2LAG	Lag due to both filters which compute filtered slope of FPC
*    L2WID	Width of the filter which computes the slope of FPC
*    OSHYST	The number of samples which of slope(FPC) which must be below
*	        the threshold before a new onset may be declared.
*   Variables
*    N, D       Numerator and denominator of prediction filters
*    FPC        Current prediction coefs
*    L2BUF, L2SUM1, L2SUM2    State of slope filter

	INTEGER L2LAG, L2WID, OSHYST, TEMP
	REAL L2
	PARAMETER (L2=1.7, L2LAG=9, L2WID=16, OSHYST=10)
	PARAMETER (TEMP=1+L2WID/2)

	REAL N, D, FPC
	REAL L2BUF(L2WID), L2SUM1, L2SUM2
	INTEGER L2PTR1, L2PTR2, I, LASTI
	LOGICAL HYST

	DATA L2BUF/L2WID*0./, L2PTR1/1/, L2PTR2/TEMP/
	DATA L2SUM1/0./, L2SUM2/0./
	DATA HYST/.FALSE./
	DATA N/0./, D/1./
	SAVE N, D, FPC
	SAVE L2BUF, L2PTR1, L2PTR2, L2SUM1, L2SUM2
	SAVE HYST, LASTI

	IF (HYST) LASTI = LASTI - 180
	DO I = SBUFH-LFRAME+1, SBUFH

*   Compute FPC; Use old FPC on divide by zero; Clamp FPC to +/- 1.
	   N=(PEBUF(I)*PEBUF(I-1)+63.*N) / 64.
	   D=(PEBUF(I-1)**2+63.*D) / 64.
	   IF (D .NE. 0.) THEN
	      IF (ABS(N) .GT. D) THEN
	         FPC = SIGN (1., N)
	      ELSE
	         FPC=N/D
	      END IF
	   END IF
*   Filter FPC
	   L2SUM2 = L2BUF(L2PTR1)
	   L2SUM1 = L2SUM1 - L2BUF(L2PTR2) + FPC
	   L2BUF(L2PTR2) = L2SUM1
	   L2BUF(L2PTR1) = FPC
	   L2PTR1 = MOD(L2PTR1,L2WID)+1
	   L2PTR2 = MOD(L2PTR2,L2WID)+1
	   IF (ABS(L2SUM1-L2SUM2) .GT. L2) THEN
	      IF (.NOT. HYST) THEN
*   Ignore if buffer full
	         IF (OSPTR .LE. OSLEN) THEN
	            OSBUF (OSPTR) = I - L2LAG
	            OSPTR = OSPTR + 1
	         END IF
	         HYST = .TRUE.
	      END IF
	      LASTI = I
	   ELSE IF (HYST .AND. I - LASTI .GE. OSHYST) THEN
	      HYST = .FALSE.
	   END IF
	END DO
	RETURN
	END
