**********************************************************************
*
*	IVFILT Version 48
*
**********************************************************************
*
*   2nd order inverse filter, speech is decimated 4:1
*
* Inputs:
*  LEN    - Length of speech buffers
*  NSAMP  - Number of samples to filter
*  LPBUF  - Low pass filtered speech buffer
* Output:
*  IVBUF  - Inverse filtered speech buffer
*  IVRC   - Inverse filter reflection coefficients (for voicing)
*
	SUBROUTINE IVFILT( LPBUF, IVBUF, LEN, NSAMP, IVRC )
	INTEGER LEN, NSAMP, I, J, K
	REAL IVBUF(LEN), LPBUF(LEN)
	REAL R(3), PC1, PC2, IVRC(2)

*  Calculate Autocorrelations

	DO I = 1,3
	   R(I) = 0.
	   K = 4*(I-1)
	   DO J = I*4+LEN-NSAMP,LEN,2
	      R(I) = R(I) + LPBUF(J)*LPBUF(J-K)
	   END DO
	END DO

*  Calculate predictor coefficients

	PC1 = 0.
	PC2 = 0.
	IVRC(1) = 0.
	IVRC(2) = 0.
	IF (R(1) .GT. 1.0E-10) THEN
	   IVRC(1) = R(2)/R(1)
	   IVRC(2) = (R(3)-IVRC(1)*R(2)) / (R(1)-IVRC(1)*R(2))
	   PC1 = IVRC(1) - IVRC(1)*IVRC(2)
	   PC2 = IVRC(2)
	END IF

*  Inverse filter LPBUF into IVBUF

	DO I = LEN+1-NSAMP,LEN
	   IVBUF(I) = LPBUF(I) - PC1*LPBUF(I-4) - PC2*LPBUF(I-8)
	END DO

	RETURN
	END
