**********************************************************************
*
*	ENERGY Version 50
*
**********************************************************************
*
* Compute RMS energy
*
* Inputs:
*  LEN    - Length of speech buffer
*  SPEECH - Speech buffer
* Output:
*  RMS    - Root Mean Square energy
*
	SUBROUTINE ENERGY( LEN, SPEECH, RMS )
	INTEGER LEN, I
	REAL SPEECH(LEN), RMS

	RMS = 0
	DO I = 1,LEN
	   RMS = RMS + SPEECH(I)*SPEECH(I)
	END DO
	RMS = SQRT( RMS / LEN )
	RETURN
	END
