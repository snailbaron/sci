**********************************************************************
*
*	DCBIAS Version 50
*
**********************************************************************
*
*    Calculate and remove DC bias from buffer
*
* Inputs:
*  LEN    - Length of speech buffers
*  SPEECH - Input speech buffer
* Output:
*  SIGOUT - Output speech buffer
*
	SUBROUTINE DCBIAS( LEN, SPEECH, SIGOUT )
	INTEGER LEN, I
	REAL BIAS, SPEECH(LEN), SIGOUT(LEN)

	BIAS = 0
	DO I = 1,LEN
	   BIAS = BIAS + SPEECH(I)
	END DO
	BIAS = BIAS/LEN
	DO I = 1,LEN
	   SIGOUT(I) = SPEECH(I) - BIAS
	END DO
	RETURN
	END
