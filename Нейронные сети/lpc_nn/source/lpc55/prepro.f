**********************************************************************
*
*	PREPRO Version 48
*
**********************************************************************
*
*    Pre-process input speech:
*
* Inputs:
*  LENGTH - Number of SPEECH samples
* In/Outputs:
*  SPEECH - Speech
*
	SUBROUTINE PREPRO( SPEECH, LENGTH )
	INTEGER LENGTH
	REAL SPEECH(LENGTH)

*   High Pass Filter at 100 Hz

	CALL HP100( SPEECH, 1, LENGTH )

	RETURN
	END
