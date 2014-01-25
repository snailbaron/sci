************************************************************************
*
*     NSA LPC-10 Voice Coder
*
*       Unix Version 54
*
*        15 March 1991
*
************************************************************************


	PROGRAM LPCSIM
	INCLUDE 'config.fh'
	INCLUDE 'contrl.fh'

	INTEGER VOICE(2), PITCH
	REAL RMS, RC(MAXORD), SPEECH(MAXFRM+MAXPIT)
	INTEGER LEN, N, SREAD, SWRITE
	LOGICAL EOF
	DATA  EOF /.FALSE./

*   Set processing options, open files

	CALL SETUP()

*   Process until end of file on input

	DO WHILE(.TRUE.)
	   CALL FRAME()

*   Read, Pre-process, and Analyze input speech

	   IF (FSI .GE. 0) THEN
	      N = SREAD(FSI, SPEECH, LFRAME)
	      IF(N .NE. LFRAME) GOTO 900
	      CALL PREPRO(SPEECH, LFRAME)
	      CALL ANALYS(SPEECH, VOICE, PITCH, RMS, RC)
	   END IF

*   Encode parameters and send over channel

	   CALL TRANS(VOICE, PITCH, RMS, RC, EOF)
	   IF(EOF) GOTO 900

*   Synthesize speech from received parameters

	   IF (FSO .GE. 0) THEN
	      CALL SYNTHS(VOICE, PITCH, RMS, RC, SPEECH, LEN)
	      N = SWRITE(FSO, SPEECH, LEN)
	   END IF
	END DO

*   Finished - wrap it up

900	CALL WRAPUP()
	CALL EXIT(0)
	END
