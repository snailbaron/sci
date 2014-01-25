************************************************************************
*
*	PLACEA Version 48
*
************************************************************************

	SUBROUTINE PLACEA( IPITCH, VOIBUF, OBOUND, AF,
     1    VWIN, AWIN, EWIN, LFRAME, MAXWIN )
	INTEGER OBOUND, AF
	INTEGER LFRAME, MAXWIN
	INTEGER IPITCH, VOIBUF(2,0:AF)
	INTEGER VWIN(2,AF), AWIN(2,AF), EWIN(2,AF)

* Local variables and parameters

	INTEGER I, J, K, L
	LOGICAL EPHASE, ALLV, WINV

	INTEGER LRANGE, HRANGE

	LRANGE = (AF-2)*LFRAME + 1
	HRANGE = AF*LFRAME

*   Place the Analysis window based on the voicing window
*   placement, onsets, tentative voicing decision, and pitch.
*
*   Case 1:  Sustained Voiced Speech
*   If the five most recent voicing decisions are 
*   voiced, then the window is placed phase-synchronously with the 
*   previous window, as close to the present voicing window if possible.
*   If onsets bound the voicing window, then preference is given to
*   a phase-synchronous placement which does not overlap these onsets.
*
*   Case 2:  Voiced Transition
*   If at least one voicing decision in AF is voicied, and there are no
*   onsets, then the window is placed as in case 1.
*
*   Case 3:  Unvoiced Speech or Onsets
*   If both voicing decisions in AF are unvoiced, or there are onsets,
*   then the window is placed coincident with the voicing window.
*
*   Note:  During phase-synchronous placement of windows, the length
*   is not altered from MAXWIN, since this would defeat the purpose
*   of phase-synchronous placement.

* Check for case 1 and case 2

	ALLV =            VOIBUF(2,AF-2) .EQ. 1
	ALLV = ALLV .AND. VOIBUF(1,AF-1) .EQ. 1
	ALLV = ALLV .AND. VOIBUF(2,AF-1) .EQ. 1
	ALLV = ALLV .AND. VOIBUF(1,AF  ) .EQ. 1
	ALLV = ALLV .AND. VOIBUF(2,AF  ) .EQ. 1

	WINV = VOIBUF(1,AF  ) .EQ. 1 .OR.  VOIBUF(2,AF  ) .EQ. 1

	IF (ALLV .OR. WINV .AND. OBOUND .EQ. 0) THEN

* APHASE:  Phase synchronous window placement.
* Get minimum lower index of the window.

	   I = (LRANGE + IPITCH - 1 - AWIN(1,AF-1)) / IPITCH
	   I = I * IPITCH
	   I = I + AWIN(1,AF-1)

* L = the actual length of this frame's analysis window.

	   L = MAXWIN

* Calculate the location where a perfectly centered window would start.

	   K = (VWIN(1,AF) + VWIN(2,AF) + 1 - L) / 2

* Choose the actual location to be the pitch multiple closest to this.

	   AWIN(1,AF) = I + NINT (FLOAT (K - I) / IPITCH) * IPITCH
	   AWIN(2,AF) = AWIN(1,AF) + L - 1

* If there is an onset bounding the right of the voicing window and the
* analysis window overlaps that, then move the analysis window backward
* to avoid this onset.

	   IF (OBOUND .GE. 2 .AND. AWIN (2,AF) .GT. VWIN (2,AF)) THEN
	      AWIN(1,AF) = AWIN(1,AF) - IPITCH
	      AWIN(2,AF) = AWIN(2,AF) - IPITCH
	   END IF

* Similarly for the left of the voicing window.

	   IF ((OBOUND .EQ. 1 .OR. OBOUND .EQ. 3) .AND.
     1        AWIN (1,AF) .LT. VWIN (1,AF)) THEN
	      AWIN(1,AF) = AWIN(1,AF) + IPITCH
	      AWIN(2,AF) = AWIN(2,AF) + IPITCH
	   END IF

* If this placement puts the analysis window above HRANGE, then
* move it backward an integer number of pitch periods.

	   DO WHILE (AWIN (2,AF) .GT. HRANGE)
	      AWIN(1,AF) = AWIN(1,AF) - IPITCH
	      AWIN(2,AF) = AWIN(2,AF) - IPITCH
	   END DO

* Similarly if the placement puts the analysis window below LRANGE.

	   DO WHILE (AWIN (1,AF) .LT. LRANGE)
	      AWIN(1,AF) = AWIN(1,AF) + IPITCH
	      AWIN(2,AF) = AWIN(2,AF) + IPITCH
	   END DO


* Make Energy window be phase-synchronous.

	   EPHASE = .TRUE.

* Case 3

	ELSE
	   AWIN(1,AF) = VWIN(1,AF)
	   AWIN(2,AF) = VWIN(2,AF)
	   EPHASE = .FALSE.
	END IF

* RMS is computed over an integer number of pitch periods in the analysis
* window.  When it is not placed phase-synchronously, it is placed as close 
* as possible to onsets.

	J = ((AWIN(2,AF)-AWIN(1,AF)+1)/IPITCH)*IPITCH
	IF (J .EQ. 0 .OR. .NOT. WINV) THEN
	   EWIN(1,AF) = VWIN(1,AF)
	   EWIN(2,AF) = VWIN(2,AF)
	ELSE IF (.NOT. EPHASE .AND. OBOUND .EQ. 2) THEN
	   EWIN(1,AF) = AWIN(2,AF) - J + 1
	   EWIN(2,AF) = AWIN(2,AF)
	ELSE
	   EWIN(1,AF) = AWIN(1,AF)
	   EWIN(2,AF) = AWIN(1,AF) + J - 1
	END IF

	RETURN
	END
