**********************************************************************
*
*	FRAME Version 54
*
**********************************************************************
*
*    Do whatever has to be done on a per frame basis:
*
*   * Update frame and block counters
*
*   * Set test data level of detail for particular frames:
*     Level  Meaning
*       -1     Vary detail level by frame 
*        0     No data file generated
*        1     Processing statistics only
*        2     Coded parameters for each frame
*        3     Scalar variables and RC's
*        4     Vectors and matrices
*        5     Synthesis buffers
*        6     Analysis buffers
*
*   To use variable detail level, create a file 'TESTDATA.DAT' with lines
*   containing < starting frame, ending frame, detail level > for each
*   range of interest.  If ending frame = 0, the specified detail level
*   will be used when not within any explicitly specified range.
*
	SUBROUTINE FRAME()
	INCLUDE 'contrl.fh'
	INTEGER FRAME1, FRAME2, LUNIT, LSAVE, LNEXT, LL
	PARAMETER (LUNIT=7)

*  Update frame counter

	NFRAME = NFRAME + 1

*   Set test data level of detail

	IF (NFRAME .LE. 1) THEN
	    LL = LISTL
	    IF (LL .LT. 0)
     1          OPEN(UNIT=LUNIT, FILE='TESTDATA.DAT', STATUS='OLD')
	END IF

	IF (LL .LT. 0) THEN
	    DO WHILE (NFRAME.GE.FRAME2)
	        READ(LUNIT,*,END=1110) FRAME1, FRAME2, LNEXT
	        LNEXT = MAX(LNEXT, 1)
		LSAVE = MAX(LSAVE, 1)
	        IF (FRAME2 .LE. 0) LSAVE = LNEXT
	    END DO
1110	    LISTL = LSAVE
	    IF (NFRAME.GE.FRAME1 .AND. NFRAME.LE.FRAME2) LISTL = LNEXT
	END IF
	RETURN
	END
