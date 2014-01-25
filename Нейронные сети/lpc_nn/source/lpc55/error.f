******************************************************************
*
*	ERROR Version 45G
*
******************************************************************
*
*  Handle LPC processing error conditions
*
*   Inputs:
*    NAME - Routine where error occurred
*    ERR  - Error code
*    INFO - Additional data
*
*  The following error codes are hardwired:
*   2 - ANALYS  Unstable RC
*   4 - DIFMAG  AMDF Overflow
*   5 - SYNTHS  Output Clipped
*   6 - BSYNZ   Synthesis Filter Overflow
*   7 -    "        "       "       "
*   8 -    "        "       "       "
*   9 - ONSET   Onset Overflow
*  10 - INVERT  RC Underflow
*  11 - INVERT  RC Overflow
*  12 -    "    "     "
*  13 -    "    "     "
*
	SUBROUTINE ERROR( NAME, ERR, INFO )
	INCLUDE 'contrl.fh'
	CHARACTER NAME*(*)
	INTEGER ERR, INFO, M, LERR, LNF, MAXERR, LNBLNK
	PARAMETER (MAXERR=20)
	INTEGER MAP(MAXERR)
	CHARACTER*30 MSGT(10), MSG
	DATA MAP / 1,2,1,3,4,3*5,6,7,3*8,7*1 /
	DATA MSGT /
     1   'Undefined Error',
     1   '    Unstable RC',
     1   '  AMDF Overflow',
     1   ' Output Clipped',
     1   'Synthesis Filter Overflow',
     1   'Onset Buffer Overflow',
     1   'Underflow at RC',
     1   ' Overflow at RC',
     1   2*'Internal Inconsistency' /

	M = ERR
	IF(M.LT.1.OR.M.GT.MAXERR) M = 1
	IF(LISTL.GE.2) THEN
	   IF(ERR.NE.LERR.OR.NFRAME.NE.LNF) THEN
	      MSG = MSGT(MAP(M))
	      WRITE(FMSG,100) NAME, ERR, NFRAME, MSG(1:LNBLNK(MSG)), INFO
100	      FORMAT(1X,A,':',T10,'Warning',I3,' at frame',I6,' - ',A,I6)
	      LINCNT = LINCNT + 1
	   END IF
	   LERR = ERR
	   LNF = NFRAME
	END IF
	IF(ERR.EQ.2) NUNSFM = NUNSFM + 1
	IF(ERR.EQ.5) ICLIP = ICLIP + INFO
	RETURN
	END
