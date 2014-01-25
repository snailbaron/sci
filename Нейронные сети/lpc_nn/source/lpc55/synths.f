******************************************************************
*
*	SYNTHS Version 54
*
******************************************************************
*
*  NOTE: There is excessive buffering here, BSYNZ and DEEMP should be
*        changed to operate on variable positions within SOUT.  Also,
*        the output length parameter is bogus, and PITSYN should be
*        rewritten to allow a constant frame length output
*
	SUBROUTINE SYNTHS(VOICE, PITCH, RMS, RC, SPEECH, K)
	INCLUDE 'config.fh'
	INCLUDE 'contrl.fh'
	INTEGER VOICE(2), PITCH
	REAL RMS, RC(ORDER), SPEECH(2*MAXFRM)
	INTEGER I, J, K, NOUT, IPITI(11), IVUV(11)
	REAL RATIO, G2PASS
	REAL RCI(MAXORD,11), RMSI(11), PC(MAXORD), SOUT(MAXFRM)
	REAL GPRIME
	DATA GPRIME/.7/

	IF(LISTL.GE.3) THEN
	   WRITE(FDEBUG,400) NFRAME, VOICE, PITCH, RMS, RC
400	   FORMAT(1X/' SYNTHESIS DATA -- FRAME',I6,
     1    T32,2I3,I6,1X,F5.0,10F8.3/)
	   IF(LISTL.GE.4) WRITE(FDEBUG,410)
410	   FORMAT(' EPOCH  G2PASS  RATIO PSCALE')
	END IF

	pitch = max(min(pitch,156),20)
	do i = 1, order
	    rc(i) = max(min(rc(i),.99),-.99)
	end do

	K = 0
	CALL PITSYN(ORDER, VOICE, PITCH, RMS, RC, LFRAME, 
     1              IVUV, IPITI, RMSI, RCI, NOUT, RATIO)
	IF(NOUT.GT.0) THEN
	   DO J = 1,NOUT
	      IF(LISTL.GE.3) THEN
	         IF(LISTL.EQ.3) THEN
	            WRITE(FDEBUG,420) J, NOUT, IVUV(J), IPITI(J), RMSI(J),
     1             (RCI(I,J),I=1,ORDER)
420	            FORMAT(1X,'PITSYN EPOCH ',I2,' OF ',I2,T32,I4,I8,1X,
     1             F5.0,T50,10F8.3)
	         ELSE
	            WRITE(FDEBUG,422) J, NOUT, IVUV(J), IPITI(J), RMSI(J),
     1             (RCI(I,J),I=1,ORDER)
422	            FORMAT(1X,I2,'/',I2,T32,I4,I8,1X,
     1             F5.0,T50,10F8.3)
	         END IF
	      END IF
	      CALL IRC2PC(RCI(1,J), PC, ORDER, GPRIME, G2PASS)
	      IF(LISTL.GE.4) WRITE(FDEBUG,430) G2PASS, RATIO, 1.0, PC
430	      FORMAT(T7,3F7.3,T50,10F8.1)
	      CALL BSYNZ(PC, IPITI(J), IVUV(J), SOUT, RMSI(J),
     1       RATIO, G2PASS)
	      CALL DEEMP(SOUT, IPITI(J))
	      DO I = 1,IPITI(J)
	         K = K + 1
	         SPEECH(K) = SOUT(I) / 4096.
	      END DO
	   END DO
	END IF

	RETURN
	END
