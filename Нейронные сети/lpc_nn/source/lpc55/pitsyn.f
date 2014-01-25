******************************************************************
*
*	PITSYN Version 53
*
******************************************************************
*
*   Synthesize a single pitch epoch
*
* Inputs:
*  ORDER  - Synthesis order (number of RC's)
*  VOICE  - Half frame voicing decisions
*  PITCH  - Pitch
*  RMS    - Energy
*  RC     - Reflection coefficients
*  LFRAME - Length of speech buffer
* Outputs:
*  IVUV   - Pitch epoch voicing decisions
*  IPITI  - Pitch epoch length
*  RMSI   - Pitch epoch energy
*  RCI    - Pitch epoch RC's
*  NOUT   - Number of pitch periods in this frame
*  RATIO  - Previous to present energy ratio
*  
	SUBROUTINE PITSYN( ORDER, VOICE, PITCH, RMS, RC, LFRAME,
     1  		   IVUV, IPITI, RMSI, RCI, NOUT, RATIO )
	INCLUDE 'config.fh'
	INTEGER ORDER, VOICE(2), PITCH, LFRAME
	INTEGER IVUV(11), IPITI(11), NOUT
	REAL RMS, RC(ORDER), RMSI(11), RCI(ORDER,11), RATIO
	REAL RCO(MAXORD), YARC(MAXORD)
	INTEGER I, J, LSAMP, IP, IPITO, ISTART, IVOICE, IVOICO
	INTEGER JSAMP, JUSED, NL
	REAL ALRN, ALRO, PROP
	REAL RMSO, SLOPE, UVPIT, VFLAG, XXY
	LOGICAL FIRST
	DATA FIRST/.TRUE./

	IF (RMS.LT.1) RMS = 1
	IF (RMSO.LT.1) RMSO = 1
	UVPIT = 0.0
	RATIO = RMS/(RMSO+8.)
	IF (FIRST) THEN
	   LSAMP = 0
	   IVOICE = VOICE(2)
	   IF (IVOICE.EQ.0) PITCH = LFRAME/4
	   NOUT = LFRAME/PITCH
	   JSAMP = LFRAME - NOUT*PITCH
	   DO I = 1,NOUT
	      DO J = 1,ORDER
	         RCI(J,I) = RC(J)
	      END DO
	      IVUV(I) = IVOICE
	      IPITI(I) = PITCH
	      RMSI(I) = RMS
	   END DO
	   FIRST = .FALSE.
	ELSE
	   VFLAG = 0
	   LSAMP = LFRAME + JSAMP
	   SLOPE = (PITCH-IPITO)/FLOAT(LSAMP)
	   NOUT = 0
	   JUSED = 0
	   ISTART = 1
	   IF ((VOICE(1).EQ.IVOICO).AND.(VOICE(2).EQ.VOICE(1))) THEN
	      IF (VOICE(2).EQ.0) THEN
* SSUV - -   0  ,  0  ,  0
	         PITCH = LFRAME/4
	         IPITO = PITCH
	         IF( RATIO.GT.8 ) RMSO = RMS
	      END IF
* SSVC - -   1  ,  1  ,  1
	      SLOPE = (PITCH-IPITO)/FLOAT(LSAMP)
	      IVOICE = VOICE(2)
	   ELSE
	      IF (IVOICO.NE.1) THEN
	         IF (IVOICO.EQ.VOICE(1)) THEN
* UV2VC2 - -  0  ,  0  ,  1
	            NL = LSAMP - LFRAME/4
	         ELSE
* UV2VC1 - -  0  ,  1  ,  1
	            NL = LSAMP - 3*LFRAME/4
	         ENDIF
	         IPITI(1) = NL/2
	         IPITI(2) = NL - IPITI(1)
	         IVUV(1) = 0
	         IVUV(2) = 0
	         RMSI(1) = RMSO
	         RMSI(2) = RMSO
	         DO I = 1,ORDER
	            RCI(I,1) = RCO(I)
	            RCI(I,2) = RCO(I)
	            RCO(I)   = RC(I)
	         END DO
	         SLOPE = 0
	         NOUT = 2
	         IPITO = PITCH
	         JUSED = NL
	         ISTART = NL + 1
	         IVOICE = 1
	      ELSE
	         IF (IVOICO.NE.VOICE(1)) THEN
* VC2UV1 - -   1  ,  0  ,  0
	            LSAMP = LFRAME/4 + JSAMP
	         ELSE
* VC2UV2 - -   1  ,  1  ,  0
	            LSAMP = 3*LFRAME/4 + JSAMP
	         END IF
	         DO I = 1,ORDER
	            YARC(I) = RC(I)
	            RC(I) = RCO(I)
	         END DO
	         IVOICE = 1
	         SLOPE = 0.
	         VFLAG = 1
	      END IF
	   END IF
	   DO WHILE (.TRUE.)
	      DO I = ISTART,LSAMP
	         IP = IPITO + SLOPE*I + .5
	         IF (UVPIT.NE.0.0) IP = UVPIT
	         IF (IP.LE.I-JUSED) THEN
	            NOUT = NOUT + 1
		    IF (NOUT .GT. 11) STOP 'PITSYN: too many epochs'
	            IPITI(NOUT) = IP
	            PITCH = IP
	            IVUV(NOUT) = IVOICE
	            JUSED = JUSED + IP
	            PROP = (JUSED-IP/2)/FLOAT(LSAMP)
	            DO J = 1,ORDER
	               ALRO = ALOG((1+RCO(J))/(1-RCO(J)))
	               ALRN = ALOG((1+RC(J))/(1-RC(J)))
	               XXY = ALRO + PROP*(ALRN-ALRO)
	               XXY = EXP(XXY)
	               RCI(J,NOUT) = (XXY-1)/(XXY+1)
	            END DO
	            RMSI(NOUT) = ALOG(RMSO) + PROP*(ALOG(RMS)-ALOG(RMSO))
	            RMSI(NOUT) = EXP(RMSI(NOUT))
	         END IF
	      END DO
	      IF (VFLAG.NE.1) GOTO 100
	      VFLAG = 0
	      ISTART = JUSED + 1
	      LSAMP = LFRAME + JSAMP
	      SLOPE = 0
	      IVOICE = 0
	      UVPIT = (LSAMP-ISTART)/2
	      IF(UVPIT.GT.90) UVPIT = UVPIT/2
	      RMSO = RMS
	      DO I = 1,ORDER
	         RC(I) = YARC(I)
	         RCO(I) = YARC(I)
	      END DO
	   END DO
100	   JSAMP = LSAMP - JUSED
	END IF
	IF (NOUT.NE.0) THEN
	   IVOICO = VOICE(2)
	   IPITO = PITCH
	   RMSO = RMS
	   DO I = 1,ORDER
	      RCO(I) = RC(I)
	   END DO
	END IF
	RETURN
	END
