******************************************************************
*
*	BSYNZ Version 54
*
******************************************************************
*
*   Synthesize One Pitch Epoch
*
* Inputs:
*  COEF  - Predictor coefficients
*  IP    - Pitch period (number of samples to synthesize)
*  IV    - Voicing for the current epoch
*  RMS   - Energy for the current epoch
*  RATIO - Energy slope for plosives
*  G2PASS- Sharpening factor for 2 pass synthesis
* Outputs:
*  SOUT  - Synthesized speech
*
	SUBROUTINE BSYNZ(COEF, IP, IV, SOUT, RMS, RATIO, G2PASS)
	INCLUDE 'config.fh'
	INCLUDE 'contrl.fh'

* Arguments

	INTEGER IP, IV
	REAL COEF(ORDER), SOUT(IP), G2PASS, RMS, RATIO

* Locals

	INTEGER RANDOM
	INTEGER KEXC(25), IPT, IPO, PX
	INTEGER I, J, K
	REAL EXC(MAXPIT+MAXORD), EXC2(MAXPIT+MAXORD), NOISE(MAXPIT+MAXORD)
	REAL LPI0, LPI1, LPI2, LPI3, HPI0, HPI1, HPI2, HPI3
	REAL A0, A1, A2, A3, B0, B1, B2, B3
	REAL PULSE, SSCALE, XSSQ, SUM, SSQ, GAIN
	REAL MESCL, PESCL, XY, RMSO
	PARAMETER (MESCL=1.0, PESCL=1.0)

	DATA KEXC /8,-16,26,-48,86,-162,294,-502,718,-728,
     1            184,672,-610,-672,184,728,718,502,294,162,
     1            86,48,26,16,8/
	DATA IPT /0/
	DATA A0/ .125/, A1/.75/, A2/ .125/, A3/0/
	DATA B0/-.125/, B1/.25/, B2/-.125/, B3/0/

*  Calculate history scale factor XY and scale filter state

	XY = MIN( RMSO/(RMS+1E-6), 8. )
	RMSO = RMS
	DO I = 1,ORDER
	   EXC2(I) = EXC2(IPO+I)*XY
	END DO
	IPO = IP

	IF(IV.EQ.0) THEN

*  Generate white noise for unvoiced

	   DO I = 1,IP
	      EXC(ORDER+I) = RANDOM() / 2**6
	   END DO

*  Impulse doublet excitation for plosives

	   PX = ((RANDOM()+32768)*(IP-1)/2**16) + ORDER + 1
	   PULSE = PESCL*(RATIO/4)*342
	   IF(PULSE.GT.2000) PULSE = 2000
	   EXC(PX)   = EXC(PX)   + PULSE
	   EXC(PX+1) = EXC(PX+1) - PULSE

*  Load voiced excitation

	ELSE
	   SSCALE = SQRT(FLOAT(IP))/6.928
	   DO I = 1,IP
	      EXC(ORDER+I) = 0.
	      IF(I.LE.25) EXC(ORDER+I) = SSCALE*KEXC(I)
	      LPI0 = EXC(ORDER+I)
	      EXC(ORDER+I) = A0*EXC(ORDER+I) + A1*LPI1 + A2*LPI2 + A3*LPI3
	      LPI3 = LPI2
	      LPI2 = LPI1
	      LPI1 = LPI0
	   END DO
	   DO I = 1,IP
	      NOISE(ORDER+I) = MESCL * RANDOM() / 2**6
	      HPI0 = NOISE(ORDER+I)
	      NOISE(ORDER+I) = B0*NOISE(ORDER+I)
     1                      + B1*HPI1 + B2*HPI2 + B3*HPI3
	      HPI3 = HPI2
	      HPI2 = HPI1
	      HPI1 = HPI0
	   END DO
	   DO I = 1,IP
	      EXC(ORDER+I) = EXC(ORDER+I) + NOISE(ORDER+I)
	   END DO
	END IF

*   Synthesis filters:
*    Modify the excitation with all-zero filter  1 + G*SUM

	XSSQ = 0
	DO I = 1,IP
	   K = ORDER + I
	   SUM = 0.
	   DO J = 1,ORDER
	      SUM = SUM + COEF(J)*EXC(K-J)
	   END DO
	   SUM = SUM*G2PASS
	   EXC2(K) = SUM + EXC(K)
	END DO

*   Synthesize using the all pole filter  1 / (1 - SUM)

	DO I = 1,IP
	   K = ORDER + I
	   SUM = 0.
	   DO J = 1,ORDER
	      SUM = SUM + COEF(J)*EXC2(K-J)
	   END DO
	   EXC2(K) = SUM + EXC2(K)
	   XSSQ = XSSQ + EXC2(K)*EXC2(K)
	END DO

*  Save filter history for next epoch

	DO I = 1,ORDER
	   EXC(I) = EXC(IP+I)
	   EXC2(I) = EXC2(IP+I)
	END DO

*  Apply gain to match RMS

	SSQ = RMS*RMS*IP
	GAIN = SQRT(SSQ/XSSQ)
	DO I = 1,IP
	   SOUT(I) = GAIN*EXC2(ORDER+I)
	END DO


*   Print test data

	IF(LISTL.GE.5) THEN
	   IF(IV.NE.0)
     1     WRITE(FDEBUG,980) 'NOISE:',(NOISE(I),I=1,IP+ORDER)
	   WRITE(FDEBUG,980) 'EXC:',  (EXC(I),  I=1,IP+ORDER)
	   WRITE(FDEBUG,980) 'EXC2:', (EXC2(I), I=1,IP+ORDER)
	   WRITE(FDEBUG,980) 'SOUT:', (SOUT(I), I=1,IP)
980	   FORMAT(1X,A,100(/1X,10F10.1))
	END IF

	RETURN
	END
