******************************************************************
*
*	IRC2PC Version 48
*
******************************************************************
*
*   Convert Reflection Coefficients to Predictor Coeficients
*
* Inputs:
*  RC     - Reflection coefficients
*  ORDER  - Number of RC's
*  GPRIME - Excitation modification gain
* Outputs:
*  PC     - Predictor coefficients
*  G2PASS - Excitation modification sharpening factor
*
	SUBROUTINE IRC2PC( RC, PC, ORDER, GPRIME, G2PASS )
	INCLUDE 'config.fh'
	INTEGER ORDER, I, J
	REAL RC(ORDER), PC(ORDER), GPRIME, G2PASS, TEMP(MAXORD)

	G2PASS = 1.
	DO I = 1,ORDER
	   G2PASS = G2PASS*( 1. - RC(I)*RC(I) )
	END DO
	G2PASS = GPRIME*SQRT(G2PASS)
	PC(1) = RC(1)
	DO I = 2,ORDER
	   DO J = 1,I-1
	      TEMP(J) = PC(J) - RC(I)*PC(I-J)
	   END DO
	   DO J = 1,I-1
	      PC(J) = TEMP(J)
	   END DO
	   PC(I) = RC(I)
	END DO
	RETURN
	END
