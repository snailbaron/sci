*****************************************************************
*
*	INVERT Version 45G
*
*****************************************************************
*
*  Invert a covariance matrix using Choleski decomposition method
*
*  Inputs:
*    ORDER            - Analysis order
*    PHI(ORDER,ORDER) - Covariance matrix
*    PSI(ORDER)       - Column vector to be predicted
*  Outputs:
*    RC(ORDER)        - Pseudo reflection coefficients
*  Internal:
*    V(ORDER,ORDER)   - Temporary matrix
*    X(ORDER)         - Column scaling factors
*
*  NOTE: Temporary matrix V is not needed and may be replaced
*    by PHI if the original PHI values do not need to be preserved.
*
	SUBROUTINE INVERT( ORDER, PHI, PSI, RC )
	INCLUDE 'config.fh'
	INTEGER ORDER, I, J, K
	REAL PHI(ORDER,ORDER), PSI(ORDER), RC(ORDER)
	REAL V(MAXORD,MAXORD), SAVE, EPS
	PARAMETER (EPS=1.0E-10)

*  Decompose PHI into V * D * V' where V is a triangular matrix whose
*   main diagonal elements are all 1, V' is the transpose of V, and
*   D is a vector.  Here D(n) is stored in location V(n,n).

	DO J = 1,ORDER
	   DO I = J,ORDER
	      V(I,J) = PHI(I,J)
	   END DO
	   DO K = 1,J-1
	      SAVE = V(J,K)*V(K,K)
	      DO I = J,ORDER
	         V(I,J) = V(I,J) - V(I,K)*SAVE
	      END DO
	   END DO

*  Compute intermediate results, which are similar to RC's

	   IF (ABS(V(J,J)) .LT. EPS) GOTO 100
	   RC(J) = PSI(J)
	   DO K = 1,J-1
	      RC(J) = RC(J) - RC(K)*V(J,K)
	   END DO
	   V(J,J) = 1./V(J,J)
	   RC(J) = RC(J)*V(J,J)
	   RC(J) = MAX(MIN(RC(J),.999),-.999)
	END DO
	RETURN

*  Zero out higher order RC's if algorithm terminated early

100	DO I = J,ORDER
	   RC(I) = 0.
	END DO

*  Back substitute for PC's (if needed)

*110	DO J = ORDER,1,-1
*	   PC(J) = RC(J)
*	   DO I = 1,J-1
*	      PC(J) = PC(J) - PC(I)*V(J,I)
*	   END DO
*	END DO

	RETURN

	END
