******************************************************************
*
*	MLOAD Version 48
*
******************************************************************
*
*  Load a covariance matrix
*
*  Inputs:
*    ORDER            - Analysis order
*    AWINS            - Analysis window start
*    AWINF            - Analysis window finish
*    SPEECH(AWINF)    - Speech buffer
*  Outputs:
*    PHI(ORDER,ORDER) - Covariance matrix
*    PSI(ORDER)       - Prediction vector
*

	SUBROUTINE MLOAD( ORDER, AWINS, AWINF, SPEECH, PHI, PSI )
	INTEGER ORDER, AWINS, AWINF
	REAL SPEECH(AWINF)
	REAL PHI(ORDER,ORDER), PSI(ORDER)
	INTEGER R, C, I, START

*   Load first column of triangular covariance matrix PHI

	START = AWINS + ORDER
	DO R = 1,ORDER
	   PHI(R,1) = 0.
	   DO I = START,AWINF
	      PHI(R,1) = PHI(R,1) + SPEECH(I-1)*SPEECH(I-R)
	   END DO
	END DO

*   Load last element of vector PSI

	PSI(ORDER) = 0.
	DO I = START,AWINF
	   PSI(ORDER) = PSI(ORDER) + SPEECH(I)*SPEECH(I-ORDER)
	END DO

*   End correct to get additional columns of PHI

	DO R = 2,ORDER
	   DO C = 2,R
	      PHI(R,C) = PHI(R-1,C-1)
     1                  - SPEECH(AWINF+1-R)*SPEECH(AWINF+1-C)
     1                    + SPEECH(START-R)*SPEECH(START-C)
	   END DO
	END DO

*   End correct to get additional elements of PSI

	DO C = 1,ORDER-1
	   PSI(C) = PHI(C+1,1) - SPEECH(START-1)*SPEECH(START-1-C)
     1                          + SPEECH(AWINF)*SPEECH(AWINF-C)
	END DO

*   Copy lower triangular section into upper (why bother?)

	DO R = 1,ORDER
	   DO C = 1,R-1
	      PHI(C,R) = PHI(R,C)
	   END DO
	END DO

	RETURN
	END
