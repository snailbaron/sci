******************************************************************
*
*	DEEMP Version 48
*
******************************************************************
*
*  De-Emphasize output speech with   1 / ( 1 - .75z**-1 )
*    cascaded with 200 Hz high pass filter
*    ( 1 - 1.9998z**-1 + z**-2 ) / ( 1 - 1.75z**-1 + .78z**-2 )
*
* Input:
*  N  - Number of samples
* In/Output:
*  X  - Speech
*
	SUBROUTINE DEEMP( X, N )
	INTEGER N, K
	REAL X(N)
	REAL DEI0, DEI1, DEI2, DEO1, DEO2, DEO3

	DO K = 1,N
	   DEI0 = X(K)
	   X(K) = X(K) - 1.9998*DEI1 + DEI2
     1     + 2.5*DEO1 - 2.0925*DEO2 + .585*DEO3
	   DEI2 = DEI1
	   DEI1 = DEI0
	   DEO3 = DEO2
	   DEO2 = DEO1
	   DEO1 = X(K)
	END DO

	RETURN
	END
