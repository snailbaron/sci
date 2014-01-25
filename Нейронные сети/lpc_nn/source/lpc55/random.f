***********************************************************************
*
*	RANDOM Version 49
*
**********************************************************************
*
*  Pseudo random number generator based on Knuth, Vol 2, p. 27.
*
* Function Return:
*  RANDOM - Integer variable, uniformly distributed over -32768 to 32767
*
	FUNCTION RANDOM ()
	INTEGER RANDOM, J, K, MIDTAP, MAXTAP
	PARAMETER (MIDTAP=2, MAXTAP=5)
	INTEGER*2 Y(MAXTAP)
	SAVE J, K, Y
	DATA Y /-21161, -8478, 30892,-10216, 16950/
	DATA J/MIDTAP/, K/MAXTAP/

*   The following is a 16 bit 2's complement addition,
*   with overflow checking disabled

	Y(K) = Y(K) + Y(J)
	RANDOM = Y(K)
	K = K - 1
	IF (K .LE. 0) K = MAXTAP
	J = J - 1
	IF (J .LE. 0) J = MAXTAP

	RETURN
	END
