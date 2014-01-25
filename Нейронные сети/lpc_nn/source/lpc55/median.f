**********************************************************************
*
*	MEDIAN Version 45G
*
**********************************************************************
*
*  Find median of three values
*
* Input:
*  D1,D2,D3 - Three input values
* Output:
*  MEDIAN - Median value
*
	FUNCTION MEDIAN( D1, D2, D3 )
	INTEGER D1, D2, D3, MEDIAN

	MEDIAN = D2
	IF    ( D2 .GT. D1 .AND. D2 .GT. D3 ) THEN
	   MEDIAN = D1
	   IF ( D3 .GT. D1 ) MEDIAN = D3
	ELSEIF( D2 .LT. D1 .AND. D2 .LT. D3 ) THEN
	   MEDIAN = D1
	   IF ( D3 .LT. D1 ) MEDIAN = D3
	END IF

	RETURN
	END
