**********************************************************************
*
*	RCCHK Version 45G
*
**********************************************************************
*
*  Check RC's, repeat previous frame's RC's if unstable
*
* Inputs:
*  ORDER - Number of RC's
*  RC1F  - Previous frame's RC's
* In/Outputs:
*  RC2F  - Present frame's RC's
*
	SUBROUTINE RCCHK( ORDER, RC1F, RC2F )
	INTEGER ORDER, I
	REAL RC1F(ORDER), RC2F(ORDER)

	DO I = 1,ORDER
	   IF(ABS(RC2F(I)).GT..99) GOTO 10
	END DO
	RETURN

10	CALL ERROR('RCCHK',2,I)
	DO I = 1,ORDER
	   RC2F(I) = RC1F(I)
	END DO
	RETURN
	END
