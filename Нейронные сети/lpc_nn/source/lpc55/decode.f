******************************************************************
*
*	DECODE Version 54
*
******************************************************************
*
*   This subroutine provides error correction and decoding
*   for all LPC parameters
*
* INPUTS:
*  IPITV  - Index value of pitch
*  IRMS   - Coded Energy
*  IRC    - Coded Reflection Coefficients
*  CORRP  - Error correction:
*    If FALSE, parameters are decoded directly with no delay.  If TRUE,
*    most important parameter bits are protected by Hamming code and
*    median smoothed.  This requires an additional frame of delay.
* OUTPUTS:
*  VOICE  - Half frame voicing decisions
*  PITCH  - Decoded pitch
*  RMS    - Energy
*  RC     - Reflection coefficients
*
*  NOTE: Zero RC's should be done more directly, but this would affect
*   coded parameter printout.
*
	SUBROUTINE DECODE(IPITV, IRMS, IRC,
     1               VOICE, PITCH, RMS, RC )
	INCLUDE 'config.fh'
	INCLUDE 'contrl.fh'
	INTEGER IPITV, IRMS, IRC(MAXORD)
	INTEGER VOICE(2), PITCH
	REAL RMS, RC(ORDER)
	LOGICAL FIRST
	INTEGER IVP2H, IVOIC, IOVOIC
	INTEGER I, J, I1, I2, I4, IAVGP, ICORF,INDEX, IOUT
	INTEGER IPIT, IPTOLD, ISHIFT, IXCOR, LSB, MEDIAN
	INTEGER ERRCNT, IVTAB(32), DETAU(128), BIT(5), NBIT(10)
	INTEGER ERATE, ETHRS, ETHRS1, ETHRS2, ETHRS3
	INTEGER QB(8), DEADD(8), DETAB7(32), RMST(64)
	REAL DESCL(8), CORTH(4,8)
	INTEGER FUT, PRES, PAST
	PARAMETER( FUT=1, PRES=2, PAST=3 )
	INTEGER DRC(3,MAXORD), ZRC(MAXORD), DPIT(3), DRMS(3)

	DATA IVTAB/ 4*O'60600', 2*O'61610', O'61613', O'61610',
     1    O'40400', 3*O'3030', O'40400', O'3430', O'3033',
     1    O'3030', 2*O'60600', O'60433', O'60430', O'62621',
     1    O'62431', O'62473', O'62471', 2*O'3030', O'17170',
     1    O'7070', 2*O'3031', O'7073', O'7071' /

	DATA CORTH/32767.,10.,5.,0., 32767.,8.,4.,0.,
     1   32.,6.4,3.2,0., 32.,6.4,3.2,0., 32.,11.2,6.4,0.,
     1   32.,11.2,6.4,0., 16.,5.6,3.2,0., 16.,5.6,3.2,0. /

	DATA DETAU/ 0,0,0,3,0,3,3,31, 0,3,3,21,3,3,29,30,
     1         0,3,3,20,3,25,27,26, 3,23,58,22,3,24,28,3,
     1  	0,3,3,3,3,39,33,32, 3,37,35,36,3,38,34,3,
     1         3,42,46,44,50,40,48,3, 54,3,56,3,52,3,3,1,
     1  	0,3,3,108,3,78,100,104, 3,84,92,88,156,80,96,3,
     1  	3,74,70,72,66,76,68,3, 62,3,60,3,64,3,3,1,
     1  	3,116,132,112,148,152,3,3, 140,3,136,3,144,3,3,1,
     1         124,120,128,3,3,3,3,1, 3,3,3,1,3,1,1,1/

	DATA RMST/1024,936,856,784,718,656,600,550,
     1  	502,460,420,384,352,328,294,270,
     1  	246,226,206,188,172,158,144,132,
     1  	120,110,102,92,84,78,70,64,
     1  	60,54,50,46,42,38,34,32,
     1  	30,26,24,22,20,18,17,16,
     1  	15,14,13,12,11,10,9,8,
     1  	7,6,5,4,3,2,1,0/
	DATA DETAB7/4,11,18,25,32,39,46,53,60,66,72,77,82,87,92,96,101,
     1  	104,108,111,114,115,117,119,121,122,123,124,125,126,
     1  	127,127/

	DATA DESCL /.6953,.6250,.5781,.5469,.5312,.5391,.4688,.3828/
	DATA DEADD /1152,-2816,-1536,-3584,-1280,-2432,768,-1920/
	DATA QB /511,511,1023,1023,1023,1023,2047,4095/
	DATA NBIT /8,8,5,5,4,4,4,4,3,2/
	DATA ETHRS,ETHRS1,ETHRS2,ETHRS3/O'4000',O'200',O'2000',O'4000'/
	DATA ZRC /4*0,0,3,0,2,0,0/
	DATA BIT /2,4,8,16,32/
	DATA IAVGP/60/, IPTOLD/60/, FIRST/.TRUE./

	IF(LISTL.GE.3) WRITE(FDEBUG,800) IPITV,IRMS,(IRC(J),J=1,ORDER)
800	FORMAT(1X,' <<ERRCOR IN>>',T32,6X,I6,I5,T50,10I8)

*  If no error correction, do pitch and voicing then jump to decode

	I4 = DETAU(IPITV+1)
	IF(.NOT.CORRP) THEN
	   VOICE(1) = 1
	   VOICE(2) = 1
	   IF(IPITV.LE.1) VOICE(1) = 0
	   IF((IPITV.EQ.0).OR.(IPITV.EQ.2)) VOICE(2) = 0
	   PITCH = I4
	   IF(PITCH.LE.4) PITCH = IPTOLD
	   IF((VOICE(1).EQ.1).AND.(VOICE(2).EQ.1)) IPTOLD = PITCH
	   IF(VOICE(1).NE.VOICE(2)) PITCH = IPTOLD
	   GOTO 900
	END IF

*  Do error correction pitch and voicing

	IF(I4.GT.4) THEN
	   DPIT(FUT) = I4
	   IVOIC = 2
	   IAVGP = (15*IAVGP+I4+8)/16
	ELSE
	   IVOIC = I4
	   DPIT(FUT) = IAVGP
	END IF
	DRMS(FUT) = IRMS
	DO I = 1,ORDER
	   DRC(FUT,I) = IRC(I)
	END DO

*  Determine index to IVTAB from V/UV decision
*  If error rate is high then use alternate table

	INDEX = 16*IVP2H + 4*IOVOIC + IVOIC + 1
	I1 = IVTAB(INDEX)
	IPIT = AND(I1,3)
	ICORF = I1/8
	IF(ERATE.LT.ETHRS) ICORF = ICORF/64

*  Determine error rate:  4=high    1=low

	IXCOR = 4
	IF(ERATE.LT.ETHRS3) IXCOR = 3
	IF(ERATE.LT.ETHRS2) IXCOR = 2
	IF(ERATE.LT.ETHRS1) IXCOR = 1

*  Voice/unvoice decision determined from bits 0 and 1 of IVTAB

	VOICE(1) = AND(ICORF/2,1)
	VOICE(2) = AND(ICORF,1)

*  Skip decoding on first frame because present data not yet available

	IF(FIRST) THEN
	   FIRST = .FALSE.
	   GO TO 500
	END IF

*  If bit 4 of ICORF is set then correct RMS and RC(1) - RC(4).
*    Determine error rate and correct errors using a Hamming 8,4 code
*    during transition or unvoiced frame.  If IOUT is negative,
*    more than 1 error occurred, use previous frame's parameters.

	IF(AND(ICORF,BIT(4)).NE.0) THEN
	   ERRCNT = 0
	   LSB = AND(DRMS(PRES),1)
	   INDEX = DRC(PRES,8)*16 + DRMS(PRES)/2
	   CALL HAM84(INDEX,IOUT,ERRCNT)
	   DRMS(PRES) = DRMS(PAST)
	   IF(IOUT.GE.0) DRMS(PRES) = IOUT*2 + LSB

	   DO I = 1,4
	      IF(I.EQ.1) THEN
	         I1  = ( AND(DRC(PRES,9),7)*2 + AND(DRC(PRES,10),1) )
	      ELSE
	         I1  = AND(DRC(PRES,9-I),15)
	      END IF
	      I2 = AND(DRC(PRES,5-I),31)
	      LSB = AND(I2,1)
	      INDEX = 16*I1 + I2/2
	      CALL HAM84(INDEX,IOUT,ERRCNT)
	      IF(IOUT.GE.0) THEN
	         IOUT = IOUT*2+LSB
	         IF(AND(IOUT,16).EQ.16) IOUT = IOUT-32
	      ELSE
	         IOUT = DRC(PAST,5-I)
	      END IF
	      DRC(PRES,5-I) = IOUT
	   END DO

*  Determine error rate

	   ERATE = ERATE*.96875 + ERRCNT*102
	   IF(ERATE.NE.0 .AND. LISTL.GE.3) WRITE(FDEBUG,987) ERATE,ERRCNT
987	   FORMAT(' ERATE=',I6,'   ERRCNT=',I6)
	END IF

*  Get unsmoothed RMS, RC's, and PITCH

	IRMS = DRMS(PRES)
	DO I = 1,ORDER
	   IRC(I) = DRC(PRES,I)
	END DO
	IF(IPIT.EQ.1) DPIT(PRES) = DPIT(PAST)
	IF(IPIT.EQ.3) DPIT(PRES) = DPIT(FUT)
	PITCH = DPIT(PRES)

*  If bit 2 of ICORF is set then smooth RMS and RC's,

	IF(AND(ICORF,BIT(2)).NE.0) THEN
	   IF(  IABS(DRMS(PRES)-DRMS(FUT)) .GE. CORTH(IXCOR,2)
     1    .AND.IABS(DRMS(PRES)-DRMS(PAST)).GE. CORTH(IXCOR,2))
     1    IRMS = MEDIAN( DRMS(PAST), DRMS(PRES), DRMS(FUT) )
	   DO I = 1,6
	      IF(  IABS(DRC(PRES,I)-DRC(FUT,I)) .GE. CORTH(IXCOR,I+2)
     1       .AND.IABS(DRC(PRES,I)-DRC(PAST,I)).GE. CORTH(IXCOR,I+2))
     1       IRC(I) = MEDIAN( DRC(PAST,I), DRC(PRES,I), DRC(FUT,I) )
	   END DO
	END IF

*  If bit 3 of ICORF is set then smooth pitch

	IF(AND(ICORF,BIT(3)).NE.0) THEN
	   IF(  IABS(DPIT(PRES)-DPIT(FUT)) .GE. CORTH(IXCOR,1)
     1    .AND.IABS(DPIT(PRES)-DPIT(PAST)).GE. CORTH(IXCOR,1))
     1    PITCH = MEDIAN( DPIT(PAST), DPIT(PRES), DPIT(FUT) )
	END IF

*  If bit 5 of ICORF is set then RC(5) - RC(10) are loaded with
*  values so that after quantization bias is removed in decode
*  the values will be zero.

500	IF(AND(ICORF,BIT(5)).NE.0) THEN
	   DO I = 5,ORDER
	      IRC(I) = ZRC(I)
	   END DO
	END IF

*  House keeping  - one frame delay

	IOVOIC = IVOIC
	IVP2H = VOICE(2)
	DPIT(PAST) = DPIT(PRES)
	DPIT(PRES) = DPIT(FUT)
	DRMS(PAST) = DRMS(PRES)
	DRMS(PRES) = DRMS(FUT)
	DO I = 1,ORDER
	   DRC(PAST,I) = DRC(PRES,I)
	   DRC(PRES,I) = DRC(FUT,I)
	END DO

900	IF(LISTL.GE.3)WRITE(FDEBUG,801)VOICE,PITCH,IRMS,(IRC(J),J=1,ORDER)
801	FORMAT(1X,'<<ERRCOR OUT>>',T32,2I3,I6,I5,T50,10I8)

*   Decode RMS

	IRMS = RMST((31-IRMS)*2+1)

*  Decode RC(1) and RC(2) from log-area-ratios
*  Protect from illegal coded value (-16) caused by bit errors

	DO I = 1,2
	   I2 = IRC(I)
	   I1 = 0
	   IF(I2.LT.0) THEN
	      I1 = 1
	      I2 = -I2
	      IF(I2.GT.15) I2 = 0
	   END IF
	   I2 = DETAB7(2*I2+1)
	   IF(I1.EQ.1) I2 = -I2
	   ISHIFT = 15 - NBIT(I)
	   IRC(I) = I2*2**ISHIFT
	END DO

*  Decode RC(3)-RC(10) to sign plus 14 bits

	DO I = 3,ORDER
	   I2 = IRC(I)
	   ISHIFT = 15 - NBIT(I)
	   I2 = I2*2**ISHIFT
	   I2 = I2 + QB(I-2)
	   IRC(I) = I2*DESCL(I-2) + DEADD(I-2)
	END DO

	IF(LISTL.GE.3) WRITE(FDEBUG,811) IRMS, (IRC(I),I=1,ORDER)
811	FORMAT(1X,'<<DECODE OUT>>',T45,I4,1X,10I8)

*  Scale RMS and RC's to reals

	RMS = IRMS
	DO I = 1,ORDER
	   RC(I) = IRC(I) / 2.**14
	END DO

	RETURN
	END
