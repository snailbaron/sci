*******************************************************************
*
*	ANALYS Version 55
*
*******************************************************************

	SUBROUTINE ANALYS(SPEECH, VOICE, PITCH, RMS, RC)
	INCLUDE 'config.fh'
	INCLUDE 'contrl.fh'
	INTEGER VOICE(2), PITCH
	REAL RMS, RC(ORDER), SPEECH(LFRAME)

*  Constants
*    NF =     Number of frames
*    AF =     Frame in which analysis is done
*    OSLEN =  Length of the onset buffer
*    LTAU =   Number of pitch lags
*    SBUFL, SBUFH =   Start and end index of speech buffers
*    LBUFL, LBUFH =   Start and end index of LPF speech buffer
*    MINWIN, MAXWIN = Min and Max length of voicing (and analysis) windows
*    PWLEN, PWINH, PWINL = Length, upper and lower limits of pitch window
*    DVWINL, DVWINH = Default lower and upper limits of voicing window

	INTEGER NF, AF, OSLEN, LTAU, SBUFL, SBUFH, LBUFL, LBUFH
	INTEGER MINWIN, MAXWIN, PWLEN, PWINL, PWINH, DVWINL, DVWINH
	PARAMETER (NF=4, AF=3, OSLEN=10, LTAU=60)
	PARAMETER (SBUFL=(AF-2)*MAXFRM+1, SBUFH=NF*MAXFRM)
	PARAMETER (LBUFL=(AF-2)*MAXFRM-MAXPIT+1, LBUFH=NF*MAXFRM)
	PARAMETER (MINWIN=90, MAXWIN=156)
	PARAMETER (PWLEN=MAXPIT+MAXWIN)
	PARAMETER (PWINH=AF*MAXFRM, PWINL=PWINH-PWLEN+1)
	PARAMETER (DVWINL=PWINH-PWLEN/2-MAXWIN/2+1)
	PARAMETER (DVWINH=DVWINL+MAXWIN-1)

*  Data Buffers
*    INBUF	Raw speech (with DC bias removed each frame)
*    PEBUF	Preemphasized speech
*    LPBUF	Low pass speech buffer
*    IVBUF	Inverse filtered speech
*    OSBUF	Indexes of onsets in speech buffers
*    VWIN	Voicing window indices
*    AWIN	Analysis window indices
*    EWIN	Energy window indices
*    VOIBUF	Voicing decisions on windows in VWIN
*    RMSBUF	RMS energy
*    RCBUF	Reflection Coefficients
*
*  Pitch is handled separately from the above parameters.
*  The following variables deal with pitch:
*    MIDX	Encoded initial pitch estimate for analysis frame
*    IPITCH	Initial pitch computed for frame AF (decoded from MIDX)
*    PITCH 	The encoded pitch value (index into TAU) for the present
*		frame (delayed and smoothed by Dyptrack)

	REAL INBUF(SBUFL:SBUFH), PEBUF(SBUFL:SBUFH)
	REAL LPBUF(LBUFL:LBUFH), IVBUF(PWINL:PWINH)
	REAL AMDF(LTAU), ABUF(MAXWIN), BIAS, TEMP
	INTEGER OSBUF(OSLEN), OSPTR, OBOUND(AF)
	INTEGER VWIN(2,AF), AWIN(2,AF), EWIN(2,AF), VOIBUF(2,0:AF)
	REAL RMSBUF(AF), RCBUF(MAXORD, AF)
	REAL PRECOEF, ZPRE

	INTEGER I, J, LANAL, HALF, TAU(LTAU)
	INTEGER IPITCH, PTAU, MINPTR, MAXPTR, MINTAU, MIDX
	REAL IVRC(2), PHI(MAXORD,MAXORD), PSI(MAXORD)
	INTEGER BUFLIM(4)
	DATA BUFLIM / SBUFL, SBUFH, LBUFL, LBUFH /

	DATA PRECOEF/.9375/, ZPRE/0./
	DATA BIAS/0/
	DATA OSPTR/1/
	DATA VWIN(1,AF) /DVWINL/
	DATA VWIN(2,AF) /DVWINH/
	DATA AWIN(1,AF) /DVWINL/
	DATA AWIN(2,AF) /DVWINH/
	DATA TAU/20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
     1    35,36,37,38,39,40,42,44,46,48,50,52,54,56,58,60,62,64,66,
     1    68,70,72,74,76,78,80,84,88,92,96,100,104,108,112,116,120,
     1    124,128,132,136,140,144,148,152,156/

	IF(LISTL.GE.3) THEN
	   WRITE(FDEBUG,900) NFRAME
900	   FORMAT(1X,//,65(2H- ),//,' ANALYSIS DATA -- FRAME',I6/)
	END IF

*   Calculations are done on future frame due to requirements
*   of the pitch tracker.  Delay RMS and RC's 2 frames to give
*   current frame parameters on return.
*   Update all buffers

	DO I = SBUFL, SBUFH-LFRAME
	   INBUF(I) = INBUF(LFRAME+I)
	   PEBUF(I) = PEBUF(LFRAME+I)
	END DO
	DO I = PWINL,PWINH-LFRAME
	   IVBUF(I) = IVBUF(LFRAME+I)
	END DO
	DO I = LBUFL,LBUFH-LFRAME
	   LPBUF(I) = LPBUF(LFRAME+I)
	END DO

	J=1
	DO I = 1, OSPTR-1
	   IF (OSBUF(I) .GT. LFRAME) THEN
	      OSBUF(J)=OSBUF(I)-LFRAME
	      J=J+1
	   END IF
	END DO
	OSPTR=J

	VOIBUF(1,0) = VOIBUF(1,1)
	VOIBUF(2,0) = VOIBUF(2,1)
	DO I = 1, AF-1
	   VWIN(1,I) = VWIN(1,I+1) - LFRAME
	   VWIN(2,I) = VWIN(2,I+1) - LFRAME
	   AWIN(1,I) = AWIN(1,I+1) - LFRAME
	   AWIN(2,I) = AWIN(2,I+1) - LFRAME
	   EWIN(1,I) = EWIN(1,I+1) - LFRAME
	   EWIN(2,I) = EWIN(2,I+1) - LFRAME
	   OBOUND(I) = OBOUND(I+1)
	   VOIBUF(1,I) = VOIBUF(1,I+1)
	   VOIBUF(2,I) = VOIBUF(2,I+1)
	   RMSBUF(I) = RMSBUF(I+1)
	   DO J = 1, ORDER
	      RCBUF(J,I) = RCBUF(J,I+1)
	   END DO
	END DO

*   Copy input speech, scale to sign+12 bit integers
*   Remove long term DC bias.

	TEMP = 0
	DO I = 1,LFRAME
	   INBUF(SBUFH-LFRAME+I) = SPEECH(I)*4096. - BIAS
	   TEMP = TEMP + INBUF(SBUFH-LFRAME+I)
	END DO
	IF( TEMP.GT. LFRAME ) BIAS = BIAS + 1
	IF( TEMP.LT.-LFRAME ) BIAS = BIAS - 1

*   Place Voicing Window

	I = SBUFH + 1 - LFRAME
	CALL PREEMP(INBUF(I), PEBUF(I), LFRAME, PRECOEF, ZPRE)
	CALL ONSET( PEBUF, OSBUF, OSPTR, OSLEN,
     1    SBUFL, SBUFH, LFRAME )
	MAXOSP = MAX( MAXOSP, OSPTR )
	CALL PLACEV( OSBUF, OSPTR, OSLEN, OBOUND(AF), VWIN, AF,
     1    LFRAME, MINWIN, MAXWIN, DVWINL, DVWINH )

*        The Pitch Extraction algorithm estimates the pitch for a frame
*   of speech by locating the minimum of the average magnitude difference
*   function (AMDF).  The AMDF operates on low-pass, inverse filtered
*   speech.  (The low-pass filter is an 800 Hz, 19 tap, equiripple, FIR
*   filter and the inverse filter is a 2nd-order LPC filter.)  The pitch
*   estimate is later refined by dynamic programming (DYPTRK).  However,
*   since some of DYPTRK's parameters are a function of the voicing
*   decisions, a voicing decision must precede the final pitch estimation.
*   See subroutines LPFILT, IVFILT, and TBDM. 

	CALL LPFILT( INBUF(LBUFH+1-PWLEN), LPBUF(LBUFH+1-PWLEN),
     1    PWLEN, LFRAME )
	CALL IVFILT( LPBUF(PWINL), IVBUF(PWINL), PWLEN, LFRAME, IVRC )
	CALL TBDM( IVBUF(PWINL), MAXWIN, TAU, LTAU, AMDF,
     1    MINPTR, MAXPTR, MINTAU )

*        Voicing decisions are made for each half frame of input speech.
*   An initial voicing classification is made for each half of the
*   analysis frame, and the voicing decisions for the present frame
*   are finalized.  See subroutine VOICIN.
*        The voicing detector (VOICIN) classifies the input signal as
*   unvoiced (including silence) or voiced using the AMDF windowed
*   maximum-to-minimum ratio, the zero crossing rate, energy measures,
*   reflection coefficients, and prediction gains. 
*        The pitch and voicing rules apply smoothing and isolated
*   corrections to the pitch and voicing estimates and, in the process,
*   introduce two frames of delay into the corrected pitch estimates and 
*   voicing decisions.

	DO HALF = 1,2
	   CALL VOICIN( VWIN(1,AF), INBUF, LPBUF, BUFLIM, HALF,
     1    AMDF(MINPTR), AMDF(MAXPTR), MINTAU, IVRC, OBOUND, VOIBUF, AF )
	END DO

*   Find the minimum cost pitch decision over several frames
*   given the current voicing decision and the AMDF array

	CALL DYPTRK( AMDF, LTAU, MINPTR, VOIBUF(2,AF), PITCH, MIDX )
	IPITCH = TAU(MIDX)

*   Place spectrum analysis and energy windows

	CALL PLACEA( IPITCH, VOIBUF, OBOUND(AF), AF,
     1    VWIN, AWIN, EWIN, LFRAME, MAXWIN )

*   Remove short term DC bias over the analysis window, Put result in ABUF

	LANAL = AWIN(2,AF) + 1 - AWIN(1,AF)
	CALL DCBIAS( LANAL, PEBUF(AWIN(1,AF)), ABUF )

*   Compute RMS over integer number of pitch periods within the
*   analysis window.
*   Note that in a hardware implementation this computation may be
*   simplified by using diagonal elements of PHI computed by MLOAD.

	CALL ENERGY( EWIN(2,AF)-EWIN(1,AF)+1,
     1              ABUF(EWIN(1,AF)-AWIN(1,AF)+1), RMSBUF(AF) )

*   Matrix load and invert, check RC's for stability

	CALL MLOAD( ORDER, 1, LANAL, ABUF, PHI, PSI )
	CALL INVERT( ORDER, PHI, PSI, RCBUF(1,AF) )
	CALL RCCHK( ORDER, RCBUF(1,AF-1), RCBUF(1,AF) )

*   Set return parameters

	VOICE(1) = VOIBUF(1,AF-2)
	VOICE(2) = VOIBUF(2,AF-2)
	RMS = RMSBUF(AF-2)
	DO I = 1,ORDER
	   RC(I) = RCBUF(I,AF-2)
	END DO

*   Print out test data

	IF(LISTL.GE.3) THEN
	   IF(LISTL.GE.4) THEN
	      IF(LISTL.GE.6) THEN
	         WRITE(FDEBUG,980) 'INBUF:',INBUF
	         WRITE(FDEBUG,980) 'LPBUF:',LPBUF
	         WRITE(FDEBUG,980) 'IVBUF:',IVBUF
	         WRITE(FDEBUG,980) 'PEBUF:',PEBUF
	      END IF
	      WRITE(FDEBUG,980) 'AMDF:',AMDF
	   END IF
	   IF(OSPTR.GT.1) WRITE(FDEBUG,970)
     1    'OSBUF Onset Locations:', (OSBUF(I),I=1,OSPTR-1)
	   IF(LISTL.GE.4) THEN
	      WRITE(FDEBUG,980) 'PHI Matrix Values:',
     1       ((PHI(I,J),J=1,ORDER),I=1,ORDER)
	      WRITE(FDEBUG,980) 'PSI Vector Values:',PSI
970	      FORMAT(1X,A,100(/1X,20I6))
980	      FORMAT(1X,A,100(/1X,10F12.1))
	   END IF
	   WRITE(FDEBUG,990)
990	   FORMAT('  FRAME   AWIN     EWIN   BIAS',T34,
     1    'V/UV  Pitch RMS',T54,
     1    'RC1     RC2     RC3     RC4     RC5     ',
     1    'RC6     RC7     RC8     RC9    RC10')
	   WRITE(FDEBUG,992) NFRAME, AWIN(1,AF), AWIN(2,AF),
     1    EWIN(1,AF), EWIN(2,AF), BIAS,
     1    VOIBUF(2,AF), IPITCH, RMSBUF(AF), (RCBUF(I,AF),I=1,ORDER)
992	   FORMAT(1X,I6,2I4,1X,2I4,F6.1,T34,I2,I8,F6.0,T50,10F8.3)
	END IF

	RETURN

********************************************************************
*   Decode pitch index (PITCH) to pitch period (PTAU)
********************************************************************

	ENTRY PITDEC( PITCH, PTAU )

	IF (PITCH .GE. 1 .AND. PITCH .LE. LTAU) THEN
	   PTAU = TAU(PITCH)
	ELSE
	   PTAU = 0
	END IF
	RETURN
	END
