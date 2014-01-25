/*********************************************************************
*
*	TRANS Version 54
*
**********************************************************************
*
*   Handle Quantization and Input/Output of LPC parameters
*
* Input:
*  ORDER - Prediction order
* In/Outputs:
*  VOICE - Half frame voicing decisions
*  PITCH - Pitch index
*  RMS   - Energy
*  RC    - Reflection coefficients
*  EOF   - End of file flag
*/

#ifdef LOWRATE
#include "defines.h"
#endif

#include <stdio.h>
#include "config.ch"
#include "contrl.ch"
#include "lpcdefs.h"


trans(voice, pitch, rms, rc)
int voice[2], *pitch;
float *rms, rc[ORDER];
{
int i, ipitv, irms, irc[MAXORD], ibits[MAXNB];

/* Initialization */
memset(ibits, 0, MAXNB*sizeof(int));

#ifndef LOWRATE
#if ANALYZER + SYNTHESIZER
	nbits = 54;
#endif /* ANALYZER + SYNTHESIZER */
#endif /* !LOWRATE */



/*     Quantize to 2400 bps, 600 bps, 800 bps or 1200 bps	*/

#ifndef SYNTHESIZER
#ifndef LOWRATE
  encode(voice, pitch, rms, rc-1, &ipitv, &irms, irc-1);
  channel(0, &ipitv, &irms, irc-1, ibits-1);
#endif /* !LOWRATE */

#ifdef LOWRATE 
  vqencode(voice, *pitch, *rms, rc-1, ibits-1);
#endif /* LOWRATE */
#endif  /* !SYNTHESIZER */

#ifdef ANALYZER
#ifdef LOWRATE
  if(nfb >= NFBLK-1)
#endif  /* LOWRATE */
    bitio(2, fbo, ibits, nbits);
#endif /* ANALYZER */

#ifdef SYNTHESIZER
#ifdef LOWRATE
  if(nfb >= NFBLK-1)
#endif  /* LOWRATE */
    if(!bitio(1, fbi, ibits, nbits)) 
      exit(0);
#endif /* SYNTHESIZER */

/*  Decode parameters from bitstream	*/
#ifndef ANALYZER
#ifndef LOWRATE
  channel(1, &ipitv, &irms, irc-1, ibits-1);
  decode(ipitv, &irms, irc-1, voice, pitch, rms, rc-1);
#endif /* !LOWRATE */

#ifdef LOWRATE
  vqdecode(voice, pitch, rms, rc-1, ibits-1);
#endif /* LOWRATE */
#endif /* !ANALYZER */

}
