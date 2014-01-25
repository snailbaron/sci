

#ifdef LOWRATE
#include "defines.h"
#endif /* LOWRATE */

/***********************************************************************
*
*     NSA LPC-10 Voice Coder
*
*   Unix C Version 
*
*      5 November 1990
*
*************************************************************************/

/*  Explanation of #ifdefs:
	sun		- to compile if running on the sun workstation
	_TMS320C30	- to compile if running on the C30
	BITS		- for saving bitstream information on the PC disk
	TURBO		- uses functions/routines from the Turbo C library
	TREES		- uses JSC's tree search routines
*/

#include <stdio.h>
#include "lpcdefs.h"
#include "config.ch"
#include "common.h"

#ifdef _TMS320C30
#include <string.h>
#endif  /* _TMS320C30 */

#ifdef sun
#define DISK 1
#endif

#define BUFSIZE 600

int count=0;

#ifdef _TMS320C30
AIC_Driver	aic_driver;
void		*aic_speech;
#endif /* _TMS320C30 */


int tau[LTAU] = {
  20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,
	35,36,37,38,39,40,42,44,46,48,50,52,54,56,58,60,62,64,66,
	68,70,72,74,76,78,80,84,88,92,96,100,104,108,112,116,120,
	124,128,132,136,140,144,148,152,156
};

int nframe, nfb, nbits, nunsfm, iclip, maxosp, NFBLK;
FILE *fbi, *fbo, *fopen();

/* analys */
float *inbuf, *pebuf, *lpbuf, *ivbuf;
float lparray[LBUFH-LBUFL+1], ivarray[PWINH-PWINL+1];
float pearray[SBUFH-SBUFL+1], inarray[SBUFH-SBUFL+1];
int vwin[2][AF], awin[2][AF], voibuf[2][AF+1];
float rmsbuf[AF], psi[MAXORD], rcbuf[MAXORD][AF];
float amdf[LTAU];
float phi[MAXORD][MAXORD];

/* bsynz.c */
float exc[MAXPIT+MAXORD], exc2[MAXPIT+MAXORD];
float noise[MAXPIT+MAXORD];

/* decode.c */
int drc[3][MAXORD], dpit[3], drms[3];

/* dyptrk */
float s[60];
int p[60][2];

/* onset */
float l2buf[16];

/* synths.c */
int ipiti[11], ivuv[11];
float rci[MAXORD][11], rmsi[11], pc[MAXORD];

main(argc,argv)
int argc;
char *argv[];
{
int          pitch;
float 			rms, rc[MAXORD];
int 			len;
int voice[2];

#if NOQUANT
int i,j, l=0;
#endif

#ifdef sun
short 		eof,	done=0;
static float 	speech[MAXFRM+MAXPIT];

#endif /* sun */

#ifdef ANALOG
static float  		lpc_speech[MAXFRM+MAXPIT];
float			*speech;
#endif /* ANALOG */

int i; /* this is only for some debugging purposes */

#ifdef PCM
printf("********** PCM Only in Main Processing loop ***********\n");
#endif /* PCM */

#ifdef sun
#ifndef FLEXIBLE
  setup();
#else
  setup(argc,argv);
#endif /* FLEXIBLE */
#endif /* sun */

#ifdef _TMS320C30
  setup();
#endif /* _TMS320C30 */

  initialize1();
  initialize2();

#ifdef TURBO

#ifdef NOQUANT
  printf("**** No Quantization ****\n");
#endif /* NOQUANT */
#ifdef ANALYZER
  printf("**** Analyzer Only ****\n");
#endif /* ANALYZER */
#ifdef SYNTHESIZER
  printf("**** Synthesizer Only ****\n");
#endif /* SYNTHESIZER */

#ifndef LOWRATE
  printf("Linear Predictive Coding at 2400 bps\n\n");
#endif /* LOWRATE */
#ifdef LOWRATE_600
  printf("Linear Predictive Coding with vector quantization at 600 bps \n\n");
  printf("\tUsing ");
#endif /* LOWRATE_600 */
#ifdef LOWRATE_800
  printf("Linear Predictive Coding with vector quantization at 800 bps \n\n");
  printf("\tUsing ");
#ifndef TREES
  printf("full search\n");
#else /* TREES */
  printf("tree search with");
#endif /* TREES */
#ifdef TREE_2x12
  printf("2 12-bit codebooks ");
#endif /* TREE_2x12 */
#ifdef UTREE_8
  printf("(Upper tree 8-bits) ");
#endif /* UTREE_8 */
#ifdef UTREE_7
  printf("(Upper tree 7-bits) ");
#endif /* UTREE_7 */
#ifdef UTREE_12
  printf("(Upper tree 12-bits) ");
#endif /* UTREE_12 */
#ifdef LTREE_9
  printf("(Lower tree 9-bits) ");
#endif /* LTREE_9 */
#ifdef LTREE_12
  printf("(Lower tree 12-bits) ");
#endif /* LTREE_12 */
#endif /* LOWRATE_800 */
#ifdef LOWRATE_1200
  printf("Linear Predictive Coding with vector quantization at 1200 bps \n\n");
#endif /* LOWRATE_1200 */

/* ---------------------- main processing loop begins -------------------*/
#ifdef _TMS320C30
  printf("\nPress any key to discontinue vocoding ....");	
  while(!kbhit()) 	
#endif /* _TMS320C30 */
#endif /* TURBO */

#ifndef TURBO
#ifdef _TMS320C30
  while(1)	
#endif /* _TMS320C30 */
#endif /* !TURBO */

#ifdef sun
  while(!done)
#endif /* sun */
  {

#if LOWRATE
nfb++;
if (nfb >= NFBLK) nfb = 0;

#endif /* LOWRATE */

#ifndef SYNTHESIZER
#ifdef ANALOG
/*   Read, Pre-process, and Analyze input speech */
    while(!AIC_get(aic_driver, (Ptr *)&aic_speech)) {  }
    float_buffer(aic_speech, LFRAME);
    scale_down(aic_speech, LFRAME);
    speech = (float *) aic_speech;
#endif /* ANALOG */
#ifdef DISK
    if (fdi != NULL) 
	diskio(0,fdi,speech,LFRAME,&eof,"",0);  
    if(eof==END) break;
#endif /* DISK */
#ifndef PCM

    hp100(speech);

    analys(speech, voice, &pitch, &rms, rc-1);

#endif /* !PCM */
#endif /* !SYNTHESIZER */

#ifdef NOQUANT
    pitdec(&pitch, &i);
    pitch = i;
#else /* NOQUANT */
    trans(voice-1, &pitch, &rms, rc);
#endif /* NOQUANT */

#ifndef ANALYZER
/*   Synthesize speech from received parameters */
#ifdef _TMS320C30
    speech = lpc_speech;
#endif
    synths(voice-1, &pitch, &rms, rc-1, speech-1, &len );

/* Equalize to LFRAME samples for output */
#ifdef ANALOG	
    buf_man(speech, aic_speech, len);
    scale_up(aic_speech, LFRAME);
	  fix_buffer(aic_speech, LFRAME);
    while(! AIC_put(aic_driver, (Ptr *)&aic_speech))
    {  }
#endif /* ANALOG */
#ifdef DISK
    buf_man(speech, speech, len);
    diskio(1,fdo,speech,LFRAME,&eof,"",0);
#endif /* DISK */
#endif /* !ANALYZER */

  }

#ifdef TURBO
	printf("\nThank you for using LPC!\n");
#endif
	
#ifdef _TMS320C30
  AIC_delete(aic_driver);
#endif

}

/*---------------------------------------------------------------------------------------------------------*/


#ifdef _TMS320C30
/*----------------------------------------------------------------------*/
void scale_down(array, length)
float array[];
int length;
{
int i;
/*float max = 0.0;
static float mmax = 0.0;*/

for(i=0;i<length;i++) {
  array[i] *= 3.05175781e-05;
	
}
}

/*----------------------------------------------------------------------*/
void scale_up(array, length)
float array[];
int length;
{
	int i;
	
	for(i=0;i<length;i++)  {
    array[i] = mmax(-32768, mmin(32767, array[i]*32768*3));
	}
	
}
#endif
/*----------------------------------------------------------------------*/
#if LOWRATE + NOQUANT
pitdec( pitch, ptau )
int *pitch, *ptau;
{
if ((*pitch >= 1) && (*pitch <= LTAU)) 
	   *ptau = tau[*pitch-1];
else
	   *ptau = 0;
}
#endif

/*----------------------------------------------------------------------*/
buf_man(inbuffer, outbuffer, len)
float outbuffer[], inbuffer[];
int len;
{
	static float big_buffer[BUFSIZE];
	static int sptr=0, eptr=360, first=1;
	int i;
	
	/* Initialize pseudo-circular buffer */
	if(first)	{
		first = 0;
		for(i=0;i<360;i++)
		  big_buffer[i] = 0.0;
	}
	
	
	/* write new data to end of buffer */
	for(i=0;i<len;i++)	{
		big_buffer[eptr++] = inbuffer[i];
		if (eptr == BUFSIZE) eptr = 0;
		/*eptr = (eptr+1) % BUFSIZE;*/
	}
	
	/* send next 180 samples */
	for(i=0;i<LFRAME;i++)	{
		outbuffer[i] = big_buffer[sptr++];
		if(sptr == BUFSIZE) sptr = 0;
		/*sptr = (sptr+1)%BUFSIZE;*/
	}
	
}


/* ------------------------ for debugging only below this line --------*/
#ifdef TURBO
look_int(array, len, offset)
int array[], len, offset;
{
int i;

for(i=offset;i<len+offset;i++)
{
  printf("\t[%d] = %d\n",i,array[i]);
}
}

look_float(array, len, offset)
float array[];
int len, offset;
{
	int i;
	
	for(i=offset;i<len+offset;i++)
	{
     printf("\t[%d] = %1.4f\n",i,array[i]);
	}
}
#endif /* TURBO */
