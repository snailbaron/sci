/***********************************************************************
*
*	SETUP Version 55 for Real Time Operation at multiple bit rates
*
*************************************************************************
*
*	Set processing options
*/


#if ANALYZER + SYNTHESIZER
#ifdef LOWRATE
#define IBITFILE "d:\\in_800.bit"
#define OBITFILE "d:\\out_800.bit"
#else
#define IBITFILE "d:\\in_2400.bit"
#define OBITFILE "d:\\out_2400.bit"
#endif
#endif



#ifdef LOWRATE
#include "defines.h"
#endif

#include "lpcdefs.h"
#include "config.ch"
#ifdef sun
#define DISK 1
#include "common.h"
#endif
#ifdef LOWRATE
#include "vqcomm.ch"
#endif
#include "contrl.ch"

#ifdef ANALOG
#include "drivers.h"
#include "os.h"
extern AIC_Driver	aic_driver;
extern Int		*aic_speech;
AIC_Attrs	aic_params;
#endif  /* ANALOG */

#ifdef DISK
#include <stdio.h>
#endif

#ifdef sun
#ifndef FLEXIBLE
setup()
#else
setup(nargs, args)
int nargs;
char *args[];
#endif /* FLEXIBLE */
#endif  /* sun */

#ifdef _TMS320C30
setup()
#endif
{
#ifdef sun
char fname[80], fname2[80], line[80];
#endif

#ifdef DISK
FILE *fopen();
#endif

/* 
	Call to getcl() gets name of input and output file and message level.
Real time doesn't need messages about performance, etc, so those will be 
eliminated.
*/

#ifdef LOWRATE /* this takes the place of a call to vqgetcl() */
#ifdef LOWRATE_600
  nfi = 4;
  ncv = 16;
  ncp = 1;
  nspl = 2;
  nde = 4;
  ncp = 32;
  nevq = 2;
  nft = 5;
  NFBLK = 8;
#endif
#ifdef LOWRATE_800
  nfi = 4;
  ncv = 16;
  ncp = 1;
#ifdef SPLIT2
  nspl = 2;
#endif
#ifdef SPLIT3
  nspl = 3;
#endif
  nde = 4;
  ncp = 32;
  nevq = 2;
  nft = 5;
  NFBLK = 8;
#endif
#ifdef LOWRATE_1200
  nfi = 3;
  ncv = 0;
  ncp = 0;
  nspl = 3;
  nde = 4;
  nevq = 1;
  nft = 3;
  NFBLK = 4;
#endif
#endif

#ifdef sun
#ifndef FLEXIBLE
printf("Enter input file name if NOT 3m3f.spd -> ");
if (getline(line, sizeof(line)) == 1)
  strcpy(fname, "3m3f.spd");
else
  sscanf(line, "%s", fname);
fdi = fopen(fname, "rb");
if(fdi<3)	{
  printf("Problem opening %s [1] ... program exiting\n",fname);
  exit(1);
}
else
  printf("Opened %s sucessfully\n",fname);

printf("Enter output file name if NOT 3m3f_lpc.spd -> ");
if (getline(line, sizeof(line)) == 1)
  strcpy(fname2, "3m3f_lpc.spd");
else
  sscanf(line, "%s", fname2);
fdo = fopen(fname2, "wb");
if(fdo<3)	{
  printf("Problem opening %s [2] ... program exiting\n",fname2);
  exit(1);
}
else
  printf("Opened %s sucessfully\n",fname2);
#endif

#ifdef FLEXIBLE
if (nargs < 3) {
  printf("Usage: %s inputfile outputfile\n", args[0]);
  exit(1);
}
else {
#ifdef SYNTHESIZER
  fbi = fopen(args[1], "r");
  if(fbi == NULL)	{
#else
  fdi = fopen(args[1], "rb");
  if(fdi==NULL)	{
#endif
    printf("Problem opening [3] %s ... program exiting\n",args[1]);
    exit(1);
  }
  else
    printf("Opened %s sucessfully\n",args[1]);


#ifdef ANALYZER
  fbo = fopen(args[2], "w");
  if(fbo == NULL)	{
#else
  fdo = fopen(args[2], "w");
  if(fdo == NULL)	{
#endif
    printf("Problem opening [4] ... program exiting %s\n",args[2]);
    exit(1);
  }
  else
    printf("Opened %s sucessfully\n",args[2]);

}
#endif

#endif

#ifdef _TMS320C30
/*   Set processing options, open files */
  set_aic8k(&aic_params, 0, LFRAME, LFRAME);
  aic_driver = AIC_create(&aic_params);
  aic_speech = (Int *)os_malloc(0, LFRAME, 0);

#ifdef ANALYZER
  fbo = fopen(OBITFILE, "w");
  if (fbo == NULL)
	{
     printf("Problem opening [5] ...program  exiting %s\n",OBITFILE);
		exit(0);
	}
	else
	  printf("%s opened successfully\n",OBITFILE);
#endif /* ANALYZER */

#ifdef SYNTHESIZER
  fbi = fopen(IBITFILE, "r");
	if(fbi == NULL)	{
     printf("Problem opening [6] ... program exiting %s\n",IBITFILE);
		exit(0);
	}
  else printf("%s opened successfully\n",IBITFILE);

#endif /* SYNTHESIZER */

#if IN_DISK
#if FR + ASC
  infile = fopen(IFILE, "r");
  if(infile == NULL)
#else
  infile = open(IFILE, O_RDONLY, 0);
	if(infile < 0)
#endif
	{
     printf("Problem opening input file: %s [7] ... program exiting\n",IFILE);
		exit(1);
	}
  else printf("%s opened for input \n",IFILE);
#endif /* IN_DISK  */


#endif

#ifdef LOWRATE
/* Setup for low rate */
  vqsetup();

#endif
}

/*-------------------------------------------------------------------*/
#ifdef sun
int getline(line, max)
char *line;
int max;
{

	if(fgets(line, max, stdin) == NULL)
		return 0;
	else
		return strlen(line);
}

#endif
