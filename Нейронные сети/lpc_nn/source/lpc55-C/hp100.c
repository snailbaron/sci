/**********************************************************************
*
*      HP100 Version 55
*
**********************************************************************
*
*    100 Hz High Pass Filter
*
* Jan 92 - corrected typo (1.937148 to 1.935715),
*          rounded coefficients to 7 places,
*          corrected and merged gain (.97466**4),
*          merged numerator into first two sections.
*/

#include "lpcdefs.h"

hp100(speech)
float speech[];
{
int i;
float si, err;
static float z11=0.0, z21=0.0, z12=0.0, z22=0.0;

for( i = 0;i<LFRAME;i++)	{
    si = speech[i];

    err = si + 1.859076*z11 - .8648249*z21;
    si = err - 2.00*z11 + z21;
    z21 = z11;
    z11 = err;

    err = si + 1.935715*z12 - .9417004*z22;
    si = err - 2.00*z12 + z22;
    z22 = z12;
    z12 = err;

    speech[i] = .902428*si;
}

}
