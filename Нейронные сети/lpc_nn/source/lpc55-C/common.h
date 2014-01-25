
/* This file takes care of the common I/O variables */

#include <stdio.h>

#ifdef sun
FILE *fdi, *fdo;
#endif

#ifdef _TMS320C30
#include "drivers.h"

#ifdef BITS
#include "bits.h"
#endif

#endif
