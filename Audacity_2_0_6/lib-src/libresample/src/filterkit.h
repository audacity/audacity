/**********************************************************************

  resamplesubs.c

  Real-time library interface by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

**********************************************************************/

/* Definitions */
#include "resample_defs.h"

/*
 * FilterUp() - Applies a filter to a given sample when up-converting.
 * FilterUD() - Applies a filter to a given sample when up- or down-
 */

float lrsFilterUp(float Imp[], float ImpD[], UWORD Nwing, BOOL Interp,
                  float *Xp, double Ph, int Inc);

float lrsFilterUD(float Imp[], float ImpD[], UWORD Nwing, BOOL Interp,
                  float *Xp, double Ph, int Inc, double dhb);

void lrsLpFilter(double c[], int N, double frq, double Beta, int Num);
