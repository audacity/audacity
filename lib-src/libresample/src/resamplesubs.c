/**********************************************************************

  resamplesubs.c

  Real-time library interface by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

  This file provides the routines that do sample-rate conversion
  on small arrays, calling routines from filterkit.

**********************************************************************/

/* Definitions */
#include "resample_defs.h"

#include "filterkit.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

/* Sampling rate up-conversion only subroutine;
 * Slightly faster than down-conversion;
 */
int lrsSrcUp(float X[],
             float Y[],
             double factor,
             double *TimePtr,
             UWORD Nx,
             UWORD Nwing,
             float LpScl,
             float Imp[],
             float ImpD[],
             BOOL Interp)
{
    float *Xp, *Ystart;
    float v;
    
    double CurrentTime = *TimePtr;
    double dt;                 /* Step through input signal */ 
    double endTime;            /* When Time reaches EndTime, return to user */
    
    dt = 1.0/factor;           /* Output sampling period */
    
    Ystart = Y;
    endTime = CurrentTime + Nx;
    while (CurrentTime < endTime)
    {
        double LeftPhase = CurrentTime-floor(CurrentTime);
        double RightPhase = 1.0 - LeftPhase;

        Xp = &X[(int)CurrentTime]; /* Ptr to current input sample */
        /* Perform left-wing inner product */
        v = lrsFilterUp(Imp, ImpD, Nwing, Interp, Xp,
                        LeftPhase, -1);
        /* Perform right-wing inner product */
        v += lrsFilterUp(Imp, ImpD, Nwing, Interp, Xp+1, 
                         RightPhase, 1);

        v *= LpScl;   /* Normalize for unity filter gain */

        *Y++ = v;               /* Deposit output */
        CurrentTime += dt;      /* Move to next sample by time increment */
    }

    *TimePtr = CurrentTime;
    return (Y - Ystart);        /* Return the number of output samples */
}

/* Sampling rate conversion subroutine */

int lrsSrcUD(float X[],
             float Y[],
             double factor,
             double *TimePtr,
             UWORD Nx,
             UWORD Nwing,
             float LpScl,
             float Imp[],
             float ImpD[],
             BOOL Interp)
{
    float *Xp, *Ystart;
    float v;

    double CurrentTime = (*TimePtr);
    double dh;                 /* Step through filter impulse response */
    double dt;                 /* Step through input signal */
    double endTime;            /* When Time reaches EndTime, return to user */
    
    dt = 1.0/factor;            /* Output sampling period */
    
    dh = MIN(Npc, factor*Npc);  /* Filter sampling period */
    
    Ystart = Y;
    endTime = CurrentTime + Nx;
    while (CurrentTime < endTime)
    {
        double LeftPhase = CurrentTime-floor(CurrentTime);
        double RightPhase = 1.0 - LeftPhase;

        Xp = &X[(int)CurrentTime];     /* Ptr to current input sample */
        /* Perform left-wing inner product */
        v = lrsFilterUD(Imp, ImpD, Nwing, Interp, Xp,
                        LeftPhase, -1, dh);
        /* Perform right-wing inner product */
        v += lrsFilterUD(Imp, ImpD, Nwing, Interp, Xp+1, 
                         RightPhase, 1, dh);

        v *= LpScl;   /* Normalize for unity filter gain */
        *Y++ = v;               /* Deposit output */
        
        CurrentTime += dt;      /* Move to next sample by time increment */
    }

    *TimePtr = CurrentTime;
    return (Y - Ystart);        /* Return the number of output samples */
}
