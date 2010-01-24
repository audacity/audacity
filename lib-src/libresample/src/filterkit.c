/**********************************************************************

  resamplesubs.c

  Real-time library interface by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

  This file provides Kaiser-windowed low-pass filter support,
  including a function to create the filter coefficients, and
  two functions to apply the filter at a particular point.

**********************************************************************/

/* Definitions */
#include "resample_defs.h"

#include "filterkit.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

/* LpFilter()
 *
 * reference: "Digital Filters, 2nd edition"
 *            R.W. Hamming, pp. 178-179
 *
 * Izero() computes the 0th order modified bessel function of the first kind.
 *    (Needed to compute Kaiser window).
 *
 * LpFilter() computes the coeffs of a Kaiser-windowed low pass filter with
 *    the following characteristics:
 *
 *       c[]  = array in which to store computed coeffs
 *       frq  = roll-off frequency of filter
 *       N    = Half the window length in number of coeffs
 *       Beta = parameter of Kaiser window
 *       Num  = number of coeffs before 1/frq
 *
 * Beta trades the rejection of the lowpass filter against the transition
 *    width from passband to stopband.  Larger Beta means a slower
 *    transition and greater stopband rejection.  See Rabiner and Gold
 *    (Theory and Application of DSP) under Kaiser windows for more about
 *    Beta.  The following table from Rabiner and Gold gives some feel
 *    for the effect of Beta:
 *
 * All ripples in dB, width of transition band = D*N where N = window length
 *
 *               BETA    D       PB RIP   SB RIP
 *               2.120   1.50  +-0.27      -30
 *               3.384   2.23    0.0864    -40
 *               4.538   2.93    0.0274    -50
 *               5.658   3.62    0.00868   -60
 *               6.764   4.32    0.00275   -70
 *               7.865   5.0     0.000868  -80
 *               8.960   5.7     0.000275  -90
 *               10.056  6.4     0.000087  -100
 */

#define IzeroEPSILON 1E-21               /* Max error acceptable in Izero */

static double Izero(double x)
{
   double sum, u, halfx, temp;
   int n;

   sum = u = n = 1;
   halfx = x/2.0;
   do {
      temp = halfx/(double)n;
      n += 1;
      temp *= temp;
      u *= temp;
      sum += u;
   } while (u >= IzeroEPSILON*sum);
   return(sum);
}

void lrsLpFilter(double c[], int N, double frq, double Beta, int Num)
{
   double IBeta, temp, temp1, inm1;
   int i;

   /* Calculate ideal lowpass filter impulse response coefficients: */
   c[0] = 2.0*frq;
   for (i=1; i<N; i++) {
      temp = PI*(double)i/(double)Num;
      c[i] = sin(2.0*temp*frq)/temp; /* Analog sinc function, cutoff = frq */
   }

   /* 
    * Calculate and Apply Kaiser window to ideal lowpass filter.
    * Note: last window value is IBeta which is NOT zero.
    * You're supposed to really truncate the window here, not ramp
    * it to zero. This helps reduce the first sidelobe. 
    */
   IBeta = 1.0/Izero(Beta);
   inm1 = 1.0/((double)(N-1));
   for (i=1; i<N; i++) {
      temp = (double)i * inm1;
      temp1 = 1.0 - temp*temp;
      temp1 = (temp1<0? 0: temp1); /* make sure it's not negative since
                                      we're taking the square root - this
                                      happens on Pentium 4's due to tiny
                                      roundoff errors */
      c[i] *= Izero(Beta*sqrt(temp1)) * IBeta;
   }
}

float lrsFilterUp(float Imp[],  /* impulse response */
                  float ImpD[], /* impulse response deltas */
                  UWORD Nwing,  /* len of one wing of filter */
                  BOOL Interp,  /* Interpolate coefs using deltas? */
                  float *Xp,    /* Current sample */
                  double Ph,    /* Phase */
                  int Inc)    /* increment (1 for right wing or -1 for left) */
{
   float *Hp, *Hdp = NULL, *End;
   double a = 0;
   float v, t;

   Ph *= Npc; /* Npc is number of values per 1/delta in impulse response */
   
   v = 0.0; /* The output value */
   Hp = &Imp[(int)Ph];
   End = &Imp[Nwing];
   if (Interp) {
      Hdp = &ImpD[(int)Ph];
      a = Ph - floor(Ph); /* fractional part of Phase */
   }

   if (Inc == 1)		/* If doing right wing...              */
   {				      /* ...drop extra coeff, so when Ph is  */
      End--;			/*    0.5, we don't do too many mult's */
      if (Ph == 0)		/* If the phase is zero...           */
      {			         /* ...then we've already skipped the */
         Hp += Npc;		/*    first sample, so we must also  */
         Hdp += Npc;		/*    skip ahead in Imp[] and ImpD[] */
      }
   }

   if (Interp)
      while (Hp < End) {
         t = *Hp;		/* Get filter coeff */
         t += (*Hdp)*a; /* t is now interp'd filter coeff */
         Hdp += Npc;		/* Filter coeff differences step */
         t *= *Xp;		/* Mult coeff by input sample */
         v += t;			/* The filter output */
         Hp += Npc;		/* Filter coeff step */
         Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
      } 
   else 
      while (Hp < End) {
         t = *Hp;		/* Get filter coeff */
         t *= *Xp;		/* Mult coeff by input sample */
         v += t;			/* The filter output */
         Hp += Npc;		/* Filter coeff step */
         Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
      }
   
   return v;
}

float lrsFilterUD(float Imp[],  /* impulse response */
                  float ImpD[], /* impulse response deltas */
                  UWORD Nwing,  /* len of one wing of filter */
                  BOOL Interp,  /* Interpolate coefs using deltas? */
                  float *Xp,    /* Current sample */
                  double Ph,    /* Phase */
                  int Inc,    /* increment (1 for right wing or -1 for left) */
                  double dhb) /* filter sampling period */
{
   float a;
   float *Hp, *Hdp, *End;
   float v, t;
   double Ho;
    
   v = 0.0; /* The output value */
   Ho = Ph*dhb;
   End = &Imp[Nwing];
   if (Inc == 1)		/* If doing right wing...              */
   {				      /* ...drop extra coeff, so when Ph is  */
      End--;			/*    0.5, we don't do too many mult's */
      if (Ph == 0)		/* If the phase is zero...           */
         Ho += dhb;		/* ...then we've already skipped the */
   }				         /*    first sample, so we must also  */
                        /*    skip ahead in Imp[] and ImpD[] */

   if (Interp)
      while ((Hp = &Imp[(int)Ho]) < End) {
         t = *Hp;		/* Get IR sample */
         Hdp = &ImpD[(int)Ho];  /* get interp bits from diff table*/
         a = Ho - floor(Ho);	  /* a is logically between 0 and 1 */
         t += (*Hdp)*a; /* t is now interp'd filter coeff */
         t *= *Xp;		/* Mult coeff by input sample */
         v += t;			/* The filter output */
         Ho += dhb;		/* IR step */
         Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
      }
   else 
      while ((Hp = &Imp[(int)Ho]) < End) {
         t = *Hp;		/* Get IR sample */
         t *= *Xp;		/* Mult coeff by input sample */
         v += t;			/* The filter output */
         Ho += dhb;		/* IR step */
         Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
      }

   return v;
}
