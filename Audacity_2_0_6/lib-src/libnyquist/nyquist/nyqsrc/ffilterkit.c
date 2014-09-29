/*
 * ffilterkit.c (library "filterkit.a"):  
 * Kaiser-windowed low-pass filter support.
 */

/* ffilterkit.c
 *
 * FilterUp() - Applies a filter to a given sample when up-converting.
 * FilterUD() - Applies a filter to a given sample when up- or down-
 *                   converting.
 */

 /* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "stdefs.h"
#include "fresample.h"
#include "ffilterkit.h"


fast_float FilterUp(float Imp[], float ImpD[], 
                     int Nwing, boolean Interp,
                     float *Xp, double Ph, int Inc)
{
    float *Hp, *Hdp = NULL, *End;
    fast_float a = 0;
    fast_float v, t;
    double exact_index = Ph * Npc;
    long index = (long) exact_index; /* convert fraction to filter index */

/*	nyquist_printf("FilterUp, Inc %d, phase %g\n", Inc, Ph);  */
    v=0;
    Hp = &Imp[index];
    End = &Imp[Nwing];
    if (Interp) {
        Hdp = &ImpD[index];
        a = exact_index - index;
/*	nyquist_printf("fraction %g\n", a); */
    }
    if (Inc == 1)		/* If doing right wing...              */
    {				/* ...drop extra coeff, so when Ph is  */
        End--;			/*    0.5, we don't do too many mult's */
        if (Ph == 0)		/* If the phase is zero...           */
        {			/* ...then we've already skipped the */
            Hp += Npc;		/*    first sample, so we must also  */
            Hdp += Npc;		/*    skip ahead in Imp[] and ImpD[] */
        }
    }
    if (Interp) {
      while (Hp < End) {
          t = *Hp;		/* Get filter coeff */
          /*  t scaled by 2^(16 + NLpScl)/LpScl */
          t += *Hdp *a;	 /* t is now interp'd filter coeff */
          Hdp += Npc;		/* Filter coeff differences step */
          t *= *Xp;		/* Mult coeff by input sample */
          /*  t scaled by 2^(16 + NLpScl)/LpScl */
          v += t;			/* The filter output */
          Hp += Npc;		/* Filter coeff step */
          Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
      } 
    } else {
      while (Hp < End) {
          t = *Hp;		/* Get filter coeff */
          t *= *Xp;		/* Mult coeff by input sample */
          v += t;			/* The filter output */
          Hp += Npc;		/* Filter coeff step */
          Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
      }
    }
    return(v);
}

fast_float FilterUD( float Imp[], float ImpD[],
                     int Nwing, boolean Interp,
                     float *Xp, double Ph, int Inc, double dhb)
{
    double a;
    float *Hp, *Hdp, *End;
    fast_float v, t;
    double Ho;
    
    v=0;
    Ho = Ph*dhb;
    End = &Imp[Nwing];
    if (Inc == 1)		/* If doing right wing...              */
    {				/* ...drop extra coeff, so when Ph is  */
        End--;			/*    0.5, we don't do too many mult's */
        if (Ph == 0)		/* If the phase is zero...           */
          Ho += dhb;		/* ...then we've already skipped the */
    }				/*    first sample, so we must also  */
                                /*    skip ahead in Imp[] and ImpD[] */
    if (Interp) {
      long HoIndex = (long) Ho;
      while ((Hp = &Imp[HoIndex]) < End) {
          t = *Hp;		/* Get IR sample */
          Hdp = &ImpD[HoIndex];  /* get interp (lower Na) bits from diff table*/
          a = Ho - HoIndex;	/* a is logically between 0 and 1 */
          t += *Hdp * a; /* t is now interp'd filter coeff */
          t *= *Xp;		/* Mult coeff by input sample */
          v += t;			/* The filter output */
          Ho += dhb;		/* IR step */
          Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
          HoIndex = (long) Ho;
      }
    } else {
      long HoIndex = (long) Ho;
      while ((Hp = &Imp[HoIndex]) < End) {
          t = *Hp;		/* Get IR sample */
          t *= *Xp;		/* Mult coeff by input sample */
          v += t;			/* The filter output */
          Ho += dhb;		/* IR step */
          Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
          HoIndex = (long) Ho;
      }
    }
    return(v);
}

