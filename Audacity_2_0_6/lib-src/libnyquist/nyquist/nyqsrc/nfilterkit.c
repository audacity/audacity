/*
 * nfilterkit.c - windowed low-pass filter support.
 *  adapted from filterkit.c, by Julius Smith et al., CCRMA, Stanford University
 *

/*
 * FilterUp() - Applies a filter to a given sample when up-converting.
 * FilterUD() - Applies a filter to a given sample when up- or down-
 *                   converting.
 */

#include "soundstruct.h"
#include "nresample.h"
#include "nfilterkit.h"

/* #include <libc.h> */
#include <stdio.h>
#include <math.h>
#include <string.h>


#include <math.h>

fast_float FilterUp(float Imp[], float ImpD[], 
                     UHWORD Nwing, BOOL Interp,
                     float *Xp, double Ph, HWORD Inc)
{
    float *Hp, *Hdp = NULL, *End;
    fast_float a = 0;
    fast_float v, t;
    double exact_index = Ph * Npc;
    long index = exact_index; /* convert fraction to filter index */

/*	printf("FilterUp, Inc %d, phase %g\n", Inc, Ph);  */
    v=0;
    Hp = &Imp[index];
    End = &Imp[Nwing];
    if (Interp) {
        Hdp = &ImpD[index];
        a = exact_index - index;
/*	printf("fraction %g\n", a); */
    }
    if (Inc == 1)		/* If doing right wing...              */
    {				/* ...drop extra coeff, so when Ph is  */
        End--;			/*    0.5, we don't do too many mult's */
        if (Ph == 0)		/* If the phase is zero...           */
        {			/* ...then we've already skipped the */
            printf("Ph == 0\n");
            Hp += Npc;		/*    first sample, so we must also  */
            Hdp += Npc;		/*    skip ahead in Imp[] and ImpD[] */
        }
    }
    if (Interp) {
      while (Hp < End) {
          t = *Hp;		/* Get filter coeff */
        /*  t scaled by 2^(16 + NLpScl)/LpScl */
/*	printf("coeff %g ", t);  */
          t += *Hdp *a;	 /* t is now interp'd filter coeff */
/*	printf("interp'd coeff %g ", t);*/
          Hdp += Npc;		/* Filter coeff differences step */
/*	printf("input sample %g ", *Xp);  */
          t *= *Xp;		/* Mult coeff by input sample */
        /*  t scaled by 2^(16 + NLpScl)/LpScl */
/*	printf("product %g\n", t); */
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
/*	printf("FilterUp, Inc %d returns %g\n", Inc, v); */
    return(v);
}

fast_float FilterUD( float Imp[], float ImpD[],
                     UHWORD Nwing, BOOL Interp,
                     float *Xp, double Ph, HWORD Inc, double dhb)
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
      long HoIndex = Ho;
      while ((Hp = &Imp[HoIndex]) < End) {
          t = *Hp;		/* Get IR sample */
          Hdp = &ImpD[HoIndex];  /* get interp (lower Na) bits from diff table*/
          a = Ho - HoIndex;	/* a is logically between 0 and 1 */
          t += *Hdp * a; /* t is now interp'd filter coeff */
          t *= *Xp;		/* Mult coeff by input sample */
          v += t;			/* The filter output */
          Ho += dhb;		/* IR step */
          Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
          HoIndex = Ho;
      }
    } else {
      long HoIndex = Ho;
      while ((Hp = &Imp[HoIndex]) < End) {
          t = *Hp;		/* Get IR sample */
          t *= *Xp;		/* Mult coeff by input sample */
          v += t;			/* The filter output */
          Ho += dhb;		/* IR step */
          Xp += Inc;		/* Input signal step. NO CHECK ON BOUNDS */
          HoIndex = Ho;
      }
    }
    return(v);
}

/* Sampling rate up-conversion only subroutine;
 * Slightly faster than down-conversion;
 */
static int SrcUp(float X[], float Y[], double factor, double *Time,
                 UHWORD Nx, UHWORD Nwing, double LpScl,
                 float Imp[], float ImpD[], BOOL Interp)
{
    mem_float *Xp, *Ystart;
    fast_float v;
    
    double dt;                  /* Step through input signal */ 
    double endTime;             /* When Time reaches EndTime, return to user */
    
/*    printf("SrcUp: interpFilt %d\n", Interp);*/

    dt = 1.0/factor;            /* Output sampling period */
    
    Ystart = Y;
    endTime = *Time + Nx;
    while (*Time < endTime)
    {
        long iTime = *Time;
        Xp = &X[iTime];      /* Ptr to current input sample */
        /* Perform left-wing inner product */
        v = FilterUp(Imp, ImpD, Nwing, Interp, Xp, *Time - iTime, -1);
        /* Perform right-wing inner product */
        v += FilterUp(Imp, ImpD, Nwing, Interp, Xp+1, 
                      (1 + iTime) - *Time, 1);
        v *= LpScl;		/* Normalize for unity filter gain */
/*	printf("SrcUp output sample %g\n", v); */
        *Y++ = v;
        *Time += dt;		/* Move to next sample by time increment */
    }
    return (Y - Ystart);        /* Return the number of output samples */
}


/* Sampling rate conversion subroutine */

static int SrcUD(float X[], float Y[], double factor, double *Time,
                 UHWORD Nx, UHWORD Nwing, double LpScl,
                 float Imp[], float ImpD[], BOOL Interp)
{
    mem_float *Xp, *Ystart;
    fast_float v;
    
    double dh;                  /* Step through filter impulse response */
    double dt;                  /* Step through input signal */
    double endTime;             /* When Time reaches EndTime, return to user */
    
    dt = 1.0/factor;            /* Output sampling period */
    
    dh = MIN(Npc, factor*Npc);  /* Filter sampling period */
    
    Ystart = Y;
    endTime = *Time + Nx;
    while (*Time < endTime)
    {
        long iTime = *Time;
        Xp = &X[iTime];		/* Ptr to current input sample */
        v = FilterUD(Imp, ImpD, Nwing, Interp, Xp, *Time - iTime,
                     -1, dh);	/* Perform left-wing inner product */
        v += FilterUD(Imp, ImpD, Nwing, Interp, Xp+1, (1 + iTime) - *Time,
                      1, dh);	/* Perform right-wing inner product */
        v *= LpScl;		/* Normalize for unity filter gain */
        *Y++ = v;
        *Time += dt;		/* Move to next sample by time increment */
    }
    return (Y - Ystart);        /* Return the number of output samples */
}


