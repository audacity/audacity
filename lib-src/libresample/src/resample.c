/**********************************************************************

  resample.c

  Real-time library interface by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

  This is the main source file, implementing all of the API
  functions and handling all of the buffering logic.

**********************************************************************/

/* External interface */
#include "../include/libresample.h"

/* Definitions */
#include "resample_defs.h"

#include "filterkit.h"

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

typedef struct {
   float  *Imp;
   float  *ImpD;
   float   LpScl;
   UWORD   Nmult;
   UWORD   Nwing;
   double  minFactor;
   double  maxFactor;
   UWORD   XSize;
   float  *X;
   UWORD   Xp; /* Current "now"-sample pointer for input */
   UWORD   Xread; /* Position to put new samples */
   UWORD   Xoff;
   UWORD   YSize;
   float  *Y;
   UWORD   Yp;
   double  Time;
} rsdata;

void *resample_dup(const void *	handle)
{
   const rsdata *cpy = (const rsdata *)handle;
   rsdata *hp = (rsdata *)malloc(sizeof(rsdata));

   hp->minFactor = cpy->minFactor;
   hp->maxFactor = cpy->maxFactor;
   hp->Nmult = cpy->Nmult;
   hp->LpScl = cpy->LpScl;
   hp->Nwing = cpy->Nwing;

   hp->Imp = (float *)malloc(hp->Nwing * sizeof(float));
   memcpy(hp->Imp, cpy->Imp, hp->Nwing * sizeof(float));
   hp->ImpD = (float *)malloc(hp->Nwing * sizeof(float));
   memcpy(hp->ImpD, cpy->ImpD, hp->Nwing * sizeof(float));

   hp->Xoff = cpy->Xoff;
   hp->XSize = cpy->XSize;
   hp->X = (float *)malloc((hp->XSize + hp->Xoff) * sizeof(float));
   memcpy(hp->X, cpy->X, (hp->XSize + hp->Xoff) * sizeof(float));
   hp->Xp = cpy->Xp;
   hp->Xread = cpy->Xread;
   hp->YSize = cpy->YSize;
   hp->Y = (float *)malloc(hp->YSize * sizeof(float));
   memcpy(hp->Y, cpy->Y, hp->YSize * sizeof(float));
   hp->Yp = cpy->Yp;
   hp->Time = cpy->Time;
   
   return (void *)hp;
}

void *resample_open(int highQuality, double minFactor, double maxFactor)
{
   double *Imp64;
   double Rolloff, Beta;
   rsdata *hp;
   UWORD   Xoff_min, Xoff_max;
   int i;

   /* Just exit if we get invalid factors */
   if (minFactor <= 0.0 || maxFactor <= 0.0 || maxFactor < minFactor) {
      #if DEBUG
      fprintf(stderr,
              "libresample: "
              "minFactor and maxFactor must be positive real numbers,\n"
              "and maxFactor should be larger than minFactor.\n");
      #endif
      return 0;
   }

   hp = (rsdata *)malloc(sizeof(rsdata));

   hp->minFactor = minFactor;
   hp->maxFactor = maxFactor;
 
   if (highQuality)
      hp->Nmult = 35;
   else
      hp->Nmult = 11;

   hp->LpScl = 1.0;
   hp->Nwing = Npc*(hp->Nmult-1)/2; /* # of filter coeffs in right wing */

   Rolloff = 0.90;
   Beta = 6;

   Imp64 = (double *)malloc(hp->Nwing * sizeof(double));

   lrsLpFilter(Imp64, hp->Nwing, 0.5*Rolloff, Beta, Npc);

   hp->Imp = (float *)malloc(hp->Nwing * sizeof(float));
   hp->ImpD = (float *)malloc(hp->Nwing * sizeof(float));
   for(i=0; i<hp->Nwing; i++)
      hp->Imp[i] = Imp64[i];

   /* Storing deltas in ImpD makes linear interpolation
      of the filter coefficients faster */
   for (i=0; i<hp->Nwing-1; i++)
      hp->ImpD[i] = hp->Imp[i+1] - hp->Imp[i];

   /* Last coeff. not interpolated */
   hp->ImpD[hp->Nwing-1] = - hp->Imp[hp->Nwing-1];

   free(Imp64);

   /* Calc reach of LP filter wing (plus some creeping room) */
   Xoff_min = ((hp->Nmult+1)/2.0) * MAX(1.0, 1.0/minFactor) + 10;
   Xoff_max = ((hp->Nmult+1)/2.0) * MAX(1.0, 1.0/maxFactor) + 10;
   hp->Xoff = MAX(Xoff_min, Xoff_max);

   /* Make the inBuffer size at least 4096, but larger if necessary
      in order to store the minimum reach of the LP filter and then some.
      Then allocate the buffer an extra Xoff larger so that
      we can zero-pad up to Xoff zeros at the end when we reach the
      end of the input samples. */
   hp->XSize = MAX(2*hp->Xoff+10, 4096);
   hp->X = (float *)malloc((hp->XSize + hp->Xoff) * sizeof(float));
   hp->Xp = hp->Xoff;
   hp->Xread = hp->Xoff;
   
   /* Need Xoff zeros at begining of X buffer */
   for(i=0; i<hp->Xoff; i++)
      hp->X[i]=0;

   /* Make the outBuffer long enough to hold the entire processed
      output of one inBuffer */
   hp->YSize = (int)(((double)hp->XSize)*maxFactor+2.0);
   hp->Y = (float *)malloc(hp->YSize * sizeof(float));
   hp->Yp = 0;

   hp->Time = (double)hp->Xoff; /* Current-time pointer for converter */
   
   return (void *)hp;
}

int resample_get_filter_width(const void   *handle)
{
   const rsdata *hp = (const rsdata *)handle;
   return hp->Xoff;
}

int resample_process(void   *handle,
                     double  factor,
                     float  *inBuffer,
                     int     inBufferLen,
                     int     lastFlag,
                     int    *inBufferUsed, /* output param */
                     float  *outBuffer,
                     int     outBufferLen)
{
   rsdata *hp = (rsdata *)handle;
   float  *Imp = hp->Imp;
   float  *ImpD = hp->ImpD;
   float  LpScl = hp->LpScl;
   UWORD  Nwing = hp->Nwing;
   BOOL interpFilt = FALSE; /* TRUE means interpolate filter coeffs */
   int outSampleCount;
   UWORD Nout, Ncreep, Nreuse;
   int Nx;
   int i, len;

   #if DEBUG
   fprintf(stderr, "resample_process: in=%d, out=%d lastFlag=%d\n",
           inBufferLen, outBufferLen, lastFlag);
   #endif

   /* Initialize inBufferUsed and outSampleCount to 0 */
   *inBufferUsed = 0;
   outSampleCount = 0;

   if (factor < hp->minFactor || factor > hp->maxFactor) {
      #if DEBUG
      fprintf(stderr,
              "libresample: factor %f is not between "
              "minFactor=%f and maxFactor=%f",
              factor, hp->minFactor, hp->maxFactor);
      #endif
      return -1;
   }

   /* Start by copying any samples still in the Y buffer to the output
      buffer */
   if (hp->Yp && (outBufferLen-outSampleCount)>0) {
      len = MIN(outBufferLen-outSampleCount, hp->Yp);
      for(i=0; i<len; i++)
         outBuffer[outSampleCount+i] = hp->Y[i];
      outSampleCount += len;
      for(i=0; i<hp->Yp-len; i++)
         hp->Y[i] = hp->Y[i+len];
      hp->Yp -= len;
   }

   /* If there are still output samples left, return now - we need
      the full output buffer available to us... */
   if (hp->Yp)
      return outSampleCount;

   /* Account for increased filter gain when using factors less than 1 */
   if (factor < 1)
      LpScl = LpScl*factor;

   for(;;) {

      /* This is the maximum number of samples we can process
         per loop iteration */

      #ifdef DEBUG
      printf("XSize: %d Xoff: %d Xread: %d Xp: %d lastFlag: %d\n",
             hp->XSize, hp->Xoff, hp->Xread, hp->Xp, lastFlag);
      #endif

      /* Copy as many samples as we can from the input buffer into X */
      len = hp->XSize - hp->Xread;

      if (len >= (inBufferLen - (*inBufferUsed)))
         len = (inBufferLen - (*inBufferUsed));

      for(i=0; i<len; i++)
         hp->X[hp->Xread + i] = inBuffer[(*inBufferUsed) + i];

      *inBufferUsed += len;
      hp->Xread += len;

      if (lastFlag && (*inBufferUsed == inBufferLen)) {
         /* If these are the last samples, zero-pad the
            end of the input buffer and make sure we process
            all the way to the end */
         Nx = hp->Xread - hp->Xoff;
         for(i=0; i<hp->Xoff; i++)
            hp->X[hp->Xread + i] = 0;
      }
      else
         Nx = hp->Xread - 2 * hp->Xoff;

      #ifdef DEBUG
      fprintf(stderr, "new len=%d Nx=%d\n", len, Nx);
      #endif

      if (Nx <= 0)
         break;

      /* Resample stuff in input buffer */
      if (factor >= 1) {      /* SrcUp() is faster if we can use it */
         Nout = lrsSrcUp(hp->X, hp->Y, factor, &hp->Time, Nx,
                         Nwing, LpScl, Imp, ImpD, interpFilt);
      }
      else {
         Nout = lrsSrcUD(hp->X, hp->Y, factor, &hp->Time, Nx,
                         Nwing, LpScl, Imp, ImpD, interpFilt);
      }

      #ifdef DEBUG
      printf("Nout: %d\n", Nout);
      #endif
      
      hp->Time -= Nx;         /* Move converter Nx samples back in time */
      hp->Xp += Nx;           /* Advance by number of samples processed */

      /* Calc time accumulation in Time */
      Ncreep = (int)(hp->Time) - hp->Xoff;
      if (Ncreep) {
         hp->Time -= Ncreep;  /* Remove time accumulation */
         hp->Xp += Ncreep;    /* and add it to read pointer */
      }

      /* Copy part of input signal that must be re-used */
      Nreuse = hp->Xread - (hp->Xp - hp->Xoff);

      for (i=0; i<Nreuse; i++)
         hp->X[i] = hp->X[i + (hp->Xp - hp->Xoff)];

      #ifdef DEBUG
      printf("New Xread=%d\n", Nreuse);
      #endif

      hp->Xread = Nreuse;  /* Pos in input buff to read new data into */
      hp->Xp = hp->Xoff;
      
      /* Check to see if output buff overflowed (shouldn't happen!) */
      if (Nout > hp->YSize) {
         #ifdef DEBUG
         printf("Nout: %d YSize: %d\n", Nout, hp->YSize);
         #endif
         fprintf(stderr, "libresample: Output array overflow!\n");
         return -1;
      }

      hp->Yp = Nout;

      /* Copy as many samples as possible to the output buffer */
      if (hp->Yp && (outBufferLen-outSampleCount)>0) {
         len = MIN(outBufferLen-outSampleCount, hp->Yp);
         for(i=0; i<len; i++)
            outBuffer[outSampleCount+i] = hp->Y[i];
         outSampleCount += len;
         for(i=0; i<hp->Yp-len; i++)
            hp->Y[i] = hp->Y[i+len];
         hp->Yp -= len;
      }

      /* If there are still output samples left, return now,
         since we need the full output buffer available */
      if (hp->Yp)
         break;
   }

   return outSampleCount;
}

void resample_close(void *handle)
{
   rsdata *hp = (rsdata *)handle;
   free(hp->X);
   free(hp->Y);
   free(hp->Imp);
   free(hp->ImpD);
   free(hp);
}

