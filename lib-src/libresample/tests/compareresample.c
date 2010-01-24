/**********************************************************************

  compareresample.c

  Real-time library interface by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

**********************************************************************/

#include "../include/libresample.h"

#include <samplerate.h>

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <sys/time.h>

#define MIN(A, B) (A) < (B)? (A) : (B)

void dostat(char *name, float *d1, float *d2, int len)
{
   int i;
   double sum, sumsq, err, rmserr;

   sum = 0.0;
   sumsq = 0.0;
   for(i=0; i<len; i++) {
      double diff = d1[i] - d2[i];
      sum += fabs(diff);
      sumsq += diff * diff;
   }
   err = sum / len;
   rmserr = sqrt(sumsq / len);
   printf("   %s: Avg err: %f RMS err: %f\n", name, err, rmserr);
}

void runtest(float *src, int srclen,
             float *ans, int anslen,
             double factor)
{
   struct timeval tv0, tv1;
   int dstlen = (int)(srclen * factor);
   float *dst_rs = (float *)malloc((dstlen+100) * sizeof(float));
   float *dst_rabbit = (float *)malloc((dstlen+100) * sizeof(float));
   void *handle;
   SRC_DATA rabbit;
   double deltat;
   int srcblocksize = srclen;
   int dstblocksize = dstlen;
   int i, out, out_rabbit, o, srcused;
   int statlen, srcpos;

   /* do resample */

   for(i=0; i<dstlen+100; i++)
      dst_rs[i] = -99.0;

   gettimeofday(&tv0, NULL);

   handle = resample_open(1, factor, factor);
   out = 0;
   srcpos = 0;
   for(;;) {
      int srcBlock = MIN(srclen-srcpos, srcblocksize);
      int lastFlag = (srcBlock == srclen-srcpos);

      o = resample_process(handle, factor,
                           &src[srcpos], srcBlock,
                           lastFlag, &srcused,
                           &dst_rs[out], MIN(dstlen-out, dstblocksize));
      srcpos += srcused;
      if (o >= 0)
         out += o;
      if (o < 0 || (o == 0 && srcpos == srclen))
         break;
   }
   resample_close(handle);

   gettimeofday(&tv1, NULL);
   deltat =
      (tv1.tv_sec + tv1.tv_usec * 0.000001) -
      (tv0.tv_sec + tv0.tv_usec * 0.000001);

   if (o < 0) {
      printf("Error: resample_process returned an error: %d\n", o);
   }

   if (out <= 0) {
      printf("Error: resample_process returned %d samples\n", out);
      free(dst_rs);
      return;
   }

   printf("   resample: %.3f seconds, %d outputs\n", deltat, out);

   /* do rabbit (Erik's libsamplerate) */

   for(i=0; i<dstlen+100; i++)
      dst_rabbit[i] = -99.0;

   rabbit.data_in = src;
   rabbit.data_out = dst_rabbit;
   rabbit.input_frames = srclen;
   rabbit.output_frames = dstlen;
   rabbit.input_frames_used = 0;
   rabbit.output_frames_gen = 0;
   rabbit.end_of_input = 1;
   rabbit.src_ratio = factor;

   gettimeofday(&tv0, NULL);

   /* src_simple(&rabbit, SRC_SINC_BEST_QUALITY, 1); */
   src_simple(&rabbit, SRC_SINC_FASTEST, 1);
   /* src_simple(&rabbit, SRC_LINEAR, 1); */

   gettimeofday(&tv1, NULL);
   deltat =
      (tv1.tv_sec + tv1.tv_usec * 0.000001) -
      (tv0.tv_sec + tv0.tv_usec * 0.000001);

   out_rabbit = rabbit.output_frames_gen;

   printf("   rabbit  : %.3f seconds, %d outputs\n",
          deltat, out_rabbit);

   statlen = MIN(out, out_rabbit);
   if (anslen > 0)
      statlen = MIN(statlen, anslen);

   if (ans) {
      dostat("resample    ", dst_rs, ans, statlen);
      dostat("rabbit      ", dst_rabbit, ans, statlen);
   }
   dostat(   "RS vs rabbit", dst_rs, dst_rabbit, statlen);

   free(dst_rs);
   free(dst_rabbit);
}

int main(int argc, char **argv)
{
   int i, srclen;
   float *src, *ans;

   printf("\n*** sin wave, factor = 1.0 *** \n\n");
   srclen = 100000;
   src = malloc(srclen * sizeof(float));
   for(i=0; i<srclen; i++)
      src[i] = sin(i/100.0);

   runtest(src, srclen, src, srclen, 1.0);

   printf("\n*** sin wave, factor = 0.25 *** \n\n");
   srclen = 100000;
   for(i=0; i<srclen; i++)
      src[i] = sin(i/100.0);
   ans = malloc((srclen/4) * sizeof(float));
   for(i=0; i<srclen/4; i++)
      ans[i] = sin(i/25.0);

   runtest(src, srclen, ans, srclen/4, 0.25);
   free(ans);

   printf("\n*** sin wave, factor = 4.0 *** \n\n");
   srclen = 20000;
   for(i=0; i<srclen; i++)
      src[i] = sin(i/100.0);
   ans = malloc((srclen*4) * sizeof(float));
   for(i=0; i<srclen*4; i++)
      ans[i] = sin(i/400.0);

   runtest(src, srclen, ans, srclen*4, 4.0);
   free(ans);
   free(src);

   return 0;
}
