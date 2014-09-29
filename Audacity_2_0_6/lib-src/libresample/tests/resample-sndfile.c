/**********************************************************************

  resample-sndfile.c

  Written by Dominic Mazzoni

  Based on resample-1.7:
    http://www-ccrma.stanford.edu/~jos/resample/

  License: LGPL - see the file LICENSE.txt for more information

**********************************************************************/

#include "../include/libresample.h"

#include <sndfile.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sys/time.h>

#define MIN(A, B) (A) < (B)? (A) : (B)

void usage(char *progname)
{
   fprintf(stderr, "Usage: %s -by <ratio> <input> <output>\n", progname);
   fprintf(stderr, "       %s -to <rate> <input> <output>\n", progname);
   fprintf(stderr, "\n");
   exit(-1);
}

int main(int argc, char **argv)
{
   SNDFILE *srcfile, *dstfile;
   SF_INFO srcinfo, dstinfo;
   SF_FORMAT_INFO formatinfo;
   char *extension;
   void **handle;
   int channels;
   int srclen, dstlen;
   float *src, *srci;
   float *dst, *dsti;
   double ratio = 0.0;
   double srcrate;
   double dstrate = 0.0;
   struct timeval tv0, tv1;
   double deltat;
   int numformats;
   int pos, bufferpos, outcount;
   int i, c;

   if (argc != 5)
      usage(argv[0]);

   if (!strcmp(argv[1], "-by")) {
      ratio = atof(argv[2]);
      if (ratio <= 0.0) {
         fprintf(stderr, "Ratio of %f is illegal\n", ratio);
         usage(argv[0]);
      }
   }
   else if (!strcmp(argv[1], "-to")) {
      dstrate = atof(argv[2]);
      if (dstrate < 10.0 || dstrate > 100000.0) {
         fprintf(stderr, "Sample rate of %f is illegal\n", dstrate);
         usage(argv[0]);
      }
   }
   else
      usage(argv[0]);

   memset(&srcinfo, 0, sizeof(srcinfo));
   memset(&dstinfo, 0, sizeof(dstinfo));
   srcfile = sf_open(argv[3], SFM_READ, &srcinfo);
   if (!srcfile) {
      fprintf(stderr, "%s", sf_strerror(NULL));
      exit(-1);
   }

   srcrate = srcinfo.samplerate;
   if (dstrate == 0.0)
      dstrate = srcrate * ratio;
   else
      ratio = dstrate / srcrate;

   channels = srcinfo.channels;

   /* figure out format of destination file */

   extension = strstr(argv[4], ".");
   if (extension) {
      extension++;
      sf_command(NULL, SFC_GET_FORMAT_MAJOR_COUNT,
                 &numformats, sizeof(numformats));
      for(i=0; i<numformats; i++) {
         memset(&formatinfo, 0, sizeof(formatinfo));
         formatinfo.format = i;
         sf_command(NULL, SFC_GET_FORMAT_MAJOR,
                    &formatinfo, sizeof(formatinfo));
         if (!strcmp(formatinfo.extension, extension)) {
            printf("Using %s for output format.\n", formatinfo.name);
            dstinfo.format = formatinfo.format |
               (srcinfo.format & SF_FORMAT_SUBMASK);
            break;
         }            
      }
   }

   if (!dstinfo.format) {
      if (extension)
         printf("Warning: output format (%s) not recognized, "
                "using same as input format.\n",
                extension);
      dstinfo.format = srcinfo.format;
   }

   dstinfo.samplerate = (int)(dstrate + 0.5);
   dstinfo.channels = channels;

   dstfile = sf_open(argv[4], SFM_WRITE, &dstinfo);
   if (!dstfile) {
      fprintf(stderr, "%s", sf_strerror(NULL));
      exit(-1);
   }

   printf("Source: %s (%d frames, %.2f Hz)\n",
          argv[3], (int)srcinfo.frames, srcrate);
   printf("Destination: %s (%.2f Hz, ratio=%.5f)\n", argv[4],
          dstrate, ratio);

   srclen = 4096;
   dstlen = (srclen * ratio + 1000);
   srci = (float *)malloc(srclen * channels * sizeof(float));
   dsti = (float *)malloc(dstlen * channels * sizeof(float));
   src = (float *)malloc(srclen * sizeof(float));
   dst = (float *)malloc(dstlen * sizeof(float));

   handle = (void **)malloc(channels * sizeof(void *));
   for(c=0; c<channels; c++)
      handle[c] = resample_open(1, ratio, ratio);

   gettimeofday(&tv0, NULL);

   pos = 0;
   bufferpos = 0;
   outcount = 0;
   while(pos < srcinfo.frames) {
      int block = MIN(srclen-bufferpos, srcinfo.frames-pos);
      int lastFlag = (pos+block == srcinfo.frames);
      int inUsed, inUsed2=0, out=0, out2=0;

      sf_readf_float(srcfile, &srci[bufferpos*channels], block);
      block += bufferpos;

      for(c=0; c<channels; c++) {
         for(i=0; i<block; i++)
            src[i] = srci[i*channels+c];

         inUsed = 0;
         out = resample_process(handle[c], ratio, src, block, lastFlag,
                                &inUsed, dst, dstlen);
         if (c==0) {
            inUsed2 = inUsed;
            out2 = out;
         }
         else {
            if (inUsed2 != inUsed || out2 != out) {
               fprintf(stderr, "Fatal error: channels out of sync!\n");
               exit(-1);
            }
         }

         for(i=0; i<out; i++)
	 {
	    if(dst[i] <= -1)
	       dsti[i*channels+c] = -1;
	    else if(dst[i] >= 1)
	       dsti[i*channels+c] = 1;
	    else
	       dsti[i*channels+c] = dst[i];
	 }
      }

      sf_writef_float(dstfile, dsti, out);

      bufferpos = block - inUsed;
      for(i=0; i<bufferpos*channels; i++)
         srci[i] = srci[i+(inUsed*channels)];
      pos += inUsed;
      outcount += out;
   }

   sf_close(srcfile);
   sf_close(dstfile);

   gettimeofday(&tv1, NULL);
   deltat =
      (tv1.tv_sec + tv1.tv_usec * 0.000001) -
      (tv0.tv_sec + tv0.tv_usec * 0.000001);

   printf("Elapsed time: %.3f seconds\n", deltat);
   printf("%d frames written to output file\n", outcount);

   free(src);
   free(srci);
   free(dst);
   free(dsti);

   exit(0);
}
