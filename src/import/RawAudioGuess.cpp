/**********************************************************************

  Audacity: A Digital Audio Editor

  RawAudioGuess.cpp

  Dominic Mazzoni

  Attempts to determine the format of an audio file that doesn't
  have any header information.  Returns the format as a
  libsndfile-compatible format, along with the guessed number of
  channels and the byte-offset.

**********************************************************************/

#include "RawAudioGuess.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <wx/defs.h>
#include <wx/ffile.h>

#include "../Internat.h"

#define RAW_GUESS_DEBUG 0

#if RAW_GUESS_DEBUG
static FILE *g_raw_debug_file = NULL;
#endif

static float AmpStat(float *data, int len)
{
   float sum, sumofsquares, avg, variance, dev;
   int i;

   if (len == 0)
      return 1.0;

   /* Calculate standard deviation of the amplitudes */

   sum = 0.0;
   sumofsquares = 0.0;

   for (i = 0; i < len; i++) {
      float x = fabs(data[i]);
      sum += x;
      sumofsquares += x * x;
   }

   avg = sum / len;
   variance = sumofsquares / len - (avg * avg);

   dev = sqrt(variance);

   return dev;
}

static float JumpStat(float *data, int len)
{
   float avg;
   int i;

   /* Calculate 1.0 - avg jump
    * A score near 1.0 means avg jump is pretty small
    */

   avg = 0.0;
   for (i = 0; i < len - 1; i++)
      avg += fabs(data[i + 1] - data[i]);
   avg = 1.0 - (avg / (len - 1) / 2.0);

   return avg;
}

static float SecondDStat(float *data, int len)
{
   int i;
   float v1=0, v2=0;
   float a1=0, a2=0;
   float sum=0;

   for (i = 1; i < len; i++) {
      a2 = a1;
      v2 = v1;
      v1 = data[i]-data[i-1];
      a1 = v1 - v2;
      sum += fabs(a1-a2);
   }

   return sum/len;
}

static float RedundantStereo(float *data, int len)
{
   int i;
   int c = 0;

   for (i = 1; i < len - 1; i += 2)
      if (fabs(data[i + 1] - data[i]) > 2*fabs(data[i] - data[i - 1]) ||
          2*fabs(data[i + 1] - data[i]) < fabs(data[i] - data[i - 1]))
         c++;

   return ((c * 2.0) / (len - 2));
}

static void ExtractFloats(bool doublePrec,
                          bool bigendian,
                          bool stereo,
                          int offset,
                          char *rawData, int dataSize,
                          float *data1, float *data2, int *len1, int *len2)
{
   int rawCount = 0;
   int dataCount1 = 0;
   int dataCount2 = 0;
   int i;
   bool swap;

   *len1 = 0;
   *len2 = 0;

   if (offset) {
      rawData += offset;
      dataSize -= offset;
   }

   #if WORDS_BIGENDIAN
   swap = !bigendian;
   #else
   swap = bigendian;
   #endif

   if (doublePrec) {
      union {
         unsigned char c[8];
         double d;
      } u;

      u.d = 0.0f;
      while (rawCount + 7 < dataSize) {
         if (swap)
            for(i=0; i<8; i++)
               u.c[7-i] = rawData[rawCount+i];
         else
            for(i=0; i<8; i++)
               u.c[i] = rawData[rawCount+i];
         data1[dataCount1] = (float)u.d;
         dataCount1++;
         rawCount += 8;
      }
   }
   else {
      union {
         unsigned char c[4];
         float f;
      } u;

      u.f = 0.0f;
      while (rawCount + 3 < dataSize) {
         if (swap)
            for(i=0; i<4; i++)
               u.c[3-i] = rawData[rawCount+i];
         else
            for(i=0; i<4; i++)
               u.c[i] = rawData[rawCount+i];
         data1[dataCount1] = u.f;
         dataCount1++;
         rawCount += 4;
      }
   }

   if (stereo) {
      dataCount1 /= 2;
      for(i=0; i<dataCount1; i++) {
         data2[i] = data1[2*i+1];
         data1[i] = data1[2*i];
      }
      dataCount2 = dataCount1;
   }

   *len1 = dataCount1;
   *len2 = dataCount2;
}

static void Extract(bool bits16,
                    bool sign,
                    bool stereo,
                    bool bigendian,
                    bool offset,
                    char *rawData, int dataSize,
                    float *data1, float *data2, int *len1, int *len2)
{
   int rawCount = 0;
   int dataCount1 = 0;
   int dataCount2 = 0;
   int i;

   *len1 = 0;
   *len2 = 0;

   if (offset && bits16) {
      /* Special case so as to not flip stereo channels during analysis */
      if (stereo && !bigendian) {
         rawData += 3;
         dataSize -= 3;
      }
      else {
         rawData++;
         dataSize--;
      }
   }

   if (bits16) {
      if (sign && bigendian)
         while (rawCount + 1 < dataSize) {
            /* 16-bit signed BE */
            data1[dataCount1] =
               (wxINT16_SWAP_ON_LE(*((signed short *)
                                     &rawData[rawCount])))
               / 32768.0;
            dataCount1++;
            rawCount += 2;
         }
      if (!sign && bigendian)
         while (rawCount + 1 < dataSize) {
            /* 16-bit unsigned BE */
            data1[dataCount1] =
               (wxUINT16_SWAP_ON_LE(*((unsigned short *)
                                      &rawData[rawCount])))
               / 32768.0 - 1.0;
            dataCount1++;
            rawCount += 2;
         }
      if (sign && !bigendian)
         while (rawCount + 1 < dataSize) {
            /* 16-bit signed LE */
            data1[dataCount1] =
               (wxINT16_SWAP_ON_BE(*((signed short *)
                                     &rawData[rawCount])))
               / 32768.0;
            dataCount1++;
            rawCount += 2;
         }
      if (!sign && !bigendian)
         while (rawCount + 1 < dataSize) {
            /* 16-bit unsigned LE */
            data1[dataCount1] =
               (wxUINT16_SWAP_ON_BE(*((unsigned short *)
                                      &rawData[rawCount])))
               / 32768.0 - 1.0;
            dataCount1++;
            rawCount += 2;
         }
   }
   else {
      /* 8-bit */
      if (sign) {
         while (rawCount < dataSize) {
            /* 8-bit signed */
            data1[dataCount1++] =
               (*(signed char *) (&rawData[rawCount++])) / 128.0;
         }
      }
      else {
         while (rawCount < dataSize) {
            /* 8-bit unsigned */
            data1[dataCount1++] =
               (*(unsigned char *) &rawData[rawCount++]) / 128.0 - 1.0;
         }
      }
   }

   if (stereo) {
      dataCount1 /= 2;
      for(i=0; i<dataCount1; i++) {
         data2[i] = data1[2*i+1];
         data1[i] = data1[2*i];
      }
      dataCount2 = dataCount1;
   }

   *len1 = dataCount1;
   *len2 = dataCount2;
}

static int GuessFloatFormats(int numTests, char **rawData, int dataSize,
                             int *out_offset, unsigned *out_channels)
{
   int format;
   int bestOffset = 0;
   int bestEndian = 0;
   int bestPrec = 0;
   float bestSmoothAvg = 1000.0;
   int offset;
   int endian;
   int prec;
   float *data1, *data2;
   int len1;
   int len2;
   int test;
   int i;
   bool guessStereo = false;
   int stereoVotes = 0;
   int monoVotes = 0;

  #if RAW_GUESS_DEBUG
   FILE *af = g_raw_debug_file;
   fprintf(af, "Testing float\n");
  #endif

   data1 = (float *)malloc((dataSize + 4) * sizeof(float));
   data2 = (float *)malloc((dataSize + 4) * sizeof(float));

   /*
    * First determine if it is possibly in a floating-point
    * format.  The nice thing about floating-point formats
    * is that random bytes or even random integers are
    * extremely unlikely to result in meaningful floats.
    * All we do is try interpreting the raw bytes as floating-point,
    * with a variety of offsets, both endiannesses, and both
    * precisions (32-bit float and 64-bit double), and if any of
    * them result in all finite numbers with reasonable ranges,
    * we accept them.
    *
    * Sometimes there is more than one plausible candidate, in
    * which case we take the smoothest one.  This usually happens
    * because big-endian floats actually still look and act
    * like floats when you interpret them as little-endian
    * floats with a 1-byte offset.
    */

   for(prec=0; prec<2; prec++) {
      for(endian=0; endian<2; endian++) {
         for(offset=0; offset<(4*prec+4); offset++) {
            int finiteVotes = 0;
            int maxminVotes = 0;
            float smoothAvg = 0;

           #if RAW_GUESS_DEBUG
            fprintf(af, "prec=%d endian=%d offset=%d\n",
                    prec, endian, offset);
           #endif

            for(test=0; test<numTests; test++) {
               float min, max;

               ExtractFloats(prec?true:false, endian?true:false,
                             true, /* stereo */
                             offset,
                             rawData[test], dataSize,
                             data1, data2, &len1, &len2);

               for(i=0; i<len1; i++)
                  if (!(data1[i]>=0 || data1[i]<=0) ||
                      !(data2[i]>=0 || data2[i]<=0))
                     break;
               if (i == len1)
                  finiteVotes++;

               min = data1[0];
               max = data1[0];
               for(i=1; i<len1; i++) {
                  if (data1[i]<min)
                     min = data1[i];
                  if (data1[i]>max)
                     max = data1[i];
               }
               for(i=1; i<len2; i++) {
                  if (data2[i]<min)
                     min = data2[i];
                  if (data2[i]>max)
                     max = data2[i];
               }

               if (min < -0.01 && min >= -100000 &&
                   max > 0.01 && max <= 100000)
                  maxminVotes++;

               smoothAvg += SecondDStat(data1, len1) / max;
            }

            smoothAvg /= numTests;

           #if RAW_GUESS_DEBUG
            fprintf(af, "finite: %d/%d maxmin: %d/%d smooth: %f\n",
                    finiteVotes, numTests, maxminVotes, numTests,
                    smoothAvg);
           #endif

            if (finiteVotes > numTests/2 &&
                finiteVotes > numTests-2 &&
                maxminVotes > numTests/2 &&
                smoothAvg < bestSmoothAvg) {

               bestSmoothAvg = smoothAvg;
               bestOffset = offset;
               bestPrec = prec;
               bestEndian = endian;
            }
         }
      }
   }

   /*
    * If none of those tests succeeded, it's probably not
    * actually floating-point data.  Return 0 so the
    * main function will try guessing an integer format.
    */

   if (bestSmoothAvg >= 1000.0) {
      free(data1);
      free(data2);
      return 0;
   }

   /*
    * We still have to test for mono/stereo.  For an explanation
    * of these tests, see the comments next to the stereo/mono
    * tests for 8-bit or 16-bit data.
    */

   for (test = 0; test < numTests; test++) {
      float leftChannel, rightChannel, combinedChannel;

      ExtractFloats(bestPrec?true:false, bestEndian?true:false,
                    true, /* stereo */
                    bestOffset,
                    rawData[test], dataSize,
                    data1, data2, &len1, &len2);
      leftChannel = JumpStat(data1, len1);
      rightChannel = JumpStat(data2, len2);
      ExtractFloats(bestPrec?true:false, bestEndian?true:false,
                    false, /* stereo */
                    bestOffset,
                    rawData[test], dataSize,
                    data1, data2, &len1, &len2);
      combinedChannel = JumpStat(data1, len1);

      if (leftChannel > combinedChannel
          && rightChannel > combinedChannel)
         stereoVotes++;
      else
         monoVotes++;
   }

  #if RAW_GUESS_DEBUG
   fprintf(af, "stereo: %d mono: %d\n", stereoVotes, monoVotes);
  #endif

   if (stereoVotes > monoVotes)
      guessStereo = true;
   else
      guessStereo = false;

   if (guessStereo == false) {

      /* test for repeated-byte, redundant stereo */

      int rstereoVotes = 0;
      int rmonoVotes = 0;

      for (test = 0; test < numTests; test++) {
         float redundant;

         ExtractFloats(bestPrec?true:false, bestEndian?true:false,
                       false, /* stereo */
                       bestOffset,
                       rawData[test], dataSize,
                       data1, data2, &len1, &len2);
         redundant = RedundantStereo(data1, len1);

        #if RAW_GUESS_DEBUG
         fprintf(af, "redundant: %f\n", redundant);
        #endif

         if (redundant > 0.8)
            rstereoVotes++;
         else
            rmonoVotes++;
      }

     #if RAW_GUESS_DEBUG
      fprintf(af, "rstereo: %d rmono: %d\n", rstereoVotes, rmonoVotes);
     #endif

      if (rstereoVotes > rmonoVotes)
         guessStereo = true;

   }

  #if RAW_GUESS_DEBUG
   if (guessStereo)
      fprintf(af, "stereo\n");
   else
      fprintf(af, "mono\n");
  #endif

   *out_offset = bestOffset;

   if (guessStereo)
      *out_channels = 2;
   else
      *out_channels = 1;

   if (bestPrec)
      format = SF_FORMAT_RAW | SF_FORMAT_DOUBLE;
   else
      format = SF_FORMAT_RAW | SF_FORMAT_FLOAT;

   if (bestEndian)
      format |= SF_ENDIAN_BIG;
   else
      format |= SF_ENDIAN_LITTLE;

   free(data1);
   free(data2);

   return format;
}

static int Guess8Bit(int numTests, char **rawData, int dataSize, unsigned *out_channels)
{
   bool guessSigned = false;
   bool guessStereo = false;
   int signvotes = 0;
   int unsignvotes = 0;
   int stereoVotes = 0;
   int monoVotes = 0;
   float *data1 = (float *)malloc((dataSize + 4) * sizeof(float));
   float *data2 = (float *)malloc((dataSize + 4) * sizeof(float));
   int len1;
   int len2;
   int test;

  #if RAW_GUESS_DEBUG
   FILE *af = g_raw_debug_file;
   fprintf(af, "8-bit\n");
  #endif

   /*
    * Compare signed to unsigned, interpreted as if the file were
    * stereo just to be safe.  If the file is actually mono, the test
    * still works, and we lose a tiny bit of accuracy.  (It would not make
    * sense to assume the file is mono, because if the two tracks are not
    * very similar we would get inaccurate results.)
    *
    * The JumpTest measures the average jump between two successive samples
    * and returns a value 0-1.  0 is maximally discontinuous, 1 is smooth.
    */

   for (test = 0; test < numTests; test++) {
      float signL, signR, unsignL, unsignR;

      Extract(0, 1, 1, 0, /* 8-bit signed stereo */
              false, rawData[test], dataSize,
              data1, data2, &len1, &len2);
      signL = JumpStat(data1, len1);
      signR = JumpStat(data2, len2);
      Extract(0, 0, 1, 0, /* 8-bit unsigned stereo */
              false, rawData[test], dataSize,
              data1, data2, &len1, &len2);
      unsignL = JumpStat(data1, len1);
      unsignR = JumpStat(data2, len2);

      if (signL > unsignL)
         signvotes++;
      else
         unsignvotes++;

      if (signR > unsignR)
         signvotes++;
      else
         unsignvotes++;
   }

  #if RAW_GUESS_DEBUG
   fprintf(af, "sign: %d unsign: %d\n", signvotes, unsignvotes);
  #endif

   if (signvotes > unsignvotes)
      guessSigned = true;
   else
      guessSigned = false;

  #if RAW_GUESS_DEBUG
   if (guessSigned)
      fprintf(af, "signed\n");
   else
      fprintf(af, "unsigned\n");
  #endif

   /* Finally we test stereo/mono.  We use the same JumpStat, and say
    * that the file is stereo if and only if for the majority of the
    * tests, the left channel and the right channel are more smooth than
    * the entire stream interpreted as one channel.
    */

   for (test = 0; test < numTests; test++) {
      float leftChannel, rightChannel, combinedChannel;

      Extract(0, guessSigned, 1, 0, 0, rawData[test], dataSize, data1,
              data2, &len1, &len2);
      leftChannel = JumpStat(data1, len1);
      rightChannel = JumpStat(data2, len2);
      Extract(0, guessSigned, 0, 0, 0, rawData[test], dataSize, data1,
              data2, &len1, &len2);
      combinedChannel = JumpStat(data1, len1);

      if (leftChannel > combinedChannel
          && rightChannel > combinedChannel)
         stereoVotes++;
      else
         monoVotes++;
   }

  #if RAW_GUESS_DEBUG
   fprintf(af, "stereo: %d mono: %d\n", stereoVotes, monoVotes);
  #endif

   if (stereoVotes > monoVotes)
      guessStereo = true;
   else
      guessStereo = false;

   if (guessStereo == false) {

      /* test for repeated-byte, redundant stereo */

      int rstereoVotes = 0;
      int rmonoVotes = 0;

      for (test = 0; test < numTests; test++) {
         float redundant;

         Extract(0, guessSigned, 0, 0, 0, rawData[test], dataSize,
                 data1, data2, &len1, &len2);
         redundant = RedundantStereo(data1, len1);

        #if RAW_GUESS_DEBUG
         fprintf(af, "redundant: %f\n", redundant);
        #endif

         if (redundant > 0.8)
            rstereoVotes++;
         else
            rmonoVotes++;
      }

     #if RAW_GUESS_DEBUG
      fprintf(af, "rstereo: %d rmono: %d\n", rstereoVotes, rmonoVotes);
     #endif

      if (rstereoVotes > rmonoVotes)
         guessStereo = true;

   }

  #if RAW_GUESS_DEBUG
   if (guessStereo)
      fprintf(af, "stereo\n");
   else
      fprintf(af, "mono\n");
  #endif

   free(data1);
   free(data2);

   if (guessStereo)
      *out_channels = 2;
   else
      *out_channels = 1;

   if (guessSigned)
      return SF_FORMAT_RAW | SF_FORMAT_PCM_S8;
   else
      return SF_FORMAT_RAW | SF_FORMAT_PCM_U8;
}

static int Guess16Bit(int numTests, char **rawData,
                      int dataSize, bool evenMSB,
                      int *out_offset, unsigned *out_channels)
{
   int format;
   bool guessSigned = false;
   bool guessStereo = false;
   bool guessBigEndian = false;
   bool guessOffset = false;
   int signvotes = 0;
   int unsignvotes = 0;
   int stereoVotes = 0;
   int monoVotes = 0;
   int formerVotes = 0;
   int latterVotes = 0;
   char *rawData2 = (char *)malloc(dataSize + 4);
   float *data1 = (float *)malloc((dataSize + 4) * sizeof(float));
   float *data2 = (float *)malloc((dataSize + 4) * sizeof(float));
   int len1;
   int len2;
   int test;

  #if RAW_GUESS_DEBUG
   FILE *af = g_raw_debug_file;
   fprintf(af, "16-bit\n");
  #endif

   /*
    * Do the signed/unsigned test by using only the MSB.
    */

   for (test = 0; test < numTests; test++) {

      float signL, signR, unsignL, unsignR;
      int i;

      /* Extract a NEW array of the MSBs only: */

      for (i = 0; i < dataSize / 2; i++)
         rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];

      /* Test signed/unsigned of the MSB */

      Extract(0, 1, 1, 0, /* 8-bit signed stereo */
              0, rawData2, dataSize / 2, data1, data2, &len1, &len2);
      signL = JumpStat(data1, len1);
      signR = JumpStat(data2, len2);
      Extract(0, 0, 1, 0, /* 8-bit unsigned stereo */
              0, rawData2, dataSize / 2, data1, data2, &len1, &len2);
      unsignL = JumpStat(data1, len1);
      unsignR = JumpStat(data2, len2);

      if (signL > unsignL)
         signvotes++;
      else
         unsignvotes++;

      if (signR > unsignR)
         signvotes++;
      else
         unsignvotes++;
   }

  #if RAW_GUESS_DEBUG
   fprintf(af, "sign: %d unsign: %d\n", signvotes, unsignvotes);
  #endif

   if (signvotes > unsignvotes)
      guessSigned = true;
   else
      guessSigned = false;

  #if RAW_GUESS_DEBUG
   if (guessSigned)
      fprintf(af, "signed\n");
   else
      fprintf(af, "unsigned\n");
  #endif

   /*
    * Test mono/stereo using only the MSB
    */

   for (test = 0; test < numTests; test++) {
      float leftChannel, rightChannel, combinedChannel;
      int i;

      /* Extract a NEW array of the MSBs only: */

      for (i = 0; i < dataSize / 2; i++)
         rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];

      Extract(0, guessSigned, 1, 0, 0,
              rawData2, dataSize / 2, data1, data2, &len1, &len2);
      leftChannel = JumpStat(data1, len1);
      rightChannel = JumpStat(data2, len2);
      Extract(0, guessSigned, 0, 0, 0,
              rawData2, dataSize / 2, data1, data2, &len1, &len2);
      combinedChannel = JumpStat(data1, len1);

      if (leftChannel > combinedChannel
          && rightChannel > combinedChannel)
         stereoVotes++;
      else
         monoVotes++;
   }

  #if RAW_GUESS_DEBUG
   fprintf(af, "stereoVotes: %d monoVotes: %d\n", stereoVotes, monoVotes);
  #endif

   if (stereoVotes > monoVotes)
      guessStereo = true;
   else
      guessStereo = false;

   if (guessStereo == false) {

      /* Test for repeated-byte, redundant stereo */

      int rstereoVotes = 0;
      int rmonoVotes = 0;

      for (test = 0; test < numTests; test++) {

         float redundant;
         int i;

         /* Extract a NEW array of the MSBs only: */

         for (i = 0; i < dataSize / 2; i++)
            rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];

         Extract(0, guessSigned, 0, 0, 0, rawData2, dataSize / 2,
                 data1, data2, &len1, &len2);

         redundant = RedundantStereo(data1, len1);

         if (redundant > 0.8)
            rstereoVotes++;
         else
            rmonoVotes++;
      }

     #if RAW_GUESS_DEBUG
      fprintf(af, "rstereoVotes: %d rmonoVotes: %d\n",
              rstereoVotes, rmonoVotes);
     #endif

      if (rstereoVotes > rmonoVotes)
         guessStereo = true;

   }

  #if RAW_GUESS_DEBUG
   if (guessStereo)
      fprintf(af, "stereo\n");
   else
      fprintf(af, "mono\n");
  #endif

   /*
    * Finally, determine the endianness and offset.
    *
    * Even MSB -> BigEndian or LittleEndian with Offset
    * Odd MSB -> LittleEndian or BigEndian with Offset
    */

   guessBigEndian = evenMSB;
   guessOffset = 0;

  #if RAW_GUESS_DEBUG
   fprintf(af, "evenMSB: %d BE: %d\n", evenMSB, guessBigEndian);
  #endif

   for (test = 0; test < numTests; test++) {

      float former, latter;
      int i, offs;

      /* Extract a NEW array of the MSBs only: */

      if (guessStereo)
         for (i = 0; i < (dataSize/4)-1; i++)
            rawData2[i] =
               rawData[test][4 * i + (evenMSB ? 0 : 1)];
      else
         for (i = 0; i < (dataSize/2)-1; i++)
            rawData2[i] =
               rawData[test][2 * i + (evenMSB ? 0 : 1)];

      former = 0.0;
      Extract(1, guessSigned, guessStereo, guessBigEndian, guessOffset,
              rawData[test], dataSize-4, data1, data2, &len1, &len2);

      offs=(!guessBigEndian);

      for(i=3; i<len1-4; i++) {
         if (rawData2[offs+i-2]==rawData2[offs+i-1] &&
             rawData2[offs+i]==rawData2[offs+i-1]+1 &&
             rawData2[offs+i]==rawData2[offs+i+1]) {
            former += data1[i]-data1[i-1];
         }
      }

      latter = 0.0;
      Extract(1, guessSigned, guessStereo, !guessBigEndian,
              !guessOffset, rawData[test], dataSize, data1, data2,
              &len1, &len2);

      offs=(guessBigEndian);

      for(i=3; i<len1-4; i++) {
         if (rawData2[offs+i-2]==rawData2[offs+i-1] &&
             rawData2[offs+i]==rawData2[offs+i-1]+1 &&
             rawData2[offs+i]==rawData2[offs+i+1]) {

            latter += data1[i]-data1[i-1];
         }
      }

     #if RAW_GUESS_DEBUG
      fprintf(af, "former: %f latter: %f\n", former, latter);
     #endif

      if (former <= latter)
         formerVotes++;
      else
         latterVotes++;
   }

  #if RAW_GUESS_DEBUG
   fprintf(af, "former (BE/LE): %d latter (LE+/BE+): %d\n",
           formerVotes, latterVotes);
  #endif

   // High barrier, since odd byte offsets are very rare
   if (latterVotes > formerVotes*2) {
      guessBigEndian = !guessBigEndian;
      guessOffset = !guessOffset;
   }

  #if RAW_GUESS_DEBUG
   if (guessBigEndian)
      fprintf(af, "big endian\n");
   else
      fprintf(af, "little endian\n");
  #endif

  #if RAW_GUESS_DEBUG
   if (guessOffset)
      fprintf(af, "offset 1 byte\n");
   else
      fprintf(af, "no byte offset\n");
  #endif

   format = SF_FORMAT_RAW | SF_FORMAT_PCM_16;

   if (guessBigEndian)
      format |= SF_ENDIAN_BIG;
   else
      format |= SF_ENDIAN_LITTLE;

   if (guessOffset)
      *out_offset = 1;

   if (guessStereo)
      *out_channels = 2;
   else
      *out_channels = 1;

   free(rawData2);

   free(data1);
   free(data2);

   return format;
}

static int GuessIntFormats(int numTests, char **rawData, int dataSize,
                           int *out_offset, unsigned *out_channels)
{
   int format = SF_FORMAT_RAW;
   bool guess16bit = false;
   bool evenMSB;
   float *data1 = (float *)malloc((dataSize + 4) * sizeof(float));
   float *data2 = (float *)malloc((dataSize + 4) * sizeof(float));
   int len1;
   int len2;
   int vote8 = 0;
   int vote16 = 0;
   int evenMSBVotes = 0;
   int oddMSBVotes = 0;
   int test;

  #if RAW_GUESS_DEBUG
   FILE *af = g_raw_debug_file;
  #endif

   *out_channels = 1;
   *out_offset = 0;

   /*
    * First test: we attempt to determine if the data is 8-bit or 16-bit.
    * We extract the odd and even bytes interpreted as signed-valued samples,
    * and compare their amplitude distributions.  Noting that in 16-bit values,
    * the less significant 8 bits should have roughly flat distribution, while
    * the more significant 8 bits should have a tighter distribution, with a
    * smaller standard deviation.
    *
    * Note that this correctly makes the distinction whether we are dealing
    * with mono or stereo data.
    */

   for (test = 0; test < numTests; test++) {
      float even, odd;

      Extract(0, 1, 1, 0, /* 8-bit signed stereo */
              false, rawData[test], dataSize,
              data1, data2, &len1, &len2);
      even = AmpStat(data1, len1);
      odd = AmpStat(data2, len2);
      if ((even > 0.15) && (odd > 0.15)) {
        #if RAW_GUESS_DEBUG
         fprintf(af, "Both appear random: %.2f, %.2f.\n", even, odd);
        #endif
      }
      else if ((even > 0.15) || (odd > 0.15))
         vote16++;
      else
         vote8++;

      /* Record which of the two was the MSB for future reference */
      if (even < odd)
         evenMSBVotes++;
      else
         oddMSBVotes++;
   }

   evenMSB = (evenMSBVotes > oddMSBVotes);

  #if RAW_GUESS_DEBUG
   fprintf(af, "evenMSBVote: %d oddMSBVote: %d\n",
           evenMSBVotes, oddMSBVotes);
   fprintf(af, "vote8: %d vote16: %d\n", vote8, vote16);
  #endif

   if (vote8 > vote16)
      guess16bit = false;
   else
      guess16bit = true;

   if (!guess16bit)
      format = Guess8Bit(numTests, rawData, dataSize, out_channels);
   else
      format = Guess16Bit(numTests, rawData,
                          dataSize, evenMSB,
                          out_offset, out_channels);

   free(data1);
   free(data2);

   return format;
}

int RawAudioGuess(const wxString &in_fname,
                  int *out_offset, unsigned *out_channels)
{
   const int numTests = 11;
   size_t headerSkipSize = 64;
   size_t dataSize = 16384;
   int format = SF_FORMAT_RAW;
   FILE *inf;
   size_t fileLen;
   char *rawData[numTests];
   int test;
   size_t read_data;

  #if RAW_GUESS_DEBUG
   FILE *af = fopen("raw.txt", "a");
   g_raw_debug_file = af;
   fprintf(af, "File: %s\n", in_fname);
  #endif

   *out_offset = 0;
   *out_channels = 1;

   wxFFile in_wxFFile(in_fname, wxT("rb"));

   // JKC FALSE changed to -1.
   if (!in_wxFFile.IsOpened())
      return -1;
   inf = in_wxFFile.fp();

   if (!inf) {
     #if RAW_GUESS_DEBUG
      fclose(af);
      g_raw_debug_file = NULL;
     #endif

      return -1;
   }

   // FIXME: TRAP_ERR fseek return in RawAudioGuess unchecked.
   fseek(inf, 0, SEEK_END);
   fileLen = ftell(inf);

   if (fileLen < 8)
      return -1;

   if (fileLen < headerSkipSize)
      headerSkipSize = 0;

   if (fileLen < dataSize)
      dataSize = fileLen / 2;

   for (test = 0; test < numTests; test++) {
      int startPoint;

      rawData[test] = (char *)malloc(dataSize + 4);
      startPoint = (fileLen - dataSize) * (test + 1) / (numTests + 2);

      /* Make it a multiple of 16 (stereo double-precision) */
      startPoint = (startPoint/16)*16;

      // FIXME: TRAP_ERR fseek return in MultiFormatReader unchecked.
      fseek(inf, headerSkipSize + startPoint, SEEK_SET);
      read_data = fread(rawData[test], 1, dataSize, inf);
      if (read_data != dataSize && ferror(inf)) {
         perror("fread error in RawAudioGuess");
      }
   }

   in_wxFFile.Close();

   /*
    * The floating-point tests will only return a valid format
    * if it's almost certainly floating-point data.  On the other
    * hand, the integer tests will always return something, since
    * almost anything looks like it could be integer data...
    */

   format = GuessFloatFormats(numTests, rawData, dataSize,
                              out_offset, out_channels);

   if (format == 0) {
      format = GuessIntFormats(numTests, rawData, dataSize,
                               out_offset, out_channels);
   }

   for (test = 0; test < numTests; test++)
      free(rawData[test]);

  #if RAW_GUESS_DEBUG
   fclose(af);
   g_raw_debug_file = NULL;
  #endif

   return format;
}
