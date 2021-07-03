/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleFormat.h

  Dominic Mazzoni

*******************************************************************//*!

\file SampleFormat.cpp
\brief Functions that work with Dither and initialise it.


  This file handles converting between all of the different
  sample formats that Audacity supports, such as 16-bit,
  24-bit (packed into a 32-bit int), and 32-bit float.

  Floating-point samples use the range -1.0...1.0, inclusive.
  Integer formats use the full signed range of their data type,
  for example 16-bit samples use the range -32768...32767.
  This means that reading in a wav file and writing it out again
  ('round tripping'), via floats, is lossless; -32768 equates to -1.0f
  and 32767 equates to +1.0f - (a little bit).
  It also means (unfortunately) that writing out +1.0f leads to
  clipping by 1 LSB.  This creates some distortion, but I (MJS) have
  not been able to measure it, it's so small.  Zero is preserved.

  http://limpet.net/audacity/bugzilla/show_bug.cgi?id=200
  leads to some of the discussions that were held about this.

   Note: These things are now handled by the Dither class, which
         also replaces the CopySamples() method (msmeyer)

*//*******************************************************************/

#include "SampleFormat.h"
#include "Dither.h" // CYCLE

#include <wx/intl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Prefs.h"
#include "Internat.h"

DitherType gLowQualityDither = DitherType::none;
DitherType gHighQualityDither = DitherType::shaped;
static Dither gDitherAlgorithm;

void InitDitherers()
{
   // Read dither preferences
   gLowQualityDither = Dither::FastDitherChoice();
   gHighQualityDither = Dither::BestDitherChoice();
}

TranslatableString GetSampleFormatStr(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      /* i18n-hint: Audio data bit depth (precision): 16-bit integers */
      return XO("16-bit PCM");
   case int24Sample:
      /* i18n-hint: Audio data bit depth (precision): 24-bit integers */
      return XO("24-bit PCM");
   case floatSample:
      /* i18n-hint: Audio data bit depth (precision): 32-bit floating point */
      return XO("32-bit float");
   }
   return XO("Unknown format"); // compiler food
}

// TODO: Risky?  Assumes 0.0f is represented by 0x00000000;
void ClearSamples(samplePtr dst, sampleFormat format,
                  size_t start, size_t len)
{
   auto size = SAMPLE_SIZE(format);
   memset(dst + start*size, 0, len*size);
}

void ReverseSamples(samplePtr dst, sampleFormat format,
                  int start, int len)
{
   auto size = SAMPLE_SIZE(format);
   samplePtr first = dst + start * size;
   samplePtr last = dst + (start + len - 1) * size;
   enum : size_t { fixedSize = SAMPLE_SIZE(floatSample) };
   wxASSERT(static_cast<size_t>(size) <= fixedSize);
   char temp[fixedSize];
   while (first < last) {
      memcpy(temp, first, size);
      memcpy(first, last, size);
      memcpy(last, temp, size);
      first += size;
      last -= size;
   }
}

void  SamplesToFloats(constSamplePtr src, sampleFormat srcFormat,
   float *dst, size_t len, size_t srcStride, size_t dstStride)
{
   CopySamples( src, srcFormat,
      reinterpret_cast<samplePtr>(dst), floatSample, len,
      DitherType::none,
      srcStride, dstStride);
}

void CopySamples(constSamplePtr src, sampleFormat srcFormat,
   samplePtr dst, sampleFormat dstFormat, size_t len,
   DitherType ditherType, /* = gHighQualityDither */
   unsigned int srcStride /* = 1 */,
   unsigned int dstStride /* = 1 */)
{
   gDitherAlgorithm.Apply(
      ditherType,
      src, srcFormat, dst, dstFormat, len, srcStride, dstStride);
}
