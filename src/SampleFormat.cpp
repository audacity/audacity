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
  It also means (unfortunatly) that writing out +1.0f leads to
  clipping by 1 LSB.  This creates some distortion, but I (MJS) have
  not been able to measure it, it's so small.  Zero is preserved.

  http://limpet.net/audacity/bugzilla/show_bug.cgi?id=200
  leads to some of the discussions that were held about this.

   Note: These things are now handled by the Dither class, which
         also replaces the CopySamples() method (msmeyer)

*//*******************************************************************/

#include <wx/intl.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SampleFormat.h"
#include "Prefs.h"
#include "Dither.h"

static Dither::DitherType gLowQualityDither = Dither::none;
static Dither::DitherType gHighQualityDither = Dither::none;
static Dither gDitherAlgorithm;

void InitDitherers()
{
   // Read dither preferences
   gLowQualityDither = (Dither::DitherType)
   gPrefs->Read(wxT("/Quality/DitherAlgorithm"), (long)Dither::none);

   gHighQualityDither = (Dither::DitherType)
   gPrefs->Read(wxT("/Quality/HQDitherAlgorithm"), (long)Dither::shaped);
}

const wxChar *GetSampleFormatStr(sampleFormat format)
{
   switch(format) {
   case int16Sample:
      /* i18n-hint: Audio data bit depth (precision): 16-bit integers */
      return _("16-bit PCM");
   case int24Sample:
      /* i18n-hint: Audio data bit depth (precision): 24-bit integers */
      return _("24-bit PCM");
   case floatSample:
      /* i18n-hint: Audio data bit depth (precision): 32-bit floating point */
      return _("32-bit float");
   }
   return wxT("Unknown format"); // compiler food
}

AUDACITY_DLL_API samplePtr NewSamples(size_t count, sampleFormat format)
{
   return (samplePtr)malloc(count * SAMPLE_SIZE(format));
}

AUDACITY_DLL_API void DeleteSamples(samplePtr p)
{
   free(p);
}

// TODO: Risky?  Assumes 0.0f is represented by 0x00000000;
void ClearSamples(samplePtr src, sampleFormat format,
                  size_t start, size_t len)
{
   auto size = SAMPLE_SIZE(format);
   memset(src + start*size, 0, len*size);
}

void ReverseSamples(samplePtr src, sampleFormat format,
                  int start, int len)
{
   auto size = SAMPLE_SIZE(format);
   samplePtr first = src + start * size;
   samplePtr last = src + (start + len - 1) * size;
   enum : size_t { fixedSize = SAMPLE_SIZE(floatSample) };
   wxASSERT(size <= fixedSize);
   char temp[fixedSize];
   while (first < last) {
      memcpy(temp, first, size);
      memcpy(first, last, size);
      memcpy(last, temp, size);
      first += size;
      last -= size;
   }
}

void CopySamples(samplePtr src, sampleFormat srcFormat,
                 samplePtr dst, sampleFormat dstFormat,
                 unsigned int len,
                 bool highQuality, /* = true */
                 unsigned int srcStride /* = 1 */,
                 unsigned int dstStride /* = 1 */)
{
   gDitherAlgorithm.Apply(
      highQuality ? gHighQualityDither : gLowQualityDither,
      src, srcFormat, dst, dstFormat, len, srcStride, dstStride);
}

void CopySamplesNoDither(samplePtr src, sampleFormat srcFormat,
                 samplePtr dst, sampleFormat dstFormat,
                 unsigned int len,
                 unsigned int srcStride /* = 1 */,
                 unsigned int dstStride /* = 1 */)
{
   gDitherAlgorithm.Apply(
      Dither::none,
      src, srcFormat, dst, dstFormat, len, srcStride, dstStride);
}
