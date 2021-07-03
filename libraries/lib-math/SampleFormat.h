/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SampleFormat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_FORMAT__
#define __AUDACITY_SAMPLE_FORMAT__

#include "MemoryX.h"
#include <cstdlib>

//
// Definitions / Meta-Information
//

enum DitherType : unsigned;
//! These global variables are assigned at application startup or after change of preferences.
extern MATH_API DitherType gLowQualityDither, gHighQualityDither;

// ----------------------------------------------------------------------------
// Supported sample formats
// ----------------------------------------------------------------------------
enum sampleFormat : unsigned
{
   //! The increasing sequence of these enum values must correspond to the increasing data type width
   //! These values persist in saved project files, so must not be changed in later program versions
   int16Sample = 0x00020001,
   int24Sample = 0x00040001,
   floatSample = 0x0004000F,

   //! Two synonyms for previous values that might change if more values were added
   narrowestSampleFormat = int16Sample,
   widestSampleFormat = floatSample,
};

// ----------------------------------------------------------------------------
// Provide the number of bytes a specific sample will take
// ----------------------------------------------------------------------------
#define SAMPLE_SIZE(SampleFormat) (SampleFormat >> 16)

// ----------------------------------------------------------------------------
// Generic pointer to sample data
// ----------------------------------------------------------------------------
using samplePtr = char *;
using constSamplePtr = const char *;

// Used to determine how to fill in empty areas of audio.
typedef enum {
   fillZero = 0,
   fillTwo = 2
}fillFormat;

/** \brief Return the size on disk of one uncompressed sample (bytes) */
#define SAMPLE_SIZE_DISK(SampleFormat) (((SampleFormat) == int24Sample) ? \
   size_t{ 3 } : SAMPLE_SIZE(SampleFormat) )

class TranslatableString;
MATH_API TranslatableString GetSampleFormatStr(sampleFormat format);

//
// Allocating/Freeing Samples
//

class SampleBuffer {

public:
   SampleBuffer()
      : mPtr(0)
   {}
   SampleBuffer(size_t count, sampleFormat format)
      : mPtr((samplePtr)malloc(count * SAMPLE_SIZE(format)))
   {}
   ~SampleBuffer()
   {
      Free();
   }

   // WARNING!  May not preserve contents.
   SampleBuffer &Allocate(size_t count, sampleFormat format)
   {
      Free();
      mPtr = (samplePtr)malloc(count * SAMPLE_SIZE(format));
      return *this;
   }


   void Free()
   {
      free(mPtr);
      mPtr = 0;
   }

   samplePtr ptr() const { return mPtr; }


private:
   samplePtr mPtr;
};

class GrowableSampleBuffer : private SampleBuffer
{
public:
   GrowableSampleBuffer()
      : SampleBuffer()
      , mCount(0)
   {}

   GrowableSampleBuffer(size_t count, sampleFormat format)
      : SampleBuffer(count, format)
      , mCount(count)
   {}

   GrowableSampleBuffer &Resize(size_t count, sampleFormat format)
   {
      if (!ptr() || mCount < count) {
         Allocate(count, format);
         mCount = count;
      }
      return *this;
   }

   void Free()
   {
      SampleBuffer::Free();
      mCount = 0;
   }

   using SampleBuffer::ptr;

private:
   size_t mCount;
};

//
// Copying, Converting and Clearing Samples
//

MATH_API
//! Copy samples from any format into the widest format, which is 32 bit float, with no dithering
/*!
 @param src address of source samples
 @param srcFormat format of source samples, determines sizeof each one
 @param dst address of floating-point numbers
 @param len count of samples to copy
 @param srcStride how many samples to advance src after copying each one
 @param dstString how many samples to advance dst after copying each one
 */
void SamplesToFloats(constSamplePtr src, sampleFormat srcFormat,
    float *dst, size_t len, size_t srcStride = 1, size_t dstStride = 1);

MATH_API
//! Copy samples from any format to any other format; apply dithering only if narrowing the format
/*!
 @copydetails SamplesToFloats()
 @param dstFormat format of destination samples, determines sizeof each one
 @param ditherType choice of dithering algorithm to use if narrowing the format
 */
void CopySamples(constSamplePtr src, sampleFormat srcFormat,
   samplePtr dst, sampleFormat dstFormat, size_t len,
   DitherType ditherType = gHighQualityDither, //!< default is loaded from a global variable
   unsigned int srcStride=1, unsigned int dstStride=1);

MATH_API
void      ClearSamples(samplePtr buffer, sampleFormat format,
                       size_t start, size_t len);

MATH_API
void      ReverseSamples(samplePtr buffer, sampleFormat format,
                         int start, int len);

//
// This must be called on startup and everytime NEW ditherers
// are set in preferences.
//

MATH_API
void      InitDitherers();

// These are so commonly done for processing samples in floating point form in memory,
// let's have abbreviations.
using Floats = ArrayOf<float>;
using FloatBuffers = ArraysOf<float>;
using Doubles = ArrayOf<double>;

#endif
