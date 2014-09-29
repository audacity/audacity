/**********************************************************************

  Audacity: A Digital Audio Editor

  SampleFormat.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_FORMAT__
#define __AUDACITY_SAMPLE_FORMAT__

#include "Audacity.h"
#include <wx/defs.h>

//
// Definitions / Meta-Information
//

typedef enum {
   int16Sample = 0x00020001,
   int24Sample = 0x00040001,
   floatSample = 0x0004000F
} sampleFormat;

// Used to determine how to fill in empty areas of audio.
typedef enum {
   fillZero = 0,
   fillTwo = 2
}fillFormat;

/** \brief Return the size (in memory) of one sample (bytes) */
#define SAMPLE_SIZE(SampleFormat) (SampleFormat >> 16)
/** \brief Return the size on disk of one uncompressed sample (bytes) */
#define SAMPLE_SIZE_DISK(SampleFormat) ((SampleFormat == int24Sample) ? \
   3 : SAMPLE_SIZE(SampleFormat) )

typedef char *samplePtr;

const wxChar *GetSampleFormatStr(sampleFormat format);

//
// Allocating/Freeing Samples
//

AUDACITY_DLL_API samplePtr NewSamples(int count, sampleFormat format);
AUDACITY_DLL_API void DeleteSamples(samplePtr p);

//
// Copying, Converting and Clearing Samples
//

void      CopySamples(samplePtr src, sampleFormat srcFormat,
                      samplePtr dst, sampleFormat dstFormat,
                      unsigned int len, bool highQuality=true,
                      unsigned int srcStride=1,
                      unsigned int dstStride=1);

void      CopySamplesNoDither(samplePtr src, sampleFormat srcFormat,
                      samplePtr dst, sampleFormat dstFormat,
                      unsigned int len,
                      unsigned int srcStride=1,
                      unsigned int dstStride=1);

void      ClearSamples(samplePtr buffer, sampleFormat format,
                       int start, int len);

//
// This must be called on startup and everytime new ditherers
// are set in preferences.
//

void      InitDitherers();

#endif
