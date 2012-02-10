/**********************************************************************

  Audacity: A Digital Audio Editor

  Dither.cpp

  Steve Harris
  Markus Meyer

*******************************************************************//*!

\class Dither
\brief

  This class implements various functions for dithering and is derived
  from the dither code in the Ardour project, written by Steve Harris.

  Dithering is only done if it really is necessary. Otherwise (e.g.
  when the source and destination format of the samples is the same),
  the samples are only copied or converted. However, copied samples
  are always checked for out-of-bounds values and possibly clipped
  accordingly.

  These dither algorithms are currently implemented:
  - No dithering at all
  - Rectangle dithering
  - Triangle dithering
  - Noise-shaped dithering

Dither class. You must construct an instance because it keeps
state. Call Dither::Apply() to apply the dither. You can call
Reset() between subsequent dithers to reset the dither state
and get deterministic behaviour.

*//*******************************************************************/


// Erik de Castro Lopo's header file that
// makes sure that we have lrint and lrintf
// (Note: this file should be included first)
#include "float_cast.h"

#include <stdlib.h>
#include <math.h>
#include <string.h>
//#include <sys/types.h>
//#include <memory.h>
//#include <assert.h>

#include <wx/defs.h>

#include "Dither.h"

//////////////////////////////////////////////////////////////////////////

// Constants for the noise shaping buffer
const int Dither::BUF_MASK = 7;
const int Dither::BUF_SIZE = 8;

// Lipshitz's minimally audible FIR
const float Dither::SHAPED_BS[] = { 2.033f, -2.165f, 1.959f, -1.590f, 0.6149f };

// This is supposed to produce white noise and no dc
#define DITHER_NOISE (rand() / (float)RAND_MAX - 0.5f)

// The following is a rather ugly, but fast implementation
// of a dither loop. The macro "DITHER" is expanded to an implementation
// of a dithering algorithm, which contains no branches in the inner loop
// except the branches for clipping the sample, and therefore should
// be quite fast.

// Defines for sample conversion
#define CONVERT_DIV16 float(1<<15)
#define CONVERT_DIV24 float(1<<23)

// Dereference sample pointer and convert to float sample
#define FROM_INT16(ptr) (*((short*)(ptr)) / CONVERT_DIV16)
#define FROM_INT24(ptr) (*((  int*)(ptr)) / CONVERT_DIV24)

// For float, we internally allow values greater than 1.0, which
// would blow up the dithering to int values.  FROM_FLOAT is
// only used to dither to int, so clip here.
#define FROM_FLOAT(ptr) (*((float*)(ptr)) >  1.0 ?  1.0 : \
                         *((float*)(ptr)) < -1.0 ? -1.0 : \
                         *((float*)(ptr)))

// Promote sample to range of specified type, keep it float, though
#define PROMOTE_TO_INT16(sample) ((sample) * CONVERT_DIV16)
#define PROMOTE_TO_INT24(sample) ((sample) * CONVERT_DIV24)

// Store float sample 'sample' into pointer 'ptr', clip it, if necessary
// Note: This assumes, a variable 'x' of type int is valid which is
//       used by this macro.
#define IMPLEMENT_STORE(ptr, sample, ptr_type, min_bound, max_bound) \
    do { \
    x = lrintf(sample); \
    if (x>(max_bound)) *((ptr_type*)(ptr))=(max_bound); \
    else if (x<(min_bound)) *((ptr_type*)(ptr))=(min_bound); \
    else *((ptr_type*)(ptr))=(ptr_type)x; } while (0)

#define STORE_INT16(ptr, sample) IMPLEMENT_STORE((ptr), (sample), short, -32768, 32767)
#define STORE_INT24(ptr, sample) IMPLEMENT_STORE((ptr), (sample), int, -8388608, 8388607)

// Dither single float 'sample' and store it in pointer 'dst', using 'dither' as algorithm
#define DITHER_TO_INT16(dither, dst, sample) STORE_INT16((dst), dither(PROMOTE_TO_INT16(sample)))
#define DITHER_TO_INT24(dither, dst, sample) STORE_INT24((dst), dither(PROMOTE_TO_INT24(sample)))

// Implement one single dither step
#define DITHER_STEP(dither, store, load, dst, src) \
    store(dither, (dst), load(src))

// Implement a dithering loop
// Note: The variable 'x' is needed for the STORE_... macros
#define DITHER_LOOP(dither, store, load, dst, dstFormat, src, srcFormat, len, stride) \
    do { \
       char *d, *s; \
       unsigned int i; \
       int x; \
       for (d = (char*)dst, s = (char*)src, i = 0; \
            i < len; \
            i++, d += SAMPLE_SIZE(dstFormat), \
                 s += SAMPLE_SIZE(srcFormat) * stride) \
          DITHER_STEP(dither, store, load, d, s); \
   } while (0)

// Shortcuts to dithering loops
#define DITHER_INT24_TO_INT16(dither, dst, src, len, stride) \
    DITHER_LOOP(dither, DITHER_TO_INT16, FROM_INT24, dst, int16Sample, src, int24Sample, len, stride)
#define DITHER_FLOAT_TO_INT16(dither, dst, src, len, stride) \
    DITHER_LOOP(dither, DITHER_TO_INT16, FROM_FLOAT, dst, int16Sample, src, floatSample, len, stride)
#define DITHER_FLOAT_TO_INT24(dither, dst, src, len, stride) \
    DITHER_LOOP(dither, DITHER_TO_INT24, FROM_FLOAT, dst, int24Sample, src, floatSample, len, stride)

// Implement a dither. There are only 3 cases where we must dither,
// in all other cases, no dithering is necessary.
#define DITHER(dither, dst, dstFormat, src, srcFormat, len, stride) \
    do { if (srcFormat == int24Sample && dstFormat == int16Sample) \
        DITHER_INT24_TO_INT16(dither, dst, src, len, stride); \
    else if (srcFormat == floatSample && dstFormat == int16Sample) \
        DITHER_FLOAT_TO_INT16(dither, dst, src, len, stride); \
    else if (srcFormat == floatSample && dstFormat == int24Sample) \
        DITHER_FLOAT_TO_INT24(dither, dst, src, len, stride); \
    else { wxASSERT(false); } \
    } while (0)


Dither::Dither()
{
    // On startup, initialize dither by resetting values
    Reset();
}

void Dither::Reset()
{
    mTriangleState = 0;
    mPhase = 0;
    memset(mBuffer, 0, sizeof(float) * BUF_SIZE);
}

// This only decides if we must dither at all, the dithers
// are all implemented using macros.
void Dither::Apply(enum DitherType ditherType,
                   const samplePtr source, sampleFormat sourceFormat,
                   samplePtr dest, sampleFormat destFormat,
                   unsigned int len, unsigned int stride /* = 1 */)
{
    unsigned int i;

    // This code is not designed for 16-bit or 64-bit machine
    wxASSERT(sizeof(int) == 4);
    wxASSERT(sizeof(short) == 2);

    // Check parameters
    wxASSERT(source);
    wxASSERT(dest);
    wxASSERT(len >= 0);
    wxASSERT(stride > 0);

    if (len == 0)
        return; // nothing to do

    if (sourceFormat == destFormat)
    {
        // No need to dither, because source and destination
        // format are the same. Just copy samples.
        unsigned int srcBytes = SAMPLE_SIZE(destFormat);

        if (stride == 1)
            memcpy(dest, source, len * srcBytes);
        else
        {
            samplePtr s = source;
            samplePtr d = dest;

            for (i = 0; i < len; i++)
            {
                memcpy(d, s, srcBytes);
                s += stride * srcBytes;
                d += srcBytes;
            }
        }

    } else
    if (destFormat == floatSample)
    {
        // No need to dither, just convert samples to float.
        // No clipping should be necessary.
        float* d = (float*)dest;

        if (sourceFormat == int16Sample)
        {
            short* s = (short*)source;
            for (i = 0; i < len; i++, d++, s+= stride)
                *d = FROM_INT16(s);
        } else
        if (sourceFormat == int24Sample)
        {
            int* s = (int*)source;
            for (i = 0; i < len; i++, d++, s+= stride)
                *d = FROM_INT24(s);
        } else {
            wxASSERT(false); // source format unknown
        }
    } else
    if (sourceFormat == int16Sample && destFormat == int24Sample)
    {
        // Special case when promoting 16 bit to 24 bit
        short* s = (short*)source;
        int* d = (int*)dest;
        for (i = 0; i < len; i++)
        {
            *d = ((int)*s) << 8;
            s += stride;
            d++;
        }
    } else
    {
        // We must do dithering
        switch (ditherType)
        {
        case none:
            DITHER(NoDither, dest, destFormat, source, sourceFormat, len, stride);
            break;
        case rectangle:
            DITHER(RectangleDither, dest, destFormat, source, sourceFormat, len, stride);
            break;
        case triangle:
            DITHER(TriangleDither, dest, destFormat, source, sourceFormat, len, stride);
            break;
        case shaped:
            Reset(); // reset shaped dither filter for this new conversion
            DITHER(ShapedDither, dest, destFormat, source, sourceFormat, len, stride);
            break;
        default:
            wxASSERT(false); // unknown dither algorithm
        }
    }
}

// Dither implementations

// No dither, just return sample
inline float Dither::NoDither(float sample)
{
    return sample;
}

// Rectangle dithering, apply one-step noise
inline float Dither::RectangleDither(float sample)
{
    return sample - DITHER_NOISE;
}

// Triangle dither - high pass filtered
inline float Dither::TriangleDither(float sample)
{
    float r = DITHER_NOISE;
    float result = sample + r - mTriangleState;
    mTriangleState = r;

    return result;
}

// Shaped dither
inline float Dither::ShapedDither(float sample)
{
    // Generate triangular dither, +-1 LSB, flat psd
    float r = DITHER_NOISE + DITHER_NOISE;
    if(sample != sample)  // test for NaN
       sample = 0; // and do the best we can with it

    // Run FIR
    float xe = sample + mBuffer[mPhase] * SHAPED_BS[0]
        + mBuffer[(mPhase - 1) & BUF_MASK] * SHAPED_BS[1]
        + mBuffer[(mPhase - 2) & BUF_MASK] * SHAPED_BS[2]
        + mBuffer[(mPhase - 3) & BUF_MASK] * SHAPED_BS[3]
        + mBuffer[(mPhase - 4) & BUF_MASK] * SHAPED_BS[4];

    // Accumulate FIR and triangular noise
    float result = xe + r;

    // Roll buffer and store last error
    mPhase = (mPhase + 1) & BUF_MASK;
    mBuffer[mPhase] = xe - lrintf(result);

    return result;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 5396bd17-2b4a-4917-96da-b2a89e494336

