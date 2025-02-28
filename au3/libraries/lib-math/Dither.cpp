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

#include "Dither.h"

#include "Internat.h"
#include "Prefs.h"

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

//////////////////////////////////////////////////////////////////////////

// Constants for the noise shaping buffer
constexpr int BUF_SIZE = 8;
constexpr int BUF_MASK = 7;

// Lipshitz's minimally audible FIR
const float SHAPED_BS[] = { 2.033f, -2.165f, 1.959f, -1.590f, 0.6149f };

// Dither state
struct State {
    int mPhase;
    float mTriangleState;
    float mBuffer[8 /* = BUF_SIZE */];
} mState;

using Ditherer = float (*)(State&, float);

// This is supposed to produce white noise and no dc
static inline float DITHER_NOISE()
{
    return rand() / (float)RAND_MAX - 0.5f;
}

// Defines for sample conversion
constexpr auto CONVERT_DIV16 = float(1 << 15);
constexpr auto CONVERT_DIV24 = float(1 << 23);

// Dereference sample pointer and convert to float sample
static inline float FROM_INT16(const short* ptr)
{
    return *ptr / CONVERT_DIV16;
}

static inline float FROM_INT24(const int* ptr)
{
    return *ptr / CONVERT_DIV24;
}

// For float, we internally allow values greater than 1.0, which
// would blow up the dithering to int values.  FROM_FLOAT is
// only used to dither to int, so clip here.
static inline float FROM_FLOAT(const float* ptr)
{
    return *((float*)(ptr)) > 1.0
           ? 1.0
           : *((float*)(ptr)) < -1.0
           ? -1.0
           : *((float*)(ptr));
}

// Store float sample 'sample' into pointer 'ptr', clip it, if necessary
template<typename dst_type>
static inline void IMPLEMENT_STORE(
    dst_type* ptr, float sample, dst_type min_bound, dst_type max_bound)
{
    int x = lrintf(sample);
    if (x > max_bound) {
        *ptr = max_bound;
    } else if (x < (min_bound)) {
        *ptr = min_bound;
    } else {
        *ptr = static_cast<dst_type>(x);
    }
}

// Dither single float 'sample' and store it in pointer 'dst', using 'dither' as algorithm
static inline void DITHER_TO_INT16(Ditherer dither, State& state,
                                   short* dst, float sample)
{
    IMPLEMENT_STORE<short>(dst,
                           dither(state, sample * CONVERT_DIV16),
                           short(-32768), short(32767));
}

// Dither single float 'sample' and store it in pointer 'dst', using 'dither' as algorithm
static inline void DITHER_TO_INT24(Ditherer dither, State& state,
                                   int* dst, float sample)
{
    IMPLEMENT_STORE<int>(dst,
                         dither(state, sample * CONVERT_DIV24), -8388608, 8388607);
}

// Implement one single dither step

// Implement a dithering loop
template<typename srcType, typename dstType>
static inline void DITHER_LOOP(Ditherer dither, State& state,
                               void (*store)(Ditherer, State&, dstType*, float),
                               float (*load)(const srcType*),
                               samplePtr dst, sampleFormat dstFormat, size_t dstStride,
                               constSamplePtr src, sampleFormat srcFormat, size_t srcStride, size_t len)
{
    char* d;
    const char* s;
    unsigned int ii;
    for (d = (char*)dst, s = (char*)src, ii = 0;
         ii < len;
         ii++, d += SAMPLE_SIZE(dstFormat) * dstStride,
         s += SAMPLE_SIZE(srcFormat) * srcStride) {
        store(dither, state, reinterpret_cast<dstType*>(d), load(reinterpret_cast<const srcType*>(s)));
    }
}

// Implement a dither. There are only 3 cases where we must dither,
// in all other cases, no dithering is necessary.
static inline void DITHER(Ditherer dither, State& state,
                          samplePtr dst, sampleFormat dstFormat, size_t dstStride,
                          constSamplePtr src, sampleFormat srcFormat, size_t srcStride, size_t len)
{
    if (srcFormat == int24Sample && dstFormat == int16Sample) {
        DITHER_LOOP<int, short>(dither, state,
                                DITHER_TO_INT16, FROM_INT24, dst,
                                int16Sample, dstStride, src, int24Sample, srcStride, len);
    } else if (srcFormat == floatSample && dstFormat == int16Sample) {
        DITHER_LOOP<float, short>(dither, state,
                                  DITHER_TO_INT16, FROM_FLOAT, dst,
                                  int16Sample, dstStride, src, floatSample, srcStride, len);
    } else if (srcFormat == floatSample && dstFormat == int24Sample) {
        DITHER_LOOP<float, int>(dither, state,
                                DITHER_TO_INT24, FROM_FLOAT, dst,
                                int24Sample, dstStride, src, floatSample, srcStride, len);
    } else {
        wxASSERT(false);
    }
}

static inline float NoDither(State&, float sample);
static inline float RectangleDither(State&, float sample);
static inline float TriangleDither(State& state, float sample);
static inline float ShapedDither(State& state, float sample);

Dither::Dither()
{
    // On startup, initialize dither by resetting values
    Reset();
}

void Dither::Reset()
{
    mState.mTriangleState = 0;
    mState.mPhase = 0;
    memset(mState.mBuffer, 0, sizeof(float) * BUF_SIZE);
}

// This only decides if we must dither at all, the dithers
// are all implemented using macros.
//
// "source" and "dest" can contain either interleaved or non-interleaved
// samples.  They do not have to be the same...one can be interleaved while
// the otner non-interleaved.
//
// The "len" argument specifies the number of samples to process.
//
// If either stride value equals 1 then the corresponding buffer contains
// non-interleaved samples.
//
// If either stride value is greater than 1 then the corresponding buffer
// contains interleaved samples and they will be processed by skipping every
// stride number of samples.

void Dither::Apply(enum DitherType ditherType,
                   constSamplePtr source, sampleFormat sourceFormat,
                   samplePtr dest, sampleFormat destFormat,
                   unsigned int len,
                   unsigned int sourceStride /* = 1 */,
                   unsigned int destStride /* = 1 */)
{
    unsigned int i;

    // This code is not designed for 16-bit or 64-bit machine
    wxASSERT(sizeof(int) == 4);
    wxASSERT(sizeof(short) == 2);

    // Check parameters
    wxASSERT(source);
    wxASSERT(dest);
    wxASSERT(len >= 0);
    wxASSERT(sourceStride > 0);
    wxASSERT(destStride > 0);

    if (len == 0) {
        return; // nothing to do
    }
    if (destFormat == sourceFormat) {
        // No need to dither, because source and destination
        // format are the same. Just copy samples.
        if (destStride == 1 && sourceStride == 1) {
            memcpy(dest, source, len * SAMPLE_SIZE(destFormat));
        } else {
            if (sourceFormat == floatSample) {
                auto d = (float*)dest;
                auto s = (const float*)source;

                for (i = 0; i < len; i++, d += destStride, s += sourceStride) {
                    *d = *s;
                }
            } else if (sourceFormat == int24Sample) {
                auto d = (int*)dest;
                auto s = (const int*)source;

                for (i = 0; i < len; i++, d += destStride, s += sourceStride) {
                    *d = *s;
                }
            } else if (sourceFormat == int16Sample) {
                auto d = (short*)dest;
                auto s = (const short*)source;

                for (i = 0; i < len; i++, d += destStride, s += sourceStride) {
                    *d = *s;
                }
            } else {
                wxASSERT(false); // source format unknown
            }
        }
    } else if (destFormat == floatSample) {
        // No need to dither, just convert samples to float.
        // No clipping should be necessary.
        auto d = (float*)dest;

        if (sourceFormat == int16Sample) {
            auto s = (const short*)source;
            for (i = 0; i < len; i++, d += destStride, s += sourceStride) {
                *d = FROM_INT16(s);
            }
        } else if (sourceFormat == int24Sample) {
            auto s = (const int*)source;
            for (i = 0; i < len; i++, d += destStride, s += sourceStride) {
                *d = FROM_INT24(s);
            }
        } else {
            wxASSERT(false); // source format unknown
        }
    } else if (destFormat == int24Sample && sourceFormat == int16Sample) {
        // Special case when promoting 16 bit to 24 bit
        auto d = (int*)dest;
        auto s = (const short*)source;
        for (i = 0; i < len; i++, d += destStride, s += sourceStride) {
            *d = ((int)*s) << 8;
        }
    } else {
        // We must do dithering
        switch (ditherType) {
        case DitherType::none:
            DITHER(NoDither, mState, dest, destFormat, destStride, source, sourceFormat, sourceStride, len);
            break;
        case DitherType::rectangle:
            DITHER(RectangleDither, mState, dest, destFormat, destStride, source, sourceFormat, sourceStride, len);
            break;
        case DitherType::triangle:
            Reset(); // reset dither filter for this NEW conversion
            DITHER(TriangleDither, mState, dest, destFormat, destStride, source, sourceFormat, sourceStride, len);
            break;
        case DitherType::shaped:
            Reset(); // reset dither filter for this NEW conversion
            DITHER(ShapedDither, mState, dest, destFormat, destStride, source, sourceFormat, sourceStride, len);
            break;
        default:
            wxASSERT(false); // unknown dither algorithm
        }
    }
}

// Dither implementations

// No dither, just return sample
inline float NoDither(State&, float sample)
{
    return sample;
}

// Rectangle dithering, apply one-step noise
inline float RectangleDither(State&, float sample)
{
    return sample - DITHER_NOISE();
}

// Triangle dither - high pass filtered
inline float TriangleDither(State& state, float sample)
{
    float r = DITHER_NOISE();
    float result = sample + r - state.mTriangleState;
    state.mTriangleState = r;

    return result;
}

// Shaped dither
inline float ShapedDither(State& state, float sample)
{
    // Generate triangular dither, +-1 LSB, flat psd
    float r = DITHER_NOISE() + DITHER_NOISE();
    if (sample != sample) { // test for NaN
        sample = 0; // and do the best we can with it
    }
    // Run FIR
    float xe = sample + state.mBuffer[state.mPhase] * SHAPED_BS[0]
               + state.mBuffer[(state.mPhase - 1) & BUF_MASK] * SHAPED_BS[1]
               + state.mBuffer[(state.mPhase - 2) & BUF_MASK] * SHAPED_BS[2]
               + state.mBuffer[(state.mPhase - 3) & BUF_MASK] * SHAPED_BS[3]
               + state.mBuffer[(state.mPhase - 4) & BUF_MASK] * SHAPED_BS[4];

    // Accumulate FIR and triangular noise
    float result = xe + r;

    // Roll buffer and store last error
    state.mPhase = (state.mPhase + 1) & BUF_MASK;
    state.mBuffer[state.mPhase] = xe - lrintf(result);

    return result;
}

static const std::initializer_list<EnumValueSymbol> choicesDither{
    { XO("None") },
    { XO("Rectangle") },
    { XC("Triangle", "dither") },
    { XO("Shaped") },
};
static auto intChoicesDither = {
    DitherType::none,
    DitherType::rectangle,
    DitherType::triangle,
    DitherType::shaped,
};

EnumSetting< DitherType > Dither::FastSetting{
    wxT("Quality/DitherAlgorithmChoice"),
    choicesDither,
    0, // none

    // for migrating old preferences:
    intChoicesDither,
    wxT("Quality/DitherAlgorithm")
};

EnumSetting< DitherType > Dither::BestSetting{
    wxT("Quality/HQDitherAlgorithmChoice"),
    choicesDither,
    3, // shaped

    // for migrating old preferences:
    intChoicesDither,
    wxT("Quality/HQDitherAlgorithm")
};

DitherType Dither::FastDitherChoice()
{
    return (DitherType)FastSetting.ReadEnum();
}

DitherType Dither::BestDitherChoice()
{
    return (DitherType)BestSetting.ReadEnum();
}
