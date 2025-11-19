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
//! The ordering of these values with operator < agrees with the order of increasing bit width
/*! These values persist in saved project files, so must not be changed in later program versions */
enum class sampleFormat : unsigned {
    undefinedSample = 0,
    int16Sample = 0x00020001,
    int24Sample = 0x00040001,
    floatSample = 0x0004000F,

    //! Two synonyms for previous values that might change if more values were added
    narrowestSampleFormat = int16Sample,
    widestSampleFormat = floatSample,
};

// C++20 using enum sampleFormat;
constexpr sampleFormat undefinedSample = sampleFormat::undefinedSample;
constexpr sampleFormat int16Sample = sampleFormat::int16Sample;
constexpr sampleFormat int24Sample = sampleFormat::int24Sample;
constexpr sampleFormat floatSample = sampleFormat::floatSample;
constexpr sampleFormat narrowestSampleFormat = sampleFormat::narrowestSampleFormat;
constexpr sampleFormat widestSampleFormat = sampleFormat::widestSampleFormat;

// ----------------------------------------------------------------------------
// Provide the number of bytes a specific sample will take
// ----------------------------------------------------------------------------
#define SAMPLE_SIZE(SampleFormat) (static_cast<unsigned>(SampleFormat) >> 16)

// ----------------------------------------------------------------------------
// Generic pointer to sample data
// ----------------------------------------------------------------------------
using samplePtr = char*;
using constSamplePtr = const char*;

// Used to determine how to fill in empty areas of audio.
typedef enum class FillFormat {
    fillZero = 0,
    fillTwo = 2
} fillFormat;

/** \brief Return the size on disk of one uncompressed sample (bytes) */
#define SAMPLE_SIZE_DISK(SampleFormat) (((SampleFormat) == int24Sample) ? \
                                        size_t { 3 } : SAMPLE_SIZE(SampleFormat))

class TranslatableString;
MATH_API TranslatableString GetSampleFormatStr(sampleFormat format);

//! Two sample formats, remembering format of original source and describing stored format
/*! Useful when imported data are stored temporarily in a wider format but should be exported bit-perfect
 without dither if to the original format again

 @invariant `Effective() <= Stored()`
 */
class SampleFormats final
{
public:
    /*! Construct sampleFormats, but may change effective to satisfy the invariant */
    SampleFormats(
        sampleFormat effective, //!< How much real information in each sample
        sampleFormat stored   //!< The form used for storage
        )
        : m_Effective{std::min(effective, stored)}
        , m_Stored{stored}
    {}

    sampleFormat Effective() const { return m_Effective; }
    sampleFormat Stored() const { return m_Stored; }

    //! Update the effective format, for insertion of more samples into the sequence
    /*! `GetEffective()` will not necessarily equal the given value, because the invariant will be preserved,
     and also `GetEffective()` will never become narrower than before:  if any material in the sequence had
     a wider format, assume that the whole sequence still requires dithering to lesser formats than that.
     */
    void UpdateEffective(sampleFormat effective)
    {
        if (effective > m_Effective) {
            m_Effective = std::min(effective, m_Stored);
        }
    }

private:
    sampleFormat m_Effective;
    sampleFormat m_Stored;
};

inline bool operator ==(SampleFormats a, SampleFormats b)
{
    return a.Effective() == b.Effective()
           && a.Stored() == b.Stored();
}

inline bool operator !=(SampleFormats a, SampleFormats b)
{
    return !(a == b);
}

//
// Allocating/Freeing Samples
//

class SampleBuffer
{
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

    SampleBuffer(SampleBuffer&& other)
    {
        mPtr = other.mPtr;
        other.mPtr = nullptr;
    }

    SampleBuffer& operator=(SampleBuffer&& other)
    {
        auto ptr = other.mPtr;
        other.mPtr = nullptr;
        mPtr = ptr;
        return *this;
    }

    // WARNING!  May not preserve contents.
    SampleBuffer& Allocate(size_t count, sampleFormat format)
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

    GrowableSampleBuffer& Resize(size_t count, sampleFormat format)
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
void SamplesToFloats(constSamplePtr src, sampleFormat srcFormat, float* dst, size_t len, size_t srcStride = 1, size_t dstStride = 1);

MATH_API
//! Copy samples from any format to any other format; apply dithering only if narrowing the format
/*!
 @copydetails SamplesToFloats()
 @param dstFormat format of destination samples, determines sizeof each one
 @param ditherType choice of dithering algorithm to use if narrowing the format
 */
void CopySamples(constSamplePtr src, sampleFormat srcFormat, samplePtr dst, sampleFormat dstFormat, size_t len,
                 DitherType ditherType = gHighQualityDither, //!< default is loaded from a global variable
                 unsigned int srcStride=1, unsigned int dstStride=1);

MATH_API
void      ClearSamples(samplePtr buffer, sampleFormat format, size_t start, size_t len);

MATH_API
void      ReverseSamples(samplePtr buffer, sampleFormat format, int start, int len);

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
