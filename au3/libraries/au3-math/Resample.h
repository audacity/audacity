/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   Resample.cpp
   Dominic Mazzoni, Rob Sykes, Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_RESAMPLE_H__
#define __AUDACITY_RESAMPLE_H__

#include "SampleFormat.h"

template< typename Enum > class EnumSetting;

struct soxr;
extern "C" void soxr_delete(soxr*);
struct soxr_deleter {
    void operator ()(soxr* p) const
    {
        if (p) {
            soxr_delete(p);
        }
    }
};
using soxrHandle = std::unique_ptr<soxr, soxr_deleter>;

class MATH_API Resample final
{
public:
    /// Resamplers may have more than one method, offering a
    /// tradeoff between speed and quality.
    /// Audacity identifies two methods out of all of the choices:
    /// a Fast method intended for real-time audio I/O, and a Best
    /// method intended for mixing and exporting.
    //
    /// The first parameter lets you select either the best method or
    /// the fast method.
    // dMinFactor and dMaxFactor specify the range of factors for variable-rate resampling.
    // For constant-rate, pass the same value for both.
    Resample(const bool useBestMethod, const double dMinFactor, const double dMaxFactor);
    ~Resample();

    Resample(Resample&&) noexcept = default;
    Resample& operator=(Resample&&) noexcept = default;

    Resample(const Resample&) = delete;
    Resample& operator=(const Resample&) = delete;

    static EnumSetting< int > FastMethodSetting;
    static EnumSetting< int > BestMethodSetting;

    /** @brief Main processing function. Resamples from the input buffer to the
     * output buffer.
     *
     * Reads samples from the input buffer, and writes samples to the output
     * buffer. Stops when either is exhausted, or we reach a convenient block
     * end, unless lastFlag is set to force emptying the input buffer.
     * The number of input samples used is returned in inBufferUsed, and the
     * number of output samples generated is the return value of the function.
     * This function may do nothing if you don't pass a large enough output
     * buffer (i.e. there is no where to put a full block of output data)
     @param factor The scaling factor to resample by.
     @param inBuffer Buffer of input samples to be processed (mono)
     @param inBufferLen Length of the input buffer, in samples.
     @param lastFlag Flag to indicate this is the last lot of input samples and
     the buffer needs to be emptied out into the rate converter.
     (unless lastFlag is true, we don't guarantee to process all the samples in
     the input this time, we may leave some for next time)
     @param outBuffer Buffer to write output (converted) samples to.
     @param outBufferLen How big outBuffer is.
     @return Number of input samples consumed, and number of output samples
     created by this call
    */
    std::pair<size_t, size_t>
    Process(double factor, const float* inBuffer, size_t inBufferLen, bool lastFlag, float* outBuffer, size_t outBufferLen);

protected:
    void SetMethod(const bool useBestMethod);

protected:
    int mMethod;  // resampler-specific enum for resampling method
    soxrHandle mHandle; // constant-rate or variable-rate resampler (XOR per instance)
    bool mbWantConstRateResampling;
};

#endif // __AUDACITY_RESAMPLE_H__
