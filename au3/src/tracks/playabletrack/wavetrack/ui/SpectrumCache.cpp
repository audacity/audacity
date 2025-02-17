/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SpectrumCache.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/

#include "SpectrumCache.h"

#include "SpectrogramSettings.h"
#include "RealFFTf.h"
#include "Sequence.h"
#include "Spectrum.h"
#include "WaveClipUIUtilities.h"
#include "WaveTrack.h"
#include "WideSampleSequence.h"
#include <cmath>

namespace {
static void ComputeSpectrumUsingRealFFTf
    (float* __restrict buffer, const FFTParam* hFFT,
    const float* __restrict window, size_t len, float* __restrict out)
{
    size_t i;
    if (len > hFFT->Points * 2) {
        len = hFFT->Points * 2;
    }
    for (i = 0; i < len; i++) {
        buffer[i] *= window[i];
    }
    for (; i < (hFFT->Points * 2); i++) {
        buffer[i] = 0; // zero pad as needed
    }
    RealFFTf(buffer, hFFT);
    // Handle the (real-only) DC
    float power = buffer[0] * buffer[0];
    if (power <= 0) {
        out[0] = -160.0;
    } else {
        out[0] = 10.0 * log10f(power);
    }
    for (i = 1; i < hFFT->Points; i++) {
        const int index = hFFT->BitReversed[i];
        const float re = buffer[index], im = buffer[index + 1];
        power = re * re + im * im;
        if (power <= 0) {
            out[i] = -160.0;
        } else {
            out[i] = 10.0 * log10f(power);
        }
    }
}

void ComputeSpectrogramGainFactors
    (size_t fftLen, double rate, int frequencyGain, std::vector<float>& gainFactors)
{
    if (frequencyGain > 0) {
        // Compute a frequency-dependent gain factor
        // scaled such that 1000 Hz gets a gain of 0dB

        // This is the reciprocal of the bin number of 1000 Hz:
        const double factor = ((double)rate / (double)fftLen) / 1000.0;

        auto half = fftLen / 2;
        gainFactors.reserve(half);
        // Don't take logarithm of zero!  Let bin 0 replicate the gain factor for bin 1.
        gainFactors.push_back(frequencyGain * log10(factor));
        for (decltype(half) x = 1; x < half; x++) {
            gainFactors.push_back(frequencyGain * log10(factor * x));
        }
    }
}
}

bool SpecCache::Matches(
    int dirty_, double samplesPerPixel,
    const SpectrogramSettings& settings) const
{
    // Make a tolerant comparison of the spp values in this wise:
    // accumulated difference of times over the number of pixels is less than
    // a sample period.
    const bool sppMatch = (fabs(samplesPerPixel - spp) * len < 1.0);

    return
        sppMatch
        && dirty == dirty_
        && windowType == settings.windowType
        && windowSize == settings.WindowSize()
        && zeroPaddingFactor == settings.ZeroPaddingFactor()
        && frequencyGain == settings.frequencyGain
        && algorithm == settings.algorithm;
}

bool SpecCache::CalculateOneSpectrum(
    const SpectrogramSettings& settings, const WaveChannelInterval& clip,
    const int xx, double pixelsPerSecond, int lowerBoundX, int upperBoundX,
    const std::vector<float>& gainFactors, float* __restrict scratch,
    float* __restrict out) const
{
    bool result = false;
    const bool reassignment
        =(settings.algorithm == SpectrogramSettings::algReassignment);
    const size_t windowSizeSetting = settings.WindowSize();

    sampleCount from;

    const auto numSamples = clip.GetSequence().GetNumSamples();
    const auto sampleRate = clip.GetRate();
    const auto stretchRatio = clip.GetStretchRatio();
    const auto samplesPerPixel = sampleRate / pixelsPerSecond / stretchRatio;
    // xx may be for a column that is out of the visible bounds, but only
    // when we are calculating reassignment contributions that may cross into
    // the visible area.

    if (xx < 0) {
        from = sampleCount(where[0].as_double() + xx * samplesPerPixel);
    } else if (xx > (int)len) {
        from = sampleCount(where[len].as_double() + (xx - len) * samplesPerPixel);
    } else {
        from = where[xx];
    }

    const bool autocorrelation
        =settings.algorithm == SpectrogramSettings::algPitchEAC;
    const size_t zeroPaddingFactorSetting = settings.ZeroPaddingFactor();
    const size_t padding = (windowSizeSetting * (zeroPaddingFactorSetting - 1)) / 2;
    const size_t fftLen = windowSizeSetting * zeroPaddingFactorSetting;
    auto nBins = settings.NBins();

    if (from < 0 || from >= numSamples) {
        if (xx >= 0 && xx < (int)len) {
            // Pixel column is out of bounds of the clip!  Should not happen.
            float* const results = &out[nBins * xx];
            std::fill(results, results + nBins, 0.0f);
        }
    } else {
        // We can avoid copying memory when ComputeSpectrum is used below
        bool copy = !autocorrelation || (padding > 0) || reassignment;
        std::vector<float> floats;
        float* useBuffer = 0;
        float* adj = scratch + padding;

        {
            auto myLen = windowSizeSetting;
            // Take a window of the track centered at this sample.
            from -= windowSizeSetting >> 1;
            if (from < 0) {
                // Near the start of the clip, pad left with zeroes as needed.
                // from is at least -windowSize / 2
                for (auto ii = from; ii < 0; ++ii) {
                    *adj++ = 0;
                }
                myLen += from.as_long_long(); // add a negative
                from = 0;
                copy = true;
            }

            if (from + myLen >= numSamples) {
                // Near the end of the clip, pad right with zeroes as needed.
                // newlen is bounded by myLen:
                auto newlen = (numSamples - from).as_size_t();
                for (decltype(myLen) ii = newlen; ii < myLen; ++ii) {
                    adj[ii] = 0;
                }
                myLen = newlen;
                copy = true;
            }

            if (myLen > 0) {
                constexpr auto iChannel = 0u;
                constexpr auto mayThrow = false; // Don't throw just for display
                mSampleCacheHolder.emplace(
                    clip.GetSampleView(from, myLen, mayThrow));
                floats.resize(myLen);
                mSampleCacheHolder->Copy(floats.data(), myLen);
                useBuffer = floats.data();
                if (copy) {
                    if (useBuffer) {
                        memcpy(adj, useBuffer, myLen * sizeof(float));
                    } else {
                        memset(adj, 0, myLen * sizeof(float));
                    }
                }
            }
        }

        if (copy || !useBuffer) {
            useBuffer = scratch;
        }

        if (autocorrelation) {
            // not reassignment, xx is surely within bounds.
            wxASSERT(xx >= 0);
            float* const results = &out[nBins * xx];
            // This function does not mutate useBuffer
            ComputeSpectrum(
                useBuffer, windowSizeSetting, windowSizeSetting, results,
                autocorrelation, settings.windowType);
        } else if (reassignment) {
            static const double epsilon = 1e-16;
            const auto hFFT = settings.hFFT.get();

            float* const scratch2 = scratch + fftLen;
            std::copy(scratch, scratch2, scratch2);

            float* const scratch3 = scratch + 2 * fftLen;
            std::copy(scratch, scratch2, scratch3);

            {
                const float* const window = settings.window.get();
                for (size_t ii = 0; ii < fftLen; ++ii) {
                    scratch[ii] *= window[ii];
                }
                RealFFTf(scratch, hFFT);
            }

            {
                const float* const dWindow = settings.dWindow.get();
                for (size_t ii = 0; ii < fftLen; ++ii) {
                    scratch2[ii] *= dWindow[ii];
                }
                RealFFTf(scratch2, hFFT);
            }

            {
                const float* const tWindow = settings.tWindow.get();
                for (size_t ii = 0; ii < fftLen; ++ii) {
                    scratch3[ii] *= tWindow[ii];
                }
                RealFFTf(scratch3, hFFT);
            }

            for (size_t ii = 0; ii < hFFT->Points; ++ii) {
                const int index = hFFT->BitReversed[ii];
                const float
                    denomRe = scratch[index],
                    denomIm = ii == 0 ? 0 : scratch[index + 1];
                const double power = denomRe * denomRe + denomIm * denomIm;
                if (power < epsilon) {
                    // Avoid dividing by near-zero below
                    continue;
                }

                double freqCorrection;
                {
                    const double multiplier = -(fftLen / (2.0f * M_PI));
                    const float
                        numRe = scratch2[index],
                        numIm = ii == 0 ? 0 : scratch2[index + 1];
                    // Find complex quotient --
                    // Which means, multiply numerator by conjugate of denominator,
                    // then divide by norm squared of denominator --
                    // Then just take its imaginary part.
                    const double
                        quotIm = (-numRe * denomIm + numIm * denomRe) / power;
                    // With appropriate multiplier, that becomes the correction of
                    // the frequency bin.
                    freqCorrection = multiplier * quotIm;
                }

                const int bin = (int)((int)ii + freqCorrection + 0.5f);
                // Must check if correction takes bin out of bounds, above or below!
                // bin is signed!
                if (bin >= 0 && bin < (int)hFFT->Points) {
                    double timeCorrection;
                    {
                        const float
                            numRe = scratch3[index],
                            numIm = ii == 0 ? 0 : scratch3[index + 1];
                        // Find another complex quotient --
                        // Then just take its real part.
                        // The result has sample interval as unit.
                        timeCorrection
                            =(numRe * denomRe + numIm * denomIm) / power;
                    }

                    // PRL: timeCorrection is scaled to the clip's raw sample rate,
                    // without the stretching ratio correction for real time. We want
                    // to find the correct X coordinate for that.
                    int correctedX = (floor(
                                          0.5 + xx + timeCorrection * pixelsPerSecond / sampleRate));
                    if (correctedX >= lowerBoundX && correctedX < upperBoundX) {
                        result = true;

                        // This is non-negative, because bin and correctedX are
                        auto ind = (int)nBins * correctedX + bin;
#ifdef _OPENMP
                        // This assignment can race if index reaches into another thread's bins.
                        // The probability of a race very low, so this carries little overhead,
                        // about 5% slower vs allowing it to race.
                  #pragma omp atomic update
#endif
                        out[ind] += power;
                    }
                }
            }
        } else {
            // not reassignment, xx is surely within bounds.
            wxASSERT(xx >= 0);
            float* const results = &out[nBins * xx];

            // Do the FFT.  Note that useBuffer is multiplied by the window,
            // and the window is initialized with leading and trailing zeroes
            // when there is padding.  Therefore we did not need to reinitialize
            // the part of useBuffer in the padding zones.

            // This function mutates useBuffer
            ComputeSpectrumUsingRealFFTf
                (useBuffer, settings.hFFT.get(), settings.window.get(), fftLen, results);
            if (!gainFactors.empty()) {
                // Apply a frequency-dependent gain factor
                for (size_t ii = 0; ii < nBins; ++ii) {
                    results[ii] += gainFactors[ii];
                }
            }
        }
    }

    return result;
}

void SpecCache::Grow(
    size_t len_, SpectrogramSettings& settings, double samplesPerPixel,
    double start_)
{
    settings.CacheWindows();

    // len columns, and so many rows, column-major.
    // Don't take column literally -- this isn't pixel data yet, it's the
    // raw data to be mapped onto the display.
    freq.resize(len_ * settings.NBins());

    // Sample counts corresponding to the columns, and to one past the end.
    where.resize(len_ + 1);

    len = len_;
    algorithm = settings.algorithm;
    spp = samplesPerPixel;
    start = start_;
    windowType = settings.windowType;
    windowSize = settings.WindowSize();
    zeroPaddingFactor = settings.ZeroPaddingFactor();
    frequencyGain = settings.frequencyGain;
}

void SpecCache::Populate(
    const SpectrogramSettings& settings, const WaveChannelInterval& clip,
    int copyBegin, int copyEnd, size_t numPixels, double pixelsPerSecond)
{
    const auto sampleRate = clip.GetRate();
    const int& frequencyGainSetting = settings.frequencyGain;
    const size_t windowSizeSetting = settings.WindowSize();
    const bool autocorrelation
        =settings.algorithm == SpectrogramSettings::algPitchEAC;
    const bool reassignment
        =settings.algorithm == SpectrogramSettings::algReassignment;
    const size_t zeroPaddingFactorSetting = settings.ZeroPaddingFactor();

    // FFT length may be longer than the window of samples that affect results
    // because of zero padding done for increased frequency resolution
    const size_t fftLen = windowSizeSetting * zeroPaddingFactorSetting;
    const auto nBins = settings.NBins();

    const size_t bufferSize = fftLen;
    const size_t scratchSize = reassignment ? 3 * bufferSize : bufferSize;
    std::vector<float> scratch(scratchSize);

    std::vector<float> gainFactors;
    if (!autocorrelation) {
        ComputeSpectrogramGainFactors(
            fftLen, sampleRate, frequencyGainSetting, gainFactors);
    }

    // Loop over the ranges before and after the copied portion and compute anew.
    // One of the ranges may be empty.
    for (int jj = 0; jj < 2; ++jj) {
        const int lowerBoundX = jj == 0 ? 0 : copyEnd;
        const int upperBoundX = jj == 0 ? copyBegin : numPixels;

// todo(mhodgkinson): I don't find an option to define _OPENMP anywhere. Is this
// still of interest?
#ifdef _OPENMP
        // Storage for mutable per-thread data.
        // private clause ensures one copy per thread
        struct ThreadLocalStorage {
            ThreadLocalStorage() { }
            ~ThreadLocalStorage() { }

            void init(SampleTrackCache& waveTrackCache, size_t scratchSize)
            {
                if (!cache) {
                    cache = std::make_unique<SampleTrackCache>(waveTrackCache.GetTrack());
                    scratch.resize(scratchSize);
                }
            }

            std::unique_ptr<SampleTrackCache> cache;
            std::vector<float> scratch;
        } tls;

      #pragma omp parallel for private(tls)
#endif
        for (auto xx = lowerBoundX; xx < upperBoundX; ++xx) {
#ifdef _OPENMP
            tls.init(waveTrackCache, scratchSize);
            SampleTrackCache& cache = *tls.cache;
            float* buffer = &tls.scratch[0];
#else
            float* buffer = &scratch[0];
#endif
            CalculateOneSpectrum(
                settings, clip, xx, pixelsPerSecond, lowerBoundX, upperBoundX,
                gainFactors, buffer, &freq[0]);
        }

        if (reassignment) {
            // Need to look beyond the edges of the range to accumulate more
            // time reassignments.
            // I'm not sure what's a good stopping criterion?
            auto xx = lowerBoundX;
            const double pixelsPerSample
                =pixelsPerSecond * clip.GetStretchRatio() / sampleRate;
            const int limit = std::min((int)(0.5 + fftLen * pixelsPerSample), 100);
            for (int ii = 0; ii < limit; ++ii) {
                const bool result = CalculateOneSpectrum(
                    settings, clip, --xx, pixelsPerSecond, lowerBoundX, upperBoundX,
                    gainFactors, &scratch[0], &freq[0]);
                if (!result) {
                    break;
                }
            }

            xx = upperBoundX;
            for (int ii = 0; ii < limit; ++ii) {
                const bool result = CalculateOneSpectrum(
                    settings, clip, xx++, pixelsPerSecond, lowerBoundX, upperBoundX,
                    gainFactors, &scratch[0], &freq[0]);
                if (!result) {
                    break;
                }
            }

            // Now Convert to dB terms.  Do this only after accumulating
            // power values, which may cross columns with the time correction.
#ifdef _OPENMP
         #pragma omp parallel for
#endif
            for (xx = lowerBoundX; xx < upperBoundX; ++xx) {
                float* const results = &freq[nBins * xx];
                for (size_t ii = 0; ii < nBins; ++ii) {
                    float& power = results[ii];
                    if (power <= 0) {
                        power = -160.0;
                    } else {
                        power = 10.0 * log10f(power);
                    }
                }
                if (!gainFactors.empty()) {
                    // Apply a frequency-dependent gain factor
                    for (size_t ii = 0; ii < nBins; ++ii) {
                        results[ii] += gainFactors[ii];
                    }
                }
            }
        }
    }
}

bool WaveClipSpectrumCache::GetSpectrogram(
    const WaveChannelInterval& clip,
    const float*& spectrogram, SpectrogramSettings& settings,
    const sampleCount*& where, size_t numPixels, double t0,
    double pixelsPerSecond)
{
    auto& mSpecCache = mSpecCaches[clip.GetChannelIndex()];

    const auto sampleRate = clip.GetRate();
    const auto stretchRatio = clip.GetStretchRatio();
    const auto samplesPerPixel = sampleRate / pixelsPerSecond / stretchRatio;

    //Trim offset comparison failure forces spectrogram cache rebuild
    //and skip copying "unchanged" data after clip border was trimmed.
    bool match = mSpecCache && mSpecCache->leftTrim == clip.GetTrimLeft()
                 && mSpecCache->rightTrim == clip.GetTrimRight()
                 && mSpecCache->len > 0
                 && mSpecCache->Matches(mDirty, samplesPerPixel, settings);

    if (match && mSpecCache->start == t0 && mSpecCache->len >= numPixels) {
        spectrogram = &mSpecCache->freq[0];
        where = &mSpecCache->where[0];

        return false; //hit cache completely
    }

    // Caching is not implemented for reassignment, unless for
    // a complete hit, because of the complications of time reassignment
    if (settings.algorithm == SpectrogramSettings::algReassignment) {
        match = false;
    }

    // Free the cache when it won't cause a major stutter.
    // If the window size changed, we know there is nothing to be copied
    // If we zoomed out, or resized, we can give up memory. But not too much -
    // up to 2x extra is needed at the end of the clip to prevent stutter.
    if (mSpecCache->freq.capacity() > 2.1 * mSpecCache->freq.size()
        || mSpecCache->windowSize * mSpecCache->zeroPaddingFactor
        < settings.WindowSize() * settings.ZeroPaddingFactor()) {
        match = false;
        mSpecCache = std::make_unique<SpecCache>();
    }

    int oldX0 = 0;
    double correction = 0.0;

    int copyBegin = 0, copyEnd = 0;
    if (match) {
        WaveClipUIUtilities::findCorrection(
            mSpecCache->where, mSpecCache->len, numPixels, t0, sampleRate,
            stretchRatio, samplesPerPixel, oldX0, correction);
        // Remember our first pixel maps to oldX0 in the old cache,
        // possibly out of bounds.
        // For what range of pixels can data be copied?
        copyBegin = std::min((int)numPixels, std::max(0, -oldX0));
        copyEnd = std::min((int)numPixels, std::max(0,
                                                    (int)mSpecCache->len - oldX0
                                                    ));
    }

    // Resize the cache, keep the contents unchanged.
    mSpecCache->Grow(numPixels, settings, samplesPerPixel, t0);
    mSpecCache->leftTrim = clip.GetTrimLeft();
    mSpecCache->rightTrim = clip.GetTrimRight();
    auto nBins = settings.NBins();

    // Optimization: if the old cache is good and overlaps
    // with the current one, re-use as much of the cache as
    // possible
    if (copyEnd > copyBegin) {
        // memmove is required since dst/src overlap
        memmove(&mSpecCache->freq[nBins * copyBegin],
                &mSpecCache->freq[nBins * (copyBegin + oldX0)],
                nBins * (copyEnd - copyBegin) * sizeof(float));
    }

    // Reassignment accumulates, so it needs a zeroed buffer
    if (settings.algorithm == SpectrogramSettings::algReassignment) {
        // The cache could theoretically copy from the middle, resulting
        // in two regions to update. This won't happen in zoom, since
        // old cache doesn't match. It won't happen in resize, since the
        // spectrum view is pinned to left side of window.
        wxASSERT(
            (copyBegin >= 0 && copyEnd == (int)numPixels) // copied the end
            || (copyBegin == 0 && copyEnd <= (int)numPixels) // copied the beginning
            );

        int zeroBegin = copyBegin > 0 ? 0 : copyEnd - copyBegin;
        int zeroEnd = copyBegin > 0 ? copyBegin : numPixels;

        memset(&mSpecCache->freq[nBins * zeroBegin], 0, nBins * (zeroEnd - zeroBegin) * sizeof(float));
    }

    // purposely offset the display 1/2 sample to the left (as compared
    // to waveform display) to properly center response of the FFT
    constexpr auto addBias = true;
    WaveClipUIUtilities::fillWhere(
        mSpecCache->where, numPixels, addBias, correction, t0, sampleRate,
        stretchRatio, samplesPerPixel);

    mSpecCache->Populate(
        settings, clip, copyBegin, copyEnd, numPixels, pixelsPerSecond);

    mSpecCache->dirty = mDirty;
    spectrogram = &mSpecCache->freq[0];
    where = &mSpecCache->where[0];

    return true;
}

WaveClipSpectrumCache::WaveClipSpectrumCache(size_t nChannels)
    : mSpecCaches(nChannels)
    , mSpecPxCaches(nChannels)
{
    for (auto& pCache : mSpecCaches) {
        pCache = std::make_unique<SpecCache>();
    }
}

WaveClipSpectrumCache::~WaveClipSpectrumCache()
{
}

std::unique_ptr<WaveClipListener> WaveClipSpectrumCache::Clone() const
{
    // Don't need to copy contents
    return std::make_unique<WaveClipSpectrumCache>(mSpecCaches.size());
}

static WaveClip::Attachments::RegisteredFactory sKeyS{ [](WaveClip& clip){
        return std::make_unique<WaveClipSpectrumCache>(clip.NChannels());
    } };

WaveClipSpectrumCache&
WaveClipSpectrumCache::Get(const WaveChannelInterval& clip)
{
    return const_cast<WaveClip&>(clip.GetClip()) // Consider it mutable data
           .Attachments::Get< WaveClipSpectrumCache >(sKeyS);
}

void WaveClipSpectrumCache::MarkChanged() noexcept
{
    ++mDirty;
}

void WaveClipSpectrumCache::Invalidate()
{
    // Invalidate the spectrum display cache
    for (auto& pCache : mSpecCaches) {
        pCache = std::make_unique<SpecCache>();
    }
}

void WaveClipSpectrumCache::MakeStereo(WaveClipListener&& other, bool)
{
    auto pOther = dynamic_cast<WaveClipSpectrumCache*>(&other);
    assert(pOther); // precondition
    mSpecCaches.push_back(move(pOther->mSpecCaches[0]));
    mSpecPxCaches.push_back(move(pOther->mSpecPxCaches[0]));
}

void WaveClipSpectrumCache::SwapChannels()
{
    mSpecCaches.resize(2);
    std::swap(mSpecCaches[0], mSpecCaches[1]);
    mSpecPxCaches.resize(2);
    std::swap(mSpecPxCaches[0], mSpecPxCaches[1]);
}

void WaveClipSpectrumCache::Erase(size_t index)
{
    if (index < mSpecCaches.size()) {
        mSpecCaches.erase(mSpecCaches.begin() + index);
    }
    if (index < mSpecPxCaches.size()) {
        mSpecPxCaches.erase(mSpecPxCaches.begin() + index);
    }
}
