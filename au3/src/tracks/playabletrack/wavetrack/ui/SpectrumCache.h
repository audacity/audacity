/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SpectrumCache.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVECLIP_SPECTRUM_CACHE__
#define __AUDACITY_WAVECLIP_SPECTRUM_CACHE__

class sampleCount;
class SpectrogramSettings;
class WaveClipChannel;
using WaveChannelInterval = WaveClipChannel;
class WideSampleSequence;

#include <vector>
#include "MemoryX.h"
#include "WaveClip.h" // to inherit WaveClipListener

using Floats = ArrayOf<float>;

class AUDACITY_DLL_API SpecCache
{
public:

    // Make invalid cache
    SpecCache()
        : algorithm(-1)
        , spp(-1.0)
        , start(-1.0)
        , windowType(-1)
        , frequencyGain(-1)
        , dirty(-1)
    {
    }

    ~SpecCache()
    {
    }

    bool Matches(
        int dirty_, double samplesPerPixel, const SpectrogramSettings& settings) const;

    // Grow the cache while preserving the (possibly now invalid!) contents
    void Grow(
        size_t len_, SpectrogramSettings& settings, double samplesPerPixel, double start /*relative to clip play start time*/);

    // Calculate the dirty columns at the begin and end of the cache
    void Populate(
        const SpectrogramSettings& settings, const WaveChannelInterval& clip, int copyBegin, int copyEnd, size_t numPixels,
        double pixelsPerSecond);

    size_t len { 0 };      // counts pixels, not samples
    int algorithm;
    double spp;      // samples per pixel
    double leftTrim{ .0 };
    double rightTrim{ .0 };
    double start;      // relative to clip start
    int windowType;
    size_t windowSize { 0 };
    unsigned zeroPaddingFactor { 0 };
    int frequencyGain;
    std::vector<float> freq;
    std::vector<sampleCount> where;

    int dirty;

private:
    // Calculate one column of the spectrum
    bool CalculateOneSpectrum(
        const SpectrogramSettings& settings, const WaveChannelInterval& clip, const int xx, double pixelsPerSecond, int lowerBoundX,
        int upperBoundX, const std::vector<float>& gainFactors, float* __restrict scratch, float* __restrict out) const;

    mutable std::optional<AudioSegmentSampleView> mSampleCacheHolder;
};

class SpecPxCache
{
public:
    SpecPxCache(size_t cacheLen)
        : len{cacheLen}
        , values{len}
    {
        scaleType = 0;
        range = gain = -1;
        minFreq = maxFreq = -1;
    }

    size_t len;
    Floats values;

    int scaleType;
    int range;
    int gain;
    int minFreq;
    int maxFreq;
};

struct WaveClipSpectrumCache final : WaveClipListener
{
    explicit WaveClipSpectrumCache(size_t nChannels);
    ~WaveClipSpectrumCache() override;

    std::unique_ptr<WaveClipListener> Clone() const override;

    // Cache of values to colour pixels of Spectrogram - used by TrackArtist
    std::vector<std::unique_ptr<SpecPxCache> > mSpecPxCaches;
    std::vector<std::unique_ptr<SpecCache> > mSpecCaches;
    int mDirty { 0 };

    static WaveClipSpectrumCache& Get(const WaveChannelInterval& clip);

    void MarkChanged() noexcept override; // NOFAIL-GUARANTEE
    void Invalidate() override; // NOFAIL-GUARANTEE

    /** Getting high-level data for screen display */
    // PRL:
    // > only the 0th channel of sequence is really used
    // > In the interim, this still works correctly for WideSampleSequence backed
    // > by a right channel track, which always ignores its partner.
    bool GetSpectrogram(const WaveChannelInterval& clip, const float*& spectrogram, SpectrogramSettings& spectrogramSettings,
                        const sampleCount*& where, size_t numPixels, double t0 /*absolute time*/, double pixelsPerSecond);

    void MakeStereo(WaveClipListener&& other, bool aligned) override;
    void SwapChannels() override;
    void Erase(size_t index) override;
};

#endif
