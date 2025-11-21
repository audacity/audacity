/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveDataCache.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <array>
#include <cstdint>
#include <numeric>
#include <vector>
#include <functional>

#include "GraphicsDataCache.h"
#include "WaveData.h"
#include "Observer.h"

class WaveClip;

//! Helper structure used to transfer the data between the data and graphics layers
struct WAVE_TRACK_PAINT_API WaveCacheSampleBlock final
{
    //! Type of the data of the request
    enum class Type
    {
        //! Each element of the resulting array is a sample
        Samples,
        /*!
         * Each element of the resulting array is a tuple (min, max, rms)
         * calculated over 256 samples.
         */
        MinMaxRMS256,
        /*!
         * Each element of the resulting array is a tuple (min, max, rms)
         * calculated over 256 samples.
         */
        MinMaxRMS64k,
    };

    //! Summary calculated over the requested range
    struct Summary final
    {
        size_t SamplesCount { 0 };

        float Min { std::numeric_limits<float>::infinity() };
        float Max { -std::numeric_limits<float>::infinity() };

        double SquaresSum { 0.0f };

        size_t SumItemsCount { 0 };
    };

    Type DataType { Type::Samples };
    int64_t FirstSample { 0 };
    size_t NumSamples { 0 };

    //! Checks if sample is in the range represented by this block
    bool ContainsSample(int64_t sampleIndex) const noexcept;

    //! Gets a pointer to a data buffer enough to store floatsCount floats
    float* GetWritePointer(size_t floatsCount);

    void Reset() noexcept;
private:
    Summary GetSummary(
        int64_t from, size_t samplesCount, const Summary& initializer) const noexcept;

    std::vector<float> mData;

    friend class WaveDataCache;
};

//! An element of a cache that contains the waveform data
struct WAVE_TRACK_PAINT_API WaveCacheElement final : GraphicsDataCacheElementBase
{
    using Columns
        =std::array<WaveDisplayColumn, GraphicsDataCacheBase::CacheElementWidth>;

    Columns Data;
    size_t AvailableColumns { 0 };

    void Smooth(GraphicsDataCacheElementBase* prevElement) override;
};

//! Cache that contains the waveform data
class WAVE_TRACK_PAINT_API WaveDataCache final : public GraphicsDataCache<WaveCacheElement>
{
public:
    using DataProvider = std::function<bool (int64_t requiredSample, WaveCacheSampleBlock::Type dataType, WaveCacheSampleBlock& block)>;

    WaveDataCache(const WaveClip& waveClip, int channelIndex);

private:
    bool InitializeElement(
        const GraphicsDataCacheKey& key, WaveCacheElement& element) override;

    DataProvider mProvider;

    WaveCacheSampleBlock mCachedBlock;

    const WaveClip& mWaveClip;
    Observer::Subscription mStretchChangedSubscription;
};
