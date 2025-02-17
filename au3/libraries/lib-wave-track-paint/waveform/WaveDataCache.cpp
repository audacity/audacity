/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveDataCache.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WaveDataCache.h"
#include "FrameStatistics.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstring>

#include "SampleBlock.h"
#include "SampleFormat.h"
#include "Sequence.h"
#include "WaveClip.h"

#include "RoundUpUnsafe.h"

namespace {
//
// Getting high-level data from the track for screen display and
// clipping calculations
//
class AppendBufferHelper final
{
public:
    bool FillBuffer(
        const WaveClip& clip, WaveCacheSampleBlock& outBlock,
        int channelIndex) noexcept
    {
        if (
            mFirstClipSampleID != clip.GetSequence(0)->GetNumSamples()
            || mSampleType != outBlock.DataType) {
            mFirstClipSampleID   = clip.GetSequence(0)->GetNumSamples();
            mLastProcessedSample = 0;
            mSampleType          = outBlock.DataType;

            if (mSampleType != WaveCacheSampleBlock::Type::Samples) {
                mCachedData.clear();
                mCachedData.resize(
                    RoundUpUnsafe(clip.GetSequence(0)->GetMaxBlockSize(), 256));
            }
        }

        const size_t appendedSamples = clip.GetAppendBufferLen(channelIndex);

        if (appendedSamples == 0) {
            return false;
        }

        const auto appendBuffer = GetAppendBufferPointer(clip, channelIndex);

        switch (mSampleType) {
        case WaveCacheSampleBlock::Type::Samples:
        {
            float* outBuffer = outBlock.GetWritePointer(appendedSamples);

            std::copy(appendBuffer, appendBuffer + appendedSamples, outBuffer);
        }
        break;
        case WaveCacheSampleBlock::Type::MinMaxRMS256:
            FillBlocksFromAppendBuffer<256>(
                appendBuffer, appendedSamples, outBlock);
            break;
        case WaveCacheSampleBlock::Type::MinMaxRMS64k:
            FillBlocksFromAppendBuffer<64 * 1024>(
                appendBuffer, appendedSamples, outBlock);
            break;
        default:
            return false;
        }

        // If the append buffer was flushed during the copy operation,
        // then outBlock content is no longer valid
        if (mFirstClipSampleID != clip.GetSequence(0)->GetNumSamples()
            || appendedSamples > clip.GetAppendBufferLen(channelIndex)) {
            return false;
        }

        return true;
    }

private:
    const float* GetAppendBufferPointer(const WaveClip& clip, int channelIndex)
    {
        const auto currentFormat = clip.GetSampleFormats().Stored();
        const auto appendBuffer
            =static_cast<const void*>(clip.GetAppendBuffer(channelIndex));

        if (currentFormat == floatSample) {
            return static_cast<const float*>(appendBuffer);
        }

        const auto samplesCount = clip.GetAppendBufferLen(channelIndex);
        mConvertedAppendBufferData.resize(samplesCount);

        if (currentFormat == int16Sample) {
            const auto int16Buffer = static_cast<const int16_t*>(appendBuffer);

            for (size_t i = 0; i < samplesCount; ++i) {
                mConvertedAppendBufferData[i] = float(int16Buffer[i]) / (1 << 15);
            }
        } else if (currentFormat == sampleFormat::int24Sample) {
            const auto int24Buffer = static_cast<const int32_t*>(appendBuffer);

            for (size_t i = 0; i < samplesCount; ++i) {
                mConvertedAppendBufferData[i] = float(int24Buffer[i]) / (1 << 23);
            }
        }

        return mConvertedAppendBufferData.data();
    }

    template<size_t blockSize>
    void FillBlocksFromAppendBuffer(
        const float* bufferSamples, size_t samplesCount,
        WaveCacheSampleBlock& outBlock)
    {
        const size_t startingBlock = mLastProcessedSample / blockSize;
        const size_t blocksCount   = RoundUpUnsafe(samplesCount, blockSize);

        for (size_t blockIndex = startingBlock; blockIndex < blocksCount;
             ++blockIndex) {
            const size_t samplesInBlock
                =std::min(blockSize, samplesCount - blockIndex * blockSize);

            float min = std::numeric_limits<float>::infinity();
            float max = -std::numeric_limits<float>::infinity();

            double sqSum = 0.0;

            auto samples = bufferSamples + blockIndex * blockSize;

            for (size_t sampleIndex = 0; sampleIndex < samplesInBlock;
                 ++sampleIndex) {
                const float sample = *samples++;

                min = std::min(min, sample);
                max = std::max(max, sample);

                const double dbl = sample;

                sqSum += dbl * dbl;
            }

            const auto rms = static_cast<float>(std::sqrt(sqSum / samplesInBlock));

            mCachedData[blockIndex] = { min, max, rms };
        }

        mLastProcessedSample = samplesCount;
        auto ptr             = outBlock.GetWritePointer(
            sizeof(CacheItem) * blocksCount / sizeof(float));

        std::memmove(ptr, mCachedData.data(), sizeof(CacheItem) * blocksCount);
    }

    WaveCacheSampleBlock::Type mSampleType {
        WaveCacheSampleBlock::Type::Samples
    };
    sampleCount mFirstClipSampleID { 0 };

    struct CacheItem final
    {
        float min;
        float max;
        float rms;
    };

    std::vector<CacheItem> mCachedData;
    std::vector<float> mConvertedAppendBufferData;
    size_t mLastProcessedSample { 0 };
};

WaveDataCache::DataProvider
MakeDefaultDataProvider(const WaveClip& clip, int channelIndex)
{
    return [sequence = clip.GetSequence(channelIndex), clip = &clip,
            channelIndex, appendBufferHelper = AppendBufferHelper()](
        int64_t requiredSample, WaveCacheSampleBlock::Type dataType,
        WaveCacheSampleBlock& outBlock) mutable
    {
        if (requiredSample < 0) {
            return false;
        }

        auto sequenceSampleCount = sequence->GetNumSamples();
        if (requiredSample >= sequenceSampleCount) {
            auto appendBufferOffset = requiredSample - sequence->GetNumSamples().as_long_long();

            if (appendBufferOffset >= clip->GetAppendBufferLen(channelIndex)) {
                return false;
            }

            outBlock.DataType    = dataType;
            outBlock.FirstSample = sequenceSampleCount.as_long_long();
            outBlock.NumSamples  = clip->GetAppendBufferLen(channelIndex);

            bool success = appendBufferHelper.FillBuffer(*clip, outBlock, channelIndex);

            // While we were filling outBlock from the append buffer
            // it wasn't flushed to sequence, we are good to go
            if (sequenceSampleCount == sequence->GetNumSamples()) {
                return success;
            }

            if (requiredSample >= sequence->GetNumSamples()) {
                return false;
            }
            // if the data was moved to sequence,
            // then continue with the rest of the function
        }

        const auto blockIndex  = sequence->FindBlock(requiredSample);
        const auto& inputBlock = sequence->GetBlockArray()[blockIndex];

        outBlock.FirstSample = inputBlock.start.as_long_long();
        outBlock.NumSamples  = inputBlock.sb->GetSampleCount();

        switch (dataType) {
        case WaveCacheSampleBlock::Type::Samples:
        {
            samplePtr ptr = static_cast<samplePtr>(
                static_cast<void*>(outBlock.GetWritePointer(outBlock.NumSamples)));

            inputBlock.sb->GetSamples(
                ptr, floatSample, 0, outBlock.NumSamples, false);
        }
        break;
        case WaveCacheSampleBlock::Type::MinMaxRMS256:
        {
            size_t framesCount = RoundUpUnsafe(outBlock.NumSamples, 256);

            float* ptr
                =static_cast<float*>(outBlock.GetWritePointer(framesCount * 3));

            inputBlock.sb->GetSummary256(ptr, 0, framesCount);
        }
        break;
        case WaveCacheSampleBlock::Type::MinMaxRMS64k:
        {
            size_t framesCount = RoundUpUnsafe(outBlock.NumSamples, 64 * 1024);

            float* ptr
                =static_cast<float*>(outBlock.GetWritePointer(framesCount * 3));

            inputBlock.sb->GetSummary64k(ptr, 0, framesCount);
        }
        break;
        default:
            return false;
        }

        outBlock.DataType = dataType;

        return true;
    };
}
} // namespace

WaveDataCache::WaveDataCache(const WaveClip& waveClip, int channelIndex)
    : GraphicsDataCache<WaveCacheElement>(
        waveClip.GetRate() / waveClip.GetStretchRatio(),
        [] { return std::make_unique<WaveCacheElement>(); }),
    mProvider { MakeDefaultDataProvider(waveClip, channelIndex) },
mWaveClip { waveClip },
mStretchChangedSubscription {
    const_cast<WaveClip&>(waveClip)
    .Observer::Publisher<StretchRatioChange>::Subscribe(
        [this](const StretchRatioChange&) {
        SetScaledSampleRate(
            mWaveClip.GetRate() / mWaveClip.GetStretchRatio());
    })
}
{
}

bool WaveDataCache::InitializeElement(
    const GraphicsDataCacheKey& key, WaveCacheElement& element)
{
    auto sw = FrameStatistics::CreateStopwatch(
        FrameStatistics::SectionID::WaveDataCache);

    element.AvailableColumns = 0;

    int64_t firstSample = key.FirstSample;

    const auto samplesPerColumn
        =std::max(0.0, GetScaledSampleRate() / key.PixelsPerSecond);

    const size_t elementSamplesCount
        =samplesPerColumn * WaveDataCache::CacheElementWidth;
    size_t processedSamples = 0;

    const WaveCacheSampleBlock::Type blockType
        =samplesPerColumn >= 64 * 1024
          ? WaveCacheSampleBlock::Type::MinMaxRMS64k
          : (samplesPerColumn >= 256 ? WaveCacheSampleBlock::Type::MinMaxRMS256
             : WaveCacheSampleBlock::Type::Samples);

    if (blockType != mCachedBlock.DataType) {
        mCachedBlock.Reset();
    }

    size_t columnIndex = 0;

    for (; columnIndex < WaveDataCache::CacheElementWidth; ++columnIndex) {
        WaveCacheSampleBlock::Summary summary;

        auto samplesLeft
            =static_cast<size_t>(std::round(samplesPerColumn * (columnIndex + 1)) - std::round(samplesPerColumn * columnIndex));

        while (samplesLeft != 0)
        {
            if (!mCachedBlock.ContainsSample(firstSample)) {
                if (!mProvider(firstSample, blockType, mCachedBlock)) {
                    break;
                }
            }

            summary = mCachedBlock.GetSummary(firstSample, samplesLeft, summary);
            if (summary.SamplesCount == 0) {
                break;
            }

            samplesLeft -= summary.SamplesCount;
            firstSample += summary.SamplesCount;
            processedSamples += summary.SamplesCount;
        }

        if (summary.SamplesCount > 0) {
            auto& column = element.Data[columnIndex];

            column.min = summary.Min;
            column.max = summary.Max;

            column.rms = std::sqrt(summary.SquaresSum / summary.SumItemsCount);
        }

        if (columnIndex > 0) {
            const auto prevColumn = element.Data[columnIndex - 1];
            auto& column = element.Data[columnIndex];

            bool updated = false;

            if (prevColumn.min > column.max) {
                column.max = prevColumn.min;
                updated    = true;
            }

            if (prevColumn.max < column.min) {
                column.min = prevColumn.max;
                updated    = true;
            }

            if (updated) {
                column.rms = std::clamp(column.rms, column.min, column.max);
            }
        }

        if (samplesLeft != 0) {
            ++columnIndex;
            break;
        }
    }

    element.AvailableColumns = columnIndex;
    element.IsComplete       = processedSamples == elementSamplesCount;

    return processedSamples != 0;
}

bool WaveCacheSampleBlock::ContainsSample(int64_t sampleIndex) const noexcept
{
    return sampleIndex >= FirstSample
           && (sampleIndex < (FirstSample + NumSamples));
}

float* WaveCacheSampleBlock::GetWritePointer(size_t floatsCount)
{
    mData.resize(floatsCount);
    return mData.data();
}

void WaveCacheSampleBlock::Reset() noexcept
{
    FirstSample = 0;
    NumSamples  = 0;
}

namespace {
template<size_t blockSize>
void processBlock(
    const float* input, int64_t from, size_t count,
    WaveCacheSampleBlock::Summary& summary)
{
    input = input + 3 * (from / blockSize);
    count = RoundUpUnsafe(count, blockSize);

    float min        = summary.Min;
    float max        = summary.Max;
    double squareSum = summary.SquaresSum;

    for (size_t idx = 0; idx < count; ++idx) {
        min = std::min(min, *input++);
        max = std::max(max, *input++);

        const double rms = *input++;

        squareSum += rms * rms * blockSize;
    }

    // By construction, min should be always not greater than max.
    assert(min <= max);

    summary.Min        = min;
    summary.Max        = max;
    summary.SquaresSum = squareSum;
    summary.SumItemsCount += count * blockSize;
}
} // namespace

WaveCacheSampleBlock::Summary WaveCacheSampleBlock::GetSummary(
    int64_t from, size_t samplesCount, const Summary& initializer) const noexcept
{
    from = from - FirstSample;
    samplesCount
        =std::min<size_t>(samplesCount, std::max<int64_t>(0, NumSamples - from));

    const auto to = from + samplesCount;

    const float* data
        =static_cast<const float*>(static_cast<const void*>(mData.data()));

    Summary summary = initializer;

    summary.SamplesCount = samplesCount;

    switch (DataType) {
    case WaveCacheSampleBlock::Type::Samples:
        summary.SumItemsCount += samplesCount;

        for (auto sampleIndex = from; sampleIndex < to; ++sampleIndex) {
            const float sample = data[sampleIndex];

            summary.Min = std::min(summary.Min, sample);
            summary.Max = std::max(summary.Max, sample);

            summary.SquaresSum += double(sample) * double(sample);
        }

        assert(summary.Min <= summary.Max);

        break;
    case WaveCacheSampleBlock::Type::MinMaxRMS256:
        processBlock<256>(data, from, samplesCount, summary);
        break;
    case WaveCacheSampleBlock::Type::MinMaxRMS64k:
        processBlock<64 * 1024>(data, from, samplesCount, summary);
        break;
    default:
        break;
    }

    return summary;
}

void WaveCacheElement::Smooth(GraphicsDataCacheElementBase* prevElement)
{
    if (prevElement == nullptr || prevElement->AwaitsEviction || AvailableColumns == 0) {
        return;
    }

    const auto prev = static_cast<WaveCacheElement*>(prevElement);

    if (prev->AvailableColumns == 0) {
        return;
    }

    const auto prevLastColumn = prev->Data[prev->AvailableColumns - 1];
    auto& firstColumn          = Data[0];

    bool updated = false;

    if (prevLastColumn.min > firstColumn.max) {
        firstColumn.max = prevLastColumn.min;
        updated         = true;
    }

    if (prevLastColumn.max < firstColumn.min) {
        firstColumn.min = prevLastColumn.max;
        updated         = true;
    }

    if (updated) {
        firstColumn.rms
            =std::clamp(firstColumn.rms, firstColumn.min, firstColumn.max);
    }
}
