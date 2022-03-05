/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveDataCache.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WaveDataCache.h"
#include "FrameStatistics.h"

#include <algorithm>
#include <cassert>
#include <cmath>


WaveDataCache::WaveDataCache(DataProvider provider, double sampleRate)
    : GraphicsDataCache<WaveCacheElement>(sampleRate)
    , mProvider(std::move(provider))
{
}

bool WaveDataCache::InitializeElement(
   const GraphicsDataCacheKey& key, WaveCacheElement& element)
{
   auto sw = FrameStatistics::CreateStopwatch(
      FrameStatistics::SectionID::SpectrumView);

   element.AvailableColumns = 0;

   int64_t firstSample = key.FirstSample;

   const size_t samplesPerColumn =
      static_cast<size_t>(std::max(0.0, GetSampleRate() / key.PixelsPerSecond));

   const WaveCacheSampleBlock::Type blockType =
      samplesPerColumn >= 64 * 1024 ?
         WaveCacheSampleBlock::Type::MinMaxRMS64k :
         (samplesPerColumn >= 256 ? WaveCacheSampleBlock::Type::MinMaxRMS256 :
                                    WaveCacheSampleBlock::Type::Samples);

   size_t columnIndex = 0;

   for (; columnIndex < WaveDataCache::CacheElementWidth; ++columnIndex)
   {
      WaveCacheSampleBlock::Summary summary;
      size_t samplesLeft = samplesPerColumn;

      while (samplesLeft != 0)
      {
         if (!mCachedBlock.ContainsSample(firstSample))
            if (!mProvider(firstSample, blockType, mCachedBlock))
               break;

         summary = mCachedBlock.GetSummary(firstSample, samplesLeft, summary);

         samplesLeft -= summary.SamplesCount;
         firstSample += summary.SamplesCount;
      }

      if (summary.SamplesCount > 0)
      {
         auto& column = element.Data[columnIndex];

         column.min = summary.Min;
         column.max = summary.Max;

         column.rms = std::sqrt(summary.SquaresSum / summary.SumItemsCount);
      }

      if (samplesLeft != 0)
      {
         if (summary.SamplesCount == 0)
            --columnIndex;
         break;
      }
   }

   element.AvailableColumns = columnIndex;
   element.IsComplete =
      element.AvailableColumns == WaveDataCache::CacheElementWidth;

   return element.AvailableColumns != 0;
}

bool WaveCacheSampleBlock::ContainsSample(int64_t sampleIndex) const noexcept
{
   return sampleIndex >= FirstSample && (sampleIndex < (FirstSample + NumSamples));
}

void* WaveCacheSampleBlock::GetWritePointer(size_t bytesCount)
{
   mData.resize(bytesCount);
   return mData.data();
}

namespace
{
template <size_t blockSize>
void processBlock(
   const float* input, int64_t from, size_t count,
   WaveCacheSampleBlock::Summary& summary)
{
   input = input + 3 * (from / blockSize);
   count = RoundUp(count, blockSize);

   float min = summary.Min;
   float max = summary.Max;
   double squareSum = summary.SquaresSum;

   for (size_t idx = 0; idx < count; ++idx)
   {
      min = std::min(min, *input++);
      max = std::max(max, *input++);

      const double rms = *input++;

      squareSum += rms * rms * blockSize;
   }

   assert(min <= max);

   summary.Min = min;
   summary.Max = max;
   summary.SquaresSum = squareSum;
   summary.SumItemsCount += count * blockSize;
}
}

WaveCacheSampleBlock::Summary WaveCacheSampleBlock::GetSummary(
   int64_t from, size_t samplesCount, const Summary& initializer) const noexcept
{
   from = from - FirstSample;
   samplesCount =
      std::min<size_t>(samplesCount, std::max<int64_t>(0, NumSamples - from));

   const auto to = from + samplesCount;

   const float* data =
      static_cast<const float*>(static_cast<const void*>(mData.data()));

   Summary summary = initializer;

   summary.SamplesCount = samplesCount;

   switch (DataType)
   {
   case WaveCacheSampleBlock::Type::Samples:
      summary.SumItemsCount += samplesCount;

      for (auto sampleIndex = from; sampleIndex < to; ++sampleIndex)
      {
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
