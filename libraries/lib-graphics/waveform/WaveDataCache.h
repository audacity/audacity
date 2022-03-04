/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

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

struct GRAPHICS_API WaveCacheSampleBlock final
{
   enum class Type
   {
      Samples,
      MinMaxRMS256,
      MinMaxRMS64k,
   };

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

   bool ContainsSample(int64_t sampleIndex) const noexcept;

   void* GetWritePointer(size_t bytesCount);
private:
   Summary GetSummary(
      int64_t from, size_t samplesCount,
      const Summary& initializer) const noexcept;

   std::vector<uint8_t> mData;

   friend class WaveDataCache;
};

struct GRAPHICS_API WaveCacheElement final : GraphicsDataCacheElementBase
{
   using Columns =
      std::array<WaveDisplayColumn, GraphicsDataCacheBase::CacheElementWidth>;

   Columns Data;
   size_t AvailableColumns { 0 };
};

class GRAPHICS_API WaveDataCache final :
    public GraphicsDataCache<WaveCacheElement>
{
public:
   using DataProvider = std::function<bool (int64_t requiredSample, WaveCacheSampleBlock::Type dataType, WaveCacheSampleBlock& block)>;

   WaveDataCache(DataProvider provider, double sampleRate);

private:
   bool InitializeElement(
      const GraphicsDataCacheKey& key, WaveCacheElement& element) override;

   DataProvider mProvider;

   WaveCacheSampleBlock mCachedBlock;
};
