/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveformCache.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/

#include "WaveformCache.h"

#include <algorithm>
#include <cmath>
#include <cstring>

#include "SampleBlock.h"
#include "Sequence.h"
#include "WaveClip.h"

#include "PixelSampleMapper.h"
#include "waveform/WaveDataCache.h"

//
// Getting high-level data from the track for screen display and
// clipping calculations
//
class AppendBufferHelper final
{
public:
   bool
   FillBuffer(const WaveClip& clip, WaveCacheSampleBlock& outBlock) noexcept
   {
      if (
         mFirstClipSampleID != clip.GetSequence(0)->GetNumSamples() ||
         mSampleType != outBlock.DataType)
      {
         mFirstClipSampleID = clip.GetSequence(0)->GetNumSamples();
         mLastProcessedSample = 0;
         mSampleType = outBlock.DataType;

         if (mSampleType != WaveCacheSampleBlock::Type::Samples)
         {
            mCachedData.clear();
            mCachedData.resize(
               RoundUpUnsafe(clip.GetSequence(0)->GetMaxBlockSize(), 256));
         }
      }

      const size_t appendedSamples = clip.GetAppendBufferLen();

      if (appendedSamples == 0)
         return false;

      samplePtr appendBuffer = const_cast<samplePtr>(clip.GetAppendBuffer(0));

      switch (mSampleType)
      {
      case WaveCacheSampleBlock::Type::Samples:
      {
         float* outBuffer = static_cast<float*>(
            outBlock.GetWritePointer(sizeof(float) * appendedSamples));

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

      return true;
   }

private:
   template <size_t blockSize>
   void FillBlocksFromAppendBuffer(
      samplePtr buffer, size_t samplesCount, WaveCacheSampleBlock& outBlock)
   {
      const float* bufferSamples =
         static_cast<const float*>(static_cast<const void*>(buffer));

      const size_t startingBlock = mLastProcessedSample / blockSize;
      const size_t blocksCount = RoundUpUnsafe(samplesCount, blockSize);

      for (size_t blockIndex = startingBlock; blockIndex < blocksCount;
           ++blockIndex)
      {
         const size_t samplesInBlock =
            std::min(blockSize, samplesCount - blockIndex * blockSize);

         float min = std::numeric_limits<float>::infinity();
         float max = -std::numeric_limits<float>::infinity();

         double sqSum = 0.0;

         auto samples = bufferSamples + blockIndex * blockSize;

         for (size_t sampleIndex = 0; sampleIndex < samplesInBlock;
              ++sampleIndex)
         {
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
      auto ptr = outBlock.GetWritePointer(sizeof(CacheItem) * blocksCount);

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
   size_t mLastProcessedSample { 0 };
};

std::shared_ptr<WaveDataCache> CreateWaveClipDataCache(const WaveClip& clip)
{
   return std::make_shared<WaveDataCache>(
      [sequence = clip.GetSequence(0), rate = clip.GetRate(), clip = &clip,
       appendBufferHelper = AppendBufferHelper()](
         int64_t requiredSample, WaveCacheSampleBlock::Type dataType,
         WaveCacheSampleBlock& outBlock) mutable
      {
         if (requiredSample < 0)
            return false;

         outBlock.DataType = dataType;

         if (requiredSample >= sequence->GetNumSamples())
         {
            requiredSample -= sequence->GetNumSamples().as_long_long();

            if (requiredSample >= clip->GetAppendBufferLen())
               return false;

            outBlock.FirstSample = sequence->GetNumSamples().as_long_long();
            outBlock.NumSamples = clip->GetAppendBufferLen();

            return appendBufferHelper.FillBuffer(*clip, outBlock);
         }

         const auto blockIndex = sequence->FindBlock(requiredSample);
         const auto& inputBlock = sequence->GetBlockArray()[blockIndex];

         outBlock.FirstSample = inputBlock.start.as_long_long();
         outBlock.NumSamples = inputBlock.sb->GetSampleCount();

         switch (dataType)
         {
         case WaveCacheSampleBlock::Type::Samples:
         {
            samplePtr ptr = static_cast<samplePtr>(
               outBlock.GetWritePointer(outBlock.NumSamples * sizeof(float)));

            inputBlock.sb->GetSamples(
               ptr, floatSample, 0, outBlock.NumSamples, false);
         }
         break;
         case WaveCacheSampleBlock::Type::MinMaxRMS256:
         {
            size_t framesCount = RoundUpUnsafe(outBlock.NumSamples, 256);

            float* ptr = static_cast<float*>(
               outBlock.GetWritePointer(framesCount * sizeof(float) * 3));

            inputBlock.sb->GetSummary256(ptr, 0, framesCount);
         }
         break;
         case WaveCacheSampleBlock::Type::MinMaxRMS64k:
         {
            size_t framesCount = RoundUpUnsafe(outBlock.NumSamples, 64 * 1024);

            float* ptr = static_cast<float*>(
               outBlock.GetWritePointer(framesCount * sizeof(float) * 3));

            inputBlock.sb->GetSummary64k(ptr, 0, framesCount);
         }
         break;
         default:
            return false;
         }

         return true;
      },
      [](){ return std::make_unique<WaveCacheElement>(); },
      clip.GetRate());
}

WaveClipWaveformCache::WaveClipWaveformCache(WaveClip& clip)
{
   mWaveDataCache = CreateWaveClipDataCache(clip);
}

WaveClipWaveformCache::~WaveClipWaveformCache() = default;

static WaveClip::Caches::RegisteredFactory sKeyW { [](WaveClip& clip) {
   return std::make_unique<WaveClipWaveformCache>(clip);
} };

WaveClipWaveformCache& WaveClipWaveformCache::Get(const WaveClip& clip)
{
   return const_cast<WaveClip&>(clip) // Consider it mutable data
      .Caches::Get<WaveClipWaveformCache>(sKeyW);
}

std::shared_ptr<WaveDataCache> WaveClipWaveformCache::GetDataCache() const
{
   return mWaveDataCache;
}

void WaveClipWaveformCache::MarkChanged()
{
   // mWaveDataCache->Invalidate();
   // mWaveBitmapCache->Invalidate();
}

void WaveClipWaveformCache::Invalidate()
{
   mWaveDataCache->Invalidate();
}
